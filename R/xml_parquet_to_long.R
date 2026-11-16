# name=xml_parquet_to_long.R
library(arrow); library(dplyr); library(tidyr); library(stringr); library(purrr)

process_parquet_and_make_long <- function(parquet_dir,
                                         keep_person_pattern = "^Personne\\..*_\\d+$",
                                         keep_address_pattern = "^DonneesAdministratives\\..*Adresse.*_\\d+$",
                                         max_persons = NULL) {
  ds <- open_dataset(parquet_dir)
  df <- collect(ds)
  df <- df %>% mutate(.foyer_row_id = dplyr::row_number())

  # PERSONNES -> long then wide-per-person
  person_cols <- names(df)[str_detect(names(df), keep_person_pattern)]
  if (length(person_cols) == 0) {
    person_wide <- tibble::tibble(); person_long <- tibble::tibble()
  } else {
    person_long <- df %>%
      select(.foyer_row_id, all_of(person_cols)) %>%
      pivot_longer(cols = - .foyer_row_id,
                   names_to = c("varpath","idx"),
                   names_pattern = "^(Personne\\.(.*))_([0-9]+)$",
                   values_to = "value",
                   values_drop_na = FALSE) %>%
      mutate(idx = as.integer(idx), varpath = str_remove(varpath, "^Personne\\."))

    person_wide <- person_long %>%
      mutate(value = as.character(value)) %>%
      pivot_wider(id_cols = c(.foyer_row_id, idx), names_from = varpath, values_from = value)

    if (!is.null(max_persons)) person_wide <- person_wide %>% filter(idx <= max_persons)
  }

  # ADRESSES -> long + select one address per foyer
  address_cols <- names(df)[str_detect(names(df), keep_address_pattern)]
  addresses_long <- tibble::tibble(); address_selected <- tibble::tibble(); address_wide <- tibble::tibble()
  if (length(address_cols) > 0) {
    address_long <- df %>%
      select(.foyer_row_id, all_of(address_cols)) %>%
      pivot_longer(cols = - .foyer_row_id,
                   names_to = c("varpath","idx"),
                   names_pattern = "^(DonneesAdministratives\\.(.*))_([0-9]+)$",
                   values_to = "value",
                   values_drop_na = FALSE) %>%
      mutate(idx = as.integer(idx), varpath = str_remove(varpath, "^DonneesAdministratives\\."))
    addresses_long <- address_long %>%
      mutate(value = as.character(value)) %>%
      pivot_wider(id_cols = c(.foyer_row_id, idx), names_from = varpath, values_from = value)
    address_wide <- addresses_long

    # find RGADR column tolerant
    rg_candidates <- names(address_wide)[str_detect(names(address_wide), regex("(^|\\.|_)RGADR($|\\b)", ignore_case = TRUE))]
    rg_col <- if (length(rg_candidates) >= 1) rg_candidates[[1]] else NULL

    addr_sel <- address_wide %>%
      mutate(.RGADR_val = if (!is.null(rg_col)) as.character(.data[[rg_col]]) else NA_character_)

    primary_by_rg <- addr_sel %>%
      filter(!is.na(.RGADR_val) & .RGADR_val == "01") %>%
      group_by(.foyer_row_id) %>% slice_head(n = 1) %>% ungroup()

    remaining_ids <- setdiff(unique(addr_sel$.foyer_row_id), unique(primary_by_rg$.foyer_row_id))
    fallback <- addr_sel %>%
      filter(.foyer_row_id %in% remaining_ids) %>%
      group_by(.foyer_row_id) %>% arrange(idx) %>% slice_head(n = 1) %>% ungroup()

    address_selected <- bind_rows(primary_by_rg, fallback) %>% select(-.RGADR_val)
    # prefix address columns to avoid name clash
    addr_cols <- setdiff(names(address_selected), c(".foyer_row_id", "idx"))
    new_names <- paste0("Adresse.", addr_cols)
    colnames(address_selected)[which(colnames(address_selected) %in% addr_cols)] <- new_names
  }

  # attach address to person_wide and to final household
  household_df <- df %>% select(-matches(keep_person_pattern), -matches(keep_address_pattern))

  if (nrow(address_selected) > 0) {
    household_df <- household_df %>% left_join(address_selected %>% select(-idx), by = ".foyer_row_id")
  }

  if (exists("person_wide") && nrow(person_wide) > 0 && nrow(address_selected) > 0) {
    person_wide <- person_wide %>% left_join(address_selected %>% select(-idx), by = ".foyer_row_id")
  }

  # optional: produce persons_by_role_wide (re-aggregate persons by role if ROLEPERS exists)
  person_by_role <- NULL
  if (exists("person_wide") && nrow(person_wide) > 0) {
    person_wide <- person_wide %>%
      mutate(NATPREST = coalesce(as.character(`Presta_RSA.NATPREST`), as.character(`Prestations.NATPREST`), NA_character_),
             ROLEPERS = coalesce(as.character(`Presta_RSA.ROLEPERS`), as.character(`Prestations.ROLEPERS`), NA_character_),
             person_label = if_else(!is.na(ROLEPERS) & ROLEPERS != "", paste0("Personne_", ROLEPERS), paste0("Personne_", idx)))
    keep_fields <- setdiff(names(person_wide), c(".foyer_row_id", "idx", "person_label"))
    person_by_role <- person_wide %>%
      select(.foyer_row_id, person_label, all_of(keep_fields)) %>%
      pivot_wider(id_cols = .foyer_row_id, names_from = person_label, values_from = all_of(keep_fields), names_sep = ".", values_fill = list(NA))
  }

  final_joined <- household_df
  if (!is.null(person_by_role)) final_joined <- final_joined %>% left_join(person_by_role, by = ".foyer_row_id")

  list(
    household = household_df,
    persons_long = if (exists("person_wide")) person_wide else tibble::tibble(),
    addresses_long = addresses_long,
    address_selected = address_selected,
    persons_by_role_wide = person_by_role,
    final_joined = final_joined
  )
}