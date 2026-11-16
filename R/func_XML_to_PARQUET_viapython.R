# name = func_XML_to_PARQUET_viapython.R
library(glue)
library(dplyr)
library(arrow)

#' Wrapper complet : convertir un XML via Python, transformer en R, écrire Parquet final
#'
#' Cette fonction appelle xml_to_parquet_via_python() puis process_parquet_and_make_long()
#' et écrit le résultat final dans une BDD parquet. Elle prend la signature complète
#' que vous utilisiez dans vos appels.
#'
#' @param chemin.xml chemin vers un fichier XML (string)
#' @param python_script chemin absolu vers xml_to_parquet_wide_direct.py
#' @param out_base_dir répertoire de base pour dossiers parquet intermédiaires
#' @param parquet_final_output_dir répertoire destination de la BDD parquet finale
#' @param record_tag nom du noeud à extraire (par défaut "InfosFoyerRSA")
#' @param py_batch_size taille des batches pour le script Python
#' @param write_persons_parquet sauvegarder persons_long sous out_dir/persons_parquet (TRUE/FALSE)
#' @param write_addresses_parquet sauvegarder address_selected sous out_dir/address_selected_parquet (TRUE/FALSE)
#' @param cleanup_intermediate supprimer out_dir après succès (TRUE/FALSE)
#' @param existing_data_behavior.fc passé à arrow::write_dataset (ex: "append" ou "overwrite_or_ignore")
#' @param apply_func_transfo si TRUE et si func_transfo existe, l'applique à final_df
#' @param return_final_df si TRUE renvoie final_df (attention mémoire)
#' @param python_cmd chemin vers exécutable Python (optionnel)
#' @return liste de méta (ok, xml, counts, final_df si demandé)
#' @export
func_XML_TO_PARQUET_viapython <- function(
  chemin.xml,
  python_script,
  out_base_dir = "py_parquets",
  parquet_final_output_dir = "BDD/datalist",
  record_tag = "InfosFoyerRSA",
  py_batch_size = 3000,
  write_persons_parquet = TRUE,
  write_addresses_parquet = TRUE,
  cleanup_intermediate = TRUE,
  existing_data_behavior.fc = "append",
  apply_func_transfo = TRUE,
  return_final_df = FALSE,
  python_cmd = NULL
) {
  # checks
  stopifnot(file.exists(chemin.xml))
  stopifnot(file.exists(python_script))
  # require the two helper functions
  if (!exists("xml_to_parquet_via_python")) stop("xml_to_parquet_via_python() non trouvé. source xml_to_parquet_via_python.R d'abord.")
  if (!exists("process_parquet_and_make_long")) {
    if (file.exists("process_parquet_and_make_long.R")) source("process_parquet_and_make_long.R")
    if (!exists("process_parquet_and_make_long")) stop("process_parquet_and_make_long() non trouvé. source process_parquet_and_make_long.R d'abord.")
  }

  msg <- function(...) message(glue::glue(...))

  xml_basename <- tools::file_path_sans_ext(basename(chemin.xml))
  out_dir <- file.path(out_base_dir, paste0(xml_basename, "_parquet"))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # 1) Conversion Python -> wide parquet parts
  msg("Running Python conversion for {basename(chemin.xml)} -> {out_dir}")
  # xml_to_parquet_via_python should accept python_cmd optionally
  if (!is.null(python_cmd)) {
    xml_to_parquet_via_python(xml_file = chemin.xml, out_dir = out_dir, python_script = python_script,
                              record_tag = record_tag, batch_size = py_batch_size, python_cmd = python_cmd)
  } else {
    xml_to_parquet_via_python(xml_file = chemin.xml, out_dir = out_dir, python_script = python_script,
                              record_tag = record_tag, batch_size = py_batch_size)
  }

  # sanity: check parts present
  parts <- list.files(out_dir, pattern = "^part-.*\\.parquet$", full.names = TRUE)
  if (length(parts) == 0) stop("Aucun part-*.parquet produit par le script Python pour ", chemin.xml)

  msg("Python conversion finished; reading parts in R...")

  # 2) R transformation wide -> persons_long, address_selected, final_joined
  res_r <- process_parquet_and_make_long(parquet_dir = out_dir)

  msg("R transform done: persons={nrow(res_r$persons_long)} addresses_selected={nrow(res_r$address_selected)}")

  # 3) assemble final_df and apply domain transform if requested
  final_df <- res_r$final_joined %>% mutate(FICHIER_SOURCE = chemin.xml)
  if (!"DATE" %in% names(final_df)) final_df$DATE <- NA_character_
  if (!"INSTITUTION" %in% names(final_df)) final_df$INSTITUTION <- NA_character_

  if (apply_func_transfo) {
    if (exists("func_transfo")) {
      tryCatch({
        final_df <- func_transfo(final_df)
      }, error = function(e) {
        warning("func_transfo erreur sur ", basename(chemin.xml), ": ", conditionMessage(e))
      })
    } else {
      msg("func_transfo() non trouvée ; transformation métier sautée.")
    }
  }

  # 4) write final_df to central parquet dataset (group by DATE, INSTITUTION)
  if (!requireNamespace("arrow", quietly = TRUE)) stop("Le package 'arrow' est requis pour écrire le dataset.")
  final_df <- final_df %>% mutate(DATE = as.character(DATE), INSTITUTION = as.character(INSTITUTION))
  arrow::write_dataset(dataset = final_df %>% dplyr::group_by(DATE, INSTITUTION),
                       path = parquet_final_output_dir,
                       existing_data_behavior = existing_data_behavior.fc)
  msg("Final dataset written/appended to {parquet_final_output_dir}")

  # 5) optionally save persons_long and address_selected under out_dir
  if (write_persons_parquet && nrow(res_r$persons_long) > 0) {
    persons_dir <- file.path(out_dir, "persons_parquet")
    dir.create(persons_dir, recursive = TRUE, showWarnings = FALSE)
    arrow::write_dataset(res_r$persons_long, path = persons_dir)
    msg("Persons saved to {persons_dir}")
  }
  if (write_addresses_parquet && nrow(res_r$address_selected) > 0) {
    addr_dir <- file.path(out_dir, "address_selected_parquet")
    dir.create(addr_dir, recursive = TRUE, showWarnings = FALSE)
    arrow::write_dataset(res_r$address_selected, path = addr_dir)
    msg("Address saved to {addr_dir}")
  }

  # 6) cleanup
  if (cleanup_intermediate) {
    try(unlink(out_dir, recursive = TRUE), silent = TRUE)
    msg("Cleaned intermediate folder {out_dir}")
  }

  # 7) return summary + optionally final_df
  out <- list(
    ok = TRUE,
    xml = chemin.xml,
    n_persons = nrow(res_r$persons_long),
    n_addresses = nrow(res_r$addresses_long),
    n_addresses_selected = nrow(res_r$address_selected),
    parts_dir = if (cleanup_intermediate) NA_character_ else out_dir,
    final_df = if (return_final_df) final_df else NULL
  )
  return(out)
}