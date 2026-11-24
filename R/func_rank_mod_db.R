#' @author Elie.
#' @title La fonction "func_rank_mod_duckdb"
#' @description  l'objectif est de générer des variables informatives de séquence  à partir d'un booléen
#' @param data un data.table. Format long
#' @param LOGVAR variable (non standard evaluation). Typiquement un booléean qui indique si la personne perçoit ou non du RSA ou si elle participe ou non à une action
#' @param valTRUE valeur pour laquelle on considère que LOGVAR est TRUE. 
#' @return un data.table avec les nouvelles variables. Format long
#' @export
func_rank_mod_duckdb <- function(db_data, LOGVAR, valTRUE = TRUE) {
  LOGVAR_char <- deparse(substitute(LOGVAR))
  
  # Construction de la requête dplyr/dbplyr
  res <- db_data %>%
    group_by(NIR) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(
      LOGVAR2 = case_when(
        is.na({{ LOGVAR }}) ~ FALSE,
        {{ LOGVAR }} == valTRUE ~ TRUE,
        TRUE ~ FALSE
      ),
      group_id = sum((!LOGVAR2) | is.na(LOGVAR2)) %>% cumsum()
    ) %>%
    group_by(NIR, group_id) %>%
    mutate(
      rank_in_group = row_number(),
      start = (rank_in_group == 1),
      end = (rank_in_group == max(rank_in_group)),
      start_rang = min(RANG_mois),
      start_date = min(date),
      end_rang = max(RANG_mois),
      end_date = max(date),
      time_rang = RANG_mois - start_rang,
      time_date = date - start_date,
      cumsum_since_last_na = cumsum(LOGVAR2)
    ) %>%
    ungroup()
  
  # Collecte du résultat en R
  res <- collect(res)
  
  # Nettoyage des noms de colonnes
  names(res)[grepl("^LOGVAR2|^group_id|^rank_in_group|^start|^end|^start_rang|^start_date|^end_rang|^end_date|^time_rang|^time_date|^cumsum_since_last_na", names(res))] <-
    gsub(
      pattern = "LOGVAR2|group_id|rank_in_group|start|end|start_rang|start_date|end_rang|end_date|time_rang|time_date|cumsum_since_last_na",
      replacement = paste0(LOGVAR_char, "_\\0"),
      x = names(res)[grepl("^LOGVAR2|^group_id|^rank_in_group|^start|^end|^start_rang|^start_date|^end_rang|^end_date|^time_rang|^time_date|^cumsum_since_last_na", names(res))]
    )
  
  return(res)
}


# seq_long[, group_rsa := cumsum(is.na(RSA_simple_corr)), by = NIR]
# seq_long[, STATUS_RSA := tidyr::replace_na(RSA_simple_corr=="RSA", FALSE)]
# seq_long[, RSA_cum_rank := cumsum(STATUS_RSA), by = NIR]
# ##### Créer un identifiant de groupe qui change à chaque NA
# ##### Calculer la somme cumulée depuis la dernière interruption (NA)
# seq_long[, RSA_cum_sum_since_last_na := cumsum(STATUS_RSA), by = .(NIR, group_rsa)]
# seq_long[, RSA_cum_sum_since_last_na := if_else((STATUS_RSA==FALSE), NA, RSA_cum_sum_since_last_na )]
# seq_long[, RSA_start_date := min(date[RSA_cum_rank==1], na.rm=TRUE), by = NIR]
# seq_long[, RSA_end_date := max(date[STATUS_RSA], na.rm=TRUE), by = NIR]
# seq_long[, RSA_start_rank := rank[RSA_cum_rank==1 & STATUS_RSA], by = NIR]
# #### TSI #####
# seq_long[, TSI_cum_rank := cumsum(STATUS_TSI), by = NIR]
# seq_long[, TSI_start_date := date[TSI_cum_rank==1 & STATUS_TSI],  by = NIR]
# seq_long[, TSI_end_date := max(date[STATUS_TSI], na.rm=TRUE), by = NIR]
# seq_long[, TSI_start_rank := rank[TSI_cum_rank==1 & STATUS_TSI], by = NIR]
# ####
# seq_long[, rang_start :=  (rank - TSI_start_rank)+1, by = NIR]
