#' Génère des variables de séquence à partir d'un booléen 
#' 
#' Cette fonction permet de créer des variables informatives sur les séquences d'un événement (par exemple, perception du RSA ou participation à une action) à partir d'une variable booléenne, en format long. Elle calcule notamment le rang, les dates de début/fin, et la durée de chaque séquence pour chaque individu. 
#' 
#' @author Elie 
#' @title Génération de variables de séquence à partir d'un booléen 
#' @description 
#' L'objectif de cette fonction est de générer, pour chaque individu, des variables décrivant les séquences continues où une condition booléenne est vérifiée. Elle identifie les débuts et fins de séquence, calcule les rangs, les dates de début/fin, et la durée depuis le début de la séquence. 
#' 
#' @param data Un \code{data.table} au format long, contenant les données individuelles (une ligne par individu et par période). 
#' @param LOGVAR Variable de type booléen indiquant la présence ou non d'un événement (ex : perception du RSA, participation à une action). 
#' @param valTRUE Valeur considérée comme "vrai" pour \code{LOGVAR}. Par défaut : \code{TRUE}. 
#' @param date_var Nom de la variable de date. Par défaut : \code{date}. 
#' @param rang_mois Nom de la variable de rang temporel. Par défaut : \code{RANG_mois}. 
#' 
#' @return Un \code{data.table} enrichi des nouvelles variables de séquence, au format long : 
#' \itemize{ 
#'   \item \code{<LOGVAR>_group} : identifiant de groupe de séquence. 
#'   \item \code{<LOGVAR>_status} : statut booléen (TRUE/FALSE) pour la séquence. 
#'   \item \code{<LOGVAR>_rank} : rang cumulatif dans la séquence. 
#'   \item \code{<LOGVAR>_start} : indicateur de début de séquence. 
#'   \item \code{<LOGVAR>_end} : indicateur de fin de séquence. 
#'   \item \code{<LOGVAR>_start_rang} : rang de début de séquence. 
#'   \item \code{<LOGVAR>_start_date} : date de début de séquence. 
#'   \item \code{<LOGVAR>_end_rang} : rang de fin de séquence. 
#'   \item \code{<LOGVAR>_end_date} : date de fin de séquence. 
#'   \item \code{<LOGVAR>_time_rang} : nombre de périodes depuis le début de la séquence. 
#'   \item \code{<LOGVAR>_time_date} : durée depuis le début de la séquence (en jours). 
#'   \item \code{<LOGVAR>_CumSum_SinceLastNA} : cumul du statut depuis la dernière interruption (NA ou FALSE). 
#' } 
#' 
#' @details 
#' Cette fonction est utile pour l'analyse de trajectoires individuelles (ex : parcours d'insertion, suivi d'allocations). Elle gère automatiquement les interruptions (NA ou FALSE) et recalcule les séquences à chaque reprise. 
#' 
#' @seealso \code{\link[data.table]{data.table}}, \code{\link[dplyr]{mutate}}, \code{\link[tidyr]{replace_na}} 
#' @export
func_rank_mod <- function(data, LOGVAR, valTRUE = TRUE, date_var = date, rang_mois = RANG_mois ) {
  require(data.table)
  require(dtplyr)

  if (
    (class((data |> pull( {{ LOGVAR }}))) == "logical" ) #&
      #sum(is.na((data |> pull({{ LOGVAR }})))) == 0
  ) {
    data <- data |>
      mutate(across({{ LOGVAR }}, ~ na_if(.x, FALSE)))
  }

  LOGVAR_char <- deparse(substitute(LOGVAR))

  res <- data |>
    dtplyr::lazy_dt(immutable = F) |>
    group_by(NIR) |>
    arrange({{date_var}}, .by_group = TRUE) |>
    mutate(func_temp_VAR_group = cumsum( is.na({{ LOGVAR }}) | {{ LOGVAR }}==FALSE )) |>
    mutate(
      func_temp_VAR_status = tidyr::replace_na({{ LOGVAR }} == valTRUE, FALSE)
    ) |>
    mutate(func_temp_VAR_rank = cumsum(func_temp_VAR_status)) |>
    mutate(
      func_temp_VAR_start = tidyr::replace_na(
        (func_temp_VAR_rank == 1 &
          !shift(func_temp_VAR_status, type = "lag", fill = FALSE)),
        FALSE
      )
    ) |>
    mutate(
      func_temp_VAR_end = tidyr::replace_na(
        (func_temp_VAR_rank == max(func_temp_VAR_rank) &
          !shift(func_temp_VAR_status, type = "lead", fill = FALSE)),
        FALSE
      )
    ) |>
    mutate(
      func_temp_VAR_start_rang = min({{rang_mois}}[func_temp_VAR_start]),
      func_temp_VAR_start_date = min({{date_var}}[func_temp_VAR_start]),
      func_temp_VAR_end_rang = max({{rang_mois}}[{{ LOGVAR }}], na.rm = TRUE),
      func_temp_VAR_end_date = max({{date_var}}[{{ LOGVAR }}], na.rm = TRUE)
    ) |>
    mutate(
      func_temp_VAR_start_rang = data.table::fifelse(
        func_temp_VAR_start_rang == Inf,
        NA,
        func_temp_VAR_start_rang
      ),
      func_temp_VAR_start_date = data.table::fifelse(
        func_temp_VAR_start_date == Inf,
        NA,
        func_temp_VAR_start_date
      )
    ) |>
    mutate(
      func_temp_VAR_time_rang = {{rang_mois}} - func_temp_VAR_start_rang,
      func_temp_VAR_time_date = {{date_var}} - func_temp_VAR_start_date
    ) |>
    group_by(NIR, func_temp_VAR_group) |>
    mutate(func_temp_VAR_CumSum_SinceLastNA = cumsum(func_temp_VAR_status)) |>
    ungroup() |>
    mutate(
      func_temp_VAR_CumSum_SinceLastNA = if_else(
        (func_temp_VAR_status == FALSE),
        NA,
        func_temp_VAR_CumSum_SinceLastNA
      )
    ) |>
    data.table::as.data.table()

  names(res)[base::startsWith(x = names(res), "func_temp_VAR_")] <- gsub(
    pattern = "func_temp_VAR_",
    replacement = paste0(LOGVAR_char, "_"),
    x = names(res)[base::startsWith(x = names(res), "func_temp_VAR_")]
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

