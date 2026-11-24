#' @author Elie.
#' @title La fonction "func_rank_mod_DT"
#' @description  l'objectif est de générer des variables informatives de séquence  à partir d'un booléen
#' @param data un data.table. Format long
#' @param LOGVAR variable (non standard evaluation). Typiquement un booléean qui indique si la personne perçoit ou non du RSA ou si elle participe ou non à une action
#' @param valTRUE valeur pour laquelle on considère que LOGVAR est TRUE. 
#' @return un data.table avec les nouvelles variables. Format long
#' @export
func_rank_mod_DT <- function(DT, LOGVAR, valTRUE = TRUE) {
  require(data.table)
  stopifnot(is.data.table(DT))
  
  LOGVAR_char <- deparse(substitute(LOGVAR))
  
  # 1. Si LOGVAR est logique sans NA, remplacer FALSE par NA
  if (is.logical(DT[[LOGVAR_char]]) && sum(is.na(DT[[LOGVAR_char]])) == 0) {
    DT[, (LOGVAR_char) := na_if(get(LOGVAR_char), FALSE)]
  }
  
  # 2. Calculs groupés par NIR
  DT <- DT[order(NIR, date)]
  
  # 3. Création des variables temporaires
  DT[, func_temp_VAR_group := cumsum(is.na(get(LOGVAR_char))), by = NIR]
  DT[, func_temp_VAR_status := fifelse(is.na(get(LOGVAR_char)), FALSE, get(LOGVAR_char) == valTRUE)]
  DT[, func_temp_VAR_rank := cumsum(func_temp_VAR_status), by = NIR]
  
  # 4. Début de séquence
  DT[, func_temp_VAR_start := fifelse(
    (func_temp_VAR_rank == 1 & 
     shift(func_temp_VAR_status, type = "lag", fill = FALSE) == FALSE),
    TRUE, FALSE
  ), by = NIR]
  
  # 5. Fin de séquence
  DT[, func_temp_VAR_end := fifelse(
    (func_temp_VAR_rank == max(func_temp_VAR_rank, na.rm = TRUE) &
     shift(func_temp_VAR_status, type = "lead", fill = FALSE) == FALSE),
    TRUE, FALSE
  ), by = NIR]
  
  # 6. Calcul des dates/rangs de début/fin
  DT[, func_temp_VAR_start_rang := ifelse(any(func_temp_VAR_start), min(RANG_mois[func_temp_VAR_start]), NA), by = NIR]
  DT[, func_temp_VAR_start_date := ifelse(any(func_temp_VAR_start), min(date[func_temp_VAR_start]), NA), by = NIR]
  DT[, func_temp_VAR_end_rang := max(RANG_mois[get(LOGVAR_char)], na.rm = TRUE), by = NIR]
  DT[, func_temp_VAR_end_date := max(date[get(LOGVAR_char)], na.rm = TRUE), by = NIR]
  
  DT[is.infinite(func_temp_VAR_start_rang), func_temp_VAR_start_rang := NA]
  DT[is.infinite(func_temp_VAR_start_date), func_temp_VAR_start_date := NA]
  
  # 7. Temps écoulé depuis le début de la séquence
  DT[, func_temp_VAR_time_rang := RANG_mois - func_temp_VAR_start_rang]
  DT[, func_temp_VAR_time_date := date - func_temp_VAR_start_date]
  
  # 8. Cumul depuis le dernier NA (par groupe)
  DT[, func_temp_VAR_CumSum_SinceLastNA := cumsum(func_temp_VAR_status), by = .(NIR, func_temp_VAR_group)]
  DT[func_temp_VAR_status == FALSE, func_temp_VAR_CumSum_SinceLastNA := NA]
  
  # 9. Renommer les colonnes temporaires
  for (col in names(DT)[startsWith(names(DT), "func_temp_VAR_")]) {
    setnames(DT, col, sub("func_temp_VAR_", paste0(LOGVAR_char, "_"), col))
  }
  return(DT)
}