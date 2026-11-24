#' @author Elie 
#' @title Tirage aléatoire de témoins appariés 
#' @description Tire au sort des individus témoins ayant les mêmes caractéristiques que ceux de data_test_val. 
#' @param data_test_val data.frame : individus tests 
#' @param DT_temoin data.frame : base de témoins potentiels 
#' @param N_temoin int : nombre de témoins par individu 
#' @param verbose bool : affichage d'infos 
#' @return data.table 
#' @export 
func_sample_id_mrg <- function( 
  data_test_val, 
  DT_temoin = data_temoin, 
  N_temoin = 10, 
  verbose = TRUE 
) { 
  # 1. Préparation de DT_temoin 
  DT_temoin[, `:=`(AGE_min = AGE, AGE_max = AGE)] 
  data.table::setnames(DT_temoin, "NIR", "NIR_temoin", skip_absent = TRUE) 
  DT_temoin <- DT_temoin[!is.na(AGE_min)] 

  # 2. Premier appariement strict 
  data_test_val[, `:=`(AGE_min = AGE - 2.5, AGE_max = AGE + 2.5)] 
  data_test_val <- data_test_val[!is.na(AGE_min)] 

  ## a_Variables d'appariement 
  var_by1 <- c("RSA_simple_corr", "Q", "SEXE", "Global_fam", 
               "typo_agence", "RSA_simple_corr_time_rang_CUT", "RSA_simple_corr_CumSum_SinceLastNA_CUT", 
               "AGE_min", "AGE_max") 
  var_temoin1 <- c("NIR_temoin", "DATE", "date", var_by1) 
  var_keep1 <- c("NIR", var_temoin1) 

  ## b_ Premier appariement strict 
  setkeyv(DT_temoin, var_by1) 
  setkeyv(data_test_val, var_by1) 
  result_1 <- foverlaps( 
    DT_temoin[, ..var_temoin1], 
    data_test_val, 
    by.x = var_by1, 
    by.y = var_by1, 
    type = "within", 
    nomatch = NULL 
  )[, ..var_keep1] 


  # 3. Deuxième passage
  data_test_val[, `:=`(AGE_min = AGE - 4, AGE_max = AGE + 4)] 
  DT_temoin[, `:=`(Q_min = Q_num, Q_max = Q_num)] 
  data_test_val[, `:=`(Q_min = Q_num - 2, Q_max = Q_num)] 
 
  ## a_relachement sur l'âge
  var_by2_age <- c("RSA_simple_corr", "Personne.Identification.SEXE", "Global_fam", 
                   "typo_agence", "RSA_simple_corr_time_rang_CUT", "RSA_simple_corr_CumSum_SinceLastNA_CUT", 
                   "AGE_min", "AGE_max") 
  var_temoin2_age <- c("NIR_temoin", "DATE", "date", var_by2_age) 
  var_keep2_age <- c("NIR", var_temoin2_age) 
 
  setkeyv(DT_temoin, var_by2_age) 
  setkeyv(data_test_val, var_by2_age) 
  result_2_age <- foverlaps( 
    DT_temoin[, ..var_temoin2_age], 
    data_test_val[!NIR %in% result_1$NIR_temoin], 
    by.x = var_by2_age, 
    by.y = var_by2_age, 
    type = "within", 
    nomatch = NULL 
  )[, ..var_keep2_age] 
 
  ## b_relachement sur Q
  var_by2_Q <- c("RSA_simple_corr", "Personne.Identification.SEXE", "Global_fam", 
                 "typo_agence", "RSA_simple_corr_time_rang_CUT", "RSA_simple_corr_CumSum_SinceLastNA_CUT", 
                 "Q_min", "Q_max") 
  var_temoin2_Q <- c("NIR_temoin", "DATE", "date", var_by2_Q) 
  var_keep2_Q <- c("NIR", var_temoin2_Q) 
  setkeyv(DT_temoin, var_by2_Q) 
  setkeyv(data_test_val, var_by2_Q) 
  result_2_Q <- foverlaps( 
    DT_temoin[, ..var_temoin2_Q], 
    data_test_val[!NIR %in% result_1$NIR_temoin], 
    by.x = var_by2_Q, 
    by.y = var_by2_Q, 
    type = "within", 
    nomatch = NULL 
  )[, ..var_keep2_Q] 
 
  ## c_Intersection stricte sur toutes les colonnes communes (hors intervalles) 
  cols_compare <- setdiff( 
    intersect(names(result_2_age), names(result_2_Q)), 
    c("Q_min", "Q_max", "AGE_min", "AGE_max") 
  ) 
  result_2_communs <- merge( 
    result_2_age[, ..cols_compare], 
    result_2_Q[, ..cols_compare], 
    by = cols_compare 
  ) 
  result_2 <- unique( 
    DT_temoin[result_2_communs, on = cols_compare[cols_compare != "NIR"], nomatch = 0] 
  ) 
 
  # 4. Assemblage final 
  ensemble_temoins <- data.table::rbindlist(list(result_1, result_2), use.names = TRUE, fill = TRUE) 
  ensemble_temoins[  , TYPE := "TEMOIN"]
  # 5. SAMPLIG
  ensemble_temoins <- ensemble_temoins |> 
    dtplyr::lazy_dt() |> 
    group_by(NIR) |> 
    mutate(Ntemoins = n_distinct(NIR_temoin)) |> 
    ungroup() |> 
    arrange(Ntemoins, NIR) |> 
    data.table::as.data.table()

unique_temoins <- ensemble_temoins |> 
      dtplyr::lazy_dt() |> 
  dplyr::distinct(NIR_temoin, .keep_all = TRUE) |> 
  group_by(NIR) |> 
  mutate(Ntemoins_unique = n_distinct(NIR_temoin)) |> 
  dplyr::slice_sample(n=N_temoin) |> 
  mutate(Ntemoins_final = n_distinct(NIR_temoin)) |> 
  ungroup() |> 
  mutate(POID = N_temoin / Ntemoins_final) |> 
  arrange(Ntemoins_final, NIR)|> 
    data.table::as.data.table()

unique_temoins <- unique_temoins |> 
  dplyr::rename( 
    NIR_TEST = NIR,      # l'ancienne colonne NIR devient NIR_test 
    NIR      = NIR_temoin  # NIR_temoin devient NIR 
  ) 
  ensemble_temoins <- ensemble_temoins |> 
  dplyr::rename( 
    NIR_test = NIR,      # l'ancienne colonne NIR devient NIR_test 
    NIR      = NIR_temoin  # NIR_temoin devient NIR 
  ) 

  if (verbose) { 
    cat("Témoins appariés trouvés :", nrow(ensemble_temoins), "\n") 
    cat("Témoins appariés retenus :", nrow(unique_temoins), "\n") 

  }
  
  res <- list(
    "unique_temoins"=unique_temoins,
    "ensemble_temoins"=ensemble_temoins
  )

  return(res) 

} 
