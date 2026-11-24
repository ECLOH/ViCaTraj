#' @author Elie.
#' @title La fonction "func_sample_id"
#' @description  l'objectif est tirer au sort des individus témoins ayant les mêmes caractéristiques que les individus décrits dans data_test_val
#' @param data_test_val un objet data.frame
#' @param DT_temoin data.frame dans lequel aller chercher les témoins
#' @param list_var vecteur des variables utilisées
#' @param EXPR_matching_temoin conditions à remplir, au format chaine de character
#' @param EXPR_matching_temoin_2 conditions à remplir au second passage, si le premier passage n'a pas permis de trouver de témoin
#' @param N_temoin nombre de témoin par individu
#' @return un data.table
#' @export
func_sample_id_mrg <- function(
  data_test_val,
  DT_temoin = data_temoin,
  N_temoin = 10,
  verbose = TRUE
) {

  DT_temoin[, AGE_min := AGE]
  DT_temoin[, AGE_max := AGE]
  data.table::setnames(DT_temoin, "NIR", "NIR_temoin",  skip_absent=TRUE) 


  DT_temoin <- DT_temoin[ !NOMTERRITOIRE %in% c('Porte des Alpes', 'Vals du Dauphiné', 'Voironnais Chartreuse') & ORIENTATION == 'Accompagnement PSSI']
  data_test_val[, `:=`(AGE_min = AGE - 2.5, AGE_max = AGE + 2.5)]

  setkey(
    DT_temoin,
    RSA_simple_corr,
    Q,
    Personne.Identification.SEXE,
    Global_fam,
    typo_agence,
    RSA_simple_corr_time_rang_CUT,
    RSA_simple_corr_CumSum_SinceLastNA_CUT,
    AGE_min,
    AGE_max
  )

  setkey(
    data_test_val,
    RSA_simple_corr,
    Q,
    Personne.Identification.SEXE,
    Global_fam,
    typo_agence,
    RSA_simple_corr_time_rang_CUT,
    RSA_simple_corr_CumSum_SinceLastNA_CUT,
    AGE_min,
    AGE_max
  )

  data_test_val <- data_test_val[!is.na(AGE_min)]
  DT_temoin <- DT_temoin[!is.na(AGE_min)]

  VAR1_by <- c("RSA_simple_corr",
      "Q",
      "Personne.Identification.SEXE",
      "Global_fam",
      "typo_agence",
      "RSA_simple_corr_time_rang_CUT",
      "RSA_simple_corr_CumSum_SinceLastNA_CUT",
      "AGE_min",
      "AGE_max")
  VAR1_temoin <- c("NIR_temoin", "DATE", "date", VAR1_by)
  VAR1_keep <- c("NIR", VAR1_temoin)


  result_1 <- foverlaps(
    DT_temoin |> select(all_of(c(
     VAR1_temoin
    ))),
    data_test_val,
    by.x = c(VAR1_by),
    by.y = c(VAR1_by),
    type = "within",
    nomatch = NULL
  )[, ..VAR1_keep]


  if(verbose){
  print("data_test_val : dim()")
  print(dim(data_test_val))
  print("result_1 |> group_by(NIR_temoin) |> slice_head(n=1) : dim()")
  print(dim(result_1 |> group_by(NIR_temoin) |> slice_head(n=1)))
  }
  data_test_val[, `:=`(AGE_min = AGE - 4, AGE_max = AGE + 4)]
  DT_temoin[, Q_min := Q_num]
  DT_temoin[, Q_max := Q_num]
  data_test_val[, `:=`(Q_min = Q_num - 2, Q_max = Q_num)]
  setkey(
    DT_temoin,
    RSA_simple_corr,
    Personne.Identification.SEXE,
    Global_fam,
    typo_agence,
    RSA_simple_corr_time_rang_CUT,
    RSA_simple_corr_CumSum_SinceLastNA_CUT,
    AGE_min,
    AGE_max
  )

  setkey(
    data_test_val,
    RSA_simple_corr,
    Personne.Identification.SEXE,
    Global_fam,
    typo_agence,
    RSA_simple_corr_time_rang_CUT,
    RSA_simple_corr_CumSum_SinceLastNA_CUT,
    AGE_min,
    AGE_max
  )
  print("b1")
  VAR2_age_by <- c(  "RSA_simple_corr",
      "Personne.Identification.SEXE",
      "Global_fam",
      "typo_agence",
      "RSA_simple_corr_time_rang_CUT",
      "RSA_simple_corr_CumSum_SinceLastNA_CUT",
      "AGE_min",
      "AGE_max")
  VAR2_age_temoin <- c("NIR_temoin", "DATE", "date", VAR2_age_by)
  VAR2_age_keep <- c("NIR", VAR2_age_temoin)

  result_2_age <- foverlaps(
    DT_temoin |>  select(all_of(c(
    VAR2_age_temoin
    ))),
    data_test_val |> anti_join(result_1, by = c("NIR" = "NIR_temoin")),
    by.x = c(
     VAR2_age_by
    ),
    by.y = c(
     VAR2_age_by
    ),
    type = "within",
    nomatch = NULL
  )[, ..VAR2_age_keep]

    print("b2")
    setkey(
    DT_temoin,
    RSA_simple_corr,
    Personne.Identification.SEXE,
    Global_fam,
    typo_agence,
    RSA_simple_corr_time_rang_CUT,
    RSA_simple_corr_CumSum_SinceLastNA_CUT,
    Q_min, Q_max
  )

  setkey(
    data_test_val,
    RSA_simple_corr,
    Personne.Identification.SEXE,
    Global_fam,
    typo_agence,
    RSA_simple_corr_time_rang_CUT,
    RSA_simple_corr_CumSum_SinceLastNA_CUT,
    Q_min, Q_max
  )

    VAR2_Q_by <- c(  "RSA_simple_corr",
      "Personne.Identification.SEXE",
       "Global_fam",
      "typo_agence",
      "RSA_simple_corr_time_rang_CUT",
      "RSA_simple_corr_CumSum_SinceLastNA_CUT",
      "Q_min",
      "Q_max")
  VAR2_Q_temoin <- c("NIR_temoin", "DATE", "date", VAR2_Q_by)
  VAR2_Q_keep <- c("NIR", VAR2_Q_temoin)

  result_2_Q <- foverlaps(
    DT_temoin |>  select(all_of(c(
     VAR2_Q_temoin
    ))) ,
    data_test_val |> anti_join(result_1, by = c("NIR" = "NIR_temoin")),
    by.x = c(
     VAR2_Q_by
    ),
    by.y = c(
     VAR2_Q_by
    ),
    type = "within",
    nomatch = NULL
  )[, ..VAR2_Q_keep]

    print("b3")

  mix2 <- basiques::mixages(vecs = list("age"=result_2_age$NIR_temoin, "Q" = result_2_Q$NIR_temoin))
  if(verbose){
  print(mix2$communs)
  print(mix2$communs %in% DT_temoin$NIR)
  print(class(DT_temoin$NIR))
  print(class(mix2$communs))
  }

  result_2 <- DT_temoin[NIR_temoin %in% mix2$communs]
# 1. Définir les colonnes de comparaison 
cols_compare <- setdiff( 
  intersect(names(result_2_age), names(result_2_Q)), 
  c("Q_min", "Q_max", "AGE_min", "AGE_max") 
) 
# 2. Intersection stricte sur toutes les colonnes communes (hors intervalles) 
  print("b52")
  print(head(result_2_age[, ..cols_compare]))
  print(head(result_2_Q[, ..cols_compare]))

result_2_communs <- data.table:::merge.data.table( 
  result_2_age[, ..cols_compare], 
  result_2_Q[, ..cols_compare], 
  by = cols_compare 
)
   print("b53")

# 3. Récupérer les lignes dans DT_temoin (optionnel, si tu veux les lignes originales) 
result_2 <- unique( 
  DT_temoin[result_2_communs, on = cols_compare[cols_compare!="NIR"], nomatch = 0] 
) 




  if(verbose){
  print("  print(dim(result_2))")
  print(dim(result_2))
  print("b4")
  print("result_2 |> group_by(NIR) |> slice_head(n=1) : dim()")
  print(dim(result_2 |> group_by(NIR) |> slice_head(n=1)))
  }

  res <- dplyr::bind_rows(result_1, result_2)
  return(res)

}




#   # Liste pour stocker les échantillons 
#   samples <- list() 
#   NIRS_l <- c()
#   NIR_manque <- c()
#   # Pour chaque individu du groupe TEST 
#   for (i in 1:nrow(data_test_val)) { 

#     # print(i)
#     # Caractéristiques de l'individu 

#     if(is.null(list_var)){
#       list_var <- c("NIR", "Q", "RSA_simple_corr", "AGE", "Personne.Identification.SEXE", "Global_fam", "typo_agence", "IDEPCI", "DonneesAdministratives.Adresse.AdresseDetailleeFrance.NUMCOM", 
#       "RSA_simple_corr_time_rang_CUT", "RSA_simple_corr_CumSum_SinceLastNA_CUT")
#     }

#     lis_i <- lapply(list_var, function(ivar){
#               res <- data_test_val[i , ..ivar] |> unlist(use.names = F)
#               if(inherits(res, "factor")){
#                 res <- as.character(res)
#               }
#       return(res)
#     })
#     names(lis_i) <- paste0(list_var, "_i")
#     #print(lis_i)
#     # lis_i <- list(
#     #   "NIR_i" =        data_test_val$NIR[i],
#     #   "Q_i" =          data_test_val$Q[i],
#     #   "RSA_i"=         data_test_val$RSA_simple_corr[i],
#     #   "age_i" =        data_test_val$AGE[i],
#     #   "sexe_i" =       data_test_val$Personne.Identification.SEXE[i],
#     #   "Global_fam_i" = data_test_val$Global_fam[i],
#     #   "typo_agence_i"= data_test_val$typo_agence[i],
#     #   "epci_i" =       data_test_val$IDEPCI[i],
#     #   "ANC_droit" =    data_test_val$RSA_simple_corr_time_rang_CUT[i],
#     #   "ANC_continue" = data_test_val$RSA_simple_corr_CumSum_SinceLastNA_CUT[i],
#     #   "COMMUNE" =      data_test_val$DonneesAdministratives.Adresse.AdresseDetailleeFrance.NUMCOM[i]
#     #   )
#     # Filtrer le groupe TEMOIN avec les mêmes caractéristiques 
#     if(is.null(EXPR_matching_temoin)){
#     matching_temoin <- DT_temoin[
#       NIR != lis_i$NIR_i & !(NIR %in% NIRS_l) &
#       RSA_simple_corr == lis_i$RSA_i &
#       Q==lis_i$Q_i & 
#       (AGE > (lis_i$age_i-2.5) & AGE < (lis_i$age_i+2.5))  &  
#       Personne.Identification.SEXE == lis_i$sexe_i &  
#       typo_agence == lis_i$typo_agence_i &  
#       #IDEPCI == epci_i &  
#       RSA_simple_corr_time_rang_CUT == lis_i$ANC_droit & 
#       RSA_simple_corr_CumSum_SinceLastNA_CUT == lis_i$ANC_continue #& 
#       #DonneesAdministratives.Adresse.AdresseDetailleeFrance.NUMCOM != lis_i$COMMUNE
#     ]
#   } else {
#     #print("here!")
#     matching_temoin <- eval(parse(text = EXPR_matching_temoin))
#     #print(lis_i$NIR_i)
#     #print(nrow(matching_temoin))
#     }
    
#     if (nrow(matching_temoin) < 1) { 
#             message("1ère tentative : échec")
#            if(!is.null(EXPR_matching_temoin_2) ){
#            message("2eme tentative : ") 
#            matching_temoin <- eval(parse(text = EXPR_matching_temoin_2))
#            }
#            if (nrow(matching_temoin) < 1) { 
#             message("Pas assez de correspondances trouvées pour l'individu avec les caractéristiques : ", 
#            paste( lis_i, collapse = " _ "))
#            NIR_manque <- c(NIR_manque, lis_i$NIR_i )
#             message(
#               paste0("Count : ", length(NIR_manque), " || ", round(length(NIR_manque)/i*100, 0), "%", " || ", round(length(NIR_manque)/nrow(data_test_val)*100, 0), "%")
#             )
#            } 
#     } else { 
      
#       matching_temoin <- matching_temoin |> 
#         dtplyr::lazy_dt() |> 
#         mutate(
#         RSA_present = !is.na(RSA_simple_corr),
#         TOTAL_NA = rowSums(across(.cols = everything(),.fns = ~ !is.na(.x)))) |>
#         group_by(NIR) |> 
#         arrange(date, desc(RSA_present), desc(TOTAL_NA), .by_group = TRUE) |> 
#         slice_head(n = 1) |> 
#         as.data.table()

#         if (nrow(matching_temoin) <= N_temoin) { 
#         Ns = nrow(matching_temoin)
#             } else {
#         Ns = N_temoin
#             }
      
#     Akeep_livar <- c(list_var, "DATE", "date")
#     samples[[i]] <- matching_temoin[sample(.N, size = Ns), ..Akeep_livar]
#     samples[[i]][  , TYPE := "TEMOIN"]
#     samples[[i]][  , NIR_TEST := unique(data_test_val$NIR[i])]
#     NIRS_l <- c(NIRS_l, samples[[i]]$NIR)
#     }
#     } 
#   #d_temoin <- bind_rows(samples) |> mutate(GROUPE = "temoin")
#   #d_test <- data_test |> mutate(GROUPE = "test")
#   #d <- rbind(d_test, d_temoin) 
#   # Ajuster le modèle de Cox 
#   #fit <- coxph(Surv(time, status) ~ GROUPE + AGE + SEXE + TERRITOIRE, data = d)  
#   # Retourner la liste des échantillons 
#   res <- list("samples" = samples, "NIR_manque"=NIR_manque)
#   return(res) 
# } 