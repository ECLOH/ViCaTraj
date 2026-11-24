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
func_sample_id <- function(data_test_val, DT_temoin = data_temoin, list_var = NULL, EXPR_matching_temoin = NULL, EXPR_matching_temoin_2 = NULL, N_temoin=10) { 
  # Liste pour stocker les échantillons 
  samples <- list() 
  NIRS_l <- c()
  NIR_manque <- c()
  # Pour chaque individu du groupe TEST 
  for (i in 1:nrow(data_test_val)) { 

    # print(i)
    # Caractéristiques de l'individu 

    if(is.null(list_var)){
      list_var <- c("NIR", "Q", "RSA_simple_corr", "AGE", "Personne.Identification.SEXE", "Global_fam", "typo_agence", "IDEPCI", "DonneesAdministratives.Adresse.AdresseDetailleeFrance.NUMCOM", 
      "RSA_simple_corr_time_rang_CUT", "RSA_simple_corr_CumSum_SinceLastNA_CUT")
    }

    lis_i <- lapply(list_var, function(ivar){
              res <- data_test_val[i , ..ivar] |> unlist(use.names = F)
              if(inherits(res, "factor")){
                res <- as.character(res)
              }
      return(res)
    })
    names(lis_i) <- paste0(list_var, "_i")
    #print(lis_i)
    # lis_i <- list(
    #   "NIR_i" =        data_test_val$NIR[i],
    #   "Q_i" =          data_test_val$Q[i],
    #   "RSA_i"=         data_test_val$RSA_simple_corr[i],
    #   "age_i" =        data_test_val$AGE[i],
    #   "sexe_i" =       data_test_val$Personne.Identification.SEXE[i],
    #   "Global_fam_i" = data_test_val$Global_fam[i],
    #   "typo_agence_i"= data_test_val$typo_agence[i],
    #   "epci_i" =       data_test_val$IDEPCI[i],
    #   "ANC_droit" =    data_test_val$RSA_simple_corr_time_rang_CUT[i],
    #   "ANC_continue" = data_test_val$RSA_simple_corr_CumSum_SinceLastNA_CUT[i],
    #   "COMMUNE" =      data_test_val$DonneesAdministratives.Adresse.AdresseDetailleeFrance.NUMCOM[i]
    #   )
    # Filtrer le groupe TEMOIN avec les mêmes caractéristiques 
    if(is.null(EXPR_matching_temoin)){
    matching_temoin <- DT_temoin[
      NIR != lis_i$NIR_i & !(NIR %in% NIRS_l) &
      RSA_simple_corr == lis_i$RSA_i &
      Q==lis_i$Q_i & 
      (AGE > (lis_i$age_i-2.5) & AGE < (lis_i$age_i+2.5))  &  
      Personne.Identification.SEXE == lis_i$sexe_i &  
      typo_agence == lis_i$typo_agence_i &  
      #IDEPCI == epci_i &  
      RSA_simple_corr_time_rang_CUT == lis_i$ANC_droit & 
      RSA_simple_corr_CumSum_SinceLastNA_CUT == lis_i$ANC_continue #& 
      #DonneesAdministratives.Adresse.AdresseDetailleeFrance.NUMCOM != lis_i$COMMUNE
    ]
  } else {
    #print("here!")
    matching_temoin <- eval(parse(text = EXPR_matching_temoin))
    #print(lis_i$NIR_i)
    #print(nrow(matching_temoin))
    }
    
    if (nrow(matching_temoin) < 1) { 
            message("1ère tentative : échec")
           if(!is.null(EXPR_matching_temoin_2) ){
           message("2eme tentative : ") 
           matching_temoin <- eval(parse(text = EXPR_matching_temoin_2))
           }
           if (nrow(matching_temoin) < 1) { 
            message("Pas assez de correspondances trouvées pour l'individu avec les caractéristiques : ", 
           paste( lis_i, collapse = " _ "))
           NIR_manque <- c(NIR_manque, lis_i$NIR_i )
            message(
              paste0("Count : ", length(NIR_manque), " || ", round(length(NIR_manque)/i*100, 0), "%", " || ", round(length(NIR_manque)/nrow(data_test_val)*100, 0), "%")
            )
           } 
    } else { 
      
      matching_temoin <- matching_temoin |> 
        dtplyr::lazy_dt() |> 
        mutate(
        RSA_present = !is.na(RSA_simple_corr),
        TOTAL_NA = rowSums(across(.cols = everything(),.fns = ~ !is.na(.x)))) |>
        group_by(NIR) |> 
        arrange(date, desc(RSA_present), desc(TOTAL_NA), .by_group = TRUE) |> 
        slice_head(n = 1) |> 
        as.data.table()

        if (nrow(matching_temoin) <= N_temoin) { 
        Ns = nrow(matching_temoin)
            } else {
        Ns = N_temoin
            }
      
    Akeep_livar <- c(list_var, "DATE", "date")
    samples[[i]] <- matching_temoin[sample(.N, size = Ns), ..Akeep_livar]
    samples[[i]][  , TYPE := "TEMOIN"]
    samples[[i]][  , NIR_TEST := unique(data_test_val$NIR[i])]
    NIRS_l <- c(NIRS_l, samples[[i]]$NIR)
    }
    } 
  #d_temoin <- bind_rows(samples) |> mutate(GROUPE = "temoin")
  #d_test <- data_test |> mutate(GROUPE = "test")
  #d <- rbind(d_test, d_temoin) 
  # Ajuster le modèle de Cox 
  #fit <- coxph(Surv(time, status) ~ GROUPE + AGE + SEXE + TERRITOIRE, data = d)  
  # Retourner la liste des échantillons 
  res <- list("samples" = samples, "NIR_manque"=NIR_manque)
  return(res) 
} 


func_DTtemoin <- function(data_test_val, DT_temoin = data_temoin){

  samples <- list() 
  NIR_manque <- c()
  # Pour chaque individu du groupe TEST 
  sample_li_sel <- lapply(1:nrow(data_test_val), function(i){ 
    #print(i)
    # Caractéristiques de l'individu 
    lis_i <- list(
    "NIR_i" =        data_test_val$NIR[i],
    "Q_i" =          data_test_val$Q[i],
    "RSA_i"=         data_test_val$RSA_simple_corr[i],
    "age_i" =        data_test_val$AGE[i],
    "sexe_i" =       data_test_val$Personne.Identification.SEXE[i],
    "Global_fam_i" = data_test_val$Global_fam[i],
    "typo_agence_i"= data_test_val$typo_agence[i],
    "epci_i" =       data_test_val$IDEPCI[i],
    "ANC_droit" =    data_test_val$RSA_simple_corr_time_rang_CUT[i],
    "ANC_continue" = data_test_val$RSA_simple_corr_CumSum_SinceLastNA_CUT[i],
    "COMMUNE" =      data_test_val$DonneesAdministratives.Adresse.AdresseDetailleeFrance.NUMCOM[i]
    )

    # Filtrer le groupe TEMOIN avec les mêmes caractéristiques 
    matching_temoin <- DT_temoin[
      NIR != lis_i$NIR_i &
      RSA_simple_corr == lis_i$RSA_i &
      Q==lis_i$Q_i & 
      (AGE > (lis_i$age_i-2.5) & AGE < (lis_i$age_i+2.5))  &  
      Personne.Identification.SEXE == lis_i$sexe_i &  
      typo_agence == lis_i$typo_agence_i &  
      #IDEPCI == epci_i &  
      RSA_simple_corr_time_rang_CUT == lis_i$ANC_droit & 
      RSA_simple_corr_CumSum_SinceLastNA_CUT == lis_i$ANC_continue #& 
      #DonneesAdministratives.Adresse.AdresseDetailleeFrance.NUMCOM != lis_i$COMMUNE
    ]
    
    if (nrow(matching_temoin) < 1) { 
           message("Pas assez de correspondances trouvées pour l'individu avec les caractéristiques : ", 
           paste( lis_i, collapse = " _ ") )
           NIR_manque <- c(NIR_manque, lis_i$NIR_i )
    } else { 
      matching_temoin[  , TYPE := "TEMOIN"]
      matching_temoin[  , NIR_TEST := unique(data_test_val$NIR[i])]
      return(matching_temoin)
    }
  })

  res_df <- data.table::rbindlist(sample_li_sel)
  res <- list("NIR_manque"=NIR_manque, "NIR_sel" = res_df)
  return(res)
}
