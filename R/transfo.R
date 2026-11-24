#' @author Elie..
#' @title La fonction "transfo"
#' @description  applique un certain nombre de transformation au df à ajouter, dont `INSTITUTION`.
#' ATTENTION : nécéssite de charger sitfam et natress au préalable.
#' @return df avec les nouvelles variables et recodages
#' @export
transfo <- function(df, SITFAM=sitfam, NATRESS=natress){
    require(ViCaTraj)
    require(dplyr)
    require(lubridate)
    df$NBENFCHA<-as.numeric(df$PrestationRSA.DetailDroitRSA.TroncCommunDroitRSA.NBENFAUTCHA)
    df$NBENFCHA_cut<-cut(df$NBENFCHA, breaks = c(0, 0.5, 1, 2, 3, 100), include.lowest = T, labels = c("Sans enfant", "1 enfant", "2 enfants", "3 enfants", "4 enfants ou +"))
    df$NBENFCHA_cut[is.na(df$NBENFCHA_cut)]<-"Sans enfant"
    
    df<-df |>
      left_join(sitfam, by=c("DonneesAdministratives.SituationFamille.SITFAM"="CODE_SITFAM"))|>
      left_join(natress, by=c("Personne.Ressources.RessourcesMensuelles.DetailRessourcesMensuelles.NATRESS"="CODE_NATRESS"))|>
      mutate(
        DTNAI = as.Date(Personne.Identification.DTNAI, format="%Y-%m-%d"), 
        date = as.Date_datalist(DATE|>gsub(pattern = "D_", replacement = "")|>gsub(pattern = "_",replacement = "-")),
        AGE = as.numeric(date-DTNAI)/365,
        AGE_c = cut(AGE, breaks = c(0, 25, 35, 55, 65, 100)),
        SITFAM = case_when(LABEL_SITFAM%in%c("DISPARU (JUGEMENT D'ABSENCE)", "CELIBATAIRE", "DIVORCE", "ISOLEMENT APRES VIE MARITALE OU PACS", "SEPARATION DE FAIT", "SEPARATION LEGALE")~"separe",
                           LABEL_SITFAM%in%c("MARIAGE", "PACS", "VIE MARITALE",
                                             "REPRISE VIE COMMUNE SUR PACS", "REPRISE VIE MARITALE", "REPRISE MARIAGE")~"couple", 
                           TRUE~LABEL_SITFAM),
        Global_fam = case_when(SITFAM=="separe"&NBENFCHA_cut=="Sans enfant"~"Celib. sans enfant", 
                               SITFAM=="separe"&NBENFCHA_cut!="Sans enfant"~"Celib. avec enfants", 
                               SITFAM=="couple"&NBENFCHA_cut!="Sans enfant"~"Couple avec enfants", 
                               SITFAM=="couple"&NBENFCHA_cut=="Sans enfant"~"Couple sans enfants",
                               SITFAM=="VEUVAGE"&NBENFCHA_cut!="Sans enfant"~"Veuvage avec enfants", 
                               SITFAM=="VEUVAGE"&NBENFCHA_cut=="Sans enfant"~"Veuvage sans enfants"),
        NATRESS = case_when(
          LABEL_NATRESS=="Allocations de chomage"~"Chômage",
          LABEL_NATRESS%in%c("Autres IJSS", "Chomage partiel (technique)", "Indemnités mater/pater/adoption")~"Autres revenus de remplacement",
          LABEL_NATRESS%in%c("Revenus d'activité salariée", "Revenus d'activité salariée (exep)", "Rémunération stage / formation")~"Salaires",
          LABEL_NATRESS%in%c("Revenu d'activité évalué prof non salariée", "Revenus d'activité des professions non salariées")~"Revenus d'act. non salariée",
          LABEL_NATRESS%in%c("Revenus fonciers et immobiliers", "Argent placé")~"Rentes",
          LABEL_NATRESS%in%c("Pré-retraite", "Pension vieillesse imposable")~"Retraite/vieillesse",
          LABEL_NATRESS%in%c("Rente AT à titre personnel", "Pension d'invalidité")~"Invalidité ou équivalent",
          LABEL_NATRESS%in%c("Autres revenus imposables", "Allocation de veuvage", "Prime et revenus exceptionnels")~"Autres ressources",
          TRUE ~ LABEL_NATRESS) 
      ) |> 
      mutate(INSTITUTION = if_else(grepl("RSABEM", FICHIER_SOURCE, fixed = TRUE), "CAF", 
                                                if_else(grepl("MSABEM", FICHIER_SOURCE, fixed=TRUE), "MSA", ""))
      ) |> 
      mutate(RSA_simple = create_RSA_simple(PrestationRSA.SituationDossierRSA.EtatDossierRSA.ETATDOSRSA)) |> 
      left_join(ViCaTraj::df.RSA.value, 
                by = c("PrestationRSA.SituationDossierRSA.EtatDossierRSA.ETATDOSRSA"="MODALITE"))
    
    return(df)
}