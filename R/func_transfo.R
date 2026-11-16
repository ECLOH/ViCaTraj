#' Transformer les données administratives et RSA
#'
#' Ajoute des variables dérivées au tableau fourni, notamment le nombre et la
#' catégorie d'enfants à charge, des âges calculés à partir des dates de
#' naissance, une situation familiale synthétique, l'organisme d'origine et des
#' variables relatives au statut RSA.
#'
#' La fonction effectue également des jointures avec les tables de situation
#' familiale et de modalités RSA. La table de situation familiale doit être
#' disponible via l'argument `SITFAM`. La table de correspondance des modalités
#' RSA est récupérée dans le package `ViCaTraj`.
#'
#' Les packages `ViCaTraj`, `dplyr` et `lubridate` doivent être installés.
#' La fonction `create_RSA_simple()` doit également être disponible dans
#' l'environnement d'exécution ou dans le package approprié.
#'
#' @param df Un data.frame ou un data.table contenant les variables nécessaires
#'   aux transformations. Il doit notamment inclure les colonnes relatives au
#'   nombre d'enfants à charge, à la situation familiale, aux dates, au fichier
#'   source et au dossier RSA utilisées dans le corps de la fonction.
#' @param SITFAM Table de correspondance des situations familiales. Elle doit
#'   contenir les colonnes `CODE_SITFAM` et `LABEL_SITFAM`. Par défaut, utilise
#'   l'objet `sitfam`.
#'
#' @return Le tableau `df`, enrichi de variables calculées et de colonnes
#'   issues des jointures. Les ajouts comprennent notamment `NBENFCHA`,
#'   `NBENFCHA_cut`, `DTNAI_DEM`, `DTNAI_CJT`, les variables d'âge associées,
#'   `SITFAM`, `Global_fam`, `INSTITUTION` et la variable de statut RSA créée
#'   par `create_RSA_simple()`.
#'
#' @family {database}
#' @author Elie
#' @export

func_transfo <- function(df, SITFAM = sitfam) {
  require(ViCaTraj)
  require(dplyr)
  require(lubridate)
  df$NBENFCHA <- as.numeric(
    df$PrestationRSA.DetailDroitRSA.TroncCommunDroitRSA.NBENFAUTCHA
  )
  df$NBENFCHA_cut <- cut(
    df$NBENFCHA,
    breaks = c(0, 0.5, 1, 2, 3, 100),
    include.lowest = T,
    labels = c(
      "Sans enfant",
      "1 enfant",
      "2 enfants",
      "3 enfants",
      "4 enfants ou +"
    )
  )
  df$NBENFCHA_cut[is.na(df$NBENFCHA_cut)] <- "Sans enfant"
  #
  df <- df |>
    left_join(
      SITFAM,
      by = c("DonneesAdministratives.SituationFamille.SITFAM" = "CODE_SITFAM")
    ) |>
    mutate(
      DTNAI_DEM = as.Date(
        Personne_DEM.Identification.DTNAI,
        format = "%Y-%m-%d"
      ),
      DTNAI_CJT = as.Date(
        Personne_CJT.Identification.DTNAI,
        format = "%Y-%m-%d"
      ),
      date = func_asDate_datalist_fast(
        DATE |>
          gsub(pattern = "D_", replacement = "") |>
          gsub(pattern = "_", replacement = "-")
      )) |> 
    mutate(across(c(DTNAI_DEM, DTNAI_CJT), ~ as.numeric(date - .x)/365, .names = "{.col}_AGE")) |> 
    mutate(across(c(DTNAI_DEM_AGE, DTNAI_CJT_AGE), ~ cut(.x, breaks = c(0, 25, 35, 55, 65, 100)), .names = "{.col}_c")) |>
    mutate(
      SITFAM = case_when(
        LABEL_SITFAM %in%
          c(
            "DISPARU (JUGEMENT D'ABSENCE)",
            "CELIBATAIRE",
            "DIVORCE",
            "ISOLEMENT APRES VIE MARITALE OU PACS",
            "SEPARATION DE FAIT",
            "SEPARATION LEGALE"
          ) ~ "separe",
        LABEL_SITFAM %in%
          c(
            "MARIAGE",
            "PACS",
            "VIE MARITALE",
            "REPRISE VIE COMMUNE SUR PACS",
            "REPRISE VIE MARITALE",
            "REPRISE MARIAGE"
          ) ~ "couple",
        TRUE ~ LABEL_SITFAM
      ),
      Global_fam = case_when(
        SITFAM == "separe" &
          NBENFCHA_cut == "Sans enfant" ~ "Celib. sans enfant",
        SITFAM == "separe" &
          NBENFCHA_cut != "Sans enfant" ~ "Celib. avec enfants",
        SITFAM == "couple" &
          NBENFCHA_cut != "Sans enfant" ~ "Couple avec enfants",
        SITFAM == "couple" &
          NBENFCHA_cut == "Sans enfant" ~ "Couple sans enfants",
        SITFAM == "VEUVAGE" &
          NBENFCHA_cut != "Sans enfant" ~ "Veuvage avec enfants",
        SITFAM == "VEUVAGE" &
          NBENFCHA_cut == "Sans enfant" ~ "Veuvage sans enfants"
      ),
    ) |>
    mutate(
      INSTITUTION = if_else(
        grepl("RSABEM", FICHIER_SOURCE, fixed = TRUE),
        "CAF",
        if_else(grepl("MSABEM", FICHIER_SOURCE, fixed = TRUE), "MSA", "")
      )
    ) |>
    mutate(
      RSA_simple = create_RSA_simple(
        PrestationRSA.SituationDossierRSA.EtatDossierRSA.ETATDOSRSA
      )
    ) |>
    left_join(
      ViCaTraj::df.RSA.value,
      by = c(
        "PrestationRSA.SituationDossierRSA.EtatDossierRSA.ETATDOSRSA" = "MODALITE"
      )
    )

  return(df)
}
