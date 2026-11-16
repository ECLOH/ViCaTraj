#' Compléter les combinaisons d'identifiants et de pas de temps
#'
#' Génère une séquence de pas de temps entre les valeurs minimale et maximale
#' de `date`, puis crée toutes les combinaisons de cette séquence avec les
#' identifiants distincts de `data`. Les données d'origine sont ensuite
#' rattachées à la grille par une jointure.
#'
#' @param data Un data frame ou un tibble contenant les colonnes d'identifiant
#'   et de pas de temps.
#' @param ID Colonne contenant l'identifiant des individus. Peut être fournie
#'   sans guillemets.
#' @param date Colonne contenant les pas de temps. Elle doit être de classe
#'   `Date` ou numérique.
#' @param seq_by Pas transmis à `seq()` pour générer la séquence. Il doit être
#'   compatible avec la classe de `date`, par exemple `"1 month"` pour une
#'   colonne de classe `Date` ou `1` pour une colonne numérique.
#'
#' @return Un tibble contenant les combinaisons entre les identifiants
#'   distincts et les pas de temps générés, avec les colonnes de `data`
#'   rattachées par jointure. Les combinaisons sans observation dans `data`
#'   auront des valeurs manquantes dans les autres colonnes.
#'
#' @details
#' La fonction affiche un message indiquant la classe de `date` et rappelle
#' que `seq_by` doit être compatible avec celle-ci. Si `date` n'est ni de
#' classe `Date` ni numérique, la fonction renvoie une erreur.
#'
#' Les valeurs manquantes de `date` sont ignorées lors du calcul des bornes
#' de la séquence.
#'
#' @examples
#' \dontrun{
#' func_expand(donnees, ID, date)
#'
#' func_expand(donnees, ID, rang, seq_by = 1)
#' }
#'
#' @export

func_expand <- function(
  data,
  ID,
  date,
  seq_by = "1 month" # time lap compatible with seq.Date() or numeric, depending on {{date}} class
) {
  # date_chr  <- deparse(substitute(date))
  # ID_chr    <- deparse(substitute(ID))

  date_chr <- rlang::as_name(rlang::ensym(date))
  ID_chr <- rlang::as_name(rlang::ensym(ID))

  date_var <- data |> dplyr::pull({{ date }})

  if (
    inherits(
      date_var,
      "Date"
    )
  ) {
    message(paste0(
      date_chr,
      " est au format 'Date': 'seq_by' [",
      seq_by,
      "] doit être compatible"
    ))
  } else {
    if (
      inherits(
        date_var,
        "numeric"
      )
    ) {
      message(paste0(
        date_chr,
        " est au format 'numeric': 'seq_by' [",
        seq_by,
        "] doit être compatible"
      ))
    } else {
      stop(paste0(date_chr, " doit être au dormat 'Date' ou 'numeric'"))
    }
  }

  if(is.null(seq_by)){
    seq_by <- func_detect_TimeStep(dates = date_var)
      message(paste0(
        "'seq_by = NULL' : le pas de temps est calculé => ", seq_by
      ))
  }


  pastemps <- seq(
    min(date_var, na.rm = TRUE),
    max(date_var, na.rm = TRUE),
    by = seq_by
  )

  tidyr::expand_grid(
    pastemps,
    unique(data |> pull({{ ID }}))
  ) |>
    setNames(nm = c(date_chr, ID_chr)) |>
    dplyr::left_join(
      data |> ungroup()
    )
}
