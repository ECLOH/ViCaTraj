#' Corriger les interruptions courtes dans des trajectoires RSA
#'
#' Prépare une variable d'état corrigée à partir de trajectoires au format long.
#' Les valeurs de `var_rsa` égales à `missing` sont considérées comme des
#' périodes hors RSA. Après expansion des combinaisons d'identifiants et de
#' dates, les périodes hors RSA formant une séquence courte peuvent être
#' recodées en période RSA.
#'
#' La fonction crée une colonne temporaire `NEWVAR`, puis la renomme en
#' concaténant le nom de `var_rsa` et `suffixe_var`, sauf si
#' `suffixe_var = NULL`. Si `setNA = TRUE`, les périodes demeurant codées comme
#' hors RSA dans la variable corrigée sont remplacées par `NA`.
#'
#' Les lignes sans identifiant ou sans date sont écartées avant l'expansion.
#' La variable de date doit être de classe `Date` ou numérique. Les données
#' sont traitées au moyen de `dtplyr` et converties au format demandé par
#' `format_collect`.
#' 
#' @family dataclean
#' 
#' @param data Un tableau au format long contenant les variables d'identifiant,
#'   de date et d'état RSA.
#' @param format_collect Format de sortie. Valeurs prévues : `"tibble"` ou
#'   `"data.table"`. La fonction ne collecte pas le résultat pour une autre
#'   valeur.
#' @param var_rsa Variable contenant l'état RSA. Peut être fournie sans guillemets
#'   lorsqu'elle respecte la syntaxe d'un nom de variable R.
#' @param suffixe_var Suffixe ajouté au nom de la variable corrigée. Par défaut,
#'   `"_corr"`. Si `NULL`, la colonne temporaire conserve le nom `NEWVAR`.
#' @param var_ID Variable identifiant les individus.
#' @param var_date Variable contenant la date ou l'index temporel. Elle doit
#'   être de classe `Date` ou numérique.
#' @param missing Valeur ou valeurs de `var_rsa` considérées comme des périodes
#'   hors RSA. Par défaut, `"SUSPENDU"`.
#' @param state.gap Libellé utilisé en interne pour représenter une période hors
#'   RSA, y compris les valeurs manquantes créées par l'expansion.
#' @param state.r Libellé représentant une période RSA et utilisé pour
#'   initialiser les combinaisons d'identifiants et de dates.
#' @param nb.min.gap Seuil, en nombre de lignes consécutives, utilisé pour la
#'   correction des périodes hors RSA. Par défaut, `4`.
#' @param setNA Si `TRUE`, remplace par `NA` les valeurs de la variable corrigée
#'   qui demeurent égales à `state.gap`. Par défaut, `TRUE`.
#'
#' @return Un tableau au format demandé par `format_collect`, avec une variable
#'   corrigée nommée selon `var_rsa` et `suffixe_var`, sauf si
#'   `suffixe_var = NULL`. Les lignes sans identifiant ou sans date ont été
#'   retirées.
#'
#' @details
#' La fonction construit des séquences de valeurs constantes dans `NEWVAR`
#' pour chaque identifiant, après tri par date. Les périodes de longueur
#' inférieure ou égale à `nb.min.gap - 1` peuvent être recodées en `state.r`,
#' sous réserve de la condition supplémentaire appliquée aux groupes en début
#' de trajectoire.
#'
#' @examples
#' \dontrun{
#' df <- tibble::tibble(
#'   id = rep(1, 8),
#'   date = seq(as.Date("2023-01-01"), by = "month", length.out = 8),
#'   rsa = c(
#'     "RSA", "RSA", "SUSPENDU", "SUSPENDU",
#'     "RSA", "RSA", "SUSPENDU", "RSA"
#'   )
#' )
#'
#' func_delete_modgap(
#'   data = df,
#'   format_collect = "tibble",
#'   var_rsa = rsa,
#'   var_ID = id,
#'   var_date = date,
#'   state.r = "RSA"
#' )
#' }
#'
#' @seealso [tidyr::expand()], [dtplyr::lazy_dt()]
#' @importFrom dplyr across arrange case_when filter group_by left_join mutate
#'   pull rename_with ungroup
#' @importFrom dplyr %>%
#' @importFrom tidyr expand replace_na
#' @importFrom dtplyr lazy_dt
#' @export
func_delete_modgap <- function(
  data = t.seqw,
  format_collect = "data.table",
  var = RSA_simple,
  suffixe_var = "_corr",
  ID = Personne.Identification.NIR,
  date = date,
  missing = "SUSPENDU",
  state.gap = "Sortie / hors RSA",
  state.r = "RSA",
  nb.min.gap = 4,
  setNA = TRUE,
  ... 
) {
  require(dtplyr)
  var.chr <- deparse(substitute(var))
  ID.chr <- deparse(substitute(ID))
  date.chr <- deparse(substitute(date))
  
  print(var.chr)

  if (
    !(inherits(x = data |> pull({{ date }}), "numeric") |
      inherits(x = data |> pull({{ date }}), "Date"))
  ) {
    stop("var_date n'est pas dans un format triable sans ambiguité")
  }


# data <- expand.grid(
#   data |>  pull({{ ID }}) |>  unique(),
#   data |>  pull({{ date }}) |>  unique()
# ) |> 
#   setNames(c(ID.chr, date.chr)) |> 
#   left_join(
#       data
#     ) |>
#   data.table::as.data.table()
  

  res <- data |>
    func_expand(ID = {{ID}}, date = {{date}}, ... ) |> 
    dtplyr::lazy_dt() |>
    mutate(
      NEWVAR = case_when(
        {{ var }} %in% c(missing) ~ state.gap,
        TRUE ~ {{ var }}
      )
    ) |>
    mutate(across(NEWVAR, ~ replace_na(.x, state.gap))) |>
    group_by({{ ID }}) |>
    arrange({{ date }}, .by_group = TRUE) |>
    mutate(
      group = cumsum(
        NEWVAR != lag(NEWVAR, default = first(NEWVAR))
      )
    ) |>
    group_by(group, .add = TRUE) |>
    mutate(counter = row_number(), NB = max(counter)) |>
    mutate(across(
      NEWVAR,
      ~ if_else(
        .x == state.gap & NB <= (nb.min.gap - 1) & group > 0,
        state.r,
        .x
      )
    ))

  if (format_collect == "tibble") {
    res <- res |>
      as_tibble()
  } else {
    if (format_collect == "data.table") {
      res <- res |>
        as.data.table()
    } else {
      "res: les données ne sont pas collectées après le pipe dtplyr()"
    }
  }

  if (setNA) {
    res <- res |>
      mutate(across(
        NEWVAR,
        ~ if_else(
          .x == state.gap,
          NA_character_,
          .x
        )
      ))
  }

  if (!is.null(suffixe_var)) {
    res <- res |>
      dplyr::rename_with(
        .cols = NEWVAR,
        .fn = ~ paste0(var.chr, suffixe_var)
      )
  }

  return(res)
}
