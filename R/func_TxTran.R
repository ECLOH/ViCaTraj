#' Calcule les fréquences et proportions de transitions
#'
#' Calcule les transitions de `var` vers sa valeur observée plus tard pour
#' chaque individu, puis résume les comptages et proportions de transitions.
#'
#' Les transitions sont construites après tri des observations par `ID` et par
#' le rang global. Le paramètre `time_step` indique combien de lignes
#' observées à décaler à l’intérieur de chaque individu ; il ne garantit pas que
#' la destination corresponde exactement à `time_step` rangs globaux plus tard.
#'
#' On peut utiliser soit `full_rank`, soit `date` pour définir l’ordre global.
#' Si `full_rank` n’est pas fourni, les rangs sont calculés avec
#' [dplyr::dense_rank()] à partir des valeurs de `date`. Cela ne crée pas de
#' rangs pour les dates absentes de `data`.
#'
#' @param data Un data frame, tibble ou table compatible avec `dplyr` contenant
#'   l’identifiant individuel, l’état et les colonnes d’ordre.
#' @param ID Nom non cité de la colonne identifiant les individus.
#' @param var Nom non cité de la colonne contenant l’état ou la valeur dont on
#'   veut compter les transitions.
#' @param date Nom non cité d’une colonne de date ou d’ordre. Utilisée pour
#'   calculer les rangs globaux lorsque `full_rank` n’est pas fourni. Peut être
#'   `NULL` lorsque `full_rank` est fourni.
#' @param full_rank Nom non cité de la colonne contenant les rangs globaux.
#'   Peut être `NULL`, dans ce cas les rangs sont calculés à partir de `date`.
#' @param time_step Nombre de lignes suivantes à utiliser comme destination pour
#'   chaque individu. Par défaut : `1`.
#' @param time_varying Si `TRUE`, calcule les proportions de transition
#'   séparément pour chaque rang global et état d’origine. Si `FALSE`,
#'   regroupe les transitions entre rangs. Par défaut : `FALSE`.
#'
#' @return Un tibble avec une ligne par transition origine-destination (et, si
#'   `time_varying = TRUE`, par rang global). Le résultat comprend :
#'   \describe{
#'     \item{n}{Nombre de transitions observées.}
#'     \item{n_i}{Nombre de individus distincts ayant réalisé la transition.}
#'     \item{prop_n}{Proportion de transitions depuis l’état d’origine
#'       (et le rang, si `time_varying = TRUE`) vers l’état de destination.}
#'     \item{prop_i}{Proportion d’individus avec l’état d’origine
#'       (et le rang, si `time_varying = TRUE`) qui réalisent la transition.}
#'   }
#'
#' @seealso [dplyr::dense_rank()], [data.table::shift()]
#'
#' @export
func_txtran <- function(
  data,
  ID,
  var,
  date,
  full_rank,
  time_step = 1,
  time_varying = FALSE
) {
  message(
    "⚠️ full_rank is not the same as individual ranks.\n
    It must be calculated from the complete set of possible time points, including those missing from the dataset.\n
    See ``ViCaTraj::func_delete_modgap()`` and ``ViCaTraj::func_expand()``."
  )
  library(dplyr)
  library(tidyr)

  date_q <- rlang::enquo(date)
  rank_q <- rlang::enquo(full_rank)

  has_arg <- function(q) {
    !rlang::quo_is_missing(q) && !rlang::quo_is_null(q)
  }

  if (has_arg(date_q) && !has_arg(rank_q)) {
    message(
      "⚠️ ``date`` has been supplied: calculating global ranks: \n
       `data |> ungroup() |> dplyr::dense_rank({{date}})`"
    )

    rank_name <- tail(
      make.unique(c(names(data), ".d_rank")),
      1L
    )

    data <- data |>
      dplyr::ungroup() |>
      dplyr::mutate(
        !!rank_name := dplyr::dense_rank(!!date_q)
      )

    print(rank_name)
    rank_q <- rlang::sym(rank_name)
  } else if (has_arg(rank_q)) {
    message("⚠️ ``full_rank`` has been supplied: no recalculation.")
  } else {
    stop("``date`` or ``full_rank`` must be supplied.")
  }

  res <- data |>
    #dtplyr::lazy_dt() |>
    dplyr::arrange({{ ID }}, !!rank_q) |>
    dplyr::group_by({{ ID }}) |>
    dplyr::mutate(
      next_step = data.table::shift({{ var }}, type = "lead", n = time_step),
      next_rank = data.table::shift(
        !!rank_q,
        type = "lead",
        n = time_step
      )
    ) |>
    dplyr::ungroup() |> 
    dplyr::filter(!is.na(next_step)) |>
    as_tibble()

  if (time_varying) {
        res_n <- res |>
      group_by(
    !!rank_q,
        {{ var }},
        next_step,
      ) |>
      dplyr::summarise(n = n(), n_i = n_distinct({{ ID }})) |>
      dplyr::group_by(!!rank_q, {{ var }}) |>
      dplyr::mutate(prop_n = n / sum(n)) |>
      ungroup() |>
      left_join(
res |>
       dplyr::filter(!is.na(next_step)) |>
       dplyr::group_by(
      !!rank_q,
         {{ var }}
       ) |>
       dplyr::summarise(n_i.total = n_distinct({{ ID }}))
      ) |> 
       dplyr::mutate(prop_i = n_i / n_i.total) |>
       dplyr::ungroup() |> 
       suppressMessages()
  } else {

    res_n <- res |>
      group_by(
        {{ var }},
        next_step,
      ) |>
      dplyr::summarise(n = n(), n_i = n_distinct({{ ID }})) |>
      dplyr::group_by({{ var }}) |>
      dplyr::mutate(prop_n = n / sum(n)) |>
      ungroup() |>
      left_join(
res |>
       dplyr::filter(!is.na(next_step)) |>
       dplyr::group_by(
         {{ var }}
       ) |>
       dplyr::summarise(n_i.total = n_distinct({{ ID }}))
      ) |> 
       dplyr::mutate(prop_i = n_i / n_i.total) |>
       dplyr::ungroup() |> 
       suppressMessages()
  }
}