#' Recoder les événements et construire les périodes de suivi
#'
#' Prépare les données longitudinales d'individus en ajoutant, si nécessaire,
#' une ligne de censure et en recodant les états d'événement en états de sortie
#' ou de censure.
#'
#' Les arguments désignant des colonnes (`time_col`, `indiv_col`, `start_col`,
#' `end_col` et `event_col`) s'écrivent sans guillemets. Si `start_col` ou
#' `end_col` vaut explicitement `NULL`, la fonction calcule la borne
#' correspondante par individu à partir des temps associés à une valeur non
#' manquante de `event_col`.
#'
#' @param data Un tableau contenant les observations longitudinales.
#' @param time_col Colonne temporelle, saisie sans guillemets. Par défaut,
#'   `ANNEE`.
#' @param indiv_col Colonne identifiant les individus, saisie sans guillemets.
#'   Par défaut, `INDIV`.
#' @param start_col Colonne contenant le début de la période, saisie sans
#'   guillemets. Par défaut, `START_annee`. Si `NULL` est fourni explicitement,
#'   la fonction crée `created_start`, égal au premier temps observé pour
#'   chaque individu parmi les lignes où `event_col` n'est pas manquant.
#' @param end_col Colonne contenant la fin de la période, saisie sans
#'   guillemets. Par défaut, `END_annee`. Si `NULL` est fourni explicitement,
#'   la fonction crée `created_end`, égal au dernier temps observé pour
#'   chaque individu parmi les lignes où `event_col` n'est pas manquant.
#' @param event_col Colonne contenant l'état ou l'événement, saisie sans
#'   guillemets. Par défaut, `TYPEGIR`.
#' @param start_time Temps de début global du suivi. Si `NULL`, utilise le
#'   minimum de `time_col`.
#' @param censor_time Temps de censure. Si `NULL`, le détermine à partir des
#'   valeurs de `time_col` : pour les dates, utilise le pas temporel détecté
#'   lorsqu'il est disponible; sinon, utilise le pas numérique suivant.
#' @param clean_na Booléen obligatoire. Si `TRUE`, supprime les lignes où
#'   `event_col` est manquant. Si `FALSE`, les conserve et recalcule la borne
#'   de fin à partir des temps observés.
#'
#' @return Un tableau contenant les observations recodées, notamment les
#'   colonnes `RANK`, `EVENTS_STATES`, `period_SORTIE` et `FALSE_censor`,
#'   ainsi que les colonnes de début et de fin utilisées.
#'
#' @examples
#' \dontrun{
#' func_recode_event(
#'   data,
#'   time_col = ANNEE,
#'   indiv_col = INDIV,
#'   start_col = START_annee,
#'   end_col = END_annee,
#'   event_col = TYPEGIR,
#'   clean_na = TRUE
#' )
#'
#' # Demander le calcul automatique des bornes de début et de fin
#' func_recode_event(
#'   data,
#'   start_col = NULL,
#'   end_col = NULL,
#'   clean_na = TRUE
#' )
#' }
#'
#' @export
func_recode_event <- function(
  data,
  time_col = ANNEE, # Nom de la colonne temporelle
  indiv_col = INDIV, # Nom de la colonne individu
  start_col = START_annee, # Colonne de début
  end_col = END_annee, # Colonne de fin
  event_col = TYPEGIR, # Colonne événement
  start_time = NULL, # Valeur de départ (ex: 2014)
  censor_time = NULL, # Valeur de censure (ex: 2026)
  clean_na # pas de valeurs par défaut
) {
  require(dplyr)
  require(tidyr)
  require(dtplyr)


 start_quo <- rlang::enquo(start_col)

  if (rlang::quo_is_null(start_quo)) {
        message("start_col is NULL : created with `min(time_col[(!is.na(event_col))]), na.rm = TRUE)`")
    data <- data |> 
      group_by({{indiv_col}}) |> 
      mutate(created_start = min({{time_col}}[(!is.na({{event_col}}))], na.rm = TRUE)) |> 
      mutate(created_start = case_when(
        is.infinite(created_start) ~ NA, 
        TRUE ~ created_start
      )) 
    start_quo <- rlang::quo(created_start)
  }

 end_quo <- rlang::enquo(end_col)
  if (rlang::quo_is_null(end_quo)) {
    message("end_col is NULL : created with `max(time_col[(!is.na(event_col))]), na.rm = TRUE)`")
    data <- data |> 
      group_by({{indiv_col}}) |> 
      mutate(created_end = max({{time_col}}[(!is.na({{event_col}}))], na.rm = TRUE)) |> 
      mutate(created_end = case_when(
        is.infinite(created_end) ~ NA, 
        TRUE ~ created_end
      )) 
    end_quo <- rlang::quo(created_end)
  }

   # Vérifier que clean_na est fourni
  if (missing(clean_na)) {
    stop(
      "Parameter 'clean_na' must be specified (TRUE or FALSE). ",
      "Set clean_na = TRUE to remove rows with NA in event_col (NA is not a state value), ",
      "or clean_na = FALSE to keep them (NA is considered as a state value)."
    )
  }
  # Nettoyer les NA si demandé
  if (clean_na) {
    message("⚠️ clean_na = TRUE => remove rows with NA in event_col (NA is not a state value)")
    n_before <- nrow(data)
    data <- data |> filter(!is.na({{ event_col }}))
    n_after <- nrow(data)
    
    if (n_before != n_after) {
      message(
        "ℹ Removed ", n_before - n_after, " rows with NA in '", 
        deparse(substitute(event_col)), "' (clean_na = TRUE)"
      )
    }
  } else {
  message("⚠️ clean_na = F => keep NA and recalculate end_col based on last observed value")
    data <- data |> 
      group_by({{ indiv_col }}) |> 
      mutate(!!end_quo := pmax(
  max({{ time_col }}[!is.na({{ event_col }})], na.rm = TRUE),
  min({{ time_col }}, na.rm = TRUE),  # minimum si aucune obs
  na.rm = TRUE
))
  }


  # Détection automatique du type
  time_vals <- data |> pull({{ time_col }})

  # Si start_time/censor_time sont NULL, on prend min/max
  if (is.null(start_time)) {
    start_time <- min(time_vals, na.rm = TRUE)
  }
  if (is.null(censor_time)) {
    if (inherits(time_vals, "Date")) {
      step_type <- ViCaTraj::func_detect_TimeStep(time_vals)
      if (step_type != "other") {
        censor_time <- seq.Date(
          max(time_vals, na.rm = TRUE),
          by = step_type,
          length.out = 2
        )[2]
      } else {
        # fallback : +1 jour
        censor_time <- max(time_vals, na.rm = TRUE) + 1
      }
    } else {
      # numérique (comme avant)
      sorted_time <- sort(unique(time_vals))
      if (length(sorted_time) > 1) {
        step_time <- min(diff(sorted_time))
      } else {
        step_time <- 1
      }
      censor_time <- max(time_vals, na.rm = TRUE) + step_time
    }
  }

  # Ajout de la ligne de censure pour chaque individu
  censor_df <- tibble(
    "{{indiv_col}}" := unique(data |> pull({{ indiv_col }})),
    "{{time_col}}" := censor_time
  )

  data_surv <- data |>
    ungroup() |>
    filter(!!start_quo >= start_time) |>
    bind_rows(
      censor_df
    ) |>
    tidyr::expand({{ indiv_col }}, {{ time_col }}) |>
    left_join(
      data |>
        ungroup() |>
        filter(!!start_quo >= start_time) |>
        distinct({{ indiv_col }}, {{ time_col }}, .keep_all = T) |>
        select(
          {{ indiv_col }},
          {{ time_col }},
          {{ event_col }},
          !!start_quo,
          !!end_quo
        )
    ) |>
    dtplyr::lazy_dt() |>
    select(
      {{ indiv_col }},
      {{ time_col }},
      {{ event_col }},
      !!start_quo,
      !!end_quo
    ) |>
    group_by({{ indiv_col }}) |>
    filter({{ time_col }} >= min(!!start_quo, na.rm = TRUE)) |>
    arrange({{ time_col }}, .by_group = T) |>
    mutate(RANK = row_number()) |>
    mutate(
      EVENTS_STATES = case_when(
        ({{ time_col }} > max(!!end_quo, na.rm = TRUE) &
          {{ time_col }} < censor_time) ~ "SORTIE",
          (clean_na == FALSE &  is.na({{ event_col }}) & {{ time_col }} < censor_time) ~ "SORTIE temporaire",
        ({{ time_col }} > max(!!end_quo, na.rm = TRUE) &
          {{ time_col }} == censor_time) ~ "CENSURE",
        TRUE ~ {{ event_col }}
      )
    ) |>
    mutate(
      start_SORTIE = tidyr::replace_na(
        grepl("SORTIE", EVENTS_STATES) & !grepl("SORTIE", lag(EVENTS_STATES)), # lag(EVENTS_STATES) != "SORTIE",
        FALSE
      )
    ) |>
    mutate(period_SORTIE = cumsum(start_SORTIE)) |>
    mutate(
      FALSE_censor = lag(EVENTS_STATES) == "SORTIE" &
        EVENTS_STATES == "CENSURE"
    ) |>
    ungroup() |>
    filter(!(FALSE_censor & EVENTS_STATES == "CENSURE")) |>
    collect() |>
    ungroup() |>
    select(
      {{ indiv_col }},
      {{ time_col }},
      RANK,
      {{ event_col }},
      EVENTS_STATES,
      period_SORTIE,
      FALSE_censor,
      !!start_quo,
      !!end_quo
    )
  
added_states <- unique(data_surv$EVENTS_STATES)[!unique(data_surv$EVENTS_STATES)%in% unique( data |> pull({{event_col}}))]
  message(paste0("ADDED STATES : ", paste0(added_states, collapse = "|")))
  return(data_surv)
}

#' Génère une table d'événements pour l'analyse de survie 
#' 
#' @description 
#' Cette fonction extrait, pour chaque individu, l'événement de sortie ou de censure final à partir d'un data.frame au format long contenant un variable d'état contenant 
#' les infos de SORTIE et CENSURE (généré par ViCaTraj::func_recode_event()) . 
#' Elle génère les variable time et event pour la création d'un objet de survie (par exemple avec la fonction `Surv`). 
#' 
#' @param data Un data.frame ou tibble contenant les données longitudinales. 
#' @param indiv_col Nom de la colonne identifiant l'individu. 
#' @param rank_col Nom de la colonne indiquant le rang ou la durée de suivi. 
#' @param events_var Nom de la colonne indiquant le type d'événement. 
#' @param events_look Valeurs des événements considérés comme "sortie" (par défaut : `"SORTIE"`). 
#' @param censure_mod Valeur de l'événement considéré comme "censure" (par défaut : `"CENSURE"`). 
#' @param which_event "first" ou "last"
#' 
#' @return Un tibble avec une ligne par individu, contenant les colonnes : identifiant, temps, et événement (TRUE = sortie, FALSE = censure). 
#' 
#' @seealso [ViCaTraj::func_recode_event()] pour la création d'un tibble avant TIME et EVENT (pour Surv())
#' @seealso [Surv()] pour la création d'un objet de survie.
#' @family survie
#' @export
func_longevent <- function(
  data,
  indiv_col,
  rank_col,
  events_var,
  events_look = c("SORTIE"),
  censure_mod = "CENSURE",
  which_event  # "first" ou "last"
) {

data <- data |> 
  group_by({{ indiv_col }}) |> # pour chaque ID
    arrange({{ rank_col }}, .by_group = T) |> # arrange par date
    mutate(
      start_EVENT = tidyr::replace_na(
        {{rank_col}} > 0 & grepl(paste(events_look, collapse = "|"), {{events_var}}) & !grepl(paste(events_look, collapse = "|"), lag({{events_var}})), # lag(EVENTS_STATES) != "SORTIE",
        FALSE
      )
    ) |> # TRUE si match de events_loof sur la ligne, et pas de match de events_looks la ligne d'avant : QUI des NA ? 
  # ATTENTION : !grepl("sortie", NA) => TRUE, du coup control de {{rank_col}}>0
    mutate(period_EVENT = cumsum(start_EVENT)) |> # on compte les événements déjà survenus
    ungroup() |> 
    select({{ indiv_col }}, {{ rank_col }}, start_EVENT, period_EVENT, {{events_var}})

  #print(head(data))

  # Déterminer si on prend le max ou le min
  period_condition <- if (which_event == "last") {
    quote(period_EVENT == max(period_EVENT, na.rm = TRUE))
  } else if (which_event == "first") {
    quote(period_EVENT == min(period_EVENT, na.rm = TRUE) & period_EVENT>0)
  } else {
    stop("which_event doit être 'first' ou 'last'")
  }

  print(period_condition)

  seq_event <- data |>
    group_by({{ indiv_col }}) |>
    mutate(
  target_period = if (which_event == "last") {
    max(period_EVENT[period_EVENT > 0 & {{ rank_col }} > 0], na.rm = TRUE) # on prend l'evenement le plus tardif, qui n'est pas au début de la séquence et qui est diff de 0
  } else {
    if (which_event == "first") {
    min(period_EVENT[period_EVENT > 0 & {{ rank_col }} > 0], na.rm = TRUE) # on prend l'evenement le plus précoce, qui n'est pas au début de la séquence et qui est diff de 0
  } else {
          stop("which_event doit être 'first' ou 'last'")
    }}
) |>
    mutate(target_period = dplyr::if_else(is.infinite(target_period), NA_integer_ , target_period)) |> # suppr. des inf
    filter(
      ({{ events_var }} %in%
        events_look &
        period_EVENT == target_period) |
        {{ events_var }} == censure_mod
    ) |>
    slice_min(n = 1, {{ rank_col }}) |>
    mutate(time = {{ rank_col }}, event = {{ events_var }} %in% events_look) |>
    select({{ indiv_col }}, time, event)

  return(seq_event)
}

#' Détecte le pas de temps d'une séquence de dates 
#' 
#' @description 
#' Cette fonction analyse une séquence de dates et retourne le type de pas de temps : "day", "month", "year" ou "other". 
#' Elle est utile pour automatiser le traitement de données longitudinales (par exemple, pour déterminer si une série est quotidienne, mensuelle ou annuelle). 
#' Utilisée par [ViCaTraj::func_recode_event()]
#' 
#' @param dates Un vecteur de dates (de classe Date ou POSIXct). 
#' 
#' @return Un caractère indiquant le pas de temps détecté : `"day"`, `"month"`, `"year"` ou `"other"`. 
#' 
#' @examples 
#' func_detect_TimeStep(as.Date(c("2020-01-01", "2020-02-01", "2020-03-01"))) 
#' # [1] "month" 
#' 
#' func_detect_TimeStep(as.Date(c("2020-01-01", "2020-01-02", "2020-01-03"))) 
#' # [1] "day" 
#' 
#' func_detect_TimeStep(as.Date(c("2020-01-01", "2021-01-01", "2022-01-01"))) 
#' # [1] "year" 
#' 
#' @family survie 
#' @export 
func_detect_TimeStep <- function(dates) {
  dates <- sort(unique(dates))
  #dates <- dates[!is.na(dates)]
if(sum(is.na(dates))>0){
    stop("NA values in 'dates'")
}
  
  if (length(dates) < 2) {
    stop("length(dates) < 2")
  }


  if(inherits(dates, "Date")){
  month_index <- as.integer(format(dates, "%Y")) * 12L +
                 as.integer(format(dates, "%m"))
  month_steps <- diff(month_index)

  if (all(month_steps == month_steps[1])) {
    step <- month_steps[1]

    if (step %% 12 == 0) {
      return(paste(step %/% 12, "year"))
    } else {
      return(paste(step, "month"))
    }
  } else {
    stop("Plusieurs pas de temps présents")
  }
  day_steps <- as.integer(diff(dates))

  if (all(day_steps == day_steps[1])) {
    return(paste(day_steps[1], "day"))
  }
} else {
     if(inherits(dates, "numeric")){
       res.num <- unique(diff(dates))
       if(length(res.num)==1){
         return(res.num)
       } else {
         stop("Plusieurs pas de temps présents")
       }
     }
}

  "other"
}