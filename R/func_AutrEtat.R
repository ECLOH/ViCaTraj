#' Analyse des transitions d'état
#'
#' Calcule les proportions de transitions entre états à partir d'un point de contrôle.
#'
#' @param data Un dataframe contenant les données de trajectoire.
#' @param var_ID Colonne identifiant les individus (non quoté).
#' @param var_time Colonne de temps/date (non quoté).
#' @param var_etat_start Colonne d'état initial. Si NULL, créée à partir de var_etat au time_ctrl.
#' @param var_etat Colonne d'état (non quoté).
#' @param time_ctrl Point de contrôle (référence temporelle).
#' @param time_view Type d'analyse : "all" (toute trajectoire), "before" (avant time_ctrl), "after" (après time_ctrl).
#'
#' @return Un dataframe avec colonnes :
#'   - var_etat_start : état initial
#'   - var_etat : état observé
#'   - Ni : nombre d'individus
#'   - Ni_total : total par état initial
#'   - prop : proportion (Ni / Ni_total)
#'
#' @examples
#' \dontrun{
#'   func_AutrEtat(
#'     data = my_data,
#'     var_ID = id,
#'     var_time = date,
#'     var_etat_start = NULL,
#'     var_etat = status,
#'     time_ctrl = as.Date("2023-01-01"),
#'     time_view = "after"
#'   )
#' }
#'
#' @export
func_AutrEtat <- function(
  data,
  var_ID,
  var_time,
  var_etat_start,
  var_etat,
  time_ctrl,
  time_view
) {

    if (!time_view %in% c("all", "before", "after")) {
    stop("time_view must be 'all', 'before' or 'after'")
  }
  var_etat_start_dp <- deparse(substitute(var_etat_start))
  print(var_etat_start_dp)

  if (var_etat_start_dp == "") {
    data_ctrl <- data |>
      ungroup() |>
      filter({{ var_time }} == time_ctrl) |>
      distinct() |>
      mutate(created_var = {{ var_etat }}) |>
      select({{ var_ID }}, created_var)

    data <- data |> left_join(data_ctrl)

    var_etat_start <- quo(created_var)

  } else {
        var_etat_start <- enquo(var_etat_start)
  }

 statd_total <- data |>
        group_by({{ var_etat_start }}) |>
        summarise(Ni_total = n_distinct({{ var_ID }}))
if(
  time_view == "all"
){
  message("Calculating over the entire trajectory, with ``time_ctr`` set to: ", time_ctrl)
} else {
  if(
  time_view == "before"
){
    data <- data |> 
          filter({{var_time}} <= {{time_ctrl}})
      message("Calculating over the entire trajectory, with ``time_ctr`` set to: ", time_ctrl)
  } else {
      data <- data |> 
          filter({{var_time}} >= {{time_ctrl}})
      message("Calculating over the entire trajectory, with ``time_ctr`` set to: ", time_ctrl)
}
}
  
  res <- data |>
    select({{ var_ID }}, {{ var_etat_start }}, {{ var_etat }}) |>
    ungroup() |>
    distinct() |>
    group_by({{ var_etat_start }}, {{ var_etat }}) |>
    summarise(Ni = n_distinct({{ var_ID }})) |> 
    bind_rows(
data |>
  group_by({{ var_etat_start }}, {{ var_ID }}) |> 
  summarise(N_etat = n_distinct({{ var_etat }})) |> 
  filter(N_etat ==1) |> 
  group_by({{ var_etat_start }}) |> 
  summarise(Ni = n_distinct({{ var_ID }})) |> 
  mutate("{{var_etat}}" := "non concerné")
    ) |>     left_join(
     statd_total
    ) |>
    mutate(prop = Ni / Ni_total)

  return(res)
}