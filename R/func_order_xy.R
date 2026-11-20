#' @export
order_xy <- function(VAR, VAR_arrange, VAR_ctrl, mods) {
  data <- dplyr::pick(everything())
  ord_vec <- dplyr::pull(
    dplyr::arrange(
      dplyr::summarise(
        dplyr::group_by(
          dplyr::filter(data, {{ VAR_ctrl }} %in% mods),
          {{ VAR }}
        ),
        across({{ VAR_arrange }}, ~ sum(.x, na.rm = TRUE))
      ),
      {{ VAR_arrange }}
    ),
    {{ VAR }}
  )
  return(ord_vec)
}

#' @export
order_xy_m_ctrl <- function(VAR, VAR_arrange, ..., mods) {
  data <- dplyr::pick(everything())
  vars_ctrl <- rlang::enquos(...)
  for (i in seq_along(vars_ctrl)) {
    var <- vars_ctrl[[i]]
    mod <- mods[[i]]
    data <- dplyr::filter(data, !!var %in% mod)
  }
  ord_vec <- data %>%
    dplyr::group_by({{ VAR }}) %>%
    dplyr::summarise(across({{ VAR_arrange }}, ~ sum(.x, na.rm = TRUE))) %>%
    dplyr::arrange({{ VAR_arrange }}) %>%
    dplyr::pull({{ VAR }})
  return(ord_vec)
}

#' @export
order_xy_mod <- function(VAR, VAR_arrange, VAR_ctrl, mods) {
  data <- dplyr::pick(everything())
  ord_vec <- dplyr::pull(
    dplyr::arrange(
      dplyr::filter(data, {{ VAR_ctrl }} %in% mods),
      {{ VAR_arrange }}
    ),
    {{ VAR }}
  )
  return(ord_vec)
}

#' @export
order_xy_traj <- function(VAR, VAR_arrange, VAR_ctrl, start = "end") {
  data <- dplyr::pick(everything())
  seq_tbl <- dplyr::mutate(
    data,
    VAR_arrange_num = as.integer(as.factor({{ VAR_arrange }}))
  )
  if (start == "end") {
    seq_tbl <- dplyr::arrange(seq_tbl, {{ VAR }}, dplyr::desc({{ VAR_ctrl }}))
  } else {
    if (start == "beg") {
      seq_tbl <- dplyr::arrange(seq_tbl, {{ VAR }}, {{ VAR_ctrl }})
    } else {
      stop("start doit être \"beg\" ou \"end\"")
    }
  }
  seq_tbl <- dplyr::summarise(
    dplyr::mutate(dplyr::group_by(seq_tbl, {{ VAR }}), NwRk = row_number()),
    seq = paste(VAR_arrange_num, collapse = "_"),
    .groups = "drop"
  )
  ord_vec <- dplyr::pull(dplyr::arrange(seq_tbl, seq), {{ VAR }})
  return(ord_vec)
}