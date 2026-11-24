#' @author Elie.
#' @title La fonction "func_framesurv"
#' @description  l'objectif est de renvoyer des données au format tibble à partir des objects inclus dans boot_obj ( voir func_boot_survreg() )
#' @param boot_obj list(). Résultat de func_boot_survreg()
#' @param type character. Nom de l'objet inclus dans boot_obj à formatter
#' @return un tibble
#' @export
func_framesurv <- function(boot_obj = boot_srv_simple, type = "km_global") {
  if (type == "km_global" | type == "km_global_24") {
    res <- lapply(1:length(boot_obj), function(x) {
      temp <- summary(boot_obj[[x]][[type]])
      bind_cols(
        "strata" = temp$strata,
        "time" = temp$time,
        "surv" = temp$surv,
        "low" = temp$lower,
        "upp" = temp$upper
      ) |>
        mutate(sample = x)
    }) |>
      bind_rows() |>
      mutate(
        across(c(surv, low, upp), ~ 1 - .x, .names = "{.col}_haz")
      )
  } else {
    if (
      type %in%
        c(
          "modsurv",
          "modsurv_ANC_g",
          "modsurv_ANC_type",
          "modsurv_ANC_type_global"
        )
    ) {
      res <- lapply(1:length(boot_obj), function(x) {
        temp <- summary(boot_obj[[x]][[type]])
        pval <- broom::glance(boot_obj[[x]][[type]])$p.value
        temp_coef <- broom::tidy(boot_obj[[x]][[type]]) |> 
          mutate(COEF  = exp(estimate)) |> 
          rename("TYPE"="term") |> 
          mutate(pval = pval) |>
          mutate(sample = x)
      }) |>
        bind_rows()
    } else {
      if(
        grepl( pattern = "prop_", x = type, fixed = TRUE)
      ) {
        res <- lapply(1:length(boot_obj), function(x) {
          boot_obj[[x]][[type]] |> 
            mutate(sample = x)
        }) |>  bind_rows()
      } else {
        res <- NULL
    }
    }
  }

  return(res)
}