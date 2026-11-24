#' Agréger des échantillons témoins et tests 
#' 
#' Cette fonction permet d'agréger les données des échantillons témoins et tests à partir de plusieurs tables, en réalisant des jointures et des transformations spécifiques. 
#' 
#' @param seq_long data.table. Table contenant les séquences longues avec au moins la colonne \code{NIR}. 
#' @param data_test_val data.table. Table contenant les valeurs des tests avec au moins les colonnes \code{NIR}, \code{DATE_list}, \code{RSA_simple_corr}. 
#' @param dt_long_sample data.table. Table d'échantillons avec au moins les colonnes \code{SAMPLE}, \code{NIR_TEST}, \code{NIR}, \code{DATE_list}, \code{RSA_simple_corr}, \code{POID}. 
#' 
#' @return Un \code{data.table} combinant les informations des témoins et des tests, avec ajout de colonnes \code{TYPE}, \code{RANG} et \code{ID}. 
#' 
#' @import data.table 
#' @export 
func_aggsample <- function(
  seq_long,
  data_test_val,
  dt_long_sample
) {
  dt_long_temoin <- data.table::rbindlist(
    fill = T,
    l = lapply(unique(dt_long_sample$SAMPLE), function(si) {
      si_dt <- dt_long_sample[
        SAMPLE == si,
        c("NIR_TEST", "NIR", "DATE_list", "RSA_simple_corr", "POID", "SAMPLE")
      ][, TYPE := "TEMOIN"][, RANG := 1]
      si_lg <- data.table::merge.data.table(
        seq_long[NIR %in% si_dt$NIR],
        si_dt[, c("NIR", "POID", "SAMPLE", "NIR_TEST", "TYPE")],
        by.x = "NIR",
        by.y = "NIR",
        all.x = TRUE
      )
      si_lg <- data.table::merge.data.table(
        si_lg,
        si_dt[, c("NIR", "DATE_list", "RANG", "RSA_simple_corr")],
        by.x = c("NIR", "DATE_list", "RSA_simple_corr"),
        by.y = c("NIR", "DATE_list", "RSA_simple_corr"),
        all.x = TRUE
      )
      si_lg[, ID := paste0(NIR, "_S", SAMPLE)]
      return(si_lg)
    })
  )
  dt_long_test <- seq_long[NIR %in% dt_long_temoin$NIR_TEST , ]
  message(dim(dt_long_test))
  dt_long_test <- data.table::merge.data.table(
    dt_long_test,
    data_test_val[ , c("NIR", "DATE_list")][, RANG := 1],
    by.x = c("NIR", "DATE_list"),
    by.y = c("NIR", "DATE_list"),
    all.x = TRUE
  )
  dt_long_test[ , TYPE := "TEST"]
  
  message(dim(dt_long_test))
  dt_long_test[, ID := as.character(NIR)]
    message(dim(dt_long_test))
  
  print(table(dt_long_test$RSA_simple_corr, exclude = NULL))

message("ID" %in% names(dt_long_test))
  res <- data.table::rbindlist(fill = T, l = list(dt_long_temoin, dt_long_test))
  return(res)
}
