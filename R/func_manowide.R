func_manowide <- function(data, id_col){
  data_l <- data |> 
    group_by({{id_col}}) |> 
    mutate(rk = row_number()) |> 
    group_by(rk) |> 
    group_split(.keep = TRUE) |> 
    purrr::map(
      function(xdf){
        xdf |> 
        rename_with( ~ paste0(.x, "_", unique(xdf$rk)), .cols = -{{id_col}})
      }
    )
  res <- purrr::reduce(data_l, dplyr::left_join, by = join_by({{id_col}}))
  return(res)
}