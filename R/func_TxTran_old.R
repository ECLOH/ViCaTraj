func_TxTran<-function(data, id, date, etat, lag){
  library(dplyr)

# 1. Créer la colonne ETAT_NEXT à 12 mois
df_trans <- data |>
  select({{id}}, {{date}}, {{etat}}) |> 
  group_by({{id}}) |> 
  arrange({{date}}, .by_group=T) |> 
  mutate(etat_lag = dplyr::lead({{etat}}, n  = lag))#(data.table::shift(x = {{etat}}, type = "lead", n = lag))

# 2. Calculer les taux de transition
taux_transition <- df_trans |>
  group_by({{etat}}, etat_lag) |>
  summarise(Ni = n_distinct({{id}}), Nt = n()) |> 
  ungroup() |> 
  left_join(
    data |> 
      group_by({{etat}}) |> 
      summarise(Ni_total = n_distinct({{id}}))
  ) |> 
  filter(!is.na(etat_lag)) |> 
  group_by({{etat}}) |> 
  mutate(prop_nt = Nt / sum(Nt), prop_ni = Ni / Ni_total)

return(taux_transition)
}
  