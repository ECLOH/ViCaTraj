#' @author Elie..
#' @title La fonction "recal_new_tidy"
#' @description  recale à gauche un df "wide" ou "long" avec des dates d'entrée et de sorties différentes (typiquement pour des nouveaux entrants sur 1 trimestre par exemple)
#' @param nb.mois 3 nombre de mois glissants de sélection des nvx entrants. Si 1 trimestre => 3
#' @return df avec les nouvelles variables et recodages
#' @export
recal_new_tidy<-function(data = t.seqw, type="wide",
                         ID, nb.mois = 3){
                        #ID = "Personne.Identification.NIR"){
if(type=="wide"){
  res <- data |> 
  tidyr::pivot_longer(cols = - all_of(ID)) |> 
  arrange( all_of(ID) , name) |> 
  group_by(across(all_of(ID))) |> 
  filter(cumsum(if_else(is.na(value), 0, if_else(value=="RSA", 1, 0 ))) > 0) |> 
  mutate(time = row_number()) |> 
  tidyr::pivot_wider(names_from = time, values_from = value, id_cols = all_of(ID)) |> 
  ungroup()
  res<-res[ , 1:(ncol(res)-nb.mois+1)]
}
  if(type=="long"){
    res<-data |> 
      arrange( all_of(ID) , name) |> 
      group_by(across(all_of(ID))) |> 
      filter(cumsum(if_else(is.na(value), 0, if_else(value=="RSA", 1, 0 ))) > 0) |> 
      mutate(time = row_number())
    #res<-res |> 
    #  filter(time<=max(time)-(nb.mois+1))
    }
  return(res)
}