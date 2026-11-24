#' @author Elie..
#' @title La fonction "delete_modgap"
#' @description  supprime les "fausses sorties" ou considérées comme telles
#' @param data jeu de données au format wide (1 pers. per row / 1 mois per columns)
#' @param type
#' @param missing "SUSPENDU" état "hors NA" considérés comme NA. Les NA sont par défaut traitées comme dess missing.
#' @param state.gap codage des NA (interne ou pas : voir setNA)
#' @param state.r "RSA" attribution de state.r aux fausses sorties". 
#' @param nb.min.gap nombre min de mois de sortie consécutifs : "value == state.gap & NB <= (nb.min.gap-1)"
#' @param ID "Personne.Identification.NIR" variable d'ID (string)
#' @param date.lab "name" variable de daye
#' @param setNA TRUE remplace à la fin les state.gap par NA_character_
#' @return a wide df
#' @export
delete_modgap<-function(data = t.seqw, type="wide", 
                        missing = "SUSPENDU",
                        state.gap = "Sortie / hors RSA", 
                        state.r="RSA", 
                        nb.min.gap = 4, 
                        ID = "Personne.Identification.NIR", 
                        date.lab = "name", setNA = TRUE){
  if(type == "wide"){
    require(dtplyr)
    res<-data |> 
      dtplyr::lazy_dt() |> 
      tidyr::pivot_longer(cols = - all_of(ID)) |> 
      mutate(value = case_when(value%in%c(missing) ~ state.gap,
                                    TRUE ~ value)) |> 
      mutate(value = replace_na(value, state.gap)) |> 
      group_by(across(all_of(ID))) |> 
      mutate(group = cumsum(value != lag(value, default = first(value)))) |> 
      group_by(group, .add=TRUE) |> 
      mutate(counter = row_number(), NB = max(counter)) |> 
      mutate(value = if_else(value == state.gap & NB <= (nb.min.gap-1) & group >0, state.r, value)) |> 
      as_tibble()

    if(setNA){
      res <- res |> mutate(value = if_else(value == state.gap, NA_character_, value))
    }

    res <- res |> tidyr::pivot_wider(id_cols = all_of(ID), names_from = all_of(date.lab)) |> 
      ungroup()

  }

  return(res)
  
}
