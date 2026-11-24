df.RSA.value<-data.frame("ETATDOSRSA_TEXT"=c("0"="Nouvelle demande en attente de décision CG pour ouverture du droit",
                                  "1"="Droit refusé", 
                                  "2"="Droit ouvert et versable",
                                  "3"="Droit ouvert et suspendu (le montant du droit est calculable, mais l'existence du droit est remis en cause)",
                                  "4"="Droit ouvert mais versement suspendu (le montant du droit n'est pas calculable)",
                                  "5"="Droit clos",
                                  "6"="Droit clos sur mois antérieur ayant eu un contrôle dans le mois de référence pour une période antérieure")) |> 
  tibble::rownames_to_column(var = "MODALITE")
usethis::use_data(df.RSA.value, overwrite = TRUE)