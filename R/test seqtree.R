# library(TraMineR)
# data(mvad)
# seqstatl(mvad[, 17:86])
# 
# mvad.alphabet <- c("employment", "FE", "HE", "joblessness", "school", 
#                    "training")
# mvad.labels <- c("employment", "further education", "higher education", 
#                  "joblessness", "school", "training")
# mvad.scodes <- c("EM", "FE", "HE", "JL", "SC", "TR")
# mvad.seq <- seqdef(mvad, 17:86, alphabet = mvad.alphabet, states = mvad.scodes, 
#                    labels = mvad.labels, xtstep = 6)
# 
# submat <- seqsubm(mvad.seq, method = "TRATE")
# dist.om1 <- seqdist(mvad.seq, method = "OM", indel = 1, sm = submat)
# 
# c("gcse5eq", "Grammar" , "funemp","weight")->VarExpl
# paste("mvad.seq",paste(VarExpl, collapse = " + "),sep=" ~ ")->TexteFormule
# 
# as.formula(TexteFormule)->Formule
# 
# st <- seqtree(Formule, data = mvad,R = 5000, diss = dist.om1, pval = 0.05)
# 
# 
# # Si l'option file n'est pas renseignée alors le grpahique est sauvegardé dans un dossier temporaire. Sinon l'image est glabal est sauvegardé dans le dossier indiqué et dans un dossier temporaire
# # Tapis
# seqtreedisplay(st, type = "I", border = NA,sortv = "from.start")
# 
# #Chronogramme
# seqtreedisplay(st, type = "d", border = NA)
# 
# 
