library(TraMineR)
data(mvad) #data()
seqstatl(mvad[, 17:86])

mvad.alphabet <- c("employment", "FE", "HE", "joblessness", "school", 
                   "training")
mvad.labels <- c("employment", "further education", "higher education", 
                 "joblessness", "school", "training")
mvad.scodes <- c("EM", "FE", "HE", "JL", "SC", "TR")

runApp(
  list(
    ui = fluidPage(
      titlePanel("Seqtree"),
                    selectInput("explan1", label=h4 ("Variables"),choices = c("gcse5eq", "Grammar" , "funemp"),multiple =TRUE),
                    actionButton(inputId = "Bouton",label = "Valider")
      # Pour Variable mettre choices="" et updateSelectInput(...,choices = c(colsouspop))
          
      ),
    
    server = function(input, output,session) {
      seqData<-reactive({
        seqdef(mvad, 17:86, alphabet = mvad.alphabet, states = mvad.scodes, 
                           labels = mvad.labels, xtstep = 6)
      }) # seqData remplacer par data.seq() car pas besoin de recalculer
      
      CalculDist<-reactive({
        req(seqData())
        submat <- seqsubm(seqData(), method = "TRATE")
        return(seqdist(seqData(), method = "OM", indel = 1, sm = submat))
      })
      
     Formule<-reactive({
       req(input$explan1,CalculDist(),seqData())
       paste("seqData()",paste(input$explan1, collapse = " + "),sep=" ~ ")->TexteFormule
       as.formula(TexteFormule)->formule1
       return(seqtree(formule1, data = mvad,R = 5000, diss = CalculDist(), pval = 0.05))
     })
      
     observe({
       input$Bouton
       isolate({
         req(Formule())
         seqtreedisplay(Formule(), type = "d", border = NA, file="C:/Temp")
       })
     })
      
    }
    
  ))