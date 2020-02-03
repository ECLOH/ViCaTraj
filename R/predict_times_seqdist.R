#' @export

predict.time.dist<-function(df=calculated.times.for.dist, nb.sequences=10000)
  library(stats{
  fit_nls <- nls(time ~ a*(amplit ^ b), data = df[ , ], start =  c(a=0.5, b = 1), trace = F, control=nls.control(maxiter=1000))
  pp<-ggplot(data = df)+geom_point(aes(x=amplit, y=time))+
    geom_line(aes(y=predict(fit_nls, newdata = data.frame(amplit = amplit)), x=amplit))
  #
  fit_nls$m$predict(newdata = data.frame(amplit=1:nb.sequences))->c.predict
  nb.secondes<-round(c.predict[nb.sequences]/1000, 2)
  nb.minutes<-round(nb.secondes/60, 2)
  message(paste("Nombre de secondes pour", nb.sequences, "trajectoires :", nb.secondes, sep=" "))
  message(paste("\n nombre de minutes :",  nb.minutes, sep=" "))
  message(paste(sep=" ", "Estimation réalisée sur", nrow(calculated.times.for.dist), "lancements de seqdist()", "pour un nombre de trajectoires allant de", min(df$amplit), "à", max(df$amplit)))
  res<-list("nb.secondes"=nb.secondes, "nb.minutes"=nb.minutes, "pp"=pp)
  return(res)
}