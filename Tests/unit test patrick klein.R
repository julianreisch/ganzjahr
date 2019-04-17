## Beide Fahrlagen fahren auf disjunkten Wegen und an disjunkten Tagen
## Die Lösung muss also eine feasible Solution sein.

## Initialize Data 
load_data<-function(){
  
  ## Init Systemtrassen Graph
  el<-read.csv("Tests/el_test.csv",sep=";",header = T)
  el<-el[,1:(ncol(el)-20)]
  
  el$von<-as.character(el$von)
  el$bis<-as.character(el$bis)
  
  
  ## Init Fahrlagen
  r<-read.csv("Tests/r_test.csv",sep=";",header = T)
  r<-r[which(r$fahrlage %in% unique(r$fahrlage)[1:4]),]
  r<-r[,1:(ncol(r)-20)]
  
  ## Init Größe der Instanz
  n<-17 # Number of Tage
  R<-nrow(r) # Number of Fahrlagen
  S<-nrow(el) # Number of Systemtrassen
  
  ## Return
  out<-list()
  out$el<-el
  out$r<-r
  out$n<-n
  
  return(out)
  
  
}