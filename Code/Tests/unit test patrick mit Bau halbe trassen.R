

## Initialize Data 
load_data<-function(){
  
  ## Init Systemtrassen Graph
  el<-read.csv("Tests/el_test_mitBau.csv",sep=";",header = T)
  
  el$von<-as.character(el$von)
  el$bis<-as.character(el$bis)
  
  el<-el[seq(1,nrow(el),3),]
  
  
  ## Init Fahrlagen
  r<-read.csv("Tests/r_test.csv",sep=";",header = T)
  
  ## Init Größe der Instanz
  n<-37 # Number of Tage
  R<-nrow(r) # Number of Fahrlagen
  S<-nrow(el) # Number of Systemtrassen
  
  ## Return
  out<-list()
  out$el<-el
  out$r<-r
  out$n<-n
  
  return(out)
  
  
}