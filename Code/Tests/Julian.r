
## Initialize Data 
load_data<-function(){
  
  ## Init Systemtrassen Graph
  el<-read.csv("Tests/el_gesamt_Julian.csv",sep=";",header = T)
  
  el$von<-as.character(el$von)
  el$bis<-as.character(el$bis)
  
  
  ## Init Fahrlagen
  r<-read.csv("Tests/r_gesamt_Julian.csv",sep=";",header = T)
  
  ## Init Größe der Instanz
  n<-3 # Number of Tage
  R<-nrow(r) # Number of Fahrlagen
  S<-nrow(el) # Number of Systemtrassen
  
  # State topological order of vertices
  v_top<-c('A','B','C','D','H','I','J','K','E','FF','G','N','M','P','O','Q','R','S','T','L','U','W','X','AA','V','Z','Y')  
  
  ## Return
  out<-list()
  out$el<-el
  out$r<-r
  out$n<-n
  out$v_top<-v_top
  
  
  return(out)
  
  
}