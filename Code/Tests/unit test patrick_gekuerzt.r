
## Initialize Data 
load_data<-function(){
  
  ## Init Systemtrassen Graph
  el<-read.csv("Tests/el_test_gekuerzt.csv",sep=";",header = T)
  
  el$von<-as.character(el$von)
  el$bis<-as.character(el$bis)
  
  
  ## Init Fahrlagen
  r<-read.csv("Tests/r_test_dyn_gekuerzt.csv",sep=";",header = T)
  
  ## Init Größe der Instanz
  n<-37 # Number of Tage
  R<-nrow(r) # Number of Fahrlagen
  S<-nrow(el) # Number of Systemtrassen
  
  # State topological order of vertices
  v_top<-c('A','D','E','F','B','G','H','C','I','J')  
  
  ## Return
  out<-list()
  out$el<-el
  out$r<-r
  out$n<-n
  out$v_top<-v_top
  
  
  return(out)
  
  
}