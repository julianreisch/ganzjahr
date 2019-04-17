## Beide Fahrlagen fahren auf disjunkten Wegen und an disjunkten Tagen
## Die Lösung muss also eine feasible Solution sein.

## Initialize Data 
init_data<-function(){
  
  ## Init Systemtrassen Graph
  el<-data.frame(id=1:5,
                 von=c("Q","Q","A","B","B"),
                 bis=c("A","B","S","A","S"),
                 weight=c(25,25,15,0,45),
                 parent=rep(0,5),
                 tag1=c(0,1,0,1,1),
                 tag2=c(1,0,1,0,0))
  
  el$von<-as.character(el$von)
  el$bis<-as.character(el$bis)
  
  
  ## Init Fahrlagen
  r<-data.frame(id=1:2,
                partition=c(1,1),
                fahrlage=c(1,2),
                von=c("Q","Q"),
                bis=c("S","S"),
                valid=c(1,1),
                minFahrzeit=c(0,0),
                homogen=c(0,0),
                res=c(0,0),
                tag1=c(0,1),
                tag2=c(1,0))
  
  ## Init Größe der Instanz
  n<-2 # Number of Tage
  R<-nrow(r) # Number of Fahrlagen
  S<-nrow(el) # Number of Systemtrassen
  
  ## Return
  out<-list()
  out$el<-el
  out$r<-r
  out$n<-n
  
  return(out)
  

}