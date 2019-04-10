## Fahrlagen haben disjunkte Gültigkeitszeiträume, aber überschneidede kürzeste Wege
## Erst mal infeasible, weil Trasse noch nicht getrennt doppelt belegt würde
## Dann würden die entsprechenden Systemtrassen im Mikrokonflikt aufgetrennt 
## Nach erneuter Wegesuche und SAT solving findet man eine Lösung

## Initialize Data 
load_data<-function(){
    
  ## Init Systemtrassen Graph
  el<-data.frame(id=1:5,
                 von=c("Q","Q","A","B","B"),
                 bis=c("A","B","S","A","S"),
                 weight=c(25,25,15,0,45),
                 parent=rep(0,5),
                 beenparent=rep(0,5),
                 tag1=c(1,1,1,1,1),
                 tag2=c(0,1,1,1,0),
                 tag3=c(1,1,1,1,1))
  
  el$von<-as.character(el$von)
  el$bis<-as.character(el$bis)
  
  ## Init Fahrlagen
  r<-data.frame(id=1:2,
                partition=c(1,1),
                fahrlage=c(1,2),
                von=c("Q","Q"),
                bis=c("S","S"),
                valid=c(1,1),
                abgelehnt=rep(0,2),
                minFahrzeit=c(0,0),
                homogen=c(1,.5),
                res=c(0,0),
                tag1=c(1,1),
                tag2=c(1,0),
                tag3=c(0,0))
  
  ## Init Größe der Instanz
  n<-3 # Number of Tage
  R<-nrow(r) # Number of Fahrlagen
  S<-nrow(el) # Number of Systemtrassen
  
  # State topological order of vertices
  v_top<-c('Q','B','A','S')
  
  ## Return
  out<-list()
  out$el<-el
  out$r<-r
  out$n<-n
  out$v_top<-v_top

  return(out)
}