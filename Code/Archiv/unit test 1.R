## Beide Fahrlagen fahren auf disjunkten Wegen und an disjunkten Tagen
## Die Lösung muss also eine feasible Solution sein.

## Init Systemtrassen Graph
el<-data.frame(id=1:5,
               von=c("Q","Q","A","B","B"),
               bis=c("A","B","S","A","S"),
               weight=c(25,25,15,0,45),
               tag1=c(0,1,0,1,1),
               tag2=c(1,0,1,0,0))

el$von<-as.character(el$von)
el$bis<-as.character(el$bis)

g<-graph_from_edgelist(as.matrix(el[,2:3]),directed = T)
E(g)$name<-el$id

## Init Fahrlagen
r<-data.frame(id=1:2,
              tag1=c(0,1),
              tag2=c(1,0),
              res=c(0,0))