## Build Graph
## Add new legal edges to solution set

extend_solution_spaces<-function(el,r,n){
  
  for(i in 1:nrow(r)){
    ## Build Restricted Graph
    el_temp<-el[0,]
    for(k in 1:nrow(el)){
      if(all(el[k,6:(n+5)]>=r[i,4:(n+3)])){
        el_temp<-rbind(el_temp,el[k,])
      }
    }
    g<-graph_from_edgelist(as.matrix(el_temp[,2:3]),directed = T)
    E(g)$name<-el_temp$id
    
    ## Shortest Path (if existent)
    spath<-NULL
    try(spath<-get.shortest.paths(g,from="Q",to="S",weights = el_temp$weight,output = 'epath')$epath[[1]])
    if(is.null(spath)){
      next
    }
    indices<-as.integer(spath)
    edges<-el_temp$id[indices]
    
    ## Add to Solution Space
    r$res[i]<-paste(r$res[i],", ",toString(edges),sep="")
  }
  
  
  ## Lösche doppelt gespeicherte legale Trassen
  for(i in 1:nrow(r)){
    ints<-as.integer(unlist(strsplit(r$res[i], split=", ")))
    r$res[i]<-toString(unique(ints))
  }
 
  ## Return
  out<-list()
  out$el<-el
  out$r<-r
  return(out)
}
