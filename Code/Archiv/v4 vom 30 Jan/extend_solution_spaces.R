## Build Graph
## Add new legal edges to solution set

extend_solution_spaces<-function(el,r,rows,n){
  wegeSuche_count_temp<-0
  
  for(i in 1:nrow(r)){
    if(i %in% rows){
      ## Build Restricted Graph
      el_temp<-el[0,]
      for(k in 1:nrow(el)){
        if(all(el[k,7:(n+6)]>=r[i,10:(n+9)])){
          el_temp<-rbind(el_temp,el[k,])
        }
      }
      g<-graph_from_edgelist(as.matrix(el_temp[,2:3]),directed = T)
      E(g)$name<-el_temp$id
      
      ## Shortest Path (if existent)
      spath<-NULL
      wegeSuche_count_temp<-wegeSuche_count_temp+1
      try(spath<-get.shortest.paths(g,from=as.character(r$von[i]),to=as.character(r$bis[i]),
                                    weights = el_temp$weight,output = 'epath')$epath[[1]],silent = TRUE)
      if(is.null(spath)){
        next
      }
      
      
      ## Add to Solution Space
      indices<-as.integer(spath)
      edges<-el_temp$id[indices]
      
      r$res[i]<-paste(r$res[i],", ",toString(edges),sep="")
      
      ## Speicher MinFahrzeit
      r$minFahrzeit[i]<-sum(as.integer(el_temp$weight[indices]))  
    }
  }
  
  
  ## L�sche doppelt gespeicherte legale Trassen
  for(i in 1:nrow(r)){
    ints<-as.integer(unlist(strsplit(r$res[i], split=", ")))
    r$res[i]<-toString(unique(ints))
  }
 
  ## Return
  out<-list()
  out$el<-el
  out$r<-r
  out$wegeSuche_count<-wegeSuche_count_temp
  return(out)
}
