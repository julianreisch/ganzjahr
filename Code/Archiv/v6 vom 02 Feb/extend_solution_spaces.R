## Build Graph
## Add new legal edges to solution set

extend_solution_spaces<-function(el,r,rows,n){
  wegeSuche_count_temp<-0
  
  for(i in 1:nrow(r)){
    if(i %in% rows){
      
      ## Build Restricted Graph, der aus Systemtrassen besteht, die für den Gültigkeitszeitraum der Fahrlagenvariante auch gültig sind
      ## Außerdem wird der Weg nur über Parent Systemtrassen gesucht, in die Lösungsmenge unten aber die relevanten Blätter aufgenommen
      el_temp<-el[0,]
      for(k in 1:nrow(el)){
        if(all(el[k,7:(n+6)]>=r[i,10:(n+9)]) & el$parent[k]==0){
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
      if(length(spath)==0){
        next
      }
      
      
      ## Add relevante Children der in dem Weg benutzen Parents zum Solution Space
      indices_ineltemp_parents<-as.integer(spath)
      ids<-c()
      
      for(k in 1:length(indices_ineltemp_parents)){
        indices_inel_children<-which(el$parent==el_temp$id[indices_ineltemp_parents[k]] |
                                       (el$id==el_temp$id[indices_ineltemp_parents[k]] & el$beenparent==0))
        
        ## Falls Child Systemtrasse relevant (überschneidender Gültigkeitszeitraum), nimm hinzu
        for(j in 1:length(indices_inel_children)){
          if(length(which(r[i,10:(9+n)]==1 & el[indices_inel_children[j],7:(6+n)]==1))>0){
            ids<-c(ids,el$id[indices_inel_children[j]])
          }
        }
      }
      
      r$res[i]<-paste(r$res[i],", ",toString(ids),sep="")
      
      ## Speicher MinFahrzeit
      r$minFahrzeit[i]<-sum(as.integer(el_temp$weight[which(el_temp$id %in% ids)]))  
    }
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
  out$wegeSuche_count<-wegeSuche_count_temp
  return(out)
}
