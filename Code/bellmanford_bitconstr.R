bellmanford_bitconstr<-function(el,v_top,bits_flg,k,n){
  
  # Setze topological ordering der vertices voraus (etwa durch zeitl Reihenfolge der Kanten)
  bits<-matrix(0,length(v_top),n)
  bits[1,]<-bits_flg
  dist<-c(0,rep(Inf,length(v_top)-1))
  pred_edge<-rep(NA,length(v_top))

  # Bellman Ford acyclic
  # Gehe durch das top. ordering
  for(indx_u in 1:length(v_top)){
    u <- v_top[indx_u]
    incident_edges <- which(el$von==u)
    
    # Für alle ausgehenden Kanten
    if(length(incident_edges)>0){
      for(j in 1:length(incident_edges)){
        v <- el$bis[incident_edges[j]]
        indx_v<-which(v_top==v)
        curr_weight <- as.numeric(el$weight[incident_edges[j]])
        curr_bits<-as.numeric(el[incident_edges[j],7:(6+n)])
        # Ist Ziel-Knoten schneller erreichbar als er es bisher ist?
        if(dist[indx_u] + curr_weight < dist[indx_v] &
           # Und: Kann ich die Kanten an mind. k Tagen verknüpfen?
           sum(bits[indx_u,] * curr_bits)>=k
        ){
          dist[indx_v]<-dist[indx_u] + curr_weight
          bits[indx_v,]<-bits[indx_u,] * curr_bits
          pred_edge[indx_v]<-el$id[incident_edges[j]]
        }#To do: Wenn keine Verknüpfung stattfinden kann, mach Backtracking
      }
    }
  }
  # Assert if no path to target has been found
  if(is.na(pred_edge[length(pred_edge)]) | pred_edge[length(pred_edge)]==v_top[length(v_top)]){
    spath<-list()
    spath$epath<-c()
    spath$bits<-bits[1,]
    spath$length<-NA
  }else{
    # Gehe rückwärts durch die Kanten um den schnellsten Pfad zu finden
    epath<-c()
    curr_indx<-length(pred_edge)
    pred_vert<-v_top[length(v_top)]
    while (pred_vert != v_top[1]) {
      pred_vert<-el$von[which(el$id==pred_edge[curr_indx])]
      epath<-c(pred_edge[curr_indx],epath)
      curr_edge<-pred_edge[which(v_top==pred_vert)]
      curr_indx<-which(pred_edge==curr_edge)
    }
    
    spath<-list()
    spath$epath<-epath
    spath$bits<-bits[length(v_top),]
    spath$length<-dist[length(v_top)]
    
  }

  return(spath)
}