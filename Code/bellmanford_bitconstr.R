bellmanford_bitconstr<-function(el,v_top_flg,bits_flg,k,n){
  ### Hard coded: k
  k<-37
  reduce<-TRUE
  
  while(reduce==TRUE){
    print("Try Wegesuche mit k=")
    print(k)
    reduce<-FALSE
    
    # Setze topological ordering der vertices voraus (etwa durch zeitl Reihenfolge der Kanten)
    bits<-matrix(0,length(v_top_flg),n)
    bits[1,]<-bits_flg
    dist<-c(0,rep(Inf,length(v_top_flg)-1))
    pred_edge<-rep(NA,length(v_top_flg))
    
    # Bellman Ford acyclic
    # Gehe durch das top. ordering
    for(indx_u in 1:length(v_top_flg)){
      u <- v_top_flg[indx_u]
      incident_edges <- which(el$von==u & el$bis %in% v_top_flg)
      
      # Für alle ausgehenden Kanten
      if(length(incident_edges)>0){
        for(j in 1:length(incident_edges)){
          v <- el$bis[incident_edges[j]]
          indx_v<-which(v_top_flg==v)
          curr_weight <- as.numeric(el$weight[incident_edges[j]])
          curr_bits<-as.numeric(el[incident_edges[j],7:(6+n)])
          
          # Ist Ziel-Knoten schneller erreichbar als er es bisher ist?
          #print("Prüfe Dreiecksungleichung für:")
          #print(u)
          #print(v)
          #print(curr_weight)
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
    if(is.na(pred_edge[length(pred_edge)]) | pred_edge[length(pred_edge)]==v_top_flg[length(v_top_flg)]){
      spath<-list()
      spath$epath<-c()
      spath$bits<-bits[1,]
      spath$length<-NA
      if(k>1){
        reduce<-TRUE
        k<-k-1
      }
    }else{
      # Gehe rückwärts durch die Kanten um den schnellsten Pfad zu finden
      epath<-c()
      curr_indx<-length(pred_edge)
      pred_vert<-v_top_flg[length(v_top_flg)]
      while (pred_vert != v_top_flg[1]) {
        pred_vert<-el$von[which(el$id==pred_edge[curr_indx])]
        epath<-c(pred_edge[curr_indx],epath)
        curr_edge<-pred_edge[which(v_top_flg==pred_vert)]
        curr_indx<-which(pred_edge==curr_edge)
      }
      
      spath<-list()
      spath$epath<-epath
      spath$bits<-bits[length(v_top_flg),]
      spath$length<-dist[length(v_top_flg)]
      
    }
    
  }

  return(spath)
}