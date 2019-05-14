## Build Graph
## Add new legal edges to solution set

add_partition<-function(el,el_blocked,r,rows,v_top,n){
  
  print("Rufe add_partition mit edgelist_blocked:")
  #print(el_blocked)
  
  
  wegeSuche_count_temp<-0
  el_temp<-el_blocked[which(el_blocked$parent==0),]
  # rows<-which(r$partition==1) # Je nach dem was Jordis befielt: Set indices of covering partition
  
  
  ## Solange meine Partition noch nicht vollständig ist, mache Wegesuche für neue Variante durch die blocked systras, 
  ## die an mind. 1 Tag gültig ist
  part_id<-max(r$partition)+1
  new_part<-c()
  bits_flg<-as.integer(apply(r[rows,11:(10+n)],2,sum))
  
  print("add_partition für FLG")
  print(r$fahrlage[rows])
  
  it<-1
  for(i in rows){
    print(paste("add-partition-iteration: ",it))
    it<-it+1
    
    # Set number of fahrlagenvarianten that are being created  
    l<-1
    
    # Set v_top of the current Fahrlage
    v_top_flg<-v_top[which(v_top==r$von[i]):which(v_top==r$bis[i])]
    
    while(T){
      var_id<-max(r$id)+l
      l<-l+1
      wegeSuche_count_temp<-wegeSuche_count_temp+1
        try(spath<-bellmanford_bitconstr(el_temp,v_top_flg,bits_flg,1,n))
      if(length(spath)==0 || length(spath$epath)){
        print("Es konnte kein Weg mehr gefunden werden mit der aktuellen Bitleiste")
        break
      }else{
        print("Der gefundene Weg lautet:")
        print(spath$epath)
      }
      
      ## Add relevante Children der in dem Weg benutzen Parents zum Solution Space
      indices_ineltemp_parents<-as.integer(spath$epath)
      ids<-c()
      
      for(k in 1:length(indices_ineltemp_parents)){
        indices_inel_children<-which((el$parent==el_temp$id[indices_ineltemp_parents[k]] & el$beenparent==0) |
                                       (el$id==el_temp$id[indices_ineltemp_parents[k]] & el$beenparent==0))
        
        ## Falls Child Systemtrasse relevant (überschneidender Gültigkeitszeitraum), nimm hinzu
        for(j in 1:length(indices_inel_children)){
          if(length(which(r[i,11:(10+n)]==1 & el[indices_inel_children[j],7:(6+n)]==1))>0){
            ids<-c(ids,el$id[indices_inel_children[j]])
          }
        }
      }
      
      new_var<-data.frame(id=var_id,partition=part_id,fahrlage=r$fahrlage[rows[1]],von=r$von[rows[1]],bis=r$bis[rows[1]],
                          valid=1,abgelehnt=0,minFahrzeit=spath$length,homogen=r$homogen[rows[1]], 
                          res=toString(ids))
      
      # Suche nur noch für eingeschränkte bitleiste
      var_bits<-r[0,11:(10+n)]
      var_bits[1,]<-spath$bits
      new_var<-cbind(new_var,var_bits)
      
      
      # Suche nur noch auf eingeschränktem Graphen
      print("Suche auf folgedem eingeshcränktem Graphen nach Wegen (erst welche Spalten dann die ganze el)")
      el_temp[,7:(6+n)]<-as.matrix(el_temp[,7:(6+n)]) %*% diag(bits_flg-spath$bits)
      print(bits_flg-spath$bits)
      #print(el_temp)
      
      # Update appendix of r data frame that contains the varianten of the current partition
      if(!is.null(new_part)){
        new_part<-rbind(new_part,new_var)
        print("Die neue FLG Variante heißt:")
        print(new_var)
      }else{
        new_part<-new_var
      }
      
      # Update the bits that are still missing
      print("update new bitleiste (prior and bitleiste des pfades)")
      print(bits_flg)
      print(spath$bits)
      bits_flg<-bits_flg-spath$bits
      
      # Exit if bit cover is found
      if(all(bits_flg<=0)){
        break
      }
    }
    
    # If covering parition has been found, 
    if(all(bits_flg<=0)){
      
      # (Make sure the partition has the right order so that we can compare bits to other partitions)
      for(j in n:1){
        new_part<-new_part[order(-new_part[,10+j]),]
      }
      
      # and this is a new partiton (i.e. not some known one with new paths)
      known_part<-F
      flg <- new_part$fahrlage[1]
      
      for(k in unique(r$partition[which(r$fahrlage==flg)])){
        var_idx<-which(r$fahrlage==flg & r$partition==k)
        #(first make sure that the sizes are the)
        if(length(var_idx)==nrow(new_part)){
          if(all(r[var_idx,11:(10+n)]==new_part[,11:(10+n)])){
            known_part<-T
            break
          }  
        }
      }
      if(known_part==F){
        #print("new partition that is about to get added:")
        #print(new_part)
        
        # Stell sicher, dass die Partition aus mind. 2 Varianten besteht
        stopifnot(nrow(new_part)>=2)
        
        # Stell sicher, dass am Ende auch alle Tage der FLG auch getroffen wurden
        stopifnot(apply(new_part[,11:(10+n)],2,sum)-as.integer(r[i,11:(10+n)])==0)

        
        print("New partition:")
        print(new_part)
        
        # then update r data frame
        r<-rbind(r,new_part)
        
      }else{
        
        # if only new paths have been found
        # then just update the res column for this partition
        j<-1
        for(k in var_idx){
          #print("Union:")
          #print(r[k,])
          #print(as.character(new_part$res[j]))
          
          r$res[k]<-paste(r$res[k],as.character(new_part$res[j]),sep=", ")
          j<-j+1
        }
        
      }
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
  
  print("Ende add_partition")
  return(out)
}
