## Build Graph
## Add new legal edges to solution set

add_partition<-function(el,el_blocked,r,rows,v_top,n){
  
  print("Rufe add_partition mit edgelist_blocked:")
  #print(el_blocked)
  
  
  
  wegeSuche_count_temp<-0
  el_temp<-el_blocked[which(el_blocked$parent==0),]
  # rows<-which(r$partition==1) # Je nach dem was Jordis befielt: Set indices of covering partition
  
  
  ## Solange meine Partition noch nicht vollst�ndig ist, mache Wegesuche f�r neue Variante durch die blocked systras, 
  ## die an mind. 1 Tag g�ltig ist
  part_id<-max(r$partition)+1
  new_part<-c()
  bits_flg<-as.integer(apply(r[rows,11:(10+n)],2,sum))
  print('bits_flg')
  print(bits_flg)
  
  print("add_partition f�r FLG")
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
      if(length(spath)==0 || is.null(spath$epath)){
        print("Es konnte kein Weg mehr gefunden werden (mit der aktuellen Bitleiste)")
        print(spath)
        break
      }else{
        print("Der gefundene Weg lautet:")
        print(spath$epath)
        print("und geht von/nach:")
        print(v_top_flg[c(1,length(v_top_flg))])
        
        # exception handling
        if(length(spath$epath)==0){
          print("ERROR in der Wegesuche")
          print(spath)
          print(v_top_flg)
          #stopifnot(length(spath$epath)>0)
        }
      }
      
      ## Add relevante Children der in dem Weg benutzen Parents zum Solution Space
      indices_ineltemp_parents<-as.integer(spath$epath)
      ids<-c()
      
      for(k in 1:length(indices_ineltemp_parents)){
        indices_inel_children<-which((el$parent==el_temp$id[indices_ineltemp_parents[k]] & el$beenparent==0) |
                                       (el$id==el_temp$id[indices_ineltemp_parents[k]] & el$beenparent==0))
        print("indices_inel_children")
        print(indices_inel_children)
        
        ## Falls Child Systemtrasse relevant (�berschneidender G�ltigkeitszeitraum), nimm hinzu
        for(j in 1:length(indices_inel_children)){
          
          ##Bestimmt die G�ltigkeit, der gefundenen Variante f�r die Pr�fung
          var_bits_pruef<-r[0,11:(10+n)]
          var_bits_pruef[1,]<-spath$bits
          
          #print("j")
          #print(j)
          #print(el[indices_inel_children[j],7:(6+n)]==1)
          #print(r[i,11:(10+n)]==1)
          #print(var_bits_pruef==1)
          
          #if(length(which(r[i,11:(10+n)]==1 & el[indices_inel_children[j],7:(6+n)]==1))>0){
          if(length(which(var_bits_pruef==1 & el[indices_inel_children[j],7:(6+n)]==1))>0){
            ids<-c(ids,el$id[indices_inel_children[j]])
          }
        }
      }
      
      new_var<-data.frame(id=var_id,partition=part_id,fahrlage=r$fahrlage[rows[1]],von=r$von[rows[1]],bis=r$bis[rows[1]],
                          valid=1,abgelehnt=0,minFahrzeit=spath$length,homogen=r$homogen[rows[1]], 
                          res=toString(ids))
      
      # Suche nur noch f�r eingeschr�nkte bitleiste
      var_bits<-r[0,11:(10+n)]
      var_bits[1,]<-spath$bits
      new_var<-cbind(new_var,var_bits)
      
      
      # Suche nur noch auf eingeschr�nktem Graphen
      print("Suche auf folgedem eingeshcr�nktem Graphen nach Wegen (erst welche Spalten dann die ganze el)")
      el_temp[,7:(6+n)]<-as.matrix(el_temp[,7:(6+n)]) %*% diag(bits_flg-spath$bits)
      print(bits_flg-spath$bits)
      #print(el_temp)
      
      # Update appendix of r data frame that contains the varianten of the current partition
      if(!is.null(new_part)){
        new_part<-rbind(new_part,new_var)
        print("Die neue FLG Variante hei�t:")
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
        print("Weg gefunden")
        break
      }
    }
    
    # If covering parition has been found, 
    if(all(bits_flg<=0)){
      print('komme ich hier hin?')
#     
#     # Versuche die Partition mit sogar noch weniger Varianten, wenn diese die ganze Bitleiste schon �berdecken
#     if(nrow(new_part)>1){
#       print(new_part)
#       bits_objective<-as.integer(apply(r[rows,11:(10+n)],2,sum))
#       
#       fahrzeiten<-c()
#       bits_trassen_matrix<-matrix(0,0,n)
#       for(i_part in 1:nrow(new_part)){
#         trassen<-as.integer(unlist(strsplit(as.character(new_part$res[i_part]), split=", ")))
#         trassen<-trassen[order(trassen)]
#         #fahrzeit_trasse<-sum(as.integer(el_blocked$weight[which(el_blocked$id %in% trassen)]))
#         fahrzeit_trasse<-1
#         print("trassen:")
#         print(trassen)
#         
#         selbst_parent_trassen<-trassen[which(el_blocked$parent[which(el_blocked$id %in% trassen)]==0)]
#         fremder_parent_trassen<-trassen[which(el_blocked$parent[which(el_blocked$id %in% trassen)]!=0)]
#         print("selbst_parent, fremnder parent:")
#         print(selbst_parent_trassen)
#         print(fremder_parent_trassen)
#         
#         parents<-el_blocked$parent[which(el_blocked$id %in% fremder_parent_trassen)]
#         print("parent:")
#         print(parent)
#         bits_trassen_df<-el_blocked[which(el_blocked$id %in% c(parents,selbst_parent_trassen)),7:(6+n)]
#         
#         print("bits trassen df:")
#         print(bits_trassen_df)
#         
#         bits_trassen<-apply(bits_trassen_df,2,min)
#         bits_trassen_matrix<-rbind(bits_trassen_matrix,bits_trassen)
#         fahrzeiten<-c(fahrzeiten,fahrzeit_trasse)
#       }
#       bits_trassen_matrix_filtered<-as.matrix(bits_trassen_matrix[,bits_objective==1])
#       print("bits_trassenmatrix_filtered:")
#       print(bits_trassen_matrix_filtered)
#
#       sol<-lp("min",fahrzeiten,t(bits_trassen_matrix_filtered),rep(">=",ncol(bits_trassen_matrix_filtered)),rep(1,ncol(bits_trassen_matrix_filtered)))
#       print("solution:")
#       print(sol$solution)
#       
#       #all_sol<-lp("min",rep(0,length(fahrzeiten)),t(bits_trassen_matrix_filtered),rep(">=",ncol(bits_trassen_matrix_filtered)),rep(1,ncol(bits_trassen_matrix_filtered)),all.bin = T,num.bin.solns = 2^length(fahrzeiten))
#       #print("All solutions:")
#       #print(all_sol)
#       
#       new_part<-new_part[sol$solution==1,]
#       new_part[,(11:(10+n))[bits_objective==1]]<-bits_trassen_matrix_filtered[sol$solution==1,]
#
#       # Ordne mehrfach �pberdeckte Tage den Varianten zu
#       indx_longest<-which.max(apply(new_part[,(11:(10+n))],1,sum))
#       bits_longest<-as.numeric(new_part[indx_longest,(11:(10+n))])
#       
#       # Erstmal den L�ngsten Komplett nehmen
#       for(k in 1:nrow(new_part)){
#         if(k!=indx_longest){
#           new_part[k,(11:(10+n))[bits_longest==1]]<-rep(0,sum(bits_longest))
#         }
#       }
#       
#       # Dann die Restlichen nach Fahrzeit sortiert nehmen
#       reihenfolge<-order(new_part$minFahrzeit)
#       if (length(reihenfolge)>=3){
#         for(j in 1:(length(reihenfolge)-1)){
#           indx<-reihenfolge[j]
#           bits_shortest<-as.numeric(new_part[indx,(11:(10+n))])
#           for(k in (j+1):length(reihenfolge)){
#             indx_2<-reihenfolge[k]
#             new_part[indx_2,(11:(10+n))[bits_shortest==1]]<-rep(0,sum(bits_shortest))
#           }
#             
#         }
#       }
#       
#       
#               
#       print("New new partition:")
#       print(new_part)
#       
#       
#     }
    
    # (Make sure the partition has the right order so that we can compare bits to other partitions)
      for(j in n:1){
        new_part<-new_part[order(-new_part[,10+j]),]
      }
      
      # and this is a new partiton (i.e. not some known one with new paths)
      known_part<-F
      flg <- new_part$fahrlage[1]
      #flg <- new_part$fahrlage[i]
      #print('flg')
      #print(i)
      #print(flg)
      #print(new_part$fahrlage[i])
      #print(r$partition[which(r$fahrlage==flg)])
      
      for(k in unique(r$partition[which(r$fahrlage==flg)])){
        print('k')
        print(k)
        #print('un')
        #print(unique(r$partition[which(r$fahrlage==flg)]))
        var_idx<-which(r$fahrlage==flg & r$partition==k)
        #print(var_idx)
       #print(new_part)
        print(length(var_idx))
        print(nrow(new_part))
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
        # stopifnot(nrow(new_part)>=2)
        
        # Stell sicher, dass am Ende auch alle Tage der FLG auch getroffen wurden
        print("das")
        print(apply(new_part[,11:(10+n)],2,sum))
        print(as.integer(r[i,11:(10+n)]))
        print(apply(new_part[,11:(10+n)],2,sum)-as.integer(r[i,11:(10+n)])==0)
        
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
  
  print("Ende add_partition")
  return(out)
}
