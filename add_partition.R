## Build Graph
## Add new legal edges to solution set

add_partition<-function(el,el_blocked,r,rows,v_top,n){
  
  print("Rufe add_partition mit edgelist_blocked:")
  print(el_blocked)
  
  
  wegeSuche_count_temp<-0
  el_temp<-el_blocked[which(el_blocked$parent==0),]
  # rows<-which(r$partition==1) # Je nach dem was Jordis befielt: Set indices of covering partition
  
  
  ## Solange meine Partition noch nicht vollständig ist, mache Wegesuche für neue Variante durch die blocked systras, 
  ## die an mind. 1 Tag gültig ist
  part_id<-max(r$partition[which(r$fahrlage %in% r$fahrlage[rows])])+1
  new_part<-c()
  bits_flg<-as.integer(r[i,11:(10+n)])
  
  
  
  for(i in 1:nrow(r)){
    if(i %in% rows){
      print(paste("add-partition-iteration: ",i))
      
      l<-1
      
      while(T){
        var_id<-max(r$id)+l
        l<-l+1
        wegeSuche_count_temp<-wegeSuche_count_temp+1

        try(spath<-bellmanford_bitconstr(el_temp,v_top,bits_flg,1,n))
        if(length(spath)==0){
          print("Es konnte kein Weg mehr gefunden werden mit der aktuellen Bitleiste")
          break
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
        print("Suche auf folgedem eingeshcränktem Graphen nach Wegen")
        print(el_temp)
        el_temp[,7:(6+n)]<-as.matrix(el_temp[,7:(6+n)]) %*% diag(bits_flg-spath$bits)
        
        # Update appendix of r data frame that contains the varianten of the current partition
        if(!is.null(new_part)){
          new_part<-rbind(new_part,new_var)
        }else{
          new_part<-new_var
        }
        
        # Update the bits that are still missing
        bits_flg<-bits_flg-spath$bits
        
        # Exit if bit cover is found
        if(sum(bits_flg)==0){
          break
        }
      }
      
      # If covering parition has been found, 
      if(sum(bits_flg)==0){
        
        # (Make sure the partition has the right order so that we can compare bits to other partitions)
        for(i in n:1){
          new_part<-new_part[order(-new_part[,10+i]),]
        }
        print("New partition:")
        print(new_part)
        
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
          
          # then update r data frame
          r<-rbind(r,new_part)
          
        }else{
          
          # if only new paths have been found
          # then just update the res column for this partition
          j<-1
          for(k in var_idx){
            print("Union:")
            print(r[k,])
            print(as.character(new_part$res[j]))
            
            r$res[k]<-paste(r$res[k],as.character(new_part$res[j]),sep=", ")
            j<-j+1
          }
          
        }
      }
    }
  }
  ### Separiere ggf. von verschiedenen varianten der neuen partition genutzte trassen
  #trassen<-as.integer(unlist(strsplit(as.character(new_part$res), split=", ")))
  ## mach das natürlich nur, falls mehr als 1 variante in der partition ist
  #if(nrow(new_part)>1){
  #  for(t in trassen){
  #    for(k in 1:(nrow(new_part)-1)){
  #      print("summe")
  #      print(new_part[k,11:(10+n)])
  #      print(el[which(el$id == t),7:(6+n)])
  #      if(t %in% as.integer(unlist(strsplit(as.character(new_part$res[k]), split=", "))) &
  #         sum(new_part[k,11:(10+n)]) < sum(el[which(el$id == t),7:(6+n)])){
  #        
  #        # Kommt die Trasse in einer zweiten variante der partition vor?
  #        second<-FALSE
  #        for(j in (k+1):nrow(new_part)){
  #          if(t %in% as.integer(unlist(strsplit(as.character(new_part$res[j]), split=", ")))){
  #            second<-TRUE
  #          }
  #        }
  #        if(second==TRUE){
  #          ##Dann Splitte all ihre Children!
  #          print("Zu Splitten sind die Children der Systra:")
  #          print(t)
  #          
  #          ## Suche die Children, die mit dem zeitfensters der variante gültigkeiten überschneiden
  #          ## um diese children dann zu zerteilen
  #          ## to do: zerteile das child dann direkt für alle varianten, die das child schneiden
  #          ## und update deren (=von den varianten) lösungsräume
  #          children<-c()
  #          for(s in el$id[which(el$parent == t | (el$beenparent==0 & el$id == t))]){
  #            if(!all(new_part[k,(11:(10+n))]-el[which(el$id==s),(7:(6+n))]!=0)){
  #              children<-c(children,s)
  #            }
  #          }            
  #          print("mit den Ids:")
  #          print(children)
  #          for(s in children){
  #            print("Jetzt ist folgendes child dran")
  #            print(s)
  #            
  #            ## Suche wieder nach Varianten, die dieses Child splitten würden
  #            vars<-c()
  #            for(l in 1:nrow(new_part)){
  #              if(!all(new_part[l,(11:(10+n))]-el[which(el$id==s),(7:(6+n))]!=0)){
  #                vars<-c(vars,l)
  #              }
  #            }
  #            
  #            ## Finde die eindeutigen Spalten, in die das child gesplitten werden muss
  #            ## das sind die Gültigkeiten der Varianten mal die Gültigkeit des Childs
  #            ## zusätzlich gibt es noch die "rest"-trasse, die die Gültigkeit der Children
  #            ## hat, die keine der varianten braucht
  #            sum_guelt_var<-apply(new_part[vars,11:(10+n)],1,max)
  #            rest_der_childtrasse<-el[which(el$id==s),(7:(6+n))] * sum_guelt_var
  #            A<-as.matrix(rbind(new_part[vars,11:(10+n)],rest_der_childtrasse))
  #            B<-diag(as.numeric(el[which(el$id==s),(7:(6+n))]))
  #            
  #            grandchildren<-unique(A %*% B, MARGIN = 2)
  #            
  #            print("und das hat die grandchrildren")
  #            print(grandchildren)
  #            
  #            ## Splitte nach jedem Grandchild
  #            el$beenparent[which(el$id==s)]<-1
  #            
  #            for(m in 1:ncol(grandchildren)){
  #              
  #              # Füge neuen Grandchild hinzu...
  #              s_prime<-el[which(el$id==s),]
  #              s_prime$id<-nrow(el)+1
  #              s_prime$beenparent[1]<-0
  #              s_prime$parent<-s
  #              s_tage_prime<-grandchildren[,m]
  #              s_prime[1,7:(6+n)]<-s_tage_prime
  #              el<-rbind(el,s_prime)
  #              
  #            }
  #            
  #            ## Update Lösungsraum aller Fahrlagenvarianten, die zuvor das Child s benutzt hatten
  #            
  #            # Merke neue edge ids
  #            ids_neu<-el$id[(nrow(el)-1):nrow(el)]
  #            
  #            for(i in 1:nrow(r)){
  #              if(s %in% as.integer(unlist(strsplit(r$res[i], split=", ")))){
  #                
  #                # Lösche geschnittene Systemtrasse
  #                res_neu<-as.integer(unlist(strsplit(r$res[i], split=", ")))[which(as.integer(unlist(strsplit(r$res[i], split=", ")))!=s)]
  #                
  #                # Füge Grandchildren hinzu
  #                indx<-rep(FALSE,length(grandchildren))
  #                for(l in 1:length(grandchildren)){
  #                  if(length(which(r[i,11:(10+n)]==1 & el[which(el$id==ids_neu[l]),7:(6+n)]==1))>0){
  #                    indx[l]<-TRUE
  #                  }
  #                }
  #                res_neu<-c(res_neu,ids_neu[indx])
  #                r$res[i]<-toString(res_neu)
  #              }
  #            }
  #          }
  #        }
  #      }
  #    }
  #  }    
  #}
  #
  ## Update parents: Wir speichern ja nur Wurzel Systemtrassen im Baum (keine Zwischenparents)
  while(length(which(el$parent[which(el$id %in% el$parent[which(el$parent!=0)])]!=0))>0){
    for(k in 1:nrow(el)){
      if(el$parent[k]!=0){
        if(el$parent[which(el$id == el$parent[k])]!=0){
          el$parent[k]<-el$parent[which(el$id == el$parent[k])]
        }
      }
    }
  }
  
  ## Lösche doppelt gespeicherte legale Trassen
  for(i in 1:nrow(r)){
    ints<-as.integer(unlist(strsplit(r$res[i], split=", ")))
    r$res[i]<-toString(unique(ints))
  }
  print(r)
  print(el)
  
  
  ## Return
  out<-list()
  out$el<-el
  out$r<-r
  out$wegeSuche_count<-wegeSuche_count_temp
  
  print("Ende add_partition")
  return(out)
}
