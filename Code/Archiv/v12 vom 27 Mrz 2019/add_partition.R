## Build Graph
## Add new legal edges to solution set

add_partition<-function(el,el_blocked,r,rows,v_top,n){
  
  print("Rufe add_partition")
  
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
        print(el_temp[,7:(6+n)])
        print(as.matrix(el_temp[,7:(6+n)]))
        print(diag(spath$bits))
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
  ## Separiere ggf. von verschiedenen varianten der neuen partition genutzte trassen
  trassen<-as.integer(unlist(strsplit(as.character(new_part$res), split=", ")))
  # mach das natürlich nur, falls mehr als 1 variante in der partition ist
  if(nrow(new_part)>1){
    for(s in trassen){
      for(k in 1:(nrow(new_part)-1)){
        print("summe")
        print(new_part[k,11:(10+n)])
        print(el[which(el$id == s),7:(6+n)])
        if(s %in% as.integer(unlist(strsplit(as.character(new_part$res[k]), split=", "))) &
           sum(new_part[k,11:(10+n)]) < sum(el[which(el$id == s),7:(6+n)])){
          
          # Kommt die Trasse in einer zweiten variante der partition vor?
          second<-FALSE
          for(j in (k+1):nrow(new_part)){
            if(s %in% as.integer(unlist(strsplit(as.character(new_part$res[j]), split=", ")))){
              second<-TRUE
            }
          }
          if(second==TRUE){
            print("Zu Splitten:")
            print(s)
            ##Dann Splitte sie!
            ### Copy from set trassen
            #########
            
            el$beenparent[which(el$id==s)]<-1
            
            # Füge Teil der ersten Fahrlage als neue, kürzere Systemtrasse hinzu...
            s_1<-el[which(el$id==s),]
            s_1$id<-nrow(el)+1
            s_1$beenparent[1]<-0
            s_1$parent<-s
            s_tage_1<-new_part[k,11:(10+n)]
            s_1[1,7:(6+n)]<-s_tage_1
            el<-rbind(el,s_1)
            
            # ... und Teil der zweiten ...
            s_2<-el[which(el$id==s),]
            s_2$id<-nrow(el)+1
            s_2$beenparent[1]<-0
            s_2$parent<-s
            s_tage_2<-abs(new_part[k,11:(10+n)]-1)
            s_2[1,7:(6+n)]<-s_tage_2
            el<-rbind(el,s_2)
            
            # Merke neue edge ids
            ids_neu<-el$id[(nrow(el)-1):nrow(el)]
            
            # Update die Lösungsräume der Fahrlagenvarianten, die diese Systemtrasse benutzt hatten
            e_id<-s
            
            
            for(i in 1:nrow(r)){
              if(e_id %in% as.integer(unlist(strsplit(r$res[i], split=", ")))){
                
                # Lösche geschnittene Systemtrasse
                res_neu<-as.integer(unlist(strsplit(r$res[i], split=", ")))[which(as.integer(unlist(strsplit(r$res[i], split=", ")))!=e_id)]
                
                # Füge s_1,s_2 und/oder s_rest hinzu
                indx<-c(FALSE,FALSE,FALSE)
                if(length(which(r[i,11:(10+n)]==1 & s_1[1,7:(6+n)]==1))>0){
                  indx[1]<-TRUE
                }
                if(length(which(r[i,11:(10+n)]==1 & s_2[1,7:(6+n)]==1))>0){
                  indx[2]<-TRUE
                }
                if(length(ids_neu)==3){ # Nur falls s_rest auch wirklich existiert
                  if(length(which(r[i,11:(10+n)]==1 & s_rest[1,7:(6+n)]==1))>0){
                    indx[3]<-TRUE
                  }else{
                    indx[3]<-FALSE
                  }
                }
                
                
                res_neu<-c(res_neu,ids_neu[indx])
                r$res[i]<-toString(res_neu)
              }
            }
            
            #######
            
          }
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
  return(out)
}
