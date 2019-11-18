## Teile Systemtrassen

## Check pro Systemtrassen im Mikrokonflikt, ob es ein Paar Fahrlagenvarianten aus dem Makrokonflikt gibt
## bei denen es ausreicht die Systemtrasse zu teilen, damit beide die Trasse nutzen können
## und füge die geteilten Systrassen dann der Lösungsmenge der beiden Fahrlagen 

sep_trassen<-function(el,r,n,mikro,makro){
  
  nrow_old<-nrow(el)
 
  i<-1
  if(length(makro)==1){
    j<-1
    lenmakro<-2
  }else{
    #j<-2 
    #auf 1 damit auch Fahrlagenvarianten einer Fahrlage eine Trasse aufteilen
    j<-1
    lenmakro<-length(makro)}
  p<-1
  q<-1
  terminate<-FALSE
  for(k in 1:length(mikro)){
    # Für alle (Paare von) Fahrlagen im Makrokonflikt
    while(i <= lenmakro-1 & terminate==FALSE){
      while(j <= lenmakro & terminate==FALSE){
        
        # Für alle Paare von (gültigen) Fahrlagenvarianten der Fahrlagen
        while(p <= length(unique(r$id[which(r$fahrlage==makro[i] & r$valid==1)])) & terminate==FALSE){
          while(q <= length(unique(r$id[which(r$fahrlage==makro[j] & r$valid==1)])) & terminate==FALSE){
            
            p_index<-which(r$id %in% r$id[which(r$fahrlage==makro[i] & r$valid==1)])[p]
            q_index<-which(r$id %in% r$id[which(r$fahrlage==makro[j] & r$valid==1)])[q]
            
            # Haben die beiden Fahrlagenvarianten überhaupt diese Mikrokonflkttrasse (k) in ihrem Lösungsraum?
            if(mikro[k] %in% intersect(as.integer(unlist(strsplit(r$res[p_index], split=", "))),
                                       as.integer(unlist(strsplit(r$res[q_index], split=", "))))){
              
              # Bestimme Gültigkeitszeitraum der Trasse
              gueltig<-which(el[which(el$id==mikro[k]),7:(6+n)]==1)
              
              print("Splitte Trasse für das Fahrlagenvariantenpaar:")
              print(r[c(p_index,q_index),])
              
              print("Die zu splittende Trasse ist:")
              print(mikro[k])
              print("Und hat den Gültigkeitszeitraum")
              print(gueltig)
              
              print("Der Schnitt der Gültigkeitszeiträume der Fahrlagenvarianten ist:")

              schnitt_var<-r[p_index,(11:(10+n))[gueltig]]*r[q_index,(11:(10+n))[gueltig]]
              print(schnitt_var)
              
              print("Und die Vereinigung der Gültigkeitszeiträume der Fahrlagenvarianten:")
              union_var<-r[p_index,(11:(10+n))[gueltig]]+r[q_index,(11:(10+n))[gueltig]]
              union_var<-sapply(as.numeric(union_var),max_1)
              print("max_1")
              print(max_1)
              print(union_var)
              
              ## Zerteile die Trasse nur, wenn sich die Varianten nicht überschneiden, aber den 
              ## Gültigkeitsraum der Trasse mit beiden FLG Var Gültigkietszeiträumen überschneidet
              if(all(schnitt_var==0) &
                 !all(el[which(el$id==mikro[k]),(7:(6+n))[gueltig]] * r[p_index,(11:(10+n))[gueltig]] == 0) &
                 !all(el[which(el$id==mikro[k]),(7:(6+n))[gueltig]] * r[q_index,(11:(10+n))[gueltig]] == 0)){
              
                
                # Füge Teil der ersten Fahrlage als neue, kürzere Systemtrasse hinzu...
                s_1<-el[which(el$id==mikro[k]),]
                s_1$id<-nrow(el)+1
                s_1$beenparent[1]<-0
                s_1$parent<-mikro[k]
                s_tage_1<-rep(0,n)
                s_tage_1[gueltig]<-as.integer(r[p_index,(11:(10+n))[gueltig]])
                s_1[1,7:(6+n)]<-s_tage_1
                
                if(sum(s_tage_1)>0){
                  el<-rbind(el,s_1)

                }

                # ... und Teil der zweiten ...
                s_2<-el[which(el$id==mikro[k]),]
                s_2$id<-nrow(el)+1
                s_2$beenparent[1]<-0
                s_2$parent<-mikro[k]
                s_tage_2<-rep(0,n)
                s_tage_2[gueltig]<-as.integer(r[q_index,(11:(10+n))[gueltig]])
                s_2[1,7:(6+n)]<-s_tage_2
                if(sum(s_tage_2)>0){
                  el<-rbind(el,s_2)
                  
                }                
                # Merke neue edge ids
                ids_neu<-el$id[(nrow(el)-1):nrow(el)]
                
                # ... und falls noch eine Resttrasse übrig bleibt, füge auch diese hinzu
                s_tage_rest<-rep(0,n)
                s_tage_rest[gueltig]<-rep(1,length(gueltig))
                s_tage_rest<-s_tage_rest-(s_tage_1+s_tage_2)
                
                if(!all(s_tage_rest==0)){
                  s_rest<-el[1,]
                  s_rest[1,1:4]<-c(nrow(el)+1,el[which(el$id==mikro[k]),]$von,el[which(el$id==mikro[k]),]$bis,
                                   el[which(el$id==mikro[k]),]$weight)
                  s_rest$parent<-mikro[k]
                  s_rest$beenparent<-0
                  s_rest[1,7:(6+n)]<-s_tage_rest
                  if(sum(s_tage_rest)>0){
                    el<-rbind(el,s_rest)
                    
                  }                  
                  ids_neu<-c(ids_neu,nrow(el))
                }
                
                # Falls nur eine (oder keine) extra Trasse hinzugekommen ist, dann wurde nicht wirklich was aufgeteilt
                stopifnot(nrow(el)>nrow_old+1)
                
                # Update die Lösungsräume der Fahrlagenvarianten, die diese Systemtrasse benutzt hatten
                e_id<-mikro[k]
                
                # Markiere, dass diese edge aufgetrennt worden ist
                el$beenparent[which(el$id==mikro[k])]<-1
                
                
                print("Gehe durch die solution spaces der FLG Var, um diese ggf. zu updaten:")
                for(i in 1:nrow(r)){
                  if(e_id %in% as.integer(unlist(strsplit(r$res[i], split=", ")))){
                    print("Update FLG Var:")
                    print(r$id[i])
                    print("alter solution space:")
                    print(r$res[i])
                    
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
                    
                    print("neuer solution space:")
                    print(r$res[i])
                  }
                }
                
                # Springe zur nächsten Systemtrasse im Mikrokonflikt
                terminate<-TRUE
              }
            }
            q<-q+1
          }
          q<-1
          p<-p+1
        }
        p<-1
        j<-j+1
      }
      i<-i+1
      j<-i+1
    }
    i<-1
    j<-2
    terminate<-FALSE
  }  
  
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
  
  
  
  ## Return
  out<-list()
  out$el<-el
  out$r<-r
  return(out)
}
