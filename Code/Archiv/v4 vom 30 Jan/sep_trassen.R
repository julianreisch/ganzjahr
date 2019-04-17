## Teile Systemtrassen

## Check pro Systemtrassen im Mikrokonflikt, ob es ein Paar Fahrlagenvarianten aus dem Makrokonflikt gibt
## bei denen es ausreicht die Systemtrasse zu teilen, damit beide die Trasse nutzen können
## und füge die geteilten Systrassen dann der Lösungsmenge der beiden Fahrlagen 

sep_trassen<-function(el,r,n,mikro,makro){
 
  i<-1
  j<-2
  p<-1
  q<-1
  terminate<-FALSE
  for(k in 1:length(mikro)){
    # Für alle Paare von Fahrlagen im Makrokonflikt
    while(i <= (length(makro)-1) & terminate==FALSE){
      while(j <= length(makro) & terminate==FALSE){
        
        # Für alle Paare von (gültigen) Fahrlagenvarianten der Fahrlagen
        while(p <= length(unique(r$id[which(r$fahrlage==makro[i] & r$valid==1)])) & terminate==FALSE){
          while(q <= length(unique(r$id[which(r$fahrlage==makro[j] & r$valid==1)])) & terminate==FALSE){
            
            p_index<-which(r$id %in% r$id[which(r$fahrlage==makro[i] & r$valid==1)])[p]
            q_index<-which(r$id %in% r$id[which(r$fahrlage==makro[j] & r$valid==1)])[q]
            
            # Haben die beiden Fahrlagenvarianten überhaupt die Mikrokonflkttrasse in ihrem Lösungsraum?
            if(mikro[k] %in% intersect(as.integer(unlist(strsplit(r$res[p_index], split=", "))),
                                       as.integer(unlist(strsplit(r$res[q_index], split=", "))))){
              # Hat das Paar von Fahrlagenvarianten keinen Schnitt?
              if(all(sapply(r[c(p_index,q_index),10:(9+n)],sum)-sapply(el[which(el$id==mikro[k]),7:(6+n)],sum)<=0)){
                
                
                el$beenparent[which(el$id==mikro[k])]<-1
                
                # Füge Teil der ersten Fahrlage als neue, kürzere Systemtrasse hinzu...
                s_1<-data.frame(id=nrow(el)+1,von=el[which(el$id==mikro[k]),]$von,bis=el[which(el$id==mikro[k]),]$bis,
                                weight=el[which(el$id==mikro[k]),]$weight,parent=which(el$id==mikro[k]),beenparent=0)
                s_1<-cbind(s_1,r[p_index,10:(n+9)])
                el<-rbind(el,s_1)
                
                # ... und Teil der zweiten ...
                s_2<-data.frame(id=nrow(el)+1,von=el[which(el$id==mikro[k]),]$von,bis=el[which(el$id==mikro[k]),]$bis,
                                weight=el[which(el$id==mikro[k]),]$weight,parent=mikro[k],beenparent=0)
                s_2<-cbind(s_2,r[q_index,10:(n+9)])
                el<-rbind(el,s_2)
                
                # Merke neue edge ids
                ids_neu<-el$id[(nrow(el)-1):nrow(el)]
                
                # ... und falls noch eine Resttrasse übrig bleibt, füge auch diese hinzu
                if(!all(sapply(r[c(p_index,q_index),10:(n+9)],sum)-sapply(el[which(el$id==mikro[k]),7:(6+n)],sum)>=0)){
                  tage<-which(sapply(r[c(p_index,q_index),10:(n+9)],sum)-sapply(el[mikro[k],7:(6+n)],sum)<0)
                  s_rest<-el[1,]
                  s_rest[1,1:6]<-c(nrow(el)+1,el[which(el$id==mikro[k]),]$von,el[which(el$id==mikro[k]),]$bis,
                                   el[which(el$id==mikro[k]),]$weight,parent=mikro[k],beenparent=0)
                  s_rest[1,7:(6+n)]<-rep(0,n)
                  s_rest[1,6+tage]<-rep(1,length(tage))
                  el<-rbind(el,s_rest)
                  
                  
                  ids_neu<-c(ids_neu,nrow(el))
                }
                
                # Update die Lösungsräume der Fahrlagenvarianten, die diese Systemtrasse benutzt hatten
                e_id<-mikro[k]
                
                
                for(i in 1:nrow(r)){
                  if(e_id %in% as.integer(unlist(strsplit(r$res[i], split=", ")))){
                    
                    # Lösche geschnittene Systemtrasse
                    res_neu<-as.integer(unlist(strsplit(r$res[i], split=", ")))[which(as.integer(unlist(strsplit(r$res[i], split=", ")))!=e_id)]
                    
                    # Füge s_1,s_2 und/oder s_rest hinzu
                    indx<-c(FALSE,FALSE,FALSE)
                    if(length(which(r[i,10:(9+n)]==1 & s_1[7:(6+n)]==1))>0){
                      indx[1]<-TRUE
                    }
                    if(length(which(r[i,10:(9+n)]==1 & s_2[7:(6+n)]==1))>0){
                      indx[2]<-TRUE
                    }
                    if(length(ids_neu)==3){ # Nur falls s_rest auch wirklich existiert
                      if(length(which(r[i,10:(9+n)]==1 & s_rest[7:(6+n)]==1))>0){
                        indx[3]<-TRUE
                      }else{
                        indx[3]<-FALSE
                      }
                    }
                    
                    
                    res_neu<-c(res_neu,ids_neu[indx])
                    r$res[i]<-toString(res_neu)
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
