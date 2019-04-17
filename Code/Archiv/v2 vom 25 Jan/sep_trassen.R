## Teile Systemtrassen

## Check pro Systemtrassen im Mikrokonflikt, ob es ein Paar Fahrlagenvarianten aus dem Makrokonflikt gibt
## bei denen es ausreicht die Systemtrasse zu teilen, damit beide die Trasse nutzen können
## und füge die geteilten Systrassen dann der Lösungsmenge der beiden Fahrlagen 

sep_trassen<-function(el,r,n,mikro){
 
  i<-1
  j<-2
  terminate<-FALSE
  for(k in 1:length(mikro)){
    while(i <= (length(makro)-1) & terminate==FALSE){
      while(j <= length(makro) & terminate==FALSE){
        
        # Hat das Paar von Fahrlagen keinen Schnitt?
        if(all(sapply(r[which(r$id %in% makro[c(i,j)]),10:(9+n)],sum)-sapply(el[which(el$id==mikro[k]),6:(5+n)],sum)<=0)){
          
          # Füge Teil der ersten Fahrlage als neue, kürzere Systemtrasse hinzu...
          s_1<-data.frame(id=nrow(el)+1,von=el[which(el$id==mikro[k]),]$von,bis=el[which(el$id==mikro[k]),]$bis,
                          weight=el[which(el$id==mikro[k]),]$weight,parent=which(el$id==mikro[k]))
          s_1<-cbind(s_1,r[i,10:(n+9)])
          el<-rbind(el,s_1)
          r$res[i]<-paste(r$res[i],", ",toString(nrow(el)),sep="")
          
          # ... und Teil der zweiten ...
          s_2<-data.frame(id=nrow(el)+1,von=el[which(el$id==mikro[k]),]$von,bis=el[which(el$id==mikro[k]),]$bis,
                          weight=el[which(el$id==mikro[k]),]$weight,parent=mikro[k])
          s_2<-cbind(s_2,r[j,10:(n+9)])
          el<-rbind(el,s_2)
          r$res[j]<-paste(r$res[j],", ",toString(nrow(el)),sep="")
          
          # Merke neue edge ids
          ids_neu<-el$id[(nrow(el)-1):nrow(el)]
          
          # ... und falls noch eine Resttrasse übrig bleibt, füge auch diese hinzu
          if(!all(sapply(r[which(r$id %in% makro[c(i,j)]),10:(n+9)],sum)-sapply(el[which(el$id==mikro[k]),6:(5+n)],sum)>=0)){
            tage<-which(sapply(r[which(r$id %in% makro[c(i,j)]),10:(n+9)],sum)-sapply(el[mikro[k],6:(5+n)],sum)<0)
            s_rest<-el[1,]
            s_rest[1,1:5]<-c(nrow(el)+1,el[which(el$id==mikro[k]),]$von,el[which(el$id==mikro[k]),]$bis,
                             el[which(el$id==mikro[k]),]$weight,mikro[k])
            s_rest[1,6:(5+n)]<-rep(0,n)
            s_rest[1,5+tage]<-rep(1,length(tage))
            el<-rbind(el,s_rest)
            
            ids_neu<-c(ids_neu,nrow(el))
          }
          
          # Update die Lösungsräume der Fahrlagenvarianten, die diese Systemtrasse benutzt hatten
          e_id<-as.integer(el$id[which(el$id==mikro[k])])
          
          for(i in 1:nrow(r)){
            if(e_id %in% as.integer(unlist(strsplit(r$res[i], split=", ")))){
              
              # Lösche geschnittene Systemtrasse
              res_neu<-as.integer(unlist(strsplit(r$res[i], split=", ")))[which(as.integer(unlist(strsplit(r$res[i], split=", ")))!=e_id)]
              
              # Füge s_1,s_2 und/oder s_rest hinzu
              indx<-c(FALSE,FALSE,FALSE)
              if(length(which(r[i,10:(9+n)]==1 & s_1[6:(5+n)]==1))>0){
                indx[1]<-TRUE
              }
              if(length(which(r[i,10:(9+n)]==1 & s_2[6:(5+n)]==1))>0){
                indx[2]<-TRUE
              }
              if(length(which(r[i,10:(9+n)]==1 & s_rest[6:(5+n)]==1))>0){
                indx[3]<-TRUE
              }
              
              res_neu<-c(res_neu,ids_neu[indx])
              r$res[i]<-toString(res_neu)
            }
          }
          
          # Springe zur nächsten Systemtrasse im Mikrokonflikt
          terminate<-TRUE
          
        }
        j<-j+1
      }
      i<-i+1
      j<-i+1
    }
    i<-1
    j<-2
    terminate<-FALSE
  }  

  
  ## Return
  out<-list()
  out$el<-el
  out$r<-r
  return(out)
}
