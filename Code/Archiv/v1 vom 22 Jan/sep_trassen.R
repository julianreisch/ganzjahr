## Teile Systemtrassen

## Check pro Systemtrassen im Mikrokonflikt, ob es ein Paar Fahrlagenvarianten aus dem Makrokonflikt gibt
## bei denen es ausreicht die Systemtrasse zu teilen, damit beide die Trasse nutzen können
## und füge die geteilten Systrassen dann der Lösungsmenge der beiden Fahrlagen 

sep_trassen<-function(el,r,n,mikro){
 
  ## Checke nur SysTras, die in ihrer gänze noch nie geteilt wurden
  checked_before<-which(mikro %in% unique(el$parent))
  if(length(checked_before)>0){
    to_check<-mikro[-checked_before]
  }else{to_check<-mikro}
  
  if(length(to_check>0)){
    k<-1
    i<-1
    j<-2
    terminate<-FALSE
    for(k in 1:length(mikro)){
      for(i in 1:(length(makro)-1)){
        for(j in (i+1):length(makro)){
          
          # Hat das Paar von Fahrlagen keinen Schnitt?
          if(all(sapply(r[which(r$id %in% makro[c(i,j)]),4:(3+n)],sum)-sapply(el[which(el$id==mikro[k]),6:(5+n)],sum)<=0)){
            
            # Füge Teil der ersten Fahrlage als neue, kürzere Systemtrasse hinzu...
            s_1<-data.frame(id=nrow(el)+1,von=el[which(el$id==mikro[k]),]$von,bis=el[which(el$id==mikro[k]),]$bis,
                            weight=el[which(el$id==mikro[k]),]$weight,parent=which(el$id==mikro[k]))
            s_1<-cbind(s_1,r[i,4:(n+3)])
            el<-rbind(el,s_1)
            r$res[i]<-paste(r$res[i],", ",toString(nrow(el)),sep="")
            
            # ... und Teil der zweiten ...
            s_2<-data.frame(id=nrow(el)+1,von=el[which(el$id==mikro[k]),]$von,bis=el[which(el$id==mikro[k]),]$bis,
                            weight=el[which(el$id==mikro[k]),]$weight,parent=mikro[k])
            s_2<-cbind(s_2,r[j,4:(n+3)])
            el<-rbind(el,s_2)
            r$res[j]<-paste(r$res[j],", ",toString(nrow(el)),sep="")
            
            # ... und falls noch eine Resttrasse übrig bleibt, füge auch diese hinzu
            if(!all(sapply(r[which(r$id %in% makro[c(i,j)]),4:(n+3)],sum)-sapply(el[which(el$id==mikro[k]),6:(5+n)],sum)>=0)){
              tage<-which(sapply(r[which(r$id %in% makro[c(i,j)]),4:(n+3)],sum)-sapply(el[mikro[k],6:(5+n)],sum)<0)
              s_rest<-el[1,]
              s_rest[1,1:5]<-c(nrow(el)+1,el[which(el$id==mikro[k]),]$von,el[which(el$id==mikro[k]),]$bis,
                               el[which(el$id==mikro[k]),]$weight,mikro[k])
              s_rest[1,6:(5+n)]<-rep(0,n)
              s_rest[1,5+tage]<-rep(1,length(tage))
              el<-rbind(el,s_rest)
            }
            
            ## Return
            out<-list()
            out$el<-el
            out$r<-r
            return(out)
          
          }
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
