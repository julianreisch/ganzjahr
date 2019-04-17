block_trassen<-function(el,r,makro,mikro,n){
  
  el_blocked<-el
  
  ## Für jede Systemtrasse im Mikrokonflikt
  for(k in 1:length(mikro)){
    
    ## Aggregiere Konfliktzeiträume der Fahrlagenvarianten, die diese Systemtrasse nutzen und aktiv sind
    r_agg<-r[which(r$fahrlage %in% makro & r$valid==1),]
    agg_indx<-c()
    
    for(i in 1:nrow(r_agg)){
      if(mikro[k] %in% as.integer(unlist(strsplit(r_agg$res[i], split=", ")))){
        agg_indx<-c(agg_indx,i)  
      }
    }
    
    r_agg<-r_agg[agg_indx,]
    
    
    ## Bestimme Konfliktzeitraum der Systemtrasse: 
    ## Pro Fahrlage im Makrokonflikt, bestimme, an welchen Tagen mindestens eine der oben aggregierten Fahrlagenvarianten
    ## die Systemtrasse nutzt
    M<-matrix(0,length(makro),n)
    for(i in 1:length(makro)){
      M[i,1:ncol(M)]<-apply(r_agg[which(r_agg$fahrlage==makro[i]),11:(10+n)],2,sum)
    }
    M<-(M>0)
    
    ## Der Konfliktzeitraum ist jetzt all die Tage, an denen mindestens zwei Fahrlagen je mindestens eine Variante besitzen,
    ## die diese Systemtrasse an dem Tag nutzen
    konflikt_zeit<-apply(M,2,sum)>1
    
    ## Blockiere nun diese Systemtrasse und ihren parent auf dem eben berechneten Konfliktzeitraum
    mikro_indx<-which(el$id %in% mikro[k])
  
    el_blocked[mikro_indx,(7:(6+n))[which(konflikt_zeit==TRUE)]]<-matrix(0,length(mikro_indx),sum(konflikt_zeit))
    parents_indx<-which(el$id %in% el$parent[mikro_indx])
    if(length(parents_indx)>0){
      el_blocked[parents_indx,(7:(6+n))[which(konflikt_zeit==TRUE)]]<-matrix(0,length(parents_indx),sum(konflikt_zeit))
    }
    
  }
  
  ## Return
  return(el_blocked)
}