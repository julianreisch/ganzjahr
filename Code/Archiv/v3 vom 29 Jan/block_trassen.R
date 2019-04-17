block_trassen<-function(el,r,makro,mikro,n){
  
  mikro_indx<-which(el$id %in% mikro)
  
  # Sperre alle Systemtrassen im Mikrokonflikt an all jenen Tagen, an denen aus mindestens zwei Fahrlagen 
  # je eine Fahrlagenvariante existiert, die an diesem Tag fahren will
  M<-matrix(0,length(makro),n)
  for(i in 1:length(makro)){
    M[i,1:ncol(M)]<-apply(r[which(r$fahrlage==makro[i] & r$valid==1),10:(9+n)],2,sum)
  }
  konflikt_zeit<-apply(M,2,sum)>1
  
  ## Sperre Trassen aus dem Mikrokonflikt und deren parents für diese Tage
  el_blocked<-el
  el_blocked[mikro_indx,(7:(6+n))[which(konflikt_zeit==TRUE)]]<-matrix(0,length(mikro),sum(konflikt_zeit))
  parents_indx<-which(el$id %in% el$parent[mikro_indx])
  if(length(parents_indx)>0){
    el_blocked[parents_indx,(7:(6+n))[which(konflikt_zeit==TRUE)]]<-matrix(0,length(parents_indx),sum(konflikt_zeit))
  }

  ## Return
  return(el_blocked)
}