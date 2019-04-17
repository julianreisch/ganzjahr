block_trassen<-function(el,r,makro,mikro,n){
  makro_inds<-which(r$id %in% makro)
  mikro_inds<-which(el$id %in% mikro)
  mikro_parents_inds<-which(el$id %in% el$parent[which(el$id %in% mikro)])
  
  ## An welchen Tagen fahren Fahrlagen aus dem Makrokonflikt gleichermaßen?
  konflikt_zeit<-sapply(r[makro_inds,4:(3+n)],sum)>1
  
  ## Sperre Trassen aus dem Mikrokonflikt und deren parents für diese Tage
  el_blocked<-el
  el_blocked[mikro_inds,(6:(5+n))[which(konflikt_zeit==TRUE)]]<-matrix(0,length(mikro),sum(konflikt_zeit))
  el_blocked[mikro_parents_inds,(6:(5+n))[which(konflikt_zeit==TRUE)]]<-matrix(0,length(mikro_parents_inds),sum(konflikt_zeit))
  
  ## Return
  return(el_blocked)
}