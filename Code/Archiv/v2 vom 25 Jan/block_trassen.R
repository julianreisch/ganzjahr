block_trassen<-function(el,r,makro,mikro,n){
  
  makro_indx<-which(r$id %in% makro)
  mikro_indx<-which(el$id %in% mikro)

  ## An welchen Tagen fahren Fahrlagen aus dem Makrokonflikt gleichermaßen?
  konflikt_zeit<-sapply(r[makro_indx,10:(9+n)],sum)>1
  
  ## Sperre Trassen aus dem Mikrokonflikt und deren parents für diese Tage
  el_blocked<-el
  el_blocked[mikro_indx,(6:(5+n))[which(konflikt_zeit==TRUE)]]<-matrix(0,length(mikro),sum(konflikt_zeit))

  ## Return
  return(el_blocked)
}