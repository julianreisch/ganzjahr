max_SAT<-function(el,r,makro,A,dir,b,omega){
  
  ## Indices der im Makrokonflikt befindlichen Fahrlagen
  indx<-which(unique(r$fahrlage) %in% makro)
  indx_else<-which(!unique(r$fahrlage) %in% makro)
  
  ## Maximale Menge von Fahrlagen im Makrokonflikt, die gleichzeitig erfüllt werden können
  A_maxsat<-cbind(A,rbind(matrix(0,nrow(A),length(indx))))
  A_maxsat[nrow(el)+indx,(ncol(A)+1):ncol(A_maxsat)]<-diag(1,length(indx))
  A_maxsat[nrow(el)+indx_else,1:ncol(A_maxsat)]<-matrix(0,length(indx_else),ncol(A_maxsat))
  
  dir_maxsat<-dir
  dir_maxsat[nrow(el)+indx_else]<-rep("=",length(indx_else))
  
  b_maxsat<-b
  b_maxsat[nrow(el)+indx_else]<-rep(0,length(indx_else))
  
  omega_maxsat<-c(omega,rep(1,length(indx)))
  
  res_maxsat<-lp(direction="min", objective.in=omega_maxsat, const.mat=A_maxsat, const.dir=dir_maxsat, const.rhs=b_maxsat, 
                 all.bin=TRUE)$solution[(length(omega)+1):length(omega_maxsat)]
  
  
  ## Return
  return(r[which(r$fahrlage %in% unique(r$fahrlage)[indx[res_maxsat]] | !(r$fahrlage %in% makro)),])  
}

