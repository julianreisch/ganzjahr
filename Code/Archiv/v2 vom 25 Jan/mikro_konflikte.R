## Mikro Konflikte

mikro_konflikte<-function(el,r,makro,A,dir,b){
  makro_inds<-which(r$id %in% makro)
  k_count<-0
  i<-1
  term<-FALSE
  
  while(i<=nrow(el) & term==FALSE){
    subsets<-combn(1:nrow(el),i,simplify = F)
    
    ## Welches Subset ist infeasible?
    for(k in subsets){
      A_mikro<-rbind(A[k,],A[nrow(el)+makro_inds,],A[(nrow(el)+nrow(r)+1):nrow(A),])
      dir_mikro<-c(dir[k],dir[nrow(el)+makro_inds],dir[(nrow(el)+nrow(r)+1):length(dir)])
      b_mikro<-c(b[k],b[nrow(el)+makro_inds],b[(nrow(el)+nrow(r)+1):length(b)])
      if(all(lp(direction="max", objective.in=omega, const.mat=A_mikro, const.dir=dir_mikro, const.rhs=b_mikro, all.bin=TRUE)$solution==0)){
        subsubsets<-combn(k,i-1,simplify = F)
        
        ## Sind alle Teilsubsets feasible?
        for(j in subsubsets){
          A_submikro<-rbind(A[j,],A[nrow(el)+makro_inds,],A[(nrow(el)+nrow(r)+1):nrow(A),])
          dir_submikro<-c(dir[j],dir[nrow(el)+makro_inds],dir[(nrow(el)+nrow(r)+1):length(dir)])
          b_submikro<-c(b[j],b[nrow(el)+makro_inds],b[(nrow(el)+nrow(r)+1):length(b)])
          if(!all(lp(direction="max", objective.in=omega, const.mat=A_submikro, const.dir=dir_submikro, const.rhs=b_submikro, all.bin=TRUE)$solution==0)){
            k_count<-k_count+1
          }
        }
        if(k_count==i){
          term<-TRUE
          break
        }
        k_count<-0
      }
    }
    i<-i+1
  }
  
  ## Return
  return(as.integer(el$id[k]))
}