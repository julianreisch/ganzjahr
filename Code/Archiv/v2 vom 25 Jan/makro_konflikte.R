## Makro Konflikte

makro_konflikte<-function(el,r,A,dir,b){
  k_count<-0
  for(i in 1:nrow(r)){
    subsets<-combn(1:nrow(r),i,simplify = F)
    
    ## Welches Subset ist infeasible?
    for(k in subsets){
      
      ## Betrifft Subset Fahrlagenvarianten derselben Fahrlage? (dann next)
      if(nrow(unique(r[k,c("partition","fahrlage")]))==i){
        A_makro<-rbind(A[1:nrow(el),],A[nrow(el)+k,],A[(nrow(el)+nrow(r)+1):nrow(A),])
        dir_makro<-c(dir[1:nrow(el)],dir[nrow(el)+k],dir[(nrow(el)+nrow(r)+1):length(dir)])
        b_makro<-c(b[1:nrow(el)],b[nrow(el)+k],b[(nrow(el)+nrow(r)+1):length(b)])
        
        if(all(lp(direction="max", objective.in=omega, const.mat=A_makro, const.dir=dir_makro, const.rhs=b_makro, all.bin=TRUE)$solution==0)){
          subsubsets<-combn(k,i-1,simplify = F)
        
          ## Sind alle Teilsubsets feasible?
          for(j in subsubsets){
            A_submakro<-rbind(A[1:nrow(el),],A[nrow(el)+j,],A[(nrow(el)+nrow(r)+1):nrow(A),])
            dir_submakro<-c(dir[1:nrow(el)],dir[nrow(el)+j],dir[(nrow(el)+nrow(r)+1):length(dir)])
            b_submakro<-c(b[1:nrow(el)],b[nrow(el)+j],b[(nrow(el)+nrow(r)+1):length(b)])
            if(!all(lp(direction="max", objective.in=omega, const.mat=A_submakro, const.dir=dir_submakro, const.rhs=b_submakro, all.bin=TRUE)$solution==0)){
              k_count<-k_count+1
            }
          }
          if(k_count==i){
            break
          }
          k_count<-0
        }
      }
    }
  }
  
  ## Return
  return(r$id[k])
}
