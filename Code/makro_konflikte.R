## Makro Konflikte

makro_konflikte<-function(el,r){
  
  # Stelle IP auf
  enc<-encode(el,r)
  A<-enc$A
  dir<-enc$dir
  b<-enc$b
  omega<-enc$omega
  
  
  ## Suche erst mal maximales Set von Constraints sodass Problem l�sbar ist
  A_max<-cbind(A,rbind(matrix(0,nrow(el),length(unique(r$fahrlage))),
                       diag(1,length(unique(r$fahrlage))),
                       matrix(0,nrow(A)-length(unique(r$fahrlage))-nrow(el),length(unique(r$fahrlage)))))
  dir_max<-dir
  b_max<-b
  omega_max<-c(omega,rep(1,length(unique(r$fahrlage))))
  
  res_max<-lp(direction="min", objective.in=omega_max, const.mat=A_max, const.dir=dir_max, const.rhs=b_max, all.bin=TRUE)$solution[(length(omega)+1):length(omega_max)]
  
  
  ## F�ge eine Constraint hinzu als ersten Kandidaten f�r ein MUS
  mus<-res_max
  mus[which(res_max==0)]<-rep(1,length(which(res_max==0)))
  mus[which(res_max==1)]<-rep(0,length(which(res_max==1)))
  mus[which(res_max==1)[1]]<-1
  
  ## Nehme so lange Constraints aus dem Kandidaten, bis all seine Teilmengen l�sbar sind
  terminate<-FALSE
  
  while(terminate==FALSE){
    k_count<-0
    mus_size<-sum(mus)
    subsets<-combn(which(mus==1),mus_size-1,simplify = F)
    
    ## F�r jedes Subset der Gr��e eins kleiner als der Kandidat, pr�fe feasibility
    for(j in subsets){
      A_submakro<-rbind(A[1:nrow(el),],A[nrow(el)+j,],A[(nrow(el)+length(unique(r$fahrlage))+1):nrow(A),])
      dir_submakro<-c(dir[1:nrow(el)],dir[nrow(el)+j],dir[(nrow(el)+length(unique(r$fahrlage))+1):length(dir)])
      b_submakro<-c(b[1:nrow(el)],b[nrow(el)+j],b[(nrow(el)+length(unique(r$fahrlage))+1):length(b)])
      
      ## Ist ein Subset feasible, z�hle counter
      if(lp(direction="max", objective.in=omega, const.mat=A_submakro, const.dir=dir_submakro, const.rhs=b_submakro, all.bin=TRUE)$status==0){
        k_count<-k_count+1
      }else{
        
        ## Ist ein Subset infeasible, wird das unser neuer Kandidat
        mus<-rep(0,length(mus))
        mus[j]<-rep(1,length(j))
        break
      }
    }
    
    ## Wenn der Z�hler sagt, dass alle Subsets feasible sind, haben wir ein MUS gefunden
    if(k_count==mus_size){
      terminate<-TRUE
    }
  }
  
  ## Return MUS
  return(unique(r$fahrlage)[mus==1])
  
}
