## Mikro Konflikte

mikro_konflikte<-function(el,r,makro){
  
 
  makro_indx<-which(unique(r$fahrlage) %in% makro)

  # Stelle IP auf
  enc<-encode(el,r)
  A<-enc$A
  dir<-enc$dir
  b<-enc$b
  omega<-enc$omega
  
  
  ## Suche erst mal maximales Set von Constraints sodass Problem lösbar ist
  A_max<-cbind(A,rbind(diag(-(nrow(r)),nrow(el)),matrix(0,nrow(A)-nrow(el),nrow(el))))
  dir_max<-dir
  b_max<-b
  omega_max<-c(omega,rep(1,nrow(el)))
  
  res_max<-lp(direction="min", objective.in=omega_max, const.mat=A_max, const.dir=dir_max, const.rhs=b_max, all.bin=TRUE)$solution[(length(omega)+1):length(omega_max)]
  
  
  ## Füge eine Constraint hinzu als ersten Kandidaten für ein MUS 
  ## Die Constraints entsprechen natürlich nur Systemtrassen, die auch
  ## von irgendeiner Fahrlagenvariante des Makrokonflikts benutzt werden
  ## da sie sonst sowieso nicht in einer Lösung Teil sind
  
  mus<-res_max
  mus[which(res_max==0)]<-rep(1,length(which(res_max==0)))
  mus[which(res_max==1)]<-rep(0,length(which(res_max==1)))
  mus[which(res_max==1)[1]]<-1
  
  ## Nehme so lange Constraints aus dem Kandidaten, bis all seine Teilmengen lösbar sind
  terminate<-FALSE
  
  while(terminate==FALSE){
    k_count<-0
    mus_size<-sum(mus)
    subsets<-combn(which(mus==1),mus_size-1,simplify = F)
    
    ## Für jedes Subset der Größe eins kleiner als der Kandidat, prüfe feasibility
    for(j in subsets){
      A_submikro<-rbind(A[j,],A[nrow(el)+makro_indx,],A[(nrow(el)+length(unique(r$fahrlage))+1):nrow(A),])
      dir_submikro<-c(dir[j],dir[nrow(el)+makro_indx],dir[(nrow(el)+length(unique(r$fahrlage))+1):length(dir)])
      b_submikro<-c(b[j],b[nrow(el)+makro_indx],b[(nrow(el)+length(unique(r$fahrlage))+1):length(b)])
      
      ## Ist ein Subset feasible, zähle counter
      if(lp(direction="max", objective.in=omega, const.mat=A_submikro, const.dir=dir_submikro, const.rhs=b_submikro, all.bin=TRUE)$status==0){
        k_count<-k_count+1
      }else{
        
        ## Ist ein Subset infeasible, wird das unser neuer Kandidat
        mus<-rep(0,length(mus))
        mus[j]<-rep(1,length(j))
        break
      }
    }
    
    ## Wenn der Zähler sagt, dass alle Subsets feasible sind, haben wir ein MUS gefunden
    if(k_count==mus_size){
      terminate<-TRUE
    }
  }
  
  ## Return MUS
  return(as.integer(el$id[which(mus==1)]))
  
}