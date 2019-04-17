encode<-function(el,r){
  ## Make Ids to Integers
  el$id<-as.integer(el$id)
  
  ## Encode Systemtrassen Clauses
  S_A<-matrix(0,nrow(el),nrow(r)+nrow(r)*nrow(el))
  S_dir<-rep("<=",nrow(el))
  S_b<-rep(1,nrow(el))
  
  for(i in 1:nrow(el)){
    S_A[i,nrow(r)+i+nrow(el)*(0:(nrow(r)-1))]<-rep(1,nrow(r))
  }
  
  #Encode Fahrlagen - Fulfill each Request
  R_A<-matrix(0,nrow(r),nrow(r)+nrow(r)*nrow(el))
  R_A[1:nrow(r),1:nrow(r)]<-diag(1,nrow(r))
  R_dir<-rep(">=",nrow(r))
  R_b<-rep(1,nrow(r))
  
  
  ## Encode Fahrlagen - Source/Sink
  QP_A<-matrix(0,nrow(r)*2,nrow(r)+nrow(r)*nrow(el))
  QP_dir<-rep("=",2*nrow(r))
  QP_b<-rep(0,2*nrow(r))
  
  for(i in 1:nrow(r)){
    QP_A[2*(i-1)+(1:2),i]<-c(-1,-1)
    
    # Exactly one Source
    ids<-el$id[which(el$von=="Q" & el$id %in% as.integer(unlist(strsplit(r$res[i], split=", "))))]
    QP_A[2*i-1,(i-1)*nrow(el)+nrow(r)+ids]<-matrix(1,1,length(ids))
    
    # Exactly one Sink
    ids<-el$id[which(el$bis=="S" & el$id %in% as.integer(unlist(strsplit(r$res[i], split=", "))))]
    QP_A[2*i,(i-1)*nrow(el)+nrow(r)+ids]<-matrix(1,1,length(ids))
  }
  
  ## Encode Fahrlagen - atMostOne Outgoing Edge
  O_A<-matrix(0,nrow(r)*2,nrow(r)+nrow(r)*nrow(el))
  O_dir<-rep("<=",nrow(r)*2)
  O_b<-rep(0,nrow(r)*2)
  
  for(i in 1:nrow(r)){
    O_A[2*(i-1)+(1:2),i]<-c(-1,-1)
    
    # Outgoing from A
    ids<-el$id[which(el$von=="A" & el$id %in% as.integer(unlist(strsplit(r$res[i], split=", "))))]
    O_A[2*i-1,(i-1)*nrow(el)+nrow(r)+ids]<-matrix(1,1,length(ids))
    
    # Outgoing from B
    ids<-el$id[which(el$von=="B" & el$id %in% as.integer(unlist(strsplit(r$res[i], split=", "))))]
    O_A[2*i,(i-1)*nrow(el)+nrow(r)+ids]<-matrix(1,1,length(ids))
  }
  
  ## Encode Fahrlagen - atMostOne Incoming Edge
  I_A<-matrix(0,nrow(r)*2,nrow(r)+nrow(r)*nrow(el))
  I_dir<-rep("<=",nrow(r)*2)
  I_b<-rep(0,nrow(r)*2)
  
  for(i in 1:nrow(r)){
    I_A[2*(i-1)+(1:2),i]<-c(-1,-1)
    
    # Outgoing from A
    ids<-el$id[which(el$bis=="A" & el$id %in% as.integer(unlist(strsplit(r$res[i], split=", "))))]
    I_A[2*i-1,(i-1)*nrow(el)+nrow(r)+ids]<-matrix(1,1,length(ids))
    
    # Outgoing from B
    ids<-el$id[which(el$bis=="B" & el$id %in% as.integer(unlist(strsplit(r$res[i], split=", "))))]
    I_A[2*i,(i-1)*nrow(el)+nrow(r)+ids]<-matrix(1,1,length(ids))
  }
  
  ## Encode Fahrlagen - Flow Conservation
  legal_trassen<-el$id[which(el$id %in% as.integer(unlist(strsplit(r$res, split=", "))))]
  legal_trassen_ohneSink<-intersect(legal_trassen,el$id[which(el$bis!="S")])
  n_legal_trassen<-length(legal_trassen)
  n_legal_trassen_ohneSink<-length(legal_trassen_ohneSink)
  sum_legal_trassen_fahrlage<-0
  n_legal_trassen_alle_fahrlagen<-length(as.integer(unlist(strsplit(r$res, split=", ")))[which(as.integer(unlist(strsplit(r$res, split=", ")))!=0)])
  
  F_A<-matrix(0,n_legal_trassen_alle_fahrlagen,nrow(r)+nrow(r)*nrow(el))
  F_dir<-rep(">=",n_legal_trassen_alle_fahrlagen)
  F_b<-rep(-1,n_legal_trassen_alle_fahrlagen)
  
  for(i in 1:nrow(r)){
    legal_trassen_fahrlage<-el$id[which(el$id %in% as.integer(unlist(strsplit(r$res[i], split=", "))))]
    n_legal_trassen_fahrlage<-length(legal_trassen_fahrlage)
    legal_trassen_fahrlage_ohneSink<-intersect(legal_trassen_fahrlage,el$id[which(el$bis!="S")])
    n_legal_trassen_fahrlage_ohneSink<-length(legal_trassen_fahrlage_ohneSink)
    
    F_A[sum_legal_trassen_fahrlage+(1:n_legal_trassen_fahrlage_ohneSink),i]<-matrix(-1,n_legal_trassen_fahrlage_ohneSink,1)
    
    for(k in 1:n_legal_trassen_fahrlage_ohneSink){
      id_temp<-legal_trassen_fahrlage_ohneSink[k]
      
      # Either the legal incoming Edge is not Active ...
      F_A[k+sum_legal_trassen_fahrlage,nrow(r)+which(el$id==id_temp)+(i-1)*nrow(el)]<-(-1)
      
      # ... Or one legal Successor is Active
      succ_ids<-intersect(el$id[which(el$von==el$bis[which(el$id==id_temp)])],legal_trassen_fahrlage)
      F_A[k+sum_legal_trassen_fahrlage,nrow(r)+which(el$id %in% succ_ids)+(i-1)*nrow(el)]<-matrix(1,1,length(which(el$id %in% succ_ids)))
    }
    sum_legal_trassen_fahrlage<-sum_legal_trassen_fahrlage+n_legal_trassen_fahrlage_ohneSink
    
  }
  
 
  
  ## Encode Parent Relation for Systemtrassen
  P_A<-matrix(0,nrow(r)*length(unique(el$parent)[which(unique(el$parent)!=0)]),nrow(r)+nrow(r)*nrow(el))
  P_dir<-rep("<=",nrow(r)*length(unique(el$parent)[which(unique(el$parent)!=0)]))
  P_b<-rep(0,nrow(r)*length(unique(el$parent)[which(unique(el$parent)!=0)]))
  
  for(i in 1:nrow(r)){
    parent_notnull<-length(unique(el$parent)[which(unique(el$parent)!=0)])
    if(parent_notnull!=0){
      for (k in 1:parent_notnull){
        parent_id<-unique(el$parent)[which(unique(el$parent)!=0)][k]
        P_A[i+(k-1)*parent_notnull,nrow(r)+nrow(el)*(i-1)+el$id[which(el$id==parent_id)][1]]<-length(which(el$parent==parent_id))
        P_A[i+(k-1)*parent_notnull,nrow(r)+nrow(el)*(i-1)+el$id[which(el$parent==parent_id)]]<-rep(1,length(which(el$parent==parent_id)))
        P_b[i+(k-1)*parent_notnull]<-length(which(el$parent==parent_id))
      } 
    }
  }
  
  
  ## Merge Matrices
  A<-rbind(S_A,R_A,QP_A,O_A,I_A,F_A,P_A)
  dir<-c(S_dir,R_dir,QP_dir,O_dir,I_dir,F_dir,P_dir)
  b<-c(S_b,R_b,QP_b,O_b,I_b,F_b,P_b)
  omega<-rep(0,nrow(r)+nrow(r)*nrow(el))
  
  ## Return
  out<-list()
  out$A<-A
  out$dir<-dir
  out$b<-b
  out$omega<-omega
  return(out)
}
