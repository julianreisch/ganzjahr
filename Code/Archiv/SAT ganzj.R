### Load Libraries
library(igraph)
library(lpSolve)

## Extend Solution Spaces
for(i in 1:nrow(r)){
  ## Build Restricted Graph
  el_temp<-el[which(el$tag1>=r$tag1[i] & el$tag2>=r$tag2[i]),]
  g<-graph_from_edgelist(as.matrix(el_temp[,2:3]),directed = T)
  E(g)$name<-el_temp$id
  
  ## Shortest Path
  spath<-get.shortest.paths(g,from="Q",to="S",weights = el_temp$weight,output = 'epath')$epath[[1]]
  indices<-as.integer(spath)
  edges<-el_temp$id[indices]
  
  ## Add to Solution Space
  r$res[i]<-paste(r$res[i],", ",toString(edges),sep="")
}

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
  ids<-el$id[which(el$von=="Q")]
  QP_A[2*i-1,(i-1)*nrow(el)+nrow(r)+ids]<-matrix(1,1,length(ids))
  
  # Exactly one Sink
  ids<-el$id[which(el$bis=="S")]
  QP_A[2*i,(i-1)*nrow(el)+nrow(r)+ids]<-matrix(1,1,length(ids))
}

## Encode Fahrlagen - atMostOne Outgoing Edge
O_A<-matrix(0,nrow(r)*2,nrow(r)+nrow(r)*nrow(el))
O_dir<-rep("<=",nrow(r)*2)
O_b<-rep(0,nrow(r)*2)

for(i in 1:nrow(r)){
  O_A[2*(i-1)+(1:2),i]<-c(-1,-1)

  # Outgoing from A
  ids<-el$id[which(el$von=="A")]
  O_A[2*i-1,(i-1)*nrow(el)+nrow(r)+ids]<-matrix(1,1,length(ids))

  # Outgoing from B
  ids<-el$id[which(el$von=="B")]
  O_A[2*i,(i-1)*nrow(el)+nrow(r)+ids]<-matrix(1,1,length(ids))
}

## Encode Fahrlagen - atMostOne Incoming Edge
I_A<-matrix(0,nrow(r)*2,nrow(r)+nrow(r)*nrow(el))
I_dir<-rep("<=",nrow(r)*2)
I_b<-rep(0,nrow(r)*2)

for(i in 1:nrow(r)){
  I_A[2*(i-1)+(1:2),i]<-c(-1,-1)
  
  # Outgoing from A
  ids<-el$id[which(el$bis=="A")]
  I_A[2*i-1,(i-1)*nrow(el)+nrow(r)+ids]<-matrix(1,1,length(ids))
  
  # Outgoing from B
  ids<-el$id[which(el$bis=="B")]
  I_A[2*i,(i-1)*nrow(el)+nrow(r)+ids]<-matrix(1,1,length(ids))
}

## Encode Fahrlagen - Flow Conservation
F_A<-matrix(0,nrow(r)*nrow(el[which(el$bis!="S"),]),nrow(r)+nrow(r)*nrow(el))
F_dir<-rep(">=",nrow(r)*nrow(el[which(el$bis!="S"),]))
F_b<-rep(-1,nrow(r)*nrow(el[which(el$bis!="S"),]))

for(i in 1:nrow(r)){
  F_A[nrow(el[which(el$bis!="S"),])*(i-1)+(1:nrow(el[which(el$bis!="S"),])),i]<-rep(-1,nrow(el[which(el$bis!="S"),]))
  
  for(k in 1:nrow(el[which(el$bis!="S"),])){
    id_temp<-el[which(el$bis!="S"),]$id[k]
    
    # Either the Incoming Edge is not Active ...
    F_A[k+(i-1)*nrow(el[which(el$bis!="S"),]),nrow(r)+which(el$id==id_temp)+(i-1)*nrow(el)]<-matrix(-1,1,length(which(el$id==id_temp)))
    
    # ... Or one Successor is Active
    candidates_ids<-el[which(el$von==el[which(el$bis!="S"),]$bis[k]),]$id # successors in graph
    succ_ids<-candidates_ids[which(candidates_ids %in% as.integer(unlist(strsplit(r$res[i], split=", "))))] # also in solution space of request
    F_A[k+(i-1)*nrow(el[which(el$bis!="S"),]),nrow(r)+which(el$id %in% succ_ids)+(i-1)*nrow(el)]<-matrix(1,1,length(which(el$id %in% succ_ids)))
  }
}

## Merge to IP
A<-rbind(S_A,R_A,QP_A,O_A,I_A,F_A)
dir<-c(S_dir,R_dir,QP_dir,O_dir,I_dir,F_dir)
b<-c(S_b,R_b,QP_b,O_b,I_b,F_b)
omega<-rep(0,nrow(r)+nrow(r)*nrow(el))

## Solve SAT
res<-lp(direction="max", objective.in=omega, const.mat=A, const.dir=dir, const.rhs=b, all.bin=TRUE)
x<-res$solution
print(res)
print(x)
