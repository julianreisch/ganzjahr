### Load Libraries
library(igraph)
library(lpSolve)

## Init Systemtrassen Graph
el<-data.frame(id=1:5,
               von=c("Q","Q","A","B","B"),
               bis=c("A","B","S","A","S"),
               weight=c(25,25,15,0,45),
               tag1=c(1,1,1,1,1),
               tag2=c(0,1,1,1,0))

el$von<-as.character(el$von)
el$bis<-as.character(el$bis)

g<-graph_from_edgelist(as.matrix(el[,2:3]),directed = T)
E(g)$name<-el$id

## Init Fahrlagen
r<-data.frame(id=1:2,
              tag1=c(1,1),
              tag2=c(1,0),
              res=c(0,0))

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
S_A<-matrix(0,nrow(el),nrow(r)+nrow(r)*5)
S_dir<-rep(">=",nrow(el))
S_b<-rep(1,nrow(el))

for(i in 1:nrow(el)){
  S_A[i,nrow(r)+i+nrow(el)*(0:(nrow(r)-1))]<-rep(1,nrow(r))
}

## Encode Fahrlagen - Source/Sink
QP_A<-matrix(0,nrow(r)*2,nrow(r)+nrow(r)*nrow(el))
QP_dir<-rep("=",2*nrow(r))
QP_b<-rep(0,2*nrow(r))

for(i in 1:nrow(r)){
  QP_A[2*(i-1)+(1:2),i]<-c(-1,-1)
  
  # Exactly one Source
  ids<-el$id[which(el$von=="Q")]
  QP_A[seq(1, 2*nrow(r), 2),(i-1)*nrow(el)+nrow(r)+ids]<-matrix(1,nrow(r),length(ids))
  
  # Exactly one Source
  ids<-el$id[which(el$bis=="S")]
  QP_A[seq(2, 2*nrow(r), 2),(i-1)*nrow(el)+nrow(r)+ids]<-matrix(1,nrow(r),length(ids))
}

## Encode Fahrlagen - atMostOne Outgoing Edge
O_A<-matrix(0,nrow(r)*2,nrow(r)+nrow(r)*nrow(el))
O_dir<-rep(">=",nrow(r)*2)
O_b<-rep(0,nrow(r)*2)

for(i in 1:nrow(r)){
  O_A[2*(i-1)+(1:2),i]<-c(-1,-1)

  # Outgoing from A
  ids<-el$id[which(el$von=="A")]
  O_A[seq(1, 2*nrow(r), 2),(i-1)*nrow(el)+nrow(r)+ids]<-matrix(1,nrow(r),length(ids))

  # Outgoing from B
  ids<-el$id[which(el$von=="B")]
  O_A[seq(2, 2*nrow(r), 2),(i-1)*nrow(el)+nrow(r)+ids]<-matrix(1,nrow(r),length(ids))
}

## Encode Fahrlagen - atMostOne Incoming Edge
I_A<-matrix(0,nrow(r)*2,nrow(r)+nrow(r)*nrow(el))
I_dir<-rep(">=",nrow(r)*2)
I_b<-rep(0,nrow(r)*2)

for(i in 1:nrow(r)){
  I_A[2*(i-1)+(1:2),i]<-c(-1,-1)
  
  # Outgoing from A
  ids<-el$id[which(el$bis=="A")]
  I_A[seq(1, 2*nrow(r), 2),(i-1)*nrow(el)+nrow(r)+ids]<-matrix(1,nrow(r),length(ids))
  
  # Outgoing from B
  ids<-el$id[which(el$bis=="B")]
  I_A[seq(2, 2*nrow(r), 2),(i-1)*nrow(el)+nrow(r)+ids]<-matrix(1,nrow(r),length(ids))
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
    F_A[(k-1)+seq(1,nrow(r)*nrow(el[which(el$bis!="S"),]),nrow(el[which(el$bis!="S"),])), 
        nrow(r)+which(el$id==id_temp)+(i-1)*nrow(el)]<-matrix(-1,1,length(which(el$id==id_temp)))
    
    # ... Or one Successor is Active
    succ_ids<-el[which(el$von==el[which(el$bis!="S"),]$bis[k]),]$id
    F_A[(k-1)+seq(1,nrow(r)*nrow(el[which(el$bis!="S"),]),nrow(el[which(el$bis!="S"),])),
        nrow(r)+which(el$id %in% succ_ids)+(i-1)*nrow(el)]<-matrix(1,nrow(r),length(succ_ids))
  }
}

## Encode Nachfolger of Systemtrassen for respective Fahrlagen 

## Merge to IP
A<-rbind(S_A,QP_A,O_A,I_A,F_A)
dir<-c(S_dir,QP_dir,O_dir,I_dir,F_dir)
b<-c(S_b,QP_b,O_b,I_b,F_b)
omega<-rep(0,nrow(r)+nrow(r)*nrow(el))

## Solve SAT
x<-lp(direction="max", objective.in=omega, const.mat=A, const.dir=dir, const.rhs=b, all.bin=TRUE)$solution
if(!all(x==0)){ # Satisfiable
  print(x)
  # break()
}

A_1<-diag(2)
A_2<-matrix(0,24*4,2)
A_2[1:(2*24),1]<-rep(x_1,2)
A_2[1:(4*24),2]<-rep(x_2,4)
A<-rbind(A_1,A_2)

b_1<-rep(1,2)
b_2<-rep(1,4*24)
b<-c(b_1,b_2)

res<-lp(direction="max", objective.in=omega, const.mat=A, const.dir="<=", const.rhs=b, all.bin=TRUE)
res_dual<-lp(direction="min", objective.in=b, const.mat=t(A), const.dir="==", const.rhs=omega)

spath<-get.shortest.paths(g,from="s",to="t",weights = el$weight,output = 'epath')
x_temp<-rep(0,24)
path<-unlist(spath$epath)
x_temp[path]<-rep(1,length(path))
#V(g)[as.integer(as.character(spath$vpath[[1]][c(2,4,6)]))]
#x<-rep(0,8)
#x[is.contained(x_temp)]<-rep(1,3)