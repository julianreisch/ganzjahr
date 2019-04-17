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
S_dir<-rep("<=",nrow(el))
S_b<-rep(1,nrow(el))
for(i in 1:nrow(el)){
  S_A[i,nrow(r)+i+nrow(el)*(0:(nrow(r)-1))]<-rep(1,nrow(r))
}

## Encode Fahrlagen - Source/Sink
QP_A<-matrix(0,nrow(r),nrow(r)+nrow(r)*nrow(el))
QP_dir<-rep("=",nrow(r))
QP_b<-rep(0,nrow(r))
for(i in 1:nrow(r)){
  QP_A[i,i]<-(-1)
  QP_A[i,1+nrow(r)+nrow(el)*(0:(nrow(r)-1))]<-rep(1,nrow(r))
}

## Encode Fahrlagen - Unit Flow
O_A<-matrix(0,nrow(el),nrow(r)+nrow(r)*nrow(el))
for(i in 1:length(unique(el$von))){
  von<-unique(el$von)[i]
  indices<-el[which(el$von==i),]$id
  O_A[i,nrow(r)+i+nrow(el)*indices]<-rep(1,length(indices))
}
#UV_A<-matrix(0,)

## Merge to IP

## Find Macro Conflict

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