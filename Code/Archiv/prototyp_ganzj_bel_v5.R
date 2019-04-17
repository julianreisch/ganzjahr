### Load Libraries
library(igraph)
library(lpSolve)

## Init Systemtrassen Graph
el<-data.frame(id=1:5,
               von=c("Q","Q","A","B","B"),
               bis=c("A","B","S","A","S"),
               weight=c(25,25,15,0,45),
               tag1=c(1,1,1,1,1),
               tag2=c(0,1,1,1,0),
               parent=rep(0,5))

el$von<-as.character(el$von)
el$bis<-as.character(el$bis)

g<-graph_from_edgelist(as.matrix(el[,2:3]),directed = T)
E(g)$name<-el$id

## Init Fahrlagen
r<-data.frame(id=1:2,
              tag1=c(1,1),
              tag2=c(1,0),
              res=c(0,0),
              parent=rep(0,2))

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

## Encode Parent Relation for Systemtrassen
P_A<-matrix(0,nrow(r)*length(unique(el$parent)[which(unique(el$parent)!=0)]),nrow(r)+nrow(r)*nrow(el))
P_dir<-rep("<=",nrow(r)*length(unique(el$parent)[which(unique(el$parent)!=0)]))
P_b<-rep(0,nrow(r)*length(unique(el$parent)[which(unique(el$parent)!=0)]))

for(i in 1:nrow(r)){
  for (k in 1:length(unique(el$parent)[which(unique(el$parent)!=0)])){
    parent_id<-unique(el$parent)[which(unique(el$parent)!=0)][k]
    P_A[i+(k-1)*length(unique(el$parent)[which(unique(el$parent)!=0)]),
        nrow(r)+nrow(el)*(i-1)+el$id[which(el$id==parent_id)][1]]<-length(which(el$parent==parent_id))
    P_A[i+(k-1)*length(unique(el$parent)[which(unique(el$parent)!=0)]),
        nrow(r)+nrow(el)*(i-1)+el$id[which(el$parent==parent_id)]]<-rep(1,length(which(el$parent==parent_id)))
    P_b[i+(k-1)*length(unique(el$parent)[which(unique(el$parent)!=0)])]<-length(which(el$parent==parent_id))
  }
}


## Merge to IP
A<-rbind(S_A,R_A,QP_A,O_A,I_A,F_A,P_A)
dir<-c(S_dir,R_dir,QP_dir,O_dir,I_dir,F_dir,P_dir)
b<-c(S_b,R_b,QP_b,O_b,I_b,F_b,P_b)
omega<-rep(0,nrow(r)+nrow(r)*nrow(el))

## Solve SAT
res<-lp(direction="max", objective.in=omega, const.mat=A, const.dir=dir, const.rhs=b, all.bin=TRUE)
x<-res$solution
print(res)
print(x)

## Find Makrokonflikte
k_count<-0
for(i in 1:nrow(r)){
  subsets<-combn(1:nrow(r),i,simplify = F)
  for(k in subsets){
    A_makro<-rbind(A[1:nrow(el),],A[nrow(el)+k,],A[(nrow(el)+nrow(r)+1):nrow(A),])
    dir_makro<-c(dir[1:nrow(el)],dir[nrow(el)+k],dir[(nrow(el)+nrow(r)+1):length(dir)])
    b_makro<-c(b[1:nrow(el)],b[nrow(el)+k],b[(nrow(el)+nrow(r)+1):length(b)])
    if(all(lp(direction="max", objective.in=omega, const.mat=A_makro, const.dir=dir_makro, const.rhs=b_makro, all.bin=TRUE)$solution==0)){
      subsubsets<-combn(k,i-1,simplify = F)
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
makro<-k

## Find Mikrokonflikte
k_count<-0
i<-1
term<-FALSE

while(i<=nrow(el) & term==FALSE){
  subsets<-combn(1:nrow(el),i,simplify = F)
  for(k in subsets){
    A_mikro<-rbind(A[k,],A[nrow(el)+makro,],A[(nrow(el)+nrow(r)+1):nrow(A),])
    dir_mikro<-c(dir[k],dir[nrow(el)+makro],dir[(nrow(el)+nrow(r)+1):length(dir)])
    b_mikro<-c(b[k],b[nrow(el)+makro],b[(nrow(el)+nrow(r)+1):length(b)])
    if(all(lp(direction="max", objective.in=omega, const.mat=A_mikro, const.dir=dir_mikro, const.rhs=b_mikro, all.bin=TRUE)$solution==0)){
      subsubsets<-combn(k,i-1,simplify = F)
      for(j in subsubsets){
        A_submikro<-rbind(A[j,],A[nrow(el)+makro,],A[(nrow(el)+nrow(r)+1):nrow(A),])
        dir_submikro<-c(dir[j],dir[nrow(el)+makro],dir[(nrow(el)+nrow(r)+1):length(dir)])
        b_submikro<-c(b[j],b[nrow(el)+makro],b[(nrow(el)+nrow(r)+1):length(b)])
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
mikro<-k

## Check pro Systemtrassen im Mikrokonflikt, ob es ein Paar Fahrlagenvarianten aus dem Makrokonflikt gibt
## bei denen es ausreicht die Systemtrasse zu teilen, damit beide die Trasse nutzen können
## und füge die geteilten Systrassen dann der Lösungsmenge der beiden Fahrlagen hinzu
k<-1
i<-1
j<-2
terminate<-FALSE
while(k <= length(mikro) & terminate==FALSE){
  while(i <= length(makro)-1 & terminate==FALSE){
    while(j <= length(makro) & terminate==FALSE){
      # Hat das Paar von Fahrlagen keinen Schnitt?
      if(all(sapply(r[makro[c(i,j)],2:3],sum)-sapply(el[mikro[k],5:6],sum)<=0)){
        # Füge Teil der ersten Fahrlage als neue, kürzere Systemtrasse hinzu...
        s_1<-data.frame(id=nrow(el)+1,von=el[mikro[k],]$von,bis=el[mikro[k],]$bis,weight=el[mikro[k],]$weight,
                      tag1=r$tag1[i],tag2=r$tag2[i],parent=mikro[k])
        el<-rbind(el,s_1)
        r$res[i]<-paste(r$res[i],", ",toString(nrow(el)),sep="")
        
        # ... und Teil der zweiten ...
        s_2<-data.frame(id=nrow(el)+1,von=el[mikro[k],]$von,bis=el[mikro[k],]$bis,weight=el[mikro[k],]$weight,
                      tag1=r$tag1[j],tag2=r$tag2[j],parent=mikro[k])
        el<-rbind(el,s_2)
        r$res[j]<-paste(r$res[j],", ",toString(nrow(el)),sep="")
        
        # ... und falls noch eine Resttrasse übrig bleibt, füge auch diese hinzu
        if(!all(sapply(r[makro[c(i,j)],2:3],sum)-sapply(el[mikro[k],5:6],sum)>=0)){
          tage<-which(sapply(r[makro[c(i,j)],2:3],sum)-sapply(el[mikro[k],5:6],sum)<0)
          s_rest<-data.frame(id=nrow(el)+1,von=el[mikro[k],]$von,bis=el[mikro[k],]$bis,weight=el[mikro[k],]$weight,
                             tag1=0,tag2=0,parent=mikro[k])
          s_test[5:6][tage]<-rep(1,length(tage))
          el<-rbind(el,s_rest)
        }
        terminate<-TRUE
      }
      j<-j+1
    }
    i<-i+1
    j<-i+1
  }
  k<-k+1
}

#A_makro<-cbind(A,rbind(matrix(0,nrow(el),2),diag(1,nrow(r)),matrix(0,nrow(A)-2-nrow(el),2)))
#dir_makro<-dir
#b_makro<-b
#omega_makro<-c(omega,1,1)

#res_makro<-lp(direction="min", objective.in=omega_makro, const.mat=A_makro, const.dir=dir_makro, const.rhs=b_makro, all.bin=TRUE)

