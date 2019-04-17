encode<-function(el,r){
  ## Make Ids to Integers
  el$id<-as.integer(el$id)
  el_enc<-el[which(!(el$id %in% el$parent)),] # Encode nur Systemtrassen, die Blätter sind im Baum der geteilten Trassen

  ## Speichere, welche (Sub-)Systemtrasse eine Wurzelsystemtrasse im Baum repräsentiert 
  ## 0 = ich bin selbst Rep.
  ## -1 = ich bin ungültig
  ## n = n ist mein Repräsentant
  
  rep<-matrix(0,nrow(r),nrow(el_enc))
  colnames(rep)<-el_enc$id
  rownames(rep)<-r$id
  
  for(i in 1:nrow(r)){
    for(k in 1:nrow(el_enc)){
      
      # Finde erstmögliche Subsystemtrasse als Repräsentanten
      if((el_enc$id[k] %in% as.integer(unlist(strsplit(r$res[i], split=", ")))) &
         length(intersect(which(r[i,10:(9+n)]==1),which(el_enc[k,6:(5+n)]==1)))!=0){
        
        # Suche nach allen anderen Subsystemtrassen, diesen Repräsentanten haben (also denselben parent haben) 
        for(j in 1:nrow(el_enc)){
          if(el_enc$parent[j]!=0 & el_enc$parent[j]==el_enc$parent[k] & 
             (el_enc$id[j] %in% as.integer(unlist(strsplit(r$res[i], split=", ")))) &
             length(intersect(which(r[i,10:(9+n)]==1),which(el_enc[j,6:(5+n)]==1)))!=0){
            rep[i,j]<-el_enc$id[k]
          }
        }
        rep[i,k]<-0
      }else{
        
          # Ansonsten: Wenn die Substrasse gar nicht in Lösungsmenge, dann bezeichne als ungültig 
          if(!(el_enc$id[k] %in% as.integer(unlist(strsplit(r$res[i], split=", "))))){
            rep[i,k]<-(-1)
          }
        }
      }
    }

  
  ## Init
  nVar<-length(unique(r$fahrlage))+nrow(unique(r[,c("partition","fahrlage")]))+nrow(r)+nrow(r)*nrow(el_enc)
  nR<-length(unique(r$fahrlage))+nrow(unique(r[,c("partition","fahrlage")]))
  
  ## Encode Systemtrassen Clauses
  S_A<-matrix(0,nrow(el_enc),nVar)
  S_dir<-rep("<=",nrow(el_enc))
  S_b<-rep(1,nrow(el_enc))
  
  for(i in 1:nrow(el_enc)){
    S_A[i,nR+nrow(r)+i+nrow(el_enc)*(0:(nrow(r)-1))]<-rep(1,nrow(r))
  }
  
  ## Encode Fahrlagen - Belege jede Fahrlage
  R_A<-matrix(0,length(unique(r$fahrlage)),nVar)
  R_A[1:length(unique(r$fahrlage)),1:length(unique(r$fahrlage))]<-diag(1,length(unique(r$fahrlage)))
  R_dir<-rep(">=",length(unique(r$fahrlage)))
  R_b<-rep(1,length(unique(r$fahrlage)))
  
  ## Encode Partitions - Für jede Fahrlage genau eine Partition
  Pa_A<-matrix(0,length(unique(r$fahrlage)),nVar)
  
  # Entweder die Fahrlage ist nicht erfüllt...
  Pa_A[1:length(unique(r$fahrlage)),1:length(unique(r$fahrlage))]<-diag(-1,length(unique(r$fahrlage)))
  
  # ... oder es muss genau eine Partition gelten
  Pa_A[1:length(unique(r$fahrlage)),]
 
  sum_partitionen<-0
  for(i in 1:length(unique(r$fahrlage))){
    indx<-unique(r$partition[which(r$fahrlage==i)])
    Pa_A[i,length(unique(r$fahrlage))+sum_partitionen+indx]<-rep(1,length(indx))
    sum_partitionen<-sum_partitionen+length(indx)
  }
  
  Pa_dir<-rep("=",length(unique(r$fahrlage)))
  Pa_b<-rep(0,length(unique(r$fahrlage)))
  
  ## Encode Fahrlagenvarianten - Wenn eine Partition aktiv ist, dann auch all ihre Fahrlagenvarianten
  FV_A<-matrix(0,nrow(unique(r[,c("partition","fahrlage")])),nVar)
  FV_dir<-rep(">=",nrow(unique(r[,c("partition","fahrlage")])))
  FV_b<-rep(0,nrow(unique(r[,c("partition","fahrlage")]))) 
  
  sum_partitionen<-0
  for(i in 1:length(unique(r$fahrlage))){
    fahrlage<-unique(r$fahrlage)[i]
    for(k in 1:length(unique(r$partition[(which(r$fahrlage==unique(r$fahrlage)[i]))]))){
      part<-r$partition[which(r$fahrlage==unique(r$fahrlage[i]))][k]
      fahrlagenVarianten<-r$id[which(r$fahrlage==fahrlage & r$partition==part)]
      FV_A[sum_partitionen+1,length(unique(r$fahrlage))+sum_partitionen+1]<-(-length(fahrlagenVarianten))
      FV_A[sum_partitionen+1,nR+which(r$id %in% fahrlagenVarianten)]<-rep(1,length(fahrlagenVarianten))
      sum_partitionen<-sum_partitionen+1
    }
  }
  
  ## Encode Fahrlagenvarianten - Source/Sink
  QS_A<-matrix(0,nrow(r)*2,nVar)
  QS_dir<-rep("=",2*nrow(r))
  QS_b<-rep(0,2*nrow(r))
  
  for(i in 1:nrow(r)){
    QS_A[2*(i-1)+(1:2),nR+i]<-c(-1,-1)
    
    # Exactly one Source (unter den Repräsentanten)
    indx_Q<-which(el_enc$id %in% el_enc[rep[i,]==0,]$id[which(el_enc[rep[i,]==0,]$von==r$von[i])])
    QS_A[2*i-1,nR+(i-1)*nrow(el_enc)+nrow(r)+indx_Q]<-matrix(1,1,length(indx_Q))
    
    # Exactly one Sink
    indx_S<-which(el_enc$id %in% el_enc[rep[i,]==0,]$id[which(el_enc[rep[i,]==0,]$bis==r$bis[i])])
    QS_A[2*i,nR+(i-1)*nrow(el_enc)+nrow(r)+indx_S]<-matrix(1,1,length(indx_S))
  }
  
  ## Encode Fahrlagen - atMostOne Outgoing Edge für jeden Knoten im Graphen außer Quelle und Senke
  ## (wieder nur unter den Repräsentanten)
  vertices<-unique(c(unique(el_enc$von),unique(el_enc$bis)))
  nVertices<-length(vertices)
  O_A<-matrix(0,nrow(r)*nVertices,nVar)
  
  for(i in 1:nrow(r)){
    O_A[nVertices*(i-1)+(1:nVertices),nR+i]<-rep(-1,nVertices)
    
    for(k in 1:nVertices){
      if(!(vertices[k] %in% r$von[i])){
        indx<-which(el_enc$id %in% el_enc[rep[i,]==0,]$id[which(el_enc[rep[i,]==0,]$von==vertices[k])])
        O_A[nVertices*(i-1)+k,nR+nrow(r)+(i-1)*nrow(el_enc)+indx]<-matrix(1,1,length(indx))
      }
    }
  } 
  # Lösche unnötige Zeilen wegen Quelle/senke
  O_A<-O_A[which(apply(O_A,1,sum)>=0),]
  O_dir<-rep("<=",nrow(O_A))
  O_b<-rep(0,nrow(O_A))
  
  ## Encode Fahrlagen - atMostOne Incoming Edge außer Quelle und Senke
  ## (wieder nur unter den Repräsentanten)
  vertices<-unique(c(unique(el_enc$von),unique(el_enc$bis)))
  nVertices<-length(vertices)
  I_A<-matrix(0,nrow(r)*nVertices,nVar)
 
  for(i in 1:nrow(r)){
    I_A[nVertices*(i-1)+(1:nVertices),nR+i]<-rep(-1,nVertices)
    
    for(k in 1:nVertices){
      if(!(vertices[k] %in% r$bis[i])){
        indx<-which(el_enc$id %in% el_enc[rep[i,]==0,]$id[which(el_enc[rep[i,]==0,]$bis==vertices[k])])
        I_A[nVertices*(i-1)+k,nR+nrow(r)+(i-1)*nrow(el_enc)+indx]<-matrix(1,1,length(indx))
      }
    }
  } 
  # Lösche unnötige Zeilen wegen Quelle/senke
  I_A<-I_A[which(apply(I_A,1,sum)>=0),]
  I_dir<-rep("<=",nrow(I_A))
  I_b<-rep(0,nrow(I_A))
  
  ## Encode Fahrlagen - Flow Conservation
  F_A<-matrix(0,0,nVar)

  for(i in 1:nrow(r)){
    for(k in 1:nrow(el_enc)){
      if(rep[i,k]==0 & el_enc$bis[k]!=r$bis[i]){
        
        # Nachfolger (außer Kanten nach Sink)
        succ_indx<-which(el_enc$id %in% el_enc[rep[i,]==0,]$id[which(el_enc[rep[i,]==0,]$von==el_enc$bis[k])])
        
        # Add Clause
        clause<-rep(0,nVar)
        clause[nR+i]<-(-1)
        clause[nR+nrow(r)+(i-1)*nrow(el_enc)+k]<-(-1)
        clause[nR+nrow(r)+(i-1)*nrow(el_enc)+succ_indx]<-rep(1,length(succ_indx))
        F_A<-rbind(F_A,clause)
      }
    }
  }
  
  F_dir<-rep(">=",nrow(F_A))
  F_b<-rep(-1,nrow(F_A))
  
  
  ## Encode Representations
  Re_A<-matrix(0,0,nVar)
  
  for(i in 1:nrow(r)){
    for(k in 1:nrow(el_enc)){
      
      ## Für jeden Prepräsentant, der wirklich andere Subsystemtrassen repräsentiert, füge clause hinzu
      if(rep[i,k]==0 & sum(rep[i,]==as.integer(colnames(rep)[k]))>0){
        
        ## Repräsentierte Subsystemtrassen
        reped_indx<-which(rep[i,]==as.integer(colnames(rep)[k]))
        
        ## Add clause
        clause<-rep(0,nVar)
        clause[nR+nrow(r)+(i-1)*nrow(el_enc)+k]<-(-length(reped_indx))
        clause[nR+nrow(r)+(i-1)*nrow(el_enc)+reped_indx]<-rep(1,length(reped_indx))
        Re_A<-rbind(Re_A,clause)
       
      }
    }
  }
  
  Re_dir<-rep(">=",nrow(Re_A))
  Re_b<-rep(0,nrow(Re_A))
  
  ## Ggf. Lösche Spalten, wo Systemtrasse nicht in den legalen Trassen der Fahrlagenvariante ist
  ## Merke dabei aber, welcher Spaltenindex für welche Kombi steht
  
  ## Merge Matrices
  A<-rbind(S_A,R_A,Pa_A,FV_A,QS_A,O_A,I_A,F_A,Re_A)
  dir<-c(S_dir,R_dir,Pa_dir,FV_dir,QS_dir,O_dir,I_dir,F_dir,Re_dir)
  b<-c(S_b,R_b,Pa_b,FV_b,QS_b,O_b,I_b,F_b,Re_b)
  omega<-rep(0,nVar)
  
  ## Return
  out<-list()
  out$A<-A
  out$dir<-dir
  out$b<-b
  out$omega<-omega
  return(out)
}
