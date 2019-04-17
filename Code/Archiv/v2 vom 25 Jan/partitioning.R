partitioning<-function(r,makro,n){
  
  ## Definiere Schneideregel
  schneideregel<-c(1,rep(0,n-1))
  r_parts<-r
  
  for(i in 1:length(makro)){
    
    ## Falls Partitionierung tatsächlich die Fahrlage in kleinere Teile schneidet
    if(length(unique(schneideregel[which(r[makro[i],4:(3+n)]==1)]))>1){
      parts<-rbind(r[which(r$id==makro[i]),],r[which(r$id==makro[i]),])
      
      # Vergebe neue Ids
      parts$id[1:2]<-c(max(r_parts$id)+1,max(r_parts$id)+2)
      
      # Schneide ersten Teil der Partition
      inds1<-which(schneideregel==1 & parts[1,4:(n+3)]==1)
      parts[1,3+inds1]<-rep(1,length(inds1))
      parts[1,3+((1:n)[-inds1])]<-rep(0,n-length(inds1))
      
      # Schneide zweiten Teil der Partition
      inds2<-which(schneideregel==0 & parts[2,4:(n+3)]==1)
      parts[2,3+inds2]<-rep(1,length(inds2))
      parts[2,3+((1:n)[-inds2])]<-rep(0,n-length(inds2))
      
      # Ersetze in r_parts die Partition durch die Parts
      r_parts<-r_parts[-which(r_parts$id==makro[i]),]
      r_parts<-rbind(r_parts,parts)
    }
  }
  
  ## Return
  return(r_parts)
  
}