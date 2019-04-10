## Je Fahrlage, setze erste Partition auf valid, für die gilt, dass all ihre Fahrlagenvarianten 
## einen Weg haben und lösche partition, wenn nicht. Wenn keine valide Partition für eine Fahlage 
## existiert, lehne Fahrlage direkt ab.

validity<-function(r){
  
  for(i in 1:length(unique(r$fahrlage))){
    fahrlage<-unique(r$fahrlage)[i]
    partition_count<-1
    next_fahrlage<-FALSE
    
    while(next_fahrlage==FALSE & partition_count <= length(unique(r[which(r$fahrlage == fahrlage),]$partition))){
      
      partition<-unique(r[which(r$fahrlage == fahrlage),]$partition)[partition_count]
      weg_ex_count<-0
      
      for(k in 1:length(unique(r[which(r$fahrlage == fahrlage & r$partition == partition),]$id))){
        weg<-as.integer(unlist(strsplit(r$res[which(r$fahrlage==fahrlage &
                                                      r$partition == partition &
                                                      r$id == unique(r[which(r$fahrlage == fahrlage & r$partition == partition),]$id)[k])], split=", ")))
        if(length(weg)>1){
          weg_ex_count<-weg_ex_count+1
        }
      }
      if(weg_ex_count==length(unique(r[which(r$fahrlage == fahrlage & r$partition == partition),]$id))){
        
        r$valid[which(r$fahrlage==fahrlage & r$partition==partition)]<-rep(1,length(unique(r[which(r$fahrlage == fahrlage & r$partition == partition),]$id)))
        next_fahrlage<-TRUE
        
      }else{
        
        r$abgelehnt[which(r$fahrlage==fahrlage & r$partition==partition)]<-rep(1,length(unique(r[which(r$fahrlage == fahrlage & r$partition == partition),]$id)))
        
      }
      
      partition_count<-partition_count+1
      
    }
    
    if(next_fahrlage==FALSE){
      
      ablehnung_id<-fahrlage
      r$abgelehnt[which(r$fahrlage==ablehnung_id)]<-rep(1,length(which(r$fahrlage==ablehnung_id)))
      print(paste("Lehne Fahrlage ",ablehnung_id," ab.",sep=""))
      
    }
  }
  
  ## Lösche abgelehnte Partitionen/Fahrlagen
  r<-r[which(r$abgelehnt==0),]
  
  ## Return
  return(r)
  
}