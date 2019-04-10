## Dekodiere die Solution des IP in eine Zuweisung
## - Welche Fahrlage nicht abgelehnt ist 
## - Welche Partition eine Fahrlage benutzt
## - Welche Fahrlagenvarianten zu diesen Partition gehören
## - Welche Systemtrassen von den Fahrlagenvarianten benutzt wurden

decode<-function(sol,el,r){
  solution<-data.frame(t(sol))
  colnames(solution)[1:length(unique(r$fahrlage[which(r$abgelehnt==0 & r$valid==1)]))]<-paste("r", unique(r$fahrlage[which(r$abgelehnt==0)]),sep="")
  
  count<-length(unique(r$fahrlage[which(r$abgelehnt==0)]))+1
  for(i in 1:nrow(unique(r[which(r$abgelehnt==0 & r$valid==1),c("partition","fahrlage")]))){
    colnames(solution)[count]<-paste("p_(",unique(r[which(r$abgelehnt==0 & r$valid==1),c("partition","fahrlage")])[i,2],",",
                                     unique(r[which(r$abgelehnt==0 & r$valid==1),c("partition","fahrlage")])[i,1],")",sep="")
    count<-count+1
  }
  
  for(i in 1:nrow(unique(r[which(r$abgelehnt==0 & r$valid==1),c("id","partition","fahrlage")]))){
    colnames(solution)[count]<-paste("x_(",unique(r[which(r$abgelehnt==0 & r$valid==1),c("id","partition","fahrlage")])[i,3],",",
                                     unique(r[which(r$abgelehnt==0 & r$valid==1),c("id","partition","fahrlage")])[i,2],")^",
                                     unique(r[which(r$abgelehnt==0 & r$valid==1),c("id","partition","fahrlage")])[i,1],sep="")
    count<-count+1
  }
  
  colnames(solution)[count:length(solution)]<-paste("s_",el$id[which(el$beenparent==0)],"^",
                                                    sort(rep(rep(r$id[which(r$abgelehnt==0 & r$valid==1)]),
                                                             nrow(el[which(el$beenparent==0),]))),sep="")
  ## Print and Return
  print(solution[which(solution==1)])
  return(solution)

}
