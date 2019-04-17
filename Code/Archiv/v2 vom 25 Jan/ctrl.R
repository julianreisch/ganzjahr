### Set Working Directory
setwd("~/02_Projekte/24_SAT und Ganzj Bel/Code")

### Load Libraries
library(igraph)
library(lpSolve)

### Load Functionalities
source("extend_solution_spaces.R")
source("encoding.R")
source("makro_konflikte.R")
source("mikro_konflikte.R")
source("sep_trassen.R")
source("block_trassen.R")
source("partitioning.R")

### Load and initialize Data
source("Tests/unit test trenne systrasse und fahrlage.R")

## Lade el und r
data<-init_data() 
el<-data$el
r<-data$r
n<-data$n

## Initialisiere Solution Spaces
init<-extend_solution_spaces(el,r,which(r$valid==1),n) 
el<-init$el
r<-init$r

## Initialisiere Lösung 
# Encode Initialdaten
enc<-encode(el,r[which(r$valid==1),])
A<-enc$A
dir<-enc$dir
b<-enc$b
omega<-enc$omega

# Solve SAT für Initialdaten
res<-lp(direction="max", objective.in=omega, const.mat=A, const.dir=dir, const.rhs=b, all.bin=TRUE)

### Solange das Gesamtproblem nicht lösbar ist...
while(all(res$solution==0)){
  
  ## Finde einen Makrokonflikt Fahrlagen
  makro<-makro_konflikte(el[which(!(el$id %in% el$parent)),],r[which(r$valid==1),],A,dir,b)
  
  ## Finde einen Mikrokonflikt
  mikro<-mikro_konflikte(el[which(!(el$id %in% el$parent)),],r[which(r$valid==1),],makro,A,dir,b)
  
  ## Löse Mikrokonflikt
  # 1. Versuche Systemtrassen aufzuteilen
  nrow_el_alt<-nrow(el)
  sep<-sep_trassen(el,r,n,mikro)
  el<-sep$el
  r<-sep$r
  
  # 2.1 Falls keine Systemtrasse geteilt werden konnte, sperre Mikrokonflikttrassen
  #    im entsprechenden Konfliktzeitraum, um alternative Wege zu finden 
  if(nrow(el)==nrow_el_alt){
    makro_indx<-which(r$id %in% makro)
    res_alt<-sum(as.integer(unlist(strsplit(r$res[makro_indx], split=", "))))
    el_blocked<-block_trassen(el,r,makro,mikro,n)
    ex<-extend_solution_spaces(el_blocked,r,makro,n)
    r<-ex$r
    
    # 2.2.1 Falls auch keine neuen Wege aufgenommen werden konnten, partitioniere die Fahrlagen
    #     des Makrokonfliktes
    if(sum(as.integer(unlist(strsplit(r$res[makro_indx], split=", "))))==res_alt){
      
      # Partioniere
      for(i in r$fahrlage[which(r$id %in% makro)]){
        for(j in r$partition[which(r$fahrlage==i & r$valid==0)]){
          r$valid[which(r$partition==j)]<-rep(1,length(which(r$partition==j)))
        }
      }
      
      ex<-extend_solution_spaces(el,r,1:nrow(r),n)

      # 2.2.2 Falls immer noch keine neuen Wege hinzugekommen sind, löse Max SAT und lehne Fahrlagen ab
      if(0!=0){
      
      }else{
        r<-ex$r
      }
    }
  }else{
    
    ## Erweitere Lösungsraum mit jetzt geteilten Systemtrassen
    ex<-extend_solution_spaces(el,r,1:nrow(r),n)
    el<-ex$el
    r<-ex$r 
  }
  
  ## Encode
  enc<-encode(el,r[which(r$valid==1),])
  A<-enc$A
  dir<-enc$dir
  b<-enc$b
  omega<-enc$omega
  
  ## Solve SAT
  res<-lp(direction="max", objective.in=omega, const.mat=A, const.dir=dir, const.rhs=b, all.bin=TRUE)
  
  ## Gib Werte aus
  print(el)
  print(r)
  print(makro)
  print(mikro)
}

## Print Gesamtlösung
x<-res$solution
print(res)
#solution<-data.frame(t(x))
#colnames(solution)[1:length(unique(r$fahrlage))]<-paste("r",1:length(unique(r$fahrlage)),sep="")
#colnames(solution)[(length(unique(r$fahrlage))+1):(length(unique(r$fahrlage))+nrow(unique(r[,c("partition","fahrlage")])))]<-rep(0,nrow(unique(r[,c("partition","fahrlage")])))
#colnames(solution)[((nrow(r)+nrow(unique(r[,c("partition","fahrlage")])))+1):(length(unique(r$fahrlage))+nrow(unique(r[,c("partition","fahrlage")]))+nrow(r))]<-paste("x",1:nrow(r),sep="")
#colnames(solution)[(length(unique(r$fahrlage))+nrow(unique(r[,c("partition","fahrlage")]))+nrow(r)):(length(solution))]<-paste("s_",el$id,"^",sort(rep(rep(r$id),nrow(el))),sep="")
#print(solution[which(solution==1)])