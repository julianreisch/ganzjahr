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
source("validity.R")
source("decoding.R")
source("bellmanford_bitconstr.R")

### Load and initialize Data
source("Tests/unit test trenne systrasse und fahrlage.R")

## Lade Daten
data<-load_data() 
el<-data$el
r<-data$r
n<-data$n

## Initialize ids
el$id<-1:nrow(el)

## Initialize topological ordering of vertices (To do: Compute the ordering here)
v_top<-c('Q','B','A','S')

## Suche Initial Wege für Solution Spaces und Zähler für Wegesuche
wegeSuche_count<-0
init<-extend_solution_spaces(el,r,1:nrow(r),n) 
el<-init$el
r<-init$r
wegeSuche_count<-init$wegeSuche_count

## Initialisiere Validity
ablehnen_count<-length(unique(r$fahrlage))
r<-validity(r)

## Initialisiere Lösung 
# Encode Initialdaten
enc<-encode(el,r)
A<-enc$A
dir<-enc$dir
b<-enc$b
omega<-enc$omega

# Solve SAT für Initialdaten
res<-lp(direction="max", objective.in=omega, const.mat=A, const.dir=dir, const.rhs=b, all.bin=TRUE)

## Initialisiere Zähler
stufe1_count<-0
stufe2_count<-0

### Solange das Gesamtproblem nicht lösbar ist...
while(res$status!=0){
  
  ## Finde einen Makrokonflikt Fahrlagen
  makro<-makro_konflikte(el[which(el$beenparent==0),],r[which(r$abgelehnt==0 & r$valid==1),])
  print(makro)
  
  
  ## Finde einen Mikrokonflikt
  benutzteSystemtrassen<-unique(as.integer(unlist(strsplit(r$res[which(r$valid==1 & r$fahrlage %in% makro)], split=", "))))
  mikro<-mikro_konflikte(el[which(el$beenparent==0 & el$id %in% benutzteSystemtrassen),],r[which(r$valid==1),],makro)
  print(mikro)
  
  ## Löse Mikrokonflikt
  # 1. Versuche Systemtrassen aufzuteilen
  nrow_el_alt<-nrow(el)
  sep<-sep_trassen(el,r,n,mikro,makro)
  el<-sep$el
  r<-sep$r
  
  # 2 Falls keine Systemtrasse geteilt werden konnte, sperre Mikrokonflikttrassen
  #   im entsprechenden Konfliktzeitraum, um alternative Wege zu finden 
  #   Gehe vor nach Stufenprinzip
  
  if(nrow(el)==nrow_el_alt){
    qual_max<-(-1000)
    terminate<-FALSE
    #terminate_local<-FALSE
    
    ## Sperre zunächst alle Systemtrassen (OR Verknüpfung) gemeinsam auf Konfliktzeit aller Fahrlagen im
    ## Makrokonflikt
    
    el_blocked<-block_trassen(el,r,makro,mikro,n)
    stufe1_count<-stufe1_count+1
    
    for(i in 1:length(makro)){
      fahrlage<-makro[i]
      partitionen_ids<-unique(r$partition[which(r$partition %in% r$partition[which(r$fahrlage == fahrlage)])])
      
      for(j in 1:length(partitionen_ids)){
        
        partition<-partitionen_ids[j]
        varianten_indx<-which(r$id %in% r$id[which(r$fahrlage == fahrlage & r$partition == partition)])
        
        ex<-extend_solution_spaces(el_blocked,r,varianten_indx,n) # Mach Wegesuche für alle Varianten der Partition j der Fahrlage i
        r_candid<-ex$r
        wegeSuche_count<-wegeSuche_count+ex$wegeSuche_count
        
        qual_candid<-(2-(as.integer(apply(r[varianten_indx,11:(10+n)],1,sum))/n) %*% 
                        r_candid$minFahrzeit[varianten_indx]/(min(r$minFahrzeit[which(r$fahrlage==fahrlage & r$minFahrzeit!=0)])))*r_candid$homogen[varianten_indx[1]]
        
        
        # Wenn meine Qualitätskennzahl besser ist und neue Wege gefunden wurden (bzw. bei 
        # neuen Partitionen, ob alle Fahrlagenvarianten einen Weg haben), dann wird diese Partition mein Gewinner
        
        if((!all(r_candid$res==r$res) | (r$valid[varianten_indx[1]]==0) & (all(r$res[varianten_indx]!="0"))) & qual_candid>qual_max){
          
          qual_max<-qual_candid
          fahrlage_gewinner<-fahrlage
          partition_gewinner<-partition
          r_gewinner<-r_candid
          terminate<-TRUE
        }
      }
    }
    
    ## Falls kein neuer Weg gefunden wurde, sperre Systemtrassen separat, aber auf Konfliktzeit aller Fahrlagen im
    ## Makrokonflikt
    
    if(terminate==FALSE){
      stufe1_count<-stufe1_count-1
      stufe2_count<-stufe2_count+1
      
      
      for(k in 0:length(mikro)){
        
        ## Schalte dazwischen: Wenn keine Trasse gesperrt wird, können dennoch neue Wege für neue Partitionen gefunden werden
        if(k==0){
          el_blocked<-el
        }else{
          el_blocked<-block_trassen(el,r,makro,mikro[k],n)
        }
        
        for(i in 1:length(makro)){
          qual_max<-(-1000)
          fahrlage<-makro[i]
          partitionen_ids<-unique(r$partition[which(r$partition %in% r$partition[which(r$fahrlage == fahrlage)])])
          
          for(j in 1:length(partitionen_ids)){
            
            partition<-partitionen_ids[j]
            varianten_indx<-which(r$id %in% r$id[which(r$fahrlage == fahrlage & r$partition == partition)])
            
            ex<-extend_solution_spaces(el_blocked,r,varianten_indx,n) # Mach Wegesuche für alle Varianten der Partition j der Fahrlage i
            r_candid<-ex$r
            wegeSuche_count<-wegeSuche_count+ex$wegeSuche_count
            
            
            qual_candid<-(2-(as.integer(apply(r[varianten_indx,11:(10+n)],1,sum))/n) %*% 
                            r_candid$minFahrzeit[varianten_indx]/(min(r$minFahrzeit[which(r$fahrlage==fahrlage & r$minFahrzeit!=0)])))*r_candid$homogen[varianten_indx[1]]
            
            
            # Wenn meine Qualitätskennzahl besser ist und neue Wege gefunden wurden (bzw. bei 
            # neuen Partitionen, ob alle Fahrlagenvarianten einen Weg haben), dann wird diese Partition mein Gewinner
            
            if((!all(r_candid$res==r$res) | (r$valid[varianten_indx[1]]==0) & (all(r$res[varianten_indx]!="0"))) & qual_candid>qual_max){
              
              qual_max<-qual_candid
              fahrlage_gewinner<-fahrlage
              partition_gewinner<-partition
              r_gewinner<-r_candid
              terminate<-TRUE
            }
          }
        }
      }  
    }
    
   
    if(terminate==TRUE){
      
      ## Ergänze r um neuen Weg/ neue Partition des Gewinners
      r<-r_gewinner
      indx_update<-which(r$partition==partition_gewinner & r$fahrlage==fahrlage_gewinner)
      r$valid[indx_update]<-rep(1,length(indx_update))
      print(paste("The winner is: Fahrlage ",fahrlage_gewinner," und Partition ",partition_gewinner,sep=""))
      
    }
    
    
    ## Falls keine neuen Wege in keiner Stufe gefunden und der Konflikt demnach nicht aufgelöst wurde, lehne Fahrlagen ab
    if(terminate==FALSE){
     
      stufe2_count<-stufe2_count-1
      
      ablehnung_id<-makro[length(makro)]
      if(ablehnung_id==19){break}
      r$abgelehnt[which(r$fahrlage==ablehnung_id)]<-rep(1,length(which(r$fahrlage==ablehnung_id)))
      print(paste("Lehne Fahrlage ",ablehnung_id," ab.",sep=""))
      
    }
  }
  
  
  ## Encode
  enc<-encode(el,r[which(r$abgelehnt==0 & r$valid==1),])
  A<-enc$A
  dir<-enc$dir
  b<-enc$b
  omega<-enc$omega
  print(dim(A))
  
  ## Solve SAT
  res<-lp(direction="max", objective.in=omega, const.mat=A, const.dir=dir, const.rhs=b, all.bin=TRUE)

}

## Dekodiere Gesamtlösung
solution<-decode(res$solution,el,r)

## Speicher als csv
#write.csv(el,"0207_el_jordis.csv")
#write.csv(r,"0207_r_jordis.csv")
#write.csv(solution,"0207_solution_jordis.csv")

KPIs<-data.frame(stufe1=stufe1_count,stufe2=stufe2_count,ablehnung=ablehnen_count-length(unique(r$fahrlage[which(r$abgelehnt==0)])),wegesuche=wegeSuche_count)
#write.csv(KPIs,"KPIsmitBau.csv")


## Print KPIs
#print(paste("Die Wegesuche wurde ",wegeSuche_count," mal aufgerufen.",sep=""))
#print(paste("ES wurden ",length(unique(el$parent))," Trassen geteilt.",sep=""))
#print(paste("Es wurde ",stufe1_count," mal Stufe 1, ",stufe2_count," mal Stufe 2 und ",stufe3_count," mal Stufe 3 angewandt.",sep=""))
#print(paste("Es wurden ",ablehnen_count-length(unique(r$fahrlage))," Fahrlagen abgelehnt.",sep=""))
#el[which(el$beenparent==0),][which(el[which(el$beenparent==0),]$id %in% c(71,72,73,100,101,138,139,140,147,148,149,159,160,161)),]
