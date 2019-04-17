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

### Load and initialize Data
source("Tests/unit test patrick klein.R")

## Lade el und r
data<-init_data() 
el<-data$el
r<-data$r
n<-data$n

## Initialisiere Solution Spaces
wegeSuche_count<-0
init<-extend_solution_spaces(el,r,which(r$valid==1),n) 
el<-init$el
r<-init$r
wegeSuche_count<-init$wegeSuche_count

## Init minFahrzeiten der FahrlagenVar (copy von 1. Partition)
for(i in 1:(nrow(r)-1)){
  if(r$fahrlage[i+1]==r$fahrlage[i]){
    r$minFahrzeit[i+1]<-r$minFahrzeit[i]
  }
}

## Initialisiere Lösung 
# Encode Initialdaten
enc<-encode(el,r[which(r$valid==1),])
A<-enc$A
dir<-enc$dir
b<-enc$b
omega<-enc$omega

# Solve SAT für Initialdaten
res<-lp(direction="max", objective.in=omega, const.mat=A, const.dir=dir, const.rhs=b, all.bin=TRUE)

## Initialisiere Zähler
stufe1_count<-0
stufe2_count<-0
stufe3_count<-0
ablehnen_count<-length(unique(r$fahrlage))

### Solange das Gesamtproblem nicht lösbar ist...
while(all(res$solution==0)){
  
  #print(el)
  #print(r)
  
  ## Finde einen Makrokonflikt Fahrlagen
  makro<-makro_konflikte(el[which(el$beenparent==0),],r[which(r$valid==1),])
  print(makro)
  
  ## Finde einen Mikrokonflikt 
  benutzteSystemtrassen<-unique(as.integer(unlist(strsplit(r$res[which(r$fahrlage %in% makro)], split=", "))))
  mikro<-mikro_konflikte(el[which(el$beenparent==0 & el$id %in% benutzteSystemtrassen),],r[which(r$valid==1),],makro)
  print(mikro)
  
  ## Löse Mikrokonflikt
  # 1. Versuche Systemtrassen aufzuteilen
  nrow_el_alt<-nrow(el)
  sep<-sep_trassen(el,r,n,mikro,makro)
  el<-sep$el
  r<-sep$r

  # 2 Falls keine Systemtrasse geteilt werden konnte, sperre Mikrokonflikttrassen
  #   (für jede noch nicht valide Partition, und füge diejenige Partition hinzu, 
  #   die die Qualitätskennzahl am wenigsten verringert) 
  #   im entsprechenden Konfliktzeitraum, um alternative Wege zu finden 
  #   Gehe vor nach Stufenprinzip
  
  if(nrow(el)==nrow_el_alt){
    qual_max<-(-1000)
    terminate<-FALSE
    
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
        
        qual_candid<-(2-(as.integer(apply(r[varianten_indx,10:(9+n)],1,sum))/n) %*% 
                        r_candid$minFahrzeit[varianten_indx]/(min(r$minFahrzeit[which(r$fahrlage==fahrlage & r$minFahrzeit!=0)])))*r_candid$homogen[varianten_indx[1]]
        

        # Wenn meine Qualitätskennzahl besser ist und neue Wege gefunden wurden, dann wird diese Partition mein Gewinner
        if(!all(r_candid$res==r$res) & qual_candid>qual_max){
          #if(r$valid[which(r$partition==partition)]
          
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
      
      
      for(k in 1:length(mikro)){
        el_blocked<-block_trassen(el,r,makro,mikro[k],n)
        
        for(i in 1:length(makro)){
          fahrlage<-makro[i]
          partitionen_ids<-unique(r$partition[which(r$partition %in% r$partition[which(r$fahrlage == fahrlage)])])
          
          for(j in 1:length(partitionen_ids)){
            
            partition<-partitionen_ids[j]
            varianten_indx<-which(r$id %in% r$id[which(r$fahrlage == fahrlage & r$partition == partition)])
            
            ex<-extend_solution_spaces(el_blocked,r,varianten_indx,n) # Mach Wegesuche für alle Varianten der Partition j der Fahrlage i
            r_candid<-ex$r
            wegeSuche_count<-wegeSuche_count+ex$wegeSuche_count

            
            qual_candid<-(2-(as.integer(apply(r[varianten_indx,10:(9+n)],1,sum))/n) %*% 
                            r_candid$minFahrzeit[varianten_indx]/(min(r$minFahrzeit[which(r$fahrlage==fahrlage & r$minFahrzeit!=0)])))*r_candid$homogen[varianten_indx[1]]
            
            # Wenn meine Qualitätskennzahl besser ist und neue Wege gefunden wurden, dann wird diese Partition mein Gewinner
            if(!all(r_candid$res==r$res) & qual_candid>qual_max){
              
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
    
    ## Falls immer noch kein neuer Weg gefunden wurde, sperre Systemtrassen separat im Konfliktzeit von 
    ## Paaren von Makrokonflikt Fahrlagen
    
    if(terminate==FALSE){
      stufe2_count<-stufe2_count-1
      stufe3_count<-stufe3_count+1
      
      for(k in 1:length(mikro)){
        for(p in 1:(length(makro)-1)){
          for(q in p:length(makro)){
            
            el_blocked<-block_trassen(el,r,makro[c(p,q)],mikro[k],n)
            
            
            for(z in c(p,q)){
              
              fahrlage<-makro[i]
              partitionen_ids<-unique(r$partition[which(r$partition %in% r$partition[which(r$fahrlage == fahrlage)])])
              
              for(j in 1:length(partitionen_ids)){
                
                partition<-partitionen_ids[j]
                varianten_indx<-which(r$id %in% r$id[which(r$fahrlage == fahrlage & r$partition == partition)])
                
                ex<-extend_solution_spaces(el_blocked,r,varianten_indx,n) # Mach Wegesuche für alle Varianten der Partition j der Fahrlage i
                r_candid<-ex$r
                wegeSuche_count<-wegeSuche_count+ex$wegeSuche_count

                qual_candid<-(2-(as.integer(apply(r[varianten_indx,10:(9+n)],1,sum))/n) %*% 
                                r_candid$minFahrzeit[varianten_indx]/(min(r$minFahrzeit[which(r$fahrlage==fahrlage & r$minFahrzeit!=0)])))*r_candid$homogen[varianten_indx[1]]
                
                # Wenn meine Qualitätskennzahl besser ist und neue Wege gefunden wurden, dann wird diese Partition mein Gewinner
                if(!all(r_candid$res==r$res) & qual_candid>qual_max){
                  
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
      stufe3_count<-stufe3_count-1
      
      ablehnung_id<-makro[length(makro)]
      r<-r[which(r$fahrlage!=ablehnung_id),]
      print(paste("Lehne Fahrlage ",ablehnung_id," ab.",sep=""))
      
    }
    
  }else{
    
    ## Erweitere Lösungsraum mit jetzt geteilten Systemtrassen
    #varianten_indx<-which(r$fahrlage %in% makro)
    #ex<-extend_solution_spaces(el,r,varianten_indx,n)
    #el<-ex$el
    #r<-ex$r 
    #wegeSuche_count<-wegeSuche_count+ex$wegeSuche_count
    
  }
  
  ## Encode
  enc<-encode(el,r[which(r$valid==1),])
  A<-enc$A
  dir<-enc$dir
  b<-enc$b
  omega<-enc$omega
  print(dim(A))
  
  ## Solve SAT
  res<-lp(direction="max", objective.in=omega, const.mat=A, const.dir=dir, const.rhs=b, all.bin=TRUE)

}

## Print Gesamtlösung
x<-res$solution
print(res)
solution<-data.frame(t(x))
colnames(solution)[1:length(unique(r$fahrlage[which(r$valid==1)]))]<-paste("r", unique(r$fahrlage),sep="")

count<-length(unique(r$fahrlage))+1
for(i in 1:nrow(unique(r[which(r$valid==1),c("partition","fahrlage")]))){
  colnames(solution)[count]<-paste("p_(",unique(r[which(r$valid==1),c("partition","fahrlage")])[i,2],",",
                                   unique(r[which(r$valid==1),c("partition","fahrlage")])[i,1],")",sep="")
  count<-count+1
}

for(i in 1:nrow(unique(r[which(r$valid==1),c("id","partition","fahrlage")]))){
  colnames(solution)[count]<-paste("x_(",unique(r[which(r$valid==1),c("id","partition","fahrlage")])[i,3],",",
                                   unique(r[which(r$valid==1),c("id","partition","fahrlage")])[i,2],")^",
                                   unique(r[which(r$valid==1),c("id","partition","fahrlage")])[i,1],sep="")
  count<-count+1
}

colnames(solution)[count:length(solution)]<-paste("s_",el$id[which(el$beenparent==0)],"^",
                                                  sort(rep(rep(r$id[which(r$valid==1)]),
                                                           nrow(el[which(el$beenparent==0),]))),sep="")
print(solution[which(solution==1)])

## Print KPIs
print(paste("Die Wegesuche wurde ",wegeSuche_count," mal aufgerufen.",sep=""))
#print(paste("ES wurden ",length(unique(el$parent))," Trassen geteilt.",sep=""))
print(paste("Es wurde ",stufe1_count," mal Stufe 1, ",stufe2_count," mal Stufe 2 und ",stufe3_count," mal Stufe 3 angewandt.",sep=""))
print(paste("Es wurden ",ablehnen_count-length(unique(r$fahrlage))," Fahrlagen abgelehnt.",sep=""))
#el[which(el$beenparent==0),][which(el[which(el$beenparent==0),]$id %in% c(100,101,104,105,106,107,108,125,126,130,131)),]