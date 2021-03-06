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
source("Tests/unit test patrick mit Bau.R")

## Lade el und r
data<-init_data() 
el<-data$el
r<-data$r
n<-data$n

## Initialisiere Solution Spaces
wegeSuche_count<-0
init<-extend_solution_spaces(el,r,1:nrow(r),n) 
el<-init$el
r<-init$r
wegeSuche_count<-init$wegeSuche_count

## Init minFahrzeiten der FahrlagenVar (copy von 1. Partition)
for(i in 1:(nrow(r)-1)){
  if(r$fahrlage[i+1]==r$fahrlage[i]){
    r$minFahrzeit[i+1]<-r$minFahrzeit[i]
  }
}

## Initialisiere L�sung 
# Encode Initialdaten
enc<-encode(el,r)
A<-enc$A
dir<-enc$dir
b<-enc$b
omega<-enc$omega

# Solve SAT f�r Initialdaten
res<-lp(direction="max", objective.in=omega, const.mat=A, const.dir=dir, const.rhs=b, all.bin=TRUE)

## Initialisiere Z�hler
stufe1_count<-0
stufe2_count<-0
stufe3_count<-0
ablehnen_count<-length(unique(r$fahrlage))

### Solange das Gesamtproblem nicht l�sbar ist...
while(res$status!=0){
  
  #print(el)
  #print(r)
  
  ## Finde einen Makrokonflikt Fahrlagen
  makro<-makro_konflikte(el[which(el$beenparent==0),],r[which(r$valid==1),])
  print(makro)
  
  ## Finde einen Mikrokonflikt 
  benutzteSystemtrassen<-unique(as.integer(unlist(strsplit(r$res[which(r$valid==1 & r$fahrlage %in% makro)], split=", "))))
  
  ## Falls der Makrokonlikt nur entsteht, weil eine Fahrlage noch �berhaupt keinen Weg hat, dann suche sofort nach
  ## bestm�glicher Partitionierung:
  if(length(benutzteSystemtrassen)==1){
    
    terminate<-FALSE
    for(i in 1:length(makro)){
      qual_max<-(-1000)
      fahrlage<-makro[i]
      partitionen_ids<-unique(r$partition[which(r$partition %in% r$partition[which(r$fahrlage == fahrlage)])])
      
      for(j in 1:length(partitionen_ids)){
        
        partition<-partitionen_ids[j]
        varianten_indx<-which(r$id %in% r$id[which(r$fahrlage == fahrlage & r$partition == partition)])
        
        ex<-extend_solution_spaces(el,r,varianten_indx,n) # Mach Wegesuche f�r alle Varianten der Partition j der Fahrlage i
        r_candid<-ex$r
        wegeSuche_count<-wegeSuche_count+ex$wegeSuche_count
        
        
        qual_candid<-(2-(as.integer(apply(r[varianten_indx,10:(9+n)],1,sum))/n) %*% 
                        r_candid$minFahrzeit[varianten_indx]/(min(r$minFahrzeit[which(r$fahrlage==fahrlage & r$minFahrzeit!=0)])))*r_candid$homogen[varianten_indx[1]]

        
        if(!all(r_candid$res==r$res) & qual_candid>qual_max){# & (r$valid[varianten_indx[1]]==1 | alleWege==TRUE)){
          
          qual_max<-qual_candid
          fahrlage_gewinner<-fahrlage
          partition_gewinner<-partition
          r_gewinner<-r_candid
          terminate<-TRUE
        }
      }
    }
    if(terminate==TRUE){
      
      ## Erg�nze r um neuen Weg/ neue Partition des Gewinners
      r<-r_gewinner
      indx_update<-which(r$partition==partition_gewinner & r$fahrlage==fahrlage_gewinner)
      r$valid[indx_update]<-rep(1,length(indx_update))
      print(paste("The winner is: Fahrlage ",fahrlage_gewinner," und Partition ",partition_gewinner,sep=""))
      
    }
    
    
    ## Falls keine neuen Wege f�r keine Partition gefunden und der Konflikt demnach nicht aufgel�st wurde, lehne Fahrlagen ab
    if(terminate==FALSE){
      #stufe3_count<-stufe3_count-1
      stufe2_count<-stufe2_count-1
      
      ablehnung_id<-makro[length(makro)]
      r<-r[which(r$fahrlage!=ablehnung_id),]
      print(paste("Lehne Fahrlage ",ablehnung_id," ab.",sep=""))
      
    }
    
  }else{
    mikro<-mikro_konflikte(el[which(el$beenparent==0 & el$id %in% benutzteSystemtrassen),],r[which(r$valid==1),],makro)
    print(mikro)
    
    ## L�se Mikrokonflikt
    # 1. Versuche Systemtrassen aufzuteilen
    nrow_el_alt<-nrow(el)
    sep<-sep_trassen(el,r,n,mikro,makro)
    el<-sep$el
    r<-sep$r
    
    # 2 Falls keine Systemtrasse geteilt werden konnte, sperre Mikrokonflikttrassen
    #   (f�r jede noch nicht valide Partition, und f�ge diejenige Partition hinzu, 
    #   die die Qualit�tskennzahl am wenigsten verringert) 
    #   im entsprechenden Konfliktzeitraum, um alternative Wege zu finden 
    #   Gehe vor nach Stufenprinzip
    
    if(nrow(el)==nrow_el_alt){
      qual_max<-(-1000)
      terminate<-FALSE
      #terminate_local<-FALSE
      
      ## Sperre zun�chst alle Systemtrassen (OR Verkn�pfung) gemeinsam auf Konfliktzeit aller Fahrlagen im
      ## Makrokonflikt
      
      el_blocked<-block_trassen(el,r,makro,mikro,n)
      stufe1_count<-stufe1_count+1
      
      for(i in 1:length(makro)){
        fahrlage<-makro[i]
        partitionen_ids<-unique(r$partition[which(r$partition %in% r$partition[which(r$fahrlage == fahrlage)])])
        
        for(j in 1:length(partitionen_ids)){
          
          partition<-partitionen_ids[j]
          varianten_indx<-which(r$id %in% r$id[which(r$fahrlage == fahrlage & r$partition == partition)])
          
          ex<-extend_solution_spaces(el_blocked,r,varianten_indx,n) # Mach Wegesuche f�r alle Varianten der Partition j der Fahrlage i
          r_candid<-ex$r
          wegeSuche_count<-wegeSuche_count+ex$wegeSuche_count
          
          qual_candid<-(2-(as.integer(apply(r[varianten_indx,10:(9+n)],1,sum))/n) %*% 
                          r_candid$minFahrzeit[varianten_indx]/(min(r$minFahrzeit[which(r$fahrlage==fahrlage & r$minFahrzeit!=0)])))*r_candid$homogen[varianten_indx[1]]
          
          # Pr�fe bei neuen Partitionen, dass wirklich jede Fahrlagenvariante einen Weg hat
          #alleWege<-TRUE
          
          # for(k in 1:length(varianten_indx)){
          #    if(r$res[varianten_indx]=="0"){
          #      alleWege<-FALSE
          #    }
          #  }
          
          # Wenn meine Qualit�tskennzahl besser ist und neue Wege gefunden wurden (bzw. bei 
          # neuen Partitionen, ob alle Fahrlagenvarianten einen Weg haben), dann wird diese Partition mein Gewinner
          
          if(!all(r_candid$res==r$res) & qual_candid>qual_max){# & (r$valid[varianten_indx[1]]==1 | alleWege==TRUE)){
            
            qual_max<-qual_candid
            fahrlage_gewinner<-fahrlage
            partition_gewinner<-partition
            r_gewinner<-r_candid
            terminate<-TRUE
            #terminate_local<-TRUE
          }
        }
        #if(terminate_local==TRUE){
        
        ## Erg�nze r um neuen Weg/ neue Partition des Gewinners pro Fahrlage im Makrokonflikt
        #  r<-r_gewinner
        #  indx_update<-which(r$partition==partition_gewinner & r$fahrlage==fahrlage_gewinner)
        #  r$valid[indx_update]<-rep(1,length(indx_update))
        #  print(paste("The winner is: Fahrlage ",fahrlage_gewinner," und Partition ",partition_gewinner,sep=""))
        #  
        #  terminate_local<-FALSE
        #  qual_max<-(-1000)
        #}
        
        
      }
      
      ## Falls kein neuer Weg gefunden wurde, sperre Systemtrassen separat, aber auf Konfliktzeit aller Fahrlagen im
      ## Makrokonflikt
      
      if(terminate==FALSE){
        stufe1_count<-stufe1_count-1
        stufe2_count<-stufe2_count+1
        
        
        for(k in 0:length(mikro)){
          
          ## Schalte dazwischen: Wenn keine Trasse gesprrt wird, k�nnen dennoch neue Wege f�r neue Partitionen gefunden werden
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
              
              ex<-extend_solution_spaces(el_blocked,r,varianten_indx,n) # Mach Wegesuche f�r alle Varianten der Partition j der Fahrlage i
              r_candid<-ex$r
              wegeSuche_count<-wegeSuche_count+ex$wegeSuche_count
              
              
              qual_candid<-(2-(as.integer(apply(r[varianten_indx,10:(9+n)],1,sum))/n) %*% 
                              r_candid$minFahrzeit[varianten_indx]/(min(r$minFahrzeit[which(r$fahrlage==fahrlage & r$minFahrzeit!=0)])))*r_candid$homogen[varianten_indx[1]]
              
              # Pr�fe bei neuen Partitionen, dass wirklich jede Fahrlagenvariante einen Weg hat
              #alleWege<-TRUE
              
              #for(k in 1:length(varianten_indx)){
              #  if(r$res[varianten_indx]=="0"){
              #    alleWege<-FALSE
              #  }
              #}
              
              # Wenn meine Qualit�tskennzahl besser ist und neue Wege gefunden wurden (bzw. bei 
              # neuen Partitionen, ob alle Fahrlagenvarianten einen Weg haben), dann wird diese Partition mein Gewinner
              
              if(!all(r_candid$res==r$res) & qual_candid>qual_max){# & (r$valid[varianten_indx[1]]==1 | alleWege==TRUE)){
                
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
      
      #if(terminate==FALSE){
      #  stufe2_count<-stufe2_count-1
      #  stufe3_count<-stufe3_count+1
      #  
      #  for(k in 1:length(mikro)){
      #    for(p in 1:(length(makro)-1)){
      #      for(q in p:length(makro)){
      #        for(a in 1:length(r$id[which(r$fahrlage==makro[p])])){
      #          for(b in 1:length(r$id[which(r$fahrlage==makro[q])])){
      #            
      #            el_blocked<-block_trassen(el,r,which(r$id %in% c(a,b)),mikro[k],n)
      #            
      #            for(z in c(p,q)){
      #              
      #              fahrlage<-makro[z]
      #              partitionen_ids<-unique(r$partition[which(r$partition %in% r$partition[which(r$fahrlage == fahrlage)])])
      #              
      #              for(j in 1:length(partitionen_ids)){
      #                
      #                partition<-partitionen_ids[j]
      #                varianten_indx<-which(r$id %in% r$id[which(r$fahrlage == fahrlage & r$partition == partition)])
      #                
      #                ex<-extend_solution_spaces(el_blocked,r,varianten_indx,n) # Mach Wegesuche f�r alle Varianten der Partition j der Fahrlage i
      #                r_candid<-ex$r
      #                wegeSuche_count<-wegeSuche_count+ex$wegeSuche_count
      #                
      #                qual_candid<-(2-(as.integer(apply(r[varianten_indx,10:(9+n)],1,sum))/n) %*% 
      #                                r_candid$minFahrzeit[varianten_indx]/(min(r$minFahrzeit[which(r$fahrlage==fahrlage & r$minFahrzeit!=0)])))*r_candid$homogen[varianten_indx[1]]
      #                
      #                # Pr�fe bei neuen Partitionen, dass wirklich jede Fahrlagenvariante einen Weg hat
      #                alleWege<-TRUE
      #                
      #                for(k in 1:length(varianten_indx)){
      #                  if(r$res[varianten_indx]=="0"){
      #                    alleWege<-FALSE
      #                  }
      #                }
      #                
      #                # Wenn meine Qualit�tskennzahl besser ist und neue Wege gefunden wurden (bzw. bei 
      #                # neuen Partitionen, ob alle Fahrlagenvarianten einen Weg haben), dann wird diese Partition mein Gewinner
      #                
      #                if(!all(r_candid$res==r$res) & qual_candid>qual_max & (r$valid[varianten_indx[1]]==1 | alleWege==TRUE)){
      #                  
      #                  qual_max<-qual_candid
      #                  fahrlage_gewinner<-fahrlage
      #                  partition_gewinner<-partition
      #                  r_gewinner<-r_candid
      #                  terminate<-TRUE
      #                }
      #              }
      #            }
      #          }
      #        }
      #      }
      #    }
      #  }
      #}
      
      if(terminate==TRUE){
        
        ## Erg�nze r um neuen Weg/ neue Partition des Gewinners
        r<-r_gewinner
        indx_update<-which(r$partition==partition_gewinner & r$fahrlage==fahrlage_gewinner)
        r$valid[indx_update]<-rep(1,length(indx_update))
        print(paste("The winner is: Fahrlage ",fahrlage_gewinner," und Partition ",partition_gewinner,sep=""))
        
      }
      
      
      ## Falls keine neuen Wege in keiner Stufe gefunden und der Konflikt demnach nicht aufgel�st wurde, lehne Fahrlagen ab
      if(terminate==FALSE){
        #stufe3_count<-stufe3_count-1
        stufe2_count<-stufe2_count-1
        
        ablehnung_id<-makro[length(makro)]
        r<-r[which(r$fahrlage!=ablehnung_id),]
        print(paste("Lehne Fahrlage ",ablehnung_id," ab.",sep=""))
        
      }
      
    }
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

## Print Gesamtl�sung
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
#print(solution[which(solution==1)])

write.csv(el,"elmitBau.csv")
write.csv(r,"rmitBau.csv")
write.csv(solution,"solutionmitBau.csv")

KPIs<-data.frame(stufe1=stufe1_count,stufe2=stufe2_count,ablehnung=ablehnen_count-length(unique(r$fahrlage)),wegesuche=wegeSuche_count)
write.csv(KPIs,"KPIsmitBau.csv")


## Print KPIs
#print(paste("Die Wegesuche wurde ",wegeSuche_count," mal aufgerufen.",sep=""))
#print(paste("ES wurden ",length(unique(el$parent))," Trassen geteilt.",sep=""))
#print(paste("Es wurde ",stufe1_count," mal Stufe 1, ",stufe2_count," mal Stufe 2 und ",stufe3_count," mal Stufe 3 angewandt.",sep=""))
#print(paste("Es wurden ",ablehnen_count-length(unique(r$fahrlage))," Fahrlagen abgelehnt.",sep=""))
#el[which(el$beenparent==0),][which(el[which(el$beenparent==0),]$id %in% c(71,72,73,100,101,138,139,140,147,148,149,159,160,161)),]