## Set Working Directory
# ##
setwd("~/Diplomarbeit/Diplom/ganzjahr-master/Code")

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
source("add_partition.R")

## Start Loggingr
filename<-"gesamt_verb_auf.txt"
sink(filename)

##Zeit
print("Startzeit")
Sys.time()

max_1<-function(x){
  return(max(x,1))
}

### Load and initialize Data
source("Tests/gesamt_verb_ganz.r")


## Lade Daten
data<-load_data() 
el<-data$el
r<-data$r
n<-data$n
v_top<-data$v_top

## Check if test set meets requirements
stopifnot(all(r$id==1:nrow(r)))


## Initialize ids
el$id<-1:nrow(el)

## Suche Initial Wege f�r Solution Spaces und Z�hler f�r Wegesuche
wegeSuche_count<-0
init<-extend_solution_spaces(el,r,1:nrow(r),v_top,n) 
el<-init$el
r<-init$r
wegeSuche_count<-init$wegeSuche_count

#for(i in 1:nrow(r)){
#  if(r$res[i]=="0"){
#    r$abgelehnt[i]<-1
#    print("Fahrlage abgelehnt")
#    print(r$id[i])
#  }
#}

for (i in unique(r$fahrlage)){
  if (all(r$res[which(r$fahrlage==i)]=="0")){
    #stopifnot(length(which(r$fahrlage==i)==1))
    r$abgelehnt[which(r$fahrlage==i)]<-1
    print("Fahrlage abgelehnt")
    print(r$id[which(r$fahrlage==i)])
  }
}

## Initialisiere Validity
ablehnen_count<-length(unique(r$fahrlage))
#r<-validity(r)

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

## Set dynamische Schneideregel
dyn_split <- T
maxcount<-1
count<-0

### Solange das Gesamtproblem nicht l�sbar ist...
while(res$status!=0 & count <= maxcount){
  count <- count #+ 1
  nrow_old<-nrow(r)
  
  ## Finde einen Makrokonflikt Fahrlagen
  makro<-makro_konflikte(el[which(el$beenparent==0),],r[which(r$abgelehnt==0 & r$valid==1),])
  print("Makro:")
  print(makro)
  print(r)
  
  
  ## Finde einen Mikrokonflikt
  benutzteSystemtrassen<-unique(as.integer(unlist(strsplit(r$res[which(r$valid==1 & r$fahrlage %in% makro)], split=", "))))
  mikro<-mikro_konflikte(el[which(el$beenparent==0 & el$id %in% benutzteSystemtrassen),],r[which(r$fahrlage %in% makro & r$valid==1),],makro)
  print("Mikro")
  print(mikro)
  
  ## L�se Mikrokonflikt
  # 1. Versuche Systemtrassen aufzuteilen
  nrow_el_alt<-nrow(el)
  sep<-sep_trassen(el,r,n,mikro,makro)
  el<-sep$el
  r<-sep$r
  
  print("el")
  print(el)
  
  # 2 Falls keine Systemtrasse geteilt werden konnte, sperre Mikrokonflikttrassen
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
      keinupdate<-F
      
      fahrlage<-makro[i]
      partitionen_ids<-unique(r$partition[which(r$partition %in% r$partition[which(r$fahrlage == fahrlage)])])
      j<-1
      #for(j in 1:length(partitionen_ids)){
        
        partition<-partitionen_ids[j]
        varianten_indx<-which(r$id %in% r$id[which(r$fahrlage == fahrlage & r$partition == partition)])
        
        if(dyn_split==T){
          ex<-add_partition(el,el_blocked,r,varianten_indx,v_top,n) # Setze neue Partitionen
        }else{
          ex<-extend_solution_spaces(el_blocked,r,varianten_indx,n) # Mach Wegesuche f�r alle Varianten der Partition j der Fahrlage i
        }
        print("Habe neue Kandidaten f�r r und el")
        r_candid<-ex$r
        el_candid<-ex$el
        wegeSuche_count<-wegeSuche_count+ex$wegeSuche_count
        
        
        #qual_candid<-(2-(as.integer(apply(r[varianten_indx,11:(10+n)],1,sum))/n) %*% 
        #                r_candid$minFahrzeit[varianten_indx]/(min(r$minFahrzeit[which(r$fahrlage==fahrlage & r$minFahrzeit!=0)])))*r_candid$homogen[varianten_indx[1]]
        
        
        # Wenn meine Qualit�tskennzahl besser ist und neue Wege gefunden wurden (bzw. bei 
        # neuen Partitionen, ob alle Fahrlagenvarianten einen Weg haben), dann wird diese Partition mein Gewinner
        
        
        if((!all(r_candid$res==r$res) | (r$valid[varianten_indx[1]]==0) & (all(r$res[varianten_indx]!="0")))){
          print("Update r und el")
          r<-r_candid
          el<-el_candid
          
          # Make sure all systras have at least one valid day
          stopifnot(all(apply(el[,7:(6+n)],1,sum)>0))
          terminate<-TRUE
          print('r')
          print(r)
          #break
        }
        
        #if(nrow(r)==nrow(r_candid)){#ich hab keine neue partition gefunden
        #  if(!all(r_candid$res==r$res)){#alle Wege waren bereits bekannt
        #    keinupdate<-TRUE  
        #  }
        #}
        #if(keinupdate==FALSE){
        #  r<-r_candid
        #  el<-el_candid
        #  terminate<-TRUE
        #}
        #if((!all(r_candid$res==r$res) | (r$valid[varianten_indx[1]]==0) & (all(r$res[varianten_indx]!="0"))) & qual_candid>qual_max){
        #  
        #  qual_max<-qual_candid
        #  fahrlage_gewinner<-fahrlage
        #  partition_gewinner<-partition
        #  r_gewinner<-r_candid
        #  terminate<-TRUE
        #}
      #}
    }
    
    ## Falls kein neuer Weg gefunden wurde, sperre Systemtrassen separat, aber auf Konfliktzeit aller Fahrlagen im
    ## Makrokonflikt
    
    if(terminate==FALSE){
      print("Versuche es mit Stufe 2")
      
      stufe1_count<-stufe1_count-1
      stufe2_count<-stufe2_count+1
      
      
      for(k in 0:length(mikro)){
        
        ## Schalte dazwischen: Wenn keine Trasse gesperrt wird, k�nnen dennoch neue Wege f�r neue Partitionen gefunden werden
        if(k==0){
          el_blocked<-el
        }else{
          print("hier")
          el_blocked<-block_trassen(el,r,makro,mikro[k],n)
        }
        
        for(i in 1:length(makro)){
          #qual_max<-(-1000)
          keinupdate<-F
          fahrlage<-makro[i]
          partitionen_ids<-unique(r$partition[which(r$partition %in% r$partition[which(r$fahrlage == fahrlage)])])
          
          for(j in 1:length(partitionen_ids)){
            j<-1
            
            partition<-partitionen_ids[j]
            varianten_indx<-which(r$id %in% r$id[which(r$fahrlage == fahrlage & r$partition == partition)])
            
            if(dyn_split==T){
              ex<-add_partition(el,el_blocked,r,varianten_indx,v_top,n) # Setze neue Partitionen
            }else{
              ex<-extend_solution_spaces(el_blocked,r,varianten_indx,n) # Mach Wegesuche f�r alle Varianten der Partition j der Fahrlage i
            }
            print("Habe neue Kandidaten f�r r und el")
            
            r_candid<-ex$r
            el_candid<-ex$el
            wegeSuche_count<-wegeSuche_count+ex$wegeSuche_count
            
            
            #qual_candid<-(2-(as.integer(apply(r[varianten_indx,11:(10+n)],1,sum))/n) %*% 
            #                r_candid$minFahrzeit[varianten_indx]/(min(r$minFahrzeit[which(r$fahrlage==fahrlage & r$minFahrzeit!=0)])))*r_candid$homogen[varianten_indx[1]]
            
            
            # Wenn meine Qualit�tskennzahl besser ist und neue Wege gefunden wurden (bzw. bei 
            # neuen Partitionen, ob alle Fahrlagenvarianten einen Weg haben), dann wird diese Partition mein Gewinner
            
            if((!all(r_candid$res==r$res) | (r$valid[varianten_indx[1]]==0) & (all(r$res[varianten_indx]!="0")))){
              print("Update r und el")
              
              r<-r_candid
              el<-el_candid
              
              # Make sure all systras have at least one valid day
              stopifnot(all(apply(el[,7:(6+n)],1,sum)>0))
              terminate<-TRUE
              #print('2.Sperrung -r')
              #print(r)
              #break
            }
            
          #  if(nrow(r)==nrow(r_candid)){#ich hab keine neue partition gefunden
          #    #if(!all(r_candid$res==r$res)){#alle Wege waren bereits bekannt
          #      keinupdate<-TRUE  
          #    #}
          #  }
          #  if(keinupdate==FALSE){
          #    r<-r_candid
          #    el<-el$candid
          #    terminate<-TRUE
          #  }
            
            #if((!all(r_candid$res==r$res) | (r$valid[varianten_indx[1]]==0) & (all(r$res[varianten_indx]!="0"))) & qual_candid>qual_max){
              
            #  qual_max<-qual_candid
            #  fahrlage_gewinner<-fahrlage
            #  partition_gewinner<-partition
            #  r_gewinner<-r_candid
            #  terminate<-TRUE
            #}
          }
        }
      }  
    }
    
   
    #if(terminate==TRUE){
    #  
    ### Erg�nze r um neuen Weg/ neue Partition des Gewinners
    #  r<-r_gewinner
    #  indx_update<-which(r$partition==partition_gewinner & r$fahrlage==fahrlage_gewinner)
    #  r$valid[indx_update]<-rep(1,length(indx_update))
    #  print(paste("The winner is: Fahrlage ",fahrlage_gewinner," und Partition ",partition_gewinner,sep=""))
    #
    #}
    #
    
    ## Falls keine neuen Wege in keiner Stufe gefunden und der Konflikt demnach nicht aufgel�st wurde, lehne Fahrlagen ab
    ## Zus�tzlich (f�r dyn. Schneideregeln): Lehne nicht ab, wenn eine neue Partition hinzu gekommen ist!
    if(terminate==FALSE & nrow(r)==nrow_old){
      stufe2_count<-stufe2_count-1
      
      ablehnung_id<-makro[length(makro)]
      #if(ablehnung_id==19){break}
      r$abgelehnt[which(r$fahrlage==ablehnung_id)]<-rep(1,length(which(r$fahrlage==ablehnung_id)))
      print(paste("Lehne Fahrlage ",ablehnung_id," ab.",sep=""))
      
    }
  }
  
  
  ## Encode
  print("Start Encoding")
  enc<-encode(el,r[which(r$abgelehnt==0 & r$valid==1),])
  A<-enc$A
  dir<-enc$dir
  b<-enc$b
  omega<-enc$omega
  print(dim(A))
  
  ## Solve SAT
  res<-lp(direction="max", objective.in=omega, const.mat=A, const.dir=dir, const.rhs=b, all.bin=TRUE)
  print(res$status)
}

## Dekodiere Gesamtl�sung
solution<-decode(res$solution,el,r)

## Speicher als csv
write.csv(el,"gesamt_verb_auf_el.csv")
write.csv(r,"gesamt_verb_auf_r.csv")
write.csv(solution,"gesamt_verb_auf_solution.csv")

KPIs<-data.frame(stufe1=stufe1_count,stufe2=stufe2_count,ablehnung=ablehnen_count-length(unique(r$fahrlage[which(r$abgelehnt==0)])),wegesuche=wegeSuche_count)
write.csv(KPIs,"gesamt_verb_auf_KPIs.csv")


## Print KPIs
print(paste("Die Wegesuche wurde ",wegeSuche_count," mal aufgerufen.",sep=""))
print(paste("ES wurden ",length(unique(el$parent))," Trassen geteilt.",sep=""))
#print(paste("Es wurde ",stufe1_count," mal Stufe 1, ",stufe2_count," mal Stufe 2 und ",stufe3_count," mal Stufe 3 angewandt.",sep=""))
#print(paste("Es wurden ",ablehnen_count-length(unique(r$fahrlage))," Fahrlagen abgelehnt.",sep=""))
#el[which(el$beenparent==0),][which(el[which(el$beenparent==0),]$id %in% c(71,72,73,100,101,138,139,140,147,148,149,159,160,161)),]

##Zeit
print("Endzeit")
Sys.time()

## Ende Logging
sink()
