#Author: D.Travin 2016/10/21

rm(list=ls())
#setwd("./")
#setwd("D:/YAZ_project/3_2016_Summer/workspace")
ptm <- proc.time()

library(xlsx)
library(ontoCAT)
library(stringr)

 args <-commandArgs(TRUE)
 species <- toString(args[1])        #ZF or carp
 form <- toString(args[2])           #xls/xlsx/tsv
 place <- toString(args[3])    
 IDtype <- toString(args[4])         #Uniprot/ZFIN/ etc.
 dataset_name <- toString(args[5])

#species<-'mouse'
#form<-'xlsx'
#place<-'D:/YAZ_project/3_2016_Summer/workspace/dataset_mouse/'
#dataset_name<-'datasetMagnus'
#IDtype <- 'ZFIN'

#=================================== Input of the table and list of IDs =============================================================
if(form=='tsv'){
  inuser=read.table(paste(place,'inuser.tsv', sep=''),header=TRUE,row.names=1, check.names = FALSE)
}else if(form=='xlsx'){
  inuser=read.xlsx(paste(place,'inuser.xlsx',sep=''),1,header=TRUE,row.names=1, check.names = FALSE)
}else if(form=='xls'){
  inuser=read.xlsx(paste(place,'inuser.xls', sep=''),1,header=TRUE,row.names=1, check.names = FALSE)
}

if(species=='mouse'){
  if((grepl(pattern='[Mm][Aa][://._]', names(inuser)[1])==TRUE)|(grepl(pattern='[Mm][Aa][://._]', names(inuser)[1])==TRUE)){
    inuser=as.data.frame(t(inuser))
  }
  for(i in 1:nrow(inuser)){                                         #replace all other variants with "_"
    substr(row.names(inuser)[i], 3,3)<-'_'
  }
  res <- readLines("./mouse/smart_mouse.txt")
  smart = lapply(as.list(res[2:length(res)]), function(i){if(i=="") NULL else unlist(strsplit(i, " "))})
  names(smart) = strsplit(res[1], " ")[[1]]
  ontol=getOntology('./ontol/ma.obo')
  
  gender<-'both'
  sort_brain<-'small'
  zfs_zfa<-'zfa'
}else if(species=='ZF'|species=='carp'){
  
  if((grepl(pattern='[Zz][Ff][AaSs][://._]', names(inuser)[1])==TRUE)|(grepl(pattern='[Mm][Aa][://._]', names(inuser)[1])==TRUE)){
    inuser=as.data.frame(t(inuser))
  }
  for(i in 1:nrow(inuser)){                                         #replace all other variants with "_"
    substr(row.names(inuser)[i], 4,4)<-'_'
  }
  res <- readLines("./zebrafish/smart_ZF.txt")
  smart = lapply(as.list(res[2:length(res)]), function(i){if(i=="") NULL else unlist(strsplit(i, " "))})
  names(smart) = strsplit(res[1], " ")[[1]]
  ontol=getOntology('./ontol/zfa.obo.txt')
  
  zfs <- 'no'
  zfa <- 'no'
  zfs_zfa <- 'no'
  
  if(TRUE %in% (grepl(pattern='[Zz][Ff][Aa][_]', row.names(inuser))==TRUE)==TRUE){
    zfa <- 'yes'
  }
  if((TRUE %in% (grepl(pattern='[Zz][Ff][Ss][_]', row.names(inuser))==TRUE)==TRUE)&(species=='ZF')){
    zfs <- 'yes'
  }
  
  if(zfs=='yes'&zfa=='yes'){
    zfs_zfa<-'both'
  }else if(zfs=='no'&zfa=='yes'){
    zfs_zfa<-'zfa'
  }else if(zfs=='yes'&zfa=='no'){
    zfs_zfa<-'zfs'
  }
  
  brain<-file('./ontol/brain.txt')
  brainlist <- strsplit(readLines(brain), " ")
  close(brain)
  
  ovary<-file('./ontol/ovary.txt')
  ovarylist <- strsplit(readLines(ovary), " ")
  close(ovary)
  
  testis<-file('./ontol/testis.txt')  
  testislist <- strsplit(readLines(testis), " ")
  close(testis)
  
  sort_brain <- 'smallbrain'
  gender_m <- FALSE
  gender_f <- FALSE
  
  for(i in 1:length(row.names(inuser))){                  #decide small or middle brain to draw
    if(row.names(inuser)[i] %in% brainlist == 'TRUE'){
      sort_brain <- 'middlebrain'
    }
    i=i+1
  }
  
  for(i in 1:length(row.names(inuser))){
    if(row.names(inuser)[i] %in% ovarylist == 'TRUE'){
      gender_f <- TRUE
    }
    i=i+1
  }
  
  for(i in 1:length(row.names(inuser))){
    if(row.names(inuser)[i] %in% testislist == 'TRUE'){
      gender_m <- TRUE
    }
    i=i+1
  }
  
  if(gender_f & gender_m == TRUE){                  #decide which gender(f/m/both/none) to draw
    gender <- 'both'
  }else if((gender_f==TRUE)&(gender_m==FALSE)){
    gender <- 'female'
  }else if((gender_f==FALSE)&(gender_m==TRUE)){
    gender <- 'male'
  }else if(gender_f & gender_m == FALSE){
    gender <- 'none'
  }
}

list_of_proteins<-names(inuser)                                    #list all protein names
write(names(inuser), paste(place,'ProteinIDlist.txt', sep=''))
outtable<-inuser[-(1:nrow(inuser)),]
file.remove(paste(place,'result_pr.txt', sep=''))
outfile<-paste(place,'result_pr.txt', sep='')
perfect_match<-c()

for(i in 1:nrow(inuser)){
  old_id<-row.names(inuser)[i]
  if(length(smart[[old_id]])>0){
    if(smart[[old_id]][1]=='too_general_ZFA'){
      write(paste(old_id, getTermNameById(ontol,as.character(old_id)),'is too general and cannot be applied to the picture', sep=' '), outfile, append=TRUE)
      print("!!")
    }else{
      write(paste(old_id,' (',getTermNameById(ontol,as.character(old_id)),')',' is found', sep=''), outfile, append=TRUE)
      for(j in 1:length(smart[[old_id]])){
        new_id<-smart[[old_id]][j]
        if(new_id==old_id){
          print('!!!')
          write(paste('=>',old_id,'(',getTermNameById(ontol,as.character(old_id)),')',' ONLY applied to ', smart[[old_id]][j],'(',getTermNameById(ontol,as.character(smart[[old_id]][j])),')', sep=''), outfile, append=TRUE)
          perfect_match[length(perfect_match)+1]<-new_id
          if((new_id %in% row.names(outtable))=='TRUE'){
            for(k in 1:nrow(outtable)){
              if(new_id==row.names(outtable)[k]){
                h<-k                                      #h is the row in the outtable where we now start to write
              }
            }
            for(l in 1:ncol(outtable)){
              outtable[h,l]=inuser[i,l]
            }
          }else{
            outtable=rbind(outtable,inuser[i,])
            row.names(outtable)[nrow(outtable)]<-new_id
          }
        }else{
          if((new_id %in% perfect_match)==FALSE){
            print('opa')
            write(paste('=>',old_id,'(',getTermNameById(ontol,as.character(old_id)),')',' applied to ', smart[[old_id]][j],'(',getTermNameById(ontol,as.character(smart[[old_id]][j])),')', sep=''), outfile, append=TRUE)
            if((new_id %in% row.names(outtable))=='TRUE'){
              write('non-uniqueID', outfile, append=TRUE)
              for(k in 1:nrow(outtable)){
                if(new_id==row.names(outtable)[k]){
                  h<-k                                      #h is the row in the outtable where we now start to write
                }
              }
              for(l in 1:ncol(outtable)){
                if(inuser[i,l]!=0){
                  outtable[h,l]=(outtable[h,l]+inuser[i,l])/2
                }
              }
            }else{
              outtable=rbind(outtable,inuser[i,])
              row.names(outtable)[nrow(outtable)]<-new_id
            }
          }
        }
      }
    }
    print(i)
  }else{
    write(paste('for ID: ', old_id,' (',getTermNameById(ontol,as.character(old_id)),')',' no corresponding term found for this ID', sep=''), outfile, append=TRUE)
  }
}

write.table(outtable,paste(place,'outuser.tsv', sep=''))

fake<-outtable

min_val=max(fake[,1:ncol(fake)])
for(i in 1:nrow(fake)){
  for(j in 1:ncol(fake)){
    if((min_val>fake[i,j]) & (fake[i,j]!=0)){
      min_val=fake[i,j]
    }
  }
  print(i)
}

max_val=max(outtable[,1:ncol(fake)])

conffile=paste(place,'data.conf', sep='')

#================================================ Create configuration file for dataset ============================================
write(dataset_name, conffile, append=TRUE)
write(species, conffile, append=TRUE)
write(IDtype, conffile, append=TRUE)
write(sort_brain, conffile, append=TRUE)
write(gender, conffile, append=TRUE)
write(min_val, conffile, append=TRUE)
write(max_val, conffile, append=TRUE)
write(zfs_zfa, conffile, append=TRUE)
#===================================================================================================================================
