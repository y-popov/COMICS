#Processsing of uploaded tables and change of the ZFA(ZFS)_IDs to suitable for our picture, selection of a picture (brain/gonads decesion)
#Author: D.Travin 2016/08/12

rm(list=ls())
setwd("./")
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

# species<-'ZF'
# form<-'xlsx'
# place<-'D:/YAZ_project/3_2016_Summer/workspace/dataset_try/'
# dataset_name<-'dataset002'
# IDtype <- 'Uniprot'

#=================================== Input of the table and list of IDs =============================================================
if(form=='tsv'){
  inuser=read.table(paste(place,'inuser.tsv', sep=''),header=TRUE,row.names=1, check.names = FALSE)
}
if(form=='xlsx'){
  inuser=read.xlsx(paste(place,'inuser.xlsx',sep=''),1,header=TRUE,row.names=1, check.names = FALSE)
}
if(form=='xls'){
  inuser=read.xlsx(paste(place,'inuser.xls', sep=''),1,header=TRUE,row.names=1, check.names = FALSE)
}

if(grepl(pattern='[Zz][Ff][AaSs][://._]', names(inuser)[1])==TRUE){
  inuser=as.data.frame(t(inuser))
}

for(i in 1:nrow(inuser)){                                         #replace all other variants with "_"
     substr(row.names(inuser)[i], 4,4)<-'_'
}

list_of_proteins<-names(inuser)                                    #list all protein names
write(names(inuser), paste(place,'ProteinIDlist.txt', sep=''))

#=================================== Ontology changes of headers ====================================================================
outfile=paste(place,'result_pr.txt', sep='')
conffile=paste(place,'data.conf', sep='')

zfs <- 'no'
zfa <- 'no'

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

if(zfa=='yes'){
  ontol=getOntology('./ontol/zfa.obo.txt')
  if(species=='ZF'){
    ref_tab=read.table('./zebrafish/ref_tab_zf_2.tsv', header=TRUE)
    #ref_tab=read.table('./ref_tab_zf_2.tsv', header=TRUE)
  }else if(species=='carp'){
    ref_tab=read.table('./carp/ref_tab_carp.tsv', header=TRUE)
    #ref_tab=read.table('.ref_tab_carp.tsv', header=TRUE)
  }
  ref_list=ref_tab[,1]
  out_table=inuser[-(1:nrow(inuser)), ]
  
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

ChiPar <- function(x,y){        #returns the list of all IDs of parts and children of the term, x - Cv-term ID, y - Ontology object
  parts_and_children_id=c()
  parts=getTermRelationsById(y,as.character(x), "part of")
  if(length(parts)!=0){
    for(h in 1:length(parts)){
      parts_and_children_id[length(parts_and_children_id)+1]<-getAccession(parts[[h]])
    }
  }
  children=getAllTermChildrenById(y, as.character(x))
  if(length(children)!=0){
    for(h in 1:length(children)){
      parts_and_children_id[length(parts_and_children_id)+1]<-getAccession(children[[h]])
    }
  }
  return(parts_and_children_id)
}

#=====================================================ZFS part==================================================================

if(zfs=='yes'){
  out_table_zfs <- inuser[-(1:nrow(inuser)), ]
  ref_tab_zfs <- read.table('./zebrafish/ref_tab_zfs.tsv', header=TRUE)
  #ref_tab_zfs <- read.table('./ref_tab_zfs.tsv', header=TRUE)
  ref_list_zfs=ref_tab_zfs[,1]
  ontol_zfs <- getOntology('./ontol/zfs.obo.txt')
  larva<-ChiPar('ZFS_0000048',ontol_zfs)
  pharing<-ChiPar('ZFS_0000050',ontol_zfs)
  for(i in 1:length(row.names(inuser))){
    l1<-FALSE
    if((grepl(pattern='[Zz][Ff][Ss][://._]', row.names(inuser)[i])==TRUE)){
      zfs_old <- row.names(inuser)[i]
      write(paste('--------------------------------------------------------------'), outfile, append=TRUE)
      if(zfs_old %in% larva){
        zfs_new<-'ZFS_0000048'
        l1<-TRUE
        if((zfs_new %in% row.names(out_table_zfs))=='TRUE'){
          for(k in 1:nrow(out_table_zfs)){
            if(zfs_new==row.names(out_table)[k]){
              h<-k
            }
          }
          for(l in 1:ncol(out_table_zfs)){
            out_table_zfs[h,l]=(out_table_zfs[h,l]+inuser[i,l])/2
          }
        }else{
          out_table_zfs=rbind(out_table_zfs,inuser[i,])
          row.names(out_table_zfs)[nrow(out_table_zfs)]<-zfs_new
          write(paste(zfs_old,' (',getTermNameById(ontol_zfs,as.character(zfs_old)),')',' ----> ', zfs_new,' (',getTermNameById(ontol_zfs,as.character(zfs_new)),')', sep=''), outfile, append=TRUE)
        }
      }
      if(zfs_old %in% pharing){
        zfs_new<-'ZFS_0000050'
        l1<-TRUE
        if((zfs_new %in% row.names(out_table_zfs))=='TRUE'){
          for(k in 1:nrow(out_table_zfs)){
            if(zfs_new==row.names(out_table)[k]){
              h<-k
            }
          }
          for(l in 1:ncol(out_table_zfs)){
            out_table_zfs[h,l]=(out_table_zfs[h,l]+inuser[i,l])/2
          }
        }else{
          out_table_zfs=rbind(out_table_zfs,inuser[i,])
          row.names(out_table_zfs)[nrow(out_table_zfs)]<-zfs_new
          write(paste(zfs_old,' (',getTermNameById(ontol_zfs,as.character(zfs_old)),')',' ----> ', zfs_new,' (',getTermNameById(ontol_zfs,as.character(zfs_new)),')', sep=''), outfile, append=TRUE)
        }
      }
      if(l1==FALSE){
        if((zfs_old %in% ref_list_zfs)==TRUE){
          print('found')
          out_table_zfs=rbind(out_table_zfs,inuser[i,])
          write(paste(zfs_old,' (',getTermNameById(ontol_zfs,as.character(zfs_old)),')',' found among available', sep=''), outfile, append=TRUE)
        }else{
          write(paste(zfs_old, getTermNameById(ontol_zfs,as.character(zfs_old)),'is not found', sep=' '), outfile, append=TRUE)
        }
      }
    }
    print(paste('i=',i,sep=''))
  }
  write.table(out_table_zfs,paste(place,'outuser_zfs.tsv', sep=''))
}

#=====================================================ZFA part===================================================================

if(zfa=='yes'){
  f <- FALSE
  for(i in 1:length(row.names(inuser))){
    if((grepl(pattern='[Zz][Ff][Aa][://._]', row.names(inuser)[i])==TRUE)){
      f2 <- FALSE
      zfa_old=row.names(inuser)[i]
      write(paste('--------------------------------------------------------------'), outfile, append=TRUE)
      if((zfa_old %in% ref_list)=='TRUE'){                                  #if ZFA is identical
        out_table=rbind(out_table,inuser[i,])
        write(paste(zfa_old,' (',getTermNameById(ontol,as.character(zfa_old)),')',' found among available', sep=''), outfile, append=TRUE)
        print(paste('i=',i,sep=''))
      }else{                                                                #if not identical -> search among parents
        parent=getTermParentsById(ontol,as.character(zfa_old))
        while(length(parent)!=0){
          parent_id=getAccession(parent[[1]])
          for(j in 1:length(ref_list)){
            if(parent_id==ref_list[j]){
              write(paste(zfa_old,' (',getTermNameById(ontol,as.character(zfa_old)),')',' ----> ', ref_list[j],' (',getTermNameById(ontol,as.character(ref_list[j])),')', sep=''), outfile, append=TRUE)
              f<-TRUE
              zfa_new=toString(ref_list[j])
              if((zfa_new %in% row.names(out_table))=='TRUE'){
                for(k in 1:nrow(out_table)){
                  if(zfa_new==row.names(out_table)[k]){
                    h<-k
                  }
                }
                for(l in 1:ncol(out_table)){
                  out_table[h,l]=(out_table[h,l]+inuser[i,l])/2
                }
              }else{
                out_table=rbind(out_table,inuser[i,])
                row.names(out_table)[nrow(out_table)]<-zfa_new
              }
            }
          }
          if(f==TRUE){
            parent<-list()
          }else{
            parent=getTermParentsById(ontol,as.character(parent_id))
          }
        }
        if(f==FALSE){           #found no matches among parents, begin among children and parts
          listcheck<-ChiPar(zfa_old,ontol)
          for(k in 1:length(listcheck)){
            for(j in 1:length(ref_list)){
              if(ref_list[j]==listcheck[k]){
                print('URA!')
                f2<-TRUE
                zfa_new=toString(ref_list[j])
                if((zfa_new %in% row.names(out_table))=='TRUE'){
                  write('non-uniqueZFA', outfile, append=TRUE)
                  for(k in 1:nrow(out_table)){
                    if(zfa_new==row.names(out_table)[k]){
                      h<-k
                    }
                  }
                  for(l in 1:ncol(out_table)){
                    out_table[h,l]=(out_table[h,l]+inuser[i,l])/2
                  }
                }else{
                  out_table=rbind(out_table,inuser[i,])
                  row.names(out_table)[nrow(out_table)]<-zfa_new
                  write(paste(zfa_old,' (',getTermNameById(ontol,as.character(zfa_old)),')',' ----> ', ref_list[j],' (',getTermNameById(ontol,as.character(ref_list[j])),')', sep=''), outfile, append=TRUE)
                }
              }
            }
          }
          if(f2==FALSE){  #the worst case, we search for terms bound with two relations (e.g. child of the part of zfa_old)
            
            check<-ChiPar(zfa_old,ontol)
            repeat{
              h1<-length(check)
              exit<-FALSE
              for(j in 1:length(check)){
                check_i<-ChiPar(check[j],ontol)
                check<-c(check,check_i)
                check<-unique(check)
                for(k in 1:length(check_i)){
                  if(check_i[k] %in% ref_list){
                    print("!iuppi")
                    print(check_i[k])
                    zfa_new=toString(check_i[k])
                    if((zfa_new %in% row.names(out_table))=='TRUE'){
                      write('non-uniqueZFA', outfile, append=TRUE)
                      for(k in 1:nrow(out_table)){
                        if(zfa_new==row.names(out_table)[k]){
                          h<-k
                        }
                      }
                      for(l in 1:ncol(out_table)){
                        out_table[h,l]=(out_table[h,l]+inuser[i,l])/2
                      }
                    }else{
                      out_table=rbind(out_table,inuser[i,])
                      row.names(out_table)[nrow(out_table)]<-zfa_new
                      write(paste(zfa_old,' (',getTermNameById(ontol,as.character(zfa_old)),')',' ----> ', check_i[k],' (',getTermNameById(ontol,as.character(check_i[k])),')', sep=''), outfile, append=TRUE)
                    }
                    exit<-TRUE
                    f2<-TRUE
                  }
                }
                if(exit==TRUE){
                  break
                }
              }
              h2<-length(check)
              exit<-TRUE
              if((h1==h2)|(exit==TRUE)){
                break
              }
            }
          }
          if(f2==FALSE){
            write(paste(zfa_old, getTermNameById(ontol,as.character(zfa_old)),'is not found', sep=' '), outfile, append=TRUE)
          }
        }
        print(paste('i=',i,sep=''))
      }
    }
  }
  write.table(out_table,paste(place,'outuser.tsv', sep=''))
}

if(zfs_zfa=='both'){
  fake<-rbind(out_table,out_table_zfs)
}else if(zfs_zfa=='zfs'){
  fake<-out_table_zfs
}else if(zfs_zfa=='zfa'){
  fake<-out_table
}

min_val=max(fake[,1:ncol(fake)])
for(i in 1:nrow(fake)){
  for(j in 1:ncol(fake)){
    if((min_val>fake[i,j]) & (fake[i,j]!=0)){
      min_val=fake[i,j]
    }
  }
}
max_val=max(out_table[,1:ncol(fake)])
  
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

proc.time() - ptm

