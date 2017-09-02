#Comparison of two protein expression values on one map

rm(list=ls())
file.remove("Rplots.pdf")
setwd("./")
#setwd("D:/YAZ_project/3_2016_Summer/workspace")
getwd()

library(grid)
library(maps)
library(maptools)
library(sp)
library(lattice)
library(latticeExtra)
library(colorspace)
library(gridSVG)
library(pxR)
library(classInt)
library(ggplot2)
library(stringr)

#==========================================Getting parameters for the script==============================================================

args <-commandArgs(TRUE)
place <- toString(args[1])
gene_ID_1 <- toString(args[2])      
gene_ID_2 <- toString(args[3])
color1 <- paste("#", toString(args[4]), sep="")
color2 <- paste("#", toString(args[5]), sep="")
nbins <- as.integer(args[6])
session <- toString(args[7]) 
adv_br <- toString(args[8]) 

config <- readLines(paste(place,'/data.conf',sep=''))
dataset_name <- config[1]
species <- config[2]
IDtype <- config[3]
brain_sort <- config[4]
gender <- config[5]
min_val <- as.numeric(config[6])
max_val <- as.numeric(config[7])

if(nbins %% 2 == 0){
  nbins=nbins+1
}
#============================================Import of data for visualisation==============================================================
if(adv_br=='yes'){
  brain_sort<-'smallbrain' 
}

if(species=='ZF'){
  if(adv_br=='yes'){
    ref_table=read.table('zebrafish/ref_tab_zf_2_adv.tsv', sep='\t', header=TRUE, na.strings=c(' '))
  }else{
    ref_table=read.table('zebrafish/ref_tab_zf_2.tsv', sep='\t', header=TRUE, na.strings=c(' ')) 
  }
  map <-  readShapePoly(fn = './zebrafish/ok2.shp')
  
  if(brain_sort=='smallbrain'){
    if(adv_br=='no'){
      for(i in (length(map)):1){
        if((map@data[i,1]>1100)&(map@data[i,1]<1200)){
          map <- map[-(i),]
        }
      }
    }
    for(i in (length(map)):1){
      if((map@data[i,1]>100)&(map@data[i,1]<200)){
        map <- map[-(i),]
      }
    }
  }else{
    for(i in (length(map)):1){
      if((map@data[i,1]>1100)&(map@data[i,1]<1200)){
        map <- map[-(i),]
      }
    }
  }
  
  if(gender=='male'){
    for(i in (length(map)):1){
      if(map@data[i,1]==30){
        map <- map[-(i),]
      }
    }
  }else if(gender=='female'){
    for(i in (length(map)):1){
      if(map@data[i,1]==29){
        map <- map[-(i),]
      }
    }
  }else if(gender=='none'){
    for(i in (length(map)):1){
      if((map@data[i,1]==29)|(map@data[i,1]==30)){
        map <- map[-(i),]
      }
    }
  }
  
  
}else if(species=='carp'){
  if(adv_br=='yes'){
    ref_table=read.table('./carp/ref_tab_carp_adv.tsv', sep='\t', header=TRUE, na.strings=c(' '))
  }else{
    ref_table=read.table('./carp/ref_tab_carp.tsv', sep='\t', header=TRUE, na.strings=c(' ')) 
  }
  map <-  readShapePoly(fn = './carp/carp2.shp')    #import of the shapefile
  
  if(brain_sort=='smallbrain'){
    if(adv_br=='no'){
      for(i in (length(map)):1){
        if((map@data[i,1]>1100)&(map@data[i,1]<1200)){
          map <- map[-(i),]
        }
      }
    }
    for(i in (length(map)):1){
      if((map@data[i,1]>1500)&(map@data[i,1]<1600)){
        map <- map[-(i),]
      }
    }
  }else{
    for(i in (length(map)):1){
      if((map@data[i,1]>1100)&(map@data[i,1]<1200)){
        map <- map[-(i),]
      }
    }
  }
  
  if(gender=='male'){
    for(i in (length(map)):1){
      if(map@data[i,1]==30){
        map <- map[-(i),]
      }
    }
  }else if(gender=='female'){
    for(i in (length(map)):1){
      if(map@data[i,1]==29){
        map <- map[-(i),]
      }
    }
  }else if(gender=='none'){
    for(i in (length(map)):1){
      if((map@data[i,1]==29)|(map@data[i,1]==30)){
        map <- map[-(i),]
      }
    }
  }
}
in_table=read.table(paste(place,'/outuser.tsv',sep=''), header=TRUE, row.names=1)

#============================================Preparation part==============================================================================

for(i in 1:ncol(in_table)){                   #search for the gene1 in in_table
  if(toString(gene_ID_1)==names(in_table)[i]){
    h1<-i
  }
  i=i+1
}

for(i in 1:ncol(in_table)){                   #search for the gene2 in in_table
  if(toString(gene_ID_2)==names(in_table)[i]){
    h2<-i
  }
  i=i+1
}

out_table=cbind(ref_table, as.data.frame(matrix(0,nrow=nrow(ref_table),ncol=4))) #create a new table for output
names(out_table)[4:7]=c('Exp1','Exp2','ratio','log10rat')

for(i in 1:nrow(out_table)){              #fill the Express_Level column of out_table
  for(j in 1:nrow(in_table)){
    if(toString(out_table[i,1])==row.names(in_table)[j]){
      if(in_table[j,h1]==0){
        out_table[i,4]<-min_val/10            
      }else{
        out_table[i,4]<-in_table[j,h1]
      }
      if(in_table[j,h2]==0){
        out_table[i,5]<-min_val/10            
      }else{
        out_table[i,5]<-in_table[j,h2]
      }
    }
  }
}

for(i in 1:nrow(out_table)){
	if(((out_table[i,4]==min_val/10)&(out_table[i,5]==min_val/10))|(is.na(out_table[i,4])&is.na(out_table[i,5]))){
    		out_table[i,4]=NA
		out_table[i,5]=NA
  	}
      	out_table[i,6]<-out_table[i,4]/out_table[i,5]
      	out_table[i,7]<-log10(out_table[i,6])
}

#==============================================Visualisation part==========================================================================

data <- out_table                             #import prepared table for one gene chosen
idx1=match(map$id,data$index)
expre=data[idx1, "log10rat"]
name=data[idx1,"ZF.name"]
CV=data[idx1,"ontology"]
map@data=cbind(map@data,expre,name,CV)
map1=as.data.frame(map)
mapaID <- as.numeric(rownames(map1))
names(map1)[2]<-'nazvan'

panel.str <- deparse(panel.polygonsplot, width=500)               
panel.str <- sub("grid.polygon\\((.*)\\)","grid.polygon(\\1, name=paste('ID', slot(pls\\[\\[i\\]\\], 'ID'\\), sep=':'))",panel.str)
panel.polygonNames <- eval(parse(text=panel.str),envir=environment(panel.polygonsplot))
set_Polypath(FALSE)

j=1
exp1=c()

for(i in 1:length(expre)){
  if(is.na(expre[i])!=TRUE){
    exp1[j]<-expre[i]
    j=j+1
  }
  i=i+1
}

int_min <- min(exp1)
int_max <- max(exp1)
exp1[(length(exp1)+1):(length(exp1)+2)] <- c(int_min*(-1),int_max*(-1))
int_min <- min(exp1)
int_max <- max(exp1)

exp1<- c()
y=int_max*(1.05)-int_min*(1.05)
for(i in 0:nbins){
  exp1<-c(exp1,int_min*(1.05)+(i*y/nbins))
}

int1 <- classIntervals(exp1, nbins, style='equal')
my_palette <- colorRampPalette(c(color1,'#FFFBFF',color2))(n = nbins)

if(species=='ZF'){
  if(adv_br=='yes'){
    dev.new(width=10.40, height=5.25)
  }else{
    if(brain_sort=='smallbrain'){
      dev.new(width=10.40, height=4.25)
    }else{
      dev.new(width=10.40, height=4.79)
    }
  }
}else if(species=='carp'){
  if(adv_br=='yes'){
    dev.new(width=10.40, height=6.50)
  }else{
    if(brain_sort=='smallbrain'){
      dev.new(width=10.58, height=5.50)
    }else{
      dev.new(width=10.58, height=5.84)
    }
  }
}

p <- spplot(map['expre'],
            panel=panel.polygonNames,
            col.regions=my_palette,
            at=signif(int1$brks, digits=2),
            par.settings=list(axis.line = list(col = "transparent")),
            main=paste('log10 ratio: ',gene_ID_1, ' / ',gene_ID_2, sep=' '))
p

grobsf <- grid.ls()
nmsf <- grobsf$name[grobsf$type == "grobListing"]
idxNamesf <- grep('ID:', nmsf)
IDsf <- nmsf[idxNamesf]
for (id in unique(IDsf)){
  i <- strsplit(id, 'ID:')
  i <- sapply(i, function(x)as.numeric(x[2]))
  dat <- map1[which(mapaID==i),]
  info <- paste(dat$name,  dat$CV, round(as.numeric(dat$expre),3), sep=' : ')
  g <- grid.get(id)
  grid.garnish(id,onmouseover=paste("showTooltip(evt, '", info, "')"),onmouseout="hideTooltip()")
}


grid.script(filename="../../bin/tooltip.js")
grid.script(filename="tooltip.js")
grid.export(paste(session, gene_ID_1, '_', gene_ID_2, ".svg", sep=""))
