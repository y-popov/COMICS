#Creates a single picture for the given gene-ID, for the list of them is run several times
#Author: D.Travin 2016/08/12

rm(list=ls())
file.remove("Rplots.pdf")
setwd("./")
#setwd("D:/YAZ_project/3_2016_Summer/workspace")

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
gene_ID <- toString(args[2])       #carp or ZF
color1 <- paste("#", toString(args[3]), sep="")
color2 <- paste("#", toString(args[4]), sep="")
color3 <- paste("#", toString(args[5]), sep="")
nbins <- as.integer(args[6])
grad <- toString(args[7])       #ident or unique
scale <- toString(args[8])      #LOG\LOG2 or LINE
session <- toString(args[9]) 
adv_br <- toString(args[10])    #advanced brain: yes or no

#place <- './dataset_try/'

config <- readLines(paste(place,'/data.conf',sep=''))
dataset_name <- config[1]
species <- config[2]
IDtype <- config[3]
brain_sort <- config[4]
gender <- config[5]

#gene_ID <- 'A7YYE3'
# gene_ID <- 'cypCar_00013291.RA'
#color1='green'
#color2='yellow'
#color3='orange'
#nbins=100
#grad='unique'
#scale <- 'LINE'

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

in_table=read.table(paste(place,'/outuser.tsv',sep=''), header=TRUE, row.names=1, check.names=FALSE)

#============================================Preparation part==============================================================================
for(i in 1:ncol(in_table)){                   #search for the gene in in_table
  if(toString(gene_ID)==names(in_table)[i]){
    h<-i
  }
  i=i+1
}

if(grad=='ident'){
  min_val <- as.numeric(config[6])
  max_val <- as.numeric(config[7])
}

if(grad=='unique'){
  min_val=max(in_table[,1:ncol(in_table)])    
  for(i in 1:nrow(in_table)){
    if((min_val>in_table[i,h]) & (in_table[i,h]!=0)){
      min_val=in_table[i,h]
    }
    i=i+1
  }
  max_val=max(in_table[1:nrow(in_table),h])
}

out_table=cbind(ref_table, as.data.frame(matrix(0,nrow=nrow(ref_table),ncol=1))) #create a new table for output
names(out_table)[4]='Expres_Level'

for(i in 1:nrow(out_table)){              #fill the Express_Level column of out_table
  for(j in 1:nrow(in_table)){
    if(toString(out_table[i,1])==row.names(in_table)[j]){
      if(in_table[j,h]==0){
        out_table[i,4]=NA            
      }else{
        out_table[i,4]<-in_table[j,h]
      }
    }
  }
}
  
if(scale=='LOG'){                           #substitute value with its log10 if scale is LOG, log2 if scale is LOG@
  for(i in nrow(out_table):1){
    out_table[i,4]=log10(out_table[i,4])
  }
}else if(scale=='LOG2'){
  for(i in nrow(out_table):1){
    out_table[i,4]=log2(out_table[i,4])
  }
}

#==============================================Visualisation part==========================================================================

data <- out_table                             #import prepared table for one gene chosen
idx1=match(map$id,data$index)
expre=data[idx1, "Expres_Level"]
name=data[idx1,"ZF.name"]
CV=data[idx1,"ontology"]
map@data=cbind(map@data,expre,name,CV)
map1=as.data.frame(map)
mapaID <- as.numeric(rownames(map1))
names(map1)[2]<-'nazvan'

panel.str <- deparse(panel.polygonsplot, width=500)               #making up a new function that works with polygon names
panel.str <- sub("grid.polygon\\((.*)\\)","grid.polygon(\\1, name=paste('ID', slot(pls\\[\\[i\\]\\], 'ID'\\), sep=':'))",panel.str)
panel.polygonNames <- eval(parse(text=panel.str),envir=environment(panel.polygonsplot))
set_Polypath(FALSE)

j=1
exp1=c()

for(i in 1:length(expre)){
  if(is.na(expre[i])!=TRUE){
    exp1[j]<-expre[i]
    j<-j+1
  }
}

if (scale=='LOG'){
  exp1=c((log10(min_val)-abs(log10(min_val)*0.05)),(log10(max_val)+abs(log10(max_val)*0.05)))
  y=exp1[1]-exp1[2]
  for(i in 1:nbins){
    exp1=c(exp1,(log10(min_val)-abs(log10(min_val)*0.05))+(i*y/nbins))
  }
}else if(scale=='LOG2'){
  exp1=c((log2(min_val)-abs(log2(min_val)*0.05)),(log2(max_val)+abs(log2(max_val)*0.05)))
  y=exp1[1]-exp1[2]
  for(i in 1:nbins){
    exp1=c(exp1,(log2(min_val)-abs(log2(min_val)*0.05))+(i*y/nbins))
  }
}else if(scale=='LINE'){
	exp1=c((min_val-min_val*0.05),(max_val+max_val*0.05))
	y=exp1[1]-exp1[2]
	for(i in 1:nbins){
		exp1=c(exp1,(min_val-min_val*0.05)+(i*y/nbins))
	}
}

int1 <- classIntervals(exp1, nbins, style='equal')

if(color2=='#NONE'){
	my_palette <- colorRampPalette(c(color1,color3))(n = nbins)
}else{
	print(color2)
	my_palette <- colorRampPalette(c(color1,color2,color3))(n = nbins)
}

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
            main=paste('ID:',gene_ID, sep=' '))
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
  grid.garnish(id,onmouseover=paste("showTooltip(evt, '", info, "')"),onmouseout="hideTooltip()", onclick=paste("window.open('https://www.ebi.ac.uk/ols/ontologies/zfa/terms?iri=http://purl.obolibrary.org/obo/",dat$CV,"')",sep=""))
}

grid.script(filename="../../bin/tooltip.js")
grid.script(filename="tooltip.js")
grid.export(paste(session, gene_ID, ".svg", sep=""))
