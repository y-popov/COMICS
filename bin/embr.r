rm(list=ls())
file.remove("Rplots.pdf")
# setwd("D:/YAZ_project/3_2016_Summer/workspace")
setwd('./')
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
gene_ID <- toString(args[2])
color1 <- paste("#", toString(args[3]), sep="")
color2 <- paste("#", toString(args[4]), sep="")
color3 <- paste("#", toString(args[5]), sep="")
nbins <- as.integer(args[6])
grad <- toString(args[7])       #ident or unique
scale <- toString(args[8])      #LOG or LINE
session <- toString(args[9])
label_em <- toString(args[10]) 

#place <- './dataset_try/'
config <- readLines(paste(place,'/data.conf',sep=''))

dataset_name <- config[1]
min_val <- as.numeric(config[6])
max_val <- as.numeric(config[7])
zfs_zfa <- config[8]
#============================================Import of data for visualisation==============================================================

ref_table <- read.table('zebrafish/ref_tab_zfs.tsv',sep='\t', header=TRUE,na.strings=c(' '))
map <-  readShapePoly(fn = './zebrafish/enbr.shp')
in_table=read.table(paste(place,'/outuser_zfs.tsv',sep=''), header=TRUE, row.names=1, check.names = FALSE)

#============================================Preparation part==============================================================================

for(i in 1:ncol(in_table)){                   #search for the gene in in_table
  if(toString(gene_ID)==names(in_table)[i]){
    h<-i
  }
  i=i+1
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

if(scale=='LOG'){
  for(i in 1:nrow(out_table)){              #fill the Express_Level column of out_table
    for(j in 1:nrow(in_table)){
      if(toString(out_table[i,1])==row.names(in_table)[j]){
        if(in_table[j,h]==0){
          out_table[i,4]=NA            #NEW!  HERE I changed NA to min_val_eff
        }else{
          out_table[i,4]<-in_table[j,h]
        }
      }
      j=j+1
    }
    i=i+1
  }
}

if(scale=='LINE'){
  for(i in 1:nrow(out_table)){              #fill the Express_Level column of out_table
    for(j in 1:nrow(in_table)){
      if(toString(out_table[i,1])==row.names(in_table)[j]){
        if(in_table[j,h]==0){
          out_table[i,4]=NA            
        }else{
          out_table[i,4]<-in_table[j,h]
        }
      }
      j=j+1
    }
    i=i+1
  }
}

if(scale=='LOG'){                           #substitute value with its log2 if scale is log
  for(i in nrow(out_table):1){
    out_table[i,4]=log10(out_table[i,4])
    i=i+1
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
    j=j+1
  }
  i=i+1
}

if (scale=='LOG'){
  exp1=c((log10(min_val)-abs(log10(min_val)*0.05)),(log10(max_val)+abs(log10(max_val)*0.05)))
  y=exp1[1]-exp1[2]
  #y=(-1*log10(min_val)+(-1*log10(min_val)*0.05))+(log10(max_val)+(log10(max_val)*0.05))
  for(i in 1:nbins){
    exp1=c(exp1,(log10(min_val)+(log10(min_val)*0.05))+(i*y/nbins))
  }
}

if (scale=='LINE'){
  exp1=c((min_val-min_val*0.05),(max_val+max_val*0.05))
  y=(-1*min_val+(-1)*min_val*0.05)+(max_val+max_val*0.05)
  for(i in 1:nbins){
    exp1=c(exp1,(min_val+min_val*0.05)+(i*y/nbins))
  }
}

int1 <- classIntervals(exp1, nbins, style='equal')

if(color2=='#NONE'){
  my_palette <- colorRampPalette(c(color1,color3))(n = nbins)
}else{
  print(color2)
  my_palette <- colorRampPalette(c(color1,color2,color3))(n = nbins)
}

dev.new(width=8.40, height=11.18)
dev.size()

p <- spplot(map['expre'],
            panel=panel.polygonNames,
            col.regions=my_palette,
            at=signif(int1$brks, digits=2),
            par.settings=list(axis.line = list(col = "transparent")),
            main=paste('ID:',gene_ID, sep=' '))
p

if(label_em=='yes'){
  hpf <- read.table('zebrafish/hpf.tsv', header=TRUE, na.strings=c(' '), check.names = FALSE)
  
  for(i in 1:nrow(hpf)){
    grid.text(hpf[i,1], x=unit(hpf[i,3], "npc"), y=unit(hpf[i,4], "npc"), rot=0, gp = gpar(fontsize = 12, fontface = "bold"))
  }
}

# if(scale=='LOG'){
#   grid.text('Log10 (Expression level)', x=unit(0.92, "npc"), y=unit(0.5, "npc"), rot=90, gp = gpar(fontsize = 12, fontface = "bold"))
# }
# if(scale=='LINE'){
#   grid.text('Expression level', x=unit(0.92, "npc"), y=unit(0.5, "npc"), rot=90, gp = gpar(fontsize = 12, fontface = "bold"))
# }

grobsf <- grid.ls()
nmsf <- grobsf$name[grobsf$type == "grobListing"]
idxNamesf <- grep('ID:', nmsf)
IDsf <- nmsf[idxNamesf]
for (id in unique(IDsf)){
  i <- strsplit(id, 'ID:')
  i <- sapply(i, function(x)as.numeric(x[2]))
  dat <- map1[which(mapaID==i),]
  if(scale=='LOG'){
    info <- paste(dat$name,  dat$CV, round(10^(as.numeric(dat$expre)),3), sep=' : ')
  }else{
    info <- paste(dat$name,  dat$CV, round(as.numeric(dat$expre),3), sep=' : ')
  }
  g <- grid.get(id)
  grid.garnish(id,onmouseover=paste("showTooltip(evt, '", info, "')"),onmouseout="hideTooltip()")
}

grid.script(filename="tooltip.js")
grid.script(filename="../../bin/tooltip.js")
grid.export(paste(session, gene_ID, "_embr.svg", sep=""))
