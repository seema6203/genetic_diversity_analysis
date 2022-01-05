file<-"nj.csv"
working_dir<-"/Users/seema/Documents/nj"

nj_tree<-function(file,working_dir)
{
library(ape)
setwd(working_dir)
library(poppr)
nou<-read.genalex(file,genclone = FALSE)
nou
D<-dist(nou)
tre<-nj(D)
pop<-pop(nou)
colr<-c(rep("purple",2),rep("red",39),rep("purple",22),rep("orange",29))
colr[6]<-"purple"
colr[21]<-"purple"
indNames(nou)
myBoots <- boot.phylo(tre, as.data.frame(nou), function(e) root(nj(dist(e)),1),B=1000)
myBoots<-myBoots/10
myBoots[myBoots<70]<-NA

myBoots<-as.integer(myBoots)
pdf("nj.pdf")
plot(tre,type="unrooted", cex=0.5,tip.color=colr, edge.width=1,label.offset=0.05,show.tip.label=TRUE)
nodelabels(myBoots,frame="c",cex=0.4,bg="black",col="white")
tiplabels(col=colr,frame="circle",pch=16,cex=0.7)
color<-c("red","purple","orange")
leg.txt<-c("Cluster 1","Cluster 2","Cluster 3")
legend("topleft",leg.txt,fill=color,cex=0.8,bty="n",border=color)
add.scale.bar("bottomleft",cex = 0.7, font = 2, col = "black")
dev.off()
}

