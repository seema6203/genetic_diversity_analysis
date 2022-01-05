
working_dir<-"/Users/Seema/Documents/pcoa/"
file<-"pcoa.csv"
setwd(working_dir)
library(ggplot2)
library(poppr)
nou<-read.genalex(file,genclone = FALSE)
D<-dist(nou)
pop(nou)
pco<-dudi.pco(D,scannf = FALSE,nf=3)
population<-c(rep("Cluster2",2),rep("Cluster1",39),rep("Cluster2",22),rep("Cluster3",29))
p<-ggFunctions::s.class(pco$li,fac=population,xax=1,yax=2,drawEllipse=FALSE,drawSegment = FALSE,cellipse = 0)
p
p<-p+theme(
   legend.background = element_rect(fill="transparent"), # get rid of legend bg
         legend.box.background = element_rect(fill = "transparent",color=NA),
           panel.grid.major = element_line(color="grey",size = 0.1),
          panel.grid.minor = element_blank(),
           plot.background   = element_rect(fill = "white",color=NA),
           panel.background   = element_rect(fill="transparent",color="black",size=0.6)
         ,legend.position="none" )
       
p<-p + labs(x="PCo1 (18.74%)", y="PCo2 (7.98%)")
q<-ggplot_build(p)
q$data[[3]]$colour<-c(rep("purple",2),rep("red",39),rep("purple",22),rep("orange",29))
q$data[[3]]$size<-c(rep(2,92))
colours <- unique(q$data[[3]][, "colour"])
colours
p<-p+scale_color_manual(values = colours)
q<-ggplot_build(p)
q$data[[2]]$size<-c(rep(0.5,92))
q$data[[1]]$size<-c(rep(0.5,92))



q$data[[4]]$vjust<-c(rep(1.2,92))
q$data[[4]]$hjust<-c(rep(1.2,92))
q$data[[4]]$label<-indNames(nou)
q$data[[4]]$size<-c(rep(1.9,92))

s<-ggplot_gtable(q)
# s+scale_color_manual(values = my_colors)
pdf("nei_name.pdf")
plot(s)
dev.off()

