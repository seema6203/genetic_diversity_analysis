# install.packages("ggplot2")
# install.packages("readxl")
library(ggplot2)
library(sf)
library(readxl)
gms<-0.09
theme_riz<-theme(panel.background = element_rect(fill = "white", colour = NA), panel.border = element_rect(fill = NA,colour = "grey20",size = gms), panel.grid = element_line(size=gms,colour = "grey92"),panel.grid.minor = element_line(size=gms),strip.background = element_rect(fill = "grey85",colour = "grey20"), legend.key = element_rect(fill = "white",colour = NA), complete = TRUE,axis.ticks=element_line(size=0.05),axis.ticks.length = unit(0.05,"cm"),axis.text = element_text(size=2))
states<-st_read("C:\\Users\\RIZWAN\\Documents\\map\\India_States_ADM1_GADM-shp\\3e563fd0-8ea1-43eb-8db7-3cf3e23174512020330-1-layr7d.ivha.shp")
assam<-subset(states, grepl("Assam", states$NAME_1))
megh<-subset(states, grepl("Meghalaya", states$NAME_1))
assam_megh<-rbind(assam,megh)
ass_meg_nau_points <- read_excel("C:\\Users\\RIZWAN\\Documents\\map\\map_ssr1.xlsx", sheet = "nauchali_assam_meglaya",col_types=c("text","text","text","text"),col_names = c("long","lat","A","B"))
ass_meg_nau_points<-ass_meg_nau_points[1:2]
ass_meg_nau_points <- st_as_sf(ass_meg_nau_points, coords = c("long", "lat"),crs = 4326, agr = "constant")
ass_meg_mic_points <- read_excel("C:\\Users\\RIZWAN\\Documents\\map\\map_ssr1.xlsx", sheet = "micrantha_assam_meghalaya",col_types=c("text","text","text","text"),col_names = c("long","lat","A","B"))
ass_meg_mic_points <- read_excel("C:\\Users\\RIZWAN\\Documents\\map\\map_ssr1.xlsx", sheet = "micrantha_assam_meghalaya",col_types=c("text","text","text","text"),col_names = c("long","lat","A","B"))
ass_meg_mic_points<-ass_meg_mic_points[1:2]
ass_meg_mic_points <- st_as_sf(ass_meg_mic_points, coords = c("long", "lat"),crs = 4326, agr = "constant")
assam_megh_plot<-ggplot(data=assam_megh)+geom_sf(fill="cornsilk")+geom_sf(data = ass_meg_nau_points,  size = 1, shape = 21, fill = "green")+annotate(geom = "text", fontface="bold",x = 91.5, y = 27.8, label = "Assam & Meghalaya",  color = "black", size =2)+geom_sf(data = ass_meg_mic_points,  size = 1, shape = 21, fill = "red")+geom_rect(xmin=90.05,xmax=91.8,ymin=25.05,ymax=26.6,fill=NA,colour="black",size=0.5) + theme_riz
districts<-st_read("C:\\Users\\RIZWAN\\Documents\\map\\India_Districts_ADM2_GADM-shp\\India_Districts_ADM2_GADM.shp")
assam1<-subset(districts, grepl("Assam", districts$NAME_1))
megh1<-subset(districts, grepl("Meghalaya", districts$NAME_1))
assam_megh1<-rbind(assam1,megh1)
assam_megh_plot1<-ggplot(data=assam_megh1)+geom_sf(fill="cornsilk")+geom_sf(data = ass_meg_nau_points,  size = 1, shape = 21, fill = "green")+geom_sf(data = ass_meg_mic_points,  size = 1, shape = 21, fill = "red")+theme_riz+coord_sf(xlim = c(90.05, 91.8), ylim = c(25.05, 26.6), expand = FALSE)
goa_nau_points <- read_excel("C:\\Users\\RIZWAN\\Documents\\map\\map_ssr1.xlsx", sheet = "goa nauchali",col_types=c("text","text","text","text"),col_names = c("long","lat","A","B"))
goa_nau_points<-goa_nau_points[1:2]
goa_nau_points <- st_as_sf(goa_nau_points, coords = c("long", "lat"),crs = 4326, agr = "constant")
india<-ggplot(data=states)+geom_sf(data=states,fill=NA,size=gms)+geom_sf(data=assam,fill="green",size=gms)+geom_sf(data=megh,fill="pink",size=gms)+geom_sf(data=goa,fill="red",size=gms)+geom_sf(data=mha,fill="yellow",size=gms)+geom_sf(data=kerala,fill="skyblue",size=gms)+geom_sf(data=manipur,size=gms,fill="cornsilk")+geom_sf(data=ass_meg_mic_points,size=t,shape=19)+geom_sf(data=ass_meg_mic_points,size=t,shape=19)+geom_sf(data=kerala_nau_points,size=t,shape=19)+geom_sf(data=manipur_mic_points,size=t,shape=19)+geom_sf(data=goa_nau_points,size=t,shape=19)+theme_void()
a<-ggplot() + coord_equal(xlim = c(0, 30), ylim = c(0, 30), expand = FALSE)  +annotation_custom(ggplotGrob(india), xmin = 11.5, xmax = 21.5, ymin = 10, ymax = 20)+annotation_custom(ggplotGrob(assam_meghalaya_plot), xmin = 0, xmax = 14, ymin = 18, ymax = 30) + annotation_custom(ggplotGrob(manipur_plot), xmin = 14, xmax = 35, ymin = 18,  ymax = 30) + annotation_custom(ggplotGrob(goa_plot), xmin = 0, xmax = 16, ymin = 0, ymax = 13)+annotation_custom(ggplotGrob(kerala_plot), xmin = 14, xmax = 35, ymin = 0, ymax = 13) + geom_blank(data=c("a","b","c"),aes(color=c("red","blue","yellow"))) +theme_void()
assam_meghalaya_plot<-india1+coord_sf(xlim = c(90.05, 91.89), ylim = c(25.03, 26.69), expand = FALSE)
manipur_plot<-india1+coord_sf(xlim = c(93.75, 94), ylim = c(24.6, 24.9), expand = FALSE)
assam_meghalaya_plot<-india1+coord_sf(xlim = c(90.05, 91.89), ylim = c(25.03, 26.69), expand = FALSE)
india1<-ggplot(data=districts)+geom_sf(data=districts,size=gms,fill=NA)+geom_sf(data=assam1,fill="green",size=gms)+geom_sf(data=megh1,fill="pink",size=gms)+geom_sf(data=goa1,fill="red",size=gms)+geom_sf(data=mha1,fill="yellow",size=gms)+geom_sf(data=kerala1,fill="skyblue",size=gms)+geom_sf(data=manipur1,fill="cornsilk",size=gms)+geom_sf(data=ass_meg_nau_points,size=gps,shape=18)+geom_sf(data=ass_meg_mic_points,size=0.5,shape=17)+geom_sf(data=kerala_nau_points,size=gps,shape=18)+geom_sf(data=manipur_mic_points,size=0.5,shape=17)+geom_sf(data=goa_nau_points,size=gps,shape=18)+theme_riz
india1<-ggplot(data=districts)+geom_sf(data=districts,size=gms,fill=NA)+geom_sf(data=assam1,fill="green",size=gms)+geom_sf(data=megh1,fill="pink",size=gms)+geom_sf(data=goa1,fill="red",size=gms)+geom_sf(data=mha1,fill="yellow",size=gms)+geom_sf(data=kerala1,fill="skyblue",size=gms)+geom_sf(data=manipur1,fill="cornsilk",size=gms)+geom_sf(data=ass_meg_nau_points,size=gps,shape=18)+geom_sf(data=ass_meg_mic_points,size=0.5,shape=17)+geom_sf(data=kerala_nau_points,size=gps,shape=18)+geom_sf(data=manipur_mic_points,size=0.5,shape=17)+geom_sf(data=goa_nau_points,size=gps,shape=18)+theme_riz
kerala_plot<-india1+coord_sf(xlim = c(75.8, 76.08), ylim = c(10.84, 11.27), expand = FALSE)
a<-ggplot(data=states) + coord_equal(xlim = c(0, 30), ylim = c(0, 30), expand = FALSE)  +annotation_custom(ggplotGrob(india), xmin = 11.5, xmax = 25, ymin = 3, ymax = 25)+annotation_custom(ggplotGrob(assam_meghalaya_plot), xmin = 0, xmax = 14, ymin = 18, ymax = 30) + annotation_custom(ggplotGrob(manipur_plot), xmin = 14, xmax = 40, ymin = 18,  ymax = 30) + annotation_custom(ggplotGrob(goa_plot), xmin = -2, xmax = 16, ymin = 0, ymax = 13)+annotation_custom(ggplotGrob(kerala_plot), xmin = 14, xmax = 40, ymin = 0, ymax = 13) +theme_void()+draw_label("Goa",x=16.5,y=2.5,size = 4,hjust=0,vjust=0)+draw_label("Maharashtra",x=16.5,y=3.1,size = 4,hjust=0,vjust=0 )+draw_label("Kerala",x=16.5,y=3.7,size = 4,hjust=0,vjust=0)+draw_label("Meghalaya",x=16.5,y=4.3,size = 4,hjust=0,vjust=0)+draw_label("Assam",x=16.5,y=4.9,size =4,hjust=0,vjust=0)+draw_label("Manipur",x=16.5,y=5.5,size =4,hjust=0,vjust=0)+geom_rect(xmin=15.5,xmax=16,ymin=5.5,ymax=6,fill="cornsilk")+geom_rect(xmin=15.5,xmax=16,ymin=4.9,ymax=5.4,fill="green")+geom_rect(xmin=15.5,xmax=16,ymin=4.3,ymax=4.8,fill="pink")+geom_rect(xmin=15.5,xmax=16,ymin=3.7,ymax=4.2,fill="skyblue")+geom_rect(xmin=15.5,xmax=16,ymin=3.1,ymax=3.6,fill="yellow")+geom_rect(xmin=15.5,xmax=16,ymin=2.5,ymax=3,fill="orange")
india1<-ggplot(data=districts)+geom_sf(data=districts,size=gms,fill=NA)+geom_sf(data=assam1,fill="green",size=gms)+geom_sf(data=megh1,fill="pink",size=gms)+geom_sf(data=goa1,fill="orange",size=gms)+geom_sf(data=mha1,fill="yellow",size=gms)+geom_sf(data=kerala1,fill="palegreen",size=gms)+geom_sf(data=manipur1,fill="cornsilk",size=gms)+geom_sf(data=ass_meg_nau_points,size=gps,shape=18)+geom_sf(data=ass_meg_mic_points,size=0.5,shape=17)+geom_sf(data=kerala_nau_points,size=gps,shape=18)+geom_sf(data=manipur_mic_points,size=0.5,shape=17)+geom_sf(data=goa_nau_points,size=gps,shape=18)+theme_riz
data<- read_excel("C:\\Users\\RIZWAN\\Documents\\map\\map_population.xlsx", sheet = "a",col_names = c("long","lat","Cluster1","Cluster2","Cluster3"))

india<-ggplot(data=states)+geom_sf(data=states,fill=NA,size=gms)+theme_void()
plot(india)
india<-ggplot()+geom_scatterpie(aes(x=lat,y=long),data=data,pie_scale = 4,cols=c("Cluster1","Cluster2","Cluster3"))  +coord_sf(crs=st_crs(3857))
pdf("aa.pdf",height = 20,width = 20)
plot(india)
dev.off()
getwd()