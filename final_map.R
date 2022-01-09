library(rgdal)
library(maps)
library(memisc)
library(tidyr)
library(assertthat)
library(sqldf)
library(magrittr)
library(dplyr)
library(reshape2)
library(ggfun)
library(ggplot2)
library(oz)
library(scatterpie)
library(rgdal)
library(maptools)
library(readxl)
library(grid)
# install.packages("rlang")

geom_map <- function(mapping = NULL, data = NULL,
                     stat = "identity",
                     ...,
                     map,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE) {
  # Get map input into correct form
  if (!is.data.frame(map)) {
    abort("`map` must be a data.frame")
  }
  if (!is.null(map$Latitude)) map$y <- map$Latitude
  if (!is.null(map$Longitude)) map$x <- map$Longitude
  if (!is.null(map$region)) map$id <- map$region
  print(names(map))
  if (!all(c("x", "y", "id") %in% names(map))) {
    abort("`map` must have the columns `x`, `y`, and `id`")
  }
  
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomMap,
    position = PositionIdentity,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      map = map,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomMap <- ggproto("GeomMap", GeomPolygon,
                   draw_panel = function(data, panel_params, coord, lineend = "butt",
                                         linejoin = "round", linemitre = 10, map) {
                     # Only use matching data and map ids
                     common <- intersect(data$map_id, map$id)
                     data <- data[data$map_id %in% common, , drop = FALSE]
                     map <- map[map$id %in% common, , drop = FALSE]
                     
                     # Munch, then set up id variable for polygonGrob -
                     # must be sequential integers
                     coords <- coord_munch(coord, map, panel_params)
                     coords$group <- coords$group %||% coords$id
                     grob_id <- match(coords$group, unique(coords$group))
                     
                     # Align data with map
                     data_rows <- match(coords$id[!duplicated(grob_id)], data$map_id)
                     data <- data[data_rows, , drop = FALSE]
                     
                     polygonGrob(coords$x, coords$y, default.units = "native", id = grob_id,
                                 gp = gpar(
                                   col = data$colour,
                                   fill = alpha(data$fill, data$alpha),
                                   lwd = data$size * .pt,
                                   lineend = lineend,
                                   linejoin = linejoin,
                                   linemitre = linemitre
                                 )
                     )
                   },
                   
                   required_aes = c("map_id")
)



geom_scatterpie_legend <- function(radius, x, y, n=5, labeller) {
  ## rvar <- as.character(mapping)["r"]
  ## if (is_fixed_radius(rvar)) {
  ##     radius <- as.numeric(rvar)
  ## } else {
  ##     rr <- range(data[, rvar])
  ##     radius <- sapply(seq(min(rr), max(rr), length.out=5), roundDigit)
  ## }
  print("one")
  print(length(radius))
#  if (length(radius) > n) {
 #   radius <- unique(sapply(seq(min(radius), max(radius), length.out=n), round_digit))
#  }
  
  label <- FALSE
  if (!missing(labeller)) {
    if (!inherits(labeller, "function")) {
      stop("labeller should be a function for converting radius")
    }
    label <- TRUE
  }
 
  dd <- data.frame(r=radius, start=0, end=2*pi, x=x, y=y + radius - max(radius), maxr=max(radius))
  
  if(label) {
    dd$label <- labeller(dd$r)
  } else {
    dd$label <- dd$r
  }
  print(dd$label)
  list(
    geom_arc_bar(aes_(x0=~x, y0=~y, r0=~r, r=~r, start=~start, end=~end), data=dd, inherit.aes=FALSE),
    geom_segment(aes_(x=~x, xend=~x+r*2.5, y=~y+r, yend=~y+r), data=dd, inherit.aes=FALSE),
    geom_text(aes_(x=~x+r*2.6, y=~y+r, label=~label), data=dd, hjust='left', inherit.aes=FALSE)
  )
}

geom_scatterpie <- function(mapping=NULL, data, cols, pie_scale = 1, sorted_by_radius = FALSE, legend_name = "type", long_format=FALSE, ...) {
  if (is.null(mapping))
    mapping <- aes_(x = ~x, y = ~y)
  mapping <- modifyList(mapping,
                        aes_(r0 = 0,
                             fill = as.formula(paste0("~", legend_name)),
                             amount=~value)
  )
  
  
  if (!'r' %in% names(mapping)) {
    xvar <- get_aes_var(mapping, "x")
    size <- diff(range(data[, xvar]))/ 50 * pie_scale
    data$r <- size
    mapping <- modifyList(mapping, aes_(r=size))
  }
  
  names(mapping)[match(c("x", "y"), names(mapping))] <- c("x0", "y0")
  if(long_format==TRUE){
    df <- data
    names(df)[which(names(df) == cols)] = legend_name
    
  } else{
    data <- data[rowSums(data[, cols]) > 0, ]
    ## df <- gather_(data, "type", "value", cols)
    cols2 <- enquo(cols)
  
    df <- gather(data, "type", "value", !!cols2)
    df$type <- factor(df$type, levels = cols) # set legend order based on order of "cols"      
    names(df)[which(names(df) == "type")] = legend_name
  }
  ## df <- gather_(data, "type", "value", cols)
  # cols2 <- enquo(cols)
  # df <- gather(data, "type", "value", !!cols2)
  # names(df)[which(names(df) == "type")] = legend_name
  print(df)
  ## df$type <- factor(df$type, levels=cols)
  if (!sorted_by_radius) {
    
    return(geom_arc_bar(mapping, data=df, stat='pie', inherit.aes=FALSE, ...) )
  }

  lapply(split(df, df$r)[as.character(sort(unique(df$r), decreasing=TRUE))], function(d) {
    
     geom_arc_bar(mapping, data=d, stat='pie', inherit.aes=FALSE, ...)
  })
}


SHAPE_FILE_PATH<-"C:\\Users\\RIZWAN\\Documents\\map\\India_States_ADM1_GADM-shp\\3e563fd0-8ea1-43eb-8db7-3cf3e23174512020330-1-layr7d.ivha.shp"
SHAPE_FILE_PATH1<-"C:\\Users\\RIZWAN\\Downloads\\India_Boundary\\India_Boundary\\India_Boundary.shp"

# assam_shape_file<-"C:\\Users\\RIZWAN\\Downloads/assam_administrative/assam_administrative.shp"
india_Shape_file<-("C:\\Users\\RIZWAN\\Documents\\map\\India_Districts_ADM2_GADM-shp\\India_Districts_ADM2_GADM.shp")

india <- readOGR(dsn = SHAPE_FILE_PATH)
india1 <- readOGR(dsn = SHAPE_FILE_PATH1)


india<-india[india[["NAME_1"]]!="Jammu and Kashmir",]

india1<-fortify(india1)
colnames(india1)[1]<-"Longitude"
colnames(india1)[2]<-"Latitude"

grey_mask<-india[india[["NAME_1"]]=="Assam" | india[["NAME_1"]]=="Maharashtra" | india[["NAME_1"]]=="Goa" | india[["NAME_1"]]=="Kerala",]

india<-fortify(india)
colnames(india)[1]<-"Longitude"
colnames(india)[2]<-"Latitude"
india

data<- read_excel("C:\\Users\\RIZWAN\\Documents\\map\\map_population.xlsx", sheet = "a",col_names = c("indNames","Longitude","Latitude","Cluster1","Cluster2","Cluster3","Cluster4","Radius"))
data$radius<-rep(1,27)
grey_mask<-fortify(grey_mask)

#goa1 <- india[(india$Latitude > 25.0 & india$Longitude > 89.5)  , ]

#grey_mask<-goa1[assam1$Latitude<27 & goa1$Longitude < 92,]
# nrow(grey_mask)
colnames(grey_mask)[1]<-"Longitude"
colnames(grey_mask)[2]<-"Latitude"
# india1
india_map1 <- ggplot(india1) + 
  geom_map(data = india1,map=india1, aes(x=Longitude, y=Latitude, map_id=id), colour="black", fill = "white") 
#india_map1
# india1
india_map<-india_map1+  geom_map(data = india,map=india, aes(x=Longitude, y=Latitude, map_id=id), colour="black", fill = "white") 
# india_map
# grey_mask
#india_map<-india_map + geom_map(data = grey_mask,map=grey_mask, aes(x=Longitude, y=Latitude, map_id=id), colour="black", fill = "grey") 

 pdf("india1.pdf",height = 12,width = 10)
india_map +
  geom_scatterpie(aes(x=Longitude, y=Latitude ),pie_scale = 1, legend_name = "Genetic_Clusters",
                            data = data, cols=c("Cluster1","Cluster2","Cluster3","Cluster4"),show.legend=NA)+ theme(),scale_fill_manual(values=c("red","purple","orange","blue"))
  
dev.off()

# rm(list=ls())
library(ggforce)



india_det <- readOGR(dsn=india_Shape_file)
assam<-india_det[ india_det@data[["NAME_1"]]== "Assam" ,]
assam<-fortify(assam)
colnames(assam)[1]<-"Longitude"
colnames(assam)[2]<-"Latitude"
assam<-assam[assam$Longitude<92,]


data_assam<- read_excel("C:\\Users\\RIZWAN\\Documents\\map\\Assam.xlsx", sheet = "Assam",col_names = c("indNames","long","lat","Cluster1","Cluster2","Cluster3","Cluster4","radius"))
data_assam$radius<-data_assam$radius/30
 pdf("Assam.pdf",height=3.8,width=7)
 ggplot(assam) + 
  geom_map(data = assam,map=assam, aes(x=Longitude, y=Latitude, map_id=id), color="black" , fill = "white") +
  geom_scatterpie(aes(x=long, y=lat,r=radius), legend_name="Genetic_Clusters",
                  data = data_assam, cols=c("Cluster1","Cluster2","Cluster3","Cluster4"))+theme(legend.background = element_rect(fill="grey94"))+scale_fill_manual(values=c("orange","blue","orange","blue")) + geom_scatterpie_legend(data_assam$radius,x=90.5,y=25.3,labeller = function(x) x*30)
 dev.off()

# funct("Assam",xcoord=95,ycoord=25)

# funct("Kerala",xcoord=78,ycoord=12)




india_det <- readOGR(dsn=india_Shape_file)
assam<-india_det[ india_det@data[["NAME_1"]]=="Goa" |india_det@data[["NAME_1"]]=="Maharashtra" ,]
    

assam<-fortify(assam)
colnames(assam)[1]<-"Longitude"
colnames(assam)[2]<-"Latitude"
assam<-assam[assam$Latitude<16.6,]
    


data_assam<- read_excel("C:\\Users\\RIZWAN\\Documents\\map\\Assam.xlsx", sheet = "Goa",col_names = c("indNames","long","lat","Cluster1","Cluster2","Cluster3","Cluster4","radius"))
data_assam$radius<-data_assam$radius/40
pdf("Goa.pdf",height=3.4,width=6.7)
  ggplot(assam) + 
    geom_map(data = assam,map=assam, aes(x=Longitude, y=Latitude, map_id=id), color="black" , fill = "white") +
    geom_scatterpie(aes(x=long, y=lat,r=radius), legend_name="Genetic_Clusters",
                    data = data_assam, cols=c("Cluster1","Cluster2","Cluster3","Cluster4"))+scale_fill_manual(values=c("red","purple","orange","blue")) + geom_scatterpie_legend(data_assam$radius,x=75,y=16,labeller = function(x) x*40)

  dev.off()
  

  
  
  india_det <- readOGR(dsn=india_Shape_file)
  assam<-india_det[ india_det@data[["NAME_1"]]== "Kerala" ,]
  assam<-fortify(assam)
  colnames(assam)[1]<-"Longitude"
  colnames(assam)[2]<-"Latitude"
  assam<-assam[assam$Latitude>10.5 & assam$Latitude<12.2,]
  
  
  data_assam<- read_excel("C:\\Users\\RIZWAN\\Documents\\map\\Assam.xlsx", sheet = "Kerala",col_names = c("indNames","long","lat","Cluster1","Cluster2","Cluster3","Cluster4","radius"))
  data_assam$radius<-data_assam$radius/30
  pdf("Kerala.pdf",height=3.4,width=6.7)
  ggplot(assam) + 
    geom_map(data = assam,map=assam, aes(x=Longitude, y=Latitude, map_id=id), color="black" , fill = "white") +
    geom_scatterpie(aes(x=long, y=lat,r=radius), legend_name="Genetic_Clusters",
                    data = data_assam, cols=c("Cluster1","Cluster2","Cluster3","Cluster4"))+scale_fill_manual(values=c("purple","purple","orange","blue")) + geom_scatterpie_legend(data_assam$radius,x=75.1,y=11.3,labeller = function(x) x*30)
  dev.off()
  
