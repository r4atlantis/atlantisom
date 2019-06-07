#' @title Mapping survey postions on Atlantis polygons & outputting table of survey stations per polygon
#' @description  Functions to be included in AtlantisOM package
#' @details INPUT: 1) An Atlantis model polygon shape file,  2) table of survey station points as input
#' @details OUTPUT: 1) Map of Atlantis polygons with survey stations plotted as points, 2) Table of Altantis polygon numbers with number of survey stations per polygon
#' @author Erik Olsen (eriko@hi.no)



#' LIBRARIES
#' -----------------
library("rgdal") #requires 'sp'
library("maptools")
library("ggplot2")
library("dplyr")


#' INPUT FORMAT TO polygonSurvey function
#  shape_dir <- "/Users/eriko/ownCloud/Research/atlantis/NOBA/spatial/nordic_grid_220812"
#  shape_file <- "MENUIIareasPolNewId_grass_tol0p01"
#  survey_file <- "../survey_positions.csv" #two columns named lat and lon

polygonSurvey <-  function (shape_dir, shape_file, survey_file) { 
  
  #' IMPORT FILES
  #' ------------------
  #' Import Atlantis shape-file 
  
  NOBAsp <- readOGR(shape_dir,shape_file)
  #slotNames(NOBAsp) # look at the slotnames
  #names(NOBAsp)
  #str(NOBAsp, max.level=3)
  
  # load survey files & extract position data
  w_pos <- read.csv2(survey_file)
  w_pos$id <- 1
  
  #' CREATE DATASETS FOR MAPPING
  #' --------------------
  NOBA.f<-fortify(NOBAsp, region="nyId") 
  cnames <- aggregate(cbind(long, lat) ~ id, data=NOBA.f, FUN=function(x)mean(range(x))) # polygon names with midpoints
  world<- map_data("world") 
  
  
  #' MAPPING
  #' --------------------
  #' Atlantis Polygon map
  #' Background map
  NOBAmap1 <- ggplot(world, aes(x=long, y=lat, group=group)) + geom_polygon(colour="gray65", fill="gray65") +  coord_map(projection="lambert", parameters = c(mean(NOBA.f$long), mean(NOBA.f$lat)), xlim = c(min(NOBA.f$long), max(NOBA.f$long)), ylim=c(min(NOBA.f$lat), max(NOBA.f$lat)))+ geom_polygon(data=NOBA.f, aes(x=long, y=lat, group=group),colour="slategray", fill=NA) +theme_bw() + theme(plot.title = element_text(size=16, face="bold"))  + geom_text(data=cnames, aes(x=long, y=lat, group=id, label = id), size=3) 
  
  #' Save polygon map to .PNG file
  NOBAmap1
  ggsave("polygons_map.png", scale = 1, dpi = 400)
  
  
  #' Survey points on Atlantis polygons
  surveymap <- ggplot(world, aes(x=long, y=lat, group=group)) + geom_polygon(colour="gray65", fill="gray65") +  coord_map(projection="lambert", parameters = c(mean(NOBA.f$long), mean(NOBA.f$lat)), xlim = c(min(NOBA.f$long), max(NOBA.f$long)), ylim=c(min(NOBA.f$lat), max(NOBA.f$lat))) + geom_point(data=w_pos, aes(x=lon, y=lat, group=id)) + geom_polygon(data=NOBA.f, aes(x=long, y=lat, group=group),colour="slategray", fill=NA)  + geom_text(data=cnames, aes(x=long, y=lat, group=id, label = id), size=3, colour="red") 
  surveymap
  
  #' save survey on polygon map to .PNG file
  ggsave("polygons_survey_stations_map.png", scale=1, dpi=400)
  
  
  #' SURVEY COVERAGE PR ATLANTIS POLYGON (TABLE)
  #' ------------------
  
  #' Count number of stations in each polygon 
  coords = cbind(w_pos$lon, w_pos$lat)
  colnames(coords) <- c("long", "lat")
  w_sp = SpatialPoints(coords, proj4string=CRS("+proj=longlat +ellps=GRS80"))
  res <- over(w_sp, NOBAsp2) #p=points as SpatialPoints, x=SpatialPolygonDataframe
  w_tbl <- count(res, box_id)
  cnames$no_st <- 1
  cnames$no_st <- w_tbl$n[match(cnames$id, w_tbl$box_id)]
  
  write.csv2(cnames, file="polygons_survey_stations.csv")
}