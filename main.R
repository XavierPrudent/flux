#!/usr/bin/env Rscript

## This script computes the road taken by a car between an OD pair
source("/Users/lavieestuntoucan/Civilia/tech/general/load_R_pkg.R")
setwd("/Users/lavieestuntoucan/civ-3r/flux/tech/R")
source("include.R")
##
## OD Enquete
od <- readRDS("data/enquete_OD_2011/Trv11pv2a.RDS")
##
## Only motorised ways
motor <- c(1, 2, 3, 4, 6, 7, 8, 10, 11, 12)
od.motor <- od %>% filter(mode1 %in% motor | mode2 %in% motor | mode3 %in% motor )
##
## Map of TR
coord <- data.frame(lat=46.357423, lon=-72.625598)
map0 <- leaflet() %>%
  addTiles('http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png') %>%
  setView(coord$lon, coord$lat, zoom = 14)
##
## Zones
zones <- create.zones.Taxibus()
##
## Plot the zones
for( i in 1:length(zones)){
  if( i == 1 ) map <- map0
  map <- map %>% addPolygons(data=zones[[i]], lng=~long, lat=~lat, label=as.character(i))
}
##
## Assign each trip to a zone
tripsInOut <- data.frame(zone = 1:length(zones), entrants = rep(NA,length(zones)), sortants = rep(NA,length(zones)))
for( i in 1:length(zones)){
  zone.i <- zones[[i]]
  od.i <- od.motor %>%
    mutate(ori.in.area = ifelse(point.in.polygon(ylatori, xlonori, zone.i$lat, zone.i$long, mode.checked=FALSE)==1, 1, 0),
           des.in.area = ifelse(point.in.polygon(ylatdes, xlondes, zone.i$lat, zone.i$long, mode.checked=FALSE)==1, 1, 0)) 
  ## Voyages sortants
  tripsInOut$sortants[i] <- od.i %>% 
    filter(ori.in.area == 1 & des.in.area == 0) %>% 
    summarise(sum(facdep)) %>% 
    as.numeric() %>% 
    round()
  ## Voyages entrants
  tripsInOut$entrants[i] <- od.i %>% filter(ori.in.area == 0 & des.in.area == 1) %>% 
    summarise(sum(facdep)) %>% 
    as.numeric() %>% 
    round()
    
  rm(od.i)
}
write.csv(x = tripsInOut, file = "tripsInOut.csv",quote = F,row.names = F)

## Statistics
##
## Number of deplacements starting in the corner
n.start.area <- od.motor %>% filter(ori.in.area == 1) %>% select(facdep) %>% colSums()
## 3567
##
## Nb. of deplacements starting in the corner, ending somewhere else
od.motor.area <- od.motor %>% filter(ori.in.area == 1 & des.in.area == 0)
n.start.area.end.else <- od.motor.area %>% select(facdep) %>% colSums()
## 3370 (94%)
##
## Nb. of depl. ending in the different zones
## Zone ouest : 125
n.start.area.end.ouest <- od.motor.area %>% filter(des.zoneOuest == 1) %>% select(facdep) %>% colSums()
## Zone sud : 522
n.start.area.end.sud <- od.motor.area %>% filter(des.zoneSud == 1) %>% select(facdep) %>% colSums()
## Zone est : 1864
n.start.area.end.est <- od.motor.area %>% filter(des.zoneEst == 1) %>% select(facdep) %>% colSums()

##
## Add all trips
map2 <- map1
for(i in seq_along(od.motor.area)){
  df <- data.frame(lon=c(od.motor.area$xlonori[i],od.motor.area$xlondes[i]), lat=c(od.motor.area$ylatori[i],od.motor.area$ylatdes[i]))
  map2 <- addPolylines(map2, 
                       data = df, 
                       lat = ~lat, 
                       lng = ~lon,
                       opacity=0.5,
                       color = "yellow")
}