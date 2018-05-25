#!/usr/bin/env Rscript

## This script computes the road taken by a car between an OD pair
source("/Users/lavieestuntoucan/Civilia/tech/general/load_R_pkg.R")
setwd("/Users/lavieestuntoucan/civ-3r/flux/tech/R")
##
## OD Enquete
od <- readRDS("data/enquete_OD_2011/Trv11pv2a.RDS")
##
## Zone
coins <- data.frame(lat = rep(x=NA,14), lon = rep(x=NA,14))
coins[1,] <- c(46.355325, -72.638880)
coins[2,] <- c(46.362196, -72.626435)
coins[3,] <- c(46.362730, -72.624632)
coins[4,] <- c(46.364033, -72.619826)
coins[5,] <- c(46.364447, -72.616478)
coins[6,] <- c(46.365466, -72.617208)
coins[7,] <- c(46.366680, -72.615706)
coins[8,] <- c(46.364518, -72.608839)
coins[9,] <- c(46.360473, -72.604376)
coins[10,] <- c(46.355438, -72.611672)
coins[11,] <- c(46.355201, -72.614461)
coins[12,] <- c(46.349751, -72.623259)
coins[13,] <- c(46.351350, -72.628881)
coins[14,] <- c(46.351054, -72.632228)

n <- 8
zoneSud <- data.frame(lat = rep(x=NA,n), lon = rep(x=NA,n))
zoneSud[1,] <- c(46.348567, -72.596029)
zoneSud[2,] <- c(46.321425, -72.557749)
zoneSud[3,] <- c(46.311585, -72.568735)
zoneSud[4,] <- c(46.347797, -72.627340)
zoneSud[5,] <- c(46.349751, -72.623259)
zoneSud[6,] <- c(46.355201, -72.614461)
zoneSud[7,] <- c(46.355438, -72.611672)
zoneSud[8,] <- c(46.360473, -72.604376)

n <- 6
zoneOuest <- data.frame(lat = rep(x=NA,n), lon = rep(x=NA,n))
zoneOuest[1,] <- c(46.351054, -72.632228)
zoneOuest[2,] <- c(46.347797, -72.627340)
zoneOuest[3,] <- c(46.311585, -72.568735)
zoneOuest[4,] <- c(46.297486, -72.583952)
zoneOuest[5,] <- c(46.269013, -72.688666)
zoneOuest[6,] <- c(46.317644, -72.743941)

n <- 7
zoneEst <- data.frame(lat = rep(x=NA,n), lon = rep(x=NA,n))
zoneEst[1,] <- c(46.360473, -72.604376)
zoneEst[2,] <- c(46.374779, -72.592201)
zoneEst[3,] <- c(46.411570, -72.574209)
zoneEst[4,] <- c(46.403758, -72.539533)
zoneEst[5,] <- c(46.370840, -72.485631)
zoneEst[6,] <- c(46.321425, -72.557749)
zoneEst[7,] <- c(46.348567, -72.596029)
##
## Map of TR
coord <- data.frame(lat=46.357423, lon=-72.625598)
map <- leaflet() %>%
  addTiles('http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png') %>%
  setView(coord$lon, coord$lat, zoom = 14)


## Zones
map1 <- addPolygons(map, 
                    lng=coins$lon,
                    lat=coins$lat,
                    opacity=1,
                    color="blue")
map1 <- addPolygons(map1, 
                    lng=zoneSud$lon,
                    lat=zoneSud$lat,
                    opacity=1,
                    color="red")
map1 <- addPolygons(map1, 
                    lng=zoneOuest$lon,
                    lat=zoneOuest$lat,
                    opacity=1,
                    color="green")
map1 <- addPolygons(map1, 
                    lng=zoneEst$lon,
                    lat=zoneEst$lat,
                    opacity=1,
                    color="orange")

map1


## Only motorised ways
motor <- c(1, 2, 3, 4, 6, 7, 8, 10, 11, 12)
od.motor <- od %>% filter(mode1 %in% motor | mode2 %in% motor | mode3 %in% motor )
##
## Mark each move by O-inside (1,0), D-outside (1,2,3)
od.motor <- od.motor %>%
  mutate(ori.in.area = ifelse(point.in.polygon(ylatori, xlonori, coins$lat, coins$lon, mode.checked=FALSE)==1, 1, 0),
         des.in.area = ifelse(point.in.polygon(ylatdes, xlondes, coins$lat, coins$lon, mode.checked=FALSE)==1, 1, 0),
         des.zoneOuest = ifelse(point.in.polygon(ylatdes, xlondes, zoneOuest$lat, zoneOuest$lon, mode.checked=FALSE)==1, 1, 0),
         des.zoneEst = ifelse(point.in.polygon(ylatdes, xlondes, zoneEst$lat, zoneEst$lon, mode.checked=FALSE)==1, 1, 0),
         des.zoneSud = ifelse(point.in.polygon(ylatdes, xlondes, zoneSud$lat, zoneSud$lon, mode.checked=FALSE)==1, 1, 0))


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