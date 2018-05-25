###############################
## Map the zones west
map.zones.west <- function(map){
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
}
###############################
## Create the zones west
create.zones.west <- function(){
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
}
################################
## Create the zones for taxibus (Ana)
create.zones.Taxibus <- function(){
  
  coins<-data_frame(lat=1:80,long=1:80)
  coins[1,] <- c(46.3087585,-72.5969696)
  coins[2,] <- c(46.289648,-72.658223)
  coins[3,] <- c(46.2830209,-72.6482105)
  coins[4,] <- c(46.3014655,-72.5828934)
  coins[5,] <- c(46.3241123,-72.6543903)
  coins[6,] <- c(46.3112486,-72.6733589)
  coins[7,] <- c(46.3056161,-72.6668358)
  coins[8,] <- c(46.3122565,-72.6426315)
  coins[9,] <- c(46.3241717,-72.659626)
  coins[10,] <- c(46.341832,-72.6989365)
  coins[11,] <- c(46.3008143,-72.7303504)
  coins[12,] <- c(46.2913253,-72.7142143)
  coins[13,] <- c(46.4062323,-72.4235487)
  coins[14,] <- c(46.391464,-72.4659061)
  coins[15,] <- c(46.3957262,-72.4728584)
  coins[16,] <- c(46.4111148,-72.4297714)
  coins[17,] <- c(46.4256707,-72.4493408)
  coins[18,] <- c(46.4050191,-72.5028992)
  coins[19,] <- c(46.3979756,-72.496376)
  coins[20,] <- c(46.41934,-72.442131)
  coins[21,] <- c(46.3847148,-72.4968052)
  coins[22,] <- c(46.3748265,-72.5028993)
  coins[23,] <- c(46.3900432,-72.5233269)
  coins[24,] <- c(46.4049005,-72.5068474)
  coins[25,] <- c(46.4237479,-72.5568008)
  coins[26,] <- c(46.431498,-72.5458574)
  coins[27,] <- c(46.4284809,-72.5408792)
  coins[28,] <- c(46.4207601,-72.5523806)
  coins[29,] <- c(46.4270462,-72.5699758)
  coins[30,] <- c(46.4263215,-72.5687742)
  coins[31,] <- c(46.4254711,-72.5700724)
  coins[32,] <- c(46.4263733,-72.5714779)
  coins[33,] <- c(46.4372062,-72.5789881)
  coins[34,] <- c(46.4458415,-72.5665855)
  coins[35,] <- c(46.4440377,-72.5636244)
  coins[36,] <- c(46.4348993,-72.5752974)
  coins[37,] <- c(46.4487837,-72.6058745)
  coins[38,] <- c(46.4522283,-72.6002097)
  coins[39,] <- c(46.4517109,-72.5992227)
  coins[40,] <- c(46.4480888,-72.6046944)
  coins[41,] <- c(46.3918488,-72.5993729)
  coins[42,] <- c(46.3982419,-72.6170969)
  coins[43,] <- c(46.4042497,-72.6066685)
  coins[44,] <- c(46.3959038,-72.594223)
  coins[45,] <- c(46.381858,-72.5940406)
  coins[46,] <- c(46.3816952,-72.5762844)
  coins[47,] <- c(46.3796228,-72.575984)
  coins[48,] <- c(46.3795043,-72.594223)
  coins[49,] <- c(46.4449544,-72.6173115)
  coins[50,] <- c(46.4353726,-72.6368809)
  coins[51,] <- c(46.4295162,-72.6279545)
  coins[52,] <- c(46.4387441,-72.6075268)
  coins[53,] <- c(46.4212329,-72.7122403)
  coins[54,] <- c(46.3975022,-72.6672647)
  coins[55,] <- c(46.3710968,-72.6970478)
  coins[56,] <- c(46.4069721,-72.7411651)
  coins[57,] <- c(46.4270314,-72.5636673)
  coins[58,] <- c(46.4166773,-72.5469303)
  coins[59,] <- c(46.4114107,-72.5649548)
  coins[60,] <- c(46.4209967,-72.5766277)
  coins[61,] <- c(46.4372062,-72.5804901)
  coins[62,] <- c(46.4348993,-72.5752974)
  coins[63,] <- c(46.424968,-72.590077)
  coins[64,] <- c(46.427316,-72.593998)
  coins[65,] <- c(46.4358458,-72.6141357)
  coins[66,] <- c(46.437916,-72.6100159)
  coins[67,] <- c(46.426614,-72.596554)
  coins[68,] <- c(46.4259666,-72.6012611)
  coins[69,] <- c(46.424665,-72.6130199)
  coins[70,] <- c(46.4144287,-72.6011753)
  coins[71,] <- c(46.401823,-72.6137924)
  coins[72,] <- c(46.4005208,-72.6252079)
  coins[73,] <- c(46.3919672,-72.5982571)
  coins[74,] <- c(46.3954894,-72.5949097)
  coins[75,] <- c(46.389898,-72.586227)
  coins[76,] <- c(46.3858397,-72.5859833)
  coins[77,] <- c(46.3874087,-72.4812269)
  coins[78,] <- c(46.3942759,-72.4716568)
  coins[79,] <- c(46.3909904,-72.4674511)
  coins[80,] <- c(46.385921,-72.477195)
  
  zone1 <- c(1,2,3,4)
  zone1 <- coins[zone1,]
  comment(zone1) <- "zone1"
  zone2 <- c(5,6,7,8)
  zone2 <- coins[zone2,]
  comment(zone2) <- "zone2"
  zone3 <- c(9,10,11,12)
  zone3 <- coins[zone3,]
  comment(zone3) <- "zone3"
  zone4 <- c(13,14,15,16)
  zone4 <- coins[zone4,]
  comment(zone4) <- "zone4"
  zone5 <- c(17,18,19,20)
  zone5 <- coins[zone5,]
  comment(zone5) <- "zone5"
  zone6 <- c(21,22,23,24)
  zone6 <- coins[zone6,]
  comment(zone6) <- "zone6"
  zone7 <- c(25,26,27,28)
  zone7 <- coins[zone7,]
  comment(zone7) <- "zone7"
  zone8 <- c(29,30,31,32)
  zone8 <- coins[zone8,]
  comment(zone8) <- "zone8"
  zone9 <- c(33,34,35,36)
  zone9 <- coins[zone9,]
  comment(zone9) <- "zone9"
  zone10 <- c(37,38,39,40)
  zone10 <- coins[zone10,]
  comment(zone10) <- "zone10"
  zone11 <- c(41,42,43,44)
  zone11 <- coins[zone11,]
  comment(zone11) <- "zone11"
  zone12 <- c(45,46,47,48)
  zone12 <- coins[zone12,]
  comment(zone12) <- "zone12"
  zone13 <- c(49,50,51,52)
  zone13 <- coins[zone13,]
  comment(zone13) <- "zone13"
  zone14 <- c(53,54,55,56)
  zone14 <- coins[zone14,]
  comment(zone14) <- "zone14"
  zone15 <- c(57,58,59,60)
  zone15 <- coins[zone15,]
  comment(zone15) <- "zone15"
  zone16 <- c(61,62,63,64)
  zone16 <- coins[zone16,]
  comment(zone16) <- "zone16"
  zone17 <- c(65,66,67,68)
  zone17 <- coins[zone17,]
  comment(zone17) <- "zone17"
  zone18 <- c(69,70,71,72)
  zone18 <- coins[zone18,]
  comment(zone18) <- "zone18"
  zone19 <- c(73,74,75,76)
  zone19 <- coins[zone19,]
  comment(zone19) <- "zone19"
  zone20 <- c(77,78,79,80)
  zone20 <- coins[zone20,]
  comment(zone20) <- "zone20"
  return(list(zone1,zone2,zone3,zone4,zone5,zone6,zone7,
              zone8,zone9,zone10,zone11,zone12,zone13,
              zone14,zone15,zone16,zone17,zone18,zone19,zone20))
}