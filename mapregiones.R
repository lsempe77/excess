distritaltura <- read_excel("altituddistritos.xlsx")

names(distritaltura) <- toupper(names(distritaltura))

distritaltura <- distritaltura %>%
  filter (!is.na(DISTRITO)) %>%
  mutate(ALTITUD=as.numeric(ALTITUD))

#distritaltura %>% group_by(DISTRITO) %>% distinct()

####

distritaltura2 <- read_excel("altituddistritos2.xlsx")

distritos_altura <- distritaltura2 %>% left_join(distritaltura,
                                                 by=c("IDDIST"="UBIGEO",
                                                      "DEPARTAMEN"="DEPARTAMENTO",
                                                      "DISTRITO","PROVINCIA")) %>%
  select(-c(IDDPTO,IDPROV,CAPITAL,CODCCPP,FUENTE)) %>% rename(UBIGEO=IDDIST,
                                                              DEPARTAMENTO=DEPARTAMEN) %>%
  select(UBIGEO, DISTRITO,  ALTITUD, ELEV_mean)

# distritos_altura %>% ggplot() +
#   geom_point(aes(ELEV_mean,ALTITUD))

# distritos_altura %>% ggplot() +
#   geom_point(aes(ELEV_median,ALTITUD))

# distritos_altura<-distritos_altura %>% mutate(
#   ALTITUD = case_when(!is.na(ALTITUD) ~ ELEV_mean,
#                       T ~ ALTITUD))

###

Poblacion_Distritos <- read_excel("Poblacion Peru 2020 Dpto Prov Dist Final INEI-actualizado.xlsx")

Poblacion_Distritos <- Poblacion_Distritos %>%
  dplyr::select(-c(6:47))

colnames(Poblacion_Distritos)[5]<-"Population2020"

Poblacion_Distritos2<-Poblacion_Distritos %>%
  left_join (distritos_altura, by="UBIGEO")

Poblacion_Distritos2$dif.names<-ifelse(Poblacion_Distritos2$DISTRITO.x==Poblacion_Distritos2$DISTRITO.y,"yes","no")

porllenar1 <- Poblacion_Distritos2 %>% filter (is.na(ALTITUD ))

#porllenar1[,c(1,4)] %>% print (n=36)

#which(Poblacion_Distritos2$UBIGEO  == "030220")
#which(Poblacion_Distritos2$UBIGEO  == "030609")
#which(Poblacion_Distritos2$UBIGEO  == "030610")
#which(Poblacion_Distritos2$UBIGEO  == "030611")
#which(Poblacion_Distritos2$UBIGEO  == "050410")

#variable.names(Poblacion_Distritos2)

Poblacion_Distritos2[964,7] <- 3633
Poblacion_Distritos2[749,7] <- 3025
Poblacion_Distritos2[524,7] <- 3174
Poblacion_Distritos2[288,7] <- 2044
Poblacion_Distritos2[922,7] <- 3873

#which(Poblacion_Distritos2$UBIGEO  == "050411")
#which(Poblacion_Distritos2$UBIGEO  == "050412")
#which(Poblacion_Distritos2$UBIGEO  == "050511")
#which(Poblacion_Distritos2$UBIGEO  == "080911")
#which(Poblacion_Distritos2$UBIGEO  == "080912")

Poblacion_Distritos2[733,7] <- 3052
Poblacion_Distritos2[632,7] <- 3365
Poblacion_Distritos2[336,7] <- 3723
Poblacion_Distritos2[1046,7] <- 1431
Poblacion_Distritos2[648,7] <- 735

#which(Poblacion_Distritos2$UBIGEO  == "080913")
#which(Poblacion_Distritos2$UBIGEO  == "080914")
#which(Poblacion_Distritos2$UBIGEO  == "090719")
#which(Poblacion_Distritos2$UBIGEO  == "090720")
#which(Poblacion_Distritos2$UBIGEO  == "090721")

Poblacion_Distritos2[650,7] <- 683
Poblacion_Distritos2[1283,7] <- 387
Poblacion_Distritos2[954,7] <- 2566
Poblacion_Distritos2[678,7] <- 2367
Poblacion_Distritos2[412,7] <- 2692

#which(Poblacion_Distritos2$UBIGEO  == "090722")
#which(Poblacion_Distritos2$UBIGEO  == "090723")
#which(Poblacion_Distritos2$UBIGEO  == "100113")
#which(Poblacion_Distritos2$UBIGEO  == "100607")
#which(Poblacion_Distritos2$UBIGEO  == "100608")

Poblacion_Distritos2[583,7] <- 2853
Poblacion_Distritos2[476,7] <- 3292
Poblacion_Distritos2[1302,7] <- 2936
Poblacion_Distritos2[916,7] <- 562
Poblacion_Distritos2[1480,7] <- 655

#which(Poblacion_Distritos2$UBIGEO  == "100609")
#which(Poblacion_Distritos2$UBIGEO  == "100610")
#which(Poblacion_Distritos2$UBIGEO  == "100704")
#which(Poblacion_Distritos2$UBIGEO  == "100705")
#which(Poblacion_Distritos2$UBIGEO  == "120609")

Poblacion_Distritos2[979,7] <- 596
Poblacion_Distritos2[719,7] <- 634
Poblacion_Distritos2[777,7] <- 558
Poblacion_Distritos2[653,7] <- 654
Poblacion_Distritos2[1036,7] <- 468

#which(Poblacion_Distritos2$UBIGEO  == "211105")
#which(Poblacion_Distritos2$UBIGEO  == "230111")
#which(Poblacion_Distritos2$UBIGEO  == "250304")
#which(Poblacion_Distritos2$UBIGEO  == "250305")


Poblacion_Distritos2[1772,7] <- 3842
Poblacion_Distritos2[1195,7] <- 36
Poblacion_Distritos2[1354,7] <- 204
Poblacion_Distritos2[1172,7] <- 232

##

#which(Poblacion_Distritos2$UBIGEO  == "030407")
#which(Poblacion_Distritos2$UBIGEO  == "051010")
#which(Poblacion_Distritos2$UBIGEO  == "100106")
#which(Poblacion_Distritos2$UBIGEO  == "030602")
#which(Poblacion_Distritos2$UBIGEO  == "250201")
#which(Poblacion_Distritos2$UBIGEO  == "120604")
#which(Poblacion_Distritos2$UBIGEO  == "120606")



Poblacion_Distritos2[57,7] <- 3139
Poblacion_Distritos2[605,7] <- 3341
Poblacion_Distritos2[883,7] <- 2500
Poblacion_Distritos2[1408,7] <- 3204
Poblacion_Distritos2[1718,7] <- 228
Poblacion_Distritos2[1726,7] <- 665
Poblacion_Distritos2[1763,7] <- 510

Poblacion_Distritos2 %>% filter(is.na(ALTITUD))



#variable.names(Poblacion_Distritos2)
#variable.names(porllenar1)

#sapply(Poblacion_Distritos2, function(x) sum(is.na(x)))


######

shape <- readOGR(dsn = "DISTRITOS2020/limites.shp",verbose = FALSE)

cord.UTM <- spTransform(shape, CRS("+init=epsg:24892"))

districts<-st_as_sf(cord.UTM)

coords <- st_coordinates(st_centroid(st_geometry(districts)))

####


addnbs <- function(sp.sample){

  queen_nb <- poly2nb(sp.sample, queen=T)

  count = card(queen_nb)
  if(!any(count==0)){
    return(queen_nb)
  }

  ## get nearest neighbour index, use centroids:
  nnbs = knearneigh(st_coordinates(st_centroid(st_geometry(sp.sample))))$nn

  no_edges_from = which(count==0)
  for(i in no_edges_from){
    queen_nb[[i]] = nnbs[i]
  }
  return(queen_nb)
}

n2 = addnbs(districts)

#summary(n2)

# plot(st_geometry(districts), border="grey")
# plot(n2, coords, add=TRUE, col="red")

###

neig2<-data.frame(t(sapply(n2, function(x) x[1:max(lengths(n2))])))

districts$id.nei<-seq(1, nrow(districts))

neig2$id.nei<-seq(1, nrow(neig2))

districts<-districts %>% left_join(neig2)

##

#summary(Poblacion_Distritos2$ALTITUD)

districts <- districts %>% right_join (Poblacion_Distritos2)

