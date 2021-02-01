shape <- readOGR(dsn = "DATA/INEI_LIMITE_DEPARTAMENTAL_GEOGPSPERU_JUANSUYO_931381206.shp",
                 verbose = FALSE)

cord.UTM <- spTransform(shape, CRS("+init=epsg:24892"))

DEPS<-st_as_sf(cord.UTM)

DEPS <- DEPS %>% left_join(tateti6,by=c("NOMBDEP"="Departamento"))


SINADEF.ts.range<-SINADEF %>%
  group_by(AÑO,Departamento,week)%>%
  summarise(deaths.sinadef=n())

pob20.r <- pob.regiones.group.age.sex.2020 %>%
  group_by(Departamento) %>%
  summarise(population2=sum(population)) %>%
  mutate(freq=population2/sum(population2))

sinadef.base2020.d.r <- SINADEF.ts.range %>%
  filter(AÑO == 2020) %>%
  group_by(Departamento) %>%
  summarise(deaths.sinadef=sum(deaths.sinadef,na.rm = T))

pop.dep.range<-combined.pop.mort%>%group_by(Departamento)%>%
  summarise(pop.INEI=sum(pop.INEI))

mortdep<-combined.pop.mort %>%
  group_by(Departamento) %>%
  summarise (mort.estim=sum(mort.estim.edades.f))

tateti.std.q<- tateti6 %>%
  left_join(sinadef.base2020.d.r) %>% left_join(mortdep) %>%
  summarise(excess.total.mean=sum(excess.total.mean,na.rm = T),
            excess.total.low=sum(excess.total.low,na.rm = T),
            excess.total.up=sum(excess.total.up,na.rm = T),
            deaths.sinadef=sum(deaths.sinadef,na.rm = T),
            excess.reg=sum(excess_deaths.sum,na.rm = T),
            excess.reg.l=sum(excess.low,na.rm = T),
            excess.reg.u=sum(excess.up,na.rm = T),
            counterfactual2020=(deaths.sinadef-excess.reg),
            counterfactual2020.u=(deaths.sinadef-excess.reg.l),
            counterfactual2020.l=(deaths.sinadef-excess.reg.u),
            total=excess.total.mean+(counterfactual2020*(1+(1-sub.mean))),
            total.l=excess.total.low+(counterfactual2020.l*(1+(1-sub.mean))),
            total.u=excess.total.up+(counterfactual2020.u*(1+(1-sub.mean)))) %>%
  left_join(pop.dep.range) %>%
  mutate(rate=total/pop.INEI)

smrd.q <- tateti.std.q %>%
  group_by(Departamento)%>%
  summarise(sinadef=sum(deaths.sinadef,na.rm = T),
            excess.T=sum(excess.total.mean,na.rm = T),
            excess.l=sum(excess.total.low,na.rm = T),
            excess.u=sum(excess.total.up,na.rm = T),
            excess.reg=sum(excess.reg,na.rm = T),
            excess.reg.l=sum(excess.reg.l,na.rm = T),
            excess.reg.u=sum(excess.reg.u,na.rm = T),
            count=sum(counterfactual2020,na.rm = T),
            count.l=sum(counterfactual2020.l,na.rm = T),
            count.u=sum(counterfactual2020.u,na.rm = T),
            total=sum(total,na.rm = T),
            total.l=sum(total.l,na.rm = T),
            total.u=sum(total.u,na.rm = T)) %>% adorn_totals()

smrd.q %>% print (n=26)

###

pob.regiones.2005.2015.long<-pob.regiones.ed.2005.2015%>%
  dplyr::mutate(rowsum = rowSums(.[6:22]))%>%group_by(Departamento,sheet)%>%
  dplyr::select(rowsum)

#head(pob.regiones.2005.2015.long)
colnames(pob.regiones.2005.2015.long)[2]<-"year"
colnames(pob.regiones.2005.2015.long)[3]<-"population"


agestd<-tateti.std.q %>%
  group_by(Departamento) %>%
  summarise(age_adjust = list(ageadjust.direct(count = total,
                                               pop = pop.INEI,
                                               rate = rate,
                                               stdpop = pob20.r$freq))) %>%
  mutate(age_adjust = map(age_adjust, as.data.frame.list))  %>%
  unnest(cols = c(age_adjust))


tasas_mortalidad_regiones_INEI <- read_excel("data/tasas_mortalidad_regiones_INEI.xlsx")

mortality.rates<-cbind(agestd[1],agestd[2:5]*10^3)%>% left_join(tasas_mortalidad_regiones_INEI) %>%
  mutate(Difference=crude.rate-`2015`)

mr5<-mortality.rates %>% arrange(Difference)

DEPS<-DEPS %>%left_join(mortality.rates,by=c("NOMBDEP"="Departamento"))

# mortality.rates %>% arrange(-crude.rate)
# mortality.rates %>% arrange(crude.rate)
# mortality.rates %>% arrange(-adj.rate)
# mortality.rates %>% arrange(adj.rate)
#
# mr5[1:5,1:8]
#
# mr5[21:25,1:8]
#


