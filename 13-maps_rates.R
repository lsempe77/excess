shape <- readOGR(dsn = "DATA/INEI_LIMITE_DEPARTAMENTAL_GEOGPSPERU_JUANSUYO_931381206.shp",
                 verbose = FALSE)

cord.UTM <- spTransform(shape, CRS("+init=epsg:24892"))

DEPS<-st_as_sf(cord.UTM)

DEPS <- DEPS %>% rename("Departamento"="NOMBDEP")%>%
  left_join(tateti6.t)

SINADEF.ts.range<-SINADEF %>%
  group_by(AÑO,Departamento,range,week)%>%
  summarise(deaths.sinadef=n())

pob20.r <- pob.regiones.group.age.sex.2020 %>% rename(range=name)%>%
  group_by(Departamento,range) %>%
  summarise(population2=sum(population)) %>%
  mutate(freq=population2/sum(population2))

sinadef.base2020.d.r <- SINADEF.ts.range %>%
  filter(AÑO == 2020) %>%
  group_by(Departamento,range) %>%
  summarise(deaths.sinadef=sum(deaths.sinadef,na.rm = T))

pop.dep.range<-combined.pop.mort%>%group_by(Departamento,range)%>%
  summarise(pop.INEI=sum(pop.INEI))

mortdep<-combined.pop.mort %>%
  group_by(Departamento,range) %>%
  summarise (mort.estim=sum(mort.estim.edades.f))

tateti.std.q<- tateti6.t %>%
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

pop.dep<-combined.pop.mort%>%group_by(Departamento)%>%
  summarise(pop.INEI=sum(pop.INEI))

crude.rate<- tateti6.t %>%
  left_join(sinadef.base2020.d.r) %>% group_by(Departamento)%>%
  summarise(excess.total.mean=sum(excess.total.mean,na.rm = T),
            excess.total.low=sum(excess.total.low,na.rm = T),
            excess.total.up=sum(excess.total.up,na.rm = T),
            deaths.sinadef=sum(deaths.sinadef,na.rm = T),
            excess.reg=sum(excess_deaths.sum,na.rm = T),
            excess.reg.l=sum(excess.low,na.rm = T),
            excess.reg.u=sum(excess.up,na.rm = T),
            counterfactual2020=deaths.sinadef-excess.reg,
            counterfactual2020.u=(deaths.sinadef-excess.reg.l),
            counterfactual2020.l=(deaths.sinadef-excess.reg.u),
            total=excess.total.mean+(counterfactual2020*(1+(1-sub.mean))),
            total.l=excess.total.low+(counterfactual2020.l*(1+(1-sub.mean))),
            total.u=excess.total.up+(counterfactual2020.u*(1+(1-sub.mean)))) %>%
  left_join(population_regions,by=c("Departamento"="DEPARTAMENTO")) %>%
  summarise(rate=total/`2020`*1000) %>% distinct()

smrd.q <- tateti.std.q %>%
  group_by(Departamento,range)%>%
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

# smrd.q %>% print (n=26)

###

pob.regiones.2005.2015.long<-pob.regiones.ed.2005.2015%>%
  dplyr::mutate(rowsum = rowSums(.[6:22]))%>%group_by(Departamento,sheet)%>%
  dplyr::select(rowsum)

#head(pob.regiones.2005.2015.long)
colnames(pob.regiones.2005.2015.long)[2]<-"year"
colnames(pob.regiones.2005.2015.long)[3]<-"population"


agestd<-tateti.std.q %>% left_join(pob20.r)%>%
  summarise(
  age_adjust = list(ageadjust.direct(count = total,
                                               pop = pop.INEI,
                                               rate = rate,
                                               stdpop = population2))) %>%
  mutate(age_adjust = map(age_adjust, as.data.frame.list))  %>%
  unnest(cols = c(age_adjust))

tasas_mortalidad_regiones_INEI <- read_excel("data/tasas_mortalidad_regiones_INEI.xlsx")


mortality.rates <- agestd %>%
  left_join(crude.rate) %>%
  dplyr::select(-2) %>%
  rename(crude.rate=rate) %>%
  mutate(adj.rate=adj.rate*1000,
         lci=lci*1000,
         uci=uci*1000) %>%
  left_join(tasas_mortalidad_regiones_INEI) %>%
  mutate(Difference=crude.rate-`2015`)

mr5<-mortality.rates %>% arrange(Difference)

DEPS<-DEPS %>%left_join(mortality.rates)

mortality.rates %>% arrange(crude.rate) %>% top_n(3)
mortality.rates %>% arrange(crude.rate) %>% top_n(-3)

mortality.rates %>% arrange(adj.rate) %>% top_n(3)
mortality.rates %>% arrange(adj.rate) %>% top_n(-3)

mortality.rates %>% arrange(Difference) %>% top_n(3)
mortality.rates %>% arrange(Difference) %>% top_n(-3)


adj.rates.mores<-as.data.frame(mortality.rates %>%
                                 arrange(adj.rate) %>%
                                 top_n(3) )

adj.rates.mores<-paste0(adj.rates.mores$Departamento)

adj.rates.mores<-str_to_title(adj.rates.mores)

adj.rates.mores<-knitr::combine_words(adj.rates.mores)



min<-mortality.rates %>%
  arrange(adj.rate) %>%
  top_n(3) %>% summarise(min=min(adj.rate))

max<-mortality.rates %>%
  arrange(adj.rate) %>%
  top_n(3) %>% summarise(max=max(adj.rate))


adj.rates.ci<-as.data.frame(mortality.rates %>%
                  arrange(adj.rate) %>%
                  top_n(3))


adj.rates.ci<-adj.rates.ci %>%
 summarise(min=min(lci),max=max(uci))

adj.rates.ci.min<-adj.rates.ci$min
adj.rates.ci.max<-adj.rates.ci$max

adj.rates.less<-as.data.frame(mortality.rates %>%
                                 arrange(adj.rate) %>%
                                 top_n(-3) )

adj.rates.less<-paste0(adj.rates.less$Departamento)

adj.rates.less<-str_to_title(adj.rates.less)

adj.rates.less<-knitr::combine_words(adj.rates.less)

min2<-mortality.rates %>%
  arrange(adj.rate) %>%
  top_n(-3) %>% summarise(min=min(adj.rate))

max2<-mortality.rates %>%
  arrange(adj.rate) %>%
  top_n(-3) %>% summarise(max=max(adj.rate))


adj.rates.ci.l<-as.data.frame(mortality.rates %>%
                              arrange(adj.rate) %>%
                              top_n(-3))


adj.rates.ci.l<-adj.rates.ci.l %>%
  summarise(min=min(lci),max=max(uci))

adj.rates.ci.l.min<-adj.rates.ci.l$min
adj.rates.ci.l.max<-adj.rates.ci.l$max

###

dif<-as.data.frame(mortality.rates %>% arrange(Difference) %>% top_n(-1))
dif<-paste0(dif$Departamento)

dif<-str_to_title(dif)

dif<-knitr::combine_words(dif)
dif

dif.n<-mortality.rates %>% arrange(Difference) %>% top_n(-1)
dif.n<-dif.n[8]

##


dif.m<-as.data.frame(mortality.rates %>% arrange(Difference) %>% top_n(2))
dif.m<-paste0(dif.m$Departamento)

dif.m<-str_to_title(dif.m)

dif.m<-knitr::combine_words(dif.m)
dif.m

dif.m.n<-mortality.rates %>% arrange(Difference) %>% top_n(2)
dif.m.n<-dif.m.n[8]
