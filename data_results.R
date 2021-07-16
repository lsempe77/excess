
exreg <- e15 %>%  summarise (ex=sum(excess,na.rm = T)) %>%
  mutate (ex=round(ex,0))

exreg.low <- e15 %>%  summarise (ex=sum(excess.low,na.rm = T)) %>%
  mutate (ex=round(ex,0))


exreg.up <- e15 %>%  summarise (ex=sum(excess.up,na.rm = T)) %>%
  mutate (ex=round(ex,0))


exreg.q <- e17 %>%  summarise (ex=sum(excess,na.rm = T)) %>%
  mutate (ex=round(ex,0))

exreg.q.low <- e17 %>%  summarise (ex=sum(excess.low,na.rm = T)) %>%
  mutate (ex=round(ex,0))

exreg.q.up <- e17 %>%  summarise (ex=sum(excess.up,na.rm = T)) %>%
  mutate (ex=round(ex,0))


a60s <- e15 %>% group_by(range) %>%
  summarise (ex=sum(excess,na.rm = T)) %>% mutate (prop=ex/sum(ex)*100) %>%  filter(range=="a60.69" |range=="a70.79"| range=="a80" ) %>%
  dplyr::select(prop) %>% summarise(prop=sum(prop))%>%
  mutate (prop=round(prop,1))

a60s.q <- e17 %>% group_by(range) %>%
  summarise (ex=sum(excess,na.rm = T)) %>% mutate (prop=ex/sum(ex)*100) %>%  filter(range=="a60.69" |range=="a70.79"| range=="a80" ) %>%
  dplyr::select(prop) %>% summarise(prop=sum(prop))%>%
  mutate (prop=round(prop,1))

##


lima1 <- e15 %>% group_by(Departamento) %>%
  summarise (ex=sum(excess,na.rm = T)) %>% mutate (prop=ex/sum(ex)*100) %>% filter(Departamento=="LIMA") %>% dplyr::select(prop) %>%
  mutate (prop=round(prop,1))

lima1.q <- e17 %>% group_by(Departamento) %>%
  summarise (ex=sum(excess,na.rm = T)) %>% mutate (prop=ex/sum(ex)*100) %>% filter(Departamento=="LIMA") %>% dplyr::select(prop) %>%
  mutate (prop=round(prop,1))

fPaste <- function(vec) sub(",\\s+([^,]+)$", " and \\1", toString(vec))

menos5<-as.data.frame(e15 %>% group_by(Departamento) %>%
                        summarise (ex=sum(excess,na.rm = T)) %>% top_n(-5))
menos5<-paste0(menos5$Departamento)
menos5<-str_to_title(menos5)

menos5<-fPaste(menos5)

menos5.q<-as.data.frame(e17 %>% group_by(Departamento) %>%
                          summarise (ex=sum(excess,na.rm = T)) %>% top_n(-5))
menos5.q<-paste0(menos5.q$Departamento)
menos5.q<-str_to_title(menos5.q)

menos5.q<-fPaste(menos5.q)

###


mortforecast <- as.data.frame(combined.pop.mort %>%
                                ungroup () %>%
                                summarise (deaths=sum(mort.estim.edades.f,na.rm = T)))

###


subrate <- as.data.frame(sub.registr.edad %>% ungroup() %>%
                         summarise (s=mean(sub.mean,na.rm =T),
                                    s=s*100) %>%
                         mutate (s=round(s,1)))

menoscomplete<-as.data.frame(sub.registr.edad %>% group_by(Departamento) %>%
                               filter (AÑO == 2019) %>%
                               summarise (sub=mean(sub.mean,na.rm = T)) %>% filter (sub<.4))

menoscomplete<-paste0(menoscomplete$Departamento)
menoscomplete<-str_to_title(menoscomplete)
menoscomplete<-fPaste(menoscomplete)

mascomplete<-as.data.frame(sub.registr.edad %>% group_by(Departamento) %>%
                             filter (AÑO == 2019) %>%
                             summarise (sub=mean(sub.mean,na.rm = T)) %>% filter (sub>.8))

mascomplete<-paste0(mascomplete$Departamento)
mascomplete<-str_to_title(mascomplete)
mascomplete<-fPaste(mascomplete)


###


proj.sinadef2020b<-proj.sinadef2020

proj.sinadef2020b<-proj.sinadef2020b %>% mutate(perc.change=perc.change-1)

perc.change <-
  as.data.frame(proj.sinadef2020b %>%
                  summarise (perc.change=mean(perc.change,na.rm = T)*100))%>%
  mutate (perc.change=round(perc.change,1))

perc.change.sd <-
  as.data.frame(proj.sinadef2020b %>%
                  summarise (perc.change=sd(perc.change,na.rm = T)*100))%>%
  mutate (perc.change=round(perc.change,1))

perc.change.dep <-
  as.data.frame(proj.sinadef2020b %>% group_by(Departamento)%>%
                  summarise (perc.change=mean(perc.change,na.rm = T)*100))


pasco<-as.data.frame(perc.change.dep %>% top_n(1))[1,2]

ancash<-as.data.frame(perc.change.dep %>% top_n(2))[1,2]

piura<-as.data.frame(perc.change.dep %>% top_n(3))[3,2]

###


unregcovid <- as.data.frame (
  tateti5 %>% ungroup %>%
    summarise (sum=sum(complete.covid))) %>%
  mutate (sum=round(sum,0))

unregcovid.q<-as.data.frame (
  tateti6 %>% ungroup %>%
    summarise (sum=sum(complete.covid))) %>%
  mutate (sum=round(sum,0))

unregcovidmean<-as.data.frame(tateti5 %>% ungroup %>%
                                summarise (mean=mean(complete.covid,na.rm = T))) %>%
  mutate (mean=round(mean,0))

unregcovidmean.q<-as.data.frame(tateti6 %>% ungroup %>%
                                  summarise (mean=mean(complete.covid,na.rm = T))) %>%
  mutate (mean=round(mean,0))

unregcovidsd<-as.data.frame(tateti5 %>% ungroup %>%
                              summarise (sd=sd(complete.covid,na.rm = T)))%>%
  mutate (sd=round(sd,0))

unregcovidsd.q<-as.data.frame(tateti6 %>% ungroup %>%
                                summarise (sd=sd(complete.covid,na.rm = T)))%>%
  mutate (sd=round(sd,0))

unregcovidmax<- as.data.frame(tateti5 %>%
                                group_by(Departamento) %>%
                                summarise (mean=sum(complete.covid,na.rm = T))) %>% top_n(3)

unregcovidmax.q<- as.data.frame(tateti6 %>%
                                  group_by(Departamento) %>%
                                  summarise (mean=sum(complete.covid,na.rm = T))) %>% top_n(3)

##

exinei <-tateti5 %>% ungroup() %>%
  summarise(excess.registered.Naive=sum(excess_deaths.sum),
            excess=sum(excess.total.mean,na.rm = T),
            excess.l=sum(excess.total.low,na.rm = T),
            excess.u=sum(excess.total.up,na.rm = T))%>%
  mutate (excess=round(excess,0),
          excess.l=round(excess.l,0),
          excess.u=round(excess.u,0))

excessmean<-as.data.frame(exinei$excess)

excesslow<-as.data.frame(exinei$excess.l)
excessup<-as.data.frame(exinei$excess.u)

exinei.q <-tateti6 %>% ungroup() %>%
  summarise(excess.registered.Naive=sum(excess_deaths.sum),
            excess=sum(excess.total.mean,na.rm = T),
            excess.l=sum(excess.total.low,na.rm = T),
            excess.u=sum(excess.total.up,na.rm = T))%>%
  mutate (excess=round(excess,0),
          excess.l=round(excess.l,0),
          excess.u=round(excess.u,0))


excessmean.q<-as.data.frame(exinei.q$excess)

excesslow.q<-as.data.frame(exinei.q$excess.l)
excessup.q<-as.data.frame(exinei.q$excess.u)

covid <- fallecidos_covid.l %>% ungroup() %>%
  summarise (c=sum(Covid_deaths))

num<-as.data.frame(excessmean-covid)

num.q<-as.data.frame(excessmean.q-covid)


num<- num %>% mutate(ex=round(`exinei$excess`,0)) %>% dplyr::select(ex)

#

toted<-as.data.frame(tateti5%>%group_by(range)%>%
                       summarise(`Excess`=sum(excess.total.mean,na.rm = T)) %>%
                       mutate(Excessp=Excess/sum(Excess)) %>%
                       filter (range== "a60.69" | range== "a70.79" | range== "a80" )  %>%
                       ungroup() %>% summarise (e=sum(Excessp)*100)) %>%
  mutate (e=round(e,0))

toted2<- as.data.frame(tateti5%>%group_by(range)%>%
                         filter (range== "a60.69" | range== "a70.79" | range== "a80" )  %>%
                         ungroup() %>%
                         summarise(`Excess`=sum(excess.total.mean,na.rm = T)))  %>%
  mutate (`Excess`=round(`Excess`,0))


toted3<- as.data.frame(tateti5%>%group_by(range)%>%
                         filter (range== "a80" )  %>%
                         ungroup() %>%
                         summarise(`Excess`=sum(excess.total.mean,na.rm = T))) %>%
  mutate (`Excess`=round(`Excess`,0))

#
#variable.names(tateti6)

#tateti6%>%group_by(range)%>% summarise(`Excess`=sum(excess.total.mean,na.rm = T)) %>%mutate(Excessp=Excess/sum(Excess)) %>%filter (range== "a60.69" | range== "a70.79" | range== "a80" ) %>% group_by(range)%>% summarise (e=sum(Excessp)*100)

toted.q<-as.data.frame(tateti6%>%group_by(range)%>%
                         summarise(`Excess`=sum(excess.total.mean,na.rm = T)) %>%
                         mutate(Excessp=Excess/sum(Excess)) %>%
                         filter (range== "a60.69" | range== "a70.79" | range== "a80" )  %>%
                         ungroup() %>% summarise (e=sum(Excessp)*100)) %>%
  mutate (e=round(e,2))

toted2.q<- as.data.frame(tateti6%>%group_by(range)%>%
                           filter (range== "a60.69" | range== "a70.79" | range== "a80" )  %>%
                           ungroup() %>%
                           summarise(`Excess`=sum(excess.total.mean,na.rm = T)))  %>%
  mutate (`Excess`=round(`Excess`,2))


toted3.q<- as.data.frame(tateti6%>%group_by(range)%>%
                           filter (range== "a80" )  %>%
                           ungroup() %>%
                           summarise(`Excess`=sum(excess.total.mean,na.rm = T))) %>%
  mutate (`Excess`=round(`Excess`,0))

###


exineiregion <- tateti5 %>% group_by(Departamento) %>%
  summarise(`Total excess`=sum(excess.total.mean,na.rm = T),            `Lower.CI95%`=sum(excess.total.low,na.rm = T),
            `Upper.CI95%`=sum(excess.total.up,na.rm = T),
            excess_deaths.sum=sum(excess_deaths.sum),
            complete.covid=sum(complete.covid),
            dif=`Total excess`-excess_deaths.sum) %>%
  mutate (prop=`Total excess`/sum(`Total excess`)) %>%
  mutate_if(is.numeric, ~round(., 1))

totallima<-exineiregion %>% arrange (-`Total excess`) %>% top_n(1)

totallima2<-as.data.frame(totallima$`Total excess`)

totallima2b<-as.data.frame(totallima$`Lower.CI95%`)
totallima2c<-as.data.frame(totallima$`Upper.CI95%`)

totallima3<-as.data.frame(totallima$prop)*100

topregions<-as.data.frame(exineiregion %>% arrange (-`Total excess`) %>% top_n(5))

sectopn<-str_to_title(topregions[2,1])
sectop<-topregions[2,5]
tertopn<-str_to_title(topregions[3,1])
tertop<-topregions[3,5]
cuartopn<-str_to_title(topregions[4,1])
cuartop<-topregions[4,5]
quintopn<-str_to_title(topregions[5,1])
quintop<-topregions[5,5]


####

exineiregion.q <- tateti6 %>% group_by(Departamento) %>%
  summarise(`Total excess`=sum(excess.total.mean,na.rm = T),            `Lower.CI95%`=sum(excess.total.low,na.rm = T),
            `Upper.CI95%`=sum(excess.total.up,na.rm = T),
            excess_deaths.sum=sum(excess_deaths.sum),
            complete.covid=sum(complete.covid),
            dif=`Total excess`-excess_deaths.sum) %>%
  mutate (prop=`Total excess`/sum(`Total excess`)) %>%
  mutate_if(is.numeric, ~round(., 1))

totallima.q<-exineiregion.q %>% arrange (-`Total excess`) %>% top_n(1)

totallima2.q<-as.data.frame(totallima.q$`Total excess`)

totallima2b.q<-as.data.frame(totallima.q$`Lower.CI95%`)
totallima2c.q<-as.data.frame(totallima.q$`Upper.CI95%`)

totallima3.q<-as.data.frame(totallima.q$prop)*100


topregions.q<-as.data.frame(exineiregion.q %>% arrange (-`Total excess`) %>% top_n(5))

sectopn.q<-str_to_title(topregions.q[2,1])
sectop.q<-topregions.q[2,5]
tertopn.q<-str_to_title(topregions.q[3,1])
tertop.q<-topregions.q[3,5]
cuartopn.q<-str_to_title(topregions.q[4,1])
cuartop.q<-topregions.q[4,5]
quintopn.q<-str_to_title(topregions.q[5,1])
quintop.q<-topregions.q[5,5]



###


diftotal<-as.data.frame(
  tateti5%>% ungroup() %>%
    summarise(`Total excess`=sum(excess.total.mean,na.rm = T),            `Lower.CI95%`=sum(excess.total.low,na.rm = T),
              `Upper.CI95%`=sum(excess.total.up,na.rm = T),
              excess_deaths.sum=sum(excess_deaths.sum),
              complete.covid=sum(complete.covid),
              dif=`Total excess`- excess_deaths.sum,
              dif.l= `Lower.CI95%`-excess_deaths.sum,
              dif.u= `Upper.CI95%`-excess_deaths.sum) %>%
    mutate (prop.m=dif/sum(excess_deaths.sum)*100,
            prop.l=dif.l/sum(excess_deaths.sum)*100,             prop.u=dif.u/sum(excess_deaths.sum)*100)%>%
    dplyr::select(prop.m,prop.l,prop.u)
)

dm<-diftotal$prop.m
dl<-diftotal$prop.l
du<-diftotal$prop.u

#

diftotal.q<-as.data.frame(
  tateti6%>% ungroup() %>%
    summarise(`Total excess`=sum(excess.total.mean,na.rm = T),
              `Lower.CI95%`=sum(excess.total.low,na.rm = T),
              `Upper.CI95%`=sum(excess.total.up,na.rm = T),
              excess_deaths.sum=sum(excess_deaths.sum),
              complete.covid=sum(complete.covid),
              dif=`Total excess`- excess_deaths.sum,
              dif.l= `Lower.CI95%`-excess_deaths.sum,
              dif.u= `Upper.CI95%`-excess_deaths.sum) %>%
    mutate (prop.m=dif/sum(excess_deaths.sum)*100,
            prop.l=dif.l/sum(excess_deaths.sum)*100,
            prop.u=dif.u/sum(excess_deaths.sum)*100)%>%
    dplyr::select(prop.m,prop.l,prop.u)
)

dm.q<-diftotal.q$prop.m
dl.q<-diftotal.q$prop.l
du.q<-diftotal.q$prop.u

###

pob20.r <- pob.regiones.group.age.sex.2020 %>%
  group_by(Departamento,name) %>%
  summarise(population2=sum(population)) %>%
  mutate(freq=population2/sum(population2))

ggplot(pob20.r)+geom_point(aes(name,freq,colour=Departamento))

SINADEF.ts.range<-SINADEF %>%
  mutate(FECHA = as.Date(FECHA, "%Y-%m-%d")) %>%
  group_by(AÑO)%>%
  mutate(week = epiweek(FECHA)) %>%
  ungroup()%>%
  group_by(AÑO,Departamento,week,range)%>%
  summarise(deaths.sinadef=n())

sinadef.base2020 <- SINADEF.ts.range %>%
  filter(AÑO == 2020) %>%
  group_by(Departamento,range) %>%
  summarise(deaths.sinadef=sum(deaths.sinadef,na.rm = T))

#sinadef.base2020 %>%  ungroup() %>% summarise(s=sum(deaths.sinadef))

tateti.std<-combined.pop.mort%>%
  filter(Año==2020) %>%
  dplyr::select(pop.INEI,range) %>%
  left_join(tateti5,by= c("Departamento",  "range")) %>%
  left_join(sinadef.base2020,by= c("Departamento",  "range"))

tateti.std <-tateti.std %>%
  left_join (pob20.r,
             by=c("Departamento","range"="name"))

#variable.names(tateti.std)

tateti.std <- tateti.std %>%
  group_by(Departamento) %>%
  mutate(excess.total.mean=sum(excess.total.mean),
         deaths.sinadef=sum(deaths.sinadef),
         Covid_deaths1=sum(Covid_deaths,na.rm=T),
         rate=((excess.total.mean+deaths.sinadef)/pop.INEI)*1000,
         dsr=rate*freq,
         total=excess.total.mean+deaths.sinadef)

#tateti.std %>% ggplot () + geom_point(aes(Departamento,dsr))+ facet_wrap(~range,scales = "free")

smrd <- tateti.std %>%
  group_by(Departamento)%>%
  summarise(std.d=mean(dsr),
            rate=mean(rate),
            sinadef=mean(deaths.sinadef),
            excess.total.mean=mean(excess.total.mean),
            total=mean(total),
            Covid_deaths1=mean(Covid_deaths1,na.rm = T))


###

tateti.std.q<-combined.pop.mort%>%
  filter(Año==2020) %>%
  dplyr::select(pop.INEI,range) %>%
  left_join(tateti6,by= c("Departamento",  "range")) %>%
  left_join(sinadef.base2020,by= c("Departamento",  "range"))

tateti.std.q <-tateti.std.q %>%
  left_join (pob20.r,
             by=c("Departamento","range"="name"))

variable.names(tateti.std)

tateti.std.q%>%glimpse()

tateti.std.q <- tateti.std.q %>%
  group_by(Departamento) %>%
  mutate(excess.total.mean=sum(excess.total.mean),
         deaths.sinadef=sum(deaths.sinadef),
         counterfactual2020=(deaths.sinadef-(sum(excess_deaths.sum)*perc.change))/sub.mean,
         counterfactual2020.u=(deaths.sinadef-(sum(excess.low)*perc.change))/sub.mean,
         counterfactual2020.l=(deaths.sinadef-(sum(excess.up)*perc.change))/sub.mean,
         Covid_deaths1=sum(Covid_deaths,na.rm=T),
         rate=(excess.total.mean+counterfactual2020)/sum(pop.INEI)*1000,
         dsr=sum(rate)*freq,
         total=excess.total.mean+counterfactual2020)

#tateti.std %>% ggplot () + geom_point(aes(Departamento,dsr))+ facet_wrap(~range,scales = "free")


smrd.q <- tateti.std.q %>%
  group_by(Departamento)%>%
  summarise(std.d=mean(dsr),
            rate=mean(rate),
            sinadef=mean(deaths.sinadef),
            excess.total.mean=mean(excess.total.mean),
            counter=mean(counterfactual2020),
            counter.l=mean(counterfactual2020.l),
            counter.u=mean(counterfactual2020.u),
                        total=mean(total),
            Covid_deaths1=mean(Covid_deaths1,na.rm = T),
            ) %>% adorn_totals()

smrd.q %>% print (n=26)
####

topsmrd<-as.data.frame(smrd %>% group_by(Departamento) %>%
                         summarise (std.d=mean(std.d,na.rm = T)) %>% filter (std.d>10))

topsmrd<-paste0(topsmrd$Departamento)
topsmrd<-str_to_title(topsmrd)
topsmrd<-fPaste(topsmrd)

topsmrd.q<-as.data.frame(smrd.q %>% group_by(Departamento) %>%
                           summarise (std.d=mean(std.d,na.rm = T)) %>% filter (std.d>10))

topsmrd.q<-paste0(topsmrd.q$Departamento)
topsmrd.q<-str_to_title(topsmrd.q)
topsmrd.q<-fPaste(topsmrd.q)

##


lowerb<-as.data.frame(exineiregion %>% group_by(Departamento) %>%
                        summarise (lower=mean(`Lower.CI95%`,na.rm = T)) %>% filter (lower<0))

lowerb<-paste0(lowerb$Departamento)
lowerb<-str_to_title(lowerb)
lowerb<-fPaste(lowerb)

#However, the estimated lower bound are below 0 in cases of `r lowerb`.

excessmean.q.perc<-((excessmean.q-covid$c)/covid$c)*100

exreg.q.perc<-((exreg.q-covid$c)/covid$c)*100
