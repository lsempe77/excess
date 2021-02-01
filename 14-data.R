#```{r lastdaycoviddeaths}

smrd.q %>% print (n=26)


pm<-((smrd.q[26,3]-smrd.q[26,6])/smrd.q[26,3])*100
pml<-((smrd.q[26,4]-smrd.q[26,7])/smrd.q[26,4])*100
pmu<-((smrd.q[26,5]-smrd.q[26,8])/smrd.q[26,5])*100

pm;pml;pmu

smrd.q[26,]

et<-smrd.q[26,3]
etl<-smrd.q[26,4]
etu<-smrd.q[26,5]

er<-smrd.q[26,6]
erl<-smrd.q[26,7]
eru<-smrd.q[26,8]

smrd.q

SINADEF %>% filter(Departamento=="PASCO") %>% group_by(AÑO) %>% tally ()
SINADEF %>% filter(Departamento=="APURIMAC") %>% group_by(AÑO) %>% tally ()
SINADEF %>% filter(Departamento=="MADRE DE DIOS") %>% group_by(AÑO) %>% tally ()



age<-tateti6%>%group_by(range) %>%
  summarise(`Total excess (TE)`=sum(excess.total.mean,na.rm = T),
            `TE - Lower CI 95%`=sum(excess.total.low,na.rm = T),
            `TE - UpperCI 95%`=sum(excess.total.up,na.rm = T)) %>% adorn_totals()

age60t<-(sum(age[7:9,2])/age[10,2])*100
age60l<-(sum(age[7:9,3])/age[10,3])*100
age60u<-(sum(age[7:9,4])/age[10,4])*100


limat<-smrd.q[15,3]
limal<-smrd.q[15,4]
limau<-smrd.q[15,5]





lastday <- fallecidos_covid.test %>%
  mutate(date = ymd(FECHA)) %>%
  filter(date == max(date)) %>%
  dplyr::select(FECHA) %>%
  distinct()

last.day <-  as.character(format(lastday, "%B %d %Y"))

covid.deaths <- fallecidos_covid.test %>%
  summarise(n=n())

covid.deaths.60 <- fallecidos_covid.test %>%
  filter (range== "a60.69" |
            range == "a70.79" |
            range == "a80")  %>%
  summarise(n=n())

covid.deaths.60.p <- fallecidos_covid.test %>%
  group_by(range) %>%
  summarise(n=n()) %>%
  mutate (ni=n/sum(n)) %>%
  filter (range== "a60.69" |
            range == "a70.79" |
            range == "a80") %>%
  summarise (ni=sum(ni)*100) %>% mutate (ni=round(ni,1)) %>%as.data.frame()

#str(covid.deaths.60.p)

covid.day <- fallecidos_covid.test %>%
  mutate(FECHA = as.Date(FECHA, "%Y-%m-%d")) %>%
  filter(FECHA == max(FECHA)) %>%
  dplyr::select(FECHA) %>%
  distinct()

deaths.growth.sinadef<-SINADEF %>% filter(AÑO >= 2019) %>%
  group_by(AÑO) %>%
  mutate(FECHA = as.Date(FECHA, "%Y-%m-%d")) %>%
  filter(FECHA <= as.Date(covid.day[1,1])) %>%
  summarise (deaths=n()) %>%
  mutate (f=deaths/sum(deaths))

deaths.growth <- round(((deaths.growth.sinadef[2,2]-deaths.growth.sinadef[1,2])/deaths.growth.sinadef[1,2])*100,1)

total.deaths <- (deaths.growth.sinadef[2,2]-deaths.growth.sinadef[1,2])

