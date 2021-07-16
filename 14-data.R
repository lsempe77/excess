# aaa %>% left_join(SINADEF.causes) %>%
#   mutate(year = lubridate::year(Date)) %>% filter(year != 2017) %>%
#   summarise    (m =  case_when (week2 < start ~ mean(deaths,na.rm=T)),
#                 m2 = case_when (week2 >= end ~ mean(deaths,na.rm=T))) %>%
#   ungroup() %>%
#   summarise(mean.prev = mean(m,na.rm=T),
#             mean.post = mean(m2,na.rm=T),
#             Change_registration = ((mean.post-mean.prev)/mean.prev)*.5)
#
#
# datatt<-tt %>% ungroup()%>%
#   summarise (mean.prev=mean(mean.prev,na.rm=T),
#              mean.post=mean(mean.post,na.rm=T),
#              Change_registration=mean(1+Change_registration,na.rm=T))
#
# ttpre<-datatt[1,1]
#
# ttpost<-datatt[1,2]
#
# ttchange<-datatt[1,3]
#
# ttchange
#


sex<-SINADEF %>% filter (AÑO!=2021)%>%
  count(SEXO) %>% filter(SEXO=="FEMENINO"| SEXO=="MASCULINO")%>%
  mutate(prop = prop.table(n)*100) %>% filter (SEXO=="MASCULINO") %>%
  rename('% males'= prop) %>% select('% males')

descriptive<-SINADEF %>% filter (AÑO!=2021)%>%
  summarise (
    Observations = n(),
    age.m = mean(as.numeric(EDAD),na.rm=T),
    age.sd = sd(as.numeric(EDAD),na.rm=T),
    'Age (sd)' = paste0(round(age.m,2)," ","(",round(age.sd,2), ")")) %>%
  bind_cols(sex) %>% select(Observations,'Age (sd)','% males')

descriptive$Data<-"SINADEF 2017-2020"

descriptive<-as.data.frame(descriptive) %>% select(Data,Observations,'Age (sd)','% males')

sinadef.years<-SINADEF %>% filter (AÑO!=2021) %>%
  group_by(AÑO) %>% tally()

sin2017<-sinadef.years[1,2]
sin2018<-sinadef.years[2,2]
sin2019<-sinadef.years[3,2]
sin2020<-sinadef.years[4,2]

#

sex.covid<-fallecidos_covid.test %>%
  count(SEXO) %>% filter(SEXO=="FEMENINO"| SEXO=="MASCULINO")%>%
  mutate(prop = prop.table(n)*100) %>% filter (SEXO=="MASCULINO") %>%
  rename('% males'= prop) %>% select('% males')

descriptive.covid<-fallecidos_covid.test %>%
  summarise (
    Observations = n(),
    age.m = mean(as.numeric(EDAD_DECLARADA),na.rm=T),
    age.sd = sd(as.numeric(EDAD_DECLARADA),na.rm=T),
    'Age (sd)' = paste0(round(age.m,2)," ","(",round(age.sd,2), ")")) %>%
  bind_cols(sex.covid) %>% select(Observations,'Age (sd)','% males')

descriptive.covid$Data<-"COVID-19"

descriptive.covid<-as.data.frame(descriptive.covid) %>% select(Data,Observations,'Age (sd)','% males')

#

descriptive <- descriptive %>% bind_rows(descriptive.covid)

#######

variable.names(smrd.q)

pm<-100-(((smrd.q[226,7])/smrd.q[226,4])*100)
pml<-100-(((smrd.q[226,9])/smrd.q[226,5])*100)
pmu<-100-(((smrd.q[226,8])/smrd.q[226,6])*100)

pm;pml;pmu


et<-smrd.q[226,4]
etl<-smrd.q[226,5]
etu<-smrd.q[226,6]

er<-smrd.q[226,7]
erl<-smrd.q[226,8]
eru<-smrd.q[226,9]

et;etl;etu

er;erl;eru

####


age<-tateti6.t %>% group_by(range) %>%
  summarise(`Total excess (TE)`=sum(excess.total.mean,na.rm = T),
            `TE - Lower CI 95%`=sum(excess.total.low,na.rm = T),
            `TE - UpperCI 95%`=sum(excess.total.up,na.rm = T)) %>% adorn_totals()

age
age60t<-(sum(age[7:9,2])/age[10,2])*100
age60u<-(sum(age[7:9,3])/age[10,3])*100
age60l<-(sum(age[7:9,4])/age[10,4])*100

age60t;age60u;age60l


##

lima<-  tateti.std.q %>% filter (Departamento=="LIMA") %>%
  group_by(range)%>%
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
            total.u=sum(total.u,na.rm = T)) %>%
  adorn_totals()


limat<-lima[10,3]
limal<-lima[10,4]
limau<-lima[10,5]

limat;limal;limau

gc()

####


summary<-tateti.std.q %>%
  ungroup()%>%
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
            total.u=sum(total.u,na.rm = T),
            complete.covid=sum(complete.covid,na.rm = T))


# pander(t(summary),
#        caption = "(\\#tab:summary) Table 1: Summary of estimations, Peru, 2020.",
#        style = 'rmarkdown',
#        split.table = Inf,
#        round=1)

###

a<-summary %>%
  mutate(`Excess registered deaths` =
           paste0(round(excess.reg,0)," ","(",round(excess.reg.l,0)," - ",round(excess.reg.u,0),")"),
         comp=round(100*(excess.reg/excess.T),1),
         comp.l=round(100*((excess.reg.l/excess.u)),1),
         comp.u=round(100*((excess.reg.u/excess.l)),1),
         # `SINADEF registration growth` = paste0(round(ttchange,1),"%"),
         `SINADEF completeness registration` =
           paste0(comp,"%"," ","(",comp.l,"% - ",comp.u,"%)"),
        `Total excess mortality` = paste0(round(excess.T,0)," ","(",round(excess.l,0)," - ",round(excess.u,0),")"),
        `Counterfactual estimated deaths in 2020`=paste0(round(count,0)," ","(",round(count.l,0)," - ",round(count.u,0),")"),
        `Total estimated deaths in 2020`=paste0(round(total,0)," ","(",round(total.l,0)," - ",round(total.u,0),")"),
        `COVID-19 deaths (MoH)`=complete.covid) %>%
  dplyr::select(`COVID-19 deaths (MoH)`,
                `Excess registered deaths`,
                `SINADEF completeness registration`,
                `Total excess mortality`,
                `Total estimated deaths in 2020`)

a<-as.data.frame(t(a))

a <- tibble::rownames_to_column(a, "VALUE")

colnames(a)[1] <- 'Terms'
colnames(a)[2] <- 'Estimates (95% CI)'

a

# pander(t(a), caption = "(\\#tab:summary) Table 1: Summary of estimations, Peru, 2020.",
#        style = 'rmarkdown',
#        split.table = Inf,
#        round=1)

###

ex.reg<-summary %>%
  mutate(`Total Excess REGISTERED mortality based on Poisson models by region and age group` =
           paste0(round(excess.reg,0),"(95% CI"," ",round(excess.reg.l,0)," - ",round(excess.reg.u,0),")"),
         ) %>%
  dplyr::select(`Total Excess REGISTERED mortality based on Poisson models by region and age group`)

covid.oficial<-fallecidos_covid.r.l %>% ungroup()%>%
  summarise(covid.oficial=sum(Covid_deaths))


pcm1<-89884

ex.reg.covid<-summary %>%
  mutate(excess.oficial=100*(round(excess.reg,0)-pcm1)/pcm1,
         excess.oficial.l=100*(round(excess.reg.l,0)-pcm1)/pcm1,
         excess.oficial.u=100*(round(excess.reg.u,0)-pcm1)/pcm1,
         excess.oficial.t=paste0(round(excess.oficial,0),"%"," ",
                                "(95% CI"," ",round(excess.oficial.l,0),"% - ",
                                round(excess.oficial.u,0),"%)"),
  ) %>%
  dplyr::select(excess.oficial.t)

###

ex.total.covid<-summary %>%
  mutate(excess.oficial=100*(round(excess.T,0)-pcm1)/pcm1,
         excess.oficial.l=100*(round(excess.l,0)-pcm1)/pcm1,
         excess.oficial.u=100*(round(excess.u,0)-pcm1)/pcm1,
         excess.oficial.t=paste0(round(excess.oficial,0),"%"," ",
                                "(95% CI"," ",round(excess.oficial.l,0),"% - ",
                                round(excess.oficial.u,0),"%)"),
  ) %>%
  dplyr::select(excess.oficial.t)


###

lima.total <- paste0(round(limat,0)," ","(95% CI"," " ,round(limal,0), " - ", round(limau,0), ")")

###


reg<-  tateti.std.q %>%
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
            total.u=sum(total.u,na.rm = T))

menos5<-as.data.frame(reg %>% group_by(Departamento) %>%
                        summarise (ex=sum(excess.T,na.rm = T)) %>% top_n(-2))


menos5<-paste0(menos5$Departamento)

menos5<-str_to_title(menos5)

menos5<-knitr::combine_words(menos5)

menos5.num<- reg %>%
  group_by(Departamento) %>%
                            summarise (ex=sum(excess.T,na.rm = T)) %>%
  top_n(-2) %>%
                            mutate(r=round(ex,-2)) %>%
                                     summarise(r=max(r))

menos5.num

####


totalage<-tateti6.t%>%group_by(range) %>%
  summarise(`Total excess (TE)`=sum(excess.total.mean,na.rm = T),
            `TE - Lower CI 95%`=sum(excess.total.low,na.rm = T),
            `TE - UpperCI 95%`=sum(excess.total.up,na.rm = T),
            `Excess registered (ER)`=sum(excess_deaths.sum),
            `ER - Lower CI 95%`=sum(excess.low),
            `ER - Upper CI 95%`=sum(excess.up),
            `Excess Covid-19`=sum(complete.covid)) %>%
  adorn_totals()

pr1<-totalage %>% group_by(range) %>% summarise(ex.t.cum=cumsum(`Total excess (TE)`)) %>%
  ungroup() %>%
  filter (range!="Total") %>%
  mutate(pr=prop.table(ex.t.cum)) %>%
  filter (range=="a60.69" | range=="a70.79" | range =="a80") %>%
  summarise(pr=sum(pr)*100)


###

#glimpse(summary)

complete.covid<-tateti6.t %>% ungroup() %>%
  summarise(complete.c = sum(complete.covid,na.rm = T))


complete.covid.reg<-tateti6.t %>% group_by(Departamento) %>%
  summarise(complete.c = sum(complete.covid,na.rm = T)) %>% top_n(3)

complete.covid.reg

apurimac.covid<-complete.covid.reg[1,2]
ayacucho.covid<-complete.covid.reg[2,2]
ucayali.covid<-complete.covid.reg[3,2]


#


ex.unreg<-summary %>% bind_cols(complete.covid) %>%
  mutate(ex.un = sum(excess.T) - sum(excess.reg)  - sum(complete.c),
         ex.un.u = excess.l - excess.reg.l - complete.c,
         ex.un.l = excess.u - excess.reg.u - complete.c,
         unreg =
           paste0(round(ex.un,0)," ",
                 "deaths"," ",
                 "(95% CI"," ",round(ex.un.u,0),
                 " - ",
                 round(ex.un.l,0),")")) %>%
  dplyr::select(unreg)

#####

ex.unreg.pc<-summary %>%
  mutate(ex.un = (excess.T - excess.reg)/excess.reg,
         ex.un.l = (excess.T - excess.reg.l)/excess.reg.l,
         ex.un.u = (excess.T - excess.reg.u)/excess.reg.u,
         unreg.pct =
           paste0(round(ex.un,3)*100,
                 "%"," ",
                 "(95% CI"," ",round(ex.un.u,2)*100,
                 "% - ",
                 round(ex.un.l,3)*100,"%)")) %>%
  dplyr::select(unreg.pct)

ex.unreg.pc

####


unreg.more<-as.data.frame(reg %>% group_by(Departamento) %>%
                        summarise (ex=sum(excess.T-excess.reg,na.rm = T)) %>% top_n(3))


unreg.more

lamba.more<-unreg.more[1,2]
lima.more<-unreg.more[2,2]
piura.more<-unreg.more[3,2]


###

e.total<-summary %>%
  mutate(total =
           paste0(round(excess.T,0)," ",
                 "(95% CI"," ",round(excess.l,0),
                 " - ",
                 round(excess.u,0),")")) %>%
  dplyr::select(total)

e.total

###

total<-summary %>%
  mutate(total =
           paste0(round(total,0)," ",
                     "(95% CI"," ",round(total.l,0),
                 " - ",
                 round(total.u,0),")")) %>%
  dplyr::select(total)

total

####


excess.pct.t<-summary %>% ungroup %>%
  mutate(pct.t=(total-192215)/192215*100,
         pct.l=(total.l-192215)/192215*100,
         pct.u=(total.u-192215)/192215*100,
        ex.pct =
           paste0(round(pct.t,1),
                 "% (95% CI"," ",round(pct.l,1),
                 "% - ",
                 round(pct.u,1),"%)")) %>%
  dplyr::select(ex.pct)

excess.pct.t



###

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
  mutate(FECHA = as.Date(FECHA_FALLECIMIENTO, "%Y-%m-%d")) %>%
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



####

#
# lastday <- fallecidos_covid.test %>%
#   mutate(date = ymd(FECHA)) %>%/
#   filter(date == max(date)) %>%
#   dplyr::select(FECHA) %>%
#   distinct()
#
# last.day <-  as.character(format(lastday, "%B %d %Y"))

##

