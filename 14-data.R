#```{r lastdaycoviddeaths}

# smrd.q %>% print (n=26)

pm<-((smrd.q[226,4]-smrd.q[226,7])/smrd.q[226,4])*100
pml<-((smrd.q[226,5]-smrd.q[226,8])/smrd.q[226,5])*100
pmu<-((smrd.q[226,6]-smrd.q[226,9])/smrd.q[226,6])*100

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


age<-tateti6.t%>%group_by(range) %>%
  summarise(`Total excess (TE)`=sum(excess.total.mean,na.rm = T),
            `TE - Lower CI 95%`=sum(excess.total.low,na.rm = T),
            `TE - UpperCI 95%`=sum(excess.total.up,na.rm = T)) %>% adorn_totals()

age60t<-(sum(age[7:9,2])/age[10,2])*100
age60u<-(sum(age[7:9,3])/age[10,3])*100
age60l<-(sum(age[7:9,4])/age[10,4])*100

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
            total.u=sum(total.u,na.rm = T))


# pander(t(summary),
#        caption = "(\\#tab:summary) Table 1: Summary of estimations, Peru, 2020.",
#        style = 'rmarkdown',
#        split.table = Inf,
#        round=1)

###

a<-summary %>%
  mutate(`Excess registered deaths` =
           paste(round(excess.reg,0),"(",round(excess.reg.l,0),"-",round(excess.reg.u,0),")"),
         comp=round(100*(excess.reg/excess.T),1),
         comp.l=round(100*(excess.reg.l/excess.T),1),
         comp.u=round(100*(excess.reg.u/excess.T),1),
         `Completeness of CRVS deaths registration` = paste(comp,"%","(",comp.l,"% -",comp.u,"%)"),
        `Excess TOTAL mortality` = paste(round(excess.T,0),"(",round(excess.l,0),"-",round(excess.u,0),")"),
        `Counterfactual estimated deaths in 2020`=paste(round(count,0),"(",round(count.l,0),"-",round(count.u,0),")"),
        `Total estimated deaths in 2020`=paste(round(total,0),"(",round(total.l,0),"-",round(total.u,0),")")) %>%
  dplyr::select(`Excess registered deaths`,
                `Completeness of CRVS deaths registration`,
                `Excess TOTAL mortality`,
                `Counterfactual estimated deaths in 2020`,
                `Total estimated deaths in 2020`)

a<-as.data.frame(t(a))

a <- tibble::rownames_to_column(a, "VALUE")

colnames(a)[1] <- 'Terms'
colnames(a)[2] <- 'Estimates (95% CI)'


# pander(t(a), caption = "(\\#tab:summary) Table 1: Summary of estimations, Peru, 2020.",
#        style = 'rmarkdown',
#        split.table = Inf,
#        round=1)

###

ex.reg<-summary %>%
  mutate(`Total Excess REGISTERED mortality based on Poisson models by region and age group` =
           paste(round(excess.reg,0),"( 95% CI",round(excess.reg.l,0),"-",round(excess.reg.u,0),")"),
         ) %>%
  dplyr::select(`Total Excess REGISTERED mortality based on Poisson models by region and age group`)

covid.oficial<-fallecidos_covid.r.l %>% ungroup()%>%
  summarise(covid.oficial=sum(Covid_deaths))

ex.reg.covid<-summary %>% bind_cols(covid.oficial)%>%
  mutate(excess.oficial=100*(round(excess.reg,0)-covid.oficial)/covid.oficial,
         excess.oficial.l=100*(round(excess.reg.l,0)-covid.oficial)/covid.oficial,
         excess.oficial.u=100*(round(excess.reg.u,0)-covid.oficial)/covid.oficial,
         excess.oficial.t=paste(round(excess.oficial,0),"%",
                                "( 95% CI",round(excess.oficial.l,0),"% -",
                                round(excess.oficial.u,0),"%)"),
  ) %>%
  dplyr::select(excess.oficial.t)

###

ex.total.covid<-summary %>% bind_cols(covid.oficial)%>%
  mutate(excess.oficial=100*(round(excess.T,0)-covid.oficial)/covid.oficial,
         excess.oficial.l=100*(round(excess.l,0)-covid.oficial)/covid.oficial,
         excess.oficial.u=100*(round(excess.u,0)-covid.oficial)/covid.oficial,
         excess.oficial.t=paste(round(excess.oficial,0),"%",
                                "( 95% CI",round(excess.oficial.l,0),"% -",
                                round(excess.oficial.u,0),"%)"),
  ) %>%
  dplyr::select(excess.oficial.t)


###

lima.total <- paste(round(limat,0),"( 95% CI", round(limal,0), "-", round(limau,0), ")")

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
                                     distinct(r)

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
  summarise(complete.c = sum(complete.covid))


complete.covid.reg<-tateti6.t %>% group_by(Departamento) %>%
  summarise(complete.c = sum(complete.covid)) %>% top_n(3)

complete.covid.reg

ayacucho.covid<-complete.covid.reg[1,2]
cax.covid<-complete.covid.reg[2,2]
puno.covid<-complete.covid.reg[3,2]


#
ex.unreg<-summary %>% bind_cols(complete.covid) %>%
  mutate(ex.un = excess.T - excess.reg  - complete.c,
         ex.un.l = excess.l - excess.reg.l - complete.c,
         ex.un.u = excess.u - excess.reg.u - complete.c,
         unreg =
           paste(round(ex.un,0),
                 "deaths",
                 "( 95% CI",round(ex.un.l,0),
                 "-",
                 round(ex.un.u,0),")")) %>%
  dplyr::select(unreg)

#####

ex.unreg.pc<-summary %>%
  mutate(ex.un = (excess.T - excess.reg)/excess.T,
         ex.un.l = (excess.T - excess.reg.l)/excess.T,
         ex.un.u = (excess.T - excess.reg.u)/excess.T,
         unreg.pct =
           paste(round(ex.un,3)*100,
                 "%",
                 "( 95% CI",round(ex.un.u,2)*100,
                 "% -",
                 round(ex.un.l,3)*100,"%)")) %>%
  dplyr::select(unreg.pct)

ex.unreg.pc

####


unreg.more<-as.data.frame(reg %>% group_by(Departamento) %>%
                        summarise (ex=sum(excess.T-excess.reg,na.rm = T)) %>% top_n(3))


unreg.more

lima.more<-unreg.more[2,2]
piura.more<-unreg.more[1,2]
lamba.more<-unreg.more[3,2]


###

e.total<-summary %>%
  mutate(total =
           paste(round(excess.T,0),
                 "( 95% CI",round(excess.l,0),
                 "-",
                 round(excess.u,0),")")) %>%
  dplyr::select(total)

e.total

###

total<-summary %>%
  mutate(total =
           paste(round(total,0),
                     "( 95% CI",round(total.l,0),
                 "-",
                 round(total.u,0),")")) %>%
  dplyr::select(total)

total

####

excess.pct.t<-summary %>% ungroup %>%
  mutate(pct.t=(excess.T-count)/count*100,
         pct.l=(excess.T-count.l)/count.l*100,
         pct.u=(excess.T-count.u)/count.u*100,
        ex.pct =
           paste(round(pct.t,1),
                 "% ( 95% CI",round(pct.u,1),
                 "% -",
                 round(pct.l,1),"%)")) %>%
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



####

#
# lastday <- fallecidos_covid.test %>%
#   mutate(date = ymd(FECHA)) %>%
#   filter(date == max(date)) %>%
#   dplyr::select(FECHA) %>%
#   distinct()
#
# last.day <-  as.character(format(lastday, "%B %d %Y"))
