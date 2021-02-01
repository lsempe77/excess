# ARIMA

SINADEF.proj <- SINADEF %>%
 mutate(week=case_when(FECHA == base::as.Date("2017-12-31") ~ 53,
            AÑO == 2017 & FECHA != "2017-12-31" ~ week,
            AÑO==2018 & FECHA ==  base::as.Date("2018-12-30")  ~ week+104,
            AÑO==2018 & FECHA ==  base::as.Date("2018-12-31")  ~ week+104,
            AÑO==2018 & (FECHA != "2018-12-30" | FECHA != "2018-12-31")  ~ week+52,
            FECHA == "2019-12-29" | FECHA == "2019-12-30" |FECHA == "2019-12-31" ~ 157,
            AÑO==2019 & (FECHA != "2019-12-29" | FECHA != "2019-12-30"
                         | FECHA != "2019-12-31") ~ week+104,
            FECHA == "2019-12-29" | FECHA == "2019-12-30" |FECHA == "2019-12-31" ~ 157,
            AÑO== 2020 ~ week+156))  %>%
  dplyr::group_by(Departamento,week) %>%
  summarise(deaths=n())


SINADEF.proj <-as_tsibble(SINADEF.proj,index=week,
                          key=c("Departamento"))


fit.sinadef <- SINADEF.proj %>%
  filter( week <  158) %>%
  tsibble::fill_gaps()%>%
  model(
    arima = ARIMA(deaths,approximation = F))

fc_sinadef <- fit.sinadef %>% forecast(h=11) %>% hilo(95)

proj.sinadef2020<-fc_sinadef %>%  unpack_hilo("95%") %>%
  as_tibble() %>%
  group_by(Departamento) %>%
  summarise (proj2019=sum(.mean,na.rm = T),
             proj2019.l=sum(`95%_lower`,na.rm=T),
             proj2019.u=sum(`95%_upper`,na.rm = T))


sinadef.baseline<-SINADEF.proj %>%
  filter(week > 157 & week < 169) %>%
  as_tibble() %>% group_by(Departamento) %>%
  summarise (deaths1718=sum(deaths))

proj.sinadef2020 <- proj.sinadef2020 %>% full_join(sinadef.baseline) %>%
  mutate (proj2019.adj = proj2019*(1+(.0179*(12/52))),
          perc.change.o = ((deaths1718 - proj2019.adj)/proj2019),
          perc.change = case_when(perc.change.o < 0 ~ 0,
                                  T ~ perc.change.o),
          proj2019.adj.l = proj2019.l*(1+(.0179*(12/52))),
          perc.change.o.l = ((deaths1718 - proj2019.l)/proj2019.l),
          perc.change.l = case_when(perc.change.o.l < 0 ~ 0,
                                    T ~ perc.change.o.l),
          proj2019.adj.u = proj2019.u*(1+(.0179*(12/52))),
          perc.change.o.u = ((deaths1718 - proj2019.u)/proj2019.u),
          perc.change.u = case_when(perc.change.o.u < 0 ~ 0,
                                    T ~ perc.change.o.u))


#proj.sinadef2020[,c(1,8,11,14)]%>% arrange(perc.change)%>%print(n=25)

proj.sinadef2020$deaths1718<- as.numeric(proj.sinadef2020$deaths1718)

growth.rate.system<-proj.sinadef2020 %>%
  group_by(Departamento)%>%
  mutate (proj2019.adj.b = case_when(proj2019.adj > deaths1718 ~ deaths1718,
                                     T ~ proj2019.adj)) %>%
  ungroup()%>%
  summarise(proj2019.adj.b = sum(proj2019.adj.b,na.rm = T),
            deaths1718 = sum(deaths1718,na.rm = T),
            perc.change2 = ((deaths1718 - proj2019.adj.b)/proj2019.adj.b))

# GROWTH ARIMA

# growth.rate.system

####

fallecidos_covid.sema<-fallecidos_covid %>%
  mutate (FECHA_FALLECIMIENTO=as.Date(FECHA_FALLECIMIENTO,"%Y.%m.%d"),
          week=epiweek(FECHA_FALLECIMIENTO)) %>% group_by(DEPARTAMENTO,week)%>%
  summarise(Covid_deaths1= n())

#### Based on VEGA and differences PRE-POST PANDEMICS

SINADEF.test.weeks <- SINADEF %>%
  filter(AÑO==2019 | AÑO==2020)%>%
  mutate(week=case_when(FECHA == "2019-12-29" | FECHA == "2019-12-30" |FECHA == "2019-12-31" ~ 53,
                        AÑO==2019 & (FECHA != "2019-12-29" | FECHA != "2019-12-30"
                                     | FECHA != "2019-12-31") ~ week,
                        AÑO== 2020 ~ week+52))  %>%
  dplyr::group_by(FECHA,week) %>%
  summarise(deaths=n())

SINADEF.test.weeks %>% filter(FECHA>="2020-01-01")
SINADEF.test.weeks %>% filter(week>65)


SINADEF.proj.end.d <- SINADEF %>%
  filter(AÑO==2019 | AÑO==2020)%>%
  mutate(week=case_when(FECHA == "2019-12-29" | FECHA == "2019-12-30" |FECHA == "2019-12-31" ~ 53,
                        AÑO==2019 & (FECHA != "2019-12-29" | FECHA != "2019-12-30"
                                     | FECHA != "2019-12-31") ~ week,
                        AÑO== 2020 ~ week+52))  %>%
    dplyr::group_by(Departamento,week) %>%
  summarise(deaths=n())


SINADEF %>%
  filter(AÑO==2019 | AÑO==2020)%>%
  mutate(week=case_when(FECHA == "2019-12-29" | FECHA == "2019-12-30" |FECHA == "2019-12-31" ~ 53,
                        AÑO==2019 & (FECHA != "2019-12-29" | FECHA != "2019-12-30"
                                     | FECHA != "2019-12-31") ~ week,
                        AÑO== 2020 ~ week+52))  %>%
  dplyr::group_by(FECHA,week) %>%
  summarise(deaths=n()) %>% filter (FECHA>="2020-03-15")



time<-SINADEF.proj.end.d %>%
  group_by(Departamento)%>%
  do(its.covid = memtiming(i.data=.[3],i.method = 1,
                           i.param =-1,
                           i.n.values = 63
  )) %>%pull(its.covid)

ta2 <- as.data.frame(do.call(rbind, lapply(time, as.vector)))

ta3<-as.data.frame(t(do.call(cbind, lapply(ta2$optimum.map, as.data.frame))))

Departamento<-unique(SINADEF.proj.end.d$Departamento)

epi1<-cbind(Departamento,ta3[4:5])


time2<-SINADEF.proj.end.d %>%
  group_by(Departamento)%>%
  do(its.covid = memtiming(i.data=.[3],i.method = 2,
                           i.param = 1,i.n.values = 63
  )) %>%pull(its.covid)

ta22 <- as.data.frame(do.call(rbind, lapply(time2, as.vector)))

ta32<-as.data.frame(t(do.call(cbind, lapply(ta22$optimum.map, as.data.frame))))


epi2<-cbind(Departamento,ta32[4:5])

time3<-SINADEF.proj.end.d %>%
  group_by(Departamento)%>%
  do(its.covid = memtiming(i.data=.[3],i.method = 3,
                           i.n.values = 63
  )) %>%pull(its.covid)

ta23 <- as.data.frame(do.call(rbind, lapply(time3, as.vector)))

ta33<-as.data.frame(t(do.call(cbind, lapply(ta23$optimum.map, as.data.frame))))



epi3<-cbind(Departamento,ta33[4:5])


time4<-SINADEF.proj.end.d %>%
  group_by(Departamento)%>%
  do(its.covid = memtiming(i.data=.[3],i.method = 4,
                           i.n.values = 63
  )) %>%pull(its.covid)


ta24 <- as.data.frame(do.call(rbind, lapply(time4, as.vector)))

ta34<-as.data.frame(t(do.call(cbind, lapply(ta24$optimum.map, as.data.frame))))




epi4<-cbind(Departamento,ta34[4:5])

epi1$m<-"1"
epi2$m<-"2"
epi3$m<-"3"
epi4$m<-"4"

epitime.regions<-epi1 %>% full_join(epi2) %>%full_join(epi3) %>%full_join(epi4)

epitime.regions[28,3]<-104

SINADEF.proj.end.d %>%
  ggplot() + geom_line(aes(week,deaths))+
  geom_vline(data=subset(epitime.regions,m=="2"),aes(xintercept=V4),linetype=2,colour="darkred")+
  geom_vline(data=subset(epitime.regions,m=="2"),aes(xintercept=V5),linetype=2,colour="darkgreen")+
facet_wrap(~Departamento,scales="free") + theme_cowplot()+xlab("Weeks since year 2019")


####

fallecidos_covid.sema.range<-fallecidos_covid %>%
  mutate (FECHA_FALLECIMIENTO=as.Date(FECHA_FALLECIMIENTO,"%Y.%m.%d"),
          week=epiweek(FECHA_FALLECIMIENTO)) %>%
  group_by(DEPARTAMENTO,week)%>%
  summarise(Covid_deaths1= n())

#epitime.regions$V4<-epitime.regions$V4-52
#epitime.regions$V5<-epitime.regions$V5-52




registration.adjustement <- SINADEF %>% filter(AÑO=="2020") %>%
  mutate(FECHA = as.Date(FECHA, "%Y-%m-%d"),
         week=epiweek(FECHA)) %>%
  group_by(Departamento,week)%>%
  summarise(deaths=n()) %>%
  left_join(epitime.regions) %>%
  left_join(fallecidos_covid.sema.range,
            by=c("Departamento"="DEPARTAMENTO","week")) %>%
  filter (m=="2") %>%
  mutate (weeks.pre= V4-52,
          weeks.post=V5-52,
                  deaths.pre.pandemic = case_when(week<weeks.pre ~ deaths,
                                                  T ~ NA_integer_),
                  deaths.post.pandemic = case_when(weeks.post<week ~ deaths,
                                                   T ~ NA_integer_),
                  covid.post = case_when(weeks.post<week ~ Covid_deaths1,
                                         T ~ NA_integer_),
                  deaths.during = case_when(week+1 > weeks.pre ~ deaths,
                                            T ~ NA_integer_),
                  deaths.during  = case_when(week > weeks.post ~ NA_integer_,
                                             T ~ deaths.during)) %>%
  group_by(Departamento) %>%
  summarise(deaths.pre.pandemic1=sum(deaths.pre.pandemic,na.rm = T)/mean(weeks.pre),
            deaths.post.pandemic1=(sum(deaths.post.pandemic,na.rm = T)-sum(covid.post,na.rm = T))/(53.14-mean(weeks.post)),
            weeks.post=mean(weeks.post,na.rm=T),
            deaths.during1=sum(deaths.during,na.rm = T)/(mean(weeks.post)-mean(weeks.pre)+1),
            weeks.post=mean(weeks.post,na.rm=T),
            weeks.pre=mean(weeks.pre,na.rm=T)) %>%
  mutate(deaths.pre.pandemic1=deaths.pre.pandemic1,
         difference=deaths.post.pandemic1-deaths.pre.pandemic1,
         perc.dif=difference/deaths.pre.pandemic1) %>%
  left_join(sub.reg) %>%
  mutate (post=weeks.post*difference,
          during=(deaths.during1)*(mean(weeks.post)-mean(weeks.pre)+1),
          during.post=(post+during)*(1-sub.mean)*(1-perc.dif)*(1-.0179))


# registration.adjustement %>%
#   summarise (s=mean(perc.dif,na.rm = T)) %>% print (n=25)
#


# registration.adjustement %>% arrange (-during.post) %>%
#   dplyr::select(-c(deaths.pre.pandemic1,deaths.post.pandemic1,weeks.post)) %>%
#   print (n=300)

###
#
#  registration.adjustement %>% ungroup()%>%
#    summarise (s=sum(during.post,na.rm = T))
# #
#
# registration.adjustement %>% ungroup()%>%
#   summarise (s=sum(during+post,na.rm = T))
#

######



###### BASED ON STANDARD CUTOFF OF PANDEMIC FOR WHOLE COUNTRY


pre<-SINADEF %>% group_by(Departamento)%>%
  #filter(Departamento=="LAMBAYEQUE") %>%
  mutate(FECHA = as.Date(FECHA, "%Y-%m-%d")) %>%
  filter ( FECHA >= "2020-01-12" & FECHA<="2020-03-14" ) %>%
  summarise(deaths.pre=n()*(1+(.0179)),
            ave.deaths=(deaths.pre/63))

post<-SINADEF %>%group_by(Departamento)%>%
  # filter(Departamento=="LAMBAYEQUE") %>%
  mutate(FECHA = as.Date(FECHA, "%Y-%m-%d")) %>%
  filter ( FECHA >= "2020-10-25" & FECHA<="2020-12-26" ) %>%
  summarise(deaths.p=n(),
            ave.deaths.p=deaths.p/63)

fallecidos_covid.adj<-fallecidos_covid %>%group_by(DEPARTAMENTO)%>%
  mutate (FECHA_FALLECIMIENTO=as.Date(FECHA_FALLECIMIENTO,"%Y.%m.%d")) %>%
  filter(FECHA_FALLECIMIENTO >= "2020-10-25" &
           FECHA_FALLECIMIENTO<="2020-12-26") %>%
  summarise(Covid_deaths1= n(),
            ave.covid=Covid_deaths1/63)

proj.sinadef2020<-proj.sinadef2020 %>%
  dplyr::select(Departamento,perc.change,perc.change.o) %>%
  left_join(pre) %>%
  left_join(post) %>%
  left_join(fallecidos_covid.adj,by=c("Departamento"="DEPARTAMENTO")) %>%
  mutate (growth=(((ave.deaths.p-ave.covid)-ave.deaths)/ave.deaths),
          real.growth=case_when(perc.change.o< 0 ~ abs(1-perc.change.o)*growth*.62,
                                T ~ growth*.62))%>%
  mutate(perc.change=growth)

##

# proj.sinadef2020 %>%
#   left_join(pre) %>%
#   left_join(post) %>%
#   left_join(fallecidos_covid.adj,by=c("Departamento"="DEPARTAMENTO","range")) %>%
#   mutate (growth=(((deaths.p-Covid_deaths1)-deaths.pre))) %>%
#   dplyr::select(Departamento,perc.change.o,growth) %>%
#   left_join(sub.reg)%>%  arrange(-growth) %>%
#   print (n=25) %>%
#   ggplot (aes(sub.mean,growth,label = Departamento)) + geom_point() +
#   geom_vline(xintercept = 0,linetype=2)+geom_hline(yintercept = 0,linetype=2)+
#   geom_label_repel()+xlab("Growth pre-post pandemics (%)")+
#   ylab("Growth until March 2020 compared to 2017-2020 (ARIMA)")



###
