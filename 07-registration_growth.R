SINADEF.proj.end.d <-  SINADEF %>%
  mutate(week2=ISOweek(FECHA),
         Date = ISOweek2date(paste0(week2, "-7")))    %>%
  filter(week2 != "2016-W52" &  Date < "2021-01-01") %>% # 2021-01-01
  group_by(Departamento,week2,.drop=F) %>%
  summarise(deaths=n()) %>%  mutate(week = 1:n()) %>%
  ungroup ()

time<-SINADEF.proj.end.d %>%
  group_by(Departamento,.drop=F)%>%
  do(its.covid = memtiming(i.data=.[3],i.method = 1,
                           i.param =-1,
                           i.n.values = 150)) %>% pull(its.covid)

ta2 <- as.data.frame(do.call(rbind, lapply(time, as.vector)))

ta3<-as.data.frame(t(do.call(cbind, lapply(ta2$optimum.map, as.data.frame))))

Departamento<-unique(SINADEF.proj.end.d$Departamento)

epi1<-cbind(Departamento,ta3[4:5])

time2<-SINADEF.proj.end.d %>%
  group_by(Departamento,.drop=F)%>%
  do(its.covid = memtiming(i.data=.[3],i.method = 2,
                           i.param = .58,i.n.values = 1
  )) %>%pull(its.covid)

ta22 <- as.data.frame(do.call(rbind, lapply(time2, as.vector)))

ta32<-as.data.frame(t(do.call(cbind, lapply(ta22$optimum.map, as.data.frame))))


epi2<-cbind(Departamento,ta32[4:5])

time3<-SINADEF.proj.end.d %>%
  group_by(Departamento,.drop=F)%>%
  do(its.covid = memtiming(i.data=.[3],i.method = 3,
                           i.n.values = 150
  )) %>%pull(its.covid)

ta23 <- as.data.frame(do.call(rbind, lapply(time3, as.vector)))

ta33<-as.data.frame(t(do.call(cbind, lapply(ta23$optimum.map, as.data.frame))))



epi3<-cbind(Departamento,ta33[4:5])


time4<-SINADEF.proj.end.d %>%
  group_by(Departamento,.drop=F)%>%
  do(its.covid = memtiming(i.data=.[3],i.method = 4,
                           i.n.values = 150
  )) %>%pull(its.covid)


ta24 <- as.data.frame(do.call(rbind, lapply(time4, as.vector)))

ta34<-as.data.frame(t(do.call(cbind, lapply(ta24$optimum.map, as.data.frame))))

epi4<-cbind(Departamento,ta34[4:5])

epi1$m<-"1"
epi2$m<-"2"
epi3$m<-"3"
epi4$m<-"4"

epitime.regions<-epi1 %>% full_join(epi2) %>%full_join(epi3) %>%full_join(epi4)

epitime.regions <- epitime.regions %>% filter(m==2) %>% rename(type=m,
                                                               start=V4,
                                                               end=V5)

#

