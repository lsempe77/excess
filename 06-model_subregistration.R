### Subregitration

### Method Adair


####

sina<-SINADEF %>% group_by(Departamento,AÑO) %>% filter(AÑO == "2017" |AÑO == "2019")%>%
  summarise(deaths.SINADEF=n())

compare.estim.age<-pob.regiones.group.age.sex.2020%>% # 2020
  group_by(Departamento,name)%>%
  summarise(pop.INEI=sum(population,na.rm = T))

compare.estim.age$AÑO<-"2019"
colnames(compare.estim.age)[2]<-"range"

# compare.estim.age %>% ungroup() %>% summarise (s=sum(pop.INEI))

##

pob.regiones.grupos.ed.2005.2015.long<-pob.regiones.ed.2005.2015%>%
  arrange(Departamento,sheet) %>%
  mutate(a0.9 = rowSums(.[6:7]),
         a10.19 = rowSums(.[8:9]),
         a20.29 = rowSums(.[10:11]),
         a30.39 = rowSums(.[12:13]),
         a40.49 = rowSums(.[14:15]),
         a50.59 = rowSums(.[16:17]),
         a60.69  = rowSums(.[18:19]),
         a70.79 = rowSums(.[20:21]),
         a80 = rowSums(.[22]))%>%  dplyr::select(Departamento,sheet,a0.9:a80)%>%
  group_by(Departamento,sheet)%>%
  pivot_longer(a0.9:a80)

colnames(pob.regiones.grupos.ed.2005.2015.long)[2]<-"AÑO"
colnames(pob.regiones.grupos.ed.2005.2015.long)[3]<-"range"
colnames(pob.regiones.grupos.ed.2005.2015.long)[4]<-"pop.INEI"

pob.regiones.grupos.ed.2005.2015.long <-pob.regiones.grupos.ed.2005.2015.long %>%
  group_by(Departamento,AÑO,range) %>% summarise(pop.INEI=sum(pop.INEI))

pob.regiones.grupos.ed.2005.2015.long %>% group_by(AÑO) %>% summarise (s=sum(pop.INEI))

pob.regiones.grupos.ed.2005.2015.long<-pob.regiones.grupos.ed.2005.2015.long %>% filter(AÑO=="2015")

population.INEI.r<-compare.estim.age %>% full_join(pob.regiones.grupos.ed.2005.2015.long)


population.INEI <- population.INEI.r %>%
  mutate(AÑO=case_when(AÑO=="2015" ~ "2017",
                       T ~ "2019")) %>%
  filter(AÑO=="2017" | AÑO=="2019") %>% group_by(Departamento,AÑO)%>%
  summarise (pop.INEI=sum(pop.INEI))#2020 and 2017 population


###

# sina %>%left_join(population.INEI)



tot60<-pob.regiones.group.age.sex.2020%>%
  group_by(Departamento)%>% filter(name=="a60.69"|name=="a70.79"|name=="a80")%>%
  summarise(tot60= sum(population,na.rm = T))

tot<-pob.regiones.group.age.sex.2020%>%
  group_by(Departamento)%>%
  summarise(total= sum(population,na.rm = T))

tot60<-tot60 %>% left_join(tot)%>% mutate(fra60=tot60/total)
tot60$AÑO <- "2019"


##
# table(population.INEI.r$AÑO)

tot60.17<-population.INEI.r%>% filter(AÑO=="2015")%>%
  group_by(Departamento)%>% filter(range=="a60.69"|range=="a70.79"|range=="a80")%>%
  summarise(tot60= sum(pop.INEI))

tot17<-population.INEI.r%>% filter(AÑO=="2015")%>%
  group_by(Departamento)%>%
  summarise(total= sum(pop.INEI))



tot60.17<-tot60.17 %>% left_join(tot17)%>% mutate(fra60=tot60/total)

tot60.17$AÑO <- "2017"


tot60<- tot60 %>% full_join(tot60.17) #2020 and 2017 fraq 60


# sina %>%left_join(population.INEI) %>% left_join(tot60)

###

u5 <- read_excel("data/u5.xlsx",
                 col_types = c("text", "numeric", "text"))

Departamento<-c(unique(population.INEI$Departamento),unique(population.INEI$Departamento))

u5$Departamento<-Departamento

colnames(u5)[2]<-"crm.u5"

###

mort.rate.regions.range.2010.2015.2020 <- read_excel("data/tasas_mortalidad_regiones_INEI.xlsx")

mort.rate.regions.range.2010.2015.2020<- mort.rate.regions.range.2010.2015.2020%>%
  pivot_longer(cols = `2010`:`2015`)

colnames(mort.rate.regions.range.2010.2015.2020)[2]<-"year.range"
colnames(mort.rate.regions.range.2010.2015.2020)[3]<-"crm"

mort.rate.regions.range.2010.2015.2020$year.range[mort.rate.regions.range.2010.2015.2020$year.range=="2015"]<-"2015-2020"

mort.rate.regions.range.2010.2015.2020$year.range[mort.rate.regions.range.2010.2015.2020$year.range=="2010"]<-"2010-2014"

colnames(mort.rate.regions.range.2010.2015.2020)[2]<-"Año"

mort.rate <-
  mort.rate.regions.range.2010.2015.2020 %>%
  mutate (AÑO = case_when (Año == "2015-2020" ~ "2019",
                           T ~ "2017")) %>% dplyr::select(Departamento,crm, AÑO)

pob.regiones.under5<-pob.regiones.ed.2005.2015%>%
  arrange(Departamento,sheet) %>%
  mutate(u5 = rowSums(.[6]))%>%  dplyr::select(Departamento,sheet,u5)%>%
  group_by(Departamento,sheet) %>% filter (sheet=="2015") %>%
  mutate(sheet=case_when(sheet=="2015" ~ "2017"))

colnames(pob.regiones.under5)[2]<-"AÑO"

Poblacion2019u5 <- read_excel("data/Poblacion2019u5.xlsx",
                              col_types = c("text", "numeric", "text"))

pob.regiones.under5<-pob.regiones.under5 %>% full_join(Poblacion2019u5,
                                                       by=c("Departamento"="DEPARTAMENTO","AÑO","u5"))

comple5<-SINADEF %>% group_by(Departamento,AÑO) %>%
  filter(AÑO == "2017" |AÑO == "2019")%>%filter(EDAD<6)%>%
  summarise(deaths.SINADEF=n()) %>% left_join(u5) %>% left_join(pob.regiones.under5) %>%
  mutate (complete5 = deaths.SINADEF/((crm.u5/1000)*u5)) %>% dplyr::select(Departamento,AÑO,complete5) %>%
  mutate (complete5 =case_when(complete5 > 1 ~ 1,
                               T ~ complete5))

# comple5 %>% arrange(complete5)


pob.regiones.over60<-pob.regiones.ed.2005.2015%>%
  arrange(Departamento,sheet) %>%
  mutate(u5 = rowSums(.[6]))%>%  dplyr::select(Departamento,sheet,u5)%>%
  group_by(Departamento,sheet) %>% filter (sheet=="2015") %>%
  mutate(sheet=case_when(sheet=="2015" ~ "2017"))

colnames(pob.regiones.under5)[2]<-"AÑO"

###

gas <- read_excel("data/gas.xlsx",
                  col_types = c("text", "numeric", "text"))

###

adair<-sina %>%left_join(population.INEI) %>% left_join(tot60) %>% left_join(u5)%>%
  left_join(mort.rate) %>% left_join(comple5) %>% left_join(gas)


adair$Departamento<-as.factor(adair$Departamento)

adair$completeness<-adair$deaths.SINADEF/((adair$crm/1000)*adair$pop.INEI)


adair$completeness.reg<-adair$deaths.SINADEF/adair$pop.INEI

adair <- adair %>% mutate (completeness = case_when(completeness > 1 ~ .999,
                                                    T ~ completeness))


adair %>% arrange(completeness.reg)%>%
  filter(AÑO=="2019")%>% ggplot()+
  geom_point(aes(Departamento,completeness.reg))

adair$log.comp<-log(adair$completeness/(1-adair$completeness))




fit1<-lmer(log.comp ~ poly(completeness.reg,2)+complete5+AÑO+
             fra60+log(crm.u5)+GAS+(1|Departamento),data=adair)

test<-as.data.frame(predict(fit1,newdata=adair))


colnames(test)[1]<-"fitted"

adair2<-cbind(adair,test)

adair2$sub.mean<-exp(adair2$fitted)/(1+exp(adair2$fitted))

sub.reg<-adair2 #%>% #filter(AÑO == "2019" & Departamento!="LAMBAYEQUE") %>% dplyr::select(Departamento,sub.mean)

sub.reg.lamba<-adair2 %>% filter(AÑO == "2017" & Departamento=="LAMBAYEQUE") %>%
  dplyr::select(Departamento,sub.mean)

#sub.reg<-sub.reg %>% full_join(sub.reg.lamba)


# sub.reg<-adair2 %>% group_by(Departamento) %>%
#   summarise(sub.mean=max(sub.mean))

#
Defunciones_reniec<- read_excel("Defunciones_reniec.xlsx")

Defunciones_reniec %>% pivot_longer(`2017`:`2019`) %>%
rename(AÑO=name,mort_RENIEC=value) %>%
left_join(sub.reg) %>% left_join(sina) %>% filter(!is.na(deaths.SINADEF)) %>%
mutate(predict.SINADEF=deaths.SINADEF*(1/(sub.mean)),
dif_RENIEC_predicted=((mort_RENIEC-predict.SINADEF)/predict.SINADEF),
dif_perc_RENIEC_SINADEF=((mort_RENIEC-deaths.SINADEF)/deaths.SINADEF)) %>%
arrange (dif_perc_RENIEC_SINADEF) %>% select(-c(tot60,total,fra60,crm,
                                          crm.u5,complete5,GAS,log.comp,fitted,completeness.reg,
completeness,pop.INEI)) %>%
  filter (dif_perc_RENIEC_SINADEF<8) %>%
  ggplot () + geom_point(aes(dif_perc_RENIEC_SINADEF,dif_RENIEC_predicted))+
  scale_x_continuous(labels = scales::percent)+
  scale_y_continuous(labels = scales::percent)+
  facet_wrap(~AÑO)+ theme_clean()+
  geom_smooth(aes(dif_perc_RENIEC_SINADEF,dif_RENIEC_predicted),method = "lm")+
  geom_label_repel(aes(dif_perc_RENIEC_SINADEF,
                       dif_RENIEC_predicted,
                       label=Departamento),hjust=0, vjust=0)

Defunciones_reniec %>%  adorn_totals()

# reniec 2017 -> 150032 / 150032
# sinadef 2017 -> ~98000

# reniec 2019 -> 159706 / 157680
# sinadef 2019 -> ~114500
# poblacion -> 32 131 400
# predicted SINADEF - > 189991
# estimacion INEI -> 188043

# crude mortality rate reniec -> 4.97
# crude mortality rate predicted sinadef -> 5.91 = Inei 5.9 (estado de la poblacion actual 2020)



Defunciones_reniec %>% pivot_longer(`2017`:`2019`) %>%
  rename(AÑO=name,mort_RENIEC=value) %>%
  left_join(sub.reg) %>% left_join(sina) %>% filter(!is.na(deaths.SINADEF)) %>%
  mutate(predict.SINADEF=deaths.SINADEF*(1/(sub.mean)),
         dif_RENIEC_predicted=mort_RENIEC/predict.SINADEF,
         sub.RENIEC=deaths.SINADEF/mort_RENIEC,
         testRENIEC=deaths.SINADEF*(1/sub.RENIEC)) %>%
  select(-c(tot60,total,fra60,crm, crm.u5,complete5,GAS,log.comp,fitted,completeness.reg,
            completeness,pop.INEI)) %>% filter(AÑO=="2019") %>%
  adorn_totals()

Defunciones_reniec %>% pivot_longer(`2017`:`2019`) %>%
  rename(AÑO=name,mort_RENIEC=value) %>%
  left_join(sub.reg) %>% left_join(sina) %>% filter(!is.na(deaths.SINADEF)) %>%
  mutate(predict.SINADEF=deaths.SINADEF*(1/(sub.mean)),
         dif_RENIEC_predicted=mort_RENIEC/predict.SINADEF,
         sub.RENIEC=deaths.SINADEF/mort_RENIEC,
         testRENIEC=deaths.SINADEF*(1/sub.RENIEC)) %>%
     select(-c(tot60,total,fra60,crm, crm.u5,complete5,GAS,log.comp,fitted,completeness.reg,
                                                  completeness,pop.INEI)) %>%
  arrange(dif_RENIEC_predicted) %>%
  ggplot() +
  geom_vline(xintercept = 1,linetype=2,colour="darkorange")+
  geom_point(aes(sub.RENIEC,sub.mean))+
  scale_y_continuous(labels = scales::percent, breaks = seq(0,1,.2)) + theme_clean()+
    scale_x_continuous(labels = scales::percent, breaks = seq(0,1.6,.2))  +
  facet_wrap(~AÑO,scales="free")+
  geom_smooth(aes(sub.RENIEC,sub.mean),method = "lm")+
  labs(x="Completeness SINADEF/RENIEC", y= "Completeness SINADEF -Adair-")



cor.reniec<-Defunciones_reniec %>% pivot_longer(`2017`:`2019`) %>%
  rename(AÑO=name,mort_RENIEC=value) %>%
  left_join(sub.reg) %>% left_join(sina) %>% filter(!is.na(deaths.SINADEF)) %>%
  mutate(predict.SINADEF=deaths.SINADEF*(1/(sub.mean)),
         dif_RENIEC_predicted=mort_RENIEC/predict.SINADEF,
         sub.RENIEC=deaths.SINADEF/mort_RENIEC,
         testRENIEC=deaths.SINADEF*(1/sub.RENIEC)) %>% group_by(AÑO) %>%
   summarise(cora = cor.test(sub.mean,sub.RENIEC)$estimate,
              p_val = cor.test(sub.mean, sub.mean)$p.value)

cor17<-cor.reniec[1,2]
cor19<-cor.reniec[2,2]

Defunciones_reniec %>% pivot_longer(`2017`:`2019`) %>%
  rename(AÑO=name,mort_RENIEC=value) %>%
  left_join(sub.reg) %>% left_join(sina) %>% filter(!is.na(deaths.SINADEF)) %>%
  mutate(predict.SINADEF=deaths.SINADEF*(1/(sub.mean)),
         dif_RENIEC_predicted=((mort_RENIEC-predict.SINADEF)/predict.SINADEF),
         dif_perc_RENIEC_SINADEF=((mort_RENIEC-deaths.SINADEF)/deaths.SINADEF)) %>%
  arrange (dif_perc_RENIEC_SINADEF) %>% select(-c(tot60,total,fra60,crm,
                                                  crm.u5,complete5,GAS,log.comp,fitted,completeness.reg,
                                                  completeness,pop.INEI)) %>%
  filter (dif_perc_RENIEC_SINADEF<8) %>%
  ggplot () + geom_point(aes(Departamento,dif_perc_RENIEC_SINADEF,
                             colour="dif_perc_RENIEC_SINADEF"))+
  geom_point(aes(Departamento,dif_RENIEC_predicted,colour="dif_RENIEC_predicted"))+
    scale_y_continuous(labels = scales::percent)+ theme_clean()+
  scale_x_discrete(guide = guide_axis(n.dodge = 3))



####

adair2 %>% ggplot() + geom_point(aes(reorder(Departamento,sub.mean,mean),sub.mean,colour=AÑO))+
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  geom_point(data=sub.reg,aes(Departamento,sub.mean,colour="max"))

#
#
# sub.registr.edad  %>% filter (AÑO ==2019) %>% dplyr::select(Departamento,sub.mean) %>%
#   arrange (Departamento) %>%
#   dplyr::bind_cols(sub.reg) %>% mutate (dif=sub.mean...4-sub.mean...2) %>% arrange (-dif)%>%
#   print (n=25)













####
SINADEF.r<-SINADEF %>%
  group_by(AÑO,Departamento,na.rm = T)%>%
  summarise(deaths.sinadef.range.year.region=n())


###

combined.pop.mort.regions <- combined.pop.mort %>%
  group_by(Departamento,na.rm = T) %>%
            summarise(mort.estim.edades.f=sum(mort.estim.edades.f,na.rm = T))

sub.registr.edad <- SINADEF.r %>% mutate(AÑO=as.numeric(AÑO)) %>%
  filter (AÑO==2018 | AÑO==2019) %>%
  filter(Departamento!="LAMBAYEQUE")%>%
    left_join(combined.pop.mort.regions,
            by=c("Departamento"))%>%
  group_by(Departamento) %>%
    mutate(sub.mean=mean(deaths.sinadef.range.year.region,na.rm = T)/mean(mort.estim.edades.f,na.rm = T))

sub.registr.edad.l <- SINADEF.r %>% mutate(AÑO=as.numeric(AÑO)) %>%
  filter(Departamento=="LAMBAYEQUE")%>%
    filter (AÑO==2017)%>%
  left_join(combined.pop.mort.regions,
            by=c("Departamento"))%>%
  group_by(Departamento) %>%
  mutate(sub.mean=mean(deaths.sinadef.range.year.region,na.rm = T)/mean(mort.estim.edades.f,na.rm = T))

sub.registr.edad <- sub.registr.edad %>% full_join(sub.registr.edad.l)

###



