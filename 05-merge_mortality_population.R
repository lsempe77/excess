
#### EXPECTED MORTALITY ####

#### Compute and use exposure ratio deaths by age from registered mortality 2018 SINADEF (no better source) ####

mortality.rate.age.sinadef<-SINADEF  %>% filter (!is.na(range)) %>%
  group_by(AÑO, Departamento,range,.drop=F) %>% summarise(deaths=n()) %>%
  mutate(d=sum(deaths),prop=deaths/d)

mortality.rate.age.sinadef$AÑO<-as.numeric(mortality.rate.age.sinadef$AÑO)
colnames(mortality.rate.age.sinadef)[1]<-"AÑO"


mortality.rate.age.sinadef.mod<-mortality.rate.age.sinadef %>%
  filter(AÑO !=2020) %>%
  mutate(AÑO=dplyr::recode(AÑO,`2018`=2020, `2017`=2015))

colnames(mortality.rate.age.sinadef.mod)[4]<-"deaths.sin.mod"

##### WITHOUTH ARIMA POPULATION

#variable.names(compare.estim.age)

#variable.names(mortality.rate.age.sinadef.mod)
#table(mortality.rate.age.sinadef.mod$AÑO)

#mort.rate.regions.range.2010.2015.2020

combined.pop.mort <- compare.estim.age%>%
  left_join(mort.rate.regions.range.2010.2015.2020) %>%
  filter(Año!="2010-2014")%>%
  mutate(mort.estim.edades.f=pop.INEI*(crm/1000))

colnames(combined.pop.mort)[2]<-"range"



#### Merge forecast population, mortality ratio age and forecast expected mortality ####

#variable.names(mortality.rate.age.sinadef.mod)
#variable.names(fc_mort.reg.piece)
#variable.names(fc_edades.arima)

#fc_edades.arima <- fc_edades.arima%>%  left_join(mortality.rate.age.sinadef.mod,by=c("Departamento","range","year"="AÑO"))


#fc_mort.reg.piece.pop.edades<-fc_mort.reg.piece%>% as.data.frame()%>% left_join(fc_edades.arima, by=c("Departamento","Año"="year"))


#variable.names(fc_mort.reg.piece.pop.edades)
#head(fc_mort.reg.piece.pop.edades)

#fc_mort.reg.piece.pop.edades<-fc_mort.reg.piece.pop.edades %>%  group_by(Departamento,Año,range) %>%  #  mutate( mort.estim.edades =((pred.tasa/1000)*pop.ARIMA),mort.estim.edades.ci.propag = abs((pred.tasa/1000)*pop.ARIMA)*sqrt(((sd.mr/1000)/(pred.tasa/1000))^2+                                                                          (sd.pop.estim/pop.ARIMA)^2),    mort.estim.edad.retro=((pred.tasa/1000)*pop.ARIMA))

#variable.names(fc_mort.reg.piece.pop.edades)


#fc_mort.reg.piece.pop.edades <- fc_mort.reg.piece.pop.edades %>% group_by(Departamento,Año,                                                                      model.mort,model.pop)%>%  mutate(sum.deaths=sum(mort.estim.edades),         sum.deaths.retro=sum(mort.estim.edad.retro),         sum.ci=sum(mort.estim.edades.ci.propag),         mort.estim.edades.f=prop*sum.deaths,         mort.estim.edad.retro.f=prop*sum.deaths.retro,     mort.estim.edades.ci.propag=sum.ci*prop) #*prop

#table(fc_mort.reg.piece.pop.edades$prop)

# chequear año de variables
#fc_mort.reg.piece.pop.edades%>%group_by(Año)%>%filter(Año >=2010)%>%summarise(  pred.tasa=mean(pred.tasa,na.rm = T),              pop.estim=mean(pop.estimated,na.rm = T), prop=mean(prop,na.rm = T),            tasacruda=mean(`Tasa Cruda`,na.rm = T), popul=mean(population,na.rm=T))

#variable.names(fc_mort.reg.piece.pop.edades)

#### Expected mortality count and CI by regions and age - 2020 ####

#estimated.mortality.region.edades.FINAL<-fc_mort.reg.piece.pop.edades%>%mutate(mort.propagate.lower=mort.estim.edades.f-1.96*sqrt(mort.estim.edades.ci.propag), mort.propagate.upper=mort.estim.edades.f+1.96*sqrt(mort.estim.edades.ci.propag))

#head(estimated.mortality.region.edades.FINAL)
#mort.estimat.nac.2005.2020.long


