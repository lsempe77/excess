target <- c("U071", "U072","B342","B972","coronavirus", "cov-2", "cov2", "covid", "sars")

SINADEF.causes <- SINADEF %>%
  mutate(week=ISOweek(FECHA),
         Date = ISOweek2date(paste0(week, "-7"))) %>%
  filter(week != "2016-W52" &  Date < "2021-01-01") %>% #2021-01-01
  filter (    `CAUSA A (CIE-X)` %in% target |
                `CAUSA B (CIE-X)` %in% target |
                `CAUSA C (CIE-X)` %in% target |
                `CAUSA D (CIE-X)` %in% target ) %>%
  group_by(Departamento, range, Date) %>%
  summarise(deaths.covid=n())

SINADEF.causes<- SINADEF.causes %>%
  as_tsibble(key = c(Departamento, range), index = Date) %>%
  fill_gaps(.full = TRUE)

SINADEF.causes<-SINADEF.causes %>% as_tibble() %>%
  mutate (deaths.covid = case_when (is.na(deaths.covid) ~ 0,
                                    T ~ as.numeric(deaths.covid)))


SINADEF.proj.t <- SINADEF %>%
  mutate(week=ISOweek(FECHA),
         Date = ISOweek2date(paste0(week, "-7"))) %>%
  filter(week != "2016-W52" &  Date < "2021-01-01") %>%#
  group_by(Departamento,range,week,Date) %>%
  summarise(deaths=n())

SINADEF.proj.t<- SINADEF.proj.t %>%
  as_tsibble(key = c(Departamento, range), index = Date) %>%
  fill_gaps(.full = TRUE)


SINADEF.proj.t<-SINADEF.proj.t %>% as_tibble() %>%
  mutate (deaths = case_when (is.na(deaths) ~ 0,
                                               T ~ as.numeric(deaths))) %>%
    group_by(Departamento,range) %>% mutate(week2=1:n())%>%
   left_join(epitime.regions) %>%
  mutate(covid = case_when(week2 >= start ~ 1,
                           T ~ 0)) %>%
  filter(!is.na(range)) %>%
  ungroup()


SINADEF.causal4.t <- SINADEF.proj.t%>%
  group_by(Departamento,range) %>%
  filter(week2 >= start) %>%
  summarise(tot=sum(deaths,na.rm = T))

SINADEF.proj.t$fourier <- harmonic(SINADEF.proj.t$week2,
                                   nfreq=6,period=52.18) # annual cycle and harmonics


pop.reg.ed.t<-pob.regiones.group.age.sex.2020 %>%rename(range=name)%>%
  group_by(Departamento,range)%>%
  summarise(population=sum(population,na.rm=T))

SINADEF.proj.t <- SINADEF.proj.t %>% left_join(pop.reg.ed.t)

#


# glimpse(SINADEF.proj.t)

aaa<- SINADEF.proj.t %>%
  group_by(Departamento,range,.drop=F) %>%
  nest() %>%
  mutate(model = data %>%
           map(~glm(deaths ~ ns(week2,3) +
                      fourier+
                      covid+
                      offset(log(population)),
                    family = quasipoisson,
                    na.action = na.exclude,
                    data = .))) %>%
  mutate(res=map2(model,data,residuals,type="deviance")) %>%
  unnest(c(data,res))

# aaa$covid<-as.factor(aaa$covid)
# aaa$Departamento<-as.factor(aaa$Departamento)
# aaa$range<-as.factor(aaa$range)
# aaa$res<-as.numeric(aaa$res)


fit<-aaa %>%
  group_by(Departamento,range) %>%
  do(its.covid = glance(glm(deaths ~ ns(week2,3)+fourier+
                              covid+lag(res,1)+
                              offset(log(population)),
                            family = quasipoisson,
                            data=.))) %>%
  unnest(cols = its.covid) %>%
  mutate(fit = 1 - (deviance/null.deviance),
         dif.dev=null.deviance-deviance,
         df=df.null-df.residual,
         p= pchisq(dif.dev, df, lower.tail=FALSE))

#ggplot(fit)+geom_histogram(aes(fit),bins = 60)
#ggplot(fit)+geom_histogram(aes(p),bins = 60)


#H0: 'the model as a whole is no better than the null model'.
#' Since this has been rejected (p<.05), we conclude that the data are not
#' consistent with the null model.

#
#
#
# tt <- aaa %>% left_join(SINADEF.causes) %>%
#   mutate(year = lubridate::year(Date)) %>% filter(year != 2017) %>%
#   group_by(Departamento,range,week2) %>%
#   summarise    (m =  case_when (week2 < start ~ mean(deaths,na.rm=T)),
#                 m2 = case_when (week2 >= end ~ mean(deaths,na.rm=T))) %>%
#   group_by(Departamento,range) %>%
#   summarise(mean.prev = mean(m,na.rm=T),
#             mean.post = mean(m2,na.rm=T),
#             Change_registration = ((mean.post-mean.prev)/mean.prev)*.5)
#

###

its.covid.list4d.t <- aaa %>% group_by(Departamento,range) %>%
  do(its.covid = tidy(coeftest(glm(deaths ~ ns(week2,3) +
                                     fourier+covid+lag(res,1)+#lag(deaths,1)+
                                     offset(log(population)),
                                   family=quasipoisson,
                                   data=.)))) %>%
  unnest(cols = its.covid) %>%
  mutate(low=exp((estimate)-(1.96*std.error)),
         up=exp((estimate)+(1.96*std.error))) %>%
 left_join (SINADEF.causal4.t) %>%
  mutate(excess=(exp(estimate)-1)/exp(estimate)*tot)

e17.t<-its.covid.list4d.t %>%
  filter(term=="covid") %>%
  mutate(excess.low=(low-1)/low*tot,
         excess.up=(up-1)/up*tot)


fallecidos_covid.t<- fallecidos_covid %>%
  mutate (FECHA_FALLECIMIENTO = as.Date(FECHA_FALLECIMIENTO,
                                        "%Y.%m.%d"),
          week = epiweek(FECHA_FALLECIMIENTO)) %>%
  filter(FECHA_FALLECIMIENTO<"2021-01-01") %>%
  group_by(DEPARTAMENTO,range) %>% rename(Departamento=DEPARTAMENTO) %>%
  summarise(Covid_deaths= n())

sub.reg <- sub.reg %>% group_by(Departamento) %>%
  mutate (sub.mean=case_when(Departamento=="LAMBAYEQUE" ~ mean(sub.mean),
                             T ~ sub.mean))

sub.reg %>% filter (Departamento== "LAMBAYEQUE") %>% select(AÑO,sub.mean)

sub.reg %>% filter (Departamento== "AMAZONAS") %>% select(AÑO,sub.mean)

FINAL5.t<-e17.t%>%
  left_join(fallecidos_covid.t)%>%
  left_join(sub.reg) %>%
  group_by(Departamento) %>%
  #mutate(sub.mean=max(sub.mean)) %>%
  filter (AÑO=='2019')%>%
  filter (!is.na(sub.mean))%>%
  mutate(Covid_deaths = ifelse(is.na(Covid_deaths), 0, Covid_deaths))


tateti.t<-FINAL5.t%>%
  group_by(Departamento,
           range,
           sub.mean,
           #mort.estim.edades.f,
           Covid_deaths,p.value) %>%
  summarise(excess_deaths.sum=mean(excess),
            excess.low=mean(excess.low),
            excess.up=mean(excess.up))

tateti6.t<- tateti.t %>%
  group_by(Departamento,range)%>%
  mutate(
    excess_deaths.sum = case_when(p.value >.05 ~ 0,
                                  T ~ excess_deaths.sum),
    excess.low = case_when(p.value >.05 ~ 0,
                           T ~ excess.low),
    excess.up = case_when(p.value >.05 ~ 0,
                          T ~ excess.up),
    excess.low = case_when(excess.low > excess_deaths.sum ~ 0,
                           T ~ excess.low),
    complete.covid = case_when(Covid_deaths > excess_deaths.sum ~
                                 (Covid_deaths-excess_deaths.sum)),
    excess.total.mean = case_when(excess_deaths.sum > 0 ~
                                     (excess_deaths.sum + !is.na(complete.covid)) +
                                    ((excess_deaths.sum - !is.na(complete.covid)) * ((1/sub.mean)-1)),
                                  T ~ excess_deaths.sum + complete.covid),
    excess.total.low = case_when(Covid_deaths > excess.low ~ Covid_deaths,
                                 T ~ excess.low +
                                   ((excess.low - !is.na(Covid_deaths)) * ((1/sub.mean)-1)) +
                                   !is.na(Covid_deaths)),
    excess.total.up = case_when(Covid_deaths > excess.up ~ Covid_deaths,
                                T ~ excess.up +
                                  ((excess.up - !is.na(Covid_deaths))*((1/sub.mean)-1))+
                                  !is.na(Covid_deaths)),
    excess.total.mean = case_when(excess.total.mean < excess.total.low ~
                                    excess.total.low,
                                  T ~ excess.total.mean),
    excess.total.mean = case_when(
      Covid_deaths > excess.total.mean ~
        Covid_deaths,
      T ~ excess.total.mean))

t5.t<-tateti6.t %>% ungroup() %>%
  summarise(excess.registered.Naive=sum(excess_deaths.sum,na.rm = T),
            excess.low=sum(excess.low,na.rm = T),
            excess.up=sum(excess.up,na.rm = T),
            excess=sum(excess.total.mean,na.rm = T),
            excess.l=sum(excess.total.low,na.rm = T),
            excess.u=sum(excess.total.up,na.rm = T))

t5.t


tateti6.t%>%group_by(range) %>%
  summarise(`Excess registered (ER)`=sum(excess_deaths.sum,na.rm = T),
            `ER - Lower 95% CI`=sum(excess.low,na.rm = T),
            `ER - Upper 95% CI`=sum(excess.up,na.rm = T),
            `Excess total` = sum(excess.total.mean,na.rm = T),
            `Excess total low` = sum(excess.total.low,na.rm = T),
            `Excess total up` = sum(excess.total.up,na.rm = T)) %>%
  adorn_totals()


tateti6.t%>%group_by(Departamento) %>%
  summarise(`Excess registered (ER)`=sum(excess_deaths.sum,na.rm = T),
            `ER - Lower 95% CI`=sum(excess.low,na.rm = T),
            `ER - Upper 95% CI`=sum(excess.up,na.rm = T),
            `Excess total` = sum(excess.total.mean,na.rm = T),
            `Excess total low` = sum(excess.total.low,na.rm = T),
            `Excess total up` = sum(excess.total.up,na.rm = T)) %>%
  adorn_totals()


#

sub.reniec<-Defunciones_reniec %>% pivot_longer(`2017`:`2019`) %>%
  rename(AÑO=name,mort_RENIEC=value) %>%
  left_join(sub.reg) %>% left_join(sina) %>% filter(!is.na(deaths.SINADEF)) %>%
  mutate(predict.SINADEF=deaths.SINADEF*(1/(sub.mean)),
         dif_RENIEC_predicted=mort_RENIEC/predict.SINADEF,
         sub.RENIEC=deaths.SINADEF/mort_RENIEC,
         testRENIEC=deaths.SINADEF*(1/sub.RENIEC)) %>% select(Departamento,AÑO,sub.RENIEC)

sub.reniec



FINAL5.t.reniec<-e17.t%>%
  left_join(fallecidos_covid.t)%>%
  left_join(sub.reniec) %>% group_by(Departamento) %>%
  mutate(sub.mean=max(sub.RENIEC)) %>% filter (AÑO=="2019")%>%
  mutate(Covid_deaths = ifelse(is.na(Covid_deaths), 0, Covid_deaths))



tateti.t.reniec<-FINAL5.t.reniec%>%
  group_by(Departamento,
           range,
           sub.mean,
           #mort.estim.edades.f,
           Covid_deaths,p.value) %>%
  summarise(excess_deaths.sum=mean(excess),
            excess.low=mean(excess.low),
            excess.up=mean(excess.up))

tateti6.t.reniec<- tateti.t.reniec %>% #left_join(tt) %>%
  group_by(Departamento,range)%>%
  mutate(
    excess_deaths.sum = case_when(p.value >.05 ~ 0,
                                  T ~ excess_deaths.sum),
    excess.low = case_when(p.value >.05 ~ 0,
                           T ~ excess.low),
    excess.up = case_when(p.value >.05 ~ 0,
                          T ~ excess.up),
    excess.low = case_when(excess.low > excess_deaths.sum ~ 0,
                           T ~ excess.low),
    complete.covid = case_when(Covid_deaths > excess_deaths.sum ~
                                 (Covid_deaths-excess_deaths.sum)),
    excess.total.mean = case_when(excess_deaths.sum > 0 ~
                                    (excess_deaths.sum + !is.na(complete.covid)) +
                                    ((excess_deaths.sum - !is.na(complete.covid)) * ((1/sub.mean)-1)),
                                  T ~ excess_deaths.sum + complete.covid),
    excess.total.low = case_when(Covid_deaths > excess.low ~ Covid_deaths,
                                 T ~ excess.low +
                                   ((excess.low - !is.na(Covid_deaths)) * ((1/sub.mean)-1)) +
                                   !is.na(Covid_deaths)),
    excess.total.up = case_when(Covid_deaths > excess.up ~ Covid_deaths,
                                T ~ excess.up +
                                  ((excess.up - !is.na(Covid_deaths))*((1/sub.mean)-1))+
                                  !is.na(Covid_deaths)),
    excess.total.mean = case_when(excess.total.mean < excess.total.low ~
                                    excess.total.low,
                                  T ~ excess.total.mean),
    excess.total.mean = case_when(
      Covid_deaths > excess.total.mean ~
        Covid_deaths,
      T ~ excess.total.mean))

t5.t.reniec<-tateti6.t.reniec %>% ungroup() %>%
  summarise(excess.registered.Naive=sum(excess_deaths.sum,na.rm = T),
            excess.low=sum(excess.low,na.rm = T),
            excess.up=sum(excess.up,na.rm = T),
            excess=sum(excess.total.mean,na.rm = T),
            excess.l=sum(excess.total.low,na.rm = T),
            excess.u=sum(excess.total.up,na.rm = T))


tateti6.t.reniec %>% filter (Departamento== "LAMBAYEQUE") %>%
  summarise(`Excess registered (ER)`=sum(excess_deaths.sum,na.rm = T),
            `ER - Lower 95% CI`=sum(excess.low,na.rm = T),
            `ER - Upper 95% CI`=sum(excess.up,na.rm = T),
            `Excess total` = sum(excess.total.mean,na.rm = T),
            `Excess total low` = sum(excess.total.low,na.rm = T),
            `Excess total up` = sum(excess.total.up,na.rm = T)) %>%
  adorn_totals()


dif.reniec<-as.data.frame(((t5.t[1,4]-t5.t.reniec[1,4])/t5.t.reniec[1,4])*100)
dif.reniec.l<-as.data.frame(((t5.t[1,5]-t5.t.reniec[1,5])/t5.t.reniec[1,5])*100)
dif.reniec.u<-as.data.frame(((t5.t[1,6]-t5.t.reniec[1,6])/t5.t.reniec[1,6])*100)

t5.t
t5.t.reniec



###

fit2 <- aaa %>% filter(range=="a80") %>%
  group_by(Departamento) %>%
  do (fit = glm(deaths ~ ns(week2,3) +
                  fourier+
                  lag (res,1) +
                  covid+
                  offset(log(population)) ,
                family = quasipoisson,
                data = .))

aaa2 <- aaa %>% mutate(covid=0)


fit1.aug <- aaa %>% filter(range=="a80") %>%
  group_by(Departamento) %>%
  nest() %>%
  full_join(fit2) %>%
  do(augment(.$fit[[1]], newdata = .$data[[1]],se_fit=T,type.predict = "response"))


fit2.aug <-  aaa2 %>% filter(range=="a80") %>%
  group_by(Departamento) %>%
  nest() %>%
  full_join(fit2) %>%
  do(augment(.$fit[[1]], newdata = .$data[[1]],se_fit=T,type.predict = "response"))

