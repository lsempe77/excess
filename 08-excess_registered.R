SINADEF.proj.t <- SINADEF %>%
  mutate(covid = case_when(FECHA > "2020-03-15" ~ 1,
                           T ~ 0)) %>%
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
  dplyr::group_by(AÑO,Departamento,range,week,covid) %>%
  summarise(deaths=n()) %>% #filter(week>53) %>%
  mutate(t = week,
         sn52 = sin((2*pi*t)/(52)),
         cs52 = cos((2*pi*t)/(52)),
         sn26 =sin((2*pi*t)/(26)),
         cs26 =cos((2*pi*t)/(26)),
         sn13 =sin((2*pi*t)/(13)),
         cs13 =cos((2*pi*t)/(13))) %>% filter(!is.na(range))

SINADEF.causal4.t <- SINADEF.proj.t%>%
  group_by(Departamento,range) %>%
  filter(week>167) %>%
  summarise(tot=sum(deaths))

SINADEF.proj.t$fourier <- harmonic(SINADEF.proj.t$t,nfreq=6,period=52.18) # annual cycle and harmonics

pop.reg.ed.t<-pob.regiones.group.age.sex.2020 %>%rename(range=name)%>%
  group_by(Departamento,range)%>%
  summarise(population=sum(population,na.rm=T))

SINADEF.proj.t <- SINADEF.proj.t %>% left_join(pop.reg.ed.t)

# glimpse(SINADEF.proj.t)

SINADEF.proj.t$AÑO<-as.factor(SINADEF.proj.t$AÑO)

SINADEF.proj.t$AÑO<-droplevels(SINADEF.proj.t$AÑO)

aaa<-SINADEF.proj.t %>%
  group_by(Departamento,range) %>%
  nest() %>%
  mutate(model = data %>%
           map(~glm(deaths ~ ns(t,df=3)+AÑO+
                                covid+
                      offset(log(population)),
                    family = quasipoisson,
                    na.action = na.exclude,
                    data = .))) %>%
  mutate(res=map2(model,data,residuals,type="deviance"),
         Pred = map2(model, data, predict)) %>%
  unnest(c(Pred, res,data))


fit<-aaa %>%
  group_by(Departamento,range) %>%
  do(its.covid = glance(glm(deaths ~ ns(t,3)+AÑO+
                              covid+
                              dplyr::lag(res, 1)+
                              offset(log(population)),
                            family = quasipoisson,
                            data=.))) %>%
  unnest(cols = its.covid) %>%
  mutate(fit = 1 - (deviance/null.deviance),
         dif.dev=null.deviance-deviance,
         df=df.null-df.residual,
         p= pchisq(dif.dev, df, lower.tail=FALSE))

ggplot(fit)+geom_histogram(aes(fit))
ggplot(fit)+geom_histogram(aes(p))

#H0: 'the model as a whole is no better than the null model'.
#' Since this has been rejected (p<.05), we conclude that the data are not
#' consistent with the null model.

###

its.covid.list4d.t <- aaa %>% group_by(Departamento,range) %>%
  do(its.covid = tidy(coeftest(glm(deaths ~  ns(t,3) + AÑO+
                                     covid+
                                     dplyr::lag(res, 1)+
                                     offset(log(population)),
                                   family = quasipoisson,
                                   data=.)))) %>%
  unnest(cols = its.covid) %>%
  mutate(low=exp(estimate)-1.96*std.error,
         up=exp(estimate)+1.96*std.error) %>%
  left_join (SINADEF.causal4.t) %>%
  mutate(excess=(exp(estimate)-1)/exp(estimate)*tot)



# its.covid.list4d.t

# its.covid.list4d.t %>% filter(term=="covid") %>%
#   mutate(excess=case_when(term!="covid" ~ NA_real_,
#                           T ~ excess),
#          excess.low=(low-1)/low*tot,
#          excess.up=(up-1)/up*tot) %>%
#   summarise(excess=sum(excess,na.rm = T),
#             excess.low=sum(excess.low,na.rm = T),
#             excess.up=sum(excess.up,na.rm = T))


e17.t<-its.covid.list4d.t %>%
  filter(term=="covid") %>%
  mutate(excess=case_when(term!="covid" ~ NA_real_,
                          T ~ excess),
         excess.low=(low-1)/low*tot,
         excess.up=(up-1)/up*tot)


fallecidos_covid.t<- fallecidos_covid %>%
  mutate (FECHA_FALLECIMIENTO = as.Date(FECHA_FALLECIMIENTO,
                                        "%Y.%m.%d"),
          week = epiweek(FECHA_FALLECIMIENTO)) %>%
  filter(FECHA_FALLECIMIENTO<"2021-01-01") %>%
  group_by(DEPARTAMENTO,range) %>% rename(Departamento=DEPARTAMENTO) %>%
  summarise(Covid_deaths= n())


FINAL5.t<-e17.t%>%
  left_join(fallecidos_covid.t)%>%
  left_join(sub.reg) %>%
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
  mutate(complete.covid = case_when(
    Covid_deaths > excess_deaths.sum ~
      (Covid_deaths-excess_deaths.sum),
    T ~ 0),
    excess_deaths.sum = case_when(
      p.value>=.05 ~ 0,
      T ~ excess_deaths.sum),
    excess.low = case_when(
      p.value>=.05 ~ 0,
      T ~ excess.low),
    excess.up = case_when(
      p.value>=.05 ~ 0,
      T ~ excess.up),
    sub.mean = case_when(
      sub.mean > 1 ~ 1,
      T ~ sub.mean)
  ) %>%
  mutate (
    excess.total.mean = excess_deaths.sum +
      (excess_deaths.sum)*(1-sub.mean)+
      complete.covid,
    excess.total.low = excess.low +
      (excess.low)*(1-sub.mean)+
      complete.covid,
    excess.total.up = excess.up  +
      (excess.up)*(1-sub.mean)+
      complete.covid)

t5.t<-tateti6.t %>% ungroup() %>%
  summarise(excess.registered.Naive=sum(excess_deaths.sum,na.rm = T),
            excess.low=sum(excess.low,na.rm = T),
            excess.up=sum(excess.up,na.rm = T),
            excess=sum(excess.total.mean,na.rm = T),
            excess.l=sum(excess.total.low,na.rm = T),
            excess.u=sum(excess.total.up,na.rm = T))


#
# ###
# aaa %>% filter(Departamento=="CALLAO") %>%
#   nest() %>%
#     mutate(model1 = data %>% map(~glm(deaths ~ # ns(t,3)+
#                                         AÑO+covid+
#    dplyr::lag(res, 1)+
#    offset(log(population)),
#    family = quasipoisson,
#    data = .)),
#    model2 = data %>% map(~glm(deaths ~  bs(t,3)+
#                                 AÑO+covid+
#                                 dplyr::lag(log(deaths), 1)+
#                                 offset(log(population)),
#                               family = quasipoisson,
#                               data = .)),
#    model3 = data %>% map(~glm(deaths ~  poly(t,3)+
#                                 AÑO+covid+
#                                 dplyr::lag(res, 1)+
#                                 offset(log(population)),
#                               family = quasipoisson,
#                               data = .)),
#    model4 = data %>% map(~glm(deaths ~  t+
#                                 AÑO+covid+
#                                 dplyr::lag(res, 1)+
#                                 offset(log(population)),
#                               family = quasipoisson,
#                               data = .)),
#    model5= data %>% map(~glm(deaths ~  ns(t,3)+
#                                 AÑO+covid+
#                                 dplyr::lag(res, 1)+fourier+
#                                 offset(log(population)),
#                               family = quasipoisson,
#                               data = .)),
#    model6 = data %>% map(~glm(deaths ~  bs(t,3)+
#                                 AÑO+covid+
#                                 dplyr::lag(res, 1)+fourier+
#                                 offset(log(population)),
#                               family = quasipoisson,
#                               data = .)),
#    model7 = data %>% map(~glm(deaths ~  poly(t,3)+
#                                 AÑO+covid+
#                                 dplyr::lag(res, 1)+fourier+
#                                 offset(log(population)),
#                               family = quasipoisson,
#                               data = .)),
#    model8 = data %>% map(~glm(deaths ~  t+
#                                 AÑO+covid+
#                                 dplyr::lag(res, 1)+fourier+
#                                 offset(log(population)),
#                               family = quasipoisson,
#                               data = .))) %>%
#    mutate(Pred1 = map2(model1, data, predict),
#           Pred2 = map2(model2, data, predict),
#           Pred3 = map2(model3, data, predict),
#           Pred4 = map2(model4, data, predict),
#           Pred5 = map2(model5, data, predict),
#           Pred6 = map2(model6, data, predict),
#           Pred7 = map2(model7, data, predict),
#           Pred8 = map2(model8, data, predict)) %>%
#    unnest(c(Pred1,Pred2, Pred3, Pred4, Pred5, Pred6, Pred7, Pred8, data)) %>%
#      ggplot() +
#         facet_wrap(~range,scales = "free")+
#           geom_point(aes(t,deaths),alpha=.1) +
#             geom_line(aes(t,exp(Pred1),colour="ns.res"))+
#             geom_line(aes(t,exp(Pred2),colour="bs.log.d"))+
#             #geom_line(aes(t,exp(Pred3),colour="poly.res"))+
#             #geom_line(aes(t,exp(Pred4),colour="t.res"))+
#                 theme_clean()+
#   geom_line(aes(t,exp(Pred5),colour="ns.res.fourier"))+
#   geom_line(aes(t,exp(Pred6),colour="bs.res.fourier"))
#   #geom_line(aes(t,exp(Pred7),colour="poly.res.fourier"))+
#   #geom_line(aes(t,exp(Pred8),colour="t.res.fourier"))
