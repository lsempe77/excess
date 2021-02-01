pop.reg.ed<-pob.regiones.group.age.sex.2020 %>%
  group_by(Departamento)%>%
  summarise(population=sum(population,na.rm=T))

SINADEF.causal4<-SINADEF %>%
  mutate(FECHA = as.Date(FECHA, "%Y-%m-%d")) %>%
  filter(FECHA >= "2018-01-01")%>%
  group_by(Departamento,FECHA)%>%
  summarise(deaths=n())%>% dplyr::select(deaths,FECHA,Departamento) %>%
  mutate(covid = case_when(FECHA > "2020-03-15" ~ 1,
                           T ~ 0))

SINADEF.causal4 <- SINADEF.causal4 %>% left_join(pop.reg.ed)


its.covid.list4b<-SINADEF.causal4 %>% group_by(Departamento) %>%
  do(its.covid = glance(glm(deaths ~ splines::bs(FECHA,knots=40) + covid+
                              offset(log(population)) + factor(wday(FECHA)),
                            family=poisson,data = .),
                        vcov = vcovHC(glm(deaths ~ splines::bs(FECHA,knots=40) + covid+
                                            factor(wday(FECHA))+
                                            offset(log(population)),
                                          family=poisson,data = .),
                                      type="HC3"))) %>%
  unnest(cols = c(its.covid)) %>%
  mutate(fit = 1 - (deviance/null.deviance),
         p= pchisq(deviance, df.residual, lower.tail=FALSE))
#
# hist(its.covid.list4b$p,breaks = c(seq(0,1,.01)))
#
# hist(its.covid.list4b$fit)
#
# its.covid.list4b %>% filter(p>.05) %>% print(n=105)

####

its.covid.list4d<-SINADEF.causal4 %>% group_by(Departamento) %>%
  do(its.covid = generics::tidy(lmtest::coeftest(glm(deaths ~ splines::bs(FECHA,knots=40) +
                                     covid+factor(wday(FECHA))+
                                     offset(log(population)),
                                   family=poisson,data = .),
                               vcov = sandwich::vcovHC(glm(deaths ~ splines::bs(FECHA,knots=40) + covid+
                                                   offset(log(population))+factor(wday(FECHA)), family=poisson,data = .),
                                             type="HC3")))) %>%
  unnest(cols = its.covid)


its.covid.list4d<-its.covid.list4d %>% mutate(low=exp(estimate)-1.96*std.error,
                                              up=exp(estimate)+1.96*std.error)


its.covid.list4d.l<-SINADEF.causal4 %>% group_by(Departamento) %>%
  filter(Departamento=="LAMBAYEQUE") %>%
  filter(FECHA > "2018-01-01") %>%
  do(its.covid = generics::tidy(lmtest::coeftest(glm(deaths ~ splines::bs(FECHA,knots=40) + covid+factor(wday(FECHA))+
                                     offset(log(population)),
                                   family=poisson,data = .),
                               vcov = sandwich::vcovHC(glm(deaths ~ splines::bs(FECHA,knots=40) + covid+
                                                   offset(log(population))+factor(wday(FECHA)),
                                                 family=poisson,data = .),
                                             type="HC3")))) %>%
  unnest(cols = its.covid) # filtrar 3, 6, 9, 12


its.covid.list4d.l<-its.covid.list4d.l %>% mutate(low=exp(estimate)-1.96*std.error,
                                                  up=exp(estimate)+1.96*std.error)


its.covid.list4d <- its.covid.list4d %>% full_join(its.covid.list4d.l)

its.covid.list4d[sapply(its.covid.list4d, is.nan)] <- NA


#its.covid.list4d %>% filter (Departamento=="LAMBAYEQUE")
#

SINADEF.causal4b <- SINADEF.causal4 %>%
  group_by(Departamento) %>%
filter(FECHA > "2020-03-15") %>%
  summarise(tot=sum(deaths))

#SINADEF.causal4b %>% filter (Departamento=="LAMBAYEQUE")

its.covid.list6d<-its.covid.list4d %>%
  left_join (SINADEF.causal4b) %>%
  mutate(excess=(exp(estimate)-1)/exp(estimate)*tot)

#

its.covid.list7d<-its.covid.list4d %>%
  left_join(its.covid.list6d)

e15<-its.covid.list7d %>%
  filter(term=="covid") %>%
  mutate(excess=case_when(term!="covid" ~ NA_real_,
                          T ~ excess),
         excess.low=(low-1)/low*tot,
         excess.up=(up-1)/up*tot)

# quasipoisson

its.covid.list4b.q<-SINADEF.causal4 %>% group_by(Departamento) %>%
  do(its.covid = glance(glm(deaths ~ splines::bs(FECHA,knots=40) + covid+
                              factor(wday(FECHA))+
                              offset(log(population)),
                            family=quasipoisson,data = .))) %>%
  unnest(cols = c(its.covid)) %>%
  mutate(fit = deviance/null.deviance,
         p= pchisq(deviance, df.residual, lower.tail=FALSE))

# ggplot(its.covid.list4b.q) + geom_histogram(aes(p),bins=100)
#
# hist(its.covid.list4b.q$p,breaks = c(seq(0,1,.001)))
#
# hist(its.covid.list4b.q$fit)
#
# its.covid.list4b.q %>% arrange(fit)%>%
#   #filter(p<.05) %>%
#   print(n=50)
#
# ##


# predicted<-SINADEF.causal4 %>% group_by(Departamento,range) %>%
#   mutate(fitted=predict(glm(deaths ~ splines::bs(FECHA,knots=40) + covid+
#                               factor(wday(FECHA))+
#                               offset(log(population)),
#                             family=quasipoisson),type="response"))
#
# predicted %>% filter(range=="a80" & Departamento=="LIMA") %>%
# ggplot() +   #geom_point(aes(FECHA,deaths),alpha=.1) +
#   geom_line(aes(FECHA,fitted)) +
#   #geom_line(aes(FECHA,deaths)) +
#     facet_wrap(~Departamento,scales = "free")



####

its.covid.list4d.q<-SINADEF.causal4 %>% group_by(Departamento) %>%
  do(its.covid = tidy(coeftest(glm(deaths ~ splines::bs(FECHA,knots=40) + covid+
                                     factor(wday(FECHA))+
                                     offset(log(population)),
                                   family=quasipoisson,data = .)))) %>%
  unnest(cols = its.covid)


its.covid.list4d.q<-its.covid.list4d.q %>% mutate(low=exp(estimate)-1.96*std.error,
                                                  up=exp(estimate)+1.96*std.error)


its.covid.list4d.l.q<-SINADEF.causal4 %>% group_by(Departamento) %>%
  filter(Departamento=="LAMBAYEQUE") %>%
  filter(FECHA > 2018-01-01) %>%
  do(its.covid = tidy(coeftest(glm(deaths ~ splines::bs(FECHA,knots=40) + covid+
                                     offset(log(population))+factor(wday(FECHA)),
                                   family=quasipoisson,data = .)))) %>%
  unnest(cols = its.covid) # filtrar 3, 6, 9, 12


its.covid.list4d.l.q<-its.covid.list4d.l.q %>% mutate(low=exp(estimate)-1.96*std.error,
                                                      up=exp(estimate)+1.96*std.error)


its.covid.list4d.q <- its.covid.list4d.q %>% full_join(its.covid.list4d.l.q)

#

its.covid.list6d.q<-its.covid.list4d.q %>%
  left_join (SINADEF.causal4b) %>%
  mutate(excess=(exp(estimate)-1)/exp(estimate)*tot)

#

its.covid.list7d.q<-its.covid.list4d.q %>%
  left_join(its.covid.list6d.q)

# its.covid.list7d.q %>% filter(term=="covid") %>%
#   mutate(excess=case_when(term!="covid" ~ NA_real_,
#                           T ~ excess),
#          excess.low=(low-1)/low*tot,
#          excess.up=(up-1)/up*tot) %>%
#   summarise(excess=sum(excess,na.rm = T),
#             excess.low=sum(excess.low,na.rm = T),
#             excess.up=sum(excess.up,na.rm = T))

e17<-its.covid.list7d.q %>%
  filter(term=="covid") %>%
  mutate(excess=case_when(term!="covid" ~ NA_real_,
                          T ~ excess),
         excess.low=(low-1)/low*tot,
         excess.up=(up-1)/up*tot)


#####


nbfit<-SINADEF.causal4 %>% group_by(Departamento) %>%
  do(its.covid = generics::tidy(lmtest::coeftest(glm.nb(deaths ~ splines::bs(FECHA,knots=40) +
                                                       covid+factor(wday(FECHA))+
                                                       offset(log(population)),data = .),
                                                 vcov = sandwich::vcovHC(glm.nb(deaths ~
                                                splines::bs(FECHA,knots=40) +  covid+
                                               offset(log(population))+factor(wday(FECHA)), data = .)),
                                               type="HC0"))) %>%
  unnest(cols = its.covid)


nbfit2<-SINADEF.causal4 %>% group_by(Departamento) %>%
  do(its.covid = glance(glm.nb(deaths ~ splines::bs(FECHA,knots=40) + covid+
                              factor(wday(FECHA))+
                              offset(log(population)),data = .))) %>%
  unnest(cols = c(its.covid)) %>%
  mutate(fit = 1 - (deviance/null.deviance),
         p= pchisq(deviance, df.residual, lower.tail=FALSE),
         prop.dev=deviance/null.deviance)
#
#
# ggplot(nbfit2) + geom_histogram(aes(p),bins=100)
#
#
# nbfit2 %>% arrange(prop.dev)%>%
#   #filter(p<.05) %>%
#   print(n=50)
#
#
# hist(nbfit2$prop.dev)
#
# hist(nbfit2$p,breaks = c(seq(0,1,.01)))
#
# hist(nbfit2$fit)
#
# nbfit2 %>% filter(p<.05) %>% print(n=105)
#

#
# its.covid.list4d.NB<-SINADEF.causal4 %>% group_by(Departamento,range) %>%
#   do(its.covid = tidy(coeftest(glm.nb(deaths ~ splines::bs(FECHA,knots=40) + covid+factor(wday(FECHA))+
#                                      offset(log(population)),data = .)))) %>%
#   unnest(cols = its.covid)
#



its.covid.list4d.NB<-nbfit %>% mutate(low=exp(estimate)-1.96*std.error,
                                                  up=exp(estimate)+1.96*std.error)

its.covid.list.NB<-its.covid.list4d.NB %>%
  left_join (SINADEF.causal4b) %>%
  mutate(excess=(exp(estimate)-1)/exp(estimate)*tot)

#

its.covid.list.NB2<-its.covid.list4d.NB %>%
  left_join(its.covid.list.NB)

# its.covid.list.NB2 %>% filter(term=="covid") %>%
#   mutate(excess=case_when(term!="covid" ~ NA_real_,T ~ excess),
#          excess.low=(low-1)/low*tot,excess.up=(up-1)/up*tot) %>%
#   summarise(excess=sum(excess,na.rm = T),
#             excess.low=sum(excess.low,na.rm = T),
#             excess.up=sum(excess.up,na.rm = T))

nb<-its.covid.list.NB2 %>%
  filter(term=="covid") %>%
  mutate(excess=case_when(term!="covid" ~ NA_real_,
                          T ~ excess),
         excess.low=(low-1)/low*tot,
         excess.up=(up-1)/up*tot)

t5
