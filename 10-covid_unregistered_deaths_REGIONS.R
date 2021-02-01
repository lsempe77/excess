
fallecidos_covid.r.l<- fallecidos_covid %>%
  mutate (FECHA_FALLECIMIENTO = as.Date(FECHA_FALLECIMIENTO,
                                        "%Y.%m.%d"),
          week = epiweek(FECHA_FALLECIMIENTO)) %>%
  filter(FECHA_FALLECIMIENTO<"2021-01-01") %>%
  group_by(DEPARTAMENTO) %>%
  summarise(Covid_deaths= n())


FINAL5<-nb%>%
  left_join(fallecidos_covid.r.l,
            by=c("Departamento"="DEPARTAMENTO"))%>%
  left_join(sub.reg) %>%
  mutate(Covid_deaths = ifelse(is.na(Covid_deaths), 0, Covid_deaths))


tateti<-FINAL5%>%
  group_by(Departamento,
           sub.mean,
           #mort.estim.edades.f,
           Covid_deaths,p.value) %>%
  summarise(excess_deaths.sum=mean(excess),
            excess.low=mean(excess.low),
            excess.up=mean(excess.up))




tateti6<- tateti %>% left_join(proj.sinadef2020,by=c("Departamento"))%>%
  left_join(regisration.adjustement[,c(1,2,9)]) %>%
  group_by(Departamento)%>%
  mutate(perc.dif= 0,
    inei.g=0,
    complete.covid = case_when(
    Covid_deaths > excess_deaths.sum ~
      (Covid_deaths-excess_deaths.sum),
    T ~ 0)
    ,
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
    excess.total.mean = (excess_deaths.sum * (1-inei.g)) +
      (excess_deaths.sum)*(1-sub.mean)* (1-inei.g)*(1-perc.dif)+
      complete.covid,
    excess.total.low = (excess.low * (1-inei.g)) +
      (excess.low)*(1-sub.mean)* (1-inei.g)*(1-perc.dif)+
      complete.covid,
    excess.total.up = (excess.up * (1-inei.g)) +
      (excess.up)*(1-sub.mean)* (1-inei.g)*(1-perc.dif)+
      complete.covid)

# tateti6 %>% filter(p.value>.05) %>% ungroup()%>%
#   summarise(s=sum(excess.total.mean))

