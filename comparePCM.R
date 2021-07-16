
SINADEF.causes.PCM <- SINADEF %>%
  mutate(week=ISOweek(FECHA),
         Date = ISOweek2date(paste0(week, "-7"))) %>%
  filter(week != "2016-W52" &  Date < "2020-12-01") %>% #2021-01-01
  filter (    `CAUSA A (CIE-X)` %in% target |
                `CAUSA B (CIE-X)` %in% target |
                `CAUSA C (CIE-X)` %in% target |
                `CAUSA D (CIE-X)` %in% target ) %>%
  group_by(Departamento, range, Date) %>%
  summarise(deaths.covid=n())

SINADEF.causes.PCM<- SINADEF.causes.PCM %>%
  as_tsibble(key = c(Departamento, range), index = Date) %>%
  fill_gaps(.full = TRUE)

SINADEF.causes.PCM<-SINADEF.causes.PCM %>% as_tibble() %>%
  mutate (deaths.covid = case_when (is.na(deaths.covid) ~ 0,
                                    T ~ as.numeric(deaths.covid)))


##

SINADEF.proj.t.PCM <- SINADEF %>%
  mutate(week=ISOweek(FECHA),
         Date = ISOweek2date(paste0(week, "-7"))) %>%
  filter(week != "2016-W52" &  Date < "2020-12-01") %>%#2021-01-01
  group_by(Departamento,range,week,Date) %>%
  summarise(deaths=n())

SINADEF.proj.t.PCM<- SINADEF.proj.t.PCM %>%
  as_tsibble(key = c(Departamento, range), index = Date) %>%
  fill_gaps(.full = TRUE)


SINADEF.proj.t.PCM<-SINADEF.proj.t.PCM %>% as_tibble() %>%
  mutate (deaths = case_when (is.na(deaths) ~ 0,
                              T ~ as.numeric(deaths))) %>%
  group_by(Departamento,range) %>% mutate(week2=1:n())%>%
  left_join(epitime.regions) %>%
  mutate(covid = case_when(week2 >= start ~ 1,
                           T ~ 0)) %>%
  filter(!is.na(range)) %>%
  ungroup()


SINADEF.causal4.t.PCM <- SINADEF.proj.t.PCM%>%
  group_by(Departamento,range) %>%
  filter(week2 >= start) %>%
  summarise(tot=sum(deaths,na.rm = T))

SINADEF.proj.t.PCM$fourier <- harmonic(SINADEF.proj.t.PCM$week2,
                                   nfreq=6,period=52.18) # annual cycle and harmonics


SINADEF.proj.t.PCM <- SINADEF.proj.t.PCM %>% left_join(pop.reg.ed.t)

#


# glimpse(SINADEF.proj.t)

aaa.PCM<- SINADEF.proj.t.PCM %>%
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



tt.PCM <- aaa.PCM %>% left_join(SINADEF.causes.PCM) %>%
  mutate(year = lubridate::year(Date)) %>% filter(year != 2017) %>%
  group_by(Departamento,range,week2) %>%
  summarise    (m =  case_when (week2 < start ~ mean(deaths,na.rm=T)),
                m2 = case_when (week2 >= end ~ mean(deaths,na.rm=T)-mean(deaths.covid,na.rm=T))) %>%
  group_by(Departamento,range) %>%
  summarise(se = sd(m2,na.rm = T),
            mean.prev = mean(m,na.rm=T),
            mean.post = mean(m2,na.rm=T),
            Change_registration = (mean.post-mean.prev)/mean.prev)




###

its.covid.list4d.t.PCM <- aaa.PCM %>% group_by(Departamento,range) %>%
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

e17.t.PCM<-its.covid.list4d.t.PCM %>%
  filter(term=="covid") %>%
  mutate(excess.low=(low-1)/low*tot,
         excess.up=(up-1)/up*tot)


fallecidos_covid.t.PCM<- fallecidos_covid %>%
  mutate (FECHA_FALLECIMIENTO = as.Date(FECHA_FALLECIMIENTO,
                                        "%Y.%m.%d"),
          week = epiweek(FECHA_FALLECIMIENTO)) %>%
  filter(FECHA_FALLECIMIENTO<"2020-12-01") %>%
  group_by(DEPARTAMENTO,range) %>% rename(Departamento=DEPARTAMENTO) %>%
  summarise(Covid_deaths= n())


FINAL5.t.PCM<-e17.t.PCM%>%
  left_join(fallecidos_covid.t.PCM)%>%
  left_join(sub.reg) %>%
  mutate(Covid_deaths = ifelse(is.na(Covid_deaths), 0, Covid_deaths))


tateti.t.PCM<-FINAL5.t.PCM%>%
  group_by(Departamento,
           range,
           sub.mean,
           #mort.estim.edades.f,
           Covid_deaths,p.value) %>%
  summarise(excess_deaths.sum=mean(excess),
            excess.low=mean(excess.low),
            excess.up=mean(excess.up))

tateti6.t.PCM<- tateti.t.PCM %>% left_join(tt.PCM) %>%
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
    excess.total.mean = case_when(excess_deaths.sum > 0 ~ excess_deaths.sum + !is.na(complete.covid) +
                                    ((excess_deaths.sum - !is.na(complete.covid))* ((1-sub.mean) * (1-Change_registration*.5))),
                                  T ~ excess_deaths.sum + complete.covid),
    excess.total.low = case_when(Covid_deaths > excess.low ~ Covid_deaths,
                                 T ~ excess.low +
                                   ((excess.low - !is.na(Covid_deaths)) * ((1-sub.mean) * (1-Change_registration*.5)))+
                                   !is.na(Covid_deaths)),
    excess.total.up = case_when(Covid_deaths > excess.up ~ Covid_deaths,
                                T ~ excess.up +
                                  ((excess.up - !is.na(Covid_deaths))*((1-sub.mean) * (1-Change_registration*.5)))+
                                  !is.na(Covid_deaths)),
    excess.total.mean = case_when(excess.total.mean < excess.total.low ~
                                    excess.total.low,
                                  T ~ excess.total.mean),
    excess.total.mean = case_when(
      Covid_deaths > excess.total.mean ~
        Covid_deaths,
      T ~ excess.total.mean))

t5.t.PCM<-tateti6.t.PCM %>% ungroup() %>%
  summarise(excess.registered.Naive=sum(excess_deaths.sum,na.rm = T),
            excess.low=sum(excess.low,na.rm = T),
            excess.up=sum(excess.up,na.rm = T),
            excess=sum(excess.total.mean,na.rm = T),
            excess.l=sum(excess.total.low,na.rm = T),
            excess.u=sum(excess.total.up,na.rm = T))

pcm<-t5.t.PCM[1,1]

colnames(pcm)<-"pcm"
