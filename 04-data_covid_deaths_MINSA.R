
#####


#url <- "https://www.datosabiertos.gob.pe/node/6460/download"

#destfile <- "data/fallecidos_covid2020.csv"

#download.file(url, destfile)

fallecidos_covid <- read.csv("data/fallecidos_covid2020.csv", sep=";")

#head(fallecidos_covid)
#table(fallecidos_covid$FECHA_FALLECIMIENTO)

fallecidos_covid <- fallecidos_covid %>%
  mutate(range = case_when(
    EDAD_DECLARADA < 10 ~ "a0.9",
    EDAD_DECLARADA >= 10 & EDAD_DECLARADA <20 ~ "a10.19",
    EDAD_DECLARADA >= 20 & EDAD_DECLARADA <30 ~ "a20.29",
    EDAD_DECLARADA >= 30 & EDAD_DECLARADA <40 ~ "a30.39",
    EDAD_DECLARADA >= 40 & EDAD_DECLARADA <50 ~ "a40.49",
    EDAD_DECLARADA >= 50 & EDAD_DECLARADA <60 ~ "a50.59",
    EDAD_DECLARADA >= 60 & EDAD_DECLARADA <70 ~ "a60.69",
    EDAD_DECLARADA >= 70 & EDAD_DECLARADA <80 ~ "a70.79",
    EDAD_DECLARADA >= 80 ~ "a80"))

#

fallecidos_covid$FECHA_FALLECIMIENTO<-sub("(.{4})(.*)", "\\1.\\2", fallecidos_covid$FECHA_FALLECIMIENTO)
fallecidos_covid$FECHA_FALLECIMIENTO<-sub("(.{7})(.*)", "\\1.\\2", fallecidos_covid$FECHA_FALLECIMIENTO)

fallecidos_covid.l<-fallecidos_covid %>%
  mutate (FECHA_FALLECIMIENTO=as.Date(FECHA_FALLECIMIENTO,"%Y.%m.%d"),
          week = epiweek(FECHA_FALLECIMIENTO)) %>%
  filter(FECHA_FALLECIMIENTO<"2020-12-01") %>% # check  2021-01-01
  group_by(DEPARTAMENTO,week,.drop=F)%>%summarise(Covid_deaths= n())

colnames(fallecidos_covid.l)[1]<-"Departamento"

fallecidos_covid.r.l<- fallecidos_covid %>%
  mutate (FECHA_FALLECIMIENTO=as.Date(FECHA_FALLECIMIENTO,"%Y.%m.%d"),
          week = epiweek(FECHA_FALLECIMIENTO)) %>%
  filter(FECHA_FALLECIMIENTO<"2021-01-01") %>%
  group_by(DEPARTAMENTO,range,.drop=F) %>%
  summarise(Covid_deaths= n())


#fallecidos_covid.r.l %>% filter (DEPARTAMENTO=="LAMBAYEQUE")


fallecidos_covid.test<-fallecidos_covid %>%
  mutate (FECHA_FALLECIMIENTO=as.Date(FECHA_FALLECIMIENTO,"%Y.%m.%d"),
          week = epiweek(FECHA_FALLECIMIENTO)) %>%
  filter(FECHA_FALLECIMIENTO<"2020-12-01") #37725 deaths COVID 2020 # # check  2021-01-01

covidofic<-fallecidos_covid.test %>% tally()


##

covidnaive<-SINADEF %>%
  mutate(FECHA = as.Date(FECHA, "%Y-%m-%d")) %>%
  filter(FECHA > "2020-03-16")%>%
  group_by(Departamento,range,.drop=F) %>%
  summarise(deaths=n())%>% dplyr::select(deaths,range,Departamento)  %>%
  left_join(fallecidos_covid.r.l,by=c("range","Departamento"="DEPARTAMENTO")) %>%
  ungroup() %>%
  mutate(complete.covid = case_when(
    Covid_deaths > deaths ~
      (Covid_deaths-deaths),
    T ~ 0L))

covidnaive
