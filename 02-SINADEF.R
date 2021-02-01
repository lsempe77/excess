# full year

#### SINADEF #####

#SINADEF - SOURCE: https://cloud.minsa.gob.pe/s/NctBnHXDnocgWAg - January 26 2021

SINADEF <- read_excel("data/SINADEF_DATOS_ABIERTOS_26012021.xlsx",skip=3)
#SINADEF %>% glimpse()

colnames(SINADEF)[10]<-"Departamento"

#str(SINADEF)
SINADEF$edad.num<-SINADEF$EDAD
SINADEF$edad.num<-as.numeric(SINADEF$edad.num)

#summary(SINADEF$edad.num)
#table(SINADEF$`TIEMPO EDAD`,SINADEF$AÑO)
#table(SINADEF$`TIEMPO EDAD`,SINADEF$EDAD)


SINADEF$edad.num[SINADEF$`TIEMPO EDAD` == "DIAS" |
                   SINADEF$`TIEMPO EDAD` == "HORAS"|
                   SINADEF$`TIEMPO EDAD` == "MESES" |
                   SINADEF$`TIEMPO EDAD` == "MINUTOS"|
                   SINADEF$`TIEMPO EDAD` == "SEGUNDOS"] <- 0

#summary(SINADEF$edad.num)
#table(SINADEF$edad.num,SINADEF$`TIEMPO EDAD`)

SINADEF <- SINADEF %>%
  mutate(range = case_when(
    edad.num < 10 ~ "a0.9",
    edad.num >= 10 & edad.num <20 ~ "a10.19",
    edad.num >= 20 & edad.num <30 ~ "a20.29",
    edad.num >= 30 & edad.num <40 ~ "a30.39",
    edad.num >= 40 & edad.num <50 ~ "a40.49",
    edad.num >= 50 & edad.num <60 ~ "a50.59",
    edad.num >= 60 & edad.num <70 ~ "a60.69",
    edad.num >= 70 & edad.num <80 ~ "a70.79",
    edad.num >= 80 ~ "a80"))

SINADEF <-SINADEF %>%
  filter(Departamento!="SIN REGISTRO" & Departamento!="EXTRANJERO")

SINADEF<-SINADEF %>%
  mutate(FECHA = as.Date(FECHA, "%Y-%m-%d")) %>%
  mutate(week = lubridate::epiweek(FECHA)) %>%
  filter (AÑO!=2021)

SINADEF %>%   group_by(AÑO)%>% summarise(n=n())

###

SINADEF.proj.end <- SINADEF %>%
  group_by(AÑO)%>%
  mutate(week = lubridate::epiweek(FECHA)) %>%
  group_by(week) %>%
  mutate(week =
           case_when(AÑO == 2017 & FECHA != "2017-12-31" ~ week,
                     FECHA == "2017-12-31" ~ 53,
                     AÑO==2018 & (FECHA != "2018-12-30" | FECHA != "2018-12-31")  ~ week+52,
                     FECHA == "2018-12-30" | FECHA == "2018-12-31" ~ 105,
                     AÑO==2019 & (FECHA != "2019-12-29" | FECHA != "2019-12-30"
                                  | FECHA != "2019-12-31") ~ week+104,
                     FECHA == "2019-12-29" | FECHA == "2019-12-30" |FECHA == "2019-12-31" ~ 157,
                     AÑO== 2020 ~ week+156,
                     FECHA == "2021-01-01" | FECHA == "2021-01-02" ~ 209,
                     AÑO==2021 & (FECHA !="2021-01-01" | FECHA != "2021-01-02") ~ week+209))  %>%
  #filter (FECHA>"2020-10-10")%>%
  dplyr::group_by(week) %>%
  summarise(deaths=n())
