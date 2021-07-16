# full year

#### SINADEF #####

#SINADEF - SOURCE: https://cloud.minsa.gob.pe/s/NctBnHXDnocgWAg - January 26 2021

SINADEF <- read_excel("data/SINADEF_DATOS_ABIERTOS_28052021.xlsx",skip=3)

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
  filter(Departamento!="SIN REGISTRO" &
           Departamento!="EXTRANJERO")

# SINADEF<-SINADEF %>%
#   mutate(FECHA = as.Date(FECHA, "%Y-%m-%d")) %>%
#   filter (AÑO!=2021)

#SINADEF %>%   group_by(AÑO) %>% summarise(n=n())

###

# SINADEF.proj.end <- SINADEF %>%
#   mutate(FECHA = as.Date(FECHA),
#          week=ISOweek(FECHA))    %>%
#   dplyr::group_by(AÑO,Departamento,range,week) %>%
#   summarise(deaths=n()) %>% ungroup() %>%
#   tidyr::complete(Departamento,range,week,fill = list(deaths = 0)) %>%
#   group_by(Departamento,range) %>%
#   mutate(t = 1:n()) %>%
#   group_by(week,t) %>%
#   summarise(deaths=n())
