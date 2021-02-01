### Mortality rates region

mort.rate.regions.range.2010.2015.2020 <- read_excel("data/tasas_mortalidad_regiones_INEI.xlsx")

mort.rate.regions.range.2010.2015.2020<- mort.rate.regions.range.2010.2015.2020%>%
  pivot_longer(cols = `2010`:`2015`)

colnames(mort.rate.regions.range.2010.2015.2020)[2]<-"year.range"
colnames(mort.rate.regions.range.2010.2015.2020)[3]<-"crm"

mort.rate.regions.range.2010.2015.2020$year.range[mort.rate.regions.range.2010.2015.2020$year.range=="2015"]<-"2015-2020"

mort.rate.regions.range.2010.2015.2020$year.range[mort.rate.regions.range.2010.2015.2020$year.range=="2010"]<-"2010-2014"

colnames(mort.rate.regions.range.2010.2015.2020)[2]<-"Año"

mort.rate.regions.range.2010.2015.2020 <-
  mort.rate.regions.range.2010.2015.2020 %>%
  filter (Año =="2015-2020") %>%
  mutate (Año = case_when (Año == "2015-2020" ~ "2020"))

