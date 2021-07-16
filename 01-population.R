####
#### RELEVANT DATA TO BE USED IN TUTORIAL OR ANALYSIS
####

#### POPULATION ####

#### Estimated population by age and sex - 1950-2020 ####

pob.nac.ed.sex.1950.2020 <- read_excel("data/poblacion_INEI.xlsx")

pob.nac.ed.sex.1950.2020$Sexo<-factor(pob.nac.ed.sex.1950.2020$Sexo,
                                      levels = c(0,1),
                                      labels = c("M", "F"))



#### Defunciones registradas INEI - departamentos 2007:2018 ####

defuncionesINEI <- read_excel("data/defunciones_INEI.xlsx")

#variable.names(defuncionesINEI)
colnames(defuncionesINEI)[1]<-"Departamento"

Departamento<-defuncionesINEI$Departamento
Departamento[Departamento=="LIMA.MET" ] <- "LIMA"
Departamento<-Departamento[-16]

#Population region and age 1

pob.regiones.ed.2005.2015 <- "data/poblaciones_regiones_grupos_edad_INEI.xls"

pob.regiones.ed.2005.2015 <- pob.regiones.ed.2005.2015 %>%
  excel_sheets() %>%
  rlang::set_names() %>%
  map_df(~ read_excel(path = pob.regiones.ed.2005.2015,
                      sheet = .x, range = "A6:U2529"), .id = "sheet")

#variable.names(pob.regiones.ed.2005.2015)

colnames(pob.regiones.ed.2005.2015)[2]<-"nivel"
colnames(pob.regiones.ed.2005.2015)[3]<-"UBIGEO"
colnames(pob.regiones.ed.2005.2015)[4]<-"Departamento"

pob.regiones.ed.2005.2015<-pob.regiones.ed.2005.2015 %>%
  filter(str_detect(UBIGEO,pattern = "0000"))

pob.regiones.ed.2005.2015$Departamento[pob.regiones.ed.2005.2015$Departamento=="PROV. CONST. DEL CALLAO"]<-"CALLAO"
pob.regiones.ed.2005.2015$Departamento[pob.regiones.ed.2005.2015$Departamento=="ÁNCASH"]<-"ANCASH"
pob.regiones.ed.2005.2015$Departamento[pob.regiones.ed.2005.2015$Departamento=="HUÁNUCO"]<-"HUANUCO"

variable.names(pob.regiones.ed.2005.2015)
colnames(pob.regiones.ed.2005.2015)[22] <- "e80"
pob.regiones.ed.2005.2015$e80<-as.numeric(pob.regiones.ed.2005.2015$e80)

headers<-names(pob.regiones.ed.2005.2015)[c(4,6:22)]

#head(pob.regiones.ed.2005.2015)

# pob regiones 2005.2015 long


#### Population per region, sex and age - 2020 ####

f <- "data/Proyecciones_poblacionales_INEI.pdf"

pob.mas.age.region <- extract_tables(f, pages = 74)
pob.mas.age.region <- do.call(rbind, pob.mas.age.region)
pob.mas.age.region<-as.data.frame(pob.mas.age.region)
pob.mas.age.region<-pob.mas.age.region[-1,]

pob.mas.age.region$V2<-NULL

# Apply custom column names
names(pob.mas.age.region) <- headers

pob.mas.age.region<-lapply( pob.mas.age.region, function(col_) {gsub( " ","",col_)} )

pob.mas.age.region<-as.data.frame(do.call(cbind, pob.mas.age.region))

pob.mas.age.region$Departamento<-Departamento

# extract tables

pob.fem.age.region <- extract_tables(f, pages = 75)
pob.fem.age.region <- do.call(rbind, pob.fem.age.region)
pob.fem.age.region<-as.data.frame(pob.fem.age.region)
pob.fem.age.region<-pob.fem.age.region[-1,]

pob.fem.age.region$V2<-NULL

# Apply custom column names

names(pob.fem.age.region) <- headers

pob.fem.age.region<-lapply( pob.fem.age.region, function(col_) {gsub( " ","",col_)} )

pob.fem.age.region<-as.data.frame(do.call(cbind, pob.fem.age.region))

pob.fem.age.region$Departamento<-Departamento

pob.mas.age.region$Sexo<-0
pob.fem.age.region$Sexo<-1

pob.regiones.group.age.sex.2020<-rbind(pob.fem.age.region,pob.mas.age.region)

cols <- names(pob.regiones.group.age.sex.2020)[2:18]

pob.regiones.group.age.sex.2020[cols] <- lapply(pob.regiones.group.age.sex.2020[cols], as.numeric)

pob.regiones.group.age.sex.2020<-pob.regiones.group.age.sex.2020%>%
  mutate(a0.9 = rowSums(.[2:3]),
         a10.19 = rowSums(.[4:5]),
         a20.29 = rowSums(.[6:7]),
         a30.39 = rowSums(.[8:9]),
         a40.49 = rowSums(.[10:11]),
         a50.59 = rowSums(.[12:13]),
         a60.69  = rowSums(.[14:15]),
         a70.79 = rowSums(.[16:17]),
         a80 = rowSums(.[18])) %>%
  dplyr::select(Departamento,Sexo,a0.9:a80)%>%
  group_by(Departamento, Sexo,.drop=F) %>%
  pivot_longer(cols = a0.9:a80)

colnames(pob.regiones.group.age.sex.2020)[4]<-"population"

pob.regiones.group.age.sex.2020$Sexo<-as.factor(pob.regiones.group.age.sex.2020$Sexo)

pob.regiones.group.age.sex.2020$Sexo<-factor(pob.regiones.group.age.sex.2020$Sexo,
                                             levels = c(0,1),
                                             labels = c("Male", "Female"))

compare.estim.age<-pob.regiones.group.age.sex.2020%>%
  group_by(Departamento,name,.drop=F)%>%
  summarise(pop.INEI=sum(population))

colnames(compare.estim.age)[2]<-"range"

###

population_regions <- read_excel("data/population_regions.xls.xlsx")

