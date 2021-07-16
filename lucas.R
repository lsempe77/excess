variable.names(districts)

lucas<-districts %>% select(NOMBDEP,NOMBPROV,NOMBDIST)

lucas$visita<-""

provs<-lucas %>% as.data.frame %>% select(NOMBPROV) %>% distinct()

provs

provs<-c("LIMA", "AREQUIPA", "TRUJILLO",
         "PIURA", "CHINCHA", "CUSCO", "ANDAHUAYLAS", "ANGARES", "ANTA", "BARRANCA", "CAÃ‘Â‘ETE",
         "CAÃ‘ETE",                 "CAJABAMBA",  "CAJATAMBO","CALCA", "CAMANA", "CANAS", "CANCHIS",
         "CANGALLO","CANTA", "CARABAYA", "CARHUAZ",
         "CHICHAYO", "HUANUCO", "CHACHAPOYAS", "MOYOBAMBA",
         "CAJAMARCA", "JAEN", "PUNO", "JULIACA", "ABANCAY", "HUANCAVELICA", "CALLAO",
         "AYACUCHO","TUMBES","SANTA", "HUARAZ", "ICA", "PISCO", "HUANCAYO","CORONEL PORTILLO",
         "IQUITOS", "PUERTO MALDONADO", "MELGAR","ZARUMILLA","YUNGUYO", "YUNGAY","YAULI","URUBAMBA",
         "TARMA", "SULLANA", "SANDIA", "PASCO","OYON","OXAPAMPA","MAYNAS","MOHO","LUYA","LAMAS",
         "LA MAR","JAUJA","HUAROCHIRI", "HUARA","HUARMEY"

         )



lucas <- lucas %>% mutate(visita=case_when(NOMBPROV %in% provs ~ 1))

lucas$visita<-as.factor(lucas$visita)

ggplot(lucas)+ geom_sf(aes(fill=visita),colour="transparent") + theme_map()
