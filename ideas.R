SINADEF %>% filter (`CAUSA A (CIE-X)`=="U071" |
                    `CAUSA A (CIE-X)`== "U072"|
                    `CAUSA B (CIE-X)`=="U071" |
                    `CAUSA B (CIE-X)`== "U072"|
                    `CAUSA C (CIE-X)`=="U071" |
                    `CAUSA C (CIE-X)`== "U072"|
                    `CAUSA D (CIE-X)`=="U071" |
                    `CAUSA D (CIE-X)`== "U072"|
                    `CAUSA E (CIE-X)`=="U071" |
                    `CAUSA E (CIE-X)`== "U072"|
                    `CAUSA F (CIE-X)`=="U071" |
                    `CAUSA F (CIE-X)`== "U072") %>%
  filter(AÑO=="2020") %>%
  pivot_longer(cols=`CAUSA A (CIE-X)`:`CAUSA F (CIE-X)`,
               names_to = "causes") %>%
  filter(value=="U071"|value=="U072") %>%
  group_by(value) %>%
  summarise(n=n()) %>% ggplot()+geom_point(aes(value,n,colour=range))+
  facet_wrap(~Departamento,scales="free")

SINADEF %>% filter (`CAUSA A (CIE-X)`=="U071" |
                      `CAUSA A (CIE-X)`== "U072"|
                      `CAUSA B (CIE-X)`=="U071" |
                      `CAUSA B (CIE-X)`== "U072"|
                      `CAUSA C (CIE-X)`=="U071" |
                      `CAUSA C (CIE-X)`== "U072"|
                      `CAUSA D (CIE-X)`=="U071" |
                      `CAUSA D (CIE-X)`== "U072"|
                      `CAUSA E (CIE-X)`=="U071" |
                      `CAUSA E (CIE-X)`== "U072"|
                      `CAUSA F (CIE-X)`=="U071" |
                      `CAUSA F (CIE-X)`== "U072") %>%
  filter(AÑO=="2020") %>%
  # pivot_longer(cols=`CAUSA A (CIE-X)`:`CAUSA F (CIE-X)`,
  #              names_to = "causes") %>%
  # filter(value=="U071"|value=="U072") %>%
  group_by(Departamento,range) %>%
  summarise(sinadef=n()) %>% left_join(fallecidos_covid.r.l,
                                 by=c("Departamento"="DEPARTAMENTO",
                                                           "range")) %>%
  ungroup()%>%
  summarise(s=sum(sinadef,na.rm = T),
            c=sum(Covid_deaths,na.rm = T))


SINADEF %>% filter (`CAUSA A (CIE-X)`=="U071"  |
                      `CAUSA B (CIE-X)`=="U071"|
                      `CAUSA C (CIE-X)`=="U071"|
                      `CAUSA D (CIE-X)`=="U071"|
                      `CAUSA E (CIE-X)`=="U071"|
                      `CAUSA F (CIE-X)`=="U071") %>%
  filter(AÑO=="2020") %>%
  # pivot_longer(cols=`CAUSA A (CIE-X)`:`CAUSA F (CIE-X)`,
  #              names_to = "causes") %>%
  # filter(value=="U071"|value=="U072") %>%
  group_by(Departamento,range) %>%
  summarise(sinadef=n()) %>%
  left_join(fallecidos_covid.r.l,
            by=c("Departamento"="DEPARTAMENTO","range")) %>%
  ungroup()%>%
  summarise(s=sum(sinadef,na.rm = T),
            c=sum(Covid_deaths,na.rm = T))
