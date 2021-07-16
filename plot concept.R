aaa %>% left_join(epitime.regions) %>%
  mutate(Date = ISOweek2date(paste0(week, "-7")),
         year = lubridate::year(Date),
         ep=epiweek(Date),
         thisyear = (year == 2020)) %>%
  group_by(Departamento,ep,year,thisyear,start,end) %>%
  mutate(
    m=case_when(year!="2020"~ mean(deaths,na.rm=T)),
    m1=case_when(year=="2020"~ mean(deaths,na.rm=T)))%>%
  summarise (deaths=sum(deaths),
             deaths2 = case_when(year == 2020 ~ sum(deaths))) %>%
  filter(Departamento=="LIMA") %>%
  ggplot (aes(ep,deaths,group=year)) +  theme_cowplot() +
    scale_color_manual(values=c("FALSE"='gray',"TRUE"='red')) +
  guides(col=F) +
  scale_x_continuous(breaks = seq(1,52,3)) +
  geom_line(aes(col=thisyear)) +
  geom_segment(aes(x = 44,  xend = 56, y= 980, yend = 980),
               linetype=2,colour="orange") +
  geom_segment(aes(x = 1,  xend = 15, y= 615, yend = 615),
               linetype=2,colour="orange") +
  geom_segment(aes(x = 15,  xend = 44, y= 615, yend = 980),
               linetype=2,colour="darkgreen") +
  geom_vline(aes(xintercept=end-(52*3)),linetype=2,colour="darkgray")+
  geom_vline(aes(xintercept=start-(52*3)),linetype=2,colour="darkgray") +
  xlab("Epidemic week") + ylab("Deaths")  +
  geom_line(aes(ep,deaths2*1.25),linetype=4,colour="blue")+
  geom_ribbon(data=. %>% filter(ep>=15 & ep<=44),
              aes(ymin=615, ymax=980),
              fill="darkgreen",alpha=0.01) +
  geom_text(aes(x=7.5, y=560), label="Mean previous",
            color="orange", size=3,family="mono") +
  geom_text(aes(x=50, y=900), label="Mean post",
            color="orange",size=3,family="mono") +
  geom_text(aes(x=29, y=4000), label="Excess unregistered",
            color="blue",size=3,family="mono")+
  geom_text(aes(x=29, y=660), label="Registration growth",
            color="darkgreen",size=3,family="mono")


