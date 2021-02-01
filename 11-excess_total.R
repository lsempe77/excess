t5<-tateti6 %>% ungroup() %>%
  summarise(excess.registered.Naive=sum(excess_deaths.sum),
            excess.low=sum(excess.low,na.rm = T),
            excess.up=sum(excess.up,na.rm = T),
            excess=sum(excess.total.mean,na.rm = T),
            excess.l=sum(excess.total.low,na.rm = T),
            excess.u=sum(excess.total.up,na.rm = T))

## for summary table

t5

# ## subregistration
#
# SINADEF %>%filter(AÑO==2019)%>%
#   group_by(Departamento)%>%
#   summarise(deaths.sinadef=n()) %>%left_join(sub.reg) %>%
#   mutate (p=deaths.sinadef*sub.mean) %>%
#   ungroup() %>% summarise (deaths=sum(deaths.sinadef),
#                            sub=sum(p),
#                            prop.sub=sub/deaths)

### excess naive, excess total
#
# t5
#
# #additional covid
# #
# tateti6 %>% filter (complete.covid>0)%>%group_by(Departamento)%>%
#   summarise(complete=sum(complete.covid,na.rm = T)) %>% arrange(-complete) %>%
#   print (n=25)
#
#
# ### over 60

# tateti6%>%group_by(range) %>%
#   summarise(`Total excess (TE)`=sum(excess.total.mean,na.rm = T),
#             `TE - Lower`=sum(excess.total.low,na.rm = T),
#             `TE - Upper`=sum(excess.total.up,na.rm = T),
#             `Excess registered (ER)`=sum(excess_deaths.sum),
#             `ER - Lower`=sum(excess.low),
#             `ER - Upper`=sum(excess.up),
#             `Excess Covid-19`=sum(complete.covid)) %>%
#   adorn_totals()
#
# (35779.777+36285.994+36978.84)/142658

# tateti6%>%group_by(Departamento) %>%
#   summarise(`Total excess (TE)`=sum(excess.total.mean,na.rm = T),
#             `TE - Lower`=sum(excess.total.low,na.rm = T),
#             `TE - Upper`=sum(excess.total.up,na.rm = T),
#             `Excess registered (ER)`=sum(excess_deaths.sum,na.rm = T),
#             `ER - Lower`=sum(excess.low,na.rm = T),
#             `ER - Upper`=sum(excess.up,na.rm = T),
#             `Excess Covid-19`=sum(complete.covid,na.rm = T),
#             `dif`=`Total excess (TE)`-`Excess registered (ER)`-`Excess Covid-19`,
#             `dif.l`=`TE - Lower`-`ER - Lower`-`Excess Covid-19`,
#             `dif.u`=`TE - Upper`-`ER - Upper`-`Excess Covid-19`) %>% dplyr::select(Departamento,dif)%>%
#   arrange (-dif) %>% print (n=25)



###


# fit subregistration



nbfit.r<-SINADEF.causal4 %>% group_by(Departamento,range) %>%
  do(qc = qchisq(0.95,
        df.residual(glm.nb(deaths ~ splines::bs(FECHA,knots=40) + covid+
                                factor(wday(FECHA))+
               offset(log(population)),data = .))),
     d = deviance(glm.nb(deaths ~ splines::bs(FECHA,knots=40) + covid+
                                      factor(wday(FECHA))+
                                      offset(log(population)),data = .)))%>%
     unnest(cols = c(qc,d))

# nbfit.r %>% filter(d>qc)
#
# tateti6 %>% group_by(Departamento)%>%
#   summarise (sub.mean=mean(sub.mean)) %>%arrange (-sub.mean)
#
#
# pop.dep.range%>% group_by(Departamento) %>%
#   summarise (sum=sum(pop.INEI)) %>% arrange(sum)%>% print (n=25)
