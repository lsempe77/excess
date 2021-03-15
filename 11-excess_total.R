
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
#             `dif.u`=`TE - Upper`-`ER - Upper`-`Excess Covid-19`) %>% print (n=25)
#


###

