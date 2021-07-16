fit<-aaa %>%
  group_by(Departamento,range) %>%
  do(its.covid = glance(glm(deaths ~ ns(week2,3)+fourier+
                              covid+lag(res,1)+
                              offset(log(population)),
                            family = quasipoisson,
                            data=.))) %>%
  unnest(cols = its.covid) %>%
  mutate(fit = 1 - (deviance/null.deviance),
         dif.dev=null.deviance-deviance,
         df=df.null-df.residual,
         p= pchisq(dif.dev, df, lower.tail=FALSE))

# ggplot(fit)+geom_histogram(aes(fit),bins = 60)
# ggplot(fit)+geom_histogram(aes(p),bins = 60)


#H0: 'the model as a whole is no better than the null model'.
#' Since this has been rejected (p<.05), we conclude that the data are not
#' consistent with the null model.

