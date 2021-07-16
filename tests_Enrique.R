
fit1 <- aaa %>%
  group_by(Departamento,range) %>%
   do (fit = glm(deaths ~ ns(week2,3) +
                      fourier+
                       lag (res,1) +
                             covid+
                      offset(log(population)) ,
                    family = quasipoisson,
                    data = .))

aaa2 <- aaa %>% mutate(covid=0)

fit1.aug <-
  aaa %>%
  group_by(Departamento,range) %>%
  nest() %>%
  full_join(fit1) %>%
  do(augment(.$fit[[1]], newdata = .$data[[1]],se_fit=T,type.predict = "response"))


fit2.aug <-
  aaa2 %>%
  group_by(Departamento,range) %>%
  nest() %>%
  full_join(fit1) %>%
  do(augment(.$fit[[1]], newdata = .$data[[1]],se_fit=T,type.predict = "response"))

#

