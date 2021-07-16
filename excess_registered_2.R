fit2 <- aaa %>% filter(range=="a80") %>%
  group_by(Departamento) %>%
  do (fit = glm(deaths ~ ns(week2,3) +
                  fourier+
                  lag (res,1) +
                  covid+
                  offset(log(population)) ,
                family = quasipoisson,
                data = .))

aaa2 <- aaa %>% mutate(covid=0)

