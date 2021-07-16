fit1.aug <- aaa %>% filter(range=="a80") %>%
  group_by(Departamento) %>%
  nest() %>%
  full_join(fit2) %>%
  do(augment(.$fit[[1]], newdata = .$data[[1]],se_fit=T,type.predict = "response"))


fit2.aug <-  aaa2 %>% filter(range=="a80") %>%
  group_by(Departamento) %>%
  nest() %>%
  full_join(fit2) %>%
  do(augment(.$fit[[1]], newdata = .$data[[1]],se_fit=T,type.predict = "response"))

