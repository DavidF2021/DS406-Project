scorecard_a<- scorecard[,-c(2, 12:15)]
colSums(is.na(scorecard_a[,]))

scorecard_a <- scorecard_a %>%
  left_join(
    speed_dating %>% 
      select(iid, pid, attr_o, sinc_o, intel_o, fun_o, amb_o, shar_o), 
    by = c("pid" = "iid", "iid" = "pid")
  ) %>%
  # if a value is missing in the scorecard 
  mutate(
    attr  = coalesce(attr,  attr_o),
    sinc  = coalesce(sinc,  sinc_o),
    intel = coalesce(intel, intel_o),
    fun   = coalesce(fun,   fun_o),
    amb   = coalesce(amb,   amb_o),
    shar  = coalesce(shar,  shar_o)
  ) %>%
  select(-attr_o, -sinc_o, -intel_o, -fun_o, -amb_o, -shar_o)

#no actual improvement made

#verify there aren't any non-valid entry
table(scorecard_a$dec) 
table(scorecard_a$shar) 

fit1 <- glm(dec ~ attr + sinc + intel + fun + amb + shar, 
               data = scorecard, 
               family = binomial(link = "logit"))
summary(fit1)
#1338 obs eliminated for missing values

exp(coef(fit1))
