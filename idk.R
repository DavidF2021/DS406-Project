

#--------------------------
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

# ----------------------------------
 # Try
x <- scorecard[,5:12]
x$avg <- apply(x[,2:7], MARGIN = 1, FUN = mean)
head(x)

x_c <- na.omit(x)
fit2 <- lm(like ~ attr+sinc+intel+fun+amb+shar, data = scorecard)
summary(fit2)

#-------------------------------
## Does having already met the partenr influence the decision?
x <- scorecard[,c(5:11,14)]
x$met <- as.factor(x$met)
x$met <- recode(x$met, '1' = 'No', '2' = 'Yes')

fit3 <- glm(dec ~ attr + sinc + intel + fun + amb + shar + met, 
            data = x, 
            family = binomial(link = 'logit'))
fit4 <- glm(dec ~ met, 
            data = x, 
            family = binomial)
fit5 <- lm (attr ~ met, data = x)
summary(fit3) # met not significative -> it does not influence the decision might influence votes???
summary(fit4) # knowing the person reduces odds
summary(fit5) # knowing the person reduces vote on attr

ggplot(x, aes(met, dec)) +
  geom_point() +
  stat_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE)

