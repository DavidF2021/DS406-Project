

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
#sometimes there are in-between values
table(scorecard_a$dec) #ok
table(scorecard_a$attr) #there's a 9.9 entry
table(scorecard_a$sinc) 
table(scorecard_a$intel) 
table(scorecard_a$fun)
table(scorecard_a$amb)
table(scorecard_a$match_es) 
 

fit1 <- glm(dec ~ attr + sinc + intel + fun + amb + shar, 
               data = scorecard, 
               family = binomial(link = "logit"))
summary(fit1)
#1338 obs eliminated for missing values
#1334/8378 16% of data is missing 

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
#model fitted taking into account gender
fit_women <- glm(dec ~ attr + sinc + intel + fun + amb + shar, 
                 data = subset(scorecard, gender == "Female"), family = "binomial")

fit_men <- glm(dec ~ attr + sinc + intel + fun + amb + shar, 
               data = subset(scorecard, gender == "Male"), family = "binomial")

#what woman look in man
fit_women <- glm(dec ~ attr1_1 + sinc1_1 + intel1_1 + fun1_1 + amb1_1 + shar1_1, 
                 data = subset(signup, gender == "Female"), family = "binomial")


pref_self <- c("attr1_1", "sinc1_1", "intel1_1", "fun1_1", "amb1_1", "shar1_1") 
pref_opp <- c("attr2_1", "sinc2_1", "intel2_1", "fun2_1", "amb2_1", "shar2_1")


attr_names <- c("Attractive", "Sincere", "Intelligent", "Fun", "Ambitious", "Shared Interests")
idx_f<-which(signup$gender == 0) #rows with female participants
idx_m<-which(signup$gender == 1) #rows with male participants
#looking at difference in expectation bewteen genders

f_reality <- colMeans(signup[idx_f, pref_self], na.rm = TRUE) 
m_percept <- colMeans(signup[idx_m, pref_opp], na.rm = TRUE) 

m_reality <- colMeans(signup[idx_m, pref_self], na.rm = TRUE)
f_percept <- colMeans(signup[idx_f, pref_opp], na.rm = TRUE) 

#Dataframe to help building a graphic
plot_data <- data.frame(
  Attribute = rep(attr_names, 4),
  Value = c(f_reality, m_percept, m_reality, f_percept),
  Group = rep(c("Reality (Women)", "Men's Perception", "Reality (Men)", "Women's Perception"), each = 6),
  TargetGender = rep(c("What Women Want", "What Men Want"), each = 12)
)


#graphic
ggplot(plot_data, aes(x = Attribute, y = Value, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~TargetGender) +
  scale_fill_brewer(palette = "Set1") + 
  theme_bw() + 
  labs(title = "Gender Perception Bias Analysis",
       subtitle = "Comparison between stated preferences and partner's expectations",
       y = "Average Score (Total 100 points)", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

#looking for signifcant differences with t-test
for (i in 1:6) {
  t_test <- t.test(signup[idx_f, pref_self[i]], signup[idx_m, pref_opp], na.rm = TRUE)
  print(paste("The p-value for differences in the variable", attr_names[i], "is", round(t_test$p.value,4)))
}

for (i in 1:6) {
  t_test <- t.test(signup[idx_m, pref_self[i]], signup[idx_f, pref_opp], na.rm = TRUE)
  print(paste("The p-value for differences in the variable", attr_names[i], "is", round(t_test$p.value,4)))
}

# Does age difference influence the match?
agediff <- abs(speed_dating$age - speed_dating$age_o)
x <- data.frame(
  match = speed_dating$match,
  agediff = agediff
  )

mod <- glm(match ~ agediff, data = x, family = binomial(link='logit'))
summary(mod)
exp(coef(mod))
ggplot(x, aes(y = match, x = agediff)) +
  geom_point(size = .1) +
  geom_jitter(width = .1, height = .1)+
  stat_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE)

# How age difference influence the decision?
x <- data.frame(
  dec = speed_dating$dec,
  agediff = speed_dating$age - speed_dating$age_o
)

mod <- glm(dec ~ agediff, data = x, family = binomial(link='logit'))
summary(mod)
exp(coef(mod))
ggplot(x, aes(y = dec, x = agediff)) +
  geom_point(size = .1) +
  geom_jitter(width = .1, height = .1)+
  stat_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE)

# Are age outliers influence our models?
which(speed_dating$age > 35)
which(signup$age >35)
x <- speed_dating[,c('gender','age','age_o','dec')]
x <- x[x$age <= 35,]
x$agediff <- x$age - x$age_o
which(x$age >35)

mod <- glm(dec ~ agediff+gender, data = x, family = binomial(link='logit'))
summary(mod)
exp(coef(mod))
ggplot(x, aes(y = dec, x = agediff)) +
  geom_point(size = .1) +
  geom_jitter(width = .1, height = .1)+
  stat_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE)

mod <- glm(dec ~ agediff, data = x, family = binomial(link='logit'))
summary(mod)
exp(coef(mod))
ggplot(x, aes(y = dec, x = agediff)) +
  geom_point(size = .1) +
  geom_jitter(width = .1, height = .1)+
  stat_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE)
breaks <- seq(15,60,by=2)
hist(signup$age, breaks = breaks)
x <- signup$age
x<-x[x<40]
breaks <- seq(15,40,by=2)
hist(x, breaks = breaks)


# Does opinions change before/after
# (differences in signup,halfway,follow up1-2)
colnames(signup)
x <- cbind(signup[,c(3,40:45,58:62)],halfway[,-1])

# dependence of Nan to gender
count_na(x)
rowSums(is.na(x)) # everybody who didnt answer, did not answer to all

rowSums(is.na(signup[,c(40:45,58:62)]))


colSums(is.na(x))

check_dep <- function(x) {
  x$M1_1 <- is.na(x[, 2])
  tb <- table(x[, c(1, 3)])
  print(tb)
  tt <- chisq.test(tb)
  print(tt)
  return(tt$p.value)
}

pval <- numeric(ncol(x) - 1)

for (i in 2:ncol(x)) {
  pval[i - 1] <- check_dep(x[, c(1, i)])
}

which(pval < 0.05) # no significant pval no response is not infulenced by gender
