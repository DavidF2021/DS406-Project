#-------------------
# Speed Dating Analysis
# DS406 - Group 13
#-------------------

library(tidyverse)
library(janitor)
library(reshape2)
library(pROC)

#-------------------
# 1. DATA LOADING & CLEANING


data <- read.csv("Speed_Dating.csv")

speed_dating <- data %>%
  clean_names() %>%
  mutate(index = row_number(), .before = 1)

# Single missing id value
speed_dating[8378, 'id'] <- 22

# --- Split into thematic sub-datasets ---

dates <- speed_dating %>% select(index:met_o)

scorecard <- speed_dating %>%
  select(iid, id, pid, gender, partner, dec:match_es) %>%
  mutate(
    gender = recode(factor(gender), '0' = 'Female', '1' = 'Male'),
    dec    = recode(factor(dec),    '0' = 'No',     '1' = 'Yes')
  )

signup <- speed_dating %>%
  select(iid, wave, gender, age:amb5_1) %>%
  distinct() %>%
  mutate(
    gender = recode(factor(gender), '0' = 'Female', '1' = 'Male'),
    race   = recode(factor(race),
                    '1' = 'Black/African American',
                    '2' = 'European/Caucasian-American',
                    '3' = 'Latino/Hispanic American',
                    '4' = 'Asian/Pacific Islander/Asian-American',
                    '5' = 'Native American',
                    '6' = 'Other')
  )

halfway  <- speed_dating %>% select(iid, attr1_s:amb3_s) %>% distinct()
followup1 <- speed_dating %>% select(iid, satis_2:amb5_2) %>% distinct()
followup2 <- speed_dating %>% select(iid, you_call:amb5_3) %>% distinct()

# --- Fix known anomalies ---



# Drop rows where pid is NA (iid 118 did not participate)
dates <- dates %>% filter(!is.na(pid))

# Impute missing partner attributes from their signup form
for (i in 1:nrow(dates)) {
  pid_i <- dates$pid[i]
  if (is.na(dates$age_o[i]))    dates$age_o[i]    <- signup$age[pid_i]
  if (is.na(dates$race_o[i]))   dates$race_o[i]   <- signup$race[pid_i]
  if (is.na(dates$pf_o_att[i])) dates$pf_o_att[i] <- signup$attr1_1[pid_i]
  if (is.na(dates$pf_o_sin[i])) dates$pf_o_sin[i] <- signup$sinc1_1[pid_i]
  if (is.na(dates$pf_o_int[i])) dates$pf_o_int[i] <- signup$intel1_1[pid_i]
  if (is.na(dates$pf_o_fun[i])) dates$pf_o_fun[i] <- signup$fun1_1[pid_i]
  if (is.na(dates$pf_o_amb[i])) dates$pf_o_amb[i] <- signup$amb1_1[pid_i]
  if (is.na(dates$pf_o_sha[i])) dates$pf_o_sha[i] <- signup$shar1_1[pid_i]
}

# Align met/met_o across scorecard and dates
scorecard <- scorecard %>%
  left_join(dates %>% select(iid, pid, met_o), by = c("pid" = "iid", "iid" = "pid")) %>%
  mutate(met = met_o, met = na_if(met, !(met %in% c(1, 2)))) %>%
  select(-met_o)

# Rescale wave 6-9 preferences from points to percentage (waves 6-9 used 1-10 instead of 100-pt)
for (i in 1:nrow(signup)) {
  if (signup$wave[i] %in% 6:9) {
    for (cols in list(39:44, 51:56, 45:50)) {
      tot <- sum(signup[i, cols], na.rm = TRUE)
      if (tot > 0) signup[i, cols] <- signup[i, cols] / tot * 100
    }
  }
}

#-------------------
# 2. DESCRIPTIVE ANALYSIS


ggplot(signup, aes(x = gender, y = age, fill = gender)) +
  geom_violin(alpha = 0.25, trim = FALSE) +
  #geom_boxplot(width = 0.15, alpha = 0.8, outlier.shape = 21, outlier.size = 2) +
  geom_jitter(width = 0.02, alpha = 0.3, size = 1.2, color = "grey30") +
  scale_fill_manual(values = c("Female" = "red", "Male" = "blue")) +
  labs(title = "Age Distribution by Gender",
       subtitle = "Violin + boxplot with individual participants overlaid",
       x = "Gender", y = "Age") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title    = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "grey50"),
        panel.grid.major.x = element_blank())
 # --- Decision rate by gender ---
table(scorecard$gender, scorecard$dec)


interests_corr <- round(cor(signup %>% select(sports:yoga), use = "complete.obs"), 2)

melt(interests_corr) %>%
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", limits = c(-1, 1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Correlation Between Participant Interests")


#-------------------
# 3. WHAT DRIVES THE DATING DECISION?


# --- Main logistic regression model ---
model_data <- scorecard %>%
  select(dec, gender, attr, intel, fun, shar, amb, sinc) %>%
  drop_na()
main_model <- glm(dec ~ attr + intel + fun + shar + amb +sinc,
                  data = model_data,
                  family = binomial)
summary(main_model)

# Odds ratios plot
coef_df <- data.frame(
  Variable = names(coef(main_model)),
  OddsRatio = exp(coef(main_model))
) %>% filter(Variable != "(Intercept)")

ggplot(coef_df, aes(x = reorder(Variable, OddsRatio), y = OddsRatio)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = 1, color = "red", linetype = "dashed", linewidth = 0.8) +
  coord_flip() +
  labs(title = "Attribute Importance in Dating Decisions",
       x = "Attribute", y = "Odds Ratio") +
  theme_minimal()

# --- Does gender moderate the effect of attractiveness? ---
fit_interaction <- glm(dec ~ gender * (attr + intel + fun + shar),
                       data = scorecard,
                       family = binomial)
summary(fit_interaction)

effect_data <- expand.grid(
  attr = seq(1, 10, by = 0.5),
  gender = c("Female", "Male"),
  intel = 7, fun = 7, shar = 7
)
effect_data$prob <- predict(fit_interaction, newdata = effect_data, type = "response")

ggplot(effect_data, aes(x = attr, y = prob, color = gender)) +
  geom_line(size = 1.2) +
  labs(title = "Gender Interaction: Impact of Attractiveness on P(Yes)",
       x = "Attractiveness Rating (1–10)",
       y = "Predicted Probability of 'Yes'",
       color = "Gender") +
  theme_minimal()

# --- Model validation ---
scorecard_clean <- na.omit(scorecard[, c("dec", "gender", "attr", "intel", "fun", "shar")])
probs <- predict(fit_interaction, newdata = scorecard_clean, type = "response")
preds <- ifelse(probs > 0.5, "Yes", "No")

cm <- table(Predicted = preds, Actual = scorecard_clean$dec)
print(cm)
cat("Accuracy:", round(sum(diag(cm)) / sum(cm) * 100, 2), "%\n")

roc_obj <- roc(scorecard_clean$dec, probs)
plot(roc_obj, main = paste("ROC Curve (AUC =", round(auc(roc_obj), 3), ")"),
     col = "blue")
abline(a = 0, b = 1, lty = 2, col = "red")




#-------------------
# 4. DO PEOPLE ACCURATELY PERCEIVE HOW OTHERS SEE THEM?


# Self-ratings from signup (how participants rate themselves)
self_ratings <- signup %>%
  select(iid, attr3_1, sinc3_1, intel3_1, fun3_1, amb3_1) %>%
  rename(attr_self = attr3_1, sinc_self = sinc3_1, intel_self = intel3_1,
         fun_self = fun3_1, amb_self = amb3_1)

# Average ratings received from dates
others_ratings <- speed_dating %>%
  group_by(iid) %>%
  summarise(
    attr_others  = mean(attr_o,  na.rm = TRUE),
    sinc_others  = mean(sinc_o,  na.rm = TRUE),
    intel_others = mean(intel_o, na.rm = TRUE),
    fun_others   = mean(fun_o,   na.rm = TRUE),
    amb_others   = mean(amb_o,   na.rm = TRUE)
  )

self_vs_others <- self_ratings %>%
  inner_join(others_ratings, by = "iid") %>%
  drop_na() %>%
  pivot_longer(-iid,
               names_to  = c("Attribute", "Source"),
               names_sep = "_",
               values_to = "Rating") %>%
  mutate(
    Attribute = recode(Attribute,
                       "attr" = "Attractive", "sinc" = "Sincere", "intel" = "Intelligent",
                       "fun"  = "Fun",        "amb"  = "Ambitious"),
    Source = recode(Source, "self" = "Self-Rating", "others" = "Rated by Others")
  )

ggplot(self_vs_others, aes(x = Attribute, y = Rating, fill = Source)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Self-Perception vs. How Others Actually Rate You",
       subtitle = "Do people know how they come across?",
       x = "Attribute", y = "Rating (1–10)", fill = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")

# T-tests per attribute
for (attr in unique(self_vs_others$Attribute)) {
  vals <- self_vs_others %>% filter(Attribute == attr)
  tt <- t.test(Rating ~ Source, data = vals)
  cat("Self vs. others -", attr, ": p =", round(tt$p.value, 4), "\n")
}


#-------------------
# 5. DOES AGE DIFFERENCE AFFECT DECISIONS?


age_data <- speed_dating %>%
  filter(age <= 35) %>%
  filter(age_o <= 35) %>%
  mutate(agediff = age - age_o,
         gender  = recode(factor(gender), '0' = 'Female', '1' = 'Male')) %>%
  select(dec, agediff, gender) %>%
  drop_na()

mod_age <- glm(dec ~ agediff + gender, data = age_data, family = binomial)
summary(mod_age)
exp(coef(mod_age))

ggplot(age_data, aes(x = agediff, y = dec, color = gender, fill = gender)) +
  geom_jitter(width = 0.1, height = 0.05, size = 0.3, alpha = 0.2) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, alpha = 0.15) +
  labs(title = "Effect of Age Difference on Dating Decision by Gender",
       subtitle = "Positive age difference = participant is older than partner",
       x = "Age Difference (self – partner)", y = "Predicted P(Yes)",
       color = "Gender", fill = "Gender") +
  theme_minimal()

# --- Decision rate by gender ---
table(scorecard$gender, scorecard$dec)

#-------------------
# 6. DO HIGH RATINGS LEAD TO REAL DATES?  (Follow-up outcome)


popularity_scores <- speed_dating %>%
  group_by(iid) %>%
  summarise(across(c(attr_o, sinc_o, intel_o, fun_o, amb_o, shar_o),
                   ~mean(.x, na.rm = TRUE),
                   .names = "{sub('_o', '', .col)}"))

final_outcome <- followup2 %>%
  select(iid, date_3) %>%
  inner_join(popularity_scores, by = "iid") %>%
  filter(date_3 %in% c(0, 1)) %>%
  mutate(RealDate = ifelse(date_3 == 1, "Yes", "No"))

attributes <- c("attr", "sinc", "intel", "fun", "amb", "shar")

for (attr in attributes) {
  t_test <- t.test(final_outcome[[attr]] ~ final_outcome$RealDate)
  cat("P-value for", attr, ":", round(t_test$p.value, 4), "\n")
  
  print(
    ggplot(final_outcome, aes(x = RealDate, y = .data[[attr]], fill = RealDate)) +
      geom_boxplot(alpha = 0.6) +
      labs(title = paste("Does high", attr, "rating lead to a real date?"),
           subtitle = paste("T-test p-value:", round(t_test$p.value, 3)),
           x = "Actual Date (3–4 weeks later)",
           y = paste("Average", attr, "Rating Received")) +
      theme_minimal()
  )
}








