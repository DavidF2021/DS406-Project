data <- read.csv("Speed_Dating.csv")

library(tidyverse)
library(reshape2) # for heatmap
library(pROC)

model_data <- scorecard %>%
  select(dec, gender, attr, intel, fun, shar) %>%
  drop_na()

main_model <- glm(dec ~ attr + intel + fun + shar,
                  data = model_data,
                  family = binomial)

#1 Do people actually prioritise what they say they do?
pref_vs_act <- scorecard %>%
  group_by(iid) %>%
  summarise(avg_attr_given = mean(attr, na.rm = TRUE)) %>%
  left_join(signup %>% select(iid, attr1_1), by = "iid")

ggplot(pref_vs_act, aes(x = attr1_1, y = avg_attr_given)) +
  geom_point(alpha = 0.4, color = "darkblue") +
  geom_abline(slope = 0.1, intercept = 0, linetype = "dashed") + # Adjusted slope for 1-10 vs 1-100 comparison
  labs(title = "The Preference Gap: Stated vs. Actual Attractiveness",
       subtitle = "Comparing pre-event priority (0-100) vs. actual ratings given (1-10)",
       x = "Stated Importance (Time 1 Survey)",
       y = "Average Rating Given to Dates") +
  theme_minimal()

###The graph shows a weak relationship between stated importance of attractiveness 
#(x-axis) and the average attractiveness ratings given to dates (y-axis). 
#Most points are clustered between 10–30 on the x-axis, but their corresponding y-values are widely spread (roughly 4–8), 
#indicating that people with similar stated preferences behave quite differently. 
#Additionally, the points do not lie close to the dashed diagonal line (which represents perfect consistency), 
#showing that participants actual behaviour does not align closely with what they claim is important.

#2 Interaction Model
coef_df <- data.frame(
  Variable = names(coef(main_model)),
  OddsRatio = exp(coef(main_model))   
) %>%
  filter(Variable != "(Intercept)")

ggplot(coef_df, aes(x = reorder(Variable, OddsRatio), y = OddsRatio)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Attribute Importance in Dating Decisions",
       x = "Attribute",
       y = "Odds Ratio") +
  theme_minimal()

###The results show that attractiveness is the most influential factor in predicting 
#a positive dating decision, followed by shared interests and fun. In contrast, 
#intelligence does not have a significant positive effect, suggesting that immediate social and 
#physical attributes dominate early romantic choices.
#The results show that attractiveness is the most influential factor in 
#predicting a positive dating decision, followed by shared interests and fun. 
#In contrast, intelligence does not have a significant positive effect, 
#suggesting that immediate social and physical attributes dominate early romantic choices.

#3 Visualising Gender paradox
#Why the interaction matters

fit_interaction <- glm(dec ~ gender * (attr + intel + fun + shar), 
                       data = scorecard, 
                       family = binomial(link = "logit"))
summary(fit_interaction)
# Interaction plot
# This visualises the "Gender Paradox" found in results
effect_data <- expand.grid(
  attr = seq(1, 10, by = 0.5),
  gender = c(0, 1), # 0 = Female, 1 = Male
  intel = 7, sinc = 7, fun = 7, shar = 7
)

# Predict probabilities
effect_data$prob <- predict(fit_interaction, newdata = effect_data, type = "response")

# Convert gender to labels for the plot
effect_data$gender_label <- factor(effect_data$gender, levels = c(0, 1), labels = c("Female", "Male"))

ggplot(effect_data, aes(x = attr, y = prob, color = gender_label)) +
  geom_line(size = 1.2) +
  labs(title = "Gender Interaction: Impact of Attractiveness",
       subtitle = "Probability of 'Yes' increases faster for Men as Attractiveness rises",
       x = "Attractiveness Rating (1-10)",
       y = "Predicted Probability of 'Yes'",
       color = "Gender") +
  theme_minimal()

# Interpretation: If gender1:attr is significant, 
# it means the impact of attractiveness on 'dec' varies by gender.

#4 Model Validation 

scorecard_clean <- na.omit(scorecard[, c("dec", "gender", "attr", "intel", "fun", "shar")])
probs <- predict(fit_interaction, newdata = scorecard_clean, type = "response")
preds <- ifelse(probs > 0.5, 1, 0)

# Confusion Matrix
cm <- table(Predicted = preds, Actual = scorecard_clean$dec)
print(cm)

# Accuracy Calculation 
acc_value <- sum(diag(cm)) / sum(cm)
print(paste0("Model Accuracy: ", round(acc_value * 100, 2), "%"))

# ROC Curve 
roc_obj <- roc(scorecard_clean$dec, probs)
plot(roc_obj, main=paste("ROC Curve (AUC =", round(auc(roc_obj), 3), ")"), col="blue")
abline(a=0, b=1, lty=2, col="red")

###
#The ROC curve lies well above the line of no discrimination, and the AUC of 0.826 
#indicates strong predictive performance. This suggests that the model captures meaningful 
#relationships between the attributes and dating decisions.

#5 Interest correlation heatmap
# Calculate correlation of interests
interests_corr <- round(cor(signup %>% select(sports:yoga), use = "complete.obs"), 2)

# Melt and Plot
melted_corr <- melt(interests_corr)
ggplot(data = melted_corr, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", limit = c(-1,1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title = "Correlation Between Different Interests")

###
#These patterns indicate that shared interests may reflect underlying lifestyle 
#compatibility, which could play a role in dating decisions.
