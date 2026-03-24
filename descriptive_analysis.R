library(dplyr)
library(ggplot2)

# Age vs Gender

signup <- signup %>% 
  mutate(
    gender = factor(gender),
    gender = recode(gender,
                    '0' = 'Female',
                    '1' = 'Male'),
    race = factor(race),
    race = recode(race,
                  '1'='Black/African American',
                  '2'='European/Caucasian-American',
                  '3'='Latino/Hispanic American',
                  '4'='Asian/Pacific Islander/Asian-American',
                  '5'='Native American',
                  '6'='Other'
    )
  )

ggplot(signup, aes(y = age, x = gender))+
  geom_boxplot() +
  #geom_jitter(width = 0.2, alpha = 0.3) +
  theme_minimal() +
  labs(title = 'Age distribution by Gender ',
       x = 'Gender', y = 'Age') +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    plot.title = element_text(hjust = 0.5)
  )

ggplot(signup, aes(y = age, x = gender, fill = race))+
  geom_boxplot() +
  #geom_jitter(width = 0.2, alpha = 0.3) +
  theme_minimal() +
  labs(title = 'Age distribution by Gender and Race ',
       x = 'Gender', y = 'Age') +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    plot.title = element_text(hjust = 0.5)
  )

# Partecipants interests

boxplot(signup[,c(21:38)], ylim = c(0,10))

# expnum, date, go_out, goal
# match_es
# match

# Who said no the most
scorecard <- scorecard %>%
  mutate(
    gender = factor(gender),
    gender = recode(gender,
                    '0' = 'Female',
                    '1' = 'Male'),
    dec = factor(dec),
    dec = recode(dec,
                 '0'='No',
                 '1'='Yes')
  )

table(scorecard$gender, scorecard$dec)

