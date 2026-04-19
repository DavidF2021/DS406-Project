library(tidyverse)

#mean votes received for every partecpiant, by attribute 
popularity_scores <- speed_dating %>%
  group_by(iid) %>%
  summarise(
    attractive = mean(attr_o, na.rm = TRUE),
    sincere = mean(sinc_o, na.rm = TRUE),
    intelligent = mean(intel_o, na.rm = TRUE),
    fun= mean(fun_o, na.rm = TRUE),
    ambitious=mean(amb_o, na.rm = TRUE),
    shared_interest=mean(shar_o, na.rm = TRUE)
  )

#joining  with followup2 
final_outcome <- followup2 %>%
  select(iid, date_3) %>%
  inner_join(popularity_scores, by = "iid") %>%
  filter(date_3 %in% c(0, 1)) %>%
  mutate(RealDate = ifelse(date_3 == 1, "Yes", "No"))

final_outcome<- as.data.frame(final_outcome)

#for every atrributes I checked if there is a difference of rating between people who actually had
#a date or not
attributes<-c("attractive","sincere", "intelligent", "fun","ambitious","shared_interest")

for (attr in attributes){
  t_test <- t.test(final_outcome[,attr] ~ final_outcome[,'RealDate'])
  print(paste("The p-value for differences in the variable", attr, "is", round(t_test$p.value,4)))
  
  p<-ggplot(final_outcome, aes(x = RealDate, y =.data[[attr]], fill = RealDate)) +
    geom_boxplot(alpha = 0.6) +
    theme_minimal() +
    labs(
      title = paste("Does being highly rated in", attr,  "lead to a real date?"),
      subtitle = paste("T-test p-value:", round(t_test$p.value, 3)),
      x = "Actual Date after 3-4 weeks",
      y = paste("Average", attr, "Rating Received")
    )
  print(p)
}




