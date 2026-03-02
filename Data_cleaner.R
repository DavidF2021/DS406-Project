###Data Cleaner
library(tidyverse)
library(janitor)
data <- read_csv("Speed_Dating.csv")

data <- data %>%
  clean_names()

glimpse(data)
summary(data)

