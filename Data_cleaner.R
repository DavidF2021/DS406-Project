###Data Cleaner
library(tidyverse)
library(janitor)
speed_dating <- read_csv("Speed_Dating.csv")

speed_dating <- data %>%
  clean_names()

glimpse(speed_dating)
summary(speed_dating)
#-----------------------------------------------
# Dates informations
## datset with columns related to every single date
dates <- speed_dating %>%
  select(iid:met_o)

## scorecard filled after each date + iid,id,partner
scorecard <- speed_dating %>%
  select(c(iid,id,partner,dec:match_es))

#-----------------------------------------------
# datasets with information unique for every iid

## data filled to sign up + iid //Time1
signup <- speed_dating %>%
  select(c(iid, age:amb5_1)) %>% 
  distinct() #-- collapsed, 551 distinct iid

## data filled half way thru + iid
halfway <- speed_dating %>%
  select(c(iid,attr1_s:amb3_s)) %>% 
  distinct() #-- collapsed, 551 distinct iid

## data filled the day after + iid //Time2
followup1 <- speed_dating %>%
  select(c(iid,satis_2:amb5_2)) %>% 
  distinct() #-- collapsed, 551 distinct iid

## data filled 3/4 weeks after + iid //Time3
followup2 <- speed_dating %>%
  select(c(iid,you_call:amb5_3)) %>%
  distinct()

## to count NA in every column of a dataset
count_na <- function(x){
  n <- ncol(x)
  rows <- vector("list", n)
  
  for (j in 1:n) {
    na <- sum(is.na(x[, j]))
    non_na <- nrow(x) - na
    
    rows[[j]] <- data.frame(
      col = colnames(x)[j],
      non_na = non_na,
      na = na,
      stringsAsFactors = FALSE
    )
  }
  
  do.call(rbind, rows)
}

git add Data_cleaner.R
git commit -m "updated data cleaning script"
git push

