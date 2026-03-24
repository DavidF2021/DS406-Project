###Data Cleaner
library(tidyverse)
library(janitor)
library(dplyr)
library(ggplot2)

speed_dating <- read_csv("Speed_Dating.csv")
speed_dating <- speed_dating %>%
  clean_names() %>%
  mutate(index = row_number(), .before = 1) # add index column

#-----------------------------------------------
# Dates informations
## datset with columns related to every single date
dates <- speed_dating %>%
  select(index:met_o)

## scorecard filled after each date + iid,id,partner
scorecard <- speed_dating %>%
  select(c(iid,id,pid,gender,partner,dec:match_es))

#-----------------------------------------------
# datasets with information unique for every iid

## data filled to sign up + iid //Time1
signup <- speed_dating %>%
  select(c(iid, wave,gender, age:amb5_1)) %>% 
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


#-----------------------------------------------
# Data cleaning \anomalies correction
## id
speed_dating[8378,'id'] = 22 # NaN -> 22

## pid
x <-which(is.na(speed_dating$pid))
speed_dating[x,c('index','iid','id','gender','wave','partner','pid')]
y<-which (dates$id == 7 & dates$wave == 5)
dates[y,c('index','iid','id','gender','wave','partner','pid')]
setdiff(min(dates$iid):max(dates$iid), dates$iid) #iid 118 did not participate
# drop rows related to 118
dates <- speed_dating %>%
  filter(!is.na(pid))

### Dates
## age_o to pf_o_sha
# get the attribute of the partner from their signup form
for (i in 1:nrow(dates)) {
  if (is.na(dates$age_o[i])) { 
    dates$age_o[i] <- signup$age[dates$pid[i]]
  } # NAs 104 -> 40
  if (is.na(dates$race_o[i])) { 
    dates$race_o[i] <- signup$race[dates$pid[i]]
  } # NAs 73 -> 40
  if (is.na(dates$pf_o_att[i])) { 
    dates$pf_o_att[i] <- signup$attr1_1[dates$pid[i]]
  } # NAs 89 -> 56
  if (is.na(dates$pf_o_sin[i])) { 
    dates$pf_o_sin[i] <- signup$sinc1_1[dates$pid[i]]
  } # NAs 89 -> 56
  if (is.na(dates$pf_o_int[i])) { 
    dates$pf_o_int[i] <- signup$intel1_1[dates$pid[i]]
  } # NAs 89 -> 56
  if (is.na(dates$pf_o_fun[i])) { 
    dates$pf_o_fun[i] <- signup$fun1_1[dates$pid[i]]
  } # NAs 98 -> 56
  if (is.na(dates$pf_o_amb[i])) { 
    dates$pf_o_amb[i] <- signup$amb1_1[dates$pid[i]]
  } # NAs 107 -> 65
  if (is.na(dates$pf_o_sha[i])) { 
    dates$pf_o_sha[i] <- signup$shar1_1[dates$pid[i]]
  } # NAs 129 -> 65
}

# changing met based on met_o
table(dates$met_o)
table(dates$met)
table(scorecard$met)

scorecard <- scorecard %>%
  left_join(
    dates %>% 
    select(iid, pid, met_o),
    by = c("pid" = "iid", "iid" = "pid")
  ) %>%
  mutate(met = met_o) %>% select(-met_o)

table(dates$met_o)
table(dates$met)
table(scorecard$met)
# wrong entries to NA
scorecard$met[!(scorecard$met %in% c(1, 2))] <- NA
table(scorecard$met)

# attr_o to met_o / the data are missing
#dates <- dates %>% #chat GPT
#  left_join(
#    scorecard %>%
#      select(iid, pid, attr, sinc, intel, fun, amb, shar, like),
#    by = c("pid" = "iid", "iid" = "pid")
#  ) %>%
#  mutate(
#    attr_o  = coalesce(attr_o,  attr),
#    sinc_o  = coalesce(sinc_o,  sinc),
#    intel_o = coalesce(intel_o, intel),
#    fun_o   = coalesce(fun_o,   fun),
#    amb_o   = coalesce(amb_o,   amb),
#    shar_o  = coalesce(shar_o,  shar),
#    like_o  = coalesce(like_o,  like)
#  ) %>%
#  select(-attr, -sinc, -intel, -fun, -amb, -shar, -like)

### Scorecard
# scaling 1-10 to 100pt for wave 6 to 9
for (i in 1:nrow(signup)) {
  if (signup$wave[i] %in% 6:9) {
    tot1 <- sum(signup[i,39:44])
    signup$attr1_1[i] = signup$attr1_1[i]/tot1 *100
    signup$sinc1_1[i] = signup$sinc1_1[i]/tot1 *100
    signup$intel1_1[i] = signup$intel1_1[i]/tot1 *100
    signup$fun1_1[i] = signup$fun1_1[i]/tot1 *100
    signup$amb1_1[i] = signup$amb1_1[i]/tot1 *100
    signup$shar1_1[i] = signup$shar1_1[i]/tot1 *100
    
    tot2 <- sum(signup[i,51:56])
    signup$attr2_1[i] = signup$attr2_1[i]/tot2 *100
    signup$sinc2_1[i] = signup$sinc2_1[i]/tot2 *100
    signup$intel2_1[i] = signup$intel2_1[i]/tot2 *100
    signup$fun2_1[i] = signup$fun2_1[i]/tot2 *100
    signup$amb2_1[i] = signup$amb2_1[i]/tot2 *100
    signup$shar2_1[i] = signup$shar2_1[i]/tot2 *100
    
    tot4 <- sum(signup[i,45:50])
    signup$attr4_1[i] = signup$attr4_1[i]/tot4 *100
    signup$sinc4_1[i] = signup$sinc4_1[i]/tot4 *100
    signup$intel4_1[i] = signup$intel4_1[i]/tot4 *100
    signup$fun4_1[i] = signup$fun4_1[i]/tot4 *100
    signup$amb4_1[i] = signup$amb4_1[i]/tot4 *100
    signup$shar4_1[i] = signup$shar4_1[i]/tot4 *100
  }
}

#-------------------------------------------------
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






