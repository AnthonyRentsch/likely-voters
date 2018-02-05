## @knitr load

#set dependencies
library(plyr)
library(readr)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(dplyr)
library(reshape2)
library(caret)
library(broom)
library(survey)
library(randomForest)

#upload CCES data
cces16 <- read_tsv(file = "/Users/anthonyrentsch/Desktop/UMass/thesis/data/CCES16_Common_OUTPUT_Jul2017_VV.tab",
                 col_names = TRUE)
cces12 <- read_tsv(file = "/Users/anthonyrentsch/Desktop/UMass/thesis/data/CCES12_Common_VV.tab",
                   col_names = TRUE)
cces08 <- read_tsv(file = "/Users/anthonyrentsch/Desktop/UMass/thesis/data/cces_2008_common.tab",
                   col_names = TRUE)

#add state abbreviations
states <- data.frame(inputstate = seq(1:56), state_abbreviation = c(
                                         "AL",
                                         "AK",
                                         "",
                                         "AZ",
                                         "AR",
                                         "CA",
                                         "",
                                         "CO",
                                         "CT",
                                         "DE",
                                         "D.C.",
                                         "FL",
                                         "GA",
                                         "",
                                         "HI",
                                         "ID",
                                         "IL",
                                         "IN",
                                         "IA",
                                         "KS",
                                         "KY",
                                         "LA",
                                         "ME",
                                         "MD",
                                         "MA",
                                         "MI",
                                         "MN",
                                         "MS",
                                         "MO",
                                         "MT",
                                         "NE",
                                         "NV",
                                         "NH",
                                         "NJ",
                                         "NM",
                                         "NY",
                                         "NC",
                                         "ND",
                                         "OH",
                                         "OK",
                                         "OR",
                                         "PA",
                                         "",
                                         "RI",
                                         "SC",
                                         "SD",
                                         "TN",
                                         "TX",
                                         "UT",
                                         "VT",
                                         "VA",
                                         "",
                                         "WA",
                                         "WV",
                                         "WI",
                                         "WY"))
cces16 <- left_join(cces16, states, by = "inputstate")
cces12 <- left_join(cces12, states, by = "inputstate")
cces08 <- cces08 %>% rename(inputstate = V206)
cces08 <- left_join(cces08, states, by = "inputstate")

#add names of racial groups
race <- data.frame(race = c(1,2,3,4,5,6,7,8,98,99), racial_group = c("White","Black","Hispanic","Asian","Native American",
                                                       "Mixed","Other","Middle Eastern","Skipped","Not Asked"))
cces16 <- left_join(cces16, race, by = "race")
cces12 <- left_join(cces12, race, by = "race")
cces08 <- cces08 %>% rename(race = V211)
cces08 <- left_join(cces08, race, by = "race")


##############

# create pooled cces data
# for now just handles vote intent, vote history
# cces <- cces08 %>% select(state_abbreviation, CC326, CC326b, CC327, vote_gen08) %>% 
#   rename(state = state_abbreviation, intent = CC326, earlyvotechoice = CC326b, votechoice = CC327,
#          validatedvote = vote_gen08)
# cces$validatedvote <- ifelse(as.integer(cces$validatedvote) == 1, "voted", "did not vote")
 
# upload pooled CCES data
pooled <- readRDS("/Users/anthonyrentsch/Downloads/cumulative_2006_2016.Rds")
## 2008
# no res mobility; need economy variable + other structural variables
cces08_tojoin <- cces08 %>% select(V100, V246, V217, V214, V244, CC301_2, CC301_3, CC335bush, voter_status) %>% 
  rename(case_id = V100, faminc = V246, religiosity = V217, marital_status = V214, 
         newsint = V244, watch_news = CC301_2, read_paper = CC301_3, 
         pres_approval = CC335bush, registration = voter_status)
pooled <- left_join(pooled, cces08_tojoin, by = 'case_id')
  
## 2012

## 2016

# add and recode 8 variables: 
# family income, religiosity, marital status, news interest, residential mobility, 
# watching the news, reading a newspaper, voter registration




############

#upload Catalist turnout data
catalistturnout <- read.csv(file = "/Users/anthonyrentsch/Desktop/UMass/thesis/data/CatalistVoteData_PlusTurnout_Apr28.csv")
