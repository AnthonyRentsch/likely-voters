## @knitr load

# set dependencies
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

# upload CCES data
cces16 <- read_tsv(file = "/Users/anthonyrentsch/Desktop/UMass/thesis/data/CCES16_Common_OUTPUT_Jul2017_VV.tab",
                 col_names = TRUE)
cces12 <- read_tsv(file = "/Users/anthonyrentsch/Desktop/UMass/thesis/data/CCES12_Common_VV.tab",
                   col_names = TRUE)
cces08 <- read_tsv(file = "/Users/anthonyrentsch/Desktop/UMass/thesis/data/cces_2008_common.tab",
                   col_names = TRUE)

# add state abbreviations
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

# add names of racial groups
race <- data.frame(race = c(1,2,3,4,5,6,7,8,98,99), racial_group = c("White","Black","Hispanic","Asian","Native American",
                                                       "Mixed","Other","Middle Eastern","Skipped","Not Asked"))
cces16 <- left_join(cces16, race, by = "race")
cces12 <- left_join(cces12, race, by = "race")
cces08 <- cces08 %>% rename(race = V211)
cces08 <- left_join(cces08, race, by = "race")

##############
 
## upload pooled CCES data
pooled <- readRDS("/Users/anthonyrentsch/Downloads/cumulative_2006_2016.Rds")

## 2008
# need vote history!
cces08_tojoin <- cces08 %>% select(V100, V246, V217, V214, V244, CC301_2, CC301_3, CC335bush, voter_status, CC334, CC326) %>% 
  rename(case_id = V100, income = V246, religiosity = V217, marital_status = V214, 
         interest = V244, registration = voter_status, residential_mobility = CC334, 
         intent = CC326)
pooled <- left_join(pooled, cces08_tojoin, by = 'case_id')
  
## 2012
cces12_tojoin <- cces12 %>% select(V101, faminc, pew_churatd, marstat, newsint,
                                   CC351, votereg, CC308a, CC354, CC316) %>% 
  rename(case_id = V101, income = faminc, religiosity = pew_churatd, marital_status = marstat,
         interest = newsint, residential_mobility = CC351, registration = votereg, 
         intent = CC354, vote_history = CC316)
pooled <- left_join(pooled, cces12_tojoin, by = 'case_id')

## 2016
cces16_tojoin <- cces16 %>% select(V101, faminc, pew_churatd, marstat, newsint,
                                   CC16_361, votereg, CC16_320a, CC16_364, CC16_326) %>% 
  rename(case_id = V101, income = faminc, religiosity = pew_churatd, marital_status = marstat,
         interest = newsint, residential_mobility = CC16_361, registration = votereg, 
         intent = CC16_364, vote_history = CC16_326)
pooled <- left_join(pooled, cces16_tojoin, by = 'case_id')

# add structural variables from Abramowitz Time for Change model:

# annualized growth rate of real GDP in 2nd quarter of election year
# taken from U.S. Department of Commerce's Bureau of Economic Analysis (BEA)
pooled$gdp_growth[pooled$year == 2008] <- 1.9
pooled$gdp_growth[pooled$year == 2008] <- 1.5
pooled$gdp_growth[pooled$year == 2008] <- 1.1

# Presidential approval
# Incumbent (party's) net approval (approve-disapprove) rating in final Gallup poll in June
pooled$pres_approval[pooled$year == 2008] <- 28-68
pooled$pres_approval[pooled$year == 2008] <- 47-46
pooled$pres_approval[pooled$year == 2008] <- 51-45

# indicator variable for whether or not an incumbent was running
pooled$incumbent[pooled$year == 2008] <- 0
pooled$incumbent[pooled$year == 2008] <- 1
pooled$incumbent[pooled$year == 2008] <- 0

# polarization variable (Abramowitz 2012)
# 1 if first-term incumbent running or open seat where incumbent president has net approval over 0
# -1 if no first-term incumbent or incumbent president has net approval less than 0
pooled$polarization[pooled$year == 2008] <- -1
pooled$polarization[pooled$year == 2008] <- 1
pooled$polarization[pooled$year == 2008] <- -1

# collapse these variables into one and get rid of all unneeded ones
# income
pooled$income[pooled$year == 2008] <- pooled$income.x[pooled$year == 2008]
pooled$income[pooled$year == 2012] <- pooled$income.y[pooled$year == 2012]
# religiosity
pooled$religiosity[pooled$year == 2008] <- pooled$religiosity.x[pooled$year == 2008]
pooled$religiosity[pooled$year == 2012] <- pooled$religiosity.y[pooled$year == 2012]
# marital status
pooled$marital_status[pooled$year == 2008] <- pooled$marital_status.x[pooled$year == 2008]
pooled$marital_status[pooled$year == 2012] <- pooled$marital_status.y[pooled$year == 2012]
# interest
pooled$interest[pooled$year == 2008] <- pooled$interest.x[pooled$year == 2008]
pooled$interest[pooled$year == 2012] <- pooled$interest.y[pooled$year == 2012]
# residential mobility
pooled$residential_mobility[pooled$year == 2008] <- pooled$residential_mobility.x[pooled$year == 2008]
pooled$residential_mobility[pooled$year == 2012] <- pooled$residential_mobility.y[pooled$year == 2012]
# registration
pooled$registration[pooled$year == 2008] <- pooled$registration.x[pooled$year == 2008]
pooled$registration[pooled$year == 2012] <- pooled$registration.y[pooled$year == 2012]
# intent
pooled$intent[pooled$year == 2008] <- pooled$intent.x[pooled$year == 2008]
pooled$intent[pooled$year == 2012] <- pooled$intent.y[pooled$year == 2012]
# vote history
# STUFF HERE

# recode intent variable -- 2012 does not line up with 2008 and 2016
pooled$intent[pooled$year == 2012 & pooled$intent == 4] <- 1 #plan to vote to definitely vote
pooled$intent[pooled$year == 2012 & pooled$intent == 5] <- 4 #no to no
pooled$intent[pooled$year == 2012 & pooled$intent == 6] <- 5 #undecided to undecided

# recode vote history variable
# STUFF HERE

# select relevant variables and filter 2008, 2012, and 2016
pooled <- pooled %>% filter(year %in% c(2008, 2012, 2016)) %>% 
  select(year, case_id, weight, state, pid7, gender, age, race, educ, vv_turnout_gvm, 
         intent_pres_16, income, religiosity, marital_status, interest, residential_mobility,
         registration, intent, gdp_growth, pres_approval, incumbent, polarization) %>% 
  rename(validated = vv_turnout_gvm, choice = intent_pres_16)


############

#upload Catalist turnout data
catalistturnout <- read.csv(file = "/Users/anthonyrentsch/Desktop/UMass/thesis/data/CatalistVoteData_PlusTurnout_Apr28.csv")
