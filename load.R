#set dependencies
library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(reshape2)

#upload CCES data
cces16 <- read_tsv(file = "/Users/anthonyrentsch/Desktop/UMass/thesis/data/CCES16_Common_OUTPUT_Jul2017_VV.tab",
                 col_names = TRUE)
cces12 <- read_tsv(file = "/Users/anthonyrentsch/Desktop/UMass/thesis/data/CCES12_Common_VV.tab",
                   col_names = TRUE)

#add state abbreviations
states <- data.frame(x = seq(1:56), y =c("AL",
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
names(states) <- c("inputstate","state_abbreviation")
cces16 <- left_join(cces16, states, by = "inputstate")
cces12 <- left_join(cces12, states, by = "inputstate")

#add names of racial groups
race <- data.frame(x = c(1,2,3,4,5,6,7,8,98,99), y = c("White","Black","Hispanic","Asian","Native American",
                                                       "Mixed","Other","Middle Eastern","Skipped","Not Asked"))
names(race) <- c("race","racial_group")
cces16 <- left_join(cces16, race, by = "race")
cces12 <- left_join(cces12, race, by = "race")

#upload Catalist turnout data
catalistturnout <- read.csv(file = "/Users/anthonyrentsch/Desktop/UMass/thesis/data/CatalistVoteData_PlusTurnout_Apr28.csv")
  