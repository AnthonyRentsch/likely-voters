#set dependencies
library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)

#upload data
cces <- read_tsv(file = "/Users/anthonyrentsch/Desktop/UMass/thesis/data/CCES16_Common_OUTPUT_Jul2017_VV.tab",
                 col_names = TRUE)

