#Load libraries

library(tidyverse)

# load data as a dplyr tibble

vert_tbl <- as_tibble(read.csv("ruminant_vert_morph_data.csv"))
vert_tbl

abrev <- c("CH", "CL", "CW")
fullname <- c("centrumHeight", "centrumLength", "centrumWidth")

vertData <- vertData %>%
  select(-X,-type, logBMSS) %>%  #remove columns we don't need
  filter(!is.na(value)) %>%  #remove NAs -- not necessary, prcomp will handle it, but nice to do
  filter(vertebra == 'C2') %>% #look at just the second cervical vertebra (C2)
  mutate(measureName = str_replace_all( #add column with full names of measurements for non-morphologists
    measure,
    c("CH" = "centrumHeight", 
      "CL" = "centrumLength", 
      "CW" = "centrumWidth",
      "DL" = "densLength",
      "DW" = "densWidth",
      "NSL" = "neuralSpineLength",
      "NSLA" = "neuralSpineLeverArm",
      "preZD" = "preZygDist",
      "TPLA" = "transProcessLeverArm"
      )
  )) %>%
  select(catNum, vertebra, measure, measureName, value, logValue, sex, spp, logBodyMass = logBMSS, fightStyle) #select columns in different order, rename body mass column

vertData #check tibble

distinct(vertData, catNum, measure)

