library(reshape2)
library(foreign)
library(tidyverse)

library(plyr)
library(readr)

mydir <-  setwd("C:\\Folder\\Диссертация\\Data\\survey_VCIOM")

myfiles <- list.files(path = mydir, pattern="*.sav", full.names=TRUE)
myfiles

data <- ldply(myfiles, read.spss, to.data.frame=TRUE, use.value.labels = FALSE)
names(data)

selectedVar <- data %>%
  select(id = ID, age = AGE, education = EDU, region = REGION, city_type = city_typ1,
         president_approval = Q2_1, pm_approval = Q2_2, government_approval = Q2_3, pres_represent_approval = Q2_4,
         governor_approval = Q2_5, mayor_approval = Q2_6, state_duma_approval = Q2_7, federal_council_approval = Q2_8,
         polParties_approval = Q2_9, police_approval = Q2_10, media_approval = Q2_11, army_approval = Q2_12,
         laborUnion_approval = Q2_13, courts_approval = Q2_14, curch_approval = Q2_15, politicalOpposition_approval = Q2_16,
         vote_choice = CHOICE, protest = q12, internet = d1, tv = TV, dohod = DOHOD_0, dohod2 = DOHOD, work = PROF, budget = d3,
         income = INCOME2, household = INCOME3, problem1 = q7_1k, problem2 = q7_2k, problem3 = q7_3k,
         problem4 = q7_4k, problem5 = q7_5k, domesticPol = ss3_1, economPol = ss3_2, socalPol = ss3_3, foreignPol = ss3_1,
         lifeSat = ss3_5, econExpect = ss4, rusEcon = ss5_1, rusPol = ss5_2, date_wave, wave)

table(selectedVar$governor_approval)

governors_data <- filter(selectedVar, governor_approval != 99)
table(governors_data$governor_approval) #selecting those who responded 


#RECODING APPROVAL VARIABLE

dataset <- mutate(governors_data, governor_approval = ifelse(governor_approval == 1, 1, 0))#1 - approve, 0 - disapprove

table(dataset$president_approval)
