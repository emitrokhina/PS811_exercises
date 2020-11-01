####Exercise 7 for PS811
###Evgeniya Mitrokhia
#31.10.2020

###1
#CReating data ftrame
new_data_just <- data.frame(Justice =  c("Clarence Thomas", "Ruth Bader Ginsburg", "Stephen Breyer", "John Roberts", 
            "Samuel Alito", "Sonia Sotomayor", "Elena Kagan", "Neil Gorsuch", "Brett Kavanaugh"),
State =  c("GA", 'NY', 'MA', 'MD', 'NJ', 'NY', 'MA', 'CO', 'MD'),
Position = c(rep("Associate Justice", 3), "Chief Justice", rep("Associate Justice", 5)),
Replacing =  c("Thurgood Marshall", "Byron White", "Harry Blackmun", "William Rehnquist",
              "Sandra Day O’Connor", "David Souter", "John Paul Stevens", "Antonin Scalia",
              "Anthony Kennedy"),
Year_Confirmed = c(1991, 1993, 1994, 2005, 2006, 2009, 2010, 2017, 2018),
Senate_confirmation_vote =  c("52-48", "96-3", "87-9", "78-22", "58-42", "68-31",  
                             "63-37", "54-45", "50-48"),
Nominated_by =  c("George H.W. Bush", "Bill Clinton", "Bill Clinton", "George W.Bush",
                 "George W.Bush", "Barack Obama", "Barack Obama", "Donald Trump", "Donald Trump"))



###2
#uploading file
# ms: shouldn't this be saved as a .csv file?
justies <- read.csv("justices.txt")

###3
# ms: load packages at the top of your .R file
library(foreign)
library(dplyr)
library(tidyverse)

scdb <- read.csv("scdb.csv")


names(justies)
names(scdb)

#The name of the variable that we're going to use these two datasets (justiceName) are the same


table(justies$justiceName)
table(scdb$justiceName) 

#Names seem to be the same

#It seems that inner_join is the most appropriate function, because we need to join the frames by common variable name
combinde_justice <- inner_join(justies, scdb, by = "justiceName")
# ms: consider this:
# combinde_justice <- inner_join(justies, scdb, by = c("justiceName", "term")) to avoid having term.x and term.y

###4
new <- combinde_justice %>% drop_na(post_mn)

#there are no missing value for the variable

###5
mq_scores <- combinde_justice %>%
  group_by(term.x) %>%
  summarise(mean = mean(post_mn))

###6
table(combinde_justice$decisionDirection)
names(table(combinde_justice$decisionDirection))
sum(is.na(combinde_justice$decisionDirection)) #no NA values

table(combinde_justice$decisionDirection == "")

subset_justice <- combinde_justice %>% filter(decisionDirection != "")

# ms: consider using case_when() for this...
# ms: also, the decision directions are numeric (see answer key for this)
combinde_justice_recoded <- mutate(combinde_justice, 
         dicision_recoded = ifelse(
          decisionDirection == "conservative", 1, 0),
         dicision_recoded = ifelse(
           decisionDirection == "liberal", -1, dicision_recoded)
        )
           
table(combinde_justice_recoded$dicision_recoded)

#In the dataset there were decision type coded as "", I wasn't able to figure out why,
#so eventually I assumed they are unspecified and recoded them as 0 as well.
#Is it only my problem (the way I uploaded the dataset) or is it smth natural to the data?
# ms: yes, so there are some NA variables in the dataset...I would just leave them as NA and only rescale 1, 2, and 3

vote_direction <- combinde_justice_recoded %>%
  group_by(term.x) %>%
  summarise(mean = mean(dicision_recoded))

###7
# ms: please see answer key for a cleaner way to determine whether there these scores are different/similar
term <- vote_direction[1]
direction <- vote_direction[2]
mq <- mq_scores[,2]


summary_compare <- data.frame(term, direction, mq)
names(summary_compare)

names(summary_compare)[names(summary_compare) == "term.x"] <- "term"
names(summary_compare)[names(summary_compare) == "mean"] <- "vote_direction_mean"
names(summary_compare)[names(summary_compare) == "mean.1"] <- "MQ_score_mean"

print(summary_compare)

#We can see from the table that these scores are different
