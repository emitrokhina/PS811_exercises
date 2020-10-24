#PS-811
#Exercise 6
#Evgeniya Mitrokhina
#23.10.2020

# MS: load all packages


###Base R tasks

#2
# MS: name your objects with more meaningful names
data <- read.csv("food_coded.csv")

#3
data[1:95, ]
head(data,95)

#4
data[, c("GPA", "calories_chicken", "drink", "fav_cuisine", "father_profession", "mother_profession")] 
data[, c(1,4,16,25,26,45)] 

names(data)

#5
table(data$healthy_feeling)

data$healthy_feeling_new <- data$healthy_feeling * 10 #check it 

table(data$healthy_feeling_new)

#6
subset(data, Gender  == 1 & GPA >=3)

#7
data[order(data$fav_cuisine), ]

#8
new_data <- data.frame(chicken_calories.mean = mean(data$calories_chicken, na.rm = T),
                       chicken_calories.sd = sd(data$calories_chicken, na.rm = T),
                       tortilla_calories.mean = mean(data$tortilla_calories, na.rm = T),
                       tortilla_calories.sd = sd(data$tortilla_calories, na.rm = T),
                       turkey_calories.mean = mean(data$turkey_calories, na.rm = T),
                       turkey_calories.sd = sd(data$turkey_calories, na.rm = T),
                       waffle_calories.mean = mean(data$waffle_calories, na.rm = T),
                       waffle_calories.sd = sd(data$waffle_calories, na.rm = T)
                       )

#9
aggregate(formula = cbind(GPA, weight) ~ cuisine  + Gender,
          data = data,
          FUN = function(x){
            c(mean = mean(x), sd = sd(x))
          })

#Tidyverse tasks
library(dplyr)

#2
facebook <- read.csv("facebook-fact-check.csv")

#3
facebook_500 <- facebook %>% top_n(-500)

#4
facb_selected <- select(facebook, 2, 4, 6, 8, 10, 12)
  
names(facb_selected)

#5
#I know how to do it in basic R
#using mutate I was tryingto do this thing, it didn't work
#facebook <- facebook %>% mutate(post_type_coded = ifelse(Post.Type == "link", 1, post_type_coded))

#Marcy, can you, please share the code how I can do it (would be very useful)
# MS: check answer key :)

facebook$post_type_coded <- NA
facebook$post_type_coded[facebook$Post.Type == "link"] <- 1
facebook$post_type_coded[facebook$Post.Type == "photo"] <- 2
facebook$post_type_coded[facebook$Post.Type == "text"] <- 3
facebook$post_type_coded[facebook$Post.Type == "video"] <- 4

table(facebook$post_type_coded )
                   
#6
arrange(facebook, desc(Page))

#7
summarise(facebook,
          share_count.mean = mean(share_count, na.rm = TRUE),
          share_count.sd = sd(share_count, na.rm = TRUE),
          reaction_count.mean = mean(reaction_count, na.rm = TRUE),
          reaction_count.sd = sd(reaction_count, na.rm = TRUE),
          comment_count.mean = mean(comment_count, na.rm = TRUE),
          comment_count.sd = sd(comment_count, na.rm = TRUE))

#8
# MS: check answer key
facebook %>% 
  group_by("mainstream", Category) %>% 
  summarise(share_count.mean = mean(share_count, na.rm = TRUE),
            share_count.sd = sd(share_count, na.rm = TRUE),
            reaction_count.mean = mean(reaction_count, na.rm = TRUE),
            reaction_count.sd = sd(reaction_count, na.rm = TRUE),
            comment_count.mean = mean(comment_count, na.rm = TRUE),
            comment_count.sd = sd(comment_count, na.rm = TRUE)) %>% 
  ungroup()

