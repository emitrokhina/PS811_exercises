library(tidyverse)

nations <- read.csv("national.csv")
View(nations)

#Loops

#1
rows_loop <- numeric()

for (i in 1:ncol(nations)) {
  rows_loop[i] <- length(nations[i, ])
}

#We have 79 variables per observation.

#2
tapply(
  X = nations$christianity_protestant, 
  INDEX = list(nations$state), 
  FUN = mean, 
  na.rm = TRUE  
)

nations %>%
  group_by(state) %>%
  summarize(
    mean_christ= mean(christianity_protestant, na.rm = TRUE)
  ) 

#3
sapply(nations, is.character)
sum(sapply(nations, is.character)) #only two variables are character

select_if(nations, is.character)

#4
nations %>%
  mutate_at(
    .vars = vars(starts_with("buddhism_")),
    .funs = function(x) log(x)
  ) %>% 
  select(starts_with("buddhism_"))

#5
year <- nations %>%
  mutate_at(
    .vars = vars(christianity_all),
    .funs = ~ . > 300000    
  )

unique(year[1])
#It is not actually a function

#6
nation_code <- nations %>%
  group_by(code) %>%
  nest()

#7
nation_new <- nation_code %>%
  mutate(
  model = map(
      .x = data, 
      .f = ~ lm(dual_religion ~ judaism_percent, data = .x)
    )
  ) %>%
  print()

#8
coeff <- nation_new %>%
  mutate(coefs = map(model, coefficients)) %>%
  print()

#9
coeff$coefs

#10
coeff %>% pull(model)

#11
data_final <- coeff %>%
  unnest(coefs) %>%
  print()
