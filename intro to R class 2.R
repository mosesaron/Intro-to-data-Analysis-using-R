# We need to set our working directory 
setwd("~/Desktop/Intro to R")
# remove everything in my work space
rm(list =ls())
# import the data but we have to install the necessary package 
#install.packages("readxl")
library(readxl)
#install.packages("haven")
library(haven)


#install.packages("tidyverse")
library(tidyverse)
#install.packages("kableExtra")
library(kableExtra)

food <- read_excel("food.xlsx")
wash <- read_sav("wash.sav")

table(wash$how_often_treat_water)
# to view 
#View(food)
#View(wash)

# descriptive statitics
# to know the variable names 
colnames(food)

# get the summary statistics 
summary(food$`Income in 2012`)

colnames(wash)
summary(wash$age_respondent)

# get the frequency table 
table(wash$edu_level)

# get the percentage and round to 1 decimal place 
round (prop.table (table(wash$edu_level))*100, 1)

# how to tell the data type
class(food$`District Name`)
class(food$`Income in 2012`)

#The best format to use in R is a .csv file 
#
write_csv(wash, "wash.csv")

Madzi <- read_csv("wash.csv")

WASH <- select(Madzi,household_id,gender_repondent,age_respondent,edu_level,household_head,total_household_members)

WASH1 <- rename(WASH,
                id = household_id,
                sex = gender_repondent,
                age = age_respondent,
                edu = edu_level,
                hh = household_head,
                hh_members = total_household_members)
summary(WASH1$age)

WASH2 <- filter(WASH1, age >= 15 & age <=60)
summary(WASH2$age)
table(WASH2$sex)

WASH2$sex[WASH2$sex == 0] <- "Male"
WASH2$sex[WASH2$sex == 1] <- "Female"
table(WASH2$sex)

WASH2$edu[WASH2$edu == 0] <- "None"
WASH2$edu[WASH2$edu == 1] <- "Primary"
WASH2$edu[WASH2$edu == 2] <- "Secondary"
WASH2$edu[WASH2$edu == 3] <- "Tertiary"
table(WASH2$edu)

table(WASH2$hh)
WASH2$hh[WASH2$hh == 0] <- "No"
WASH2$hh[WASH2$hh == 1] <- "Yes"
WASH2$hh[WASH2$hh == 2] <- "Yes"
table(WASH2$hh)

WASH3 <- mutate(WASH2,
                age_cat = ifelse(age %in% 15:30, "15-30",
                          ifelse(age %in% 31:45, "31-45",
                                 "46+")))

age_cat <- ifelse(WASH2$age >=15 & WASH2$age <=30, "15-30",
            ifelse(WASH2$age >=31 & WASH2$age <=45, "31-45",
            ifelse(WASH2$age >=46, "46+", NA)))

WASH2$age_cat2 <- age_cat


table(WASH3$age_cat)
table(WASH3$edu, WASH3$sex)

ggplot(data = WASH3, aes(fill =sex, x=edu))+geom_bar(position = "dodge") +ggtitle("Distribution of respondent level of Education by sex") +xlab("Highest level of Education") + geom_text(aes(label=..count..),stat = "count", position = position_dodge(0.9), vjust =-0.2)

kkk<-WASH3 %>%
  group_by(sex) %>%
  summarise(n = n(),
            mean = mean (age,),
            min = min (age))
kkk %>% kable() %>% kable_styling()
