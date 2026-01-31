
# load the packages for this 
library(tidyverse)    # the core tidyverse packages (tibble, ggplot2, etc.)
library(dplyr)
library(usethis)
library(readr)
insurance <- read_csv("~/Documents/insurance.csv") ### Make sure the CSV File is in a folder called Documents

# get insurance data and format it as a tibble
insurance_data <- as_tibble(insurance)
insurance_data

#Display the summary statistics for the age and charges columns#
summary(insurance_data)
summary(select(insurance_data, "age", "charges", "region"))
#Q1 - A: The mean and median for the age data is pretty close. This contrasts with the charges data as the mean/average is much higher than the median value.#

# How many people were sampled from each region?#
insurance_data %>%
  group_by(region) %>%
  summarise(count = n())
#Q2 - A:northeast= 324, northwest = 325, southeast = 364, southwest = 325#

#Q3 - How are average medical charges related to age? Use a line plot to answer this question# 
#Summary :  The older you get, the more costly it is for heathcare & charges are commensurate to that
avg_charges_age <- insurance_data %>%
  group_by(age) %>%
  summarise(avg_charges = mean(charges))
#Display the new summary#
avg_charges_age
#Display the line plot that shows how avg medical charges are related to age#
ggplot(avg_charges_age, aes(x = age, y = avg_charges)) +
  geom_line() +
  labs(
    title = "Average Medical Charges by Age",
    x = "Age",
    y = "Average Medical Charges"
  )

#Jonathon 1/31 -  Q4 
#How are average medical charges related to the number of children?# 
#Use a line plot to answer this question#
#Summary : Cost seems to decline with the number of children.  EG 3 is the highest and 5 is lower
avg_charges_numchildren <- insurance_data %>%
  group_by(children) %>% -- #Add region in group by for facet_wrap
  summarise(avg_charges = mean(charges))
#Display the new summary#
avg_charges_numchildren
#Display the line plot that shows how avg medical charges are related to age#
ggplot(avg_charges_numchildren, aes(x = children, y = avg_charges)) +
  geom_line() +
#  facet_wrap(facets = vars(region), nrow = 2, ncol = 2) +
  labs(
    title = "Average Medical Charges by the Number of Children",
    x = "Number of Children",
    y = "Average Medical Charges"
  )


