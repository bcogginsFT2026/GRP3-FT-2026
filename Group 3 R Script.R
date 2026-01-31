
# load the packages for this 
library(tidyverse)    # the core tidyverse packages (tibble, ggplot2, etc.)
library(dplyr)
library(usethis)
library(readr)
insurance <- read_csv("~/Documents/insurance.csv") ### Make sure the CSV File is in a folder called Documents

# get insurance data and format it as a tibble
insurance_data <- as_tibble(insurance)
insurance_data

#Johnathon
#Display the summary statistics for the age and charges columns#
summary(insurance_data)
summary(select(insurance_data, "age", "charges"))
#A: The mean and median for the age data is pretty close. This contrasts with the charges data as the mean/average is much higher than the median value.#

# plot data to see what we get..sort it out by region
insurance_data |>
  group_by(age, region) |>
  ggplot(aes(x = age, y = bmi,
             color = sex)) +
  geom_line(position = position_dodge()) +  #- Experimenting w/ Dodge command
  geom_point(charges) +
  facet_wrap(facets = vars(region), nrow = 2, ncol = 2)


#Johnathon
# How many people were sampled from each region?#
insurance_data %>%
  group_by(region) %>%
  summarise(count = n())
#A:northeast= 324, northwest = 325, southeast = 364, southwest = 325#

