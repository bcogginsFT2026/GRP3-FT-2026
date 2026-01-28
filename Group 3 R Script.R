
# load the packages for this 
library("tidyverse")    # the core tidyverse packages (tibble, ggplot2, etc.)

library(readr)
insurance <- read_csv("~/Documents/insurance.csv") ### Make sure the CSV File is in a folder called Documents

# get insurance data and format it as a tibble
insurance_data <- as_tibble(insurance)
insurance_data

# plot data to see what we get..sort it out by region
insurance_data |>
  group_by(age, region) |>
  
  ggplot(aes(x = age, y = bmi,
             color = sex)) +
  geom_line(position = position_dodge()) +  #- Experimenting w/ Dodge command
  geom_point(charges) +
  facet_wrap(facets = vars(region), nrow = 2, ncol = 2)
