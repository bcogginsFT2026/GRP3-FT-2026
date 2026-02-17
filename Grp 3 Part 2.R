#Group 3 Part 2 R Script

library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)

insurance <- read_csv("insurance.csv")

str(insurance)
spec(insurance)
spec_csv("insurance.csv")

# Display the column names and data types.
insurance |>
sapply(insurance, function(x) n_distinct(x)) |>
summarise(insurance,(count = n()))

# Display a count of unique values for each column.
summarise(across(everything(), list(min = min, max = max)))
distinct(insurance, across(everything()))
unique(insurance) 
count(unique(insurance))

insurance |> apply(MARGIN = 2, FUN = unique) |>
       str()

insurance |> apply(MARGIN = 2, FUN = unique) |>
  sapply(FUN = length) |>  
  str()

sapply(insurance, unique)
insurance |>
summarise(across(everything(), ~ n_distinct(.)))

# Generate Plot of Charges distributed | USE Density Plot
#How are the BMI values distributed? Are there any outliers?   
insurance |>
ggplot(data = insurance, mapping = aes(x = bmi, y = charges)) +
         geom_density_2d()
   
#What’s the relationship between age and charges?
insurance |>
  ggplot(data = insurance, mapping = aes(x = age, y = charges)) +
  geom_density_2d()


