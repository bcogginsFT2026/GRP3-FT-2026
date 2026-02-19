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

# Display a count of unique values for each column.  Tried several approaches to see what works best
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

#Average Charges by Age (Trend)
insurance %>%
  group_by(age) %>%
  summarise(avg_charges = mean(charges)) %>%
  ggplot(aes(x = age, y = avg_charges)) +
  geom_line() +
  labs(title = "Average Charges by Age", x = "Age", y = "Average Charges")

#Create BMI Bins
insurance <- insurance %>%
  mutate(bmi_category = case_when(
    bmi < 18.5 ~ "low",
    bmi >= 18.5 & bmi < 25 ~ "middle",
    bmi >= 25 & bmi < 30 ~ "high",
    bmi >= 30 ~ "very high"
  ))

#People with Low BMI in Each Region
insurance %>%
  filter(bmi_category == "low") %>%
  count(region)

#Average Charges for Each BMI Bin
insurance %>%
  group_by(bmi_category) %>%
  summarise(avg_charges = mean(charges))
ggplot(insurance, aes(x = bmi_category, y = charges)) +
  geom_boxplot() +
  labs(title = "Charges by BMI Category", x = "BMI Category", y = "Charges")

#Average Charges for Each Smoker Status
insurance %>%
  group_by(smoker) %>%
  summarise(avg_charges = mean(charges))
# Plot
ggplot(insurance, aes(x = smoker, y = charges)) +
  geom_boxplot() +
  labs(title = "Charges by Smoker Status", x = "Smoker", y = "Charges")

#Average Charges for Each Region
insurance %>%
  group_by(region) %>%
  summarise(avg_charges = mean(charges))

# Plot
ggplot(insurance, aes(x = region, y = charges)) +
  geom_boxplot() +
  labs(title = "Charges by Region", x = "Region", y = "Charges") 

