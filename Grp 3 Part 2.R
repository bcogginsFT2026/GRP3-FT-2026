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
  glimpse(insurance)

# Display a count of unique values for each column.  Tried several approaches to see what works best
insurance |> 
  summarise(across(everything(), list(min = min, max = max))) 
distinct(insurance, across(everything()))

insurance |> apply(MARGIN = 2, FUN = unique) |>
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

## Start Ashleigh & Jon


#Average Charges by Age (Trend)
insurance %>%
  group_by(age) %>%
  summarise(avg_charges = mean(charges, na.rm = TRUE)) %>%
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

#JC Code - Bin the bmi column so it labels each person as low (<18.5), middle (18.5-25), high (25-30), or very high (>30).#
#insurance <- insurance %>%
  # mutate(
  #   bmi_status = cut(
  #     bmi,
  #     breaks = c(-Inf, 18.5, 25, Inf),
  #     labels = c("Low", "Normal", "High"),
  #     right = FALSE
  #   )
  # ) 

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

# JC Code
# #What are the average charges for each BMI bin?#
# insurance %>%
#   group_by(bmi_status) %>%
#   summarise(avg_charges = mean(charges, na.rm = TRUE))

#Average Charges for Each Smoker Status
insurance %>%
  group_by(smoker) %>%
  summarise(avg_charges = mean(charges))
# Plot
ggplot(insurance, aes(x = smoker, y = charges)) +
  geom_boxplot() +
  labs(title = "Charges by Smoker Status", x = "Smoker", y = "Charges")

# #JC Code - What are the average charges for each smoker status?#
# insurance %>%
#   group_by(smoker) %>%
#   summarise(avg_charges = mean(charges, na.rm = TRUE))

#Average Charges for Each Region 
insurance %>%
  group_by(region) %>%
  summarise(avg_charges = mean(charges))

#JC Code - What are the average charges for each region?#
# insurance %>%
#   group_by(region) %>%
#   summarise(avg_charges = mean(charges, na.rm = TRUE))

# Plot
ggplot(insurance, aes(x = region, y = charges)) +
  geom_boxplot() +
  labs(title = "Charges by Region", x = "Region", y = "Charges") 

