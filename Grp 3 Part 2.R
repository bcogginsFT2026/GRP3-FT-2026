#Group 3 Part 2 R Script

library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)

insurance <- read_csv("insurance.csv")

#drop any garbage from dataset (if exists)
insurance <- drop_na(insurance)

str(insurance)
spec(insurance)
spec_csv("insurance.csv")

# Display the column names and data types.
  glimpse(insurance)

# Display a count of unique values for each column.  Tried several approaches to address
insurance |> 
  summarise(across(everything(), list(min = min, max = max))) 
distinct(insurance, across(everything()))

insurance |> apply(MARGIN = 2, FUN = unique) |>
       str()

sapply(insurance, unique)
insurance |>
summarise(across(everything(), ~ n_distinct(.)))

# Generate Plot of Charges distributed | USE Density Plot
#How are charges distributed? Use a density plot for your answer.
#How are the BMI values distributed? Are there any outliers?  
  # 1 big outlier of 30 - 45K in charges pops up.  Otherwise the rest
  # appears to follow a normalized pattern where more BMI = higher cost
insurance |>
  group_by(bmi)
ggplot(data = insurance, mapping = aes(x = bmi, y = charges)) +
geom_density2d() + 
  labs(title = "Charge/BMI Distribution", x = "BMI", y = "Charges")
   
#What’s the relationship between age and charges?
 # As age increases, so do the respective charges.  EG charges are higher.
 # There are a few outliers displayed as well in the 50K region specifically, but
 # also in the 12 - 30K and 35K to 45K
insurance |>
  group_by(age)
  ggplot(data = insurance, mapping = aes(x = age, y = charges)) +
  geom_density_2d() + 
  labs(title = "Age/Charge Distribution", x = "Age", y = "Charges")

# Start Ashleigh, Shakirah & Jon Code additions.  Including both for reference and consideration


#Average Charges by Age (Trend)
#What’s the relationship between age and charges?
  # As age increases, so do the respective charges.  EG avg charges increase

insurance %>%
  group_by(age) %>%
  summarise(avg_charges = mean(charges, na.rm = TRUE)) %>%
  ggplot(aes(x = age, y = avg_charges)) +
  geom_line() +
  labs(title = "Average Charges by Age", x = "Age", y = "Average Charges")

#Create BMI Bins
#Bin the bmi column so it labels each person as low (<18.5), middle (18.5-25), high (25-30), or very high (>30). 
insurance <- insurance %>%
  mutate(bmi_category = case_when(
    bmi < 18.5 ~ "Low (<18.5)",
    bmi >= 18.5 & bmi < 25 ~ "Middle (18.5-25)",
    bmi >= 25 & bmi < 30 ~ "High (25-30)",
    bmi >= 30 ~ "Very High (>30)"
  ))

insurance |> 
  ggplot(aes(x = bmi)) +
  geom_histogram(bins = 4) +
  facet_wrap(~ bmi_category) +
  labs(title = "BMI Bins ", x = "BMI", y = "Count") 

#JC Code - Bin the bmi column so it labels each person as low (<18.5), middle (18.5-25), high (25-30), or very high (>30).#
  # Covered above
# insurance <- insurance %>%
# mutate(
#   bmi_status = cut(
#     bmi,
#     breaks = c(-Inf, 18.5, 25, Inf),
#     labels = c("Low", "Normal", "High"),
#     right = FALSE
#   )
# )

#People with Low BMI in Each Region
#How many people with low BMIs are in each region?
  # 10 in Northeast; 7 in Northwest; 3 in Southwest
insurance %>%
  filter(bmi_category == "low") %>%
  count(region)

#Average Charges for Each BMI Bin
#What are the average charges for each BMI bin?
 # 10988 - High; 8852 - Low; 10409 - Middle; 15552 - Very High
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
# What are the average charges for each smoker status?
  # No - 8434; Yes - 32050 - Smokers have higher charges and incidents vs non smokers
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
#What are the average charges for each region?
  #Northeast - 13406; Northwest - 12418; Southeast - 14735; Southwest - 12347 (Southeast is the highest)
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

