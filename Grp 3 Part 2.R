#Group 3 Part 2 R Script

library(tidyr)
library(dplyr)

library(readr)

insurance <- read_csv("insurance.csv")

str(insurance)
spec(insurance)
spec_csv("insurance.csv")

insurance |>
sapply(insurance, function(x) n_distinct(x)) |>
summarise(insurance,(count = n()))