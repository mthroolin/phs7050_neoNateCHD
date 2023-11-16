# The Impact of Neurobehavior on Feeding Outcomes in Neonates with CHD
Haojia Li
2023-11-15

``` r
library(tidyverse)
# handle missing data
library(VIM) # visualization
library(mice) # imputation

# read in raw data
nnns0 <- read.csv("../NNNS_score_data.csv")
```

# Data Exploration

``` r
# clean up variable names
# remove the dots at the end of variable names
colnames(nnns0) <- gsub("\\.+$", "", colnames(nnns0))
# replace all the dots in variable names with underscores
colnames(nnns0) <- gsub("\\.+", "_", colnames(nnns0))

# summary of the data
summary(nnns0)

# exclude neurologic or airway anomaly
nnns <- nnns0 |> filter(Neurologic_Complication == 0, AirwayAnomalyYN == 0)

nnns <- nnns0 |>
  
  # some binary variables have values of 1/2 or Y/N, recode them to 0/1
  mutate(
    Female = as.integer(sex_1_M_2_F == 2),
    Premature = as.integer(Premature == 1),
    Extubation_failure = as.integer(Extubation_failure == "Y"),
    Intubated_Pre_operatively = as.integer(Intubated_Pre_operatively == "Y")
  ) |>
  
  # relabel cardiac anatomy
  mutate(
    Cardiac_Anatomy = factor(
      Cardiac_Anatomy, levels = 1:4,
      labels = c(
        "Single ventricle w/o arch obstruction",
        "Single ventricle w/ arch obstruction",
        "Two ventricle w/o arch obstruction",
        "Two ventricle w/ arch obstruction"
      )
    ),
    # for model building purposes, combine the 2 levels w/o arch obstruction
    Cardiac_Anatomy_collapsed = fct_collapse(
      Cardiac_Anatomy,
      "W/o arch obstruction" = c("Single ventricle w/o arch obstruction", "Two ventricle w/o arch obstruction"),
      "Single ventricle w/ arch obstruction" = "Single ventricle w/ arch obstruction",
      "Two ventricle w/ arch obstruction" = "Two ventricle w/ arch obstruction"
    )
  ) |>
  
  # convert date variables to date class
  mutate_at(vars("Date_PO_feeds_started", "Date_Reaching_Full_PO", "Date_Identified_as_not_yet_full_PO"), as_date, format = "%m/%d/%Y")
```
