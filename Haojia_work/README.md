# The Impact of Neurobehavior on Feeding Outcomes in Neonates with CHD
Haojia Li
2023-11-16

``` r
library(tidyverse)
# handle missing data
library(VIM) # visualization
library(mice) # imputation

# read in raw data
nnns0 <- read.csv("../NNNS_score_data.csv")
```

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
  mutate_at(
    vars("Date_PO_feeds_started", "Date_Reaching_Full_PO", "Date_Identified_as_not_yet_full_PO"), 
    as_date, format = "%m/%d/%Y"
  )

# drop variables not of interest
nnns <- nnns |> select(!c(
  "sex_1_M_2_F", "Intubated_Pre_operatively",
  "bypass_used", "bypass_time_min",  
  "Neurologic_Complication", "AirwayAnomalyYN",
  "Percent_of_feeds_taken_by_mouth_at_discharge"
)) 
```

29 variables had missing values: 13 pre-op NNNS scores, 13 post-op NNNS
scores, and 3 date variables.

The date variables are only related to the secondary outcome, so we will
explore them later.

``` r
# variables with missing data
data.frame(nmiss = colSums(is.na(nnns))) |>
  # only keep variables with missing data
  filter(nmiss > 0) |>
  # sort by number of missing data
  arrange(desc(nmiss)) |>
  # add percentage
  mutate(perc = nmiss / nrow(nnns) * 100)
```

                                            nmiss     perc
    Date_Identified_as_not_yet_full_PO        104 80.62016
    Post_Op_NNNS_habituation_score             94 72.86822
    Pre_Op_NNNS_habituation_score              93 72.09302
    Pre_Op_NNNS_attention_score                66 51.16279
    Pre_Op_NNNS_handling_score                 57 44.18605
    Post_Op_NNNS_attention_score               40 31.00775
    Pre_Op_NNNS_Regulation_Score               39 30.23256
    Pre_Op_NNNS_Stress_Score                   39 30.23256
    Pre_Op_NNNS_Quality_of_Movement_Score      38 29.45736
    Pre_Op_NNNS_Non_Optimal_Reflexes_Score     38 29.45736
    Pre_Op_NNNS_Arousal_Score                  38 29.45736
    Pre_Op_NNNS_Hypertonic_Score               38 29.45736
    Pre_Op_NNNS_Hypotonic_Score                38 29.45736
    Pre_Op_NNNS_Asymmetry_Score                38 29.45736
    Pre_Op_NNNS_Excitability_Score             38 29.45736
    Pre_Op_NNNS_Lethargy_Score                 38 29.45736
    Post_Op_NNNS_handling_score                37 28.68217
    Post_Op_NNNS_Regulation_Score              31 24.03101
    Post_Op_NNNS_Quality_of_Movement_Score     30 23.25581
    Post_Op_NNNS_Non_Optimal_Reflexes_Score    30 23.25581
    Post_Op_NNNS_Stress_Score                  30 23.25581
    Post_Op_NNNS_Arousal_Score                 30 23.25581
    Post_Op_NNNS_Hypertonic_Score              30 23.25581
    Post_Op_NNNS_Hypotonic_Score               30 23.25581
    Post_Op_NNNS_Asymmetry_Score               30 23.25581
    Post_Op_NNNS_Excitability_Score            30 23.25581
    Post_Op_NNNS_Lethargy_Score                30 23.25581
    Date_Reaching_Full_PO                      29 22.48062
    Date_PO_feeds_started                       8  6.20155

Missing data pattern of pre-op and post-op NNNS scores is shown below.
Scores with suffix “0” are pre-op scores, and scores with suffix “1” are
post-op scores.

``` r
nnns_score <- nnns |> select(contains("NNNS"))
# shorten variable names
colnames(nnns_score) <- sapply(
  colnames(nnns_score), 
  \(x) 
  # remove the "Pre_/Post_Op_NNNS_" prefix and "_score" suffix
  # use nchar("_score") to avoid to distinguish upper and lower case
  str_sub(x, str_locate(x, "NNNS_")[2] + 1, nchar(x) - nchar("_score")) |> 
    # capitalize the first letter
    str_to_title() |> 
    # replace underscore with space
    str_replace_all("_", " ") |>
    # add 0/1 suffix to indicate pre-op/post-op
    paste0(as.integer(str_starts(x, "Post")))
)

aggr(
  nnns_score, 
  col = c("navyblue", "red"), numbers = T, sortVars = F, combined = T,
  cex.axis = .8, cex.numbers = .8, oma = c(10,0,0,2)
  )
```

![](README_files/figure-commonmark/missing%20data%20pattern%20of%20pre-op%20and%20post-op%20NNNS%20scores-1.png)
