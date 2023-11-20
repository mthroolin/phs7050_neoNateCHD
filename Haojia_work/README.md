# The Impact of Neurobehavior on Feeding Outcomes in Neonates with CHD
Haojia Li
2023-11-20

# Research Objectives

Study the impact of neonatal attention on feeding outcomes among infants
undergoing CHD surgery.

1.  Assess the association between NNNS attention score and percentage
    of feeds taken by mouth at discharge. Investigator’s assumption is
    that lower pre- or post-op attention scores are associated with
    lower percentage of oral feeds at discharge.

2.  Assess the association between NNNS attention score and time to
    achieve full oral feeds. Investigator’s assumption is that lower
    pre- or post-op attention scores are associated with longer time to
    achieve full oral feeds after cardiac surgery.

# Data

Data file: “NNNS_score_data.csv”; Data dictionary:
“NNNS_score_data_dictionary.xlsx”.

The raw data contains 129 observations and 44 columns. Infants with
neurologic or airway anomaly should be excluded from the analysis.

Outcome 1 is the percentage of feeds taken by mouth at discharge, which
is bounded by \[0,1\], with a substantial fraction of the data falling
at the boundaries (exactly 0’s and 1’s).

Outcome 2 is the time to achieve full oral feeds after cardiac surgery,
which is calculated as the time difference in days between the start
date of PO feed and either the date of reaching full PO or the date of
“not yet full PO”. Non-missing date of “not yet full PO” indicates
censoring.

Besides the predictor, NNNS attention score, the variables of interest
include: sex, genetic syndrome, age at surgery in days, prematurity,
cardiac anatomy, length of intubation, length of stay, extubation
failure and gastrointestinal complications.

The 12 other NNNS scales, pre-op and post-op, are not of interest in
this analysis, but could be used as auxiliary variables in the
imputation model.

## Data cleaning

``` r
library(tidyverse)
library(gtsummary)
# handle missing data
library(VIM) # visualization
library(mice) # imputation
# plot matrix of variables
library(GGally)

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

nnns <- nnns0 |> 
  filter(
    # exclude neurologic or airway anomaly
    Neurologic_Complication == 0, AirwayAnomalyYN == 0,
    # include infants from birth to 4 weeks old
    # there are two outliers with age at surgery > 30 days
    Age_at_Surgery_days <= 30
    )

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

# drop unnecessary variables
nnns <- nnns |> select(!c(
  "sex_1_M_2_F", # use Female instead
  "Intubated_Pre_operatively", "bypass_used", "bypass_time_min", # not of interest 
  "Neurologic_Complication", "AirwayAnomalyYN" # already excluded
)) 
```

## Missing data

There are in total of 30 variables with missing values: percentage of
oral feed at discharge, 13 pre-op NNNS scores, 13 post-op NNNS scores,
and 3 date variables. The missing values in the date variables are not
real missing values, but rather an indication of censoring.

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

                                                 nmiss      perc
    Date_Identified_as_not_yet_full_PO             104 80.620155
    Post_Op_NNNS_habituation_score                  94 72.868217
    Pre_Op_NNNS_habituation_score                   93 72.093023
    Pre_Op_NNNS_attention_score                     66 51.162791
    Pre_Op_NNNS_handling_score                      57 44.186047
    Post_Op_NNNS_attention_score                    40 31.007752
    Pre_Op_NNNS_Regulation_Score                    39 30.232558
    Pre_Op_NNNS_Stress_Score                        39 30.232558
    Pre_Op_NNNS_Quality_of_Movement_Score           38 29.457364
    Pre_Op_NNNS_Non_Optimal_Reflexes_Score          38 29.457364
    Pre_Op_NNNS_Arousal_Score                       38 29.457364
    Pre_Op_NNNS_Hypertonic_Score                    38 29.457364
    Pre_Op_NNNS_Hypotonic_Score                     38 29.457364
    Pre_Op_NNNS_Asymmetry_Score                     38 29.457364
    Pre_Op_NNNS_Excitability_Score                  38 29.457364
    Pre_Op_NNNS_Lethargy_Score                      38 29.457364
    Post_Op_NNNS_handling_score                     37 28.682171
    Post_Op_NNNS_Regulation_Score                   31 24.031008
    Post_Op_NNNS_Quality_of_Movement_Score          30 23.255814
    Post_Op_NNNS_Non_Optimal_Reflexes_Score         30 23.255814
    Post_Op_NNNS_Stress_Score                       30 23.255814
    Post_Op_NNNS_Arousal_Score                      30 23.255814
    Post_Op_NNNS_Hypertonic_Score                   30 23.255814
    Post_Op_NNNS_Hypotonic_Score                    30 23.255814
    Post_Op_NNNS_Asymmetry_Score                    30 23.255814
    Post_Op_NNNS_Excitability_Score                 30 23.255814
    Post_Op_NNNS_Lethargy_Score                     30 23.255814
    Date_Reaching_Full_PO                           29 22.480620
    Date_PO_feeds_started                            8  6.201550
    Percent_of_feeds_taken_by_mouth_at_discharge     5  3.875969

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
  col = c("navyblue", "red"), numbers = T, prop = F, sortVars = F, combined = T,
  cex.axis = .8, cex.numbers = .8, oma = c(10,0,0,2)
)
```

![](README_files/figure-commonmark/missing%20data%20pattern%20of%20pre-op%20and%20post-op%20NNNS%20scores-1.png)

There are 66 (51.2%) infants with missing pre-op NNNS attention scores,
and 40 (31.0%) infants with missing post-op NNNS attention scores.
Missingness in the NNNS attention score may be due to a variety of
factors:

1.  Infant has sternal precautions, examiner cannot do lifting, crawling
    etc.
2.  Infant is either too quiet or too active. e.g., too long to
    response, sleeping, too fussy.
3.  Examiner error or logistical reasons: e.g. skipped by examiner due
    to holiday, weekend, forgot.

``` r
# missingness in pre-op NNNS attention score
nnns |> 
  mutate(na_pre_attention = ifelse(is.na(Pre_Op_NNNS_attention_score), "Missing", "Non-missing")) |>
  select(
    na_pre_attention, 
    Age_at_Surgery_days, Female, Premature,
    Genetic_Syndrome_or_Chromosomal_Abnormality, Cardiac_Anatomy,
    GI_Complication, Length_of_Stay_days, Length_of_intubation_days, Extubation_failure,
    starts_with("Pre_Op_NNNS"), Post_Op_NNNS_attention_score, -Pre_Op_NNNS_attention_score
  ) |>
  tbl_summary(
    by = na_pre_attention,
    # set the type of NNNs scores to continuous
    type = list(
      Pre_Op_NNNS_habituation_score ~ "continuous",
      Pre_Op_NNNS_handling_score ~ "continuous",
      Pre_Op_NNNS_Quality_of_Movement_Score ~ "continuous",
      Pre_Op_NNNS_Regulation_Score ~ "continuous",
      Pre_Op_NNNS_Non_Optimal_Reflexes_Score ~ "continuous",
      Pre_Op_NNNS_Stress_Score ~ "continuous",
      Pre_Op_NNNS_Arousal_Score ~ "continuous",
      Pre_Op_NNNS_Hypertonic_Score ~ "continuous",
      Pre_Op_NNNS_Hypotonic_Score ~ "continuous",
      Pre_Op_NNNS_Asymmetry_Score ~ "continuous",
      Pre_Op_NNNS_Excitability_Score ~ "continuous",
      Pre_Op_NNNS_Lethargy_Score ~ "continuous"
    ),
    label = list(
      Age_at_Surgery_days ~ "Age at surgery in days",
      Genetic_Syndrome_or_Chromosomal_Abnormality ~ "Genetic Syndrome / Chromosomal Abnormality",
      Cardiac_Anatomy ~ "Cardiac Anatomy",
      GI_Complication ~ "Gastrointestinal Complication",
      Length_of_Stay_days ~ "Length of Stay in days",
      Length_of_intubation_days ~ "Length of intubation in days",
      Extubation_failure ~ "Extubation failure",
      Pre_Op_NNNS_habituation_score ~ "Pre-op NNNS habituation",
      Pre_Op_NNNS_handling_score ~ "Pre-op NNNS handling",
      Pre_Op_NNNS_Quality_of_Movement_Score ~ "Pre-op NNNS quality of movement",
      Pre_Op_NNNS_Regulation_Score ~ "Pre-op NNNS regulation",
      Pre_Op_NNNS_Non_Optimal_Reflexes_Score ~ "Pre-op NNNS non-optimal reflexes",
      Pre_Op_NNNS_Stress_Score ~ "Pre-op NNNS stress",
      Pre_Op_NNNS_Arousal_Score ~ "Pre-op NNNS arousal",
      Pre_Op_NNNS_Hypertonic_Score ~ "Pre-op NNNS hypertonic",
      Pre_Op_NNNS_Hypotonic_Score ~ "Pre-op NNNS hypotonic",
      Pre_Op_NNNS_Asymmetry_Score ~ "Pre-op NNNS asymmetry",
      Pre_Op_NNNS_Excitability_Score ~ "Pre-op NNNS excitability",
      Pre_Op_NNNS_Lethargy_Score ~ "Pre-op NNNS lethargy"
    ),
    missing_text = "N-Missing"
  ) |>
  add_p()
```

<div id="zgpnrdpxqk" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#zgpnrdpxqk table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#zgpnrdpxqk thead, #zgpnrdpxqk tbody, #zgpnrdpxqk tfoot, #zgpnrdpxqk tr, #zgpnrdpxqk td, #zgpnrdpxqk th {
  border-style: none;
}
&#10;#zgpnrdpxqk p {
  margin: 0;
  padding: 0;
}
&#10;#zgpnrdpxqk .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#zgpnrdpxqk .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#zgpnrdpxqk .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#zgpnrdpxqk .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#zgpnrdpxqk .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#zgpnrdpxqk .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#zgpnrdpxqk .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#zgpnrdpxqk .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#zgpnrdpxqk .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#zgpnrdpxqk .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#zgpnrdpxqk .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#zgpnrdpxqk .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#zgpnrdpxqk .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#zgpnrdpxqk .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#zgpnrdpxqk .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#zgpnrdpxqk .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#zgpnrdpxqk .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#zgpnrdpxqk .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#zgpnrdpxqk .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zgpnrdpxqk .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#zgpnrdpxqk .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#zgpnrdpxqk .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#zgpnrdpxqk .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zgpnrdpxqk .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#zgpnrdpxqk .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#zgpnrdpxqk .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#zgpnrdpxqk .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zgpnrdpxqk .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#zgpnrdpxqk .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#zgpnrdpxqk .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#zgpnrdpxqk .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#zgpnrdpxqk .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#zgpnrdpxqk .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zgpnrdpxqk .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#zgpnrdpxqk .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zgpnrdpxqk .gt_left {
  text-align: left;
}
&#10;#zgpnrdpxqk .gt_center {
  text-align: center;
}
&#10;#zgpnrdpxqk .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#zgpnrdpxqk .gt_font_normal {
  font-weight: normal;
}
&#10;#zgpnrdpxqk .gt_font_bold {
  font-weight: bold;
}
&#10;#zgpnrdpxqk .gt_font_italic {
  font-style: italic;
}
&#10;#zgpnrdpxqk .gt_super {
  font-size: 65%;
}
&#10;#zgpnrdpxqk .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#zgpnrdpxqk .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#zgpnrdpxqk .gt_indent_1 {
  text-indent: 5px;
}
&#10;#zgpnrdpxqk .gt_indent_2 {
  text-indent: 10px;
}
&#10;#zgpnrdpxqk .gt_indent_3 {
  text-indent: 15px;
}
&#10;#zgpnrdpxqk .gt_indent_4 {
  text-indent: 20px;
}
&#10;#zgpnrdpxqk .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Missing&lt;/strong&gt;, N = 66&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>Missing</strong>, N = 66<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Non-missing&lt;/strong&gt;, N = 63&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>Non-missing</strong>, N = 63<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;2&lt;/sup&gt;&lt;/span&gt;"><strong>p-value</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>2</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Age at surgery in days</td>
<td headers="stat_1" class="gt_row gt_center">7 (5, 10)</td>
<td headers="stat_2" class="gt_row gt_center">7 (5, 10)</td>
<td headers="p.value" class="gt_row gt_center">0.7</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Female</td>
<td headers="stat_1" class="gt_row gt_center">26 (39%)</td>
<td headers="stat_2" class="gt_row gt_center">26 (41%)</td>
<td headers="p.value" class="gt_row gt_center">0.8</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Premature</td>
<td headers="stat_1" class="gt_row gt_center">59 (89%)</td>
<td headers="stat_2" class="gt_row gt_center">54 (86%)</td>
<td headers="p.value" class="gt_row gt_center">0.5</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Genetic Syndrome / Chromosomal Abnormality</td>
<td headers="stat_1" class="gt_row gt_center">15 (23%)</td>
<td headers="stat_2" class="gt_row gt_center">9 (14%)</td>
<td headers="p.value" class="gt_row gt_center">0.2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Cardiac Anatomy</td>
<td headers="stat_1" class="gt_row gt_center"></td>
<td headers="stat_2" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center">0.6</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Single ventricle w/o arch obstruction</td>
<td headers="stat_1" class="gt_row gt_center">4 (6.1%)</td>
<td headers="stat_2" class="gt_row gt_center">6 (9.5%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Single ventricle w/ arch obstruction</td>
<td headers="stat_1" class="gt_row gt_center">13 (20%)</td>
<td headers="stat_2" class="gt_row gt_center">16 (25%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Two ventricle w/o arch obstruction</td>
<td headers="stat_1" class="gt_row gt_center">26 (39%)</td>
<td headers="stat_2" class="gt_row gt_center">18 (29%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Two ventricle w/ arch obstruction</td>
<td headers="stat_1" class="gt_row gt_center">23 (35%)</td>
<td headers="stat_2" class="gt_row gt_center">23 (37%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Gastrointestinal Complication</td>
<td headers="stat_1" class="gt_row gt_center">8 (12%)</td>
<td headers="stat_2" class="gt_row gt_center">4 (6.3%)</td>
<td headers="p.value" class="gt_row gt_center">0.3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Length of Stay in days</td>
<td headers="stat_1" class="gt_row gt_center">25 (18, 34)</td>
<td headers="stat_2" class="gt_row gt_center">21 (17, 28)</td>
<td headers="p.value" class="gt_row gt_center">0.14</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Length of intubation in days</td>
<td headers="stat_1" class="gt_row gt_center">5.00 (3.80, 6.88)</td>
<td headers="stat_2" class="gt_row gt_center">4.90 (3.00, 6.10)</td>
<td headers="p.value" class="gt_row gt_center">0.2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Extubation failure</td>
<td headers="stat_1" class="gt_row gt_center">7 (11%)</td>
<td headers="stat_2" class="gt_row gt_center">5 (7.9%)</td>
<td headers="p.value" class="gt_row gt_center">0.6</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Pre-op NNNS habituation</td>
<td headers="stat_1" class="gt_row gt_center">9.00 (7.00, 9.00)</td>
<td headers="stat_2" class="gt_row gt_center">8.50 (7.54, 9.00)</td>
<td headers="p.value" class="gt_row gt_center">0.6</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    N-Missing</td>
<td headers="stat_1" class="gt_row gt_center">52</td>
<td headers="stat_2" class="gt_row gt_center">41</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Pre-op NNNS handling</td>
<td headers="stat_1" class="gt_row gt_center">0.38 (0.38, 0.50)</td>
<td headers="stat_2" class="gt_row gt_center">0.38 (0.25, 0.50)</td>
<td headers="p.value" class="gt_row gt_center">>0.9</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    N-Missing</td>
<td headers="stat_1" class="gt_row gt_center">57</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Pre-op NNNS quality of movement</td>
<td headers="stat_1" class="gt_row gt_center">4.45 (4.17, 4.71)</td>
<td headers="stat_2" class="gt_row gt_center">4.67 (4.17, 5.00)</td>
<td headers="p.value" class="gt_row gt_center">0.2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    N-Missing</td>
<td headers="stat_1" class="gt_row gt_center">38</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Pre-op NNNS regulation</td>
<td headers="stat_1" class="gt_row gt_center">4.64 (4.00, 4.86)</td>
<td headers="stat_2" class="gt_row gt_center">4.93 (4.50, 5.35)</td>
<td headers="p.value" class="gt_row gt_center">0.009</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    N-Missing</td>
<td headers="stat_1" class="gt_row gt_center">39</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Pre-op NNNS non-optimal reflexes</td>
<td headers="stat_1" class="gt_row gt_center">7 (6, 8)</td>
<td headers="stat_2" class="gt_row gt_center">5 (4, 7)</td>
<td headers="p.value" class="gt_row gt_center">0.002</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    N-Missing</td>
<td headers="stat_1" class="gt_row gt_center">38</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Pre-op NNNS stress</td>
<td headers="stat_1" class="gt_row gt_center">0.06 (0.04, 0.07)</td>
<td headers="stat_2" class="gt_row gt_center">0.05 (0.02, 0.06)</td>
<td headers="p.value" class="gt_row gt_center">0.6</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    N-Missing</td>
<td headers="stat_1" class="gt_row gt_center">38</td>
<td headers="stat_2" class="gt_row gt_center">1</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Pre-op NNNS arousal</td>
<td headers="stat_1" class="gt_row gt_center">3.57 (3.11, 3.86)</td>
<td headers="stat_2" class="gt_row gt_center">3.71 (3.29, 4.00)</td>
<td headers="p.value" class="gt_row gt_center">0.2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    N-Missing</td>
<td headers="stat_1" class="gt_row gt_center">38</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Pre-op NNNS hypertonic</td>
<td headers="stat_1" class="gt_row gt_center">0 (0, 1)</td>
<td headers="stat_2" class="gt_row gt_center">0 (0, 0)</td>
<td headers="p.value" class="gt_row gt_center">0.12</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    N-Missing</td>
<td headers="stat_1" class="gt_row gt_center">38</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Pre-op NNNS hypotonic</td>
<td headers="stat_1" class="gt_row gt_center">1 (0, 1)</td>
<td headers="stat_2" class="gt_row gt_center">0 (0, 1)</td>
<td headers="p.value" class="gt_row gt_center">0.13</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    N-Missing</td>
<td headers="stat_1" class="gt_row gt_center">38</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Pre-op NNNS asymmetry</td>
<td headers="stat_1" class="gt_row gt_center">1 (0, 1)</td>
<td headers="stat_2" class="gt_row gt_center">1 (0, 1)</td>
<td headers="p.value" class="gt_row gt_center">0.8</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    N-Missing</td>
<td headers="stat_1" class="gt_row gt_center">38</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Pre-op NNNS excitability</td>
<td headers="stat_1" class="gt_row gt_center">3 (2, 4)</td>
<td headers="stat_2" class="gt_row gt_center">3 (2, 4)</td>
<td headers="p.value" class="gt_row gt_center">>0.9</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    N-Missing</td>
<td headers="stat_1" class="gt_row gt_center">38</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Pre-op NNNS lethargy</td>
<td headers="stat_1" class="gt_row gt_center">6 (5, 6)</td>
<td headers="stat_2" class="gt_row gt_center">7 (5, 9)</td>
<td headers="p.value" class="gt_row gt_center">0.045</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    N-Missing</td>
<td headers="stat_1" class="gt_row gt_center">38</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Post_Op_NNNS_attention_score</td>
<td headers="stat_1" class="gt_row gt_center">4.43 (4.00, 5.14)</td>
<td headers="stat_2" class="gt_row gt_center">4.29 (3.71, 4.64)</td>
<td headers="p.value" class="gt_row gt_center">0.2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    N-Missing</td>
<td headers="stat_1" class="gt_row gt_center">16</td>
<td headers="stat_2" class="gt_row gt_center">24</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> Median (IQR); n (%)</td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>2</sup></span> Wilcoxon rank sum test; Pearson’s Chi-squared test; Fisher’s exact test</td>
    </tr>
  </tfoot>
</table>
</div>

``` r
# missingness in post-op NNNS attention score
nnns |>
  mutate(na_post_attention = ifelse(is.na(Post_Op_NNNS_attention_score), "Missing", "Non-missing")) |>
  select(
    na_post_attention,
    Age_at_Surgery_days, Female, Premature,
    Genetic_Syndrome_or_Chromosomal_Abnormality, Cardiac_Anatomy,
    GI_Complication, Length_of_Stay_days, Length_of_intubation_days, Extubation_failure,
    starts_with("Post_Op_NNNS"), Pre_Op_NNNS_attention_score, -Post_Op_NNNS_attention_score
  ) |>
  tbl_summary(
    by = na_post_attention,
    # set the type of NNNs scores to continuous
    type = list(
      Pre_Op_NNNS_attention_score ~ "continuous",
      Post_Op_NNNS_habituation_score ~ "continuous",
      Post_Op_NNNS_handling_score ~ "continuous",
      Post_Op_NNNS_Quality_of_Movement_Score ~ "continuous",
      Post_Op_NNNS_Regulation_Score ~ "continuous",
      Post_Op_NNNS_Non_Optimal_Reflexes_Score ~ "continuous",
      Post_Op_NNNS_Stress_Score ~ "continuous",
      Post_Op_NNNS_Arousal_Score ~ "continuous",
      Post_Op_NNNS_Hypertonic_Score ~ "continuous",
      Post_Op_NNNS_Hypotonic_Score ~ "continuous",
      Post_Op_NNNS_Asymmetry_Score ~ "continuous",
      Post_Op_NNNS_Excitability_Score ~ "continuous",
      Post_Op_NNNS_Lethargy_Score ~ "continuous"
    ),
    label = list(
      Age_at_Surgery_days ~ "Age at surgery in days",
      Genetic_Syndrome_or_Chromosomal_Abnormality ~ "Genetic Syndrome / Chromosomal Abnormality",
      Cardiac_Anatomy ~ "Cardiac Anatomy",
      GI_Complication ~ "Gastrointestinal Complication",
      Length_of_Stay_days ~ "Length of Stay in days",
      Length_of_intubation_days ~ "Length of intubation in days",
      Extubation_failure ~ "Extubation failure",
      Pre_Op_NNNS_attention_score ~ "Pre-op NNNS attention",
      Post_Op_NNNS_habituation_score ~ "Post-op NNNS habituation",
      Post_Op_NNNS_handling_score ~ "Post-op NNNS handling",
      Post_Op_NNNS_Quality_of_Movement_Score ~ "Post-op NNNS quality of movement",
      Post_Op_NNNS_Regulation_Score ~ "Post-op NNNS regulation",
      Post_Op_NNNS_Non_Optimal_Reflexes_Score ~ "Post-op NNNS non-optimal reflexes",
      Post_Op_NNNS_Stress_Score ~ "Post-op NNNS stress",
      Post_Op_NNNS_Arousal_Score ~ "Post-op NNNS arousal",
      Post_Op_NNNS_Hypertonic_Score ~ "Post-op NNNS hypertonic",
      Post_Op_NNNS_Hypotonic_Score ~ "Post-op NNNS hypotonic",
      Post_Op_NNNS_Asymmetry_Score ~ "Post-op NNNS asymmetry",
      Post_Op_NNNS_Excitability_Score ~ "Post-op NNNS excitability",
      Post_Op_NNNS_Lethargy_Score ~ "Post-op NNNS lethargy"
    ),
    missing_text = "N-Missing"
  ) |>
  add_p()
```

<div id="upptijntnu" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#upptijntnu table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#upptijntnu thead, #upptijntnu tbody, #upptijntnu tfoot, #upptijntnu tr, #upptijntnu td, #upptijntnu th {
  border-style: none;
}
&#10;#upptijntnu p {
  margin: 0;
  padding: 0;
}
&#10;#upptijntnu .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#upptijntnu .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#upptijntnu .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#upptijntnu .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#upptijntnu .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#upptijntnu .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#upptijntnu .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#upptijntnu .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#upptijntnu .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#upptijntnu .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#upptijntnu .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#upptijntnu .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#upptijntnu .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#upptijntnu .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#upptijntnu .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#upptijntnu .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#upptijntnu .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#upptijntnu .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#upptijntnu .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#upptijntnu .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#upptijntnu .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#upptijntnu .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#upptijntnu .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#upptijntnu .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#upptijntnu .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#upptijntnu .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#upptijntnu .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#upptijntnu .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#upptijntnu .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#upptijntnu .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#upptijntnu .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#upptijntnu .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#upptijntnu .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#upptijntnu .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#upptijntnu .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#upptijntnu .gt_left {
  text-align: left;
}
&#10;#upptijntnu .gt_center {
  text-align: center;
}
&#10;#upptijntnu .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#upptijntnu .gt_font_normal {
  font-weight: normal;
}
&#10;#upptijntnu .gt_font_bold {
  font-weight: bold;
}
&#10;#upptijntnu .gt_font_italic {
  font-style: italic;
}
&#10;#upptijntnu .gt_super {
  font-size: 65%;
}
&#10;#upptijntnu .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#upptijntnu .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#upptijntnu .gt_indent_1 {
  text-indent: 5px;
}
&#10;#upptijntnu .gt_indent_2 {
  text-indent: 10px;
}
&#10;#upptijntnu .gt_indent_3 {
  text-indent: 15px;
}
&#10;#upptijntnu .gt_indent_4 {
  text-indent: 20px;
}
&#10;#upptijntnu .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Missing&lt;/strong&gt;, N = 40&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>Missing</strong>, N = 40<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Non-missing&lt;/strong&gt;, N = 89&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>Non-missing</strong>, N = 89<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;2&lt;/sup&gt;&lt;/span&gt;"><strong>p-value</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>2</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Age at surgery in days</td>
<td headers="stat_1" class="gt_row gt_center">8 (6, 11)</td>
<td headers="stat_2" class="gt_row gt_center">7 (5, 10)</td>
<td headers="p.value" class="gt_row gt_center">0.14</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Female</td>
<td headers="stat_1" class="gt_row gt_center">16 (40%)</td>
<td headers="stat_2" class="gt_row gt_center">36 (40%)</td>
<td headers="p.value" class="gt_row gt_center">>0.9</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Premature</td>
<td headers="stat_1" class="gt_row gt_center">33 (83%)</td>
<td headers="stat_2" class="gt_row gt_center">80 (90%)</td>
<td headers="p.value" class="gt_row gt_center">0.3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Genetic Syndrome / Chromosomal Abnormality</td>
<td headers="stat_1" class="gt_row gt_center">8 (20%)</td>
<td headers="stat_2" class="gt_row gt_center">16 (18%)</td>
<td headers="p.value" class="gt_row gt_center">0.8</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Cardiac Anatomy</td>
<td headers="stat_1" class="gt_row gt_center"></td>
<td headers="stat_2" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center">0.019</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Single ventricle w/o arch obstruction</td>
<td headers="stat_1" class="gt_row gt_center">3 (7.5%)</td>
<td headers="stat_2" class="gt_row gt_center">7 (7.9%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Single ventricle w/ arch obstruction</td>
<td headers="stat_1" class="gt_row gt_center">3 (7.5%)</td>
<td headers="stat_2" class="gt_row gt_center">26 (29%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Two ventricle w/o arch obstruction</td>
<td headers="stat_1" class="gt_row gt_center">14 (35%)</td>
<td headers="stat_2" class="gt_row gt_center">30 (34%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Two ventricle w/ arch obstruction</td>
<td headers="stat_1" class="gt_row gt_center">20 (50%)</td>
<td headers="stat_2" class="gt_row gt_center">26 (29%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Gastrointestinal Complication</td>
<td headers="stat_1" class="gt_row gt_center">2 (5.0%)</td>
<td headers="stat_2" class="gt_row gt_center">10 (11%)</td>
<td headers="p.value" class="gt_row gt_center">0.3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Length of Stay in days</td>
<td headers="stat_1" class="gt_row gt_center">19 (14, 30)</td>
<td headers="stat_2" class="gt_row gt_center">23 (18, 32)</td>
<td headers="p.value" class="gt_row gt_center">0.2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Length of intubation in days</td>
<td headers="stat_1" class="gt_row gt_center">3.60 (1.90, 5.80)</td>
<td headers="stat_2" class="gt_row gt_center">5.00 (4.20, 6.80)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Extubation failure</td>
<td headers="stat_1" class="gt_row gt_center">2 (5.0%)</td>
<td headers="stat_2" class="gt_row gt_center">10 (11%)</td>
<td headers="p.value" class="gt_row gt_center">0.3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Post-op NNNS habituation</td>
<td headers="stat_1" class="gt_row gt_center">8.50 (8.00, 9.00)</td>
<td headers="stat_2" class="gt_row gt_center">9.00 (7.50, 9.00)</td>
<td headers="p.value" class="gt_row gt_center">>0.9</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    N-Missing</td>
<td headers="stat_1" class="gt_row gt_center">32</td>
<td headers="stat_2" class="gt_row gt_center">62</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Post-op NNNS handling</td>
<td headers="stat_1" class="gt_row gt_center">0.38 (0.19, 0.59)</td>
<td headers="stat_2" class="gt_row gt_center">0.38 (0.13, 0.50)</td>
<td headers="p.value" class="gt_row gt_center">0.8</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    N-Missing</td>
<td headers="stat_1" class="gt_row gt_center">36</td>
<td headers="stat_2" class="gt_row gt_center">1</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Post-op NNNS quality of movement</td>
<td headers="stat_1" class="gt_row gt_center">4.20 (4.05, 4.38)</td>
<td headers="stat_2" class="gt_row gt_center">4.60 (4.17, 5.00)</td>
<td headers="p.value" class="gt_row gt_center">0.051</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    N-Missing</td>
<td headers="stat_1" class="gt_row gt_center">30</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Post-op NNNS regulation</td>
<td headers="stat_1" class="gt_row gt_center">3.67 (3.46, 4.13)</td>
<td headers="stat_2" class="gt_row gt_center">5.00 (4.57, 5.38)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    N-Missing</td>
<td headers="stat_1" class="gt_row gt_center">31</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Post-op NNNS non-optimal reflexes</td>
<td headers="stat_1" class="gt_row gt_center">3 (2, 4)</td>
<td headers="stat_2" class="gt_row gt_center">2 (1, 3)</td>
<td headers="p.value" class="gt_row gt_center">0.016</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    N-Missing</td>
<td headers="stat_1" class="gt_row gt_center">30</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Post-op NNNS stress</td>
<td headers="stat_1" class="gt_row gt_center">0.08 (0.06, 0.10)</td>
<td headers="stat_2" class="gt_row gt_center">0.06 (0.02, 0.08)</td>
<td headers="p.value" class="gt_row gt_center">0.2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    N-Missing</td>
<td headers="stat_1" class="gt_row gt_center">30</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Post-op NNNS arousal</td>
<td headers="stat_1" class="gt_row gt_center">3.64 (2.86, 4.26)</td>
<td headers="stat_2" class="gt_row gt_center">3.57 (3.29, 3.86)</td>
<td headers="p.value" class="gt_row gt_center">0.7</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    N-Missing</td>
<td headers="stat_1" class="gt_row gt_center">30</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Post-op NNNS hypertonic</td>
<td headers="stat_1" class="gt_row gt_center">0 (0, 0)</td>
<td headers="stat_2" class="gt_row gt_center">0 (0, 0)</td>
<td headers="p.value" class="gt_row gt_center">0.3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    N-Missing</td>
<td headers="stat_1" class="gt_row gt_center">30</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Post-op NNNS hypotonic</td>
<td headers="stat_1" class="gt_row gt_center">0 (0, 1)</td>
<td headers="stat_2" class="gt_row gt_center">0 (0, 0)</td>
<td headers="p.value" class="gt_row gt_center">0.2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    N-Missing</td>
<td headers="stat_1" class="gt_row gt_center">30</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Post-op NNNS asymmetry</td>
<td headers="stat_1" class="gt_row gt_center">0 (0, 1)</td>
<td headers="stat_2" class="gt_row gt_center">0 (0, 1)</td>
<td headers="p.value" class="gt_row gt_center">0.6</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    N-Missing</td>
<td headers="stat_1" class="gt_row gt_center">30</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Post-op NNNS excitability</td>
<td headers="stat_1" class="gt_row gt_center">6 (5, 7)</td>
<td headers="stat_2" class="gt_row gt_center">3 (1, 4)</td>
<td headers="p.value" class="gt_row gt_center">0.004</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    N-Missing</td>
<td headers="stat_1" class="gt_row gt_center">30</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Post-op NNNS lethargy</td>
<td headers="stat_1" class="gt_row gt_center">5 (4, 5)</td>
<td headers="stat_2" class="gt_row gt_center">5 (4, 7)</td>
<td headers="p.value" class="gt_row gt_center">0.3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    N-Missing</td>
<td headers="stat_1" class="gt_row gt_center">30</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Pre-op NNNS attention</td>
<td headers="stat_1" class="gt_row gt_center">3.67 (3.14, 4.02)</td>
<td headers="stat_2" class="gt_row gt_center">3.42 (2.80, 3.94)</td>
<td headers="p.value" class="gt_row gt_center">0.3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    N-Missing</td>
<td headers="stat_1" class="gt_row gt_center">16</td>
<td headers="stat_2" class="gt_row gt_center">50</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> Median (IQR); n (%)</td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>2</sup></span> Wilcoxon rank sum test; Pearson’s Chi-squared test; Fisher’s exact test; Wilcoxon rank sum exact test</td>
    </tr>
  </tfoot>
</table>
</div>
