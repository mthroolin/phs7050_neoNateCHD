library(tidyverse)
library(mice) # imputation

# clean up variable names
# remove the dots at the end of variable names
colnames(nnns0) <- gsub("\\.+$", "", colnames(nnns0))
# replace all the dots in variable names with underscores
colnames(nnns0) <- gsub("\\.+", "_", colnames(nnns0))

# summary of the data
# summary(nnns0)

nnns <- nnns0 |> 
  
  filter(
    # exclude neurologic or airway anomaly
    Neurologic_Complication == 0, AirwayAnomalyYN == 0,
    # include infants from birth to 4 weeks old
    # there are two outliers with age at surgery > 30 days
    Age_at_Surgery_days <= 30
  ) |>
  
  mutate(
    
    # some binary variables have values of 1/2 or Y/N, recode them to 0/1
    Female = as.integer(sex_1_M_2_F == 2),
    Premature = as.integer(Premature == 1),
    Extubation_failure = as.integer(Extubation_failure == "Y"),
    
    # for model building purposes, combine the 2 levels w/o arch obstruction in cardiac anatomy
    Cardiac_Anatomy = factor(case_when(
      Cardiac_Anatomy %in% c(1,3) ~ 1,
      Cardiac_Anatomy == 2 ~ 2,
      Cardiac_Anatomy == 4 ~ 3
    ), levels = 1:3, labels = c("W/o arch obstruction", "Single ventricle w/ arch obstruction", "Two ventricle w/ arch obstruction"))
    
  ) |>
  
  # convert date variables to date class
  mutate_at(
    vars("Date_PO_feeds_started", "Date_Reaching_Full_PO", "Date_Identified_as_not_yet_full_PO"), 
    as_date, format = "%m/%d/%Y"
  ) |> 
  
  # drop unnecessary variables
  select(!c(
    "sex_1_M_2_F", # use Female instead
    "Intubated_Pre_operatively", "bypass_used", "bypass_time_min", # not of interest 
    "Neurologic_Complication", "AirwayAnomalyYN" # already excluded
  )) 


nnns <- nnns |>
  # create indicator of missing pre-op and post-op habituation score
  mutate(
    Pre_Op_habituation_missing = as.integer(is.na(Pre_Op_NNNS_habituation_score)),
    Post_Op_habituation_missing = as.integer(is.na(Post_Op_NNNS_habituation_score))
  ) |>
  # create indicator of missing pre-op and post-op attention score
  mutate(
    Pre_Op_attention_missing = as.integer(is.na(Pre_Op_NNNS_attention_score)),
    Post_Op_attention_missing = as.integer(is.na(Post_Op_NNNS_attention_score))
  ) |>
  # create a count variable of missing NNNS scores
  mutate(
    preop_nnns_missing = rowSums(is.na(pick(preop_nnns))) - Pre_Op_attention_missing - Pre_Op_habituation_missing,
    postop_nnns_missing = rowSums(is.na(pick(postop_nnns))) - Post_Op_attention_missing - Post_Op_habituation_missing
  )

nnns <- nnns |> mutate(
  Arousal_missing = factor(case_when(
    !is.na(Pre_Op_NNNS_Arousal_Score) & !is.na(Post_Op_NNNS_Arousal_Score) ~ 0,
    is.na(Post_Op_NNNS_Arousal_Score) ~ 1,
    TRUE ~ 2
  ), levels = 0:2, labels = c("Both assessed", "Pre-op assessed only", "Post-op assessed only"))
)

basevar_imp <- c(
  "Age_at_Surgery_days", "Female",
  "Genetic_Syndrome_or_Chromosomal_Abnormality", "Cardiac_Anatomy",
  "Length_of_intubation_days"
) |> paste(collapse = " + ")

nnns_mice <- mice(
  nnns, 
  # avoid imputing the date variables
  where = as.data.frame.array(is.na(nnns)) |>
    mutate_at(vars(starts_with("Date")), \(x) x = FALSE),
  m = 20, maxit = 10, seed = 7050, print = F,
  formulas = list(
    # ---- formula for pre-op scores ----
    Pre_Op_NNNS_handling_score = as.formula(
      paste("Pre_Op_NNNS_handling_score ~", basevar_imp, "+ Pre_Op_habituation_missing + Post_Op_attention_missing + Post_Op_habituation_missing")
    ),
    Pre_Op_NNNS_Quality_of_Movement_Score = as.formula(
      paste("Pre_Op_NNNS_Quality_of_Movement_Score ~", basevar_imp, "+ Post_Op_attention_missing")
    ),
    Pre_Op_NNNS_Regulation_Score = as.formula(
      paste("Pre_Op_NNNS_Regulation_Score ~", basevar_imp, "+ Pre_Op_habituation_missing + Post_Op_attention_missing")
    ),
    Pre_Op_NNNS_Non_Optimal_Reflexes_Score = as.formula(
      paste("Pre_Op_NNNS_Non_Optimal_Reflexes_Score ~", basevar_imp, "+ Post_Op_attention_missing")
    ),
    Pre_Op_NNNS_Stress_Score = as.formula(
      paste("Pre_Op_NNNS_Stress_Score ~", basevar_imp, "+ Post_Op_attention_missing")
    ),
    Pre_Op_NNNS_Arousal_Score = as.formula(
      paste("Pre_Op_NNNS_Arousal_Score ~", basevar_imp, "+ Post_Op_attention_missing")
    ),
    Pre_Op_NNNS_Hypertonic_Score = as.formula(
      paste("Pre_Op_NNNS_Hypertonic_Score ~", basevar_imp, "+ Post_Op_attention_missing")
    ),
    Pre_Op_NNNS_Hypotonic_Score = as.formula(
      paste("Pre_Op_NNNS_Hypotonic_Score ~", basevar_imp, "+ Post_Op_attention_missing")
    ),
    Pre_Op_NNNS_Asymmetry_Score = as.formula(
      paste("Pre_Op_NNNS_Asymmetry_Score ~", basevar_imp, "+ Post_Op_attention_missing")
    ),
    Pre_Op_NNNS_Excitability_Score = as.formula(
      paste("Pre_Op_NNNS_Excitability_Score ~", basevar_imp, "+ Post_Op_attention_missing")
    ),
    Pre_Op_NNNS_Lethargy_Score = as.formula(
      paste("Pre_Op_NNNS_Lethargy_Score ~", basevar_imp, "+ Post_Op_attention_missing")
    ),
    # ---- formula for post-op scores ----
    Post_Op_NNNS_handling_score = as.formula(
      paste("Post_Op_NNNS_handling_score ~", basevar_imp, "+ Percent_of_feeds_taken_by_mouth_at_discharge + Pre_Op_habituation_missing + Post_Op_attention_missing")
    ),
    Post_Op_NNNS_Quality_of_Movement_Score = as.formula(
      paste("Post_Op_NNNS_Quality_of_Movement_Score ~", basevar_imp, "+ Percent_of_feeds_taken_by_mouth_at_discharge + Pre_Op_habituation_missing + Pre_Op_attention_missing")
    ),
    Post_Op_NNNS_Regulation_Score = as.formula(
      paste("Post_Op_NNNS_Regulation_Score ~", basevar_imp, "+ Percent_of_feeds_taken_by_mouth_at_discharge + Pre_Op_habituation_missing + Pre_Op_attention_missing")
    ),
    Post_Op_NNNS_Non_Optimal_Reflexes_Score = as.formula(
      paste("Post_Op_NNNS_Non_Optimal_Reflexes_Score ~", basevar_imp, "+ Percent_of_feeds_taken_by_mouth_at_discharge + Pre_Op_habituation_missing + Pre_Op_attention_missing")
    ),
    Post_Op_NNNS_Stress_Score = as.formula(
      paste("Post_Op_NNNS_Stress_Score ~", basevar_imp, "+ Percent_of_feeds_taken_by_mouth_at_discharge + Pre_Op_habituation_missing + Pre_Op_attention_missing")
    ),
    Post_Op_NNNS_Arousal_Score = as.formula(
      paste("Post_Op_NNNS_Arousal_Score ~", basevar_imp, "+ Percent_of_feeds_taken_by_mouth_at_discharge + Pre_Op_habituation_missing + Pre_Op_attention_missing")
    ),
    Post_Op_NNNS_Hypertonic_Score = as.formula(
      paste("Post_Op_NNNS_Hypertonic_Score ~", basevar_imp, "+ Percent_of_feeds_taken_by_mouth_at_discharge + Pre_Op_habituation_missing + Pre_Op_attention_missing")
    ),
    Post_Op_NNNS_Hypotonic_Score = as.formula(
      paste("Post_Op_NNNS_Hypotonic_Score ~", basevar_imp, "+ Percent_of_feeds_taken_by_mouth_at_discharge + Pre_Op_habituation_missing + Pre_Op_attention_missing")
    ),
    Post_Op_NNNS_Asymmetry_Score = as.formula(
      paste("Post_Op_NNNS_Asymmetry_Score ~", basevar_imp, "+ Percent_of_feeds_taken_by_mouth_at_discharge + Pre_Op_habituation_missing + Pre_Op_attention_missing")
    ),
    Post_Op_NNNS_Excitability_Score = as.formula(
      paste("Post_Op_NNNS_Excitability_Score ~", basevar_imp, "+ Percent_of_feeds_taken_by_mouth_at_discharge + Pre_Op_habituation_missing + Pre_Op_attention_missing")
    ),
    Post_Op_NNNS_Lethargy_Score = as.formula(
      paste("Post_Op_NNNS_Lethargy_Score ~", basevar_imp, "+ Percent_of_feeds_taken_by_mouth_at_discharge + Pre_Op_habituation_missing + Pre_Op_attention_missing")
    ),
    # ---- formula for attention and % oral feed ----
    Pre_Op_NNNS_attention_score = as.formula(
      paste("Pre_Op_NNNS_attention_score ~", basevar_imp, "+ postop_nnns_missing + Pre_Op_NNNS_Regulation_Score + Pre_Op_NNNS_Non_Optimal_Reflexes_Score + Pre_Op_NNNS_Lethargy_Score + Post_Op_NNNS_Regulation_Score")
    ),
    Post_Op_NNNS_attention_score = as.formula(
      paste("Post_Op_NNNS_attention_score ~", basevar_imp, "+ preop_nnns_missing + Post_Op_NNNS_Quality_of_Movement_Score + Post_Op_NNNS_Regulation_Score + Post_Op_NNNS_Non_Optimal_Reflexes_Score +  Post_Op_NNNS_Excitability_Score + Post_Op_NNNS_Lethargy_Score + Pre_Op_NNNS_handling_score")
    ),
    Percent_of_feeds_taken_by_mouth_at_discharge = as.formula(
      paste0("Percent_of_feeds_taken_by_mouth_at_discharge ~", basevar_imp, "+ GI_Complication + Post_Op_NNNS_Stress_Score + Post_Op_NNNS_Asymmetry_Score + Post_Op_NNNS_Non_Optimal_Reflexes_Score + Post_Op_habituation_missing")
    )
  )
)

# save the imputed data
# saveRDS(nnns_mice, "nnns_imputed.rds")