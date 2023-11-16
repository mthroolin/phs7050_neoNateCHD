# The Impact of Neurobehavior on Feeding Outcomes in Neonates with CHD
Haojia Li
2023-11-15

``` r
library(tidyverse)

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
```

      sex_1_M_2_F    Age_at_Surgery_days   Premature    
     Min.   :1.000   Min.   : 0.000      Min.   :1.000  
     1st Qu.:1.000   1st Qu.: 5.000      1st Qu.:1.000  
     Median :1.000   Median : 7.000      Median :1.000  
     Mean   :1.403   Mean   : 9.101      Mean   :1.124  
     3rd Qu.:2.000   3rd Qu.:10.000      3rd Qu.:1.000  
     Max.   :2.000   Max.   :70.000      Max.   :2.000  
                                                        
     Genetic_Syndrome_or_Chromosomal_Abnormality Cardiac_Anatomy GI_Complication  
     Min.   :0.000                               Min.   :1.000   Min.   :0.00000  
     1st Qu.:0.000                               1st Qu.:2.000   1st Qu.:0.00000  
     Median :0.000                               Median :3.000   Median :0.00000  
     Mean   :0.186                               Mean   :2.977   Mean   :0.09302  
     3rd Qu.:0.000                               3rd Qu.:4.000   3rd Qu.:0.00000  
     Max.   :1.000                               Max.   :4.000   Max.   :1.00000  
                                                                                  
     Length_of_Stay_days Length_of_intubation_days Extubation_failure
     Min.   :  6.00      Min.   : 0.400            Length:129        
     1st Qu.: 17.00      1st Qu.: 3.100            Class :character  
     Median : 23.00      Median : 4.900            Mode  :character  
     Mean   : 26.41      Mean   : 5.222                              
     3rd Qu.: 32.00      3rd Qu.: 6.300                              
     Max.   :132.00      Max.   :24.000                              
                                                                     
      bypass_used     bypass_time_min Intubated_Pre_operatively
     Min.   :0.0000   Min.   :  0.0   Length:129               
     1st Qu.:1.0000   1st Qu.:103.0   Class :character         
     Median :1.0000   Median :128.0   Mode  :character         
     Mean   :0.9302   Mean   :120.8                            
     3rd Qu.:1.0000   3rd Qu.:147.0                            
     Max.   :1.0000   Max.   :286.0                            
                                                               
     Neurologic_Complication Pre_Op_NNNS_habituation_score
     Min.   :0.00000         Min.   :3.00                 
     1st Qu.:0.00000         1st Qu.:7.00                 
     Median :0.00000         Median :8.50                 
     Mean   :0.04651         Mean   :7.69                 
     3rd Qu.:0.00000         3rd Qu.:9.00                 
     Max.   :1.00000         Max.   :9.00                 
                             NA's   :93                   
     Pre_Op_NNNS_attention_score Pre_Op_NNNS_handling_score
     Min.   :0.5115              Min.   :0.0000            
     1st Qu.:2.9842              1st Qu.:0.2500            
     Median :3.4751              Median :0.3750            
     Mean   :3.3865              Mean   :0.3958            
     3rd Qu.:3.9792              3rd Qu.:0.5000            
     Max.   :5.4905              Max.   :1.0000            
     NA's   :66                  NA's   :57                
     Pre_Op_NNNS_Quality_of_Movement_Score Pre_Op_NNNS_Regulation_Score
     Min.   :2.333                         Min.   :3.571               
     1st Qu.:4.167                         1st Qu.:4.283               
     Median :4.600                         Median :4.786               
     Mean   :4.458                         Mean   :4.777               
     3rd Qu.:4.833                         3rd Qu.:5.196               
     Max.   :5.167                         Max.   :6.143               
     NA's   :38                            NA's   :39                  
     Pre_Op_NNNS_Non_Optimal_Reflexes_Score Pre_Op_NNNS_Stress_Score
     Min.   : 1.000                         Min.   :0.00000         
     1st Qu.: 5.000                         1st Qu.:0.02551         
     Median : 6.000                         Median :0.06122         
     Mean   : 6.044                         Mean   :0.05513         
     3rd Qu.: 7.000                         3rd Qu.:0.06122         
     Max.   :10.000                         Max.   :0.20408         
     NA's   :38                             NA's   :39              
     Pre_Op_NNNS_Arousal_Score Pre_Op_NNNS_Hypertonic_Score
     Min.   :2.286             Min.   :0.0000              
     1st Qu.:3.286             1st Qu.:0.0000              
     Median :3.714             Median :0.0000              
     Mean   :3.631             Mean   :0.2967              
     3rd Qu.:3.929             3rd Qu.:1.0000              
     Max.   :5.143             Max.   :2.0000              
     NA's   :38                NA's   :38                  
     Pre_Op_NNNS_Hypotonic_Score Pre_Op_NNNS_Asymmetry_Score
     Min.   :0.0000              Min.   :0.0000             
     1st Qu.:0.0000              1st Qu.:0.0000             
     Median :1.0000              Median :1.0000             
     Mean   :0.7692              Mean   :0.7472             
     3rd Qu.:1.0000              3rd Qu.:1.0000             
     Max.   :4.0000              Max.   :4.0000             
     NA's   :38                  NA's   :38                 
     Pre_Op_NNNS_Excitability_Score Pre_Op_NNNS_Lethargy_Score
     Min.   :0.000                  Min.   : 2.000            
     1st Qu.:2.000                  1st Qu.: 5.000            
     Median :3.000                  Median : 6.000            
     Mean   :3.209                  Mean   : 6.659            
     3rd Qu.:4.000                  3rd Qu.: 8.000            
     Max.   :8.000                  Max.   :13.000            
     NA's   :38                     NA's   :38                
     Percent_of_feeds_taken_by_mouth_at_discharge Date_PO_feeds_started
     Min.   :0.0000                               Length:129           
     1st Qu.:0.0000                               Class :character     
     Median :0.0700                               Mode  :character     
     Mean   :0.2640                                                    
     3rd Qu.:0.4325                                                    
     Max.   :1.0000                                                    
     NA's   :5                                                         
     Date_Reaching_Full_PO Date_Identified_as_not_yet_full_PO
     Length:129            Length:129                        
     Class :character      Class :character                  
     Mode  :character      Mode  :character                  
                                                             
                                                             
                                                             
                                                             
     Post_Op_NNNS_habituation_score Post_Op_NNNS_attention_score
     Min.   :2.000                  Min.   :2.000               
     1st Qu.:8.000                  1st Qu.:3.857               
     Median :9.000                  Median :4.286               
     Mean   :7.633                  Mean   :4.433               
     3rd Qu.:9.000                  3rd Qu.:5.000               
     Max.   :9.000                  Max.   :7.143               
     NA's   :94                     NA's   :40                  
     Post_Op_NNNS_handling_score Post_Op_NNNS_Quality_of_Movement_Score
     Min.   :0.0000              Min.   :3.400                         
     1st Qu.:0.1250              1st Qu.:4.167                         
     Median :0.3750              Median :4.500                         
     Mean   :0.3424              Mean   :4.556                         
     3rd Qu.:0.5000              3rd Qu.:5.000                         
     Max.   :0.8750              Max.   :5.800                         
     NA's   :37                  NA's   :30                            
     Post_Op_NNNS_Regulation_Score Post_Op_NNNS_Non_Optimal_Reflexes_Score
     Min.   :2.500                 Min.   :0.000                          
     1st Qu.:4.414                 1st Qu.:1.000                          
     Median :4.967                 Median :2.000                          
     Mean   :4.868                 Mean   :1.889                          
     3rd Qu.:5.327                 3rd Qu.:3.000                          
     Max.   :6.333                 Max.   :6.000                          
     NA's   :31                    NA's   :30                             
     Post_Op_NNNS_Stress_Score Post_Op_NNNS_Arousal_Score
     Min.   :0.00000           Min.   :2.143             
     1st Qu.:0.03061           1st Qu.:3.214             
     Median :0.06122           Median :3.571             
     Mean   :0.06494           Mean   :3.533             
     3rd Qu.:0.08163           3rd Qu.:3.857             
     Max.   :0.24490           Max.   :5.000             
     NA's   :30                NA's   :30                
     Post_Op_NNNS_Hypertonic_Score Post_Op_NNNS_Hypotonic_Score
     Min.   :0.0000                Min.   :0.0000              
     1st Qu.:0.0000                1st Qu.:0.0000              
     Median :0.0000                Median :0.0000              
     Mean   :0.1313                Mean   :0.1818              
     3rd Qu.:0.0000                3rd Qu.:0.0000              
     Max.   :2.0000                Max.   :3.0000              
     NA's   :30                    NA's   :30                  
     Post_Op_NNNS_Asymmetry_Score Post_Op_NNNS_Excitability_Score
     Min.   :0.0000               Min.   :0.00                   
     1st Qu.:0.0000               1st Qu.:1.00                   
     Median :0.0000               Median :3.00                   
     Mean   :0.4848               Mean   :3.03                   
     3rd Qu.:1.0000               3rd Qu.:4.00                   
     Max.   :5.0000               Max.   :9.00                   
     NA's   :30                   NA's   :30                     
     Post_Op_NNNS_Lethargy_Score AirwayAnomalyYN  
     Min.   : 2.000              Min.   :0.00000  
     1st Qu.: 4.000              1st Qu.:0.00000  
     Median : 5.000              Median :0.00000  
     Mean   : 5.354              Mean   :0.06202  
     3rd Qu.: 6.000              3rd Qu.:0.00000  
     Max.   :10.000              Max.   :1.00000  
     NA's   :30                                   

``` r
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
    Cardiac_Anatomy_collapsed = case_when(
      Cardiac_Anatomy %in% c(1,3) ~ 1,
      Cardiac_Anatomy == 2        ~ 2,
      Cardiac_Anatomy == 4        ~ 3
    ) |> factor(
      levels = 1:3,
      labels = c(
        "W/o arch obstruction",
        "Single ventricle w/ arch obstruction",
        "Two ventricle w/ arch obstruction"
      )
    )
  ) |>
  
  # convert date variables to date class
  mutate_at(vars("Date_PO_feeds_started", "Date_Reaching_Full_PO", "Date_Identified_as_not_yet_full_PO"), as_date, format = "%m/%d/%Y")
```
