# The Impact of Neurobehavior on Feeding Outcomes in Neonates with CHD
Haojia Li
Invalid Date

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
