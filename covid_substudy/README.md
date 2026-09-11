# COVID-19 Substudy

This folder contains code for the COVID-19 substudy.

There will be code for preprocessing the data. Then, there will be code for running 6 linear logistic regressions and their respective likelihood ratio tests. Finally, there will be code for conducting a Fisher's exact test.

First, we list out baseline covariates for the substudy that we will control for in logistic regressions:
- age,
- gender,
- race,
- ethnicity,
- social support survey total score,
- degree of social distancing.

The exact analyses we need to conduct are listed below:
- Fit a logistic regression model for MS breakthrough disease as a function of DMT altered adherence and above covariates.
- Fit a logistic regression model for new/worsened MS symptoms as a function of cancelled visits and above covariates.
- Fit a logistic regression model for DMT altered adherence and as a function of Neuro-QoL Anxiety T-score and above covariates.
- Fit a logistic regression model for DMT altered adherence and as a function of Neuro-QoL Depression T-score and above covariates.
- Fit a logistic regression model for postponed/cancelled visits and as a function of Neuro-QoL Anxiety T-score and above covariates.
- Fit a logistic regression model for postponed/cancelled visits and as a function of Neuro-QoL Depression T-score and above covariates.
- Perform a Fisher's exact test on the relationship between treatment class and hospitalization due to COVID-19.

The files in this folder are as follows:
- ``README.md`` (this file): documentation.
- ``global_variables.R`` states file names and other global variables used throughout this folder.
- ``process_breakthrough.R`` processes the data to get a dataframe with a binary indicator for whether each patient experienced breakthrough disease during the substudy period. The file then saves this dataframe as an RDS file for future processing.
- ``process_adherence.R`` does the same as above but for DMT altered adherence instead.
- ``process_postpone.R`` does the same as above but for whether a patient postponed or cancelled a visit.
- ``process_newworse.R`` does the same as above but for whether a patient has new or worsening symptoms.
- ``process_covariates.R`` processes covariate data.
