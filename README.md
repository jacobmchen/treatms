# TREAT-MS Analysis Code

The code for data pre-processing and implementation of the restricted mean survival time (RMST) analysis is contained in the folder ``primary_analysis``. The data itself is not contained in the repository due to data privacy. A short description of each file in the repository is as follows:
- The file ``global_variables.R`` contains declarations for global variables that are used throughout this analysis.
- The file ``compute_censoring_time.R`` computes the censoring time for each individual.
- The file ``get_covariate_data.R`` retrieves and cleans the baseline covariate data.
- The file ``event_time.R`` computes the event time (sustained disability progression) for each individual, if they experienced the event. Missing values are imputed using MICE under the MAR assumption. To run this file, it needs the outputs from the two previous files.
- The file ``combine_data.R`` combines the data computed in the above three files into one centralized dataset.
- The file ``rmst_analysis.R`` executes the RMST analysis and outputs the RMST for each treatment group as well as the square root of the variance for the difference in means estimate. This file also contains simulations where we try different time windows and evaluate the variance.
- The file ``plot_simulation_results.R`` plots simulation results from the variance simulations in the ``rmst_analysis.R`` file.

The folder ``pdds_explore`` contains some code plotting histograms for the PDDS score at various time intervals as well as the histograms themselves.

Below are notes on codebase updates with dates.

2025-12-19
- Out of the patients who returned for at least 1 post-baseline follow-up visit, how many have at least 2 post-baseline EDSS+ measured?
- We will run this query on a separate file ``edss_missingness.R``.
- First, we need all patients with at least one baseline visit. We will subset the data to just those patients.
- Then, we will need all patients with at least 1 post-basline follow-up visit. We will subset the data to just those patients.
- Finally, we will need all patients with at least 2 post-baseline follow-up visits. We will report the number of patients with at least 2 post-baseline out of the number of patients with at least 1 post-baseline follow-up visit.

2025-12-30

The file ``test_estimator.R`` contains example code provided by Diaz et al. to demonstrate their method. We thoroughly annotate their code to understand how to clean our data to pass it through their functions.

We are going to implement the primary analysis of time to disease progression. We will use the implementation provided by Diaz et al. (contained in the file ``estimator_functions.R``) to estimate the RMST for A=0 and A=1 while including the cluster as a covariate. 

2026-01-05

We need to clean the longitudinal data to have the following properties:

- All covariates listed out for each individual.

- One-hot encode all categorical variables, including the location of the cluster.

- Each individual has two rows where all variables are the same between the two rows. One row will contain the censoring time ($event_type = 1$) and one row will contain the event time ($event_type=2$). If the event did not happen for a particular individual, then their event time will be the same as their censoring time.

The steps of the data cleaning are as follows:

1. One-hot encode all categorical variables.

    - The baseline covariates are: randomization site, risk stratification, age at consent, sex at birth, race, ethnicity.
    
    - We remove patient 0225-016 since we don't have any data on them, but they show up in the baseline chars data.

2. Use MICE to fill in missing data for all covariates.

    - The baseline covariates listed in the previous step end up being fully observed.

3. Compute the censoring time for each individual. The censoring time will be the last value at which every observation afterwards is a missing value. We need to compute the censoring time for four variables: EDSS, T25FW, 9HPT dominant hand, and 9HPT non-dominant hand. The individual's censoring time is the maximum of these values.

4. Compute the event time (disease progression) for each individual.

    a. Sustained disability progression for EDSS is defined as an increase of $\geq 1$ from baseline if baseline EDSS is $\leq 5.5$ that is sustained 6 months later. If baseline EDSS is $\geq 6.0$, a sustained increase of $\geq 0.5$ from baseline qualifies as a significant change. For instance, an individual's observed EDSS scores could be [0:3, 6:4, 12:3.5, 18:4, 24:4] (the number to the left of the colon is the month, and the number to the right of the column is the observed EDSS score), and they would have experienced sustained disability progression at month 18.

    b. Sustained disability progression for timed 25 foot walk (T25FW) and 9 hole peg test (9HPT) is defined as an increase of $\geq 20\%$ of the baseline value that is sustained 6 months later. 

    c. We need to be careful about how we handle missing values between the baseline observation and the censoring time. For instance, an individual's observed EDSS scores may be [0:3, 6:4, 12:NA, 18:NA, 24:3]. The values at month 12 and month 18 could have been any value, even 10. If at month 12, the missing value was 4, then they would have experienced sustained disability progression at month 6.

    - If the last observed value at time $t$ was a significant increase compared to baseline, then just one missing value could potentially be sustained disability progression at time $t$.
    - If the last observed value at time $t$ was not a significant increase, then one missing value after time $t$ is not enough for there to be sustained disability progression at time $t$.
    - If at time $t$ there was a significant increase and there is a missing value at time $t-1$, then there could be sustained disability progression at time $t-1$.
    - If there are two consecutive missing values, then there could have been sustained disability progression.

    d. To start, we will use linear interpolation where we use the two observed values at each end of the missingness span and draw a straight line between them to fill in the missing values. Then, we will repeat the analysis two times: once with a "best-case" imputation method where all missing values have the most favorable EDSS score possible then a "worst-case" imputation method where all missing values have the worst EDSS score possible.

    e. We compute an event time for four variables: EDSS, T25FW, 9HPT dominant hand, and 9HPT non-dominant hand. The event time for the individual is the minimum of these four values. If disease progression does not occur for any of these values, then the patient did not experience disease progression.

5. Combine all of the pieces above together.

With the cleaned data, we will apply the code from Diaz et al. to estimate the RMST.

In order to promote a more modular code design that is easier to understand, revisit, and update over time, each of the numbered points will be developed in separate modules and files.

2026-02-11

We seek to plot histograms for the PDDS score. We will plot histograms of the baseline PDDS score and even numbered visits. The code for plotting these histograms are contained in the folder ``pdds_explore``, in the file ``plot_histograms.R``.

2026-02-16

Rename the folder ``data_clean`` to ``primary_analysis`` to better align with what the files in the folder are actually doing.

Need to add simulations in the ``primary_analysis`` folder where we simulate treatments randomly and run the RMST analysis with different restriction windows.

We want to change the way we deal with missing data in the RMST analysis to be through MICE rather than through linear interpolation, which is what we have implemented now. This was successfully implemented in the file ``rmst_analysis.R`` file.

2026-02-17

After implementing the simulation study successfully, we need to plot the results. This is done in the file ``plot_simulation_results.R``, at the end of the file.

2026-02-20

We now seek to evaluate the variance of the RMST estimator under the following scenarios:

1. One-hot encode and adjust on all clusters.
2. Combine some of the clusters.
3. Leave all clusters out.

The current implementation already adjusts on all clusters, so we simply need to modify ``get_covariate_data.R`` to process the covariate data differently and re-run the analysis using these different versions of the cleaned data.

In my implementations, I endeded up doing two different versions of 2. One version combined all clusters with less than 10 patients as "Other", and one version converted the clusters to the states that they were in. To get the implementation to flow more smoothly I changed a lot of files to define a function for the task and re-run the respective tasks for the different versions of the dataset that handle the clusters differently.

2026-03-11

We received updated data with additional covariates and additional outcome data. The first order of business is to update the input file for all of the analysis files so that they read this new spreadsheet instead of the old one. To make changes like this more modular in the future, I'm going to create one file that defines the name of the string of the spreadsheet to read the data from, and all analysis files will use this string.

This file is titled ``global_variables.R`` and will contain the filename as well as all of the global variables needed for this analysis in the future.

One thing to watch out for is that sometimes NA values are populated with the string "not obtained".

2026-03-13

Updated global variables to use the updated file and also added a new function that preprocesses MSFC data with mean values.

Still left to do includes processing the baseline covariate variables that are predictive of outcome. We need to clean it up and see how much missingness there is in there. After that, we will impute the missing values with MICE as usual.

2026-03-15

Notes on updates for covariate data.

- There are a couple of variables in the baseline covariates that overlap with the baseline characteristics that we already have. These variables are: male, older or very young, African American, Hispanic.
- First thing, I will want to check that the four variables above actually match with the baseline characteristics variables.
    - Patient ids 0400-011 and 0400-020 have missing values in baseline covariates since they are early withdrawal patients. However, their values are observed in baseline characteristics data.
    - For the column gender, there are two patients with conflicting values. In baseline characteristics, patient ids 0402-002 and 0412-021 are female, but these patient ids are male in baseline covariates. Does it also make more sense to code non-binary the same as not male since there's only 3 patients that are non-binary?
    - For the column race/african american, the baseline characteristics data is more fine since some patients are labelled as multiple races. I think it makes more sense to use the column race/african american since the label "MULTIPLE" could mean a lot of things, and it doesn't make sense to group patients labelled as "MULTIPLE" together.
    - For the column ethnicity/hispanic, some patients are labelled as hispanic in baseline characteristics but not hispanic in baseline covariates, or vice versa.
- Summary of discrepancies:
    - Patients 0402-002 and 0412-021 are coded as female in baseline chars but male in baseline covars. 
    - Patient 0101-010 is coded as American Indian or Alaska Native in baseline chars but African American in baseline covars.
    - Patients 0241-006 and 0289-023 are coded as Other Race in baseline chars but African American in baseline covars. 
    - Patients  "0104-098" "0104-128" "0238-014" "0241-022" "0241-025" "0267-004" "0267-006" "0405-014" "0420-005" "0420-007" have different values for Hispanic vs. Not Hispanic between baseline chars and baseline covars.

2026-03-16

Notes on data cleaning for the columns in the baseline covars data.

- For now, use gender from baseline chars, AfrAmerican from baseline covars, and Hispanic from baseline chars.
- For some early withdrawal patients, we have missing data on race in baseline covars whereas those values are observed in baseline chars.
- Summary of missing values for baseline covars: "AfrAmerican": 6, "early second relapse missing": 81, "frequent relapses": 71, "incomplete recovery missing": 68, "high lesion burden missing": 33, "new T2 lesions missing": 484, "enchancing lesions missing": 55, "BS cerebellum SC missing": 18.
- Updated ``get_covariate_data.R`` to clean the baseline covars data and merge them with the baseline chars data. As before there are 4 versions of the baseline covar data based on how the clusters are handled: all clusters, no clusters, merge rare clusters, and merge clusters by state.
