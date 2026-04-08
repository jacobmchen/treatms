# TREAT-MS Analysis Code

The code for data pre-processing and implementation of the restricted mean survival time (RMST) analysis is contained in the folder ``primary_analysis``. The data itself is not contained in the repository due to data privacy. A short description of each file in the repository is as follows:
- The file ``global_variables.R`` contains declarations for global variables that are used throughout this analysis.
- The file ``compute_censoring_time.R`` computes the censoring time for each individual.
- The file ``get_covariate_data.R`` retrieves and cleans the baseline covariate data. The end of the file also currently contains some code that attempts to check for collinearity of the covariates.
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
- Summary of missing values for baseline covars: "AfrAmerican": 6, "early second relapse": 81, "frequent relapses": 71, "incomplete recovery": 68, "high lesion burden": 33, "new T2 lesions": 484, "enchancing lesions": 55, "BS cerebellum SC": 18.
- Updated ``get_covariate_data.R`` to clean the baseline covars data and merge them with the baseline chars data. As before there are 4 versions of the baseline covar data based on how the clusters are handled: all clusters, no clusters, merge rare clusters, and merge clusters by state.
- At the end of ``get_covariate_data.R``, added some short code that checks for collinearity of the covariate data by checking the condition number of the design matrix. Basically, merging clusters based on state gives the lowest condition number. Merging rare clusters is a bit better. Not having clusters at all is very collinear.

2026-03-20

Notes on data cleaning for the column in the baseline covars data after today's meeting with collaboraters.

- For relapse, we only need one symptom to recover in order for patient to be considered to have complete relapse recovery. We have a column for complete recovery and a column for incomplete recovery. It may be the case that a patient who has relapse has neither complete nor incomplete recovery. This may occur if for example the patient has sustained worsening of their symptoms or if their relapse symptoms worsen at month 6 but then improve to levels that are still above baseline.
- For discrepancies between baseline char and covar data, we will defer to baseline char data as this is what the patient filled in. For age, we can just use the patient's actual age. We will also get additional sex at birth data to make sure that the data is just binary.
- The variable "new T2 lesions" has a lot of Unknown values. We will get additional data that fills in these Unknown values based on what the clinicians actually inputted into the risk stratification forms.

Updated ``get_covariate_data.R`` to use baseline chars for gender, ethnicity, and race. We still have a lot of missing data for other baseline covariate data, but we will receive that soon.

2026-03-25

Received some new data, and here are some notes on it.

Sex at birth data cleaning.

- The social status tab has a new column titled "sex_at_birth". It seems like it is haphazardly filled in, though, so will need to check it for missing data.
- There are 243 patients for whom there is no value for "sex_at_birth".
- Patient 0402-002 should be coded as female (which they already are in baseline chars data). Patient 0412-021 is transgender; their sex at birth should be male, and their gender should be female.

Risk strat data cleaning.

- The risk strat data contains one row per patient. Depending on when the patient enrolled, different questions are answered for them. If they enrolled within 6 months of their first attack, there are two questions answered. Otherwise, there are 4 questions answered. At first glance, there shouldn't be any missing values.
- The difficulty with using risk strat data to fill in baseline covariates is that the risk strat questions use OR logic for a couple of the questions, so it's hard to tell which conditions are actually checked off. For instance, if I want to know whether certain patients had new T2 lesions and I see a "Yes" in the risk stratification data, 3 other variables could have been "Yes" instead of new T2 lesions.
- Basically, the risk stratification data does not help us change "Unknown" to "Yes" or "No".

2026-03-26

Sex at birth data cleaning continued.

- Use the sex at birth variable where available, and use gender where it's missing. There is only one patient whose gender and sex at birth values do not match, patient 0270-012. Furthermore, there is one patient whose sex at birth is male since they are transgender, patient 0412-021. Since we are using sex at birth as the primary variable, we should change this patient's sex at birth to male.
- Successfully updated the ``get_covariate_data.R`` code to use sex at birth data and fill in gender when needed.

2026-03-30 to 2026-04-01

- New T2 lesions variable: we decided that there is probably not enough information in this baseline covariate due to the high amount of missingness for it to be helpful. Therefore, I am just going to drop this column in ``get_covariate_data.R``.
- By the way, quick sanity check on the baseline covariate data: aside from new T2 lesions, each baseline covar has at least around 25% patients with a value of 1. This gives some confidence that these variables have some information on the outcome.
- Need to impute missing values for the other baseline covars when they have an unknown value. This is now taken care of in the ``get_covariate_data.R`` file.

- Create a new folder ``secondary_analyses`` to store code for all of the secondary analyses. We wish to start coding the secondary analysis for relapse recovery, both patient-reported and exam-based.
- A total of 97 patients have exam-confirmed relapse at least once. 15 patients experience relapse more than once. These patients are "0100-123" "0173-071" "0216-040" "0231-002" "0262-002" "0267-001" "0267-086" "0267-091" "0289-004" "0289-011" "0300-028" "0401-002" "0403-013" "0410-016" "0425-011".
- One patient has 3 relapses, each about a year apart. This patient is "0289-011".
- For the patients that have multiple relapses, their second relapse can happen within months, or it can be about a year later. What to do about patients with multiple relapses? Ignore them for now, but can change how we deal with them later.

- As collaborators mentioned, there are 7 patients for whom they had an exam-confirmed, yet they do not have any recorded symptoms. For now, we will ignore these patients until we receive data regarding which symptoms are associated with their relapse. For now, though, these patients are "0106-012" "0173-053" "0231-035" "0256-029" "0265-034" "0301-035" "0427-013".
- Some patients have only an "Interim Information" for their form group data, which makes it hard to match over to the EDSS data since the EDSS data is recorded in terms of which 6 month interval the visit is. To account for this, the 9 patients with "Interim Information" will use their visit dates to match with the closest 6 month interval visit in the relapse data.

2026-04-08

- EDSS data for certain subscores sometimes just says "Not Obtained". I suppose we can consider this as missing data?
