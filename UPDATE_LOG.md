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
- For patient 0100-007, their date of relapse is Month 12 and their relevant symptom was Sensory, but their FSS score at Month 12 for Sensory is 0. Furthermore, their EDSS data at Month 24 is not recorded. Is it possible to calculate relapse recovery in this case?
- At the moment, there are 75 patients that experience only one relapse and have at least one recorded symptom. However, 33 of these patients have at least one missing value for their relevant functional system scores. How can we compute relapse recovery for these patients? Do we just try to impute their functional system scores like we did with total EDSS scores?
- The easiest way to deal with the above problem is probably to impute all of the functional system scores using MICE assuming MAR, just like we did when computing time to sustained disability progression. We can do this by just reusing the code we wrote in ``event_time.R``, except we ask it to impute missing values for all of the functional system scores as a function of all of the covariates. Then, we use the resulting dataframe to compute relapse recovery.
- The imputed data is stored in ``imputed_edss_data.RDS`` in the ``primary_analysis`` folder.
- When processing EDSS data, we have to ignore patient 0256-013 because we have no data for them.
- Fixed bug in ``event_time.R`` where MICE was imputting data for a patient for whom we don't have covariate data for, 0256-013. This patient is early withdrawal anyways, so we don't need to worry about them. This should also make the run time for the imputation faster.

2026-04-10

- Collaborators asked for the list of patients that experience multiple relapses. The function find_multiple_relapse_patients in the file ``exam_based_relapse_recovery.R`` outputs a csv file of patients that experience multiple relapses.

2026-04-14 to 2026-04-15

- Collaborators state that we can make the definition of relapse recovery simpler, just record whether patients had complete recovery, or not. To this end, we will define the outcome as "exam-based __complete__ relapse recovery" rather than "exam-based __incomplete__ relapse recovery". The working definition of complete relapse recovery is: A patient is said to have complete relapse recovery if ALL increased functional system scores (FSS) associated with symptoms of relapse return to pre-relapse values or lower 6 months after the exam-confirmed relapse. Pre-relapse values are defined as the value(s) of the relevant FSS 6 months before the exam-confirmed relapse.
- This is now implemented in ``exam_based_relapse_recovery.R`` in the folder ``secondary_analyses``.
- For patient determined relapse recovery, there is only one score to keep track of. But other than that, everything should be the same. To code this outcome, we need to first find which month patients had an exam-confirmed relapse. Then find the PDDS scores around the relapse and evaluate whether there was complete recovery. This is probably easiest done in the same file as ``exam_based_relapse_recovery.R`` and renaming the file to indicate that it does two things.
- However, a complication arises in that PDDS has missing values, just like for EDSS. Thus, we need to impute the values with MICE. We can probably do the imputation at the same time as we do the imputation for EDSS. This should align better with the research plan as well since I think one of the purposes for collecting PDDS is to make the imputation of EDSS better.

 - Successfully implemented imputation of PDDS scores along with EDSS scores. However, I think I need to refactor the imputation of EDSS and PDDS to a different file. Then, ``event_time.R`` can read this imputed data directly to compute the event time.
- Refactoring was successful, so now imputing EDSS and PDDS data can be done separately from computing the event time.
- Finished implementation of patient-determined relapse recovery.

2026-04-20 to 2026-04-23

- Want to investigate missingness in the updated MSIS data where we were given answers to individual questions. The answer is that there is not that much missingness at all in the MSIS-29 data. The vast majority of observations had all 29 questions answered.
- Want to create a function in the ``global_variables.R`` file that can convert visit dates and patient ids to 6 month visit windows. Can base this function off of the function I already wrote in ``relapse_recovery.R`` that does something similar.
- There seems to be a bug where every patient's race is set to value 0 in the covariate data. Will need to go back and check that it is properly imputed.
- The likelihood ratio test seems to give a low p-value even when treatment is randomized. Need to investigate why this is happening and whether the significance value is less than 0.05 more often than expected. Perhaps using a bootstrap likelihood ratio test is preferable here.

2026-05-04

- Next orders of business are to run the experiments for a bootstrap likelihood ratio test to make sure that we get a false positivity rate of 0.05. Also, I need to figure out why every patient's race is set to 0 in the covariate data. Perhaps something went wrong during the imputation of the data.
- Update for today: added comments to the secondary analysis of MSIS for clarity.

2026-05-18

- Figured out why every patient's race is set to 0. It is because the string I used to binarize race included spaces, but I had removed all the spaces in the column for race in a previous processing step. This bug should be fixed now after I updated the reference string binarizing race.
- Updated implementation of the no clusters covariate dataset to just remove the columns with site information fromt he dataset with all of the clusters.

2026-05-19

- Add functionality to ``get_covariate_data.R`` to specify whether to include the risk stratification variable.

2026-06-04

- For tertiary outcomes: a linear mixed effects model with age as the time variable, and both linear and quadratic terms for age.

2026-06-16

- According to the SAP, we will re-run the same (bootstrap) likelihood ratio test for most of the secondary outcomes. Thus, it will be helpful to define functions that run the likelihood ratio test for you and re-use these functions for each secondary outcome. We will implement this change by taking the implementations of the likelihood ratio tests in ``msis29.R`` and put them in ``global_variables.R``.
- Successfully refactored code so that the ``global_variables.R`` document contains the code for the chi-square tests. ``msis29.R`` still contains some code for simulation experiments that will verify whether the bootstrap likelihood ratio test has a false positivity rate of 0.05.

2026-06-23

- We hope to run simulations for the RMST analysis that give a sense as to how well the output of our analysis will cover the true causal effect without raising a false negative.
- We can do this by first randomly assigning treatment still, then go in and tweak the time to event outcomes only for those that were assigned treatment using a prespecified value. This works because after randomly assigning treatment, the mean for both potential outcomes should be just equal to the sample mean of the overall dataset. Then, after tweaking the outcome for those randomly assigned treatment, we will have artificially created a causal effect.
- We are trying to figure out the sample size needed to detect a certain causal effect.

2026-06-30

- Added a .sh file that runs the bootstrap likelihood ratio test simulation on the slurm machine for the biostats cluster.
- The bootstrap likelihood ratio test simulation is in the file ``msis29.R``. There are 100 simulations for assignment of the treatments and each bootstrap test repeats 200 times.
- It may be necessary to extend the time needed to run the simulations, in which case can just edit the .sh file for the script running the simulations.

2026-07-01

- The simulations from yesterday did not turn out as expected; the Type I error rate for the bootstrap likelihood ratio test was 27%, only slightly better than the normal likelihood ratio test Type I error rate of 29%.
- Added a new file ``likelihood_ratio_sim.R`` explicitly for running the bootstrap likelihood ratio test simulations.
- Now going to try the bootstrap likelihood ratio test except turn up the number of bootstraps to 1000. Also updated the script for the simulation to run in parallel instead.
- Will also need to write a script that interprets the results of the simulations in 100 separate files.
