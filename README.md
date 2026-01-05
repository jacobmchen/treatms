# TREAT-MS Analysis Code

``data_exploration.R`` contains preliminary analysis code. At the moment, all the code is in a single file, so some refactoring needs to be done.

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

2. Use MICE to fill in missing data for all covariates.

3. Compute the censoring time for each individual. The censoring time will be the last value at which every observation afterwards is a missing value. We need to compute the censoring time for four variables: EDSS, T25FW, 9HPT dominant hand, and 9HPT non-dominant hand. The individual's censoring time is the maximum of these values.

4. Compute the event time (disease progression) for each individual.

    a. Sustained disability progression for EDSS is defined as an increase of $\geq 1$ if baseline EDSS is $\leq 5.5$ that is sustained 6 months later. If baseline EDSS is $\geq 6.0$, a sustained increase of $\geq 0.5$ qualifies as a significant change. For instance, an individual's observed EDSS scores could be [0:3, 6:4, 12:3.5, 18:4, 24:4] (the number to the left of the colon is the month, and the number to the right of the column is the observed EDSS score), and they would have experienced sustained disability progression at month 18.

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
