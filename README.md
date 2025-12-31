# TREAT-MS Analysis Code

``data_exploration.R`` contains preliminary analysis code. At the moment, all the code is in a single file, so some refactoring is probably due soon.

2025-12-19
- Out of the patients who returned for at least 1 post-baseline follow-up visit, how many have at least 2 post-baseline EDSS+ measured?
- We will run this query on a separate file ``edss_missingness.R``.
- First, we need all patients with at least one baseline visit. We will subset the data to just those patients.
- Then, we will need all patients with at least 1 post-basline follow-up visit. We will subset the data to just those patients.
- Finally, we will need all patients with at least 2 post-baseline follow-up visits. We will report the number of patients with at least 2 post-baseline out of the number of patients with at least 1 post-baseline follow-up visit.

2025-12-30

The file ``test_estimator.R`` contains example code provided by Diaz et al. to demonstrate their method. We thoroughly annotate their code to understand how to clean our data to pass it through their functions.

We are going to implement the primary analysis of time to disease progression. We will use the implementation provided by Diaz et al. (contained in the file ``estimator_functions.R``) to estimate the RMST for A=0 and A=1 while including the cluster as a covariate. The steps of the analysis are as follows
1. Clean the longitudinal data to have (i) censoring time and (ii) time to event. We will need to be careful about how to handle missing data here for time to disease progression since it may be difficult to compute it if there are missing values in between visits. For now, we'll impute missing values as a linear progression between the two time points that were observed.
2. 
