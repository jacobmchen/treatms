# TREAT-MS Analysis Code

The code for data pre-processing and implementation of the restricted mean survival time (RMST) analysis is contained in the folder ``primary_analysis``. The data itself is not contained in the repository due to data privacy. A short description of each file in the repository is as follows:
- The file ``global_variables.R`` contains declarations for global variables that are used throughout this analysis. Importantly, one of these global variables is the function implementing the likelihood ratio test used in the secondary analyses.
- The file ``compute_censoring_time.R`` computes the censoring time for each individual.
- The file ``get_covariate_data.R`` retrieves and cleans the baseline covariate data. The end of the file also currently contains some code that attempts to check for collinearity of the covariates.
- The file ``impute_edss_pdds.R`` uses MICE to impute EDDS and PDDS values for every time point in the study and saves the imputed data into an .RDS file for future use.
- The file ``event_time.R`` computes the event time (sustained disability progression) for each individual, if they experienced the event. Missing values for MSFC are imputed using MICE under the MAR assumption. For EDSS, we use the imputed values from ``impute_edss_pdds.R``. To run this file, it needs the outputs from the three previous files (``compute_censoring_time.R``, ``get_covariate_data.R``, and ``impute_edss_pdds.R``). 
- The file ``combine_data.R`` combines the data computed in the above three files into one centralized dataset. In this file, we also simulate random treatment assignments.
- The file ``rmst_analysis.R`` executes the RMST analysis and outputs the RMST for each treatment group as well as the square root of the variance for the difference in means estimate. This file also contains simulations where we try different time windows and evaluate the variance.
- The file ``plot_simulation_results.R`` plots simulation results from the variance simulations in the ``rmst_analysis.R`` file.

In the folder ``secondary_analyses``,
- The file ``exam_based_relapse_recovery.R`` contains code for computing the secondary outcome exam based relapse recovery.

The folder ``pdds_explore`` contains some code plotting histograms for the PDDS score at various time intervals as well as the histograms themselves.

The file ``update_log.md`` contains detailed updates and notes by date for this repository.
