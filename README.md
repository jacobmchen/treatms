# TREAT-MS Analysis Code

The code for data pre-processing and implementation of the restricted mean survival time (RMST) analysis is contained in the folder ``primary_analysis``. The data itself is not contained in the repository due to data privacy. A short description of each file in the repository is as follows:
- The file ``global_variables.R`` contains declarations for global variables that are used throughout this analysis. Importantly, one of these global variables is the function implementing the likelihood ratio test used in the secondary analyses.
- The file ``compute_censoring_time.R`` computes the censoring time for each individual.
- The file ``get_covariate_data.R`` retrieves and cleans the baseline covariate data. Until the data is unmasked, this file also generates random treatment assignments to each patient and saves this as an RDS file. Once the treatment data is unmasked, we will replace this randomly generated treatments with the actual treatments. The end of the file also currently contains some code that attempts to check for collinearity of the covariates.
- The file ``impute_edss_pdds.R`` imputes EDDS and PDDS values for every time point in the study and saves the imputed data into an .RDS file for future use. The exact imputation method depends on an argument that the user must pass into the script at run-time on the command line. The options are as follows:
    - "mice": Impute all of the missing values via MICE.
    - "A best, B worst": Impute PDDS using MICE but replace imputed values of EDSS by 0.0 if treatment is A and 9.5 if treatment is B.
    - "A worst, B best": Impute PDDS using MICE but replace imputed values of EDSS by 9.5 if treatment is A and 0.0 if treatment is B.
Note that it is necessary to re-run the entire analysis with these different options to see the results of the RMST analysis with the different imputation options.
- The file ``event_time.R`` computes the event time (sustained disability progression) for each individual, if they experienced the event. Missing values for MSFC are imputed using MICE under the MAR assumption. For EDSS, we use the imputed values from ``impute_edss_pdds.R``. To run this file, it needs the outputs from the three previous files (``compute_censoring_time.R``, ``get_covariate_data.R``, and ``impute_edss_pdds.R``). 
- The file ``combine_data.R`` combines the data computed in the above three files into one centralized dataset. In this file, we also simulate random treatment assignments.
- The file ``rmst_analysis.R`` executes the RMST analysis and outputs the RMST for each treatment group as well as the square root of the variance for the difference in means estimate. This file also contains simulations where we try different time windows and evaluate the variance. If running the simulations are not desired, then simply exit the program early.
- The file ``plot_simulation_results.R`` plots simulation results from the variance simulations in the ``rmst_analysis.R`` file.

In the folder ``secondary_analyses``, all of the files for the secondary analysis are included. Each file executes a separate analysis. This folder also includes code for simulating the likelihood ratio tests on a cluster using the SLURM workload manager.

The folder ``pdds_explore`` contains some code plotting histograms for the PDDS score at various time intervals as well as the histograms themselves.

The file ``update_log.md`` contains detailed updates and notes by date for this repository.
