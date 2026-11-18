# TREAT-MS Analysis Code

Abbreviations:
- t25fw: Timed 25 foot walk.
- nhpt: Nine-hole peg test.

The code for data pre-processing and implementation of the restricted mean survival time (RMST) analysis is contained in the folder ``primary_analysis``. The data itself is not contained in the repository due to data privacy. A short description of each file in the repository is below. To run the primary analysis, we may execute the files in the order below. Note that it is important to run the files in the order specified below because files further in the list depend on the output of files earlier in the list.

- The file ``global_variables.R`` contains declarations for global variables that are used throughout this analysis. Importantly, one of these global variables is the function implementing the likelihood ratio test used in the secondary analyses.
- The file ``create_conosrt.R`` initializes two blank csv files that act as consorts where we keep track of which patients are dropped at which parts of the analysis and why. One consort is for the analysis where we keep values with an exception finding, and the other consort is for the analysis where we do not keep values with an exception finding.
- The file ``get_covariate_data.R`` retrieves and cleans the baseline covariate data. This file also gets the treatment data and saves it in a separate file for later use. There is still commented out code for generating the treatment variables differently if that is still desired. The end of the file also currently contains some code that attempts to check for collinearity of the covariates.
- The file ``impute_edss.R`` imputes EDDS values for missing 6 month intervals in the study and saves the imputed data into an .RDS file for future use. We will look at the form group and impute values for form groups that are missing in 6 month intervals. The visit date will be imputed as the dates between the observed values, evenly spaced out. The exact imputation method depends on an argument that the user must pass into the script at run-time on the command line. The options are as follows:
    - "none": Do not impute any values and leave them as NA.
    - "A best, B worst": Replace imputed values of EDSS by 0.0 if treatment is A and 9.5 if treatment is B.
    - "A worst, B best": Replace imputed values of EDSS by 9.5 if treatment is A and 0.0 if treatment is B.

    Note that it is necessary to re-run the entire analysis with these different options to see the results of the RMST analysis with the different imputation options.
    The user must also specify whether to include measurements of EDSS with exception findings. The options are as follows:
    - "exclude exceptions": Do not include any values of EDSS with an exception finding.
    - "include exceptions": Include values of EDSS with an exception finding.
- The file ``impute_msfc.R`` imputes MSFC values for every time point in the study and saves the imputed data into .RDS files for future use. Like above, the user can specify how the imputation happens. The imputation options are the same as above. For t25fw best case values are 0, and worst case values are 80. For nhpt best case values are 0, and worst case values are 200.
- The file ``event_time.R`` computes the event time (sustained disability progression) for each individual, if they experienced the event. For EDSS, we use the imputed values from ``impute_edss.R``. For t25fw and nhpt, we use the imputed values from ``impute_msfc.R``. To run this file, it needs the outputs from the three previous files (``get_covariate_data.R``, ``impute_edss.R``, and ``impute_msfc.R``).
- The file ``compute_censoring_time.R`` computes the censoring time for each individual. The censoring time depends on the event time, so compute the event time first.
- The file ``combine_data.R`` combines the data computed in the above three files into one centralized dataset.
- The file ``rmst_analysis.R`` executes the RMST analysis and outputs the RMST for each treatment group, the difference in the RMST for the two treatment groups, and the confidence interval for the difference. The censoring times and event times are rounded to the nearest month (based on assumption that a month is 30 days) to reduce run-time complexity. This file also contains simulations where we try different time windows and evaluate the variance. If running the simulations are not desired, then simply exit the program early. The code in this file uses implementations from https://github.com/idiazst/survtmlerct/tree/master.

- The file ``plot_simulation_results.R`` plots simulation results from the variance simulations in the ``rmst_analysis.R`` file.
- The file ``save_consort.R`` saves the consort data into csv files.
- The file ``analyze_no_event_time_patients.R`` performs some exploratory data analysis on patients who do not have an event time because they have less than 3 measurements for all of EDSS, T25FW, and NHPT.
- The file ``inter_visit_lengths.R`` plots the distribution of the number of days elapsed between visits of EDSS and MSFC.

In the folder ``secondary_analyses``, all of the files for the secondary analysis are included. Each file executes a separate analysis. This folder also includes code for simulating the likelihood ratio tests on a cluster using the SLURM workload manager.

The folder ``pdds_explore`` contains some code plotting histograms for the PDDS score at various time intervals as well as the histograms themselves.

The file ``UPDATE_LOG.md`` contains detailed updates and notes by date for this repository.
