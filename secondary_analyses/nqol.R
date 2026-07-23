# code for evaluating the secondary outcome 
# NQOL, of which there are 11 subscales
# analysis of all 11 subscales will be conducted in
# this file

# read the global variables
source("../primary_analysis/global_variables.R")

# package for real excel files
library(readxl)

# read the baseline covariate data
covariate_data <- readRDS("../primary_analysis/baseline_data_merge_states.RDS")

cols <- c("NQSAT03", "NQSAT23", "NQSAT14", "NQSAT11", "NQSAT33", "NQSAT32", "NQSAT47", "NQSAT46")

data <- data.frame(read_excel(nqol_file_name, sheet="satisf_mismatch")) %>%
    filter(!is.na(NQSAT03)) %>%
    filter(!is.na(NQSAT23)) %>%
    filter(!is.na(NQSAT14)) %>%
    filter(!is.na(NQSAT11)) %>%
    filter(!is.na(NQSAT33)) %>%
    filter(!is.na(NQSAT32)) %>%
    filter(!is.na(NQSAT47)) %>%
    filter(!is.na(NQSAT46))
print(nrow(data))

data <- data %>%
    mutate(all_5_or_all_1 = if_all(all_of(cols), ~ .x == 5) | 
                            if_all(all_of(cols), ~ .x == 1)) %>%
    filter(all_5_or_all_1 == TRUE)
print(nrow(data))

data %>% slice_head(n=10) %>% print()

write.csv(data, "csv_files/all_5_or_all_1_satisf.csv", row.names=FALSE)

q()

# set a seed so that experiments are reproducible
set.seed(0)

# save a list of strings representing the sheet names of
# the nqols we have to analyze
subscales <- c("ANX", "DEP", "FATIGUE", "COG", "POS", "SLEEP", "SOC_ACT",
               "SOC_SATISF", "STIGMA")

for (i in 1:length(subscales)) {
    cur_subscale <- subscales[i]
    print(paste("analyzing", cur_subscale))

    # read the NQOL data for anxiety
    # nqol_file_name is defined in the global_variables.R file
    data <- data.frame(read_excel(nqol_file_name, sheet=cur_subscale)) %>%
        # rename columns so that we can compute which visit month
        # each visit corresponds to
        rename(PatientName=PIN, visit_date=Assmnt)

    # compute the visit month from the visit windows data
    data <- compute_month_interval(data, c("TScore")) %>%
        # rename the closest month to just month
        rename(month=closest_month) %>%
        # delete the column visit_date
        select(-visit_date) %>%
        # remove all rows that have duplicate patient name
        # and month
        distinct(PatientName, month, .keep_all=TRUE) %>%
        # merge covariate data
        group_by(PatientName) %>%
        inner_join(covariate_data, by="PatientName") %>%
        ungroup()

    # add a randomly generated treatment to the data for simulation
    data <- data %>%
        group_by(PatientName) %>%
        mutate(treatment = rbinom(1, size=1, prob=0.5)) %>%
        ungroup()

    print(paste("number of rows after data processing", nrow(data)))
    data %>% slice_head(n=5) %>% print()

    formulas <- create_formulas("treatment", "month", "PatientName", "TScore", data)
    formula_red <- as.formula(formulas[1])
    formula_full <- as.formula(formulas[2])

    # run a single chi-square test
    print(run_chi_square_test(data, formula_red, formula_full, "continuous"))

    # run a bootstrap likelihood ratio test
    print(run_bootstrap_test(data, 2, formula_red, formula_full, "TScore", "continuous"))
}
