library(readxl)
library(stringr)

baseline_data <- data.frame(read_excel("preliminary_longitudinal_data.xlsx", sheet="baseline chars"))

# print number of rows in the data
print(paste("number of rows", nrow(baseline_data)))

# print the number of unique patient ids
print(paste("number of unique patient ids", length(unique(baseline_data$PatientName))))

# get a table for the patient status
print("table of patient status")
print(table(baseline_data$patient_status))

# get EDSS data
edss_data <- data.frame(read_excel("preliminary_longitudinal_data.xlsx", sheet="edss"))

# get the patient IDs and print the number of different patients
patient_ids <- unique(edss_data$PatientName)
print("number of unique patient_ids in EDSS data")
print(length(patient_ids))

# get the site IDs and print the number of different sites
site_ids <- unique(edss_data$SiteName)
print("number of unique site_ids in EDSS data")
print(length(site_ids))

# convert the FormGroup column to numbers
print(unique(edss_data$FormGroup))
FormGroup_num <- as.integer(word(edss_data$FormGroup, 2))
edss_data$FormGroup_num <- FormGroup_num

# record the censoring time for each patient, which is defined as
# the time point at which every value after is missing
censoring_time <- c()
# also record the index at which you become censored
censoring_index <- c()

# iterate through all of the patients
for (patient_id in patient_ids) {
    cur_patient_data <- edss_data[edss_data$PatientName == patient_id, ]
    cur_patient_edss <- cur_patient_data$total_edss_score
    cur_patient_formgroup_num <- cur_patient_data$FormGroup_num

    # get the index at which every recorded value is missing after
    # step 1: get a vector of 0s and 1s representing whether the value is missing
    is_missing <- as.integer(is.na(cur_patient_edss))

    # step 2: declare a variable that stores the value at which the last value
    # is observed
    
    # catch the corner case that there are no observed edss scores
    if (sum(is_missing[1:length(is_missing)]) == length(is_missing)) {
        month_censored <- 0
        index_censored <- -1
    } else {
        month_censored <- 0
        # step 3: iterate through each index
        for (i in 1:length(is_missing)) {
            # if the count of missing values after this index is the same as the
            # number of values left in the vector, then we are censored after this
            if (i == length(is_missing) | sum(is_missing[(i+1):length(is_missing)]) == length(is_missing)-i) {
                month_censored <- cur_patient_formgroup_num[i]
                index_censored <- i
                break
            }
        }    
    }

    # add this censoring time to the vector
    censoring_time <- c(censoring_time, month_censored)
    censoring_index <- c(censoring_index, index_censored)
}

print(paste("number of patients with no observations:", length(which(censoring_index == -1))))

observed_patient_ids <- patient_ids[-which(censoring_index == -1)]
observed_censoring_time <- censoring_time[-which(censoring_index == -1)]
observed_censoring_index <- censoring_index[-which(censoring_index == -1)]

# note that the censoring time and the censoring index don't need to match; in the raw data,
# sometimes months are skipped
# for instance patient 0156-021 skips from month 18 to month 72
# censoring_time_index_mismatch <- c()
# for (i in 1:length(censoring_time)) {
#     if (censoring_time[i] != 6*(censoring_index[i]-1)) {
#         censoring_time_index_mismatch <- c(censoring_time_index_mismatch, patient_ids[i])
#     }
# }

print(paste("average observed censoring time:", mean(observed_censoring_time)))
print(paste("median observed censoring time:", median(observed_censoring_time)))

# now get the number of missing values for the edss score prior to censoring for each patient
num_censored_observations <- 0
for (patient_id in observed_patient_ids) {
    cnt <- 0
    # first count 
}
