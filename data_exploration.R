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

print(paste("average observed censoring time:", mean(observed_censoring_time)))
print(paste("median observed censoring time:", median(observed_censoring_time)))

# now get the number of missing values for the edss score prior to censoring for each patient
num_missing_observations <- c()
percent_missing_observations <- c()

# we also want to get how many times missing values are surrounded by observed values
# and how many consecutive missingness; we can keep track of this by making a list
# of vectors each representing the "span" of missingness for a particular patient
missingness_spans <- list()

# create a vector for storing patient IDs where there is a missingness span of more than 3
missingness_span_morethan3 <- c()

# create a vector for patient IDs where the first value is missing
first_value_missing <- c()

# keep track of an iterating counter
i <- 1
for (patient_id in observed_patient_ids) {
    missing_cnt <- 0

    cur_patient_data <- edss_data[edss_data$PatientName == patient_id, ]
    cur_patient_edss <- cur_patient_data$total_edss_score
    cur_patient_formgroup_num <- cur_patient_data$FormGroup_num

    # get the edss scores and months up to the censored index
    cur_patient_edss_truncated <- cur_patient_edss[1:observed_censoring_index[i]]
    cur_patient_month_truncated <- cur_patient_formgroup_num[1:observed_censoring_index[i]]

    # create a named list that behaves like a dictionary where the key is the month and the
    # value is the edss value
    month_edss_dict <- list()
    for (j in 1:observed_censoring_index[i]) {
        month_edss_dict[[toString(cur_patient_month_truncated[j])]] <- cur_patient_edss_truncated[j]
    }

    # first fill in any gaps in the months by creating a sequence that inclues every
    # 6 month interval
    last_observed_month <- cur_patient_month_truncated[length(cur_patient_month_truncated)]
    patient_months <- seq(0, last_observed_month, by=6)
    # use the named list to copy over the edss values at each 6 month interval; if a value
    # is not in the named list, then insert a missing value
    patient_edss <- c()
    for (j in 1:length(patient_months)) {
        if (is.null(month_edss_dict[[toString(patient_months[j])]])) {
            patient_edss <- c(patient_edss, NA)
        } else {
            patient_edss <- c(patient_edss, month_edss_dict[[toString(patient_months[j])]])
        }
    }

    # get the spans of missingness, which is a vector of missingness lengths
    cur_missingness_span <- c()

    j <- 1
    while (j <= length(patient_edss)) {
        if (is.na(patient_edss[j])) {
            cnt <- 1
            # walk forward until we reach something that is observed
            for (k in (j+1):length(patient_edss)) {
                if (is.na(patient_edss[k])) {
                    cnt <- cnt + 1
                } else {
                    j <- k
                    break
                }
            }
            cur_missingness_span <- c(cur_missingness_span, cnt)
        } else {
            j <- j + 1
        }
    }

    # see how many people have first value missing
    if (is.na(patient_edss[1])) {
        first_value_missing <- c(first_value_missing, patient_id)
    }

    # see if the current missingness span contains a span greater than 3
    if (!is.null(cur_missingness_span) ) {
        if (max(cur_missingness_span) > 3) {
            missingness_span_morethan3 <- c(missingness_span_morethan3, patient_id)
        }
    }

    # save the missingness spans in a list
    missingness_spans[[i]] <- cur_missingness_span

    # the number of missing observations is the number of missing values in patient_edss
    num_missing_observations <- c(num_missing_observations, sum(is.na(patient_edss)))
    # the percent of missing observations is the number of missing values divided by the total length
    percent_missing_observations <- c(percent_missing_observations, sum(is.na(patient_edss))/length(patient_edss))

    i <- i + 1
}
print(paste("average number of missing rows for each patient:", mean(num_missing_observations)))
print(paste("average percent of of missing rows for each patient:", mean(percent_missing_observations)))
print(paste("number of patients with more than 3 consecutive missing values", length(missingness_span_morethan3)))
print(paste("number of patients with first value missing", length(first_value_missing)))
