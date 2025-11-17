library(readxl)
library(stringr)

# read the data
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

# get MSFC data for timed walk and nine hole peg test data
msfc_data <- data.frame(read_excel("preliminary_longitudinal_data.xlsx", sheet="msfc"))

# get the patient IDs and print the number of different patients
# note that we have also verified that the patient IDs in the EDSS data and the
# MSFC data are exactly the same
patient_ids <- unique(edss_data$PatientName)
print("number of unique patient_ids in EDSS data")
print(length(patient_ids))

# get the site IDs and print the number of different sites
site_ids <- unique(edss_data$SiteName)
print("number of unique site_ids in EDSS data")
print(length(site_ids))

# convert the FormGroup column to numbers
# the FormGroup column tells us the month at which the visit was conducted (0, 6, 12, etc.)
FormGroup_num <- as.integer(word(edss_data$FormGroup, 2))
edss_data$FormGroup_num <- FormGroup_num
# do the same for MSFC data
FormGroup_num <- as.integer(word(msfc_data$FormGroup, 2))
msfc_data$FormGroup_num <- FormGroup_num

# create a function that takes in a missingness vector and formgroup vector and
# returns a two value vector containing the month and index at which there is censoring
compute_censoring_time <- function(is_missing, cur_patient_formgroup_num) {
    # catch the corner case that there are no observed edss scores,
    # by counting the number of missing values in the vector is_missing and
    # comparing it to the length of the vector
    if (sum(is_missing) == length(is_missing)) {
        # assign default values of -1
        month_censored <- -1
        index_censored <- -1
    } else {
        month_censored <- 0
        # iterate through each index
        for (i in 1:length(is_missing)) {
            # if the count of missing values after this index is the same as the
            # number of values left in the vector, then we are censored after this
            # also remember the corner case where we're just at the last value of the vector
            if (i == length(is_missing) | sum(is_missing[(i+1):length(is_missing)]) == length(is_missing)-i) {
                month_censored <- cur_patient_formgroup_num[i]
                index_censored <- i
                break
            }
        }    
    }
    
    # create a vector containing the month and index of censoring
    result <- c(month_censored, index_censored)

    return(result)
}

# record the censoring time for each patient, which is defined as
# the time point at which every value after is missing
censoring_time <- c()
# also record the index at which you become censored
censoring_index <- c()

# record the censoring time and index for t25fw and nhpt
censoring_time_t25fw <- c()
censoring_index_t25fw <- c()
censoring_time_nhpt <- c()
censoring_index_nhpt <- c()

# iterate through all of the patients
for (patient_id in patient_ids) {
    # first get all of the relevant information for this patient

    # get EDSS data for this patient
    cur_patient_data <- edss_data[edss_data$PatientName == patient_id, ]
    cur_patient_edss <- cur_patient_data$total_edss_score
    cur_patient_formgroup_num <- cur_patient_data$FormGroup_num
    # get the index at which every recorded value is missing after, i.e. the 
    # censoring time
    # step 1: get a vector of 0s and 1s representing whether the value is missing
    is_missing <- as.integer(is.na(cur_patient_edss))

    # repeat the process for MSFC data
    # get MSFC data for this patient
    # note that sometimes observation times for t25fw can be different from
    # nhpt observation
    cur_patient_msfc_data <- msfc_data[msfc_data$PatientName == patient_id, ]
    cur_patient_msfc_formgroup_num <- cur_patient_msfc_data$FormGroup_num
    cur_patient_t25fw_status <- cur_patient_msfc_data$trial_average_seconds
    cur_patient_nhpt_status <- cur_patient_msfc_data$dominant_hand_average_seconds
    t25fw_is_missing <- as.integer(is.na(cur_patient_t25fw_status))
    nhpt_is_missing <- as.integer(is.na(cur_patient_nhpt_status))
    
    # compute censoring time for EDSS
    result <- compute_censoring_time(is_missing, cur_patient_formgroup_num)

    # add this censoring time to the vector
    censoring_time <- c(censoring_time, result[1])
    censoring_index <- c(censoring_index, result[2])       

    # compute censoring time for t25fw
    result <- compute_censoring_time(t25fw_is_missing, cur_patient_msfc_formgroup_num)

    # add this censoring time to the vector
    censoring_time_t25fw <- c(censoring_time_t25fw, result[1])
    censoring_index_t25fw <- c(censoring_index_t25fw, result[2])       

    # compute censoring time for t25fw
    result <- compute_censoring_time(nhpt_is_missing, cur_patient_msfc_formgroup_num)

    # add this censoring time to the vector
    censoring_time_nhpt <- c(censoring_time_nhpt, result[1])
    censoring_index_nhpt <- c(censoring_index_nhpt, result[2])       
}

print(paste("number of patients with no EDSS observations:", length(which(censoring_index == -1))))
print(paste("number of patients with no t25fw observations:", length(which(censoring_index_t25fw == -1))))
print(paste("number of patients with no nhpt observations:", length(which(censoring_index_nhpt == -1))))

# keep track of patients with observed values of EDSS
observed_patient_ids <- patient_ids[-which(censoring_index == -1)]
observed_censoring_time <- censoring_time[-which(censoring_index == -1)]
observed_censoring_index <- censoring_index[-which(censoring_index == -1)]

# keep track of patients with observed values of t25fw
observed_patient_ids_t25fw <- patient_ids[-which(censoring_index_t25fw == -1)]
observed_censoring_time_t25fw <- censoring_time_t25fw[-which(censoring_index_t25fw == -1)]
observed_censoring_index_t25fw <- censoring_index_t25fw[-which(censoring_index_t25fw == -1)]

# keep track of patients with observed values of nhpt
observed_patient_ids_nhpt <- patient_ids[-which(censoring_index_nhpt == -1)]
observed_censoring_time_nhpt <- censoring_time_nhpt[-which(censoring_index_nhpt == -1)]
observed_censoring_index_nhpt <- censoring_index_nhpt[-which(censoring_index_nhpt == -1)]

print(paste("average observed censoring time EDSS:", mean(observed_censoring_time)))
print(paste("median observed censoring time EDSS:", median(observed_censoring_time)))

print(paste("average observed censoring time t25fw:", mean(observed_censoring_time_t25fw)))
print(paste("median observed censoring time t25fw:", median(observed_censoring_time_t25fw)))

print(paste("average observed censoring time nhpt:", mean(observed_censoring_time_nhpt)))
print(paste("median observed censoring time nhpt:", median(observed_censoring_time_nhpt)))
print("")

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

# keep track of baseline EDSS, which is the EDSS score at month 0;
# note that 17 patients don't have a baseline EDSS, in which case
# their baseline EDSS is a missing value
baseline_edss <- c()

# keep track of whether a patient has disease progression, which is denoted as
# (a) increase of >= 1.0 if baseline EDSS is <= 5.5 or (b) increase of >= 0.5
# if baseline EDSS is >= 6.0; this will be a vector of binary variables
disease_progression <- c()

# keep track of whether a patient has disease progression with respect to
# timed 25 foot walk or nine hole peg test. disease progression is defined
# as an increase of 20% or more between 6 months
disease_progression_t25fw <- c()
disease_progression_nhpt <- c()

# define a function that returns a vector of observed values with every interim month filled in
create_full_vector <- function(censoring_index, cur_patient_value_truncated, 
                              cur_patient_month_truncated) {
    # create a named list that behaves like a dictionary where the key is the month and the
    # value is the edss value
    month_value_dict <- list()
    for (j in 1:censoring_index) {
        month_value_dict[[toString(cur_patient_month_truncated[j])]] <- cur_patient_value_truncated[j]
    }

    # first fill in any gaps in the months by creating a sequence that inclues every
    # 6 month interval
    last_observed_month <- cur_patient_month_truncated[length(cur_patient_month_truncated)]
    patient_months <- seq(0, last_observed_month, by=6)
    # use the named list to copy over the edss values at each 6 month interval; if a value
    # is not in the named list, then insert a missing value
    patient_values <- c()
    for (j in 1:length(patient_months)) {
        if (is.null(month_value_dict[[toString(patient_months[j])]])) {
            patient_values <- c(patient_values, NA)
        } else {
            patient_values <- c(patient_values, month_value_dict[[toString(patient_months[j])]])
        }
    }

    # return the full vector
    return(patient_values)
}

# get the spans of missingness, which is a vector of missingness lengths
# ex. 1 the vector NA NA 1 1 NA 1 1 NA NA NA 1 will return 2, 1, 3
compute_missingness_span <- function(patient_values) {
    # declare an empty vector to save output
    cur_missingness_span <- c()

    # declare an iterator that will walk through patient_values
    j <- 1
    while (j <= length(patient_values)) {
        # check if current value is missing
        if (is.na(patient_values[j])) {
            cnt <- 1
            # walk forward until we reach something that is observed
            for (k in (j+1):length(patient_values)) {
                # if the next value is also missing, increment cnt
                if (is.na(patient_edss[k])) {
                    cnt <- cnt + 1
                } else {
                    # otherwise break out of the loop and update iterator
                    # to where we walked forward to
                    j <- k
                    break
                }
            }
            # add the count of missing values to the missingness span
            cur_missingness_span <- c(cur_missingness_span, cnt)
        } else {
            # if the current value is missing, just walk to the next index
            # for the iterator
            j <- j + 1
        }
    }

    return(cur_missingness_span)
}

# keep track of an iterating counter
i <- 1
# note here that we are only considering patients with observed data for EDSS
for (patient_id in observed_patient_ids) {
    missing_cnt <- 0

    cur_patient_data <- edss_data[edss_data$PatientName == patient_id, ]
    cur_patient_edss <- cur_patient_data$total_edss_score
    cur_patient_formgroup_num <- cur_patient_data$FormGroup_num

    # get the edss scores and months up to the censored index
    cur_patient_edss_truncated <- cur_patient_edss[1:observed_censoring_index[i]]
    cur_patient_month_truncated <- cur_patient_formgroup_num[1:observed_censoring_index[i]]

    patient_edss <- create_full_vector(observed_censoring_index[i], cur_patient_edss_truncated,
                                            cur_patient_month_truncated)

    last_observed_month <- cur_patient_month_truncated[length(cur_patient_month_truncated)]
    patient_months <- seq(0, last_observed_month, by=6)

    cur_missingness_span <- compute_missingness_span(patient_edss)

    # compute disease progression
    # first check if they have a baseline value
    if (is.na(patient_edss[1])) {
        first_value_missing <- c(first_value_missing, patient_id)
        baseline_edss <- c(baseline_edss, NA)
        # if there is no baseline EDSS, then we don't know if their
        # disease progressed (can change later if needed)
        disease_progression <- c(disease_progression, NA)
    } else {
        baseline_edss <- c(baseline_edss, patient_edss[1])
        # evaluate if there was disease progression
        if (patient_edss[1] <= 5.5) threshold <- 1
        else threshold <- 0.5

        # get only the observed values of EDSS
        # note: this implementation may not be exactly correct since I'm
        # only looking at observed values of EDSS
        patient_edss_observed <- patient_edss[!is.na(patient_edss)]
        patient_months_observed <- patient_months[!is.na(patient_edss)]

        # Inf means EDSS did not progress, otherwise return the index at which they experienced
        # progression
        progress <- Inf
        # first check that there is at least two observed values, since
        # we need to check of sustained progression
        if (length(patient_edss_observed) > 2) {
            # iterate through all of the observed values
            for (j in 1:(length(patient_edss_observed)-2)) {
                # check if the change is more than the threshold
                # and if the change is sustained
                if (patient_edss_observed[j+1] - patient_edss_observed[j] >= threshold & patient_edss_observed[j+2] >= patient_edss_observed[j+1]) {
                    progress <- patient_months_observed[j+2]
                    break
                }
            }
        } else {
            # if there is only one observed value, we don't know if their disease
            # progressed
            progress <- NA
        }

        disease_progression <- c(disease_progression, progress)
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
print(paste("average percent of missing rows for each patient:", mean(percent_missing_observations)))
print(paste("number of patients with more than 3 consecutive missing values", length(missingness_span_morethan3)))
# missing EDSS values are not bookended by observed values if and only if there is no
# observed first value since we are only considering up until the censoring timepoint
print(paste("number of patients with first value missing", length(first_value_missing)))
print(paste("mean of baseline EDSS, ignoring missing", mean(baseline_edss, na.rm=TRUE)))
print(paste("median of baseline EDSS, ignoring missing", median(baseline_edss, na.rm=TRUE)))
# a value is missing if we didn't have baseline or if there is only one observed value
print(paste("number of missing values for EDSS progression", sum(is.na(disease_progression))))
disease_progression_no_inf <- ifelse(is.infinite(disease_progression), NA, disease_progression)
print(paste("mean of EDSS disease progression, ignoring missing, ignoring no progression", mean(disease_progression_no_inf, na.rm=TRUE)))
print(paste("number of patients with disease progression", sum(!is.na(disease_progression_no_inf))))
print(paste("length of disease_progression", length(disease_progression)))
print("")

# create a dataframe for EDSS progression
EDSS_progression_df <- data.frame(patient_id=observed_patient_ids,
                                    EDSS_progression=disease_progression)

# define a function that returns Inf, month of progression, or NA for whether a patient had disease progression
# for an MSFC score
msfc_progression <- function(patient_values, patient_months) {
    # get only the observed values of patient values
    patient_values_observed <- patient_values[!is.na(patient_values)]
    patient_months_observed <- patient_months[!is.na(patient_months)]

    # check if there was progression for this MSFC metric
    # first check if there are at least two values that are observed
    # since we need to check whether progression was sustained
    if (length(patient_values_observed) > 2) {
        for (j in 1:(length(patient_values_observed)-2)) {
            # check if there is an at least 20% increase in value and 
            # check whether that increase was sustained at the next observed value
            if (patient_values_observed[j+1] >= 1.2*patient_values_observed[j] &
                patient_values_observed[j+2] >= patient_values_observed[j+1]) {
                return(patient_months[j+2])
            }
        }
    } else {
        # if there were 2 or less observed values, then we cannot check sustained progression
        return(NA)
    }

    # there was no disease progression
    return(0)
}

# keep track of an iterating counter
i <- 1
# note here that we are only considering patients with observed data
# now do the same for t25fw
for (patient_id in observed_patient_ids_t25fw) {
    missing_cnt <- 0

    cur_patient_msfc_data <- msfc_data[msfc_data$PatientName == patient_id, ]
    cur_patient_t25fw <- cur_patient_msfc_data$trial_average_seconds
    cur_patient_msfc_formgroup_num <- cur_patient_msfc_data$FormGroup_num

    # get the t25fw and nhpt up to the censored index
    cur_patient_t25fw_truncated <- cur_patient_t25fw[1:observed_censoring_index_t25fw[i]]
    cur_patient_t25fw_month_truncated <- cur_patient_msfc_formgroup_num[1:observed_censoring_index_t25fw[i]]

    patient_t25fw <- create_full_vector(observed_censoring_index_t25fw[i], cur_patient_t25fw_truncated,
                                            cur_patient_t25fw_month_truncated)

    last_observed_month <- cur_patient_t25fw_month_truncated[length(cur_patient_t25fw_month_truncated)]
    patient_months <- seq(0, last_observed_month, by=6)
    disease_progression_t25fw <- c(disease_progression_t25fw, msfc_progression(patient_t25fw, patient_months))

    i <- i + 1
}
print(paste("number of missing values in disease progression of t25fw", sum(is.na(disease_progression_t25fw))))
print(paste("mean of disease progression of t25fw", mean(disease_progression_t25fw, na.rm=TRUE)))
print("")

# create a dataframe for t25fw progression
t25fw_progression_df <- data.frame(patient_id=observed_patient_ids_t25fw,
                                    EDSS_progression=disease_progression_t25fw)

# keep track of an iterating counter
i <- 1
# note here that we are only considering patients with observed data
for (patient_id in observed_patient_ids_nhpt) {
    missing_cnt <- 0

    cur_patient_msfc_data <- msfc_data[msfc_data$PatientName == patient_id, ]
    cur_patient_nhpt <- cur_patient_msfc_data$dominant_hand_t1_seconds
    cur_patient_msfc_formgroup_num <- cur_patient_msfc_data$FormGroup_num

    # get the t25fw and nhpt up to the censored index
    cur_patient_nhpt_truncated <- as.double(cur_patient_nhpt[1:observed_censoring_index_nhpt[i]])
    cur_patient_nhpt_month_truncated <- cur_patient_msfc_formgroup_num[1:observed_censoring_index_nhpt[i]]

    patient_nhpt <- create_full_vector(observed_censoring_index_nhpt[i], cur_patient_nhpt_truncated,
                                            cur_patient_nhpt_month_truncated)

    # see how many people have first value missing
    if (is.na(patient_nhpt[1])) {
        # if there is no baseline EDSS, then we don't know if their
        # disease progressed (can change later if needed)
        disease_progression_nhpt <- c(disease_progression_nhpt, NA)
    } else {
        # get only the observed values of nhpt
        # note: this implementation may not be exactly correct since I'm
        # only looking at observed values of EDSS
        patient_nhpt_observed <- patient_nhpt[!is.na(patient_nhpt)]
        # 0 means nhpt did not progress, 1 means EDSS did progress
        progress <- 0
        # check if nhpt progressed
        if (length(patient_nhpt_observed) > 1) {
            for (j in 1:(length(patient_nhpt_observed)-1)) {
                if (patient_nhpt_observed[j+1] - patient_nhpt_observed[j] >= 0.2*patient_nhpt_observed[j]) {
                    progress <- 1
                    break
                }
            }
        } else {
            progress <- NA
        }

        disease_progression_nhpt <- c(disease_progression_nhpt, progress)
    }

    i <- i + 1
}
print(paste("number of missing values in disease progression of nhpt", sum(is.na(disease_progression_nhpt))))
print(paste("mean of disease progression of nhpt", mean(disease_progression_nhpt, na.rm=TRUE)))
