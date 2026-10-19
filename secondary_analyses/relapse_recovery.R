# code for determining and evaluating exam-based relapse recovery

# read the global variables
source("../primary_analysis/global_variables.R")

# package for real excel files
library(readxl)

# define a function that fills in the form group when the value is
# interim information
fill_in_formgroup <- function(relapse_data) {
    # get patients with form group "interim information"
    interim_formgroup <- relapse_data %>%
        filter(FormGroup == "Interim Information")

    # read the visit windows data
    visit_windows <- data.frame(read_excel(data_file_name, sheet="visit windows"))

    # process visit windows data
    visit_windows <- visit_windows %>%
        # rename the column name for patient names so that it matches
        # with the relapse data
        rename(PatientName = Patient) %>%
        # get only the patients who have "Interim Information" as their
        # form group data
        filter(PatientName %in% interim_formgroup$PatientName) %>%
        # renomve the columns Site and Status
        select(-c(Site, Status)) %>%
        # subtract one day to each close date
        # the across() function takes two arguments: a list of columns and
        # a function to apply to each column; we pass in every column that
        # starts with "close" and apply the function that subtracts 1
        # to each column. The syntax ~ .x - 1 is a purrr-style formula.
        mutate(across(starts_with("close"), ~ .x - 1))

    # get the names of the columns of the visit windows sheet after processing
    window_names <- names(visit_windows %>% select(-PatientName))

    # process the interim_formgroup data to get the closest date to the visit date
    # where the exam-confirmed relapse was detected
    interim_formgroup <- merge(interim_formgroup, visit_windows, by="PatientName") %>%
        # make subsequent changes occur row-wise across the whole dataframe
        rowwise() %>%
        # create a new column called closest date as follows
        # here, we use an in-line function that returns the final object in the code block
        mutate(closest_date = {
                # get the value of the visit_date
                v <- visit_date
                # get the values of the dates across all the time 
                # windows
                xs <- c_across(all_of(window_names))
                # return the name of the column that is closest in date
                # to visit_date by identifying the index where visit_date
                # and the value of the column are closest
                window_names[ which.min(abs(as.numeric(xs - v))) ]
            }
        ) %>%
        ungroup() %>%
        # change the string open or close to "Month " to match with
        # the values of the formgroup
        mutate(closest_date = sub("open", "Month ", closest_date)) %>%
        mutate(closest_date = sub("close", "Month ", closest_date)) %>%
        # keep only the patient name and the closest 6 month interval
        select(c(PatientName, closest_date)) %>%
        # rename the column to closest_formgroup
        rename(closest_formgroup = closest_date)

    # merge the relapse data with the interim formgroup data where the closest 6 month
    # interval was found
    relapse_data <- merge(relapse_data, interim_formgroup, by="PatientName", all.x=TRUE) %>%
        # if a patient has interim information as its form group, change it to the
        # formgroup that we found
        mutate(FormGroup = ifelse(FormGroup == "Interim Information", closest_formgroup, FormGroup)) %>%
        # remove the closest formgroup column since no patient should have an interim formgroup now
        select(-closest_formgroup)

    return(relapse_data)
}

# define a function that finds the patients with multiple relapses
# and saves them into a csv file
find_multiple_relapse_patients <- function() {
    # get patients that experience multiple relapses
    relapse_data <- relapse_data_orig %>%
        # only consider patients with exam-confirmed relapse recovery
        filter(ra_est_event == "Exam-confirmed exacerbation") %>%
        group_by(PatientName) %>%
        # for now, only use patients that have one relapse to evaluate
        # relapse recovery
        filter(n() > 1)

    print("multiple relapses")
    print(head(relapse_data))
    write.csv(relapse_data, "multiple_relapse_patients.csv", row.names=FALSE)
}

# read the relapse data
relapse_data_orig <- data.frame(read_excel(data_file_name, sheet="relapse"))

# process the relapse data to only patients that experience relapse once
# and have at least one recorded symptom
relapse_data <- relapse_data_orig %>%
    # only consider patients with exam-confirmed relapse recovery
    filter(ra_est_event == "Exam-confirmed exacerbation") %>%
    group_by(PatientName) %>%
    # for now, only use patients that have one relapse to evaluate
    # relapse recovery
    filter(n() == 1) %>%
    # for each patient, create a vector with a string indicating
    # the exam-confirmed relapse symptoms that they are experiencing
    mutate(symptoms = list(as.vector(na.omit(c(ra_deficit_bowel_bladder,
                        ra_deficit_brainstem,
                        ra_deficit_cerebellar,
                        ra_deficit_mental,
                        ra_deficit_pyramidal,
                        ra_deficit_sensory,
                        ra_deficit_visual))))) %>%
    # count the number of symptoms that each patient experience
    mutate(num_symptoms = length(symptoms[[1]])) %>%
    # for now, remove patients that don't have any recorded symptoms
    filter(num_symptoms > 0) %>%
    select(c(PatientName, FormGroup, visit_date, symptoms, num_symptoms))

# replace patients with the formgroup "interim information" with the 6 month
# window corresponding to when they had their relapse
relapse_data <- fill_in_formgroup(relapse_data)

print(head(relapse_data))

# read the imputed EDSS and PDDS data
imputed_data <- readRDS("../primary_analysis/imputed_edss_pdds_data.RDS")

print(head(imputed_data))

# filter the EDSS and PDDS data to only patients that experience relapse only once
# and have at least one symptom recorded
imputed_data <- imputed_data %>%
    filter(PatientName %in% relapse_data$PatientName) %>%
    # rename columns so that symptoms match the name of the functional
    # system scores
    rename(Mental = fs_cfss_total,
           Visual = fsvs_on_total,
           Brainstem = fs_bfss_total,
           Pyramidal = total_pyramidal_score,
           Sensory = sensory_system_score_total,
           Cerebellar = cerebellar_system_score_total,
           Bowel_or_bladder = bowel_bladder_sys_score_total,
           FormGroup = month)

# define a function that takes as input a PatientName, a FormGroup month
# representing when the relapse occurred, and a list of relevant symptoms
# and returns whether the patient experienced full recovery or not
determine_relapse_recovery <- function(patient, month, symptoms) {
    # filter edss and pdds data so that it only has the relevant patients;
    # the imputed_data dataframe is a global variable
    patient_data <- imputed_data %>%
        filter(PatientName == patient) 

    # replace all spaces with underscores in the symptom names
    # to match with column names in the edss data
    symptoms <- gsub(" ", "_", symptoms)

    # change the string of the month of relapse to a numeric number
    month <- as.numeric(gsub("\\D", "", month))

    # keep only the columns that are relevant to the symptoms that
    # worsened during relapse
    patient_data <- patient_data %>%
        select(c(PatientName, FormGroup), all_of(symptoms)) %>%
        ungroup()

    # if there are less than 3 observed values of the relevant symptoms, then it
    # is not possible to compute relapse recovery
    if (nrow(patient_data) < 3) {
        return(NA)
    }

    # keep a count of how many FSS scores return to baseline
    complete_count <- 0

    # iterate through each symptom associated with the exam-confirmed
    # relapse
    for (symptom in symptoms) {
        # get the data column pertaining to only this column
        patient_data_symptom <- patient_data %>% select(c(FormGroup, all_of(symptom)))

        # get the most recently observed pre-relapse data
        pre_relapse <- patient_data_symptom %>%
            # keep only observed values at least 6 months before the relapse
            filter(!is.na(.data[[symptom]]), FormGroup <= month-6) %>%
            # keep only the row with the greatest month value
            slice_max(FormGroup, n=1)

        if (nrow(pre_relapse) < 1) {
            # there is no recorded pre-relapse value for this symptom,
            # so we cannot compute relapse recovery
            return(NA)
        }

        # save the computed pre-relapse value
        pre_relapse_val <- pre_relapse[[symptom]][1]

        # get the relapse value, if it exists
        relapse <- patient_data_symptom %>%
            filter(!is.na(.data[[symptom]]), FormGroup == month)

        if (nrow(pre_relapse) < 1) {
            # there is no recorded relapse value for this symptom,
            # so we cannot compute relapse recovery
            return(NA)
        }

        # save the computed relapse value
        relapse_val <- relapse[[symptom]][1]

        # get the most recently observed post-relapse data
        post_relapse <- patient_data_symptom %>%
            # keep only observed values at least 6 months after the relapse
            filter(!is.na(.data[[symptom]]), FormGroup >= month+6) %>%
            # keep only the row with the smallest month value
            slice_min(FormGroup, n=1)

        if (nrow(post_relapse) < 1) {
            # there is no recorded post-relapse value for this symptom,
            # so we cannot compute relapse recovery
            return(NA)
        }

        # save the computed post-relapse value
        post_relapse_val <- post_relapse[[symptom]][1]

        # see if this symptom experienced recovery
        if (post_relapse_val <= pre_relapse_val) {
            complete_count <- complete_count + 1
        }
    }

    # if all symptoms recovered, then the patient recovered
    if (complete_count == length(symptoms)) {
        return(1)
    } else {
        return(0)
    }
}

# compute whether patients had recovery or not
relapse_data <- relapse_data %>%
    rowwise() %>%
    # use functional system scores related to symptoms to compute exam-based
    # recovery
    mutate(recovery = determine_relapse_recovery(PatientName, FormGroup, symptoms)) %>%
    # use PDDS to compute patient-determined recovery
    mutate(pdds_recovery = determine_relapse_recovery(PatientName, FormGroup, c("pdds_total_score")))

print(head(relapse_data))

print("exam based")
print(nrow(relapse_data %>% filter(recovery == 1)))
print(nrow(relapse_data %>% filter(recovery == 0)))
print(sum(is.na(relapse_data$recovery)))

print("patient based")
print(nrow(relapse_data %>% filter(pdds_recovery == 1)))
print(nrow(relapse_data %>% filter(pdds_recovery == 0)))
print(sum(is.na(relapse_data$pdds_recovery)))
