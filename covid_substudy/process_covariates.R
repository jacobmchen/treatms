# import the global variables file
source("global_variables.R")

# library for one-hot encoding categorical variables
library(fastDummies)

# read the data with breakthrough disease data
data <- data.frame(read_excel(covar_breakthrough_med_postpone, sheet="Sheet1")) %>%
    select(c(patient_id, age_at_consent, gender, race_calculated, ethnicity)) %>%
    # there are two missing values for ethnicity, but they are both not hispanic
    mutate(ethnicity = ifelse(is.na(ethnicity), "Not Hispanic or Latino", ethnicity)) %>%
    # binarize gender
    mutate(gender = ifelse(gender == "Male", 1, 0)) %>%
    # binarize ethnicity
    mutate(ethnicity = ifelse(ethnicity == "Hispanic or Latino", 1, 0)) %>%
    # anything other than white or black race should be considered as other
    mutate(race_calculated = ifelse(race_calculated == "multi" | 
                                    race_calculated == "indian" | race_calculated == "asian", "other", race_calculated)) %>%
    # one-hot encode the variable race
    dummy_cols(select_columns=c("race_calculated"), remove_first_dummy=TRUE) %>%
    # deselect the original race variable
    select(-race_calculated)

data %>% slice_head(n=10) %>% print()

# read the rest of the covariate data
other_covar <- data.frame(read_excel(other_covar, sheet="Sheet1")) %>%
    # calculate the msss total score
    mutate(msss = (take_doctor_BL + understand_problems_BL + prepare_meals_BL + good_time_BL + hug_BL - 5)*5) %>%
    # compute whether a patient was distancing while socializing
    mutate(distancing = ifelse(str_detect(social_distancing_degree_BL, "more than 6 feet"), "socializing_with_distance", "socializing_without_distance")) %>%
    # compute whether a patient was socializing at all
    mutate(no_socializing = ifelse(str_detect(social_behavior_BL, "not socializing") | str_detect(social_behavior_BL, "I am not going to public places or socializing"), 1, 0)) %>%
    # if a patient was not socializing, distancing should be set to "no socializing"
    mutate(distancing = ifelse(no_socializing == 1, "no_socializing", distancing)) %>%
    # if a patient has missing values for distancing, set them up as a separate category
    # called "missing"
    mutate(distancing = ifelse(is.na(distancing), "missing", distancing)) %>%
    # one-hot encode the variable for social distancing
    dummy_cols(select_columns=c("distancing"), remove_first_dummy=TRUE) %>%
    # select only relevant columns
    select(c(patient_id, msss, distancing_no_socializing, distancing_socializing_with_distance, distancing_socializing_without_distance))

# merge the two covariate dataframes together
covar_data <- data %>%
    inner_join(other_covar, by="patient_id")

covar_data %>% slice_head(n=10) %>% print()

# save the data to an RDS file
saveRDS(covar_data, "covar_data.RDS")
