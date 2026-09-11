# import the global variables file
source("global_variables.R")

# read the covid hospitalization data
covid_hosp_data <- data.frame(read_excel(covid_hosp, sheet="DATASET aim2")) %>%
    select(c(patient_id, covidhosp, txgroup))

# get the counts for each entry in the table
treat_A_hosp <- nrow(covid_hosp_data %>% filter(txgroup == 0) %>% filter(covidhosp == 1))
treat_A_no_hosp <- nrow(covid_hosp_data %>% filter(txgroup == 0) %>% filter(covidhosp == 0))
treat_B_hosp <- nrow(covid_hosp_data %>% filter(txgroup == 1) %>% filter(covidhosp == 1))
treat_B_no_hosp <- nrow(covid_hosp_data %>% filter(txgroup == 1) %>% filter(covidhosp == 0))

# construct the table
dat <- data.frame(
    "treat_A" = c(treat_A_no_hosp, treat_A_hosp),
    "treat_B" = c(treat_B_no_hosp, treat_B_hosp),
    row.names = c("no_hosp", "hosp"),
    stringsAsFactors = FALSE
)

print(dat)

# run fisher's test
test <- fisher.test(dat)
print(test)
