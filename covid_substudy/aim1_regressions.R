# import the global variables file
source("global_variables.R")

# read the relevant data files
covar_data <- readRDS("covar_data.RDS")
adherence_data <- readRDS("adherence_data.RDS")
breakthrough_data <- readRDS("breakthrough_data.RDS")
new_worse_data <- readRDS("new_worse_data.RDS")
postpone_data <- readRDS("postpone_data.RDS")

data <- covar_data %>%
    inner_join(adherence_data, by="patient_id") %>%
    inner_join(breakthrough_data, by="patient_id") 

# get the list of features
features <- colnames(data %>% select(-c(patient_id, breakthrough_disease)))

# create the formula for the first regression 
formula_string <- paste("breakthrough_disease ~", paste0(features, collapse="+"))

model <- glm(as.formula(formula_string), data=data, family=binomial)

print("Breakthrough disease vs. Med adherence")
print(model)
print(confint.default(model))

data <- covar_data %>%
    inner_join(new_worse_data, by="patient_id") %>%
    inner_join(postpone_data, by="patient_id") 

features <- colnames(data %>% select(-c(patient_id, new_or_worsen)))

formula_string <- paste("new_or_worsen ~", paste0(features, collapse="+"))

model <- glm(as.formula(formula_string), data=data, family=binomial)

print("New/worsen symptoms vs. Postponed/cancelled visits")
print(model)
print(confint.default(model))
