# import the global variables file
source("global_variables.R")

# read the relevant data files
covar_data <- readRDS("covar_data.RDS")
adherence_data <- readRDS("adherence_data.RDS")
postpone_data <- readRDS("postpone_data.RDS")

nqol_data <- data.frame(read_excel(nqol, sheet="Sheet1")) %>%
    select(c(patient_id, ANXTscore, DEPTscore))

data <- covar_data %>%
    inner_join(adherence_data, by="patient_id") %>%
    inner_join(nqol_data, by="patient_id") 

# get the list of features
features <- colnames(data %>% select(-c(patient_id, adherence_change, DEPTscore)))

# create the formula for the first regression 
formula_string <- paste("adherence_change ~", paste0(features, collapse="+"))

model <- glm(as.formula(formula_string), data=data, family=binomial)

print("Med adherence vs. Anxiety subscore")
print(model)
print(confint.default(model))

# get the list of features
features <- colnames(data %>% select(-c(patient_id, adherence_change, ANXTscore)))

# create the formula for the first regression 
formula_string <- paste("adherence_change ~", paste0(features, collapse="+"))

model <- glm(as.formula(formula_string), data=data, family=binomial)

print("Med adherence vs. Depression subscore")
print(model)
print(confint.default(model))

data <- covar_data %>%
    inner_join(postpone_data, by="patient_id") %>%
    inner_join(nqol_data, by="patient_id") 

# get the list of features
features <- colnames(data %>% select(-c(patient_id, yes_postpone, DEPTscore)))

# create the formula for the first regression 
formula_string <- paste("yes_postpone ~", paste0(features, collapse="+"))

model <- glm(as.formula(formula_string), data=data, family=binomial)

print("Postpone/cancel visit vs. Anxiety subscore")
print(model)
print(confint.default(model))

# get the list of features
features <- colnames(data %>% select(-c(patient_id, yes_postpone, ANXTscore)))

# create the formula for the first regression 
formula_string <- paste("yes_postpone ~", paste0(features, collapse="+"))

model <- glm(as.formula(formula_string), data=data, family=binomial)

print("Postpone/cancel visit vs. Depression subscore")
print(model)
print(confint.default(model))


