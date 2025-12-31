# test the estimator code using the example given

source("estimator_functions.R")

# colon is the name of a dataset included in the survival package.
# The survival package includes functions for basic survival analysis such
# as Kaplan-Meier.
library(survival)
library(tidyverse)
library(fastDummies)
# library(dummies)

# The dataset contains 16 columns:
# id: The patient ID
# study: 1 for all patients
# rx: Treatment assignment - 'Obs' indicates no treatment, 'Lev' indicates
# Levamisole, 'Lev+5FU' indicates Levamisole and 5-Fluorouracil
# sex: Patient gender
# age: Patient age in years
# obstruct: Binary indicator of obstruction of colon by cancer
# perfor: Binary indicator of perforation of colon
# adhere: Binary indicator of adherence to nearby organs
# nodes: Number of lymph nodes with detectable cancer
# time: Days until event or censoring
# status: Binary indicator of censoring status; 0 means censored and 1 means observed death
# differ: Differentiation of tumor; measure of severity from 1 through 3
# extent: Extent of local spread; measure of severity from 1 through 4
# surg: Time from surgery to registration; measure of severity from 0 through 1
# node4: Binary indicator of more than 4 positive lymph nodes
# etype: Event type of 1=recurrence or 2=death

# each patient has two rows in the dataset, one to record time of recurrence and another
# to record time of death
print(head(colon))

# check whether there are any patients for whom death occurs before recurrence (there should not be any)
# data <- colon %>% 
#     group_by(id) %>% summarise(G = ifelse(time[etype==2] < time[etype==1], 1, 0)) %>% filter(G == 1)
# print(data)

# the pipe operator %>% "puts" the object on the left into the function on the right
# consecutive applications of the operator puts the result from the first application
# into subsequent ones
data <- colon %>% dummy_cols(select_columns=c("differ", "extent")) %>% # one-hot encode the columns differ and extent
  filter(rx != "Obs") %>% # remove all patients with treatment assigned as "Obs", so we are comparing
  # between the two treatment arms
  mutate(A = rx == "Lev+5FU", id = as.numeric(as.factor(id)),
         nanodes = is.na(nodes), nodes = ifelse(is.na(nodes), 0, nodes)) %>% 
  # create additional columns: A is TRUE if the treatment is "Lev+5FU" and FALSE otherwise
  #                            id is the numeric version of id which was a string before
  #                            nanodes is TRUE if nodes is missing and FALSE otherwise
  #                            nodes is 0 if it is missing and its original value otherwise
  select(-rx) %>% # remove the column rx
  group_by(id) %>% # merge the rows with the same id
  summarise_all(min) %>% # and summarize each column using the minimum value,
                         # the minimum value tells us whether
  select(-study) %>% # remove the column study since it is all 1s
  rename(T = time, D = status) # rename the column time as T and censoring status as D (D is the
                               # censoring indicator, representing Delta)

# the second parameter divides the time by 30, converting the unit from number of days to 
# number of months
dlong <- transformData(data, 30)

# fit initial estimators
fitL <- glm(Lm ~ A * (m + sex + age + obstruct + perfor + adhere + nodes + D +
                        differ1 + differ2 + differ3 + differNA + extent1 + extent2 +
                        extent3 + extent4 + surg + node4 + etype),
            data = dlong, subset = Im == 1, family = binomial())
fitR <- glm(Rm ~ A * (m + sex + age + obstruct + perfor + adhere + nodes + D +
                        differ1 + differ2 + differ3 + differNA + extent1 + extent2 +
                        extent3 + extent4 + surg + node4 + etype),
            data = dlong, subset = Jm == 1, family = binomial())
fitA <- glm(A ~ sex + age + obstruct + perfor + adhere + nodes + D +
              differ1 + differ2 + differ3 + differNA + extent1 + extent2 +
              extent3 + extent4 + surg + node4 + etype,
            data = dlong, subset = m == 1, family = binomial())

# dlong <- mutate(dlong,
#                 gR1 = bound01(predict(fitR, newdata = mutate(dlong, A = 1), type = 'response')),
#                 gR0 = bound01(predict(fitR, newdata = mutate(dlong, A = 0), type = 'response')),
#                 h1 = bound01(predict(fitL, newdata = mutate(dlong, A = 1), type = 'response')),
#                 h0 = bound01(predict(fitL, newdata = mutate(dlong, A = 0), type = 'response')),
#                 gA1 = bound01(predict(fitA, newdata = mutate(dlong, A = 1), type = 'response')))
#
# tau <- max(dlong$m)
#
# tmle(dlong, tau)
