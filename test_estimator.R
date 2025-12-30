# test the estimator code using the example given

source("estimator_functions.R")

# colon is the name of a dataset included in the survival package.
# The survival package includes functions for basic survival analysis such
# as Kaplan-Meier.
library(survival)
library(tidyverse)
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
# status: Binary indicator of censoring status
# differ: Differentiation of tumor; measure of severity from 1 through 3
# extent: Extent of local spread; measure of severity from 1 through 4
# surg: Time from surgery to registration; measure of severity from 0 through 1
# node4: Binary indicator of more than 4 positive lymph nodes
# etype: Event type of 1=recurrence or 2=death
print(head(colon))
print(unique(colon$extent))

# data <- colon %>% dummy.data.frame(c('differ', 'extent')) %>%
#   filter(rx != 'Obs') %>%
#   mutate(A = rx == 'Lev+5FU', id = as.numeric(as.factor(id)),
#          nanodes = is.na(nodes), nodes = ifelse(is.na(nodes), 0, nodes)) %>%
#   select(-rx) %>%  group_by(id) %>% summarise_all(funs(min)) %>% select(-study) %>%
#   rename(T = time, D = status)
#
# dlong <- transformData(data, 30)
#
# fitL <- glm(Lm ~ A * (m + sex + age + obstruct + perfor + adhere + nodes + D +
#                         differ1 + differ2 + differ3 + differNA + extent1 + extent2 +
#                         extent3 + extent4 + surg + node4 + etype),
#             data = dlong, subset = Im == 1, family = binomial())
# fitR <- glm(Rm ~ A * (m + sex + age + obstruct + perfor + adhere + nodes + D +
#                         differ1 + differ2 + differ3 + differNA + extent1 + extent2 +
#                         extent3 + extent4 + surg + node4 + etype),
#             data = dlong, subset = Jm == 1, family = binomial())
# fitA <- glm(A ~ sex + age + obstruct + perfor + adhere + nodes + D +
#               differ1 + differ2 + differ3 + differNA + extent1 + extent2 +
#               extent3 + extent4 + surg + node4 + etype,
#             data = dlong, subset = m == 1, family = binomial())
#
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
