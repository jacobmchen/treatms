consort <- readRDS("consort.RDS")

consort_no_exceptions <- readRDS("consort_no_exceptions.RDS")

write.csv(consort, "consort.csv")

write.csv(consort_no_exceptions, "consort_no_exceptions.csv")
