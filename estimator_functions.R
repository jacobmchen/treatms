###
# This code was obtained from the appendix of "Improved Precision in the Analysis of Randomized
# Trials with Survival Outcomes, without Assuming Proportional Hazards" by Ivan Diaz,
# Elizabeth Colantuoni, Daniel F. Hanley, and Michael Rosenblum.
###

# need to import the glm2 package since this file uses it
library(glm2)

#' Bound a vector of probabilities in [r, 1 - r] for r > 0
#'
#' @param x Vector of probabilities
#' @param r Bound
#'
#' @return Bounded vector of probabilities
bound01 <- function(x, r = 1e-10){
    xx <- x
    xx[x < r] <- r
    xx[x > 1-r] <- 1-r
    return(as.numeric(xx))
}

#' Bound a vector of probabilities in [r, 1] for r > 0
#'
#' @param x Vector of probabilities
#' @param r Bound
#'
#' @return Bounded vector of probabilities
bound <- function(x, r = 0.001){
    xx <- x
    xx[x < r] <- r
    return(as.numeric(xx))
}

#' Transform a survival dataset from short to long form
#'
#' @param df A data frame containing columns T (time to event) and C (censoring indicator)
#' @param freq.time If time T must be ccategorized into coarser intervals (e.g., use freq.time = 7 if T is in days and you want the output in weeks)
#'
#' @return A long form data set
transformData <- function(df, freq.time){

    ## Transform a dataset from the short to the long form.
    if(freq.time > 1) df$T <- df$T %/% freq.time + 1

    n <- dim(df)[1]
    K <- max(df$T)

    # m goes from 1 to the max observed timepoint n times, once for
    # each individual
    m <- rep(1:K, n)
    Lm <- Rm <- rep(NA, n*K)
    Im <- Jm <- 1*(m == 1)

    ## Note: R and J are lagged with respect to definitions in the paper.

    for(t in 1:K){
        # R_t is 1 when the censoring indicator is 0 (unobserved) and T=t
        Rm[m == t] <- (1 - df$D) * (df$T == t)
        # L_t is 1 when the censoring indicator is 1 (observed) and T=t
        Lm[m == t] <- df$D * (df$T == t)
        # I_t is 1 when T >= t, which ensures that (R_1, L_1) through (R_{t-1}, L_{t-1}) are all 0
        Im[m == t] <- (df$T >= t)
        # J_t is 1 when (if the censoring indicator is 1) T > t or when (if the censoring
        # indicator is 0) T >= t
        Jm[m == t] <- (df$T > t) * df$D + (df$T >= t) * (1 - df$D)
    }

    # we have to cast df as a data.frame first or else R throws an error when
    # trying to make copies of the dataframe using the gl function
    df <- data.frame(df)

    data <- data.frame(df[gl(n, K), ], m = m, Im, Jm, Rm, Lm)

    return(data)

}

#' Compute TMLE of RMST with efficiency gains
#'
#' @param data A long form data frame (e.g., the output of transformData*())
#' @param tau A restriction time of interest
#'
#' @return RMST estimate for each treatment arm with influence function based standard error for the difference
#'
#' @examples
#' library(survival)
#' library(tidyverse)
#' library(dummies)
#'
#' data <- colon %>% dummy.data.frame(c('differ', 'extent')) %>%
#'   filter(rx != 'Obs') %>%
#'   mutate(A = rx == 'Lev+5FU', id = as.numeric(as.factor(id)),
#'          nanodes = is.na(nodes), nodes = ifelse(is.na(nodes), 0, nodes)) %>%
#'   select(-rx) %>%  group_by(id) %>% summarise_all(funs(min)) %>% select(-study) %>%
#'   rename(T = time, D = status)
#'
#' dlong <- transformData(data, 30)
#'
#' fitL <- glm(Lm ~ A * (m + sex + age + obstruct + perfor + adhere + nodes + D +
#'                         differ1 + differ2 + differ3 + differNA + extent1 + extent2 +
#'                         extent3 + extent4 + surg + node4 + etype),
#'             data = dlong, subset = Im == 1, family = binomial())
#' fitR <- glm(Rm ~ A * (m + sex + age + obstruct + perfor + adhere + nodes + D +
#'                         differ1 + differ2 + differ3 + differNA + extent1 + extent2 +
#'                         extent3 + extent4 + surg + node4 + etype),
#'             data = dlong, subset = Jm == 1, family = binomial())
#' fitA <- glm(A ~ sex + age + obstruct + perfor + adhere + nodes + D +
#'               differ1 + differ2 + differ3 + differNA + extent1 + extent2 +
#'               extent3 + extent4 + surg + node4 + etype,
#'             data = dlong, subset = m == 1, family = binomial())
#'
#' dlong <- mutate(dlong,
#'                 gR1 = bound01(predict(fitR, newdata = mutate(dlong, A = 1), type = 'response')),
#'                 gR0 = bound01(predict(fitR, newdata = mutate(dlong, A = 0), type = 'response')),
#'                 h1 = bound01(predict(fitL, newdata = mutate(dlong, A = 1), type = 'response')),
#'                 h0 = bound01(predict(fitL, newdata = mutate(dlong, A = 0), type = 'response')),
#'                 gA1 = bound01(predict(fitA, newdata = mutate(dlong, A = 1), type = 'response')))
#'
#' tau <- max(dlong$m)
#'
#' tmle(dlong, tau)
tmle <- function(data, tau){

    h1 <- data$h1
    h0 <- data$h0
    gR1 <- data$gR1
    gR0 <- data$gR0
    gA1 <- data$gA1[data$m == 1]
    id <- data$id
    A <- data$A

    n <- length(unique(id))
    m <- as.numeric(data$m)
    K <- max(m)

    gA0 <- 1 - gA1
    h  <- A*h1 + (1-A)*h0
    gR <- A*gR1 + (1-A)*gR0

    crit <- TRUE
    iter <- 1

    ind <- outer(m, 1:K, '<=')

    while(crit && iter <= 20){

        S1 <- tapply(1-h1, id, cumprod, simplify = FALSE)
        S0 <- tapply(1-h0, id, cumprod, simplify = FALSE)

        G1 <- tapply(1-gR1, id, cumprod, simplify = FALSE)
        G0 <- tapply(1-gR0, id, cumprod, simplify = FALSE)

        St1 <- do.call('rbind', S1[id])
        St0 <- do.call('rbind', S0[id])

        Sm1 <- unlist(S1)
        Sm0 <- unlist(S0)
        Gm1 <- unlist(G1)
        Gm0 <- unlist(G0)

        Z1 <- -rowSums((ind * St1)[, 1:(tau-1)]) / bound(Sm1 * gA1[id] * Gm1)
        Z0 <- -rowSums((ind * St0)[, 1:(tau-1)]) / bound(Sm0 * gA0[id] * Gm0)

        if(iter == 1){

            DT1 <- with(data, tapply(Im * A*Z1 * (Lm - h), id, sum))
            DT0 <- with(data, tapply(Im * (1-A)*Z0 * (Lm - h), id, sum))
            DW1 <- with(data, rowSums(St1[as.numeric(m) == 1, 1:(tau-1)]))
            DW0 <- with(data, rowSums(St0[as.numeric(m) == 1, 1:(tau-1)]))
            aipw <- 1 + c(mean(DT0 + DW0), mean(DT1 + DW1))

            Z1ipw <- -rowSums(ind[, 1:(tau-1)]) / bound(gA1[id] * Gm1)
            Z0ipw <- -rowSums(ind[, 1:(tau-1)]) / bound(gA0[id] * Gm0)
            DT1ipw <- with(data, tapply(Im * A*Z1ipw * Lm, id, sum))
            DT0ipw <- with(data, tapply(Im * (1-A)*Z0ipw * Lm , id, sum))
            ipw  <- tau + c(mean(DT0ipw), mean(DT1ipw))

        }

        M <- tapply(Sm1 / bound(gA1[id]) * (m <= tau-1), id, sum) +
            tapply(Sm0 / bound(gA0[id]) * (m <= tau-1), id, sum)
        H1 <- -rowSums((ind * St1)[,1:(tau-1)]) / bound(gA1[id] * Gm1)
        H0 <- -rowSums((ind * St0)[,1:(tau-1)]) / bound(gA0[id] * Gm0)
        H <- A * H1 - (1-A) * H0

        eps   <- coef(glm2(Lm ~ 0 + offset(qlogis(h)) + I(A * Z1) + I((1-A) * Z0),
                           family = binomial(), subset = Im == 1, data = data))
        gamma <- coef(glm2(Rm ~ 0 + offset(qlogis(gR)) + H, family = binomial(),
                           subset = Jm == 1, data = data))
        nu    <- coef(glm2(A[m == 1] ~ 0 + offset(qlogis(gA1)) + M,
                           family = binomial()))

        h1old <- h1
        h0old <- h0
        gR1old <- gR1
        gR0old <- gR0
        gA1old <- gA1

        eps[is.na(eps)] <- 0
        gamma[is.na(gamma)] <- 0
        nu[is.na(nu)] <- 0

        h1 <- bound01(plogis(qlogis(h1) + eps[1] * Z1))
        h0 <- bound01(plogis(qlogis(h0) + eps[2] * Z0))
        gR1 <- bound01(plogis(qlogis(gR1) + gamma * H1))
        gR0 <- bound01(plogis(qlogis(gR0) - gamma * H0))
        gA1 <- bound01(plogis(qlogis(gA1) + nu * M))
        gA0 <- 1 - gA1
        h  <- A*h1  + (1-A)*h0
        gR <- A*gR1 + (1-A)*gR0
        iter <-  iter + 1

        crit <- any(abs(c(eps, gamma, nu)) > 1e-3/n^(0.6))

    }

    S1 <- tapply(1-h1, id, cumprod, simplify = FALSE)
    S0 <- tapply(1-h0, id, cumprod, simplify = FALSE)

    G1 <- tapply(1-gR1, id, cumprod, simplify = FALSE)
    G0 <- tapply(1-gR0, id, cumprod, simplify = FALSE)

    St1 <- do.call('rbind', S1[id])
    St0 <- do.call('rbind', S0[id])

    Sm1 <- unlist(S1)
    Sm0 <- unlist(S0)
    Gm1 <- unlist(G1)
    Gm0 <- unlist(G0)

    Z1 <- -rowSums((ind * St1)[, 1:(tau-1)]) / bound(Sm1 * gA1[id] * Gm1)
    Z0 <- -rowSums((ind * St0)[, 1:(tau-1)]) / bound(Sm0 * gA0[id] * Gm0)

    DT <- with(data, tapply(Im * (A*Z1 - (1-A)*Z0) * (Lm - h), id, sum))
    DW1 <- with(data, rowSums(St1[m==1, 1:(tau-1)]))
    DW0 <- with(data, rowSums(St0[m==1, 1:(tau-1)]))
    # in the following two lines, I added na.rm=TRUE so that taking the
    # mean even with missing values will not result in theta1 and theta2
    # being NA
    theta1 <- 1 + mean(DW1, na.rm=TRUE)
    theta0 <- 1 + mean(DW0, na.rm=TRUE)
    theta <- theta1 - theta0
    D <- DT + DW1 - DW0 - theta
    # in the var() function below, I added na.rm=TRUE so that taking
    # the variance of the vector D will not be NA even if it has
    # NA values
    sdn <- sqrt(var(D, na.rm=TRUE) / n)

    return(list(theta = c(theta0, theta1), sdn = sdn))

}

#' Compute IPW of RMST
#'
#' @param data A long form data frame (e.g., the output of transformData*())
#' @param tau A restriction time of interest
#'
#' @return RMST estimate for each treatment arm
#'
#' @examples
#' # See example of tmle() for the creation of dlong
#' ipw(dlong, tau)
ipw <- function(data, tau){

    h1 <- data$h1
    h0 <- data$h0
    gR1 <- data$gR1
    gR0 <- data$gR0
    gA1 <- data$gA1[data$m == 1]
    id <- data$id
    A <- data$A

    n <- length(unique(id))
    m <- as.numeric(data$m)
    K <- max(m)

    gA0 <- 1 - gA1
    h  <- A*h1 + (1-A)*h0
    gR <- A*gR1 + (1-A)*gR0

    ind <- outer(m, 1:K, '<=')

    G1 <- tapply(1-gR1, id, cumprod, simplify = FALSE)
    G0 <- tapply(1-gR0, id, cumprod, simplify = FALSE)

    Gm1 <- unlist(G1)
    Gm0 <- unlist(G0)

    Z1ipw <- -rowSums(ind[, 1:(tau-1)]) / bound(gA1[id] * Gm1)
    Z0ipw <- -rowSums(ind[, 1:(tau-1)]) / bound(gA0[id] * Gm0)
    DT1ipw <- with(data, tapply(Im * A*Z1ipw * Lm, id, sum))
    DT0ipw <- with(data, tapply(Im * (1-A)*Z0ipw * Lm , id, sum))
    ipw  <- tau + c(mean(DT0ipw), mean(DT1ipw))

    return(ipw)

}

#' Compute AIPW of RMST
#'
#' @param data A long form data frame (e.g., the output of transformData*())
#' @param tau A restriction time of interest
#'
#' @return RMST estimate for each treatment arm with influence function based standard error for the difference
#'
#' @examples
#' # See example of tmle() for the creation of dlong
#' aipw(dlong, tau)
aipw <- function(data, tau){

    h1 <- data$h1
    h0 <- data$h0
    gR1 <- data$gR1
    gR0 <- data$gR0
    gA1 <- data$gA1[data$m == 1]
    id <- data$id
    A <- data$A

    n <- length(unique(id))
    m <- as.numeric(data$m)
    K <- max(m)

    gA0 <- 1 - gA1
    h  <- A*h1 + (1-A)*h0
    gR <- A*gR1 + (1-A)*gR0

    ind <- outer(m, 1:K, '<=')

    S1 <- tapply(1-h1, id, cumprod, simplify = FALSE)
    S0 <- tapply(1-h0, id, cumprod, simplify = FALSE)

    G1 <- tapply(1-gR1, id, cumprod, simplify = FALSE)
    G0 <- tapply(1-gR0, id, cumprod, simplify = FALSE)

    St1 <- do.call('rbind', S1[id])
    St0 <- do.call('rbind', S0[id])

    Sm1 <- unlist(S1)
    Sm0 <- unlist(S0)
    Gm1 <- unlist(G1)
    Gm0 <- unlist(G0)

    Z1 <- -rowSums((ind * St1)[, 1:(tau-1)]) / bound(Sm1 * gA1[id] * Gm1)
    Z0 <- -rowSums((ind * St0)[, 1:(tau-1)]) / bound(Sm0 * gA0[id] * Gm0)

    DT1 <- with(data, tapply(Im * A*Z1 * (Lm - h), id, sum))
    DT0 <- with(data, tapply(Im * (1-A)*Z0 * (Lm - h), id, sum))
    DW1 <- with(data, rowSums(St1[as.numeric(m) == 1, 1:(tau-1)]))
    DW0 <- with(data, rowSums(St0[as.numeric(m) == 1, 1:(tau-1)]))
    aipw <- 1 + c(mean(DT0 + DW0), mean(DT1 + DW1))

    D <- DT1 - DT0 + DW1 - DW0
    sdn <- sqrt(var(D) / n)

    return(list(theta = aipw, sdn = sdn))

}

#' Compute unadjusted IPW and Kaplan-Meier estimators of RMST
#'
#' @param data A long form data frame (e.g., the output of transformData*())
#' @param tau A restriction time of interest
#'
#' @return RMST estimate for each treatment arm
#'
#' @examples
#' # See example of tmle() for the creation of dlong
#' unadjusted(dlong, tau)
unadjusted <- function(data, tau){

    fith  <- lm(Lm ~ as.factor(m)*as.factor(A), subset = Im == 1, data = data)
    fitgR <- lm(Rm ~ as.factor(m)*as.factor(A), subset = Jm == 1, data = data)
    fitgA <- lm(A ~ 1, subset = m == 1, data = data)

    h1 <- bound01(predict(fith, newdata = data.frame(m = data$m, A = 1),
                          type = 'response'))
    h0 <- bound01(predict(fith, newdata = data.frame(m = data$m, A = 0),
                          type = 'response'))
    gR1 <- bound01(predict(fitgR, newdata = data.frame(m = data$m, A = 1),
                           type = 'response'))
    gR0 <- bound01(predict(fitgR, newdata = data.frame(m = data$m, A = 0),
                           type = 'response'))
    gA1 <- bound01(predict(fitgA, type = 'response'))

    id <- data$id
    n <- length(unique(id))
    m <- as.numeric(data$m)
    K <- max(m)
    A <- data$A

    gA0 <- 1 - gA1
    h  <- A*h1 + (1-A)*h0
    gR <- A*gR1 + (1-A)*gR0
    ind <- outer(m, 1:K, '<=')
    S1 <- tapply(1-h1, id, cumprod, simplify = FALSE)
    S0 <- tapply(1-h0, id, cumprod, simplify = FALSE)
    G1 <- tapply(1-gR1, id, cumprod, simplify = FALSE)
    G0 <- tapply(1-gR0, id, cumprod, simplify = FALSE)
    St1 <- do.call('rbind', S1[id])
    St0 <- do.call('rbind', S0[id])
    Sm1 <- unlist(S1)
    Sm0 <- unlist(S0)
    Gm1 <- unlist(G1)
    Gm0 <- unlist(G0)

    Z1ipw <- -rowSums(ind[, 1:(tau-1)]) / bound(gA1[id] * Gm1)
    Z0ipw <- -rowSums(ind[, 1:(tau-1)]) / bound(gA0[id] * Gm0)
    DT1ipw <- with(data, tapply(Im * A*Z1ipw * Lm, id, sum))
    DT0ipw <- with(data, tapply(Im * (1-A)*Z0ipw * Lm , id, sum))
    ipw  <- tau + c(mean(DT0ipw), mean(DT1ipw))

    DW1 <- with(data, rowSums(St1[m==1, 1:(tau-1)]))
    DW0 <- with(data, rowSums(St0[m==1, 1:(tau-1)]))
    km <- 1 + c(mean(DW0), mean(DW1))

    return(list(km = km, ipw = ipw))

}

