# Summary of Procedure for Deciding Event and Censoring Time

## Definitions

- Baseline date: The earleist visit date for a particular patient.
- Sustained disabiltiy progression: Let $\gamma$ be some threshold above which we consider a patient to have progressed. Sustained disability progression is defined as a visit where a value above $\gamma$ is observed AND a subsequent visit at least 180 days later also observes a value above $\gamma$.
- Censoring time: The time (in days from baseline) at which the participant is last observed in the study.
- Event time: The time (in days from baseline) at which a participant is first observed to experience sustained disability progression. An event time of infinity means that the event did not occur for a participant.

There are three repeated outcome measures: EDSS, T25FW, and NHPT. Each of these will be referred to as simply "outcome" below.

## Procedure for Deciding Censoring Time and Event Time for One Outcome

Input: (1) vector of visit dates of measurements, (2) vector of values of outcome measurements.

- Decide the threshold $\gamma$ for disease progression as defined in the SAP depending on whether the outcome is EDSS, T25FW, or NHPT.

- Define B as the baseline visit date.

- Let $k$ be the visit date of the final measurement.

- If there is only one visit,
    - Output: $k$ for the censoring time, and $\infty$ for the event time.

- Let $k_{-1}$ be the visit date of the second to last measurement.
- Let $t$ be the visit date of the first sustained disability progression. If sustained disability progression does not occur, $t=\infty$.

- If $t < k$,
    - Output: $k$ for the censoring time, and $t$ for the event time.
- Else,
    - If the outcome at visit date $k$ is above $\gamma$,
        - Output: $k-1$ for the censoring time, and $\infty$ for the event time.
    - Else,
        - Output: $k_{-1}$ for the censoring time, and $\infty$ for the event time. 

## Procedure for Deciding Censoring Time and Event Time for a Participant

- The censoring time for a participant is the maximum of the censoring times for EDSS, T25FW, and NHPT, while ignoring missing values.
- The event time for a participant is the minimum of the event times for EDSS, T25FW, and NHPT, while ignoring missing values.
- If the censoring and event times for a participant for all three outcomes are NA, then their censoring and event times are both NA.

## Examples for Deciding Censoring and Event Times for One Outcome

Below, suppose that the measurements are EDSS. Because the baseline value is always 4, the threshold for disease progression will be 5 for all of the examples below.

Input: Vector of months (0, 6, 12, 18), vector of measurements (4, 5, 4, 4).
Output: Censoring time is Month 18, and event time is infinity.

Input: Vector of months (0, 6, 18, 36), vector of measurements (4, 5, 6, 4).
Output: Censoring time is Month 36, and event time is Month 6.

Input: Vector of months (0, 6, 18, 36), vector of measurements (4, 5, 4, 6).
Output: Censoring time is Month 18, and event time is infinity.

Input: Vector of months (0, 6, 18, 36), vector of measurements (4, 4, 5, 6).
Output: Censoring time is Month 36, and event time is Month 18.

Input: Vector of months (0, 6, 18, 42), vector of measurements (4, 4, 4, 7).
Output: Censoring time is Month 18, and event time is infinity.

Input: Vector of months (0, 12), vector of measurements (4, 4).
Output: Censoring time is Month 12, and event time is infinity.

Input: Vector of months (0, 6), vector of measurements (4, 5).
Output: Censoring time is Month 0, and event time is infinity.

## Procedure for Imputing Values for Sensitivity Analyses

The following procedure for imputing EDSS, T25FW, and NHPT values applies only to the sensitivity analyses and will be the same regardless of the repeated outcome measure. Thus, we will define the procedure for EDSS, and it can be extended to the other outcome measures.

1. Temporarily set aside measurements labeled "Interim Information".
2. Let $\alpha$ be the month label of the first measurement and $\beta$ be the month label of the final measurement. By construction, $\alpha$ and $\beta$ will be multiples of 6.
3. For each multiple of 6 between $\alpha$ and $\beta$ that has no measurement, create a new row with the corresponding month label. For each new row, we will need to impute a visit_date and an EDSS value.
4. Consider a row that we need to impute the visit_date for. Let prev_date and next_date be the visit date of the previous and next observed measurements, respectively. Let prev_month and next_month be the month label of the previous and next observed measurements, respectively. Let cur_month be the month label of the current row. Impute the visit date as prev_date + (next_date - prev_date) * (cur_month/6 - prev_month/6) / (next_month/6 - prev_month/6).
5. There are two choices of imputation methods for the actual value of EDSS: "A best, B worst" and "A worst, B best". For the first imputation option, impute all missing values of EDSS with the best possible value if treatment is 0 and the worst possible value otherwise. For the second, impute all missing values of EDSS with the worst possible value if treatment is 0 and the best possible value otherwise.
6. Put the measurements labled "Interim Information" back into the data.

Best and worst values of EDSS are 0 and 9.5.
Best and worst values of T25FW are 0 and 80.
Best and worst values of NHPT are 0 and 200.
