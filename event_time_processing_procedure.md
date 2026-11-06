# Summary of Procedure for Deciding Event Time

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

For simplicity, suppose that the threshold for disease progression is always 5.

Input: Vector of months (0, 6, 12, 18), vector of measurements (3, 5, 4, 4).
Output: Censoring time is Month 18, and event time is infinity.

Input: Vector of months (0, 6, 18, 36), vector of measurements (3, 5, 6, 4).
Output: Censoring time is Month 36, and event time is Month 6.

Input: Vector of months (0, 6, 18, 36), vector of measurements (3, 5, 4, 6).
Output: Censoring time is Month 18, and event time is infinity.

Input: Vector of months (0, 6, 18, 36), vector of measurements (3, 4, 5, 6).
Output: Censoring time is Month 36, and event time is Month 18.

Input: Vector of months (0, 6, 18, 42), vector of measurements (3, 4, 4, 7).
Output: Censoring time is Month 18, and event time is infinity.

Input: Vector of months (0, 12), vector of measurements (3, 4).
Output: Censoring time is Month 12, and event time is infinity.

Input: Vector of months (0, 6), vector of measurements (3, 5).
Output: Censoring time is Month 0, and event time is infinity.

