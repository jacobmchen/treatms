# Summary of Procedure for Deciding Event Time

## Definitions

- Censoring time: The time (in months from baseline) at which the participant is last observed in the study. Month 0 denotes the baseline visit.
- Event time: The time (in months from baseline) at which a participant is first observed to progress and is sustained in the subsequent visit. An event time of infinity means that the event did not occur for a participant.

There are three repeated outcome measures: EDSS, T25FW, and NHPT. Each of these will be referred to as simply "outcome" below.

## Procedure for Deciding Censoring Time and Event Time for One Outcome

Input: (1) vector of months of measurements, (2) vector of values of outcome measurements.

- Decide the threshold for disease progression as defined in the SAP depending on whether the outcome is EDSS, T25FW, or NHPT.

- Define C as the censoring time, which is the month of the last measurement.
- Define T as the event time, which is the month at which a value of the measurement is greater than or equal to the threshold and remains greater than or equal to the threshold at the next measurement. If the event does not occur, T is set to infinity.

- If the total number of measurements is less than or equal to 1,
    - Output: NA for the censoring time, and NA for the event time.

- If T <= C,
    - Output: C for the censoring time, and T for the event time.
- Otherwise,
    - If the value of the final measurement is greater than or equal to the threshold,
        - Change the censoring time C to be the month of the second to last measurement.
        - Output: C for the censoring time, and infinity for the event time.
    - Otherwise,
        - Output: C for the censoring time, and infinity for the event time.

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

