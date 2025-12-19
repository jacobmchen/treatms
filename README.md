# TREAT-MS Analysis Code

``data_exploration.R`` contains preliminary analysis code. At the moment, all the code is in a single file, so some refactoring is probably due soon.

2025-12-19
- Out of the patients who returned for at least 1 post-baseline follow-up visit, how many have at least 2 post-baseline EDSS+ measured?
- We will run this query on a separate file ``edss_missingness.R``.
- First, we need all patients with at least one baseline visit. We will subset the data to just those patients.
- Then, we will need all patients with at least 1 post-basline follow-up visit. We will subset the data to just those patients.
- Finally, we will need all patients with at least 2 post-baseline follow-up visits. We will report the number of patients with at least 2 post-baseline out of the number of patients with at least 1 post-baseline follow-up visit.
