
# Function for crisis-specific 5q0  from u1 and 1to4 deaths and population
# Input: df [data.frame; columns listed below]
# - ed_0_1: excess deaths age 0 years
# - ed_1_5: excess deaths age 1-4 years
# - pop_0_1: population age 0 years
# - pop_1_5: population age 1-4 years
get_ed_5q0 <- function (df) {
  df <- df %>%
    mutate(ed_1m0 = ed_0_1 / pop_0_1,
           ed_4m1 = ed_1_5 / pop_1_5) %>%
    mutate(ed_1q0 = ed_1m0 / (1 + (1 - (0.3)) * ed_1m0),
           ed_4q1 = 4 * ed_4m1 / (1 + (4 - 4 * (0.4)) * ed_4m1)) %>%
    mutate(ed_5q0 = 1 - (1 - ed_1q0) * (1 - ed_4q1))
  return(df)
}

