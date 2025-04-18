library(here)
library(readxl)
library(dplyr)




# Read Data ---------------------------------------------------------------

cohort <- here("data", "raw", "cohort_fall_2023.csv") %>% read.csv()


# Clean ------------------------------------------------------------------

cohort <- cohort %>%
  mutate(
    # format data
    birth_dte = as.Date(birth_dte, format = "%m/%d/%Y"),
    # Add UIC, format: "1986-11-25 R 01"
    UIC = paste(
      format(birth_dte, "%Y-%m-%d"),
      substr(last_name, 1, 1),
      FYS_Section
    )
  )




# Check and Remove Duplicates ---------------------------------------------

I <- cohort$UIC %>%
  duplicated() %>%
  which()
II <-  all_duplicate_indices <- which(duplicated(cohort$UIC) |
                                        duplicated(cohort$UIC, fromLast = TRUE))
# Duplicates were Twins in same section!
# No choice but to remove one ??
cohort$UIC[II]
