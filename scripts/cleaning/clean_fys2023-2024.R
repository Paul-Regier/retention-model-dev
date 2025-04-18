library(here)
library(readxl)
library(dplyr)
library(stringr)

# Clean Survey Utility
source(here::here("R", "clean_fys_survey.R"))


#  2023 Data --------------------------------------------------------------

# Read Data
pre  <- here("data", "raw", "fys2023_pre.xlsx") %>% read_excel(sheet = 1)
post <- here("data", "raw", "fys2023_post.xlsx") %>% read_excel(sheet = 1)

# Clean
pre <- clean_fys_survey(pre)
pre <- clean_fys_survey(post)


# Check for duplicates - this is just for first time Analysis
if (FALSE) {
  # Pre
  I <-  df$UIC %>%
    duplicated() %>%
    which()
  
  II <- all_duplicate_indices <- which(duplicated(df$UIC) |
                                         duplicated(df$UIC, fromLast = TRUE))
  df$UIC[II]
  
  # Post
  I <- post$UIC %>%
    duplicated() %>%
    which()
  
  II <- all_duplicate_indices <- which(duplicated(post$UIC) |
                                         duplicated(post$UIC, fromLast = TRUE))
  post$UIC[II]
}


# Write Data

library(openxlsx)
filename = here("data", "processed", "fys_2023_pre_clean.xlsx")
write.xlsx(pre, filename)

filename = here("data", "processed", "fys_2023_post_clean.xlsx")
write.xlsx(post, filename)



#  2024 Data --------------------------------------------------------------

# Read Data
pre  <- here("data", "raw", "fys2024_pre.xlsx") %>% read_excel(sheet = 1)
post <- here("data", "raw", "fys2024_post.xlsx") %>% read_excel(sheet = 1)

# Clean
source(here::here("R", "clean_fys_survey.R"))
pre <- clean_fys_survey(pre,  year = 2024)
post <- clean_fys_survey(post, year = 2024)



# Check for duplicates - this is just for first time Analysis
if (FALSE) {
  # Pre
  I <-  df$UIC %>%
    duplicated() %>%
    which()
  
  II <- all_duplicate_indices <- which(duplicated(df$UIC) |
                                         duplicated(df$UIC, fromLast = TRUE))
  df$UIC[II]
  
  # Post
  I <- post$UIC %>%
    duplicated() %>%
    which()
  
  II <- all_duplicate_indices <- which(duplicated(post$UIC) |
                                         duplicated(post$UIC, fromLast = TRUE))
  post$UIC[II]
}


# Write Data

library(openxlsx)
filename = here("data", "processed", "fys_2024_pre_clean.xlsx")
write.xlsx(pre, filename)

filename = here("data", "processed", "fys_2024_post_clean.xlsx")
write.xlsx(post, filename)
