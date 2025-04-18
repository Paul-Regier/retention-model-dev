#' Clean FYS Survey Data
#'
#' This function takes a raw First-Year Seminar (FYS) survey data frame and applies standard cleaning steps:
#' - Cleans column names
#' - Filters out rows without student IDs
#' - Recodes data
#'
#' @param df A data frame containing raw FYS survey responses.
#'
#' @return A cleaned data frame ready for analysis.
#' @export
#'
#' @examples
#'

library(dplyr)
library(stringr)

clean_fys_survey = function(df, year = 2023) {
  # Clean Data --------------------------------------------------------------

  # Remove non-participants
  df <- df %>%
    filter(str_starts(Participate, "I agree"))
  
  
  df <- df %>%
    mutate(
      # Format Section Number
      FYS_Section = str_extract(Section, "\\d+") %>% as.numeric,
      # Add UIC, format: "1986-11-25 R 01"
      UIC = paste(DOB, substr(FirstLetterLast, 1, 1), FYS_Section)
    )
  
  
  
  
  # RECODE DATA -------------------------------------------------------------
  
  
  # Recode GMS -------------------------------------------------------------
  
  # Note GMS is negatively worded
  recode_map <- c(
    "Strongly Agree" = 1,
    "Agree" = 1,
    "Mostly Agree" = 3,
    "Mostly Disagree" = 4,
    "Disagree" = 5,
    "Strongly Disagree" = 6
  )
  # Recode specific columns (GMS1, GMS2, GMS3)
  df <- df %>%
    mutate(across(c(GMS1, GMS2, GMS3), ~ recode(.x, !!!recode_map)))
  
  
  # Recode SMM --------------------------------------------------------------
  
  recode_map <- c(
    "Strongly Disagree" = 0,
    "Disagree" = 1,
    "Neither Agree nor Disagree" = 2,
    "Agree" = 3,
    "Strongly Agree" = 4
  )
 
  df <- df %>%
    mutate(
      # Reverse code SMM1, SMM3, SMM5 by applying rev() to the recoding vector
      SMM1 = recode(SMM1, !!!recode_map %>% rev()),
      SMM3 = recode(SMM3, !!!recode_map %>% rev()),
      SMM5 = recode(SMM5, !!!recode_map %>% rev()),
      # Regular recoding for SMM2, SMM4, and SMM6
      SMM2 = recode(SMM2, !!!recode_map),
      SMM4 = recode(SMM4, !!!recode_map),
      SMM6 = recode(SMM6, !!!recode_map)
    )
  
if ( year > 2023)
  df <- df %>%
    mutate(
      # Reverse code SMM7
      SMM7 = recode(SMM1, !!!recode_map %>% rev()),
      # Regular code SMM8
      SMM8 = recode(SMM2, !!!recode_map)
    )
  
   
  # Recode CMS --------------------------------------------------------------
  
  
  recode_map <- c(
    "Strongly Agree" = 5,
    "Agree" = 4,
    "Neither Agree nor Disagree" = 3,
    "Disagree" = 1,
    "Strongly Disagree" = 0
  )
  var_names <- paste0("CMS", 1:6)
  df <- df %>%
    mutate(across(all_of(var_names), ~ recode(.x, !!!recode_map)))
  
  
  
  # Recode CMA --------------------------------------------------------------
  
  recode_map <- c(
    "1-Never" = 5,
    "2" = 4,
    "3" = 3,
    "4" = 1,
    "5-All the time" = 0
  )
  var_names <- paste0("CMA", 1:6)
  df <- df %>%
    mutate(across(all_of(var_names), ~ recode(.x, !!!recode_map)))
  
  df
}