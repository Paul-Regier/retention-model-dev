# This file contains data processing and jittering for student use

source("Fall 2023 Data Import.R")

table(cohort$FYS_Course)
table(cohort$FYS_Section)
table(post$FYS_Section)
table(pre$FYS_Section)


table(cohort$CapEd_Student)
table(cohort$First_Gen_FAFSA)
colnames(cohort)

head(cohort$UIC)
head(pre$UIC)


data_pred = left_join(pre, cohort, by = "UIC")


vars_pre  = c("UIC", "Gender", paste0("GMS",1:3), paste0("SMM",1:3), paste0("NEOS",1:9), "Section")
vars_post = c("UIC", "Gender", paste0("GMS",1:3), paste0("SMM",1:3), paste0("NEOS",1:9), paste0("Value",1:4), "Section")
pre2 = pre %>% select(
  vars_pre
)

post2 = post %>% select(
  vars_post
)

# Join and save Original data

first_join_pre = merge(cohort, pre2, by = "UIC", all = F) %>%
  rename(FYS_Section_Name = Section) %>%
  relocate(FYS_Section_Name, .after = FYS_Section)

first_join_post = merge(cohort, post2, by = "UIC", all = F) %>%
  rename(FYS_Section_Name = Section) %>%
  relocate(FYS_Section_Name, .after = FYS_Section)


second_join = merge( first_join_pre, post2, by = "UIC", all = F, suffixes = c(".pre",".post") )


getwd()

write.csv( first_join_pre %>% select(-UIC),
           "data_pre_orig2.csv", row.names = FALSE)


write.csv( first_join_post %>% select(-UIC),
           "data_post_orig2.csv", row.names = FALSE)

write.csv( second_join %>% select(-UIC),
           "data_wide_orig2.csv", row.names = FALSE)

write.csv( cohort %>% select(-UIC),
           "data_institution2.csv", row.names = FALSE)

write.csv( data_pred,
           "data_pred.csv", row.names = FALSE)


## JITTERING

#  jitter_discrete
jitter_discrete = function( v, alpha = 0.1 ) {
  n = length(v)
  Max = max( na.omit(v) )
  Min = min( na.omit(v) )
  i = 1
  for( i in 1:n ) {
    if ( !is.na(v[i]) & v[i] < Max ) {
      J = sample(0:1, 1, replace = T, prob = c(1-alpha, alpha))
      #cat("initial",v[i], '\n')
      v[i] = v[i] + J
      #cat("after:", v[i], '\n')
    }
    if ( !is.na(v[i]) & v[i] > Min ) {
      J = sample(0:1, 1, replace = T, prob = c(1-alpha, alpha))
      #cat("initial",v[i], '\n')
      v[i] = v[i] - J
      #cat("after:", v[i], '\n')
    }
  }
  v
}

## Example jitter_discrete
head( pre2$SMM1 )
head( jitter_discrete( v=pre2$SMM1 ) )
v = sample( 1:5, 100, replace = T)
jitter_discrete(v)

jitter_continuous <- function(x, jitter_fraction = 0.05) {
  # Calculate the range
  x_min <- min(x)
  x_max <- max(x)
  
  # Determine the maximum jitter allowed
  max_jitter <- jitter_fraction * (x_max - x_min)
  
  # Add random jitter to each value
  jittered_x <- x + runif(length(x), -max_jitter, max_jitter)
  
  # Clamp values to stay within the original range
  jittered_x <- pmin(pmax(jittered_x, x_min), x_max)
  
  round(jittered_x,2)
}


# Example vector
set.seed(123)  # For reproducibility
x <- c(1, 2, 3, 4, 3.5)
# Apply the jitter function
jittered_x <- jitter_continuous(x, jitter_fraction = 0.3)
jittered_x


# --- jitter_categorical ---
jitter_categorical <- function(strings, alpha = 0.15) {
  # Ensure strings is a vector
  if (!is.vector(strings) || !is.character(strings)) {
    stop("Input must be a vector of strings.")
  }
  
  # Iterate through each element of the vector
  sapply(strings, function(x) {
    if (runif(1) < alpha) {
      # Randomly pick a new value different from the current one
      new_value <- sample(strings[strings != x], 1)
      return(new_value)
    }
    # Keep the original value if no change
    return(x)
  })
}

# Example jitter_categorical
set.seed(123)  # Set seed for reproducibility
string_vector <- c("apple", "banana", "cherry", "date")
changed_vector <- jitter_categorical(string_vector, alpha = 0.2)
changed_vector

# Add jitter
set.seed(123)  # Set seed for reproducibility
cohort3 <- cohort %>%
  mutate(across(c(GPA_Career), jitter_continuous),
         across(c(major_1, Fall_Year_2_Enrolled), jitter_categorical))

head(cohort)
head(cohort3)

pre3 = pre2 %>%
  mutate(across(-c(UIC, Gender, Section), jitter_discrete))

post3 = post2 %>%
  mutate(across(-c(UIC, Gender, Section, paste0("Value",1:4)), jitter_discrete),
         across( c(paste0("Value",1:4)), jitter_categorical))

head(post2)
head(post3)



# Join and save jittered data
{
first_join_pre = merge(cohort3, pre3, by = "UIC", all = F) %>%
  rename(FYS_Section_Name = Section) %>%
  relocate(FYS_Section_Name, .after = FYS_Section)

first_join_post = merge(cohort3, post3, by = "UIC", all = F) %>%
  rename(FYS_Section_Name = Section) %>%
  relocate(FYS_Section_Name, .after = FYS_Section)


second_join = merge( first_join_pre, post3, by = "UIC", all = F, suffixes = c(".pre",".post") )


write.csv( first_join_pre %>% select(-UIC),
           "data_pre.csv", row.names = FALSE)


write.csv( first_join_post %>% select(-UIC),
           "data_post.csv", row.names = FALSE)

write.csv( second_join %>% select(-UIC),
           "data_wide.csv", row.names = FALSE)

write.csv( cohort3 %>% select(-UIC),
           "data_institution.csv", row.names = FALSE)
}



long1 = pre3 %>%
  select( paste0("GMS",1:3), paste0("SMM",1:3), paste0("NEOS",1:9) )
long2 = post3 %>%
  select( paste0("GMS",1:3), paste0("SMM",1:3), paste0("NEOS",1:9) )

long = merge(long1, long2, all = T)

write.csv( long, "data_long.csv", row.names = FALSE)
