# Loading the packages:
library(janitor)
library(tidyverse)
library(here)



# Loading the raw_data:



loans_dictionary <- read_csv(here("defaults_data/LCDataDictionary.csv"))
loans <- read_csv(here("defaults_data/lending_club_loans.csv"))
state_info <- read_csv(here("defaults_data/state_names_info.csv"))
grade_info <- read_csv(here("defaults_data/grade_info.csv"))



# Cleaning the data:



# Removing all columns with NA's greater than 95%:
loans_cols_na_removed <- loans[lapply(loans, function(x)
  sum(is.na(x)) / length(x) ) < 0.95]

loans_clean <- loans_cols_na_removed %>%
  # Making percentage variables into numeric and taking away the '%' sign
  mutate(int_rate = as.numeric(str_replace_all(string = int_rate,
                                               pattern = "\\%",
                                               replacement = "")),
         revol_util = as.numeric(str_replace_all(string = revol_util,
                                                 pattern = "\\%",
                                                 replacement = "")),
         
         # Removing the "Borrower added on mm/dd/yy > " from the desc
         desc = str_replace_all(string = desc,
                                pattern = c("Borrower.*?>"),
                                replacement = ""),
         
         # Obtaining the mean FICO scores
         mean_fico_scores = ((fico_range_low + fico_range_high) / 2),
         
         # Grouping NA and "n/a" together for emp_length
         emp_length = na_if(emp_length, "n/a"),
         
         # Make pymnt_plan binary
         pymnt_plan = if_else(pymnt_plan == "y", 1, 0))%>%
  
  # Removing variables with no relevance
  select(-c(url, initial_list_status, application_type)) %>%
  
  # Renaming variables to make some more understandable at a glance
  rename(c("debt_to_income_ratio" = "dti", "no_open_cred_lines" = "open_acc",
           "derog_pub_rec" = "pub_rec", "revol_util_rate" = "revol_util")) %>%
  
  # Filtering out where the response variable,loan_status, is NA
  filter(!is.na(loan_status))



# Joining all the data together:



# Creating a new data.frame to append the state_info:
state_abb <- c("DC", NA)
state_name <- c("Washington, D.C.", NA)
new_states <- data.frame(state_abb, state_name)

# Binding the data frame onto state_info
state_info_new <- rbind(state_info, new_states)



# Joining grade_info
grades_joined <- left_join(loans_clean, grade_info, by = "sub_grade") 

# Joining state_info_new
loans_joined <- left_join(grades_joined, state_info_new,
                          by = c("addr_state" = "state_abb"))

write_csv(x = loans_clean, here("clean_data/loans_clean.csv"))

