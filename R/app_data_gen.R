#' app_data_gen: Generate Example Data for use with EligModelShinyApp() 
#'
#' Generates a list of example datasets to use as input for EligModelShinyApp(). The returned list includes 
#' an example list for patient eligibility data (elig_dat_list), patient characteristics (char_dat_list), 
#' time-to-event data (surv_dat_list), and data dictionaries (data_dict_list). The length of each list
#' corresponds to the number of trial and real-world dataset combinations specified. 
#'
#' @param n (type = integer) number of observations (patients) to include in the example data (default=1000)
#' @param n_trials (type = integer) number of clinical trials in the example data (default=3)
#' @param n_rwd (type = integer)  number of real-world datasets in the example data (default=2) 
#' @param miss_pct (type = numeric) proportion missing in the example eligibility criteria data (default=0.10) 
#'
#' @returns A named list with the following components: 
#' \itemize{
#' \item elig_dat_list - list of data.frames with example eligibility criteria data. The length of the list equals the number of trial/RWD combinations specified. 
#' \item char_dat_list - list of data.frames with example patient characteristic data. The length of the list equals the number of trial/RWD combinations specified. 
#' \item surv_dat_list - list of data.frames with example survival data. The length of the list equals the number of trial/RWD combinations specified. 
#' \item data_dict_list - list of data.frames with example data dictionaries. The length of the list equals the number of trial/RWD combinations specified. 
#' }
#' @export
#' @importFrom stats rbinom runif
app_data_gen <- function(n=1000, n_trials = 3, n_rwd = 2, n_cohorts = 1, miss_pct = 0.10){

  gen_trial_index <- function(n, trial_n, rwd_n, n_cohorts){
    record_ids <- 1:n
    cohort_vector <- paste0("Cohort ", 1:n_cohorts)
    
    withr::with_seed(98, {
      ret <- data.frame(record_id = record_ids,
                        trial = rep(paste0('Trial ', trial_n), n),
                        rwd = rep(paste0('Data ', rwd_n), n),
                        # shouldn't pose any issues, but cohort sizes will be
                        # slightly imbalanced
                        cohort = sample(cohort_vector, n, replace = TRUE))
    })
    
    return(ret)
  }
  
  gen_data_dict <- function(trial_n, rwd_n){
    # number of eligibility criteria could be provided as function argument
    
    ecs <- data.frame("trial" = rep(paste0('Trial ', trial_n), 5),
                      "rwd" = rep(paste0('Data ', rwd_n), 5),
                      "var_type" = rep("Eligibility Criteria", 5),
                      "col_name" = paste0("IE_", 1:5),
                      "clean_name" = paste0("Eligibility Criteria ", 1:5),
                      "miscellaneous" = paste0("Meets Eligibility Criteria ", 1:5))
    
    bsl_chars <- data.frame(
                      "trial" = rep(paste0('Trial ', trial_n), 6),
                      "rwd" = rep(paste0('Data ', rwd_n), 6),
                      "var_type" = rep("Patient Characteristic", 6),
                      "col_name" = c("race", "ethnicity", "sex", "age_dx", "diag_year", "geographic"),
                      "clean_name" = c("Race", "Ethnicity", "Sex", "Age at Diagnosis", "Diagnosis Year", "Geography"),
                      "miscellaneous" = rep("", 6))
    
    ret <- rbind(ecs, bsl_chars)
    
    return(ret)
                      
  }
  
  gen_os_df <- function(trial_n, rwd_n, n_cohorts){
    os_df <- gen_trial_index(n, trial_n, rwd_n, n_cohorts)
    # from survival package
    sample_data <- survival::lung
    # In the lung dataset from the survival package, the status column is coded as:
    # 1 = censored
    # 2 = event 
    # recode as 2 == 1 and 1 == 0
    
    sample_data$status[which(sample_data$status == 1)] <- 0
    sample_data$status[which(sample_data$status == 2)] <- 1
    
    os_df$time <- fractional_rep(sample_data$time, n/nrow(sample_data))
    os_df$status <- fractional_rep(sample_data$status, n/nrow(sample_data))
    
    return(os_df)
  }
  
  gen_char_df <- function(trial_n, rwd_n, n_cohorts){
    bsl_df <- gen_trial_index(n, trial_n, rwd_n, n_cohorts)
    
    bsl_df$race <- sample(
      c("Asian", "Black", "Other", "Unknown", "White"),
      size = n,
      replace = TRUE,
      prob = c(0.08, 0.14, 0.02, 0.16, 0.60)
    )
    
    bsl_df$ethnicity <- sample(
      c("Hispanic or Latino", "Not Hispanic or Latino", "Other", "Unknown"),
      size = n,
      replace = TRUE,
      prob = c(0.09, 0.85, 0.02, 0.04)
    )
    
    bsl_df$sex <- sample(
      c("Female", "Male"),
      size = n,
      replace = TRUE, 
      prob = c(0.45, 0.55)
    )
    
    bsl_df$age_dx <- sample(18:85, n, replace = TRUE)
    
    bsl_df$diag_year <- sample(1996:2017, n, replace = TRUE)
    
    state_vec <- c(
      "MA", "NY", "GA", "LA", "CA", "TN", "VT", "NC", "VA", "SC",
      "CO", "AZ", "NV", "NM", "FL", "TX", "PA", "NJ", "WV", "NH"
    )
      
    bsl_df$geographic <- sample(state_vec, n, replace = TRUE)
    
    return(bsl_df)
  }
  
  gen_ec_df <- function(trial_n, rwd_n, n_cohorts){
    ec_df <- gen_trial_index(n, trial_n, rwd_n, n_cohorts)
    
    ec_col_names <- paste0("IE_", 1:5)
    
    for(col in ec_col_names){
      
      elig_prob <- runif(1, min = 0.55, max = 0.75)
      inelig_prob <- (1 - elig_prob - miss_pct)
      
      ec_df[[col]] <- sample(
        c(1, 0, NA), 
        size = n, 
        replace = TRUE,
        prob = c(elig_prob, inelig_prob, miss_pct)
      )
    }
    
    return(ec_df)
  }
  
  datasets <- 1:n_rwd
  trials <- 1:n_trials
  
  gen_list <- expand.grid(trial_n = trials, rwd_n = datasets)
  
  # should n_cohorts be a function argument to app_data_gen()?
  gen_list$n_cohorts <- rep(n_cohorts, nrow(gen_list))
  
  elig_dat_list <- lapply(
    seq_len(nrow(gen_list)), 
    function(i){
      row <- gen_list[i, ]
      gen_ec_df(row$trial_n, row$rwd_n, row$n_cohorts)
    }
  )
  
  char_dat_list <- lapply(
    seq_len(nrow(gen_list)), 
    function(i){
      row <- gen_list[i, ]
      gen_char_df(row$trial_n, row$rwd_n, row$n_cohorts)
    }
  )
  
  surv_dat_list <- lapply(
    seq_len(nrow(gen_list)), 
    function(i){
      row <- gen_list[i, ]
      gen_os_df(row$trial_n, row$rwd_n, row$n_cohorts)
    }
  )
  
  data_dict_list <- lapply(
    seq_len(nrow(gen_list)), 
    function(i){
      row <- gen_list[i, ]
      gen_data_dict(row$trial_n, row$rwd_n)
    }
  )
  
  ret = list(elig_dat_list = elig_dat_list,
             char_dat_list = char_dat_list,
             surv_dat_list = surv_dat_list,
             data_dict_list = data_dict_list)
  
  return(ret)
  
}