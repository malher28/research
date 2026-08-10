# clear all memory
rm(list = ls())

# packages
library(dplyr)
library(rsample)
library(data.table)

# set seed
set.seed(42)

# equity bootstrapping function
equity_bs <- function(
    data,
    group_var,
    target_var,
    bucket_size = NULL) {
  
    # check that supplied columns exist
    required_vars <- c(group_var, target_var)
    
    if (!all(required_vars %in% names(data))) {
      stop(
        "The following columns were not found: ",
        paste(setdiff(required_vars, names(data)), collapse = ", ")
      )
    }
  
    # divide data into the groups
    groups <- split(
      data,
      list(
        data[[group_var]],
        data[[target_var]]
      ),
      drop = TRUE
    )
  
  
    # equity bootstrapping
    # make list for boot groups
    boot_groups <- vector(
      mode = "list",
      length = length(groups)
    )
    
    # if no bucket size is given, make the largest bucket the default size
    if (is.null(bucket_size)) {
      N <- max(vapply(groups, nrow, integer(1)))
    } else {
      N <- bucket_size
    }
  
    # updated bootstrap -> if bucket < N, sample replace otherwise, no replacement
    N = bucket_size # biggest bucket size
  
    for (name in names(groups)){
      if (nrow(groups[[name]]) < N) {
        boot_groups[[name]] <- groups[[name]] %>% 
          slice_sample(
            n = N,      # number of obs in each group
            replace = TRUE # sample with replacement
          )
      }
      else {
        boot_groups[[name]] <- groups[[name]] %>% 
          slice_sample( 
            n = N,      # number of obs in each group
            replace = FALSE # sample without replacement
          )
      }
    }
    
    # combine all sampled buckets into one data frame
    ebs_data <- dplyr::bind_rows(boot_groups)
    
    return(ebs_data)
   
}
