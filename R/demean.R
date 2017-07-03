#' Demean data
#'
#' This function generates a GUIDE batch input file
#' @param i_data,
#' @param i_var
#' @keywords demean, fixed effect
#' @export
#'
#'
demean <- function(i_data, i_var) {

  # Compute mean
  mutate_call <- lazyeval::interp(~ mean(i_var, na.rm=TRUE), i_var = as.name(i_var))
  i_data <- i_data %>%
    mutate_(.dots = setNames(list(mutate_call), "varmean"))

  # Demean
  mutate_call <- lazyeval::interp(~ i_var - varmean, i_var = as.name(i_var))
  i_data <- i_data %>% mutate_(.dots = setNames(list(mutate_call), i_var))

  # Remove variable mean
  i_data <- i_data %>%
    select(-varmean)

  return(i_data)
}
