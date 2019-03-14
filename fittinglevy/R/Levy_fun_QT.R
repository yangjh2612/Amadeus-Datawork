# Wrapper function for Levy-alpha stable estimation using McCulloch with McCullochParametersEstim from StableEstim package

if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(boot,dplyr,StableEstim)

# Levy_fun_QT is the main function to estimate the Levy alpha stable distribution
Levy_fun_QT <- function(x) {
  # Wrapper function
  # Arguments:
  #     x: numeric, dataset to be fitted
  # Returns:
  #     list of the four parameters.
  ok <- McCullochParametersEstim(x)
  return(ok)
}
