# Function for providing KL divergence as goodness of fit measure

if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(boot,dplyr,StableEstim)

# Kullback-Leibler divergence
kl_fun <- function(p, q) {
  # Arguments:
  #     p: numeric, predicted values
  #     q: numeric, sample values
  # Returns:
  #     kl divergences
  kl_values <- ifelse(p == 0, 0, p * log(p / q))
  return(kl_values)
}
