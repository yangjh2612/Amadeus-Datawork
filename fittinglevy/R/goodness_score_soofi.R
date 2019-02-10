# Function for providing Soofi score as goodness of fit measure

if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(boot,dplyr,StableEstim)

# Soofi ID
soofi_gen <- function(pred_m, obs_m) {
  # Arguments:
  #     pred_m: numeric, predicted values
  #     obs_m: numeric, sample values
  # Returns:
  #     soofi score
  soofi <- 1 - exp(-sum(kl_fun(pred_m, obs_m)))

  return(soofi)
}
