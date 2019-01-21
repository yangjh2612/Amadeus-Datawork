if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(boot,dplyr,StableEstim)

# Soofi ID
# Soofi ID
soofi_gen <- function(pred_m, obs_m) {
  soofi <- 1 - exp(-sum(kl_fun(pred_m, obs_m)))

  return(soofi)
}
