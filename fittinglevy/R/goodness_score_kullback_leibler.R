if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(boot,dplyr,StableEstim)

# Kullback-Leibler divergence
kl_fun <- function(p, q) {
  kl_values <- ifelse(p == 0, 0, p * log(p / q))
  return(kl_values)
}
