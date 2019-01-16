if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(boot,dplyr,StableEstim)

wrapper_nonparametric_Levy_fun_QT <- function(dat, indices) {
  dat2 <- dat[indices]
  res <- Levy_fun_QT(dat2)
  return(res)
}
