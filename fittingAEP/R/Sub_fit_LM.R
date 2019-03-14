# Wrapper function for 4-parameter Subbotin / AEP (symmetric exponential power) estimation using 
#    Asquith method using paraep4 from lmomco package based on L-Moments obtained from lmoms 
#    from lmomco package.

Sub_fun_LM <- function(x){
  # Wrapper function
  #  Arguments:
  #     x: list of float - data
  #  Returns:
  #     list of float of length 4 - fitted parameter values of AEP / Subbotin distribution
  
  # Compute L-Moments
  kk <- lmoms(x, nmom=4)
  # Fit parameters using Asquith method
  kk_par <- paraep4(kk, method= "A") #"DG"
  
  return(kk_par)
}
