## ------------------------------------------------------------------------

load("All_list_for_prod.Rda")

library(colorspace)

colores_10 <- rainbow_hcl(10, end=300)
year_names <- c(2006:2015)


## ------------------------------------------------------------------------
# Levy Set up


library(StableEstim)


#Levy_fun_QT is the main function to use 
Levy_fun_QT <- function(x){
  ok <- McCullochParametersEstim(x)
  return(ok)
}

Levy_fun_ML <- function(x){
  ok <- MLParametersEstim(x = x, PrintTime = TRUE)
  return(ok)
}


## ------------------------------------------------------------------------
## Asymmetric Subottin set up
library(gamlss.dist)
library(gamlss)
library(lmomco)

Sub_fun_ML <- function(x){
  m1 <- gamlss(x~1, family=SEP1, n.cyc=50)
}

Sub_fun_LM <- function(x){
  kk <- lmoms(x, nmom=4)
  kk_par <- paraep4(kk, method= "DG") #"DG"
  
  return(kk_par)
}


## ------------------------------------------------------------------------
## log (by convention 0*log(0)= 0). For this, I create a new log fun
log_fun <- function(x){
  ifelse(x == 0, 0, log(x))
}

# KL
kl_fun <- function(p,q){
  ifelse(p == 0, 0, p*log_fun(p/q)) 
  }
  
# Soofi ID
soofi_gen <- function(pred_m, obs_m){
  soofi <- 1-exp(-sum(kl_fun(pred_m,obs_m)))
  
  return(soofi)
}  


## ------------------------------------------------------------------------
# Levy and Sub info generation
fun_info_gen <- function(dat_t, bin_num){

p_data <- dat_t 

# estimation result
est_levy_qt <-  Levy_fun_QT(p_data)
est_sub_lm  <- Sub_fun_LM(p_data)

p_data_h <- hist(p_data, plot = F, breaks = seq(min(p_data),max(p_data),l= bin_num))

# binned data (mid point and normalized density)
obs_mid <- p_data_h$mids
obs_p <- p_data_h$counts/sum(p_data_h$counts)

# normalized predicted density from the levy model 
pred_p_levy_b <- dstable(obs_mid, est_levy_qt[1], est_levy_qt[2], est_levy_qt[3], est_levy_qt[4])
pred_p_levy <- pred_p_levy_b/sum(pred_p_levy_b)

# normalized predicted density from the sub model 
pred_p_sub_b <- pdfaep4(obs_mid, est_sub_lm)
pred_p_sub <- pred_p_sub_b/sum(pred_p_sub_b)
##pred_p_sub_b <- dSEP1(obs_mid, mu =  est_sub_ml[1], sigma =  est_sub_ml[2], nu =  est_sub_ml[3], tau = est_sub_ml[4])

# soofi ID
levy_soofi <- 1 - soofi_gen(obs_p, pred_p_levy)
sub_soofi <- 1 - soofi_gen(obs_p, pred_p_sub)

# AIC
levy_aic <- 2*4 - sum(dstable(p_data, est_levy_qt[1], est_levy_qt[2], est_levy_qt[3], est_levy_qt[4], log = T))
sub_aic <- 2*4 - sum(log(pdfaep4(obs_mid, est_sub_lm)))


ok_list <- list(raw_data = p_data, data_mid = obs_mid, data_p = obs_p, levy_q = pred_p_levy, sub_q = pred_p_sub, levy_para = est_levy_qt, sub_info = est_sub_lm, sub_para = est_sub_lm[[2]], levy_soofi = round(levy_soofi,4)*100, sub_soofi = round(sub_soofi,4)*100, levy_aid = round(levy_aic,0), soofi_aic = round(sub_aic,0))

return(ok_list)

}


## ------------------------------------------------------------------------
### Fitting function
fun_fit <- function(dat, bin_num, var_num, cut_ind_quan, cut_neg, cut_pov){

result_list <- list()
# looping for country
for(k in 1:length(dat)){
  print(k)
  y_ind <- which(names(dat) == "Year")
  ok <- dat[[k]][,c(y_ind,var_num)]
  names(ok) <- c("Year", "Var")
  
# looping for year  
  y_list <- list()
  for(y in 1:length(unique(ok$Year))){
     print(paste("y = ", y))
    
    ok <- na.omit(ok)
    # cut_ind_quan == 1 means cutting by quantile, otherwise cut by absolute cutoff number
    if(cut_ind_quan == 1){
       ok_y <- subset(ok, Year == sort(unique(ok$Year))[y] & Var > quantile(Var, cut_neg) & Var < quantile(Var, cut_pov))
    }else{
       ok_y <- subset(ok, Year == sort(unique(ok$Year))[y] & Var > cut_neg & Var < cut_pov)
    }
    
    # The sample size should be larger than 2000
    if(nrow(ok_y) < 2000){
      y_list[[y]] <- NA
    } else{
      
    ok_y_2 <- ok_y$Var
    
    y_list[[y]] <- fun_info_gen(dat_t = ok_y_2, bin_num = bin_num)
 
    }
   
  }
  
  result_list[[k]] <- y_list
  
  }
return(result_list)
}


## ------------------------------------------------------------------------

# LP change

LP_change_list <- fun_fit(dat = All_list_for_prod, bin_num = 100, var_num = 5, cut_ind_quan = 1, cut_neg = 0.025, cut_pov = 0.975)

save(LP_change_list , file = "LP_change_list.Rda")

# CP change

CP_change_list <- fun_fit(dat = All_list_for_prod, bin_num = 100, var_num = 5, cut_ind_quan = 1, cut_neg = 0.025, cut_pov = 0.975)

save(CP_change_list , file = "CP_change_list.Rda")

# zeta

Zeta_list <- fun_fit(dat = All_list_for_prod, bin_num = 100, var_num = 5, cut_ind_quan = 1, cut_neg = 0.025, cut_pov = 0.975)


save(Zeta_list , file = "Zeta_list.Rda")


## ------------------------------------------------------------------------
library(knitr)
purl("Prod_Fitting.Rmd")  

