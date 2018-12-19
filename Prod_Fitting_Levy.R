## ------------------------------------------------------------------------

load("All_list_Cleaned_cut.Rda")

year_names <- c(2006:2015)


## ------------------------------------------------------------------------
# Levy Set up=
library(dplyr)
library(StableEstim) # main package for Levy
library(ald)
library(cubfits)
library(stabledist)
library(lattice)
library(normalp)

#Levy_fun_QT is the main function to use 
Levy_fun_QT <- function(x){
  ok <- McCullochParametersEstim(x)
  return(ok)
}


Levy_fun_ML <- function(x){
  ok <- MLParametersEstim(x = x, PrintTime = TRUE)
  return(ok)
}



Levy_fun_RG <- function(x){
  ok <- KoutParametersEstim(x = x, spacing = "Kout")
  return(ok)
}


## ------------------------------------------------------------------------
## model selection: AIC and Soofi

kl_fun <- function(p,q){
  ifelse(p == 0, 0, p*log(p/q)) 
  }
  
soofi_gen <- function(pred_m, obs_m){
  soofi <- 1-exp(-sum(kl_fun(pred_m,obs_m)))
  
  return(soofi)
}  


## ------------------------------------------------------------------------
fun_info_gen <- function(dat_t, bin_num){

p_data <- dat_t  
est_levy_qt <-  Levy_fun_QT(p_data) # Levy estimation 

p_data_h <- hist(p_data, plot = F, breaks = seq(min(p_data),max(p_data),l= bin_num)) # binning the data 

obs_mid <- p_data_h$mids # location of the bin
obs_p <- p_data_h$counts/sum(p_data_h$counts) # normalized counts of the bin: the normalized empirical density

pred_p_levy_b <- dstable(obs_mid, est_levy_qt[1], est_levy_qt[2], est_levy_qt[3], est_levy_qt[4]) # the predicted density from the Levy model given the estimated parameters

pred_p_levy <- pred_p_levy_b/sum(pred_p_levy_b) # normalized density 

levy_soofi <- 1 - soofi_gen(obs_p, pred_p_levy) # soofi index 


ok_list <- list(raw_data = p_data, data_mid = obs_mid, data_p = obs_p, levy_q = pred_p_levy, levy_para = est_levy_qt, levy_soofi = round(levy_soofi,4)*100)

return(ok_list)

}


## ------------------------------------------------------------------------

fun_fit_levy <- function(dat, bin_num, cond_ind, var_ind, c_names, cut_num, neg_cut, pov_cut){

result_list <- list(); c_uni_list <- list(); c_uni_num_list <- list()
for(k in 1:length(dat)){
  print(k)
  

  zz <- dat[[k]]%>%
    select(IDNR, Year, COMPCAT_one, NACE_CAT, LP, LP_g, Zeta, EMPL) %>%
    filter(EMPL > 1) %>%
    mutate(LP = LP/1000)%>%
    mutate(LP_g = LP_g * 100)
  
  zz <- as.data.frame(zz)
  
  zz$Cond <- zz[, cond_ind] # creating a new column of the class variable for convenience
  zz$Var <- zz[, var_ind] # creating a new column of the value variable for convenience

  
  zz <- zz %>%
    select(IDNR, Year, Var, Cond) %>%
    na.omit() %>%
    filter(Var > quantile(Var, neg_cut) & Var < quantile(Var, pov_cut)) %>% # cut the tail 
    group_by(Cond) %>%
    filter(length(IDNR) > cut_num)


  if(nrow(zz) == 0){ # min obs = 1000
      result_list[[k]] <- NA
    } else{
      

  c_uni <- unique(zz$Cond)
  
  c_uni_name <- c(); c_uni_num <- c()
  if(is.numeric(c_uni)){
    c_uni_num <- sort(c_uni)
    c_uni_name <-  c_uni_num
  }else{
    for(i in 1:length(c_uni)){
      c_uni_num[i] <- which(c_names%in%c_uni[i])
    
    }
    c_uni_num <- sort(c_uni_num) # the numerical value of the unique factor in the subsample. That is the location of the unique class of the subsample in the all classes
    c_uni_name <- c_names[c_uni_num]
  }
  
  c_list <- list()
  for(c in 1:length(c_uni_name)){
  print(paste(c_uni, c))
    c_lp <- zz$Var[zz$Cond == c_uni_name[c]]
   
    c_list[[c]] <- fun_info_gen(dat_t = c_lp, bin_num = bin_num)
    
 
   }
     c_uni_list[[k]] <- c_uni_name
     result_list[[k]] <- c_list 
     c_uni_num_list[[k]] <- c_uni_num 
    }
}
all_list <- list(result_list, c_uni_list, c_uni_num_list)
return(all_list)
}


## ------------------------------------------------------------------------
#### BASE fit

# LP change

names(All_list_Cleaned_cut[[1]])

LP_year_Levy_list <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = 2, var_ind = 5, c_names = year_names, cut_num = 0, neg_cut = neg_cut, pov_cut = pov_cut)

LP_g_year_Levy_list <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = 2, var_ind = 6, c_names = year_names, cut_num = 0, neg_cut = neg_cut, pov_cut = pov_cut)

Zeta_g_year_Levy_list <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = 2, var_ind = 7, c_names = year_names, cut_num = 0, neg_cut = neg_cut, pov_cut = pov_cut)

LP_size_Levy_list <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = 3, var_ind = 5, c_names = size_names, cut_num = 5000, neg_cut = neg_cut, pov_cut = pov_cut)

LP_g_size_Levy_list <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = 3, var_ind = 6,  c_names = size_names, cut_num = 5000, neg_cut = neg_cut, pov_cut = pov_cut)

Zeta_g_size_Levy_list <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = 3, var_ind = 7,  c_names = size_names, cut_num = 5000, neg_cut = neg_cut, pov_cut = pov_cut)

LP_ind_Levy_list <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100,  cond_ind = 4, var_ind = 5, c_names = ind_names, cut_num = 5000, neg_cut = neg_cut, pov_cut = pov_cut)

LP_g_ind_Levy_list <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100,  cond_ind = 4, var_ind = 6, c_names = ind_names, cut_num = 5000, neg_cut = neg_cut, pov_cut = pov_cut)

Zeta_g_ind_Levy_list <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100,  cond_ind = 4, var_ind = 7, c_names = ind_names, cut_num = 5000, neg_cut = neg_cut, pov_cut = pov_cut)


setwd("~/Desktop/Cleaned Rda/Productivity")
save(LP_year_Levy_list, LP_g_year_Levy_list, Zeta_g_year_Levy_list, LP_size_Levy_list, LP_g_size_Levy_list, Zeta_g_size_Levy_list, LP_ind_Levy_list,LP_g_ind_Levy_list, Zeta_g_ind_Levy_list, file = "Levy_list.Rda")




## ------------------------------------------------------------------------
setwd("~/Desktop/Cleaned Rda/Productivity")
library(knitr)
purl("Prod_Fitting_Levy.Rmd")  

