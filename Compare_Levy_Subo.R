# Script to perform cross-validation with Levy-alpha stable fits and AEP/Subbotin fits in comparison

############ 0. Basic Set up ############
## 0.1. loading of required libraries
if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(dplyr,StableEstim,lmomco,devtools)

## 0.2 loading local packages
# functions from cross.validation package
devtools::load_all("crossvalidation")

load("All_list_Cleaned_cut.Rda")
load("Labels.Rda")
## 0.3. Setting of basic functions

# the fuction for the fitting result
fun_CV <- function(dat, bin_num, cond_ind, var_ind, c_names, cut_num, neg_cut, pov_cut) { # the function takes 8 arguments: 1) data generated and cleaned in section 0.2, 2) the number of bins, 3) the index of the variable that is used as the conditional class, 4) the index for target variable, 5) the name of class, 6) the minimum number of observations for each class,  7) the cutting point on the left tail, 8) the cutting point on the right tail
  result_list <- list()
  
  c_uni_list <- list()
  c_uni_num_list <- list()
  for (k in 1:length(dat)) {
    print(k)
    
    zz <- dat[[k]] %>%
      select(IDNR, Year, COMPCAT, NACE_CAT, LP, LP_diff, EMPL) %>% # Firm ID, Year, Firm Size, Industry ID, Labor Produtivity, Labor Productivity Growth, TFP Growth, Employment
      filter(EMPL > 1) %>% # remove self-employed persons
      mutate(LP = LP / 1000) %>% # change the unit scale of the labor productivity by dividing it by 1000
      mutate(LP_diff = LP_diff / 1000) # percentage unit for the growth variables
    
    zz <- as.data.frame(zz)
    
    zz$Cond <- zz[, cond_ind] # create a new column of the class variable
    zz$Var <- zz[, var_ind] # create a new column of the value variable
    
    
    zz <- zz %>%
      select(IDNR, Year, Var, Cond) %>%
      na.omit() %>%
      filter(Var > quantile(Var, neg_cut) & Var < quantile(Var, pov_cut)) %>% # cut the tail
      group_by(Cond) %>%
      filter(length(IDNR) > cut_num) # set the minimum number of obs for each class
    
    
    if (nrow(zz) == 0) {
      result_list[[k]] <- NA
    } else {
      c_uni <- unique(zz$Cond) # unique class 
      
      c_uni_name <- c()
      c_uni_num <- c() 
      
      for (i in 1:length(c_uni)) {
        c_uni_num[i] <- which(c_names %in% c_uni[i])
      }
      
      c_uni_num <- sort(c_uni_num)
      c_uni_name <- c_names[c_uni_num]
      
      c_list <- list()
      for (c in 1:length(c_uni_name)) {
        print(paste(k, "-", country_names[[k]], ":", c, "out of", length(c_uni)))
        c_lp <- zz$Var[zz$Cond == c_uni_name[c]] # for each class
        
        cv_levy <- CV_fun(n_fold = 10, n_rep = 10, uni_data = c_lp, distribution = "Levy") # Cross validation function
        cv_AEP <- CV_fun(n_fold = 10, n_rep = 10, uni_data = c_lp, distribution = "AEP") # Cross validation function
        c_list[[c]] <- list(cv_levy, cv_AEP) # Cross validation function
        
      }
      c_uni_list[[k]] <- c_uni_name # record the ordered name of unique class
      c_uni_num_list[[k]] <- c_uni_num # record the ordered numeric name of unique class
      result_list[[k]] <- c_list # record the result from "fun_info_gen"
    }
  }
  all_list <- list(result_list, c_uni_list, c_uni_num_list)
  return(all_list)
}

# main entry point


##  0.4. loading of required data and cleaning          # TODO: Since this is duplicated in many scripts, this should sit in an external package
# 0.4.0 Load the data file created from "Productivity_Analysis_Data.Rmd"



############ 1. Fit the Levy distribution  ############
## note the following index
# con_ind: 2: year, 3: size, 4: industry
# var_ind: 5: LP, 6: LP_change, 7: TFP growth

## set up the cut-off point
neg_cut <- 0.005 # negative cut-off point
pov_cut <- 0.995 # positive cut-off point


## Year class
# LP conditional on year (year class)
LP_year_list_compare <- fun_CV(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "Year", var_ind = "LP", c_names = year_names, cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut)

# LP_change conditional on year
LP_Change_year_list_compare <- fun_CV(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "Year", var_ind = "LP_diff", c_names = year_names, cut_num = 100000, neg_cut = neg_cut, pov_cut = pov_cut)

#setwd("~/Desktop/Cleaned Rda/Productivity")
save(LP_year_list_compare ,  LP_Change_year_list_compare,  file = "Year_list_compare.Rda")



## Size class
# LP conditional on size
#LP_size_list_compare <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = 3, var_ind = 5, c_names = size_names, cut_num = 5000, neg_cut = neg_cut, pov_cut = pov_cut)

# LP_change conditional on size
#LP_g_size_list_compare <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = 3, var_ind = 6, c_names = size_names, cut_num = 5000, neg_cut = neg_cut, pov_cut = pov_cut)

# Zeta  conditional on size
#Zeta_g_size_list_compare <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = 3, var_ind = 7, c_names = size_names, cut_num = 5000, neg_cut = neg_cut, pov_cut = pov_cut)

#save(LP_size_list_compare , LP_g_size_list_compare, Zeta_g_size_list_compare, file = "Size_list_compare.Rda")

## Industry class
# LP conditional on sector
#LP_ind_list_compare <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = 4, var_ind = 5, c_names = ind_name_table$ind_names, cut_num = 1000, neg_cut = neg_cut, pov_cut = pov_cut)

# LP_change conditional on sector
#LP_g_ind_list_compare <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = 4, var_ind = 6, c_names = ind_name_table$ind_names, cut_num = 1000, neg_cut = neg_cut, pov_cut = pov_cut)

# Zeta conditional on sector
#Zeta_g_ind_list_compare <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = 4, var_ind = 7, c_names = ind_name_table$ind_names, cut_num = 1000, neg_cut = neg_cut, pov_cut = pov_cut)

#save(LP_ind_list_compare, LP_g_ind_list_compare, Zeta_g_ind_list_compare, file = "Industry_list_compare.Rda")
### Save the result
# setwd("~/Desktop/Cleaned Rda/Productivity")
# save(LP_year_Levy_list, LP_g_year_Levy_list, Zeta_g_year_Levy_list, LP_size_Levy_list, LP_g_size_Levy_list, Zeta_g_size_Levy_list, LP_ind_Levy_list, LP_g_ind_Levy_list, Zeta_g_ind_Levy_list, file = "Levy_list.Rda")

############ 2. Soofi and AIC ############

# Fittig function for the levy
fun_info_gen <- function(dat_t, bin_num) { # two arguments: 1) data and 2) the bin number
  p_data <- dat_t 
  est_levy_qt <-  Levy_fun_QT(p_data)
  est_sub_lm  <- Sub_fun_LM(p_data)
  
  p_data_h <- hist(p_data, plot = F, breaks = seq(min(p_data),max(p_data),l= bin_num))
  
  obs_mid <- p_data_h$mids
  obs_p <- p_data_h$counts/sum(p_data_h$counts)
  
  pred_p_levy_b <- dstable(obs_mid, est_levy_qt[1], est_levy_qt[2], est_levy_qt[3], est_levy_qt[4])
  
  pred_p_sub_b <- pdfaep4(obs_mid, est_sub_lm)
  
  
  
  #pred_p_sub_b <- dSEP1(obs_mid, mu =  est_sub_ml[1], sigma =  est_sub_ml[2], nu =  est_sub_ml[3], tau = est_sub_ml[4])
  
  
  #hist(rlmomco(100000, vec2par( est_sub_lm$para, type="aep4")))
  
  pred_p_levy <- pred_p_levy_b/sum(pred_p_levy_b)
  pred_p_sub <- pred_p_sub_b/sum(pred_p_sub_b)
  
  levy_soofi <- 1 - soofi_gen(obs_p, pred_p_levy)
  sub_soofi <- 1 - soofi_gen(obs_p, pred_p_sub)
  
  levy_aic <- 2*4 - 2*sum(log(pred_p_levy_b))
  sub_aic <- 2*4 - 2*sum(log(pred_p_sub_b))
  
  
  ok_list <- list(data_p = obs_p, levy_q = pred_p_levy, sub_q = pred_p_sub, levy_para = est_levy_qt, sub_info = est_sub_lm, sub_para = est_sub_lm[[2]], levy_soofi = round(levy_soofi,4)*100, sub_soofi = round(sub_soofi,4)*100, levy_aid = round(levy_aic,0), soofi_aic = round(sub_aic,0))
  
  return(ok_list)
}


# the fuction for the fitting result
fun_AIC_SOOFI <- function(dat, bin_num, cond_ind, var_ind, c_names, cut_num, neg_cut, pov_cut) { # the function takes 8 arguments: 1) data generated and cleaned in section 0.2, 2) the number of bins, 3) the index of the variable that is used as the conditional class, 4) the index for target variable, 5) the name of class, 6) the minimum number of observations for each class,  7) the cutting point on the left tail, 8) the cutting point on the right tail
  result_list <- list()
  
  c_uni_list <- list()
  c_uni_num_list <- list()
  for (k in 1:length(dat)) {
    print(k)
    
    zz <- dat[[k]] %>%
      select(IDNR, Year, COMPCAT, NACE_CAT, LP, LP_diff, EMPL) %>% # Firm ID, Year, Firm Size, Industry ID, Labor Produtivity, Labor Productivity Growth, TFP Growth, Employment
      filter(EMPL > 1) %>% # remove self-employed persons
      mutate(LP = LP / 1000) %>% # change the unit scale of the labor productivity by dividing it by 1000
      mutate(LP_diff = LP_diff / 1000) # percentage unit for the growth variables
    
    zz <- as.data.frame(zz)
    
    zz$Cond <- zz[, cond_ind] # create a new column of the class variable
    zz$Var <- zz[, var_ind] # create a new column of the value variable
    
    
    zz <- zz %>%
      select(IDNR, Year, Var, Cond) %>%
      na.omit() %>%
      filter(Var > quantile(Var, neg_cut) & Var < quantile(Var, pov_cut)) %>% # cut the tail
      group_by(Cond) %>%
      filter(length(IDNR) > cut_num) # set the minimum number of obs for each class
    
    
    if (nrow(zz) == 0) {
      result_list[[k]] <- NA
    } else {
      c_uni <- unique(zz$Cond) # unique class 
      
      c_uni_name <- c()
      c_uni_num <- c() 
      
      for (i in 1:length(c_uni)) {
        c_uni_num[i] <- which(c_names %in% c_uni[i])
      }
      
      c_uni_num <- sort(c_uni_num)
      c_uni_name <- c_names[c_uni_num]
      
      c_list <- list()
      for (c in 1:length(c_uni_name)) {
        print(paste(length(c_uni), c))
        c_lp <- zz$Var[zz$Cond == c_uni_name[c]] # for each class
        
        c_list[[c]] <- fun_info_gen(dat_t = c_lp, bin_num = bin_num) # Levy estimation
      }
      c_uni_list[[k]] <- c_uni_name # record the ordered name of unique class
      c_uni_num_list[[k]] <- c_uni_num # record the ordered numeric name of unique class
      result_list[[k]] <- c_list # record the result from "fun_info_gen"
    }
  }
  all_list <- list(result_list, c_uni_list, c_uni_num_list)
  return(all_list)
}


# con_ind: 2: year, 3: size, 4: industry
# var_ind: 5: LP, 6: LP_change, 7: TFP growth

## Year class
# LP conditional on year (year class)
LP_year_list_compare <- fun_AIC_SOOFI(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "Year", var_ind = "LP", c_names = year_names, cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut)

# LP_change conditional on year
LP_g_year_list_compare <- fun_AIC_SOOFI(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "Year", var_ind = "LP_diff", c_names = year_names, cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut)

setwd("~/Desktop/Cleaned Rda/Productivity")
save(LP_year_list_compare , LP_g_year_list_compare, file = "Year_list_compare.Rda")

### Save the result
# setwd("~/Desktop/Cleaned Rda/Productivity")
# save(LP_year_Levy_list, LP_g_year_Levy_list, Zeta_g_year_Levy_list, LP_size_Levy_list, LP_g_size_Levy_list, Zeta_g_size_Levy_list, LP_ind_Levy_list, LP_g_ind_Levy_list, Zeta_g_ind_Levy_list, file = "Levy_list.Rda")

############ 2. Summary table ############
#### averaged over country
arr_fun <- function(dat_r){
  f_frame <- list()
  for(k in 1:15){
    levy_soofi <- mean(unlist(lapply(dat_r[[1]][[k]], function(x) x$levy_soofi)))
    sub_soofi <- mean(unlist(lapply(dat_r[[1]][[k]], function(x) x$sub_soofi)))
    
    levy_aic <- mean(unlist(lapply(dat_r[[1]][[k]], function(x) x$levy_aid)))
    sub_aic <- mean(unlist(lapply(dat_r[[1]][[k]], function(x) x$soofi_aic)))
    
    
    soofi_diff <- levy_soofi - sub_soofi
    aic_diff <- levy_aic-sub_aic
    
    f_frame[[k]] <- data.frame(Empty = "",Country = country_names[[k]],  levy_soofi = levy_soofi, sub_soofi = sub_soofi, soofi_diff = soofi_diff, levy_aic = levy_aic, sub_aic = sub_aic, aic_diff = aic_diff)
  }
  
  f_frame <- do.call("rbind", f_frame)
  
  return(f_frame)
  
}

### country-year

LP_year_comp <- arr_fun(LP_year_list_compare)
LP_g_year_comp <- arr_fun(LP_g_year_list_compare)


## 
library(xtable)
print(xtable(LP_year_comp, digits = c(0,0,0,2,2,2,1,1,1,3,3,3)),  include.rownames=FALSE)
print(xtable(LP_g_year_comp, digits = c(0,0,0,2,2,2,1,1,1,3,3,3)),  include.rownames=FALSE)



