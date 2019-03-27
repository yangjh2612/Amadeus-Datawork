# Function to do Levy alpha stable parameter error estimates using bootstraps.

############ 0. Basic Set up ############
## 0.1. loading of required libraries

if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(boot,dplyr,StableEstim,devtools)

## 0.2 loading fitting functions from fitting.levy package
devtools::load_all("fittinglevy")

## 0.3 remaining function definitions: 
# the fuction for the fitting result
fun_fit_levy <- function(dat, bin_num, cond_ind, var_ind, c_names, cut_num, neg_cut, pov_cut) { # the function takes 8 arguments: 1) data generated and cleaned in section 0.2, 2) the number of bins, 3) the index of the variable that is used for the conditional class, 4) the target variable name, 5) the name of class, 6) the minimum number of observations for each class,  7) the cutting point on the left tail, 8) the cutting point on the right tail
  result_list <- list()
  
  c_uni_list <- list()
  c_uni_num_list <- list()
  for (k in 1:length(dat)) {
    print(k)
  
    zz <- dat[[k]] %>%
      select(IDNR, Year, COMPCAT, NACE_CAT, LP, LP_diff, EMPL) %>% # Firm ID, Year, Firm Size, Industry ID, Labor Produtivity, Labor Productivity Change, Employment
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
      c_uni_name <- c_names[c_uni_num] # record the class names
      
      c_list <- list()
      for (c in 1:length(c_uni_name)) {
        print(paste(length(c_uni), c))
        c_lp <- zz$Var[zz$Cond == c_uni_name[c]] # for each class

        
        levy_result <- levy_fitting(dat_t = c_lp, bin_num = bin_num, include_bootstrap=TRUE) # Levy estimation
        
            
        c_list[[c]] <- list(levy_para = levy_result$levy_para, levy_soofi = levy_result$levy_soofi, est_levy_std_error = levy_result$est_levy_std_error, data_mid = levy_result$data_mid , data_p = levy_result$data_p, levy_q = levy_result$levy_q)
      }
      c_uni_list[[k]] <- c_uni_name # record the ordered name of unique class
      #c_uni_list_2[[k]] <- c_uni_name_2 #
      c_uni_num_list[[k]] <- c_uni_num # record the ordered numeric name of unique class
      result_list[[k]] <- c_list # record the result from "fun_info_gen"
    }
  }
  all_list <- list(result_list, c_uni_list, c_uni_num_list)
  return(all_list)
}


# main entry point

##  1.1. loading of required data and cleaning
load("All_list_Cleaned_cut.Rda") ## load the data file created from "Productivity_Analysis_Data.Rmd"
load("Labels.Rda")



############ 2. Fit the Levy distribution  ############
## note the following index
# con_ind: 2: year, 3: size, 4: industry
# var_ind: 5: LP, 6: LP_change, 7: TFP growth

## set up the cut-off point
neg_cut <- 0.0025 # negative cut-off point
pov_cut <- 0.9975 # positive cut-off point


## Year class
# LP conditional on year (year class)
LP_year_Levy_list_boot <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "Year", var_ind = "LP", c_names = year_names, cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut)

# LP_change conditional on year
LP_Change_year_Levy_list_boot <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "Year", var_ind = "LP_diff", c_names = year_names, cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut)

save(LP_year_Levy_list_boot, LP_Change_year_Levy_list_boot, file = "Year_Levy_list_boot.Rda")

## Size class
# LP conditional on size
LP_size_Levy_list_boot <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "COMPCAT", var_ind = "LP", c_names = size_names_long, cut_num = 5000, neg_cut = neg_cut, pov_cut = pov_cut)

# LP_change conditional on size
LP_Change_size_Levy_list_boot <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "COMPCAT", var_ind = "LP_diff", c_names = size_names_long, cut_num = 5000, neg_cut = neg_cut, pov_cut = pov_cut)

save(LP_size_Levy_list_boot, LP_Change_size_Levy_list_boot, file = "Size_Levy_list_boot.Rda")

## Industry class
# LP conditional on sector
LP_ind_Levy_list_boot <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "NACE_CAT", var_ind = "LP", c_names = ind_name_table$ind_names, cut_num = 1000, neg_cut = neg_cut, pov_cut = pov_cut)

# LP_change conditional on sector
LP_Change_ind_Levy_list_boot <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "NACE_CAT", var_ind = "LP_diff", c_names = ind_name_table$ind_names, cut_num = 1000, neg_cut = neg_cut, pov_cut = pov_cut)

save(LP_ind_Levy_list_boot, LP_Change_ind_Levy_list_boot, file = "Industry_Levy_list_boot.Rda")

