

############ 0. Basic Set up ############
## 0.1. loading of required libraries

if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(boot)

library(boot)
library(dplyr)
library(StableEstim) # main package for the estimation of the Levy stable distributon


##  0.2. loading of required data and cleaning
load("All_list_Cleaned.Rda") ## load the data file created from "Productivity_Analysis_Data.Rmd"

country_names <- c(
  "Albania", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina",
  "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia",
  "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland",
  "Italy", "Kosovo", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg",
  "Macedonia, FYR", "Malta", "Monaco", "Montenegro", "Netherlands", "Norway",
  "Poland", "Portugal", "Moldova", "Romania", "Russian Federation", "Serbia",
  "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "Ukraine",
  "United Kingdom"
)

country_names_five <- c("France", "Germany", "Italy", "Spain", "United Kingdom") # name of

# Cleaning
All_list_Cleaned_cut <- list()
for (k in 1:length(All_list_Cleaned)) {
  print(k)
  All_list_Cleaned_cut[[k]] <- All_list_Cleaned[[k]] %>%
    select(IDNR, Year, COMPCAT, NACE_CAT, LP, LP_g, EMPL, VA, Zeta) %>% # Firm ID, Year, Firm Size, Industry ID, Labor Produtivity, Labor Productivity Growth, Employment, Value added, TFP Growth
    mutate(COMPCAT_one = substr(COMPCAT, 1, 1)) %>% # the first letter of the size variable
    group_by(Year) %>%
    filter(length(IDNR) > 10000) %>% # Note that 23 countries out of 44 do not have VA info. Out of 21 countries, 15 countries have enough info for anlaysis (more than 5 years with 10,000 firms )
    group_by()
}

# Countries with more than 5 year obs with 10,000 firms
take_this <- which(unlist(lapply(All_list_Cleaned_cut, function(x) length(unique(x$Year)) > 5)))

All_list_Cleaned_cut <- All_list_Cleaned_cut[take_this]
country_names <- country_names[take_this]

rm(All_list_Cleaned)

## 0.3. Setting the class names: Year, Size, Industry,
# industry index

year_names <- c(2006:2015) 

ind_name_table <- data.frame(ind_names = unique(All_list_Cleaned_cut[[15]]$NACE_CAT), 
                             ind_names_alphabet = c("J", "L", "G", "C", "I", "H", "S", "P", "M", "K", "B", "N", "A", "Q", "R", "F", "E", "O", "D", "T"))

ind_name_table <- ind_name_table[order(ind_name_table$ind_names_alphabet),]

size_names <- c("S", "M", "L", "V") # size index
names(size_names) <- 1:4 # create the numerical name for the size index


## 0.4. Setting of basic functions
# Levy_fun_QT is the main function to estimate the Levy alpha stable distribution
Levy_fun_QT <- function(x) {
  ok <- McCullochParametersEstim(x)
  return(ok)
}

Levy_fun_GMM <- function(x) {
  pm <- 0
  regularization = "cut-off"
  WeightingMatrix = "OptAsym"
  alphaReg = 0.005
  algo = "2SGMM"
  
  t_scheme = "free"
  t_seq = seq(0.1, 2, length.out = 12)
  GMM_1 <- GMMParametersEstim(x = x,
                              algo = algo, alphaReg = alphaReg,
                              regularization = regularization,
                              WeightingMatrix = WeightingMatrix,
                              t_scheme = t_scheme,
                              pm = pm, PrintTime = TRUE, t_free = t_seq)
  
}
# Kullback–Leibler divergence
kl_fun <- function(p, q) {
  ifelse(p == 0, 0, p * log(p / q))
}

# Soofi ID
soofi_gen <- function(pred_m, obs_m) {
  soofi <- 1 - exp(-sum(kl_fun(pred_m, obs_m)))

  return(soofi)
}


get_stabledist_variates <- function(dat, est_levy_qt) {
  variates <- stabledist::rstable(n=length(dat), alpha=est_levy_qt[[1]], 
                                  beta=est_levy_qt[[2]], gamma=est_levy_qt[[3]], 
                                  delta=est_levy_qt[[4]])
  return(variates)
}

wrapper_nonparametric_Levy_fun_QT <- function(dat, indices) {
  dat2 <- dat[indices]
  res <- Levy_fun_QT(dat2)
  return(res)
}


# Fittig function for the levy
fun_info_gen <- function(dat_t, bin_num) { # two arguments: 1) data and 2) the bin number
  p_data <- dat_t
  est_levy_qt <- Levy_fun_QT(p_data) # Levy estimation with QT
  #est_levy_gmm <- Levy_fun_GMM(p_data)
  
  # bootstrapping
  est_levy_qt_std_error <- boot(p_data, wrapper_nonparametric_Levy_fun_QT, R=1000, sim="ordinary", parallel="multicore", ncpus=3)
  ## Note: if this does not work on windows, try with snow instead of multicore
  #est_levy_qt_std_error <- boot(p_data, wrapper_nonparametric_Levy_fun_QT, R=100, sim="ordinary", parallel="snow", ncpus=3)
  ## ... if this does also not work, try without parallelization. This will take multiple times longer.
  #est_levy_qt_std_error <- boot(p_data, wrapper_nonparametric_Levy_fun_QT, R=100, sim="ordinary")
  
  #est_levy_qt_np_std_error <- boot(p_data, Levy_fun_QT, R=1000, sim="parametric", ran.gen=get_stabledist_variates, mle=est_levy_qt, parallel="multicore", ncpus=3)
  

  p_data_h <- hist(p_data, plot = F, breaks = seq(min(p_data), max(p_data), l = bin_num)) # binning the data

  
  obs_mid <- p_data_h$mids # location of the bin
  obs_p <- p_data_h$counts / sum(p_data_h$counts) # normalized counts of the bin: the normalized empirical density

  pred_p_levy_b <- dstable(obs_mid, est_levy_qt[1], est_levy_qt[2], est_levy_qt[3], est_levy_qt[4]) # the predicted density from the Levy model given the estimated parameters
  
  #pred_p_levy_b <- dstable(obs_mid, est_levy_gmm$Estim$par[1], est_levy_gmm$Estim$par[2], est_levy_gmm$Estim$par[3], est_levy_gmm$Estim$par[4]) # the predicted density from the Levy model given the estimated parameters
  
  pred_p_levy <- pred_p_levy_b / sum(pred_p_levy_b) # normalized density

  levy_soofi <- 1 - soofi_gen(obs_p, pred_p_levy) # soofi index


  ok_list <- list(levy_para = est_levy_qt, levy_soofi = round(levy_soofi, 4) * 100, est_levy_qt_std_error = est_levy_qt_std_error)
  #ok_list <- list(raw_data = p_data, data_mid = obs_mid, data_p = obs_p, levy_q = pred_p_levy, levy_para = est_levy_gmm, levy_soofi = round(levy_soofi, 4) * 100)
  
  return(ok_list)
}


# the fuction for the fitting result
fun_fit_levy <- function(dat, bin_num, cond_ind, var_ind, c_names, cut_num, neg_cut, pov_cut) { # the function takes 8 arguments: 1) data generated and cleaned in section 0.2, 2) the number of bins, 3) the index of the variable that is used as the conditional class, 4) the index for target variable, 5) the name of class, 6) the minimum number of observations for each class,  7) the cutting point on the left tail, 8) the cutting point on the right tail
  result_list <- list()
  
  c_uni_list <- list()
  c_uni_list_2 <- list()
  c_uni_num_list <- list()
  for (k in 1:length(dat)) {
    print(k)


    zz <- dat[[k]] %>%
      select(IDNR, Year, COMPCAT_one, NACE_CAT, LP, LP_g, Zeta, EMPL) %>% # Firm ID, Year, Firm Size, Industry ID, Labor Produtivity, Labor Productivity Growth, TFP Growth, Employment
      filter(EMPL > 1) %>% # remove self-employed persons
      mutate(LP = LP / 1000) %>% # change the unit scale of the labor productivity by dividing it by 1000
      mutate(LP_g = LP_g * 100) # percentage unit for the growth variables

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
      c_uni <- unique(zz$Cond) # unique classes

      c_uni_name <- c()
      c_uni_num <- c() 
      c_uni_name_2 <- c()
      
      for (i in 1:length(c_uni)) {
        c_uni_num[i] <- which(c_names %in% c_uni[i])
      }
      
      c_uni_num <- sort(c_uni_num)
      c_uni_name <- c_names[c_uni_num]
      
      if(cond_ind == 4){
        c_uni_name_2 <- ind_name_table$ind_names_alphabet[c_uni_num]
      } else{
        c_uni_name_2 <- c_uni_name
      }
      
      c_list <- list()
      for (c in 1:length(c_uni_name)) {
        print(paste(length(c_uni), c))
        c_lp <- zz$Var[zz$Cond == c_uni_name[c]] # for each class

        c_list[[c]] <- fun_info_gen(dat_t = c_lp, bin_num = bin_num) # Levy estimation
      }
      c_uni_list[[k]] <- c_uni_name # record the ordered name of unique class
      c_uni_list_2[[k]] <- c_uni_name_2 #
      c_uni_num_list[[k]] <- c_uni_num # record the ordered numeric name of unique class
      result_list[[k]] <- c_list # record the result from "fun_info_gen"
    }
  }
  all_list <- list(result_list, c_uni_list, c_uni_num_list, c_uni_list_2)
  return(all_list)
}


############ 1. Fit the Levy distribution  ############
## note the following index
# con_ind: 2: year, 3: size, 4: industry
# var_ind: 5: LP, 6: LP_change, 7: TFP growth

## set up the cut-off point
neg_cut <- 0.0025 # negative cut-off point
pov_cut <- 0.9975 # positive cut-off point


## Year class
# LP conditional on year (year class)
LP_year_Levy_list_boot <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = 2, var_ind = 5, c_names = year_names, cut_num = 0, neg_cut = neg_cut, pov_cut = pov_cut)

# LP_change conditional on year
LP_g_year_Levy_list_boot <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = 2, var_ind = 6, c_names = year_names, cut_num = 0, neg_cut = neg_cut, pov_cut = pov_cut)

# Zeta  conditional on year
Zeta_g_year_Levy_list_boot <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = 2, var_ind = 7, c_names = year_names, cut_num = 0, neg_cut = neg_cut, pov_cut = pov_cut)

setwd("~/Desktop/Cleaned Rda/Productivity")
save(LP_year_Levy_list_boot, LP_g_year_Levy_list_boot, Zeta_g_year_Levy_list_boot, file = "Year_Levy_list_boot.Rda")
## Size class
# LP conditional on size
LP_size_Levy_list_boot <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = 3, var_ind = 5, c_names = size_names, cut_num = 5000, neg_cut = neg_cut, pov_cut = pov_cut)

# LP_change conditional on size
LP_g_size_Levy_list_boot <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = 3, var_ind = 6, c_names = size_names, cut_num = 5000, neg_cut = neg_cut, pov_cut = pov_cut)

# Zeta  conditional on size
Zeta_g_size_Levy_list_boot <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = 3, var_ind = 7, c_names = size_names, cut_num = 5000, neg_cut = neg_cut, pov_cut = pov_cut)

save(LP_size_Levy_list_boot, LP_g_size_Levy_list_boot, Zeta_g_size_Levy_list_boot, file = "Size_Levy_list_boot.Rda")

## Industry class
# LP conditional on sector
LP_ind_Levy_list_boot <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = 4, var_ind = 5, c_names = ind_name_table$ind_names, cut_num = 1000, neg_cut = neg_cut, pov_cut = pov_cut)

# LP_change conditional on sector
LP_g_ind_Levy_list_boot <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = 4, var_ind = 6, c_names = ind_name_table$ind_names, cut_num = 1000, neg_cut = neg_cut, pov_cut = pov_cut)

# Zeta conditional on sector
Zeta_g_ind_Levy_list_boot <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = 4, var_ind = 7, c_names = ind_name_table$ind_names, cut_num = 1000, neg_cut = neg_cut, pov_cut = pov_cut)

save(LP_ind_Levy_list_boot, LP_g_ind_Levy_list_boot, Zeta_g_ind_Levy_list_boot, file = "Industry_Levy_list_boot.Rda")
### Save the result
# setwd("~/Desktop/Cleaned Rda/Productivity")
# save(LP_year_Levy_list, LP_g_year_Levy_list, Zeta_g_year_Levy_list, LP_size_Levy_list, LP_g_size_Levy_list, Zeta_g_size_Levy_list, LP_ind_Levy_list, LP_g_ind_Levy_list, Zeta_g_ind_Levy_list, file = "Levy_list.Rda")


############ 2. Summary Statistics  ############