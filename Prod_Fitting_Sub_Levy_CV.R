# Script to perform cross-validation with Levy-alpha stable fits and AEP/Subbotin fits in comparison

############ 0. Basic Set up ############
## 0.1. loading of required libraries
if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(dplyr,StableEstim,lmomco,devtools)

## 0.2 loading local packages
# functions from cross.validation package
devtools::load_all("crossvalidation")

## 0.3. Setting of basic functions

# the fuction for the fitting result
fun_fit_levy <- function(dat, bin_num, cond_ind, var_ind, c_names, cut_num, neg_cut, pov_cut) { # the function takes 8 arguments: 1) data generated and cleaned in section 0.2, 2) the number of bins, 3) the index of the variable that is used as the conditional class, 4) the index for target variable, 5) the name of class, 6) the minimum number of observations for each class,  7) the cutting point on the left tail, 8) the cutting point on the right tail
  result_list <- list()
  
  c_uni_list <- list()
  c_uni_list_2 <- list()
  c_uni_num_list <- list()
  for (k in 1:length(dat)) {

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
        print(paste(k, "-", country_names[[k]], ":", c, "out of", length(c_uni)))
        c_lp <- zz$Var[zz$Cond == c_uni_name[c]] # for each class

        cv_levy <- CV_fun(n_fold = 10, n_rep = 10, uni_data = c_lp, distribution = "Levy") # Cross validation function
        cv_AEP <- CV_fun(n_fold = 10, n_rep = 10, uni_data = c_lp, distribution = "AEP") # Cross validation function
        c_list[[c]] <- list(cv_levy, cv_AEP) # Cross validation function
        
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

# main entry point


##  0.4. loading of required data and cleaning          # TODO: Since this is duplicated in many scripts, this should sit in an external package
# 0.4.0 Load the data file created from "Productivity_Analysis_Data.Rmd"
load("All_list_Cleaned.Rda")

# 0.4.1 Set list of country names
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

# 0.4.2 Cleaning
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

# 0.4.3 Countries with more than 5 year obs with 10,000 firms
take_this <- which(unlist(lapply(All_list_Cleaned_cut, function(x) length(unique(x$Year)) > 5)))

All_list_Cleaned_cut <- All_list_Cleaned_cut[take_this]
country_names <- country_names[take_this]

rm(All_list_Cleaned)

## 0.4.4 Setting the class names: Year, Size, Industry,
# industry index

year_names <- c(2006:2015) 

ind_name_table <- data.frame(ind_names = unique(All_list_Cleaned_cut[[15]]$NACE_CAT), 
                             ind_names_alphabet = c("J", "L", "G", "C", "I", "H", "S", "P", "M", "K", "B", "N", "A", "Q", "R", "F", "E", "O", "D", "T"))

ind_name_table <- ind_name_table[order(ind_name_table$ind_names_alphabet),]

size_names <- c("S", "M", "L", "V") # size index
names(size_names) <- 1:4 # create the numerical name for the size index

############ 1. Fit the Levy distribution  ############
## note the following index
# con_ind: 2: year, 3: size, 4: industry
# var_ind: 5: LP, 6: LP_change, 7: TFP growth

## set up the cut-off point
neg_cut <- 0.0025 # negative cut-off point
pov_cut <- 0.9975 # positive cut-off point


## Year class
# LP conditional on year (year class)
LP_year_list_compare <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = 2, var_ind = 5, c_names = year_names, cut_num = 0, neg_cut = neg_cut, pov_cut = pov_cut)

# LP_change conditional on year
LP_g_year_list_compare <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = 2, var_ind = 6, c_names = year_names, cut_num = 0, neg_cut = neg_cut, pov_cut = pov_cut)

# Zeta  conditional on year
Zeta_g_year_list_compare <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = 2, var_ind = 7, c_names = year_names, cut_num = 0, neg_cut = neg_cut, pov_cut = pov_cut)

#setwd("~/Desktop/Cleaned Rda/Productivity")
save( LP_g_year_list_compare, Zeta_g_year_list_compare, file = "Year_list_compare.Rda")



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

############ 2. Summary table ############
