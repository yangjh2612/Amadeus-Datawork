## ------------------------------------------------------------------------
library(colorspace)

year_names <- c(2006:2015)
country_names  <- c('Albania', 'Austria', 'Belarus', 'Belgium', 'Bosnia and Herzegovina', 'Bulgaria', 'Croatia', 'Cyprus', 'Czech Republic', 'Denmark', 'Estonia', 'Finland', 'France', 'Germany', 'Greece', 'Hungary', 'Iceland', 'Ireland', 'Italy', 'Kosovo', 'Latvia', 'Liechtenstein', 'Lithuania', 'Luxembourg', 'Macedonia, FYR', 'Malta', 'Monaco', 'Montenegro', 'Netherlands', 'Norway', 'Poland', 'Portugal', 'Moldova', 'Romania', 'Russian Federation', 'Serbia', 'Slovakia', 'Slovenia', 'Spain', 'Sweden', 'Switzerland', 'Turkey', 'Ukraine', 'United Kingdom')


## ------------------------------------------------------------------------
library(plyr)
library(dplyr)
library(FSA)
#### distribution info

fun_info_gen <- function(dat_list, var_ind, cut_ind, cut_neg, cut_pov, bin_num, log_ind){

  # take year and the target variable, and check the lenght of observations per country
  var_list <- lapply(dat_list, function(x) x[,c(2,var_ind)]) 
  var_list_na_check <- lapply(var_list, function(x) length(na.omit(x[,2]))) 
  
  # cutting some countries that do not have more than 1,000 obs for the target variable
  zero_obs <- which(unlist(var_list_na_check) < 1000) 
  var_list <- var_list[-c(zero_obs)]
  country_names_c <- country_names[-zero_obs]
  
  hist_list <- list(); year_list <- list()
  
    # looping for each country 
  for(k in 1:length(var_list)){
    
  print(k)
    
  dat_cut <- var_list[[k]]
  dat_cut <- na.omit(dat_cut)
  
## Cutting data: ind = 1 means cutting by absolute value, ind = 0 means cutting by tail percentage, ind = else means cutting the left tail by absolute value and cutting the right tail by percentage
  if(cut_ind == 1){
     dat_cut <- subset(dat_cut, dat_cut[,2] > cut_neg & dat_cut[,2] < cut_pov)
  } else if(cut_ind == 0){
     dat_cut <- subset(dat_cut, dat_cut[,2] > quantile(dat_cut[,2], c(cut_neg)) & dat_cut[,2] < quantile(dat_cut[,2], c(cut_pov)))
  } else {
    dat_cut <- subset(dat_cut, dat_cut[,2] > cut_neg & dat_cut[,2] < quantile(dat_cut[,2], c(cut_pov)))
  }
  
  
  # No obs gives NA     

  ## Looping for each year  
   dat_year <- year_names[year_names%in%c(sort(unique(dat_cut[,1])))]
    y_list <- list()
      for(j in 1:length(dat_year)){
    #print(j)
        ok_1 <- subset(dat_cut, Year == dat_year[j])[,2]
        
        # A minimum number of obs for each is is 1000 otherwise gives NA
        if(length(ok_1 ) < 1000){
           y_list[[j]]  <- NA
        } else{
          # log_ind = 1 give the histogram of the log of the targe variable 
                if(log_ind == 1){
                   y_list[[j]] <- hist(log(ok_1),breaks=seq(min(log(ok_1)),max(log(ok_1)),l=bin_num+1))
                   
                } else{
                   
                    y_list[[j]] <- hist(ok_1,breaks=seq(min(ok_1),max(ok_1),l=bin_num+1))
                }
           
             }
          }
    
    hist_list[[k]]  <- y_list
    year_list[[k]] <- dat_year
      }
    
hist_final <- list(hist_list, year_list, country_names_c)

return(hist_final)
}


## ------------------------------------------------------------------------
## change in the cut 
fun_info_gen_change <- function(dat_list, var_ind, cut_ind, cut_neg, cut_pov, bin_num, log_ind){
    
#dat_list <- Productivity_list
#var_ind <- 6
var_list <- lapply(dat_list, function(x) x[,c(1,2,var_ind)])
var_list_na_check <- lapply(var_list, function(x) length(na.omit(x[,3])))

zero_obs <- which(unlist(var_list_na_check) < 1000) 
var_list <- var_list[-c(zero_obs)]
country_names_c <- country_names[-zero_obs]

hist_list <- list(); year_list <- list()
for(k in 1:length(var_list)){
  
print(k)

  dat_cut <- var_list[[k]]
  dat_cut <- na.omit(dat_cut)
  
  # cutting those firms that have only one observation. 
  drop_id <- names(which(table(dat_cut$IDNR) > 1))
  dat_cut <- dat_cut[dat_cut$IDNR%in%c(drop_id),]
  
  # cutting the first observation of each firm 
  dat_cut <- dat_cut %>% 
    arrange(IDNR, Year)%>%
       group_by(IDNR) %>% 
       slice(2:n())
  
    if(cut_ind == 1){
     dat_cut <- subset(dat_cut, dat_cut[[3]] > cut_neg & dat_cut[[3]] < cut_pov)
  } else if (cut_ind == 0){
     dat_cut <- subset(dat_cut, dat_cut[[3]] > quantile(dat_cut[[3]], c(cut_neg)) & dat_cut[[3]] < quantile(dat_cut[[3]], c(cut_pov)))
  } else {
    dat_cut <- subset(dat_cut, dat_cut[[3]] > cut_neg & dat_cut[[3]] < quantile(dat_cut[[3]], c(cut_pov)))
  }

     dat_year <- year_names[year_names%in%c(sort(unique(dat_cut[[2]])))]
    y_list <- list()
      for(j in 1:length(dat_year)){
    #print(j)
        ok_1 <- subset(dat_cut, Year == dat_year[j])[[3]]
        
        if(length(ok_1 ) < 1000){
           y_list[[j]]  <- NA
        } else{
                if(log_ind == 1){
      y_list[[j]] <- hist(log(ok_1),breaks=seq(min(log(ok_1)),max(log(ok_1)),l=bin_num+1))
                  # y_list[[j]] <- hist(~log(ok_1), w = bin_wid)
                } else{
                   #y_list[[j]] <- hist(~ok_1, w = bin_wid)
                    y_list[[j]] <- hist(ok_1,breaks=seq(min(ok_1),max(ok_1),l=bin_num+1))
                }
    
             }
          }
       hist_list[[k]]  <- y_list
       year_list[[k]] <- dat_year
      }

hist_final <- list(hist_list, year_list, country_names_c)

return(hist_final)
}  

## ------------------------------------------------------------------------
#Profitability 

Profitability_list <- list()

for(i in 1:44){
  print(i)
  file_name <- paste("panels_", paste(country_names[i], collapse=""), ".Rda", sep="")
  load(file_name)
  Profitability_list[[i]] <- Cleaned_dat_Profitability
}

names(Profitability_list[[1]])

#RoC, 3
RoC_info <- fun_info_gen(dat_list = Profitability_list, var_ind = 3, cut_ind = 0, cut_neg = 0.025, cut_pov = 0.975, bin_num = 100, log_ind = 0)
                         
#RoC_fix, 4
RoC_fix_info <- fun_info_gen(dat_list = Profitability_list, var_ind = 4, cut_ind = 0, cut_neg = 0.025, cut_pov = 0.975, bin_num = 100, log_ind = 0)

#RoC_RCEM, 5 (data in percentage) 
RoC_RCEM_info <- fun_info_gen(dat_list = Profitability_list, var_ind = 5,  cut_ind = 0, cut_neg = 0.025, cut_pov = 0.975,  bin_num = 100,log_ind = 0)


## calculating the mean of RoC for each firm with more than 5 year obs. 
Profitability_list_mean <- list()
for(k in c(1:44)){
    print(k)
  
Profitability_list_mean[[k]] <- Profitability_list[[k]]  %>% 
  select(IDNR, Year, RoC, RoC_RCEM) %>%
  arrange(IDNR, Year)%>%
  group_by(IDNR) %>% 
  filter(length(Year) > 5) %>% 
  summarize(roc_mean = mean(RoC, na.rm = T), roc_rcem_mean = mean(RoC_RCEM, na.rm = T))

}

Profitability_info_list <- list(RoC_info, RoC_fix_info, RoC_RCEM_info)
rm(Profitability_list)

## ------------------------------------------------------------------------
#Productivity
Productivity_list <- list()

for(i in 1:44){
  print(i)
  file_name <- paste("panels_", paste(country_names[i], collapse=""), ".Rda", sep="")
  load(file_name)
  Productivity_list[[i]] <- Cleaned_dat_Productivity
  } 

names(Productivity_list[[1]])

#LP 3
LP_info <- fun_info_gen(dat_list = Productivity_list, var_ind = 3,  cut_ind = 0, cut_neg = 0.025, cut_pov = 0.975, bin_num = 100, log_ind = 0)
                         
#CP 4
CP_info <- fun_info_gen(dat_list = Productivity_list, var_ind = 4,  cut_ind = 0, cut_neg = 0.025, cut_pov = 0.975, bin_num = 100, log_ind = 0)

# LP_change 5
LP_change_info <- fun_info_gen_change(dat_list = Productivity_list,   var_ind = 5, cut_ind = 0, cut_neg = 0.025, cut_pov = 0.975,  bin_num = 100, log_ind = 0)

#CP_change 6
CP_change_info <- fun_info_gen_change(dat_list = Productivity_list,   var_ind = 6, cut_ind = 0, cut_neg = 0.025, cut_pov = 0.975,  bin_num = 100, log_ind = 0)

#Zeta 7
Zeta_info <- fun_info_gen_change(dat_list = Productivity_list, var_ind = 7,  cut_ind = 0, cut_neg = 0.025,   cut_pov = 0.975, bin_num = 100, log_ind = 0)

Productivity_list_mean <- list()
for(k in c(1:44)){
    print(k)
  
Productivity_list_mean[[k]] <- Productivity_list[[k]]  %>% 
  select(IDNR, Year, Zeta, LP_change) %>%
  arrange(IDNR, Year)%>%
  group_by(IDNR) %>% 
  slice(2:n()) %>% 
  summarize(lp_c_mean = mean(LP_change, na.rm = T), zeta_mean = mean(Zeta, na.rm = T))

}

Productivity_info_list <- list(LP_info, CP_info, LP_change_info, CP_change_info, Zeta_info)

rm(Productivity_list)

## ------------------------------------------------------------------------
Firm_size_list <- list()

for(i in 1:44){
  print(i)
  file_name <- paste("panels_", paste(country_names[i], collapse=""), ".Rda", sep="")
  load(file_name)
  Firm_size_list[[i]] <- Cleaned_dat_firm_size
}


names(Firm_size_list[[1]])


#SALE 3
SALE_info <- fun_info_gen(dat_list = Firm_size_list, var_ind = 3, cut_ind = 0, cut_neg = 0.025, cut_pov = 0.975, bin_num = 100, log_ind = 1)
                         
#EMPL 4

EMPL_info <- fun_info_gen(dat_list = Firm_size_list, var_ind = 4, cut_ind = 2, cut_neg = 0.025, cut_pov = 0.975, bin_num = 100, log_ind = 0)

#TOAS 4
TOAS_info <- fun_info_gen(dat_list = Firm_size_list, var_ind = 5, cut_ind = 0, cut_neg = 0.025, cut_pov = 0.975, bin_num = 100, log_ind = 1)

#TURN_change 6
SALE_change_info <- fun_info_gen_change(dat_list = Firm_size_list,  var_ind = 6,  cut_ind = 0, cut_neg = 0.025, cut_pov = 0.975,  bin_num = 100, log_ind = 0)

#EMPL_change 7 (pov and neg separately)
# EMPL_change_info <- fun_info_gen_change(dat_list = Firm_size_list,  var_ind = 7, cut_ind = 0, cut_neg = 0.025, cut_pov = 0.975, bin_num = 100, log_ind = 0)

#TOAS_change 8
TOAS_change_info <- fun_info_gen_change(dat_list = Firm_size_list,  var_ind = 8,  cut_ind = 0, cut_neg = 0.025, cut_pov = 0.975, bin_num = 100, log_ind = 0)

#EMPL_change Again

Firm_size_list_mean <- list()
for(k in c(1:44)){
    print(k)
  
Firm_size_list_mean[[k]] <- Firm_size_list[[k]]  %>% 
  select(IDNR, Year, EMPL_change, TOAS_change) %>%
  arrange(IDNR, Year)%>%
  group_by(IDNR) %>% 
  slice(2:n()) %>% 
  summarize(emp_c_mean = mean(EMPL_change, na.rm = T), toas_c_mean = mean(TOAS_change, na.rm = T))

}


Firm_size_info_list <- list(SALE_info, EMPL_info, CP_info, SALE_change_info,  TOAS_change_info)




rm(Firm_size_list)

## ------------------------------------------------------------------------
Cost_structure_list <- list()

for(i in 1:44){
  print(i)
  file_name <- paste("panels_", paste(country_names[i], collapse=""), ".Rda", sep="")
  load(file_name)
  Cost_structure_list[[i]] <- Cleaned_dat_cost_structure
 
}

names(Cost_structure_list[[1]])

#WS 3
WS_info <- fun_info_gen(dat_list = Cost_structure_list, var_ind = 3, cut_ind = 2, cut_neg = 0.025, cut_pov = 0.975, bin_num = 100, log_ind = 0)

# PW ratio 5
PW_ratio_info <- fun_info_gen(dat_list = Cost_structure_list, var_ind = 5, cut_ind = 2, cut_neg = 0.025, cut_pov = 0.975, bin_num = 100, log_ind = 0)

# C_com 6
#C_com_info <- fun_info_gen(dat_list = Cost_structure_list, var_ind = 6, cut_ind = 2, cut_neg = 0.025, cut_pov = 0.975, bin_num = 100, log_ind = 0)

Cost_structure_info_list <- list(WS_info, PW_ratio_info)

rm(Cost_structure_list)

## ------------------------------------------------------------------------

## the function for the distribution of the mean
fun_info_gen_mean <- function(dat_list, var_ind, cut_ind, cut_neg, cut_pov, bin_num, log_ind){
#dat_list <- Firm_size_list_mean
  # take year and variable, and check the lenght of observations per country
  var_list <- lapply(dat_list, function(x) x[,c(var_ind)]) 
  var_list_na_check <- lapply(var_list, function(x) nrow(na.omit(x[,1]))) 

  # cutting some countries that do not have more than 1,000 obs. 
  zero_obs <- which(unlist(var_list_na_check) < 1000) 
  var_list <- var_list[-c(zero_obs)]
  country_names_c <- country_names[-zero_obs]
  
  hist_list <- list(); year_list <- list()
  
  # looping by each country 
  for(k in 1:length(var_list)){
    
  print(k)
    
  dat_cut <- var_list[[k]]
  dat_cut <- na.omit(dat_cut)
  
## Cutting data: ind = 1 means cutting by absolute value, ind = 0 means cutting by tail percentage, ind = else means cutting the left tail by absolute value and cutting the right tail by percentage
  if(cut_ind == 1){
     dat_cut <- subset(dat_cut[[1]], dat_cut[[1]] > cut_neg & dat_cut[[1]] < cut_pov)
  } else if(cut_ind == 0){
     dat_cut <- subset(dat_cut[[1]], dat_cut[[1]] > quantile(dat_cut[[1]], c(cut_neg)) & dat_cut[[1]] < quantile(dat_cut[[1]], c(cut_pov)))
  } else {
    dat_cut <- subset(dat_cut[[1]], dat_cut[[1]] > cut_neg & dat_cut[[1]] < quantile(dat_cut[[1]], c(cut_pov)))
  }
  
  
  ## Looping by each year  
    #print(j)
        ok_1 <- dat_cut
        
          # log_ind = 1 give the histogram of the log of the targe variable 
                if(log_ind == 1){
                    hist_list[[k]] <- hist(log(ok_1),breaks=seq(min(log(ok_1)),max(log(ok_1)),l=bin_num+1))
                   
                } else{
                   
                     hist_list[[k]] <- hist(ok_1,breaks=seq(min(ok_1),max(ok_1),l=bin_num+1))
                }
          
      }
   
hist_final <- list(hist_list, country_names_c)

return(hist_final)
}




TOAS_change_mean_info <- fun_info_gen_mean(dat_list = Firm_size_list_mean,  var_ind = 3,  cut_ind = 0, cut_neg = 0.025, cut_pov = 0.975, bin_num = 100, log_ind = 0)


LP_change_mean_info <- fun_info_gen_mean(dat_list = Productivity_list_mean,  var_ind = 2,  cut_ind = 0, cut_neg = 0.025, cut_pov = 0.975, bin_num = 100, log_ind = 0)

Zeta_mean_info <- fun_info_gen_mean(dat_list = Productivity_list_mean,  var_ind = 3,  cut_ind = 0, cut_neg = 0.025, cut_pov = 0.975, bin_num = 100, log_ind = 0)

RoC_mean_info <- fun_info_gen_mean(dat_list = Profitability_list_mean,  var_ind = 2,  cut_ind = 0, cut_neg = 0.025, cut_pov = 0.975, bin_num = 100, log_ind = 0)

RoC_RCEM_mean_info <- fun_info_gen_mean(dat_list = Profitability_list_mean,  var_ind = 3,  cut_ind = 0, cut_neg = 0.025, cut_pov = 0.975, bin_num = 100, log_ind = 0)

mean_info_list <- list(EMPL_change_mean_info, TOAS_change_mean_info, LP_change_mean_info, Zeta_mean_info, RoC_mean_info, RoC_RCEM_info)


## ------------------------------------------------------------------------
setwd("~/Desktop/Cleaned Rda/First Report")
save(Profitability_info_list, 
     Productivity_info_list,
     Firm_size_info_list,
     Cost_structure_info_list,
     mean_info_list,
     file = "Cleaned_up_info_list.Rda"
     )

