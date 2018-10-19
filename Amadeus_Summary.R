## ------------------------------------------------------------------------
setwd("~/Desktop/Cleaned Rda")
library(colorspace)

colores_10 <- rainbow_hcl(10, end=300)
year_names <- c(2006:2015)
country_names  <- c('Albania', 'Austria', 'Belarus', 'Belgium', 'Bosnia and Herzegovina', 'Bulgaria', 'Croatia', 'Cyprus', 'Czech Republic', 'Denmark', 'Estonia', 'Finland', 'France', 'Germany', 'Greece', 'Hungary', 'Iceland', 'Ireland', 'Italy', 'Kosovo', 'Latvia', 'Liechtenstein', 'Lithuania', 'Luxembourg', 'Macedonia, FYR', 'Malta', 'Monaco', 'Montenegro', 'Netherlands', 'Norway', 'Poland', 'Portugal', 'Moldova', 'Romania', 'Russian Federation', 'Serbia', 'Slovakia', 'Slovenia', 'Spain', 'Sweden', 'Switzerland', 'Turkey', 'Ukraine', 'United Kingdom')


## ------------------------------------------------------------------------
# summary statistics (5)
library(plyr)
library(dplyr)
library(zoo)
#####

## calculating the median after removing NAs
fun_median <- function(x){
  x <- na.omit(x)
  return(median(x))
}

## calculating the empirical entropy after removing NAs: ind detmermines the cutting principle (ind = 1: cutting only the right tail, ind = 0: cutting both tails)
ent_fun <- function(x, ind, bin_num){
  
  
  library(entropy)
  
  x <- na.omit(x)
  if(ind == 0){
    dat <- x
  }else if(ind == 1){
   dat <- subset(x, x < quantile(x, 0.99))
  }else{
     dat <- subset(x, x > quantile(x, 0.01) & x < quantile(x, 0.99))
  }
  
  # if the # of obs are below 200, we remove the sample
  if(length(dat) < 200){
    ok_final <- NA
  }else{
  bin_num <- bin_num
  ok <- discretize(dat, bin_num)
  ok_final <- entropy(ok/sum(ok))
  }
  
  return(ok_final)
}

## weighted average: dat_x weighted by dat_y, ind is again the cutting principle
fun_wei_mean <- function(dat_x, dat_y, ind){
  dat_xy <- na.omit(data.frame(dat_x = dat_x, dat_y = dat_y))
  
  if(ind == 0){
    dat_xy <- dat_xy
  }else if(ind == 1){
   dat_xy <- subset(dat_xy, dat_x < quantile(dat_x, 0.99))
  }else{
     dat_xy <- subset(dat_xy, dat_x > quantile(dat_x, 0.01) & dat_x < quantile(dat_x, 0.99))
  }
  
  ok <- sum(dat_xy$dat_y/sum(dat_xy$dat_y)*dat_xy$dat_x)
  
  return(ok)
  
}

## ------------------------------------------------------------------------

# Loading the cleaned up data (panels_) for firm index
Index_list <- list()

for(i in 1:44){
  print(i)
  file_name <- paste("panels_", paste(country_names[i], collapse=""), ".Rda", sep="")
  load(file_name)
  Index_list[[i]] <- Cleaned_dat_INDEX
}

# number of firms and the proportion of small firms
ind_summary <- list()
for(k in c(1:44)){
    print(k)
    ind_summary[[k]] <- Index_list[[k]]  %>% 
             select(IDNR, COMPCAT) %>%
             summarize(n_firms = length(unique(IDNR)),
                       prop_small_firms = round(length(COMPCAT[COMPCAT%in%c("SMALL")])/length(COMPCAT), 3)*100)
             }

# to clear up the memory
rm(Index_list)
rm(Cleaned_dat_cost_structure)
rm(Cleaned_dat_firm_size)
rm(Cleaned_dat_INDEX)
rm(Cleaned_dat_Productivity)
rm(Cleaned_dat_Profitability)
rm(Cleaned_dat_RD)

## ------------------------------------------------------------------------

# Loading the cleaned up data (panels_) for firm size variables
Firm_size_list <- list()

for(i in 1:44){
  print(i)
  file_name <- paste("panels_", paste(country_names[i], collapse=""), ".Rda", sep="")
  load(file_name)
  Firm_size_list[[i]] <- Cleaned_dat_firm_size
}


# Size summary
size_summary <- list()
for(k in c(1:44)){
    print(k)
  if(nrow(Firm_size_list[[k]]) < 200){
    size_summary[[k]] <- NA
  }else{
     size_summary[[k]] <- Firm_size_list[[k]]  %>% 
             summarize(emp_med = fun_median(EMPL),
                       emp_max = max(EMPL, na.rm = T),
                       emp_ent =  round(ent_fun(EMPL, ind = 1, bin_num = 200),2),
                      
                       toas_med = round(fun_median(TOAS),0),
                       toas_ent = round(ent_fun(as.numeric(TOAS), ind = 1, bin_num = 200),2),
                       
                       sale_med = round(fun_median(as.numeric(SALE)),0),
                       sale_ent = round(ent_fun(as.numeric(SALE), ind = 1, bin_num = 200),2)
                       )

     }
}


# Size Growth Summary (after cutting the first obs in each firm)
size_summary_2 <- list()
for(k in c(1:44)){
    print(k)
  if(nrow(Firm_size_list[[k]]) < 200){
    size_summary_2[[k]] <- NA
  }else{
     size_summary_2[[k]] <- Firm_size_list[[k]]  %>% 
             select(IDNR, Year, EMPL, EMPL_change, TOAS_change, SALE_change)%>% 
             arrange(IDNR, Year)%>%
             group_by(IDNR) %>% 
             slice(2:n()) %>% 
             mutate(EMPL_m = lag(EMPL,1)) %>% 
             filter(!is.na(EMPL_m)) %>%
             group_by() %>% 
             summarize(emp_change_med = round(fun_median(EMPL_change),4)*100,
                       emp_change_ent = round(ent_fun(EMPL_change, ind = 1, bin_num = 200),2),
                       emp_change_wei = round(fun_wei_mean(EMPL_change, EMPL_m, 1),4)*100,
                       toas_change_med = round(fun_median(TOAS_change),4)*100,
                       toas_change_ent = round(ent_fun(TOAS_change, ind = 1, bin_num = 200),2),
                       toas_change_wei = round(fun_wei_mean(TOAS_change, EMPL_m, 1),4)*100,
                       sale_change_med = round(fun_median(SALE_change),4)*100,
                       sale_change_ent = round(ent_fun(SALE_change, ind = 1, bin_num = 200),2),
                       sale_change_wei = round(fun_wei_mean(SALE_change, EMPL_m, 1),4)*100
                       )

  }
                }

## the proportion of firms with zero employment growth (this can actully be doen by just looking at the outcome in size_summary_2)
emp_zero_prob <- list()
for(k in c(1:44)){
    print(k)
  
    emp_zero_prob[[k]] <- Firm_size_list[[k]]  %>% 
             select(IDNR, Year, EMPL_change)%>% 
             arrange(IDNR, Year)%>%
             group_by(IDNR) %>% 
             slice(2:n()) %>% 
             filter(!is.na(EMPL_change))%>%
             group_by() %>% 
             summarize(zero_prop = round(sum(EMPL_change == 0)/length(EMPL_change),3)*100)
             }
 


## ------------------------------------------------------------------------

# Loading the cleaned up data for productivity
Productivity_list <- list()

for(i in 1:44){
  print(i)
  file_name <- paste("panels_", paste(country_names[i], collapse=""), ".Rda", sep="")
  load(file_name)
  Productivity_list[[i]] <- Cleaned_dat_Productivity
  } 


# the proportion of negative value added
neg_va_prop <- list()
for(k in c(1:44)){
  print(k)
    neg_va_prop[[k]] <- Productivity_list[[k]]  %>% 
             filter(!is.na(LP)) %>% 
             filter(!is.infinite(LP)) %>% 
             summarize(neg_va_prop = round(length(LP[LP < 0])/length(LP), 3)*100)
             }

# the level of productivity
prod_summary <- list()
for(k in c(1:44)){
  print(k)
  if(length(na.omit(Productivity_list[[k]]$LP)) < 200||
     length(na.omit(Productivity_list[[k]]$CP)) < 200){
       prod_summary[[k]] <- NA
    } else{
    Productivity_list[[k]]$EMPL <- Firm_size_list[[k]]$EMPL  
    prod_summary[[k]] <- Productivity_list[[k]]  %>% 
             select(IDNR, LP, CP, EMPL) %>%
             mutate(EMPL_m = lag(EMPL,1)) %>%
             filter(!is.na(EMPL_m)) %>%
             filter(!is.na(LP)) %>% 
             summarize(neg_va_prop = round(length(LP[LP < 0])/length(LP), 3)*100,
                       lp_med = round(fun_median(LP), 0),
                       lp_wei = round(fun_wei_mean(LP, EMPL_m, 2), 0),
                       lp_ent = round(ent_fun(LP, ind = 2, bin_num = 200), 2),
                       cp_med = round(fun_median(CP), 2),
                       cp_wei = round(fun_wei_mean(CP, EMPL_m, 2), 2),
                       cp_ent = round(ent_fun(CP, ind = 2, bin_num = 200), 2)
                        )
             }
}

# the growth rate of productivty
prod_summary_2 <- list()
for(k in c(1:44)){
  print(k)
  if(length(na.omit(Productivity_list[[k]]$LP_change)) < 200 ||
     length(na.omit(Productivity_list[[k]]$CP_change)) < 200||
     length(na.omit(Productivity_list[[k]]$Zeta)) < 200){
    
       prod_summary_2[[k]] <- NA
       
    }else{
      Productivity_list[[k]]$EMPL <- Firm_size_list[[k]]$EMPL 
      prod_summary_2[[k]] <- Productivity_list[[k]]  %>% 
             select(IDNR, Year, LP_change, CP_change, Zeta, EMPL)%>% 
             arrange(IDNR, Year)%>%
             group_by(IDNR) %>% 
             slice(2:n()) %>% 
             mutate(EMPL_m = lag(EMPL,1)) %>%
             filter(!is.na(EMPL_m)) %>%
             group_by() %>% 
             summarize(lp_change_med = round(fun_median(LP_change),3)*100,
                       lp_change_wei = round(fun_wei_mean(LP_change, EMPL_m, 2),4)*100,
                       lp_change_ent = round(ent_fun(LP_change, ind = 2, bin_num = 200), 2),
                       cp_change_med = round(fun_median(CP_change),3)*100,
                       cp_change_wei = round(fun_wei_mean(CP_change, EMPL_m, 2),4)*100,
                       cp_change_ent = round(ent_fun(CP_change, ind = 2, bin_num = 200), 2),
                       zeta_med = round(fun_median(Zeta),3)*100,
                       zeta_wei = round(fun_wei_mean(Zeta, EMPL_m, 2),4)*100,
                       zeta_ent = round(ent_fun(Zeta, ind = 2, bin_num = 200), 2)
                       )
             }

}


##
rm(Productivity_list)


## ------------------------------------------------------------------------

# Loading the cleaned up data for profitability
Profitability_list <- list()

for(i in 1:44){
  print(i)
  file_name <- paste("panels_", paste(country_names[i], collapse=""), ".Rda", sep="")
  load(file_name)
  Profitability_list[[i]] <- Cleaned_dat_Profitability
}


roc_summary <- list()
for(k in c(1:44)){
  
  print(k)
  if(length( na.omit(Profitability_list[[k]]$RoC_RCEM )) < 200 ||
         length( na.omit(Profitability_list[[k]]$RoC_fix )) < 200 ||
         length( na.omit(Profitability_list[[k]]$RoC_RCEM )) < 200){
     roc_summary[[k]]  <- NA
  }else{
      Profitability_list[[k]]$EMPL <- Firm_size_list[[k]]$EMPL
     roc_summary[[k]] <- Profitability_list[[k]]  %>% 
             select(IDNR, RoC, RoC_fix, RoC_RCEM, EMPL) %>%
             filter(!is.na(RoC), !is.na(RoC_fix), !is.na(RoC_RCEM), !is.na(EMPL)) %>%
             filter(is.finite(RoC), is.finite(RoC_fix), is.finite(RoC_RCEM)) %>%
             mutate(EMPL_m = lag(EMPL,1)) %>%
             filter(!is.na(EMPL_m)) %>%
             summarize(neg_prop = round(length(RoC[RoC < 0])/length(RoC),4)*100,
                       roc_sum_med = round(fun_median(RoC),3)*100,
                       roc_sum_ent = round(ent_fun(RoC, ind = 2, bin_num = 200),2),
                       roc_wei = round(fun_wei_mean(RoC, EMPL_m, 2),4)*100,
                       
                       roc_fix_sum_med = round(fun_median(RoC_fix),4)*100, 
                       roc_fix_sum_ent = round(ent_fun(RoC_fix, ind = 2, bin_num = 200), 2),
                       roc_fix_wei = round(fun_wei_mean(RoC_fix, EMPL_m, 2),4)*100,
                       
                       roc_rcem_med = round(fun_median(RoC_RCEM), 2),
                       roc_rcem_ent = round(ent_fun(RoC_RCEM, ind = 2, bin_num = 200),2),
                       roc_rcem_wei = round(fun_wei_mean(RoC_RCEM, EMPL_m, 2),2)
                       )
     

  }
}



rm(Profitability_list)

## ------------------------------------------------------------------------
# Loading the cleaned up data for cost structure
Cost_structure_list <- list()

for(i in 1:44){
  print(i)
  file_name <- paste("panels_", paste(country_names[i], collapse=""), ".Rda", sep="")
  load(file_name)
  Cost_structure_list[[i]] <- Cleaned_dat_cost_structure
 
}

cost_summary <- list()
for(k in c(1:44)){
  print(k)
      if(length(na.omit(Cost_structure_list[[k]]$PW_ratio)) < 200){
       cost_summary[[k]] <- NA
    } else{
    Cost_structure_list[[k]]$EMPL <- Firm_size_list[[k]]$EMPL
    cost_summary[[k]] <- Cost_structure_list[[k]]  %>% 
             select(IDNR, PW_ratio,WS, EMPL) %>%
              mutate(EMPL_m = lag(EMPL,1)) %>%
             filter(!is.na(EMPL_m)) %>%
             filter(!is.na(PW_ratio)) %>%
             summarize(pw_med  = round(fun_median(PW_ratio),2),
                       pw_wei = round(fun_wei_mean(PW_ratio, EMPL_m, 2),2),
                       pw_ent = round(ent_fun(PW_ratio, ind = 1, bin_num = 200),2),
                       ws_med  = round(fun_median(WS),2),
                       ws_wei = round(fun_wei_mean(WS, EMPL_m, 1),2),
                       ws_ent = round(ent_fun(WS, ind = 1, bin_num = 200),2)
                       )
    }
}

rm(Cost_structure_list)
##

## ------------------------------------------------------------------------
# save all the results in Rda file
save(ind_summary, size_summary, size_summary_2, emp_zero_prob, 
neg_va_prop,  roc_summary,  cost_summary,  prod_summary, prod_summary_2, 
file = "Summary_all.Rda")

## ------------------------------------------------------------------------
# hand picked variables from the results
size_ind_frame <- data.frame(
  Country = country_names, 
  N_firms = format(unlist(lapply(ind_summary, function(x) x[1])),  big.mark = ","),
  S_firms = unlist(lapply(ind_summary, function(x) x[2])),
  E_me = unlist(lapply(size_summary, function(x) x[1])),
  E_h = unlist(lapply(size_summary, function(x) x[3])),
  K_me = format(unlist(lapply(size_summary, function(x) x[4])), big.mark = ","),
  K_h = unlist(lapply(size_summary, function(x) x[5])),
  S_me = format(unlist(lapply(size_summary, function(x) x[6])), big.mark = ","),
  S_h = unlist(lapply(size_summary, function(x) x[7]))
  )

size_ind_frame_2 <- data.frame(
  Country = country_names, 
  delta_E_z = unlist(emp_zero_prob, function(x) x[1]),
  delta_E_wm = unlist(lapply(size_summary_2, function(x) x[3])),
  delta_E_me = unlist(lapply(size_summary_2, function(x) x[1])),
  delta_E_h = unlist(lapply(size_summary_2, function(x) x[2])),
  delta_K_wm = format(unlist(lapply(size_summary_2, function(x) x[6])), big.mark = ","),
  delta_K_me = format(unlist(lapply(size_summary_2, function(x) x[4])), big.mark = ","),
  delta_K_h = unlist(lapply(size_summary_2, function(x) x[5])),
  delta_S_wm = format(unlist(lapply(size_summary_2, function(x) x[9])), big.mark = ","),
  delta_S_me = format(unlist(lapply(size_summary_2, function(x) x[7])), big.mark = ","),
  delta_S_h = unlist(lapply(size_summary_2, function(x) x[8]))
  )

profitability_frame <- data.frame(
  Country = country_names,  
  Neg_V = unlist(neg_va_prop), 
  r_wm = unlist(lapply(roc_summary, function(x) x[4])),
  r_me = unlist(lapply(roc_summary, function(x) x[2])),
  r_h =  unlist(lapply(roc_summary, function(x) x[3])),
  #roc_fix_w_mean = round(unlist(lapply(roc_summary, function(x) x[6])),2),
  #roc_fix_med = round(unlist(lapply(roc_summary, function(x) x[6])),2),
  #roc_fix_ent = unlist(lapply(roc_summary, function(x) x[7])),
  roc_rcem_w_mean = round(unlist(lapply(roc_summary, function(x) x[10])),2),
  roc_rcem_med = round(unlist(lapply(roc_summary, function(x) x[8])),2),
  roc_rcem_ent = unlist(lapply(roc_summary, function(x) x[9])),
  tau_we = unlist(lapply(cost_summary, function(x) x[2])),
  tau_me = unlist(lapply(cost_summary, function(x) x[1])),
  tau_h = unlist(lapply(cost_summary, function(x) x[3]))
  #ws_w_mean = unlist(lapply(cost_summary, function(x) x[5])),
  #ws_med = unlist(lapply(cost_summary, function(x) x[4])),
  #ws_ent = unlist(lapply(cost_summary, function(x) x[6]))
  )


productivity_frame <- data.frame(
  Country = country_names, 
  x_wm = format(unlist(lapply(prod_summary, function(x) x[3])), big.mark = ","),
  x_me = format(unlist(lapply(prod_summary, function(x) x[2])), big.mark = ","),
  x_h = unlist(lapply(prod_summary, function(x) x[4])),
  rho_wm = unlist(lapply(prod_summary, function(x) x[6])),
  rho_me = unlist(lapply(prod_summary, function(x) x[5])),
  rho_h = unlist(lapply(prod_summary, function(x) x[7]))
    )

productivity_frame_2 <- data.frame(
  Country = country_names, 
  delta_x_wm =  unlist(lapply(prod_summary_2, function(x) x[2])),
  delta_x_me =  unlist(lapply(prod_summary_2, function(x) x[1])),
  delta_x_h = unlist(lapply(prod_summary_2, function(x) x[3])),
  delta_rho_wm =  unlist(lapply(prod_summary_2, function(x) x[5])),
  delta_rho_me = unlist(lapply(prod_summary_2, function(x) x[4])),
  delta_rho_h = unlist(lapply(prod_summary_2, function(x) x[6])),
  zeta_wm =  unlist(lapply(prod_summary_2, function(x) x[8])),
  zeta_me = unlist(lapply(prod_summary_2, function(x) x[7])),
  zeta_h = unlist(lapply(prod_summary_2, function(x) x[9]))
  )


## ------------------------------------------------------------------------
# report those countries whose obs are greater than 50000. 
take_con <- which(as.numeric(gsub(",","",size_ind_frame$N_firms)) > 50000)

size_final <- size_ind_frame[take_con,]
size_final_2 <- size_ind_frame_2[take_con,]
prof_final <- profitability_frame[take_con,]
prod_final <- productivity_frame[take_con,]
prod_final_2 <- productivity_frame_2[take_con,]

row.names(size_final) <- 1:length(take_con)
row.names(size_final_2) <- 1:length(take_con)
row.names(prof_final) <- 1:length(take_con)
row.names(prod_final) <- 1:length(take_con)
row.names(prod_final_2) <- 1:length(take_con)

country_names_c <- country_names[take_con]

## ------------------------------------------------------------------------
library(xtable)
## Latex form 
size_table <- xtable(size_final)
size_table_2 <- xtable(size_final_2)
prof_table <- xtable(prof_final)
prod_table <- xtable(prod_final)
prod_table_2 <- xtable(prod_final_2)

