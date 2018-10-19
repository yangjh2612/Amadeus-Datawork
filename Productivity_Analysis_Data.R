## ------------------------------------------------------------------------
setwd("~/Desktop/Cleaned Rda")
library(colorspace)

colores_10 <- rainbow_hcl(10, end=300)
year_names <- c(2006:2015)
country_names  <- c('Albania', 'Austria', 'Belarus', 'Belgium', 'Bosnia and Herzegovina', 'Bulgaria', 'Croatia', 'Cyprus', 'Czech Republic', 'Denmark', 'Estonia', 'Finland', 'France', 'Germany', 'Greece', 'Hungary', 'Iceland', 'Ireland', 'Italy', 'Kosovo', 'Latvia', 'Liechtenstein', 'Lithuania', 'Luxembourg', 'Macedonia, FYR', 'Malta', 'Monaco', 'Montenegro', 'Netherlands', 'Norway', 'Poland', 'Portugal', 'Moldova', 'Romania', 'Russian Federation', 'Serbia', 'Slovakia', 'Slovenia', 'Spain', 'Sweden', 'Switzerland', 'Turkey', 'Ukraine', 'United Kingdom')


## ------------------------------------------------------------------------
# summary statistics 
library(plyr)
library(dplyr)
library(zoo)
#####
# Loading the cleaned up data for Index
Index_list <- list()

for(i in 1:44){
  print(i)
  file_name <- paste("panels_J!&", paste(country_names[i], collapse=""), ".Rda", sep="")
  load(file_name)
  Index_list[[i]] <- Cleaned_dat_INDEX
}

take_ind <- which(unlist(lapply(Index_list, function(x) length(unique(x$IDNR))))  < 50000)

Index_list <- Index_list[-c(take_ind)]


## ------------------------------------------------------------------------
# Loading the cleaned up data for Firm_size
Firm_size_list <- list()

for(i in c(1:44)[-c(take_ind)]){
  print(i)
  file_name <- paste("panels_J!&", paste(country_names[i], collapse=""), ".Rda", sep="")
  load(file_name)
  Firm_size_list[[i]] <- Cleaned_dat_firm_size
}


Firm_size_list <- Firm_size_list[-c(take_ind)]

# taking target variables only
Firm_size_list <- lapply(Firm_size_list, function(x) x[-c(1,2)])
Index_list <- lapply(Index_list, function(x) x[-c(1,2,3,4,5,7,9,12,13)])


## ------------------------------------------------------------------------
# Loading the cleaned up data for Productiviy
Productivity_list <- list()

for(i in c(1:44)[-c(take_ind)]){
  print(i)
  file_name <- paste("panels_J!&", paste(country_names[i], collapse=""), ".Rda", sep="")
  load(file_name)
  Productivity_list[[i]] <- Cleaned_dat_Productivity
  } 

Productivity_list <- Productivity_list[-c(take_ind)]

rm(Cleaned_dat_cost_structure)
rm(Cleaned_dat_firm_size)
rm(Cleaned_dat_INDEX)
rm(Cleaned_dat_Productivity)
rm(Cleaned_dat_Profitability)
rm(Cleaned_dat_RD)



## ------------------------------------------------------------------------
country_names   <- country_names[-c(take_ind)]

## ------------------------------------------------------------------------
All_list_for_prod <- list()

for(k in 1:length(Productivity_list)){
  print(k)
  All_list_for_prod[[k]] <- cbind(Productivity_list[[k]], Index_list[[k]], Firm_size_list[[k]])
  
  Productivity_list[[k]] <- 1
  Index_list[[k]] <- 1
  Firm_size_list[[k]] <- 1
}
rm(Productivity_list)
rm(Index_list)
rm(Firm_size_list)


## ------------------------------------------------------------------------
save(All_list_for_prod, country_names, file = "All_list_for_prod.Rda")


## ------------------------------------------------------------------------
library(knitr)
purl("Productivity_Analysis_Data.Rmd")  

