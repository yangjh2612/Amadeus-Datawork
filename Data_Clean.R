# Script to clean and combine the country-level Rda file generated from the masterfile. It also creats several objects for indexing variables that will be used in other scripts.


year_names <- c(2006:2015) # Year Index
country_names  <- c('Albania', 'Austria', 'Belarus', 'Belgium', 'Bosnia and Herzegovina', 'Bulgaria', 'Croatia', 'Cyprus', 'Czech Republic', 'Denmark', 'Estonia', 'Finland', 'France', 'Germany', 'Greece', 'Hungary', 'Iceland', 'Ireland', 'Italy', 'Kosovo', 'Latvia', 'Liechtenstein', 'Lithuania', 'Luxembourg', 'Macedonia, FYR', 'Malta', 'Monaco', 'Montenegro', 'Netherlands', 'Norway', 'Poland', 'Portugal', 'Moldova', 'Romania', 'Russian Federation', 'Serbia', 'Slovakia', 'Slovenia', 'Spain', 'Sweden', 'Switzerland', 'Turkey', 'Ukraine', 'United Kingdom') # Country Index


## ------------------------------------------------------------------------
## Clean up the data and extract relevant variables

# loading of libraries
if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(dplyr,zoo)


non_na_ind <- list(); Firm_Size_list <- list(); Index_list <- list(); Productivity_list <- list(); Profitability_list <- list(); Cost_Structure_list <- list() # empty lists

for(i in c(1:44)){
  print(i)
  file_name <- paste("panels_J!&", paste(country_names[i], collapse=""), ".Rda", sep="")
  load(file_name)
  
  names(Cleaned_dat_Firm_Size)
  names(Cleaned_dat_INDEX)
  names(Cleaned_dat_Productivity)
  names(Cleaned_dat_Profitability)
  names(Cleaned_dat_Cost_Structure)

  non_na_ind[[i]] <- which(apply(is.na(Cleaned_dat_Firm_Size[,c("EMPL","TOAS","VA")]), 1, sum) == 0) # non-missing for employment, total asset, and value added
  
  Firm_Size_list[[i]] <- Cleaned_dat_Firm_Size[non_na_ind[[i]],] 
  
  Index_list[[i]] <- Cleaned_dat_INDEX[non_na_ind[[i]], c("NUTS_1", "NUTS_2", "NUTS_3","NACE_PRIM_CODE","COMPCAT","LSTATUS", "QUOTED","Firm_Age")]# three geographical index (nut_1, nut_2, nut_3), nace code, firm_size, legal status, quoted, firm aga
  
  Productivity_list[[i]] <- Cleaned_dat_Productivity[non_na_ind[[i]],c("LP", "CP","TFP", "LP_lr",  "CP_lr", "TFP_lr", "CP_diff", "LP_diff", "TFP_diff", "ZOMBIE")] # Labor productivity, Capital Productivity, TFP, LP log return, CP log return, TFP log return, CP difference, LP difference, TFP difference, Zombie firm index
  
  Profitability_list[[i]] <- Cleaned_dat_Profitability[non_na_ind[[i]],-c(1,2)] # all but year and ID
  
  Cost_Structure_list[[i]] <- Cleaned_dat_Cost_Structure[non_na_ind[[i]],c("WS","WS_AD")] # Wage share before and after depreciation
  
  
}



## ------------------------------------------------------------------------
All_list_Cleaned <- list()

for(k in 1:length(Productivity_list)){
  print(k)
  All_list_Cleaned[[k]] <- cbind(Index_list[[k]], Firm_Size_list[[k]], Productivity_list[[k]], Profitability_list[[k]], Cost_Structure_list[[k]]) # column wise binding
  
  # To save memory, we eliminate the already processed element in each of the lists
  Index_list[[k]] <- 1 
  Firm_Size_list[[k]] <- 1
  Productivity_list[[k]] <- 1  
  Profitability_list[[k]] <- 1
  Cost_Structure_list[[k]] <- 1
}

## ------------------------------------------------------------------------
#NACE code
load("NACE2_CLASS_DES.Rda")

nace2$code <- as.numeric(nace2$code)
nace2 <- nace2[!is.na(nace2$code),]

All_list_Cleaned <- lapply(All_list_Cleaned, function(x){
    x <- x %>% left_join(nace2 %>% select(NACE_PRIM_CODE = code, NACE_DES = description, NACE_CAT = category), by = "NACE_PRIM_CODE") # change the NACE index
})


## ------------------------------------------------------------------------

save(All_list_Cleaned, country_names, file = "All_list_Cleaned.Rda") # save the result


## ------------------------------------------------------------------------
##  0.2 loading of required data and cleaning

load("All_list_Cleaned.Rda") ## load the data file created from "Productivity_Analysis_Data.Rmd"
names(All_list_Cleaned[[1]])

# Cleaning
All_list_Cleaned_cut <- list() 
for (k in 1:length(All_list_Cleaned)) {
  print(k)
  All_list_Cleaned_cut[[k]] <- All_list_Cleaned[[k]] %>%
    select(IDNR, Year, COMPCAT, NACE_CAT, LSTATUS, EMPL, Firm_Age, LP, TFP, LP_lr, TFP_lr, LP_diff, TFP_diff, ZOMBIE) %>% # Firm ID, Year, Firm Size, Industry ID, Legal Status, Employment, Firm age, Labor Produtivity, TFP, LP log return, TFP log return, LP difference, TFP difference,  Zombie firm index
    mutate(COMPCAT_one = substr(COMPCAT, 1, 1)) %>% # the first letter of the size variable
    group_by(Year) %>%
    filter(length(IDNR) > 10000) %>% # Note that 23 countries out of 44 do not have VA info. Out of 21 countries, 15 countries have enough info for anlaysis (more than 5 years with 10,000 firms )
    group_by()
}

# Countries with more than 5 year obs with 10,000 firms
take_this <- which(unlist(lapply(All_list_Cleaned_cut, function(x) length(unique(x$Year)) > 5))) 

All_list_Cleaned_cut <- All_list_Cleaned_cut[take_this]
country_names <- country_names[take_this]

## ------------------------------------------------------------------------

# This part of code is only for the old version of the masterfile where the dplyr lag function doesn't properly work
for(k in 1:15){
  print(k)
  All_list_Cleaned_cut[[k]] <- All_list_Cleaned_cut[[k]]%>%
    group_by(IDNR) %>%
    arrange(Year) %>%
    mutate(LP_diff = LP - lag(LP, 1, order_by = Year))
}
## ------------------------------------------------------------------------


save(All_list_Cleaned_cut, file = "All_list_Cleaned_cut.Rda") # save the final object


rm(All_list_Cleaned)

## indexing variables
# country index
country_names_five <- c("France", "Germany", "Italy", "Spain", "United Kingdom") # country name for the five largest countries

# industry index
ind_name_table <- data.frame(ind_names = unique(All_list_Cleaned_cut[[15]]$NACE_CAT), 
ind_names_alphabet = c("J", "L", "G", "C", "I", "H", "S", "P", "M", "K", "B", "N", "A", "Q", "R", "F", "E", "O", "D", "T")) # alphabet index according to the appendix in the manuscript

ind_name_table <- ind_name_table[order(ind_name_table$ind_names_alphabet),]
ind_name_table$ind_names_short <-  c("Agr", "Mine", "Manu", "Elec", "Water", "Cons", "Whole", "Trans", "Accom", "Info", "F&I", "Real", "Prof-S", "Admin", "Public", "Edu", "Heath", "Art", "O-Serv", "House") # short index

ind_name_table <- ind_name_table[-c(20),] # remove the household sector

size_names <- c("S", "M", "L", "V") # size index short
size_names_long <- c("SMALL", "MEDIUM SIZED", "LARGE", "VERY LARGE") # size index long
names(size_names) <- 1:4 # create the numerical name for the size index


save(year_names, country_names, country_names_five, ind_name_table, size_names,  size_names_long, file = "Labels.Rda") 

