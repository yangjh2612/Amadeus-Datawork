## ------------------------------------------------------------------------

# Load libraries

library(data.table)
library(plyr)
library(dplyr)



# Functions

# Data cleaning function
fun_data_clean <- function(dat){
  
  data_c <- dat %>%
    filter(!is.na(CLOSDATE_year)) %>% # year
    filter(!is.na(IDNR)) %>% # firm indicator 
    filter(CLOSDATE_year >= 2006 & CLOSDATE_year <= 2015) %>% # year cut 
    filter(CONSOL == 'C1' | CONSOL == 'U1') %>% #  consolidate or unconsolidate with no companion statements 
    distinct(IDNR, CLOSDATE_year, .keep_all = TRUE) %>% # remove duplicated rows
    
    # remove nonsensical values
    mutate(EMPL = replace(EMPL, EMPL <= 0, NA),
           TOAS = replace(TOAS, TOAS <= 0, NA),
           STAF = replace(STAF, STAF < 0, NA),
           TURN = replace(TURN, TURN < 0, NA)) %>% 
    
    
    arrange(IDNR, CLOSDATE_year) %>% # arranging data by firm and year
    
    # deflate variables
    mutate(def_RCEM = as.numeric(RCEM) / p_ind_va,  # returns
           def_RTAS = as.numeric(RTAS) / p_ind_va,
           def_EBTA = as.numeric(EBTA) / p_ind_va,  # earnings
           def_EBIT = as.numeric(EBIT) / p_ind_va,
           def_PL   = as.numeric(PL)   / p_ind_va,  # profits
           def_PLAT = as.numeric(PLAT) / p_ind_va,
           def_CF   = as.numeric(CF)   / p_ind_va,  # cash flow
           def_STAF = as.numeric(STAF) / p_ind_va,  # wages
           def_DEPR = as.numeric(DEPR) / p_ind_va,  # depreciation
           def_TOAS = as.numeric(TOAS) / p_ind_cp,  # assets
           def_FIAS = as.numeric(FIAS) / p_ind_cp,
           def_TURN = as.numeric(TURN) / p_ind_go   #sales
    ) %>%
   
    mutate(VA = as.numeric(EBTA) + as.numeric(STAF), #Imputed Value added (EBTA is earning before depreciation)
           VA_AD = as.numeric(EBIT) + as.numeric(STAF), #Imputed Value added (EBTA is earning before depreciation)
           
           LP= as.numeric(VA)/as.numeric(EMPL), # Labor productivity
           CP = as.numeric(VA)/(as.numeric(FIAS)+as.numeric(DEPR)), # capital productivity with fixed asset
           
           LP_AD= as.numeric(VA_AD)/as.numeric(EMPL), # Labor productivity after depreciation
           CP_AD = as.numeric(VA_AD)/as.numeric(FIAS), # capital productivity with fixed asset
           
           C_com = as.numeric(TOAS)/as.numeric(STAF), # capital intensity with total asset
           C_com_FI = as.numeric(FIAS)/as.numeric(STAF), # capital intensity with fixed asset
           
           RoC_G = as.numeric(CF)/(as.numeric(TOAS)+as.numeric(DEPR)), # gross profit rate with interest, CF = cash flow (gross profit + depreciation)
           RoC_G_FI = as.numeric(EBTA)/(as.numeric(FIAS)+as.numeric(DEPR)), # gross profit rate without interest 
           
           RoC_G_AD = as.numeric(PL)/as.numeric(TOAS), # gross profit rate with interest after depreciation
           RoC_G_AD_FI = as.numeric(EBIT)/as.numeric(FIAS), # gross profit rate without interest after depreciation 
           
           RoC_N = as.numeric(PLAT)/as.numeric(TOAS), # net profit rate (after tax) 
             
           WS = as.numeric(STAF)/as.numeric(VA), # wage share
           WS_AD = as.numeric(STAF)/as.numeric(VA_AD), # wage share after depreciation
            
           PW = as.numeric(EBTA)/as.numeric(STAF), # profit wage ratio
           PW_AD = as.numeric(EBIT)/as.numeric(STAF), # profit wage ratio after depreciation
           
           # And the same variables again deflated
           #  ...except for WS and PW as these are dimensionless and would be deflated with the same deflators
           
           def_VA = as.numeric(def_EBTA) + as.numeric(def_STAF), #Imputed Value added (EBTA is earning before depreciation)
           def_VA_AD = as.numeric(def_EBIT) + as.numeric(def_STAF), #Imputed Value added (EBTA is earning before depreciation)
           
           def_LP= as.numeric(def_VA)/as.numeric(EMPL), # Labor productivity
           def_CP = as.numeric(def_VA)/(as.numeric(def_FIAS)+as.numeric(def_DEPR)), # capital productivity with fixed asset
           
           def_LP_AD= as.numeric(def_VA_AD)/as.numeric(EMPL), # Labor productivity after depreciation
           def_CP_AD = as.numeric(def_VA_AD)/as.numeric(def_FIAS), # capital productivity with fixed asset
           
           def_C_com = as.numeric(def_TOAS)/as.numeric(def_STAF), # capital intensity with total asset
           def_C_com_FI = as.numeric(def_FIAS)/as.numeric(def_STAF), # capital intensity with fixed asset
           
           def_RoC_G = as.numeric(def_CF)/(as.numeric(def_TOAS)+as.numeric(def_DEPR)), # gross profit rate with interest, CF = cash flow (gross profit + depreciation)
           def_RoC_G_FI = as.numeric(def_EBTA)/(as.numeric(def_FIAS)+as.numeric(def_DEPR)), # gross profit rate without interest 
           
           def_RoC_G_AD = as.numeric(def_PL)/as.numeric(def_TOAS), # gross profit rate with interest after depreciation
           def_RoC_G_AD_FI = as.numeric(def_EBIT)/as.numeric(def_FIAS), # gross profit rate without interest after depreciation 
           
           def_RoC_N = as.numeric(def_PLAT)/as.numeric(def_TOAS) # net profit rate (after tax) 
             
           ) %>% 
  
    
    group_by(IDNR) %>% # group by firm index
    
    # firm size growth
    mutate(EMPL_g = (as.numeric(EMPL) - 
                       lag(as.numeric(EMPL),1))/lag(as.numeric(EMPL),1),    # wrt employment 
           FIAS_g = (as.numeric(FIAS) - 
                       lag(as.numeric(FIAS),1))/lag(as.numeric(FIAS),1),    # wrt fixed assets
           TOAS_g = (as.numeric(TOAS) - 
                       lag(as.numeric(TOAS),1))/lag(as.numeric(TOAS),1),    # wrt total assets
           SALE_g = (as.numeric(TURN) - 
                       lag(as.numeric(TURN),1))/lag(as.numeric(TURN),1),    # wrt sales
           
           # And the same variables again defalated
           
           def_FIAS_g = (as.numeric(def_FIAS) - 
                       lag(as.numeric(def_FIAS),1))/lag(as.numeric(def_FIAS),1),    # wrt fixed assets
           def_TOAS_g = (as.numeric(def_TOAS) - 
                       lag(as.numeric(def_TOAS),1))/lag(as.numeric(def_TOAS),1),    # wrt total assets
           def_SALE_g = (as.numeric(def_TURN) - 
                       lag(as.numeric(def_TURN),1))/lag(as.numeric(def_TURN),1)    # wrt sales
           ) %>% 
    
    # productivity growth
    
    mutate(CP_g = (CP - lag(CP,1))/lag(CP,1),
           CP_AD_g = (CP_AD - lag(CP_AD,1))/lag(CP_AD,1),                   # capital productivity undeflated
           LP_g = (LP - lag(LP,1))/lag(LP,1),
           LP_AD_g = (LP_AD - lag(LP_AD,1))/lag(LP_AD,1),                   # labor productivity undeflated
           
           def_CP_g = (def_CP - lag(def_CP,1))/lag(def_CP,1),
           def_CP_AD_g = (def_CP_AD - lag(def_CP_AD,1))/lag(def_CP_AD,1),   # capital productivity deflated
           def_LP_g = (def_LP - lag(def_LP,1))/lag(def_LP,1),
           def_LP_AD_g = (def_LP_AD - lag(def_LP_AD,1))/lag(def_LP_AD,1),   # labor productivity deflated
           
           Zeta = CP_g * (1-WS) + LP_g * WS,
           Zeta_AD = CP_AD_g * (1-WS_AD) + LP_AD_g * WS_AD,                 # TFP undeflated
           cpdef_Zeta = def_CP_g * (1-WS) + LP_g * WS,
           cpdef_Zeta_AD = def_CP_AD_g * (1-WS_AD) + LP_AD_g * WS_AD,       # TFP with only capital productivity deflated (labor productivity undeflated)
           def_Zeta = def_CP_g * (1-WS) + def_LP_g * WS,
           def_Zeta_AD = def_CP_AD_g * (1-WS_AD) + def_LP_AD_g * WS_AD      # TFP deflated (both capital and labor productivity)
           ) %>% # G_CP
    
    # etc
    
    mutate(PW_g = (PW - lag(PW,1))/lag(PW,1)) %>% #
    mutate(PW_AD_g = (PW_AD - lag(PW_AD,1))/lag(PW_AD,1)) #
    
  return(data_c)
}


# Country-wise master function. Reads data, calls function for cleaning and producing the balanced panels. Saves the panels
fun_read_by_country <- function(filename, country_name, country_abbrv, filename_nuts){             
  
  print(paste(unlist(country_name), collapse=" "))
  
  # 1. Read firm data as csv/Rda
  
  print(paste("         Commence reading data: ", filename))
  
  csv_filename = paste(filename, ".csv", sep="")
  #csv_filename = paste(filename, ".Rda", sep="")
  
  cdata <- fread(csv_filename, header=T, stringsAsFactors = F)
  
  save(cdata, file = paste(filename, ".Rda", sep = ""))
  #load(rfn)
  
  # 2. Assign NUTS codes by matching ZIP codes to ZIP-NUTS correspondence files

  if (sum(country_name == c("Ireland", "Malta", "Poland", "Portugal", "Sweden"))==1){
    cdata$ZIPCODE <- cdata$ZIPCODE # as.integer, as.character, or none
  } else if(sum(country_name == c("United Kingdom"))==1){
    cdata$ZIPCODE <- as.character(cdata$ZIPCODE)
  } else{
    cdata$ZIPCODE <- as.integer(cdata$ZIPCODE)
  }
  
  if (!is.na(filename_nuts)) {
    cdata_nuts <- read.csv(filename_nuts, header=T, sep=";", stringsAsFactors = F)
    #join nuts into data, 
    # ZIP code to NUTS code mapping files are available for most countries from #http://ec.europa.eu/eurostat/tercet/flatfiles.do
    #                                     for Albania, I created one since the mapping is straightforward
    #                                     for Monaco, Russia, Moldova, Bosnia and Herzegovina, Ukraine, Belarus, there are no NUTS codes and therefore no valid mapping; for Serbia, the NUTS codes exist but are apparently not published or at least not easily available.
    cdata <- merge(cdata, cdata_nuts, by.x="ZIPCODE", by.y="CODE", all.x=TRUE)
    #compute NUTS 1 2 3, 
    cdata$NUTS_2 <- substr(cdata$NUTS_3, 1, 4)
    cdata$NUTS_1 <- substr(cdata$NUTS_3, 1, 3)
  } else {
    cdata$NUTS_3 <- NA
    cdata$NUTS_2 <- NA
    cdata$NUTS_1 <- NA
  }

  # 3. Assign deflators by matching country, year, and NACE codes to industry level deflators from EUKLEMS
  
  load("DEF_KLEMS_2017ii.Rda")  # reads DataFrame object all_p_ind with columns c("nace2", "def_cd", "ctry", "year", "p_ind_va", "p_ind_go", "p_ind_cp")
  all_p_ind <- all_p_ind[all_p_ind$ctry==country_abbrv,]    # select country in deflator data frame
  all_p_ind$ctry <- NULL                                    # remove unused variables
  all_p_ind$def_cd <- NULL
  all_p_ind$nace2 <- as.numeric(all_p_ind$nace2)            # change NACE code to numeric to match firm data file structure
  colnames(all_p_ind) <- c("NACE_PRIM_CODE", "CLOSDATE_year", "p_ind_va", "p_ind_go", "p_ind_cp")   # replace colnames to match firm data file structure
  cdata <- merge(cdata, all_p_ind, by=c("NACE_PRIM_CODE", "CLOSDATE_year"), all.x=TRUE)             # merge deflators into firm data frame (data.table, actually)
  
  # 4. compute firm age from CLOSDATE_year and DATEINC_char
  cdata$CLOSDATE_year <- as.numeric(cdata$CLOSDATE_year)
  cdata$DATEINC_char <- as.character(cdata$DATEINC_char)
  cdata$DATEINC_year <- as.numeric(regmatches(cdata$DATEINC_char, gregexpr("\\d\\d\\d\\d+", cdata$DATEINC_char)))
  cdata[cdata$DATEINC_year > cdata$CLOSDATE_year]$DATEINC_year <- NA      # no negative firm ages
  cdata$Firm_Age <-  cdata$CLOSDATE_year - cdata$DATEINC_year
  
  # 5. clean data
  print("     Reading data complete ... commence cleaning")
  country_results <- fun_data_clean(cdata)
  
  # 6. put target variables in table form
  print("     Cleaning data complete ... commence preparing tables")
  
  ###### Table Form for variables 
  
  # General Info
  
  attach(country_results)
  
  Cleaned_dat_INDEX <- data.frame(
    IDNR = IDNR, Year = CLOSDATE_year, NUTS_1 = NUTS_1, NUTS_2 = NUTS_2,
    NUTS_3 = NUTS_3,  NACE_PRIM_CODE =  NACE_PRIM_CODE, CONSOL = CONSOL, 
    COMPCAT = COMPCAT, LSTATUS = LSTATUS, QUOTED = QUOTED, Firm_Age = Firm_Age, 
    EXCHRATE = EXCHRATE
  )
  
  Cleaned_dat_Profitability <- data.frame(
    IDNR = IDNR, Year = CLOSDATE_year, RoC_G = RoC_G, RoC_G_FI = RoC_G_FI,
    RoC_G_AD = RoC_G_AD, RoC_G_AD_FI = RoC_G_AD_FI, RoC_N = RoC_N,
    RoC_RCEM = RCEM, RoC_RTAS = RTAS
  )

  Cleaned_dat_Profitability_Deflated <- data.frame(
    IDNR = IDNR, Year = CLOSDATE_year, RoC_G = def_RoC_G, RoC_G_FI = def_RoC_G_FI,
    RoC_G_AD = def_RoC_G_AD, RoC_G_AD_FI = def_RoC_G_AD_FI, RoC_N = def_RoC_N,
    RoC_RCEM = def_RCEM, RoC_RTAS = def_RTAS
  )
  
  Cleaned_dat_Productivity <- data.frame(
    IDNR = IDNR, Year = CLOSDATE_year,  LP =  LP,  CP =  CP,  LP_AD = LP_AD, 
    CP_AD = CP_AD, CP_g = CP_g, CP_AD_g = CP_AD_g, LP_g = LP_g, 
    LP_AD_g = LP_AD_g, Zeta = Zeta, Zeta_AD = Zeta_AD
    )
  
  Cleaned_dat_Productivity_Deflated <- data.frame(
    IDNR = IDNR, Year = CLOSDATE_year, LP = def_LP, CP =  def_CP, LP_AD = def_LP_AD, 
    CP_AD = def_CP_AD, CP_g = def_CP_g, CP_AD_g = def_CP_AD_g, LP_g = def_LP_g, 
    LP_AD_g = def_LP_AD_g, Zeta = def_Zeta, Zeta_AD = def_Zeta_AD, cpdef_Zeta = cpdef_Zeta, 
    cpdef_Zeta_AD = cpdef_Zeta_AD
    )

  Cleaned_dat_Cost_Structure <- data.frame(
    IDNR = IDNR, Year = CLOSDATE_year,  WS = WS, WS_AD = WS_AD, PW = PW, 
    PW_AD = PW_AD,  PW_g = PW_g,  PW_AD_g = PW_AD_g
    )

  Cleaned_dat_Firm_Size <- data.frame(
    IDNR = IDNR, Year = CLOSDATE_year, SALE = TURN, EMPL =  EMPL, 
    TOAS = TOAS, FIAS = FIAS, VA = VA, EMPL_g = EMPL_g, FIAS_g = FIAS_g, TOAS_g = TOAS_g, 
    SALE_g = SALE_g
  )

  Cleaned_dat_Firm_Size_Deflated <- data.frame(
    IDNR = IDNR, Year = CLOSDATE_year, SALE = def_TURN, EMPL = EMPL, 
    TOAS = def_TOAS, FIAS = def_FIAS, VA = def_VA, EMPL_g = EMPL_g, FIAS_g = def_FIAS_g, 
    TOAS_g = def_TOAS_g, SALE_g = def_SALE_g
  )
   
  # 7. save panels
  # save undeflated series
  print("     Saving panels")    
  save(
    Cleaned_dat_INDEX,
    Cleaned_dat_Profitability,
    Cleaned_dat_Productivity,
    Cleaned_dat_Cost_Structure, 
    Cleaned_dat_Firm_Size, 
    
    file=paste("panels_Undeflated_J!&", paste(unlist(country_name), collapse=""), ".Rda", sep="")  # either panels_ or consolidated_panels
  )

  # overwrite variables for saving of deflated series
  Cleaned_dat_Profitability <- Cleaned_dat_Profitability_Deflated
  Cleaned_dat_Productivity <- Cleaned_dat_Productivity_Deflated
  Cleaned_dat_Firm_Size <- Cleaned_dat_Firm_Size_Deflated
  
  # save deflated series
  save(
    Cleaned_dat_INDEX,
    Cleaned_dat_Profitability,
    Cleaned_dat_Productivity,
    Cleaned_dat_Cost_Structure, 
    Cleaned_dat_Firm_Size, 
    
    file=paste("panels_J!&", paste(unlist(country_name), collapse=""), ".Rda", sep="")  # either panels_ or consolidated_panels
  )
  
  detach(country_results)
}



# main entry point

#####filenames
# This is the full list of countries 
country_names <- c('Albania', 'Austria', 'Belarus', 'Belgium', 'Bosnia and Herzegovina', 
                   'Bulgaria', 'Croatia', 'Cyprus', 'Czech Republic', 'Denmark', 'Estonia',
                   'Finland', 'France', 'Germany', 'Greece', 'Hungary', 'Iceland', 
                   'Ireland', 'Italy', 'Kosovo', 'Latvia', 'Liechtenstein', 'Lithuania',
                   'Luxembourg', 'Macedonia, FYR', 'Malta', 'Monaco', 'Montenegro', 
                   'Netherlands', 'Norway', 'Poland', 'Portugal', 'Moldova', 'Romania', 
                   'Russian Federation', 'Serbia', 'Slovakia', 'Slovenia', 'Spain', 
                   'Sweden', 'Switzerland', 'Turkey', 'Ukraine', 'United Kingdom')
country_abbrv = c("AL", "AT", "BY", "BE", "BH", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", 
                  "FR", "DE", "GR", "HU", "IS", "IE", "IT", "XK", "LV", "LI", "LT", "LU", 
                  "MK", "MT", "MC", "ME", "NL", "NO", "PL", "PT", "RO", "RU", "RS", "SK", 
                  "SI", "ES", "SE", "CH", "TK", "UA", "UK")
filenames <- c('Albania', 'Austria', 'Belarus', 'Belgium', 'BOSNIA AND HERZEGOVINA', 
               'BULGARIA', 'CROATIA', 'CYPRUS', 'CZECH REPUBLIC', 'DENMARK', 'ESTONIA',
               'FINLAND', 'France', 'GERMANY', 'GREECE', 'HUNGARY', 'ICELAND', 'IRELAND', 
               'ITALY', 'KOSOVO', 'LATVIA', 'LIECHTENSTEIN', 'LITHUANIA', 'LUXEMBOURG',
               'MACEDONIA (FYROM)', 'MALTA', 'MONACO', 'MONTENEGRO', 'NETHERLANDS', 
               'NORWAY', 'POLAND', 'PORTUGAL', 'REPUBLIC OF MOLDOVA', 'ROMANIA', 
               'RUSSIAN FEDERATION', 'SERBIA', 'SLOVAKIA', 'SLOVENIA', 'SPAIN', 'SWEDEN', 
               'SWITZERLAND', 'TURKEY', 'UKRAINE', 'UNITED KINGDOM')
filenames_nuts <- c('NUTS/pc2016_al_NUTS-2013_v2.3.csv','NUTS/pc2016_at_NUTS-2013_v2.3.csv',
                    NA,'NUTS/pc2016_be_NUTS-2013_v2.3.csv', NA,'NUTS/pc2016_bg_NUTS-2013_v2.3.csv',
                    'NUTS/pc2016_hr_NUTS-2013_v2.3.csv', 'NUTS/pc2016_cy_NUTS-2013_v2.3.csv', 
                    'NUTS/pc2016_cz_NUTS-2013_v2.3.csv', 'NUTS/pc2016_dk_NUTS-2013_v2.3.csv', 
                    'NUTS/pc2016_ee_NUTS-2013_v2.3.csv', 'NUTS/pc2016_fi_NUTS-2013_v2.3.csv',
                    'NUTS/pc2016_fr_NUTS-2016_modified.csv', 'NUTS/pc2016_de_NUTS-2016_modified.csv',
                    'NUTS/pc2016_el_NUTS-2013_v2.3.csv', 'NUTS/pc2016_hu_NUTS-2013_v2.3.csv', 
                    'NUTS/pc2016_is_NUTS-2013_v2.3.csv', 'NUTS/pc2016_ie_NUTS-2013_v2.3.csv', 
                    'NUTS/pc2016_it_NUTS-2013_v2.3.csv', NA, 'NUTS/pc2016_lv_NUTS-2013_v2.3.csv', 
                    'NUTS/pc2016_li_NUTS-2013_v2.3.csv', 'NUTS/pc2016_lt_NUTS-2013_v2.3.csv', 
                    'NUTS/pc2016_lu_NUTS-2013_v2.3.csv', 'NUTS/pc2016_mk_NUTS-2013_v2.3.csv', 
                    'NUTS/pc2016_mt_NUTS-2013_v2.3.csv', NA, 'NUTS/pc2016_me_NUTS-2013_v2.3.csv', 
                    'NUTS/pc2016_nl_NUTS3-2013_v_2.5.csv', 'NUTS/pc2016_no_NUTS-2013_v2.3.csv', 
                    'NUTS/pc2016_pl_NUTS-2013_v2.3.csv', 'NUTS/pc2016_pt_NUTS-2013_v2.3.csv', NA, 
                    'NUTS/pc2016_ro_NUTS-2013_v2.3.csv', NA, 'NUTS/pc2018_rs_NUTS-2013_v2.3.csv', 
                    'NUTS/pc2016_sk_NUTS-2013_v2.5.csv', 'NUTS/pc2016_si_NUTS-2013_v2.3.csv', 
                    'NUTS/pc2016_es_NUTS-2013_v2.3.csv', 'NUTS/pc2016_se_NUTS-2013_v2.3.csv', 
                    'NUTS/pc2016_ch_NUTS-2013_v2.4.csv', 'NUTS/pc2016_tr_NUTS-2013_v2.3.csv', NA, 
                    'NUTS/pc2016_uk_NUTS-2013_v2.3_modified.csv')

## Since computing all of them will be time- and memory consuming, we should normally use subsets, like so:
#country_names <- country_names[c(1:3,5)]
#filenames <- filenames[c(1:3,5)]
# ...


print("Commence reading and cleaning data...")

for (i in 1:length(filenames)) {
  #tryCatch({
  fun_read_by_country(filenames[[i]], country_names[[i]], country_abbrv[[i]], filenames_nuts[[i]])      
  #}, error=function(e){})
  # function saves directly, so no need to save the return value
}

print("All complete")

