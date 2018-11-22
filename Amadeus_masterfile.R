## ------------------------------------------------------------------------

# Load libraries

library(data.table)
library(plyr)
library(dplyr)



# Functions
# Functions

# Data cleaning function
fun_data_clean <- function(dat){
  
  data_c <- dat %>%
    filter(!is.na(CLOSDATE_year)) %>% # year
    filter(!is.na(IDNR)) %>% # firm indicator 
    filter(CLOSDATE_year >= 2006 & CLOSDATE_year <= 2015) %>% # year cut 
    filter(CONSOL == 'C1' | CONSOL == 'U1') %>% #  consolidate or unconsolidate with no companion statements 
    distinct(IDNR, CLOSDATE_year, .keep_all = TRUE) %>% # remove duplicated rows
    
    mutate(EMPL = replace(EMPL, EMPL <= 0, NA),
           TOAS = replace(TOAS, TOAS <= 0, NA),
           STAF = replace(STAF, STAF < 0, NA),
           TURN = replace(TURN, TURN < 0, NA)) %>% # remove nonsensical values
    
    
    arrange(IDNR, CLOSDATE_year) %>% # arranging data by firm and year
    
    
   
    mutate(VA = EBTA + STAF, #Imputed Value added (EBTA is earning before depreciation)
           VA_AD = EBIT + STAF, #Imputed Value added (EBTA is earning before depreciation)
           
           LP= VA/EMPL, # Labor productivity
           CP = VA/(FIAS+DEPR), # capital productivity with fixed asset
           
           LP_AD= VA_AD/EMPL, # Labor productivity after depreciation
           CP_AD = VA_AD/FIAS, # capital productivity with fixed asset
           
           C_com = TOAS/STAF, # capital intensity with total asset
           C_com_FI = FIAS/STAF, # capital intensity with fixed asset
           
           RoC_G = CF/(TOAS+DEPR), # gross profit rate with interest, CF = cash flow (gross profit + depreciation)
           RoC_G_FI = EBTA/(FIAS+DEPR), # gross profit rate without interest 

           RoC_G_AD = PL/TOAS, # gross profit rate with interest after depreciation
           RoC_G_AD_FI = EBIT/FIAS, # gross profit rate without interest after depreciation 
           
           RoC_N = PLAT/TOAS, # net profit rate (after tax) 
             
           WS = STAF/VA, # wage share
           WS_AD = STAF/VA_AD, # wage share after depreciation
            
           PW = EBTA/STAF, # profit wage ratio
           PW_AD = EBIT/STAF # profit wage ratio after depreciation
           ) %>% 
  
    
    group_by(IDNR) %>% # group by firm index
    
    # firm size growth
    mutate(EMPL_g = (EMPL - lag(EMPL,1))/lag(EMPL,1), 
           FIAS_g = (FIAS - lag(FIAS,1))/lag(FIAS,1),
           TOAS_g = (TOAS - lag(TOAS,1))/lag(TOAS,1),
           SALE_g = (TURN - lag(TURN,1))/lag(TURN,1)
           ) %>% 
    
    # productivity growth
    
    mutate(CP_g = (CP - lag(CP,1))/lag(CP,1),
           CP_AD_g = (CP_AD - lag(CP_AD,1))/lag(CP_AD,1),
           LP_g = (LP - lag(LP,1))/lag(LP,1),
           LP_AD_g = (LP_AD - lag(LP_AD,1))/lag(LP_AD,1),
           
           Zeta = CP_g * (1-WS) + LP_g * WS,
           Zeta_AD = CP_AD_g * (1-WD_AD) + LP_AD_g * WS_AD
           ) %>% # G_CP
    
    # etc
    
    mutate(PW_g = (PW - lag(PW,1))/lag(PW,1)) %>% #
    mutate(PW_AD_g = (PW_AD - lag(PW_AD,1))/lag(PW_AD,1)) %>% #
    
  return(data_c)
}


# Country-wise master function. Reads data, calls function for cleaning and producing the balanced panels. Saves the panels
fun_read_by_country <- function(filename_list, country_name_list, filename_nuts_list){             
  
  # 0. shape input lists as vector (This enables to deal with either single countries at a time or with lists or vectors of countries.)
  filename_list <- c(filename_list)
  country_name_list <- c(country_name_list)
  filename_nuts_list <- c(filename_nuts_list)
  country_list <- list()
  print(paste(unlist(country_name_list), collapse=" "))
  #print(filename_list)
  
  # 1. Read data
  fn = filename_list
  fn <- filenames[i]
  filename_list <- filenames[[i]] 
  country_name_list <- country_names[[i]]
  filename_nuts_list <- filenames_nuts[[i]]
  
  print(paste("         Commence reading data: ", fn))
  
  rfn = paste(fn, ".csv", sep="")
  
  cdata <- fread(rfn, header=T)
  
  if (sum(country_name_list == c("Ireland", "Malta", "Poland", "Portugal", "Sweden"))==1){
    cdata$ZIPCODE <- cdata$ZIPCODE # as.integer, as.character, or none
  } else if(sum(country_name_list == c("United Kingdom"))==1){
    cdata$ZIPCODE <- as.character(cdata$ZIPCODE)
  } else{
    cdata$ZIPCODE <- as.integer(cdata$ZIPCODE)
  }
  
  
  #cdata$ZIPCODE <- as.integer(cdata$ZIPCODE) # as.integer, as.character, or none
  
  fn_nuts = filename_nuts_list
  #fn_nuts = filenames_nuts[[2]]
  
  if (!is.na(fn_nuts)) {
    cdata_nuts <- read.csv(fn_nuts, header=T, sep=";", stringsAsFactors = F)
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
  
  # 2. compute firm age from CLOSDATE_year and DATEINC_char
  #cdata$DATEINC_year <- as.numeric(substr(cdata$DATEINC_char, 7, 10))
  cdata$CLOSDATE_year <- as.numeric(cdata$CLOSDATE_year)
  #DATEINC_errors <- cdata$DATEINC_year < 1400                             # no impossible incorporation dates before 1400
  #cdata[DATEINC_errors]$DATEINC_year <- as.numeric(regmatches(cdata[DATEINC_errors]$DATEINC_char, gregexpr("\\d\\d\\d\\d+", cdata[DATEINC_errors]$DATEINC_char)))
  cdata$DATEINC_char <- as.character(cdata$DATEINC_char)
  cdata$DATEINC_year <- as.numeric(regmatches(cdata$DATEINC_char, gregexpr("\\d\\d\\d\\d+", cdata$DATEINC_char)))
  cdata[cdata$DATEINC_year > cdata$CLOSDATE_year]$DATEINC_year <- NA      # no negative firm ages
  cdata$Firm_Age <-  cdata$CLOSDATE_year - cdata$DATEINC_year
  
  # This bit is not important for the country-wise handling of the data; only if lists larger than one are collected at the same time
  # 3. join to list called country_list; see below!
  country_list <- append(country_list, list(cdata))
  

  # 4. clean data
  print("     Reading data complete ... commence cleaning")
  
  country_list_c <- lapply(country_list, fun_data_clean, def_d, def_d_list)
  
  # 5. put target variables in table form
  print("     Cleaning data complete ... commence preparing tables")
  
  ###### Table Form for variables 
  
  # General Info
  
  attach(country_list_c[[1]])
  
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
  
  Cleaned_dat_Productivity <- data.frame(
    IDNR = IDNR, Year = CLOSDATE_year,  LP =  LP,  CP =  CP,  LP_AD = LP_AD, 
    CP_AD = CP_AD, CP_g = CP_g, CP_AD_g = CP_AD_g, LP_g = LP_g, 
    LP_AD_g = LP_AD_g, Zeta = Zeta, Zeta_AD = Zeta_AD
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
   
  
  # 8. save panels
  print("     Saving panels")    
  save(
    Cleaned_dat_INDEX,
    Cleaned_dat_Profitability,
    Cleaned_dat_Productivity,
    Cleaned_dat_Cost_Structure, 
    Cleaned_dat_Firm_Size, 
    
    file=paste("panels_J!&", paste(unlist(country_name_list), collapse=""), ".Rda", sep="")  # either panels_ or consolidated_panels
  )
  
  detach(country_list_c[[1]])
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
#filenames_nuts <- filenames_nuts[c(1:3,5)]
#country_names <- country_names[c(1,3)]
#filenames <- filenames[c(1,3)]
#filenames_nuts <- filenames_nuts[c(1,3)]
## Montenegro and Moldova:
#country_names <- country_names[c(28,33)]
#filenames <- filenames[c(28,33)]
#filenames_nuts <- filenames_nuts[c(28,33)]
## Serbia:
#country_names <- country_names[c(36)]
#filenames <- filenames[c(36)]
#filenames_nuts <- filenames_nuts[c(36)]


## Or you could have a few small countries handled together
#country_names <- list('Belarus', 'Austria', list('Albania', 'Bosnia and Herzegovina', 'Montenegro', 'Kosovo', 'Macedonia, FYR'), 'Belgium', 'Bulgaria')
#filenames <- list('Belarus', 'Austria', list('Albania', 'BOSNIA AND HERZEGOVINA',  'MONTENEGRO', 'KOSOVO', 'MACEDONIA (FYROM)'), 'Belgium', 'BULGARIA')
#filenames_nuts <- list('NUTS/pc2016_be_NUTS-2013_v2.3.csv', 'NUTS/pc2016_at_NUTS-2013_v2.3.csv', list('NUTS/pc2016_al_NUTS-2013_v2.3.csv', NA, 'NUTS/pc2016_mk_NUTS-2013_v2.3.csv', NA, 'NUTS/pc2016_me_NUTS-2013_v2.3.csv'), NA, 'NUTS/pc2016_bg_NUTS-2013_v2.3.csv')


## ------------------------------------------------------------------------



print("Commence reading and cleaning data...")

for (i in 1:length(filenames)) {
  #tryCatch({
  fun_read_by_country(filenames[[i]], country_names[[i]], filenames_nuts[[i]])      
  #}, error=function(e){})
  # function saves directly, so no need to save the return value
}

print("All complete")



## ------------------------------------------------------------------------
