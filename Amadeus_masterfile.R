## ------------------------------------------------------------------------

# Load libraries

library(data.table)
library(plyr)
library(dplyr)



# Functions

# Put deflator into data.frame format
fun_def_frame <- function(x){
    def <- data.frame(CLOSDATE_year = c(2006:2015), deflator = as.numeric(x)) # year ahead
    return(def)
}

# Data cleaning function
fun_data_clean <- function(dat, def_d, def_d_list){

    #print(dat$COUNTRY[1])
    data_c <- dat
    upper_case_rownames <- toupper(rownames(def_d))
    upper_case_alternative <- toupper(rownames(def_d))
    upper_case_alternative[upper_case_alternative == "MACEDONIA, FYR"] <- "MACEDONIA (FYROM)"
    upper_case_alternative[upper_case_alternative == "MOLDOVA"] <- "REPUBLIC OF MOLDOVA"

    #print(def_d)
    #def_ind <- which(rownames(def_d) == data_c$COUNTRY[1] | upper_case_rownames ==  data_c$COUNTRY[1] | upper_case_alternative == data_c$COUNTRY[1])
    #print("def_ind")
    #print(def_ind)
    #print("def_d_list")
    #print(def_d_list)
    #print("rownames def_d")
    #print(rownames(def_d))
    #print("upper_case_rownames")
    #print(upper_case_rownames)
    #print("data_c$COUNTRY[1]")
    #print(data_c$COUNTRY[1])
    def_pick <- def_d_list
    #print("selected successfully")

    #dim(cdata)
    #data_c <- cdata


    #data_c <- data_c[!(duplicated(data_c[,c(1,5)]) | duplicated(data_c[,c(1,5)], fromLast = TRUE)), ]



    data_c <- data_c %>%
      filter(!is.na(CLOSDATE_year)) %>% # year
      filter(!is.na(NAICS_CORE_CODE)) %>% # naics code
      filter(!is.na(IDNR)) %>% # firm indicator 
      filter(CLOSDATE_year >= 2006 & CLOSDATE_year <= 2015) %>% # year cut 
      #filter(REPBAS == 'Unconsolidated data') %>% # only either consolidate or unconsolidate   
      filter(CONSOL == 'C1' | 'U1') %>% #  consolidate or unconsolidate with no companion statements 
      distinct(IDNR, CLOSDATE_year, .keep_all = TRUE) %>% # removing duplicated rows
      mutate(EMPL = replace(EMPL, EMPL <= 0, NA)) %>%
      mutate(TOAS = replace(TOAS, TOAS <= 0, NA)) %>%
      mutate(STAF = replace(STAF, STAF < 0, NA)) %>%
      mutate(TURN = replace(TURN, TURN < 0, NA)) %>%
      mutate(RD = replace(RD, RD < 0, NA)) %>%
      #filter(!is.na(NUTS_3)) %>%
  
      arrange(IDNR, CLOSDATE_year) %>% # arranging data by firm and year
  
      mutate(NAICS_two_digit = substr(NAICS_CORE_CODE, 1, nchar(NAICS_CORE_CODE)-2)) %>% # changing naics code to two digit       

      filter(NAICS_two_digit != 52) %>% #remove finanacial companies                  
      left_join(def_pick, by = c("CLOSDATE_year")) %>% # joining with deflator file 
      mutate(VA_imp = EBIT + STAF) %>% # imputed value added
      mutate(LP_imp= VA_imp/EMPL) %>% # Imputed Labor Productivity
      mutate(LP_imp_conv= LP_imp*EXCHRATE) %>% # Imputed Labor Productivity
      mutate(LP_imp_conv_def= LP_imp_conv/deflator*100) %>% # Imputed Labor Productivity
      mutate(CP_imp= VA_imp/TOAS) %>% # Imputed Capital Productivity using total Asset 
      mutate(PW_ratio = EBIT/STAF) %>% # profit-wage ratio 
      mutate(WS = STAF/VA_imp) %>% # wage share
      mutate(PS = EBIT/VA_imp) %>% # profit sahre
      mutate(C_com = TOAS/STAF) %>% # composition of capital 
      mutate(RoC = EBIT/TOAS) %>%  # profit rate using total asset
      mutate(RoC_fix = EBIT/FIAS) %>%  # profit rate using fixed asset
  
       
      mutate(RD_sales = RD/TURN) %>%
      mutate(RD_va = RD/VA_imp) %>%
  
       
      mutate(TURN_conv_def = TURN*EXCHRATE/deflator*100) %>% # 
  
      mutate(TOAS_conv_def = TOAS*EXCHRATE/deflator*100) %>% # 
      mutate(CUAS_conv_def = CUAS*EXCHRATE/deflator*100) %>% #
      mutate(FIAS_conv_def = FIAS*EXCHRATE/deflator*100) %>% #
      mutate(IFAS_conv_def = IFAS*EXCHRATE/deflator*100) %>% #
      mutate(TFAS_conv_def = TFAS*EXCHRATE/deflator*100) %>% #
      mutate(OCAS_conv_def = OCAS*EXCHRATE/deflator*100) %>% #
      mutate(OFAS_conv_def = OFAS*EXCHRATE/deflator*100) %>% #
  
  
      group_by(IDNR) %>%
  
      mutate(EMPL_change = (EMPL - lag(EMPL,1))/lag(EMPL,1)) %>% # G_empl   
  
      mutate(C_com_change = (C_com - lag(C_com,1))/lag(C_com,1)) %>% #G_C_com
      mutate(PW_ratio_change = (PW_ratio - lag(PW_ratio,1))/lag(PW_ratio,1)) %>% #
      mutate(CP_change = (CP_imp - lag(CP_imp,1))/lag(CP_imp,1)) %>% # G_CP
      mutate(LP_change = (LP_imp_conv_def - lag(LP_imp_conv_def,1))/lag(LP_imp_conv_def,1)) %>% # G_lp
                                   
      mutate(Zeta = CP_change * PS + LP_change * WS) %>%     
  
  
      mutate(TOAS_change = (TOAS_conv_def - lag(TOAS_conv_def,1))/lag(TOAS_conv_def,1)) %>% # Total Asset Change
      mutate(TURN_change = (TURN_conv_def - lag(TURN_conv_def,1))/lag(TURN_conv_def,1)) %>% # TURN
  
  
      # compute log rates of change: log(x_{t}/x{t-1})
  
      mutate(EMPL_lroc = log(EMPL / lag(EMPL,1))) %>% # employment   
      mutate(C_com_lroc = log(C_com / lag(C_com,1))) %>% # C_com   
      mutate(PW_ratio_lroc = log(PW_ratio / lag(PW_ratio,1))) %>% # profit wage ratio
      mutate(CP_lroc = log(CP_imp / lag(CP_imp,1))) %>% # capital productivity   
      mutate(LP_lroc = log(LP_imp_conv_def / lag(LP_imp_conv_def,1))) %>% # labor productivity
      mutate(TOAS_lroc = log(TOAS_conv_def / lag(TOAS_conv_def,1))) %>% # total asset change   
      mutate(TURN_lroc = log(TURN_conv_def / lag(TURN_conv_def,1))) # TURN   
  
      # there is not ZETA_lroc.
      # Zeta is computed like so: Z = CP_1 * PS / CP_0 - PS + LP_1 * WS / LP_0 - WS = -PS-WS + CP_1/CP_0 + LP_1/LP_0 = -PS-WS + exp(CP_lroc) + exp(LP_lroc)
      # The problem is, that shares cannot be nicely expressed in logarithms, i.e. you cannot express log(a+b) as a function of log(a) and log(b) [only of log(a) and log(1-b/a)].
      # Therefore I am almost sure that this measure has no non-problematic lroc's equivalent. But let's try:

    return(data_c)
}

# reshape list of data frames with entries {IDNR, CLOSDATE_year, <dat_var>} to IDNR x CLOSDATE_year matrix with entries <dat_var>
fun_Mat_gen <- function(dat_list, dat_var) {             
    dat_var_table <- list()
    for(i in 1:length(dat_list)){
        ok <- dat_list[[i]]
        setDT(ok)
        dat_var_table[[i]] <- dcast(ok, IDNR ~ CLOSDATE_year, value.var = dat_var, fun.aggregate = mean)
    }
    return(dat_var_table)
}

# Balacned Panel for 10 years
fun_gen_bp <- function(x){
      ok <- x[,-c(1)]
      zz <- rowSums(!is.na(ok))
      ten_year <- x[which(zz == 10),]
      
      return(ten_year)                                                                 
}

# Country-wise master function. Reads data, calls function for cleaning and producing the balanced panels. Saves the panels
fun_read_by_country <- function(filename_list, country_name_list, filename_nuts_list){             
    #i <- 33
    #filename_list <- filenames[[i]]
    #country_name_list <- country_names[[i]]
    #filename_nuts_list <- filenames_nuts[[i]]

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
  
  
    # 4. handle deflator 
    deflator_d <- read.csv("deflator.csv", header = T, skip = 3, stringsAsFactors = F)
    deflator_d$Country.Name[which(deflator_d$Country.Name == 'Slovak Republic')] = 'Slovakia'

    def_d <- deflator_d[deflator_d$Country.Name %in% c("United States"),]
    #def_d <- deflator_d[deflator_d$Country.Name %in% c(country_names),]

    #rownames(def_d) <- c(country_names)

    def_d <- def_d[,c(c(ncol(def_d)-12):c(ncol(def_d)-3))]
    def_d_list <- data.frame(CLOSDATE_year = c(2006:2015), deflator = as.numeric(def_d)) 

    # 5. clean data
    print("     Reading data complete ... commence cleaning")

    country_list_c <- lapply(country_list, fun_data_clean, def_d, def_d_list)

    # 6. put target variables in table form
    print("     Cleaning data complete ... commence preparing tables")

    ###### Table Form for variables 

    # General Info

    attach(country_list_c[[1]])

    Cleaned_dat_INDEX <- data.frame(
        IDNR = IDNR, Year = CLOSDATE_year, NUTS_1 = NUTS_1, NUTS_2 = NUTS_2, NUTS_3 = NUTS_3, NAICs_two_digit = NAICS_two_digit, NACE_PRIM_CODE =  NACE_PRIM_CODE, CONSOL = CONSOL, COMPCAT = COMPCAT, LSTATUS = LSTATUS, QUOTED = QUOTED, Firm_Age = Firm_Age, EXCHRATE = EXCHRATE, Deflator = deflator
    )

    Cleaned_dat_Profitability <- data.frame(
        IDNR = IDNR, Year = CLOSDATE_year, RoC = RoC, RoC_fix = RoC_fix, RoC_RCEM = RCEM, RoC_RTAS = RTAS
    )

    Cleaned_dat_Productivity <- data.frame(
        IDNR = IDNR, Year = CLOSDATE_year, LP = LP_imp_conv_def, CP = CP_imp, LP_change = LP_change, CP_change = CP_change, Zeta = Zeta
    )

    Cleaned_dat_cost_structure <- data.frame(
        IDNR = IDNR, Year = CLOSDATE_year, WS = WS, PS = PS, PW_ratio = PW_ratio, C_com = C_com, PW_ratio_change = PW_ratio_change , PW_ratio_lr = PW_ratio_lroc
    )

    Cleaned_dat_firm_size <- data.frame(
        IDNR = IDNR, Year = CLOSDATE_year, SALE = as.numeric(TURN_conv_def), EMPL =  as.numeric(EMPL), TOAS = as.numeric(TOAS_conv_def), SALE_change = TURN_change, EMPL_change = EMPL_change,  VA = as.numeric(VA_imp),
        SALE_lr = as.numeric(TURN_lroc), EMPL_lr = as.numeric(EMPL_lroc), TOAS_lr = as.numeric(TOAS_lroc)
    )

     
    Cleaned_dat_RD <- data.frame(
        IDNR = IDNR, Year = CLOSDATE_year, RD = as.numeric(RD), SALE = as.numeric(TURN_conv_def), EMPL =  as.numeric(EMPL), TOAS = as.numeric(TOAS_conv_def), TOAS =  as.numeric(TOAS), CUAS =  as.numeric(CUAS), FIAS =  as.numeric(FIAS), IFAS =  as.numeric(IFAS), TFAS =  as.numeric(TFAS), OCAS =  as.numeric(OCAS), OFAS =  as.numeric(OFAS)
    )
 

    # 8. save panels
    print("     Saving panels")    
    save(
        Cleaned_dat_INDEX,
        Cleaned_dat_Profitability,
        Cleaned_dat_Productivity,
        Cleaned_dat_cost_structure, 
        Cleaned_dat_firm_size, 
        Cleaned_dat_RD,

        file=paste("panels_J!&", paste(unlist(country_name_list), collapse=""), ".Rda", sep="")  # either panels_ or consolidated_panels
    )

    detach(country_list_c[[1]])
}
# main entry point


#####filenames
# This is the full list of countries 
country_names <- c('Albania', 'Austria', 'Belarus', 'Belgium', 'Bosnia and Herzegovina', 'Bulgaria', 'Croatia', 'Cyprus', 'Czech Republic', 'Denmark', 'Estonia', 'Finland', 'France', 'Germany', 'Greece', 'Hungary', 'Iceland', 'Ireland', 'Italy', 'Kosovo', 'Latvia', 'Liechtenstein', 'Lithuania', 'Luxembourg', 'Macedonia, FYR', 'Malta', 'Monaco', 'Montenegro', 'Netherlands', 'Norway', 'Poland', 'Portugal', 'Moldova', 'Romania', 'Russian Federation', 'Serbia', 'Slovakia', 'Slovenia', 'Spain', 'Sweden', 'Switzerland', 'Turkey', 'Ukraine', 'United Kingdom')
filenames <- c('Albania', 'Austria', 'Belarus', 'Belgium', 'BOSNIA AND HERZEGOVINA', 'BULGARIA', 'CROATIA', 'CYPRUS', 'CZECH REPUBLIC', 'DENMARK', 'ESTONIA', 'FINLAND', 'France', 'GERMANY', 'GREECE', 'HUNGARY', 'ICELAND', 'IRELAND', 'ITALY', 'KOSOVO', 'LATVIA', 'LIECHTENSTEIN', 'LITHUANIA', 'LUXEMBOURG', 'MACEDONIA (FYROM)', 'MALTA', 'MONACO', 'MONTENEGRO', 'NETHERLANDS', 'NORWAY', 'POLAND', 'PORTUGAL', 'REPUBLIC OF MOLDOVA', 'ROMANIA', 'RUSSIAN FEDERATION', 'SERBIA', 'SLOVAKIA', 'SLOVENIA', 'SPAIN', 'SWEDEN', 'SWITZERLAND', 'TURKEY', 'UKRAINE', 'UNITED KINGDOM')
filenames_nuts <- c('NUTS/pc2016_al_NUTS-2013_v2.3.csv','NUTS/pc2016_at_NUTS-2013_v2.3.csv', NA,'NUTS/pc2016_be_NUTS-2013_v2.3.csv', NA,'NUTS/pc2016_bg_NUTS-2013_v2.3.csv', 'NUTS/pc2016_hr_NUTS-2013_v2.3.csv', 'NUTS/pc2016_cy_NUTS-2013_v2.3.csv', 'NUTS/pc2016_cz_NUTS-2013_v2.3.csv', 'NUTS/pc2016_dk_NUTS-2013_v2.3.csv', 'NUTS/pc2016_ee_NUTS-2013_v2.3.csv', 'NUTS/pc2016_fi_NUTS-2013_v2.3.csv', 'NUTS/pc2016_fr_NUTS-2016_modified.csv', 'NUTS/pc2016_de_NUTS-2016_modified.csv', 'NUTS/pc2016_el_NUTS-2013_v2.3.csv', 'NUTS/pc2016_hu_NUTS-2013_v2.3.csv', 'NUTS/pc2016_is_NUTS-2013_v2.3.csv', 'NUTS/pc2016_ie_NUTS-2013_v2.3.csv', 'NUTS/pc2016_it_NUTS-2013_v2.3.csv', NA, 'NUTS/pc2016_lv_NUTS-2013_v2.3.csv', 'NUTS/pc2016_li_NUTS-2013_v2.3.csv', 'NUTS/pc2016_lt_NUTS-2013_v2.3.csv', 'NUTS/pc2016_lu_NUTS-2013_v2.3.csv', 'NUTS/pc2016_mk_NUTS-2013_v2.3.csv', 'NUTS/pc2016_mt_NUTS-2013_v2.3.csv', NA, 'NUTS/pc2016_me_NUTS-2013_v2.3.csv', 'NUTS/pc2016_nl_NUTS3-2013_v_2.5.csv', 'NUTS/pc2016_no_NUTS-2013_v2.3.csv', 'NUTS/pc2016_pl_NUTS-2013_v2.3.csv', 'NUTS/pc2016_pt_NUTS-2013_v2.3.csv', NA, 'NUTS/pc2016_ro_NUTS-2013_v2.3.csv', NA, 'NUTS/pc2018_rs_NUTS-2013_v2.3.csv', 'NUTS/pc2016_sk_NUTS-2013_v2.5.csv', 'NUTS/pc2016_si_NUTS-2013_v2.3.csv', 'NUTS/pc2016_es_NUTS-2013_v2.3.csv', 'NUTS/pc2016_se_NUTS-2013_v2.3.csv', 'NUTS/pc2016_ch_NUTS-2013_v2.4.csv', 'NUTS/pc2016_tr_NUTS-2013_v2.3.csv', NA, 'NUTS/pc2016_uk_NUTS-2013_v2.3_modified.csv')

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

#setwd("D:/Firm-Level Data/Amadeus")
#setwd("~/eecon/datalakeX/Amadeus")

#setwd("D:/Firm-Level Data/Amadeus")

print("Commence reading and cleaning data...")

for (i in 1:length(filenames)) {
    #tryCatch({
    fun_read_by_country(filenames[[i]], country_names[[i]], filenames_nuts[[i]])      
    #}, error=function(e){})
    # function saves directly, so no need to save the return value
}

print("All complete")



## ------------------------------------------------------------------------
##
##


#i <- match(c("United Kingdom"), country_names)
#country_names[[i]]
#fun_read_by_country(filenames[[i]], country_names[[i]], filenames_nuts[[i]])    
##
##
##

