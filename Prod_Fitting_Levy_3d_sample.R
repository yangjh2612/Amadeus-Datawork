# Script to produce lists of Levy parameters of compound categories: Year-FirmSize-Industry and scatter plots

# loading of libraries
if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(dplyr,StableEstim,devtools,ggplot2,gridExtra)

# loading fitting functions from fitting.levy package
devtools::load_all("fittinglevy")

# function definitions
fun_fit_levy <- function(dat, bin_num, cond_ind, var_ind, c_names, cut_num, neg_cut, pov_cut) { 
    # Function to handle the Levy fits by all compound categories: Year-FirmSize-Industry
    # Arguments: 
    #   1) dat: dataframe, the data, 
    #   2) bin_num, IGNORED 
    #   3) cond_ind, IGNORED 
    #   4) var_ind: int, the index number of the column in the data frame that will be fitted
    #   5) c_names, IGNORED
    #   6) cut_num: int, the minimum size of each compound category to be fitted; smaller categories are ignored
    #   7) neg_cut: float, the cutting point on the left tail
    #   8) pov_cut: float, the cutting point on the right tail
    # Return: 
    #   results df: data frame
  
  # prepare results df
  results_df = data.frame(Country=character(), Industry=character(), FS=character(), Year=character(), alpha=numeric(), beta=numeric(), gamma=numeric(), delta=numeric(), stringsAsFactors = TRUE)
  for (k in 1:length(dat)) {    # for each country
    print(k)

    # prepare data set
    zz <- dat[[k]] %>%
      select(IDNR, Year, COMPCAT_one, NACE_CAT, LP, LP_g, Zeta, EMPL) %>% # Firm ID, Year, Firm Size, Industry ID, Labor Produtivity, Labor Productivity Growth, TFP Growth, Employment
      filter(EMPL > 1) %>% # remove self-employed persons
      mutate(LP = LP / 1000) %>% # change the unit scale of the labor productivity by dividing it by 1000
      mutate(LP_g = LP_g * 100) # percentage unit for the growth variables

    zz <- as.data.frame(zz)

    zz$Var <- zz[, var_ind] # create a new column of the value variable

    zz <- zz %>%
      select(IDNR, Year, Var, COMPCAT_one, NACE_CAT) %>%
      na.omit() %>%
      filter(Var > quantile(Var, neg_cut) & Var < quantile(Var, pov_cut)) %>% # cut the tail
      group_by(.dots=c("Year", "COMPCAT_one", "NACE_CAT")) %>%
      filter(length(IDNR) > cut_num) # set the minimum number of obs for each class

    # test if remaining data set is big enough (larger than zero); then proceed with the computation
    if (nrow(zz) != 0) {
      
      # obtain lists of unique category classes along all three dimensions
      c_uni_Year <- unique(zz$Year) # unique classes
      #c_uni_FS <- unique(zz$COMPCAT_one) # unique classes
      c_uni_Industry <- unique(zz$NACE_CAT) # unique classes

      # loop through each of those
      for (VIndustry in c_uni_Industry) {
        #for (VFS in c_uni_FS) {
          for (VYear in c_uni_Year) {
            
            # select relevant part of the data
            #c_lp <- zz$Var[zz$Year == VYear & zz$COMPCAT_one == VFS & zz$NACE_CAT == VIndustry] # for each class
            c_lp <- zz$Var[zz$Year == VYear & zz$NACE_CAT == VIndustry] # for each class
            
            # check that data in this compound category ois big enough; then proceed with the Levy fit
            if (length(c_lp) > cut_num) { 
                # Levy estimation
                levy_result <- levy_fitting(dat_t = c_lp, bin_num = bin_num, include_bootstrap=FALSE) 
                # Append result to data frame
                ##results_df <- rbind(results_df, data.frame(Country=country_names_five[k], Industry=VIndustry, FS=VFS, Year=VYear, alpha=levy_result$levy_para[1], beta=levy_result$levy_para[2], gamma=levy_result$levy_para[3], delta=levy_result$levy_para[4]))
                #results_df <- rbind(results_df, data.frame(Country=country_names[k], Industry=VIndustry, FS=VFS, Year=VYear, alpha=levy_result$levy_para[1], beta=levy_result$levy_para[2], gamma=levy_result$levy_para[3], delta=levy_result$levy_para[4]))
                results_df <- rbind(results_df, data.frame(Country=country_names[k], Industry=VIndustry, Year=VYear, alpha=levy_result$levy_para[1], beta=levy_result$levy_para[2], gamma=levy_result$levy_para[3], delta=levy_result$levy_para[4]))
                #print(results_df)
                #print(country_names[k])
            }
          }
        #}
      }
    }
  }
  return(results_df)
}

# plotting function
plot_scatter <- function(df, axis1, axis2, pdf_file_name) {
    # function to produce scatter plot
    # Arguments:
    #   df: data frame
    #   axis1: string, Variable name to be plotted on axis 1
    #   axis2: string, Variable name to be plotted on axis 2
    #   pdf_file_name: string, output file name
    # Returns: None.
    df$IndCode <- substr(df$Industry,1,1)
    aspect_ratio_this_plot = (max(df[axis1], na.rm=T) - min(df[axis1], na.rm=T)) / (max(df[axis2], na.rm=T) - min(df[axis2], na.rm=T))
    ggplot(df, aes(get(axis1), get(axis2), colour=IndCode)) + geom_point(size=2,shape=16) + geom_rug(size=0.5) + theme(legend.title = element_blank(), legend.position="bottom") + coord_fixed(ratio=aspect_ratio_this_plot) + labs(x=axis1, y=axis2)
    ggsave(file = pdf_file_name)
}


# main entry point

## 1 loading of required data and cleaning
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

## 1.2. Setting the class names: Year, Size, Industry,
# industry index
#
#year_names <- c(2006:2015) 
#
#ind_name_table <- data.frame(ind_names = unique(All_list_Cleaned_cut[[15]]$NACE_CAT), 
#                             ind_names_alphabet = c("J", "L", "G", "C", "I", "H", "S", "P", "M", "K", "B", "N", "A", "Q", "R", "F", "E", "O", "D", "T"))
#
#ind_name_table <- ind_name_table[order(ind_name_table$ind_names_alphabet),]
#
#size_names <- c("S", "M", "L", "V") # size index
#names(size_names) <- 1:4 # create the numerical name for the size index


############ 2. Fit the Levy distribution  ############
## note the following index
# con_ind: 2: year, 3: size, 4: industry
# var_ind: 5: LP, 6: LP_change, 7: TFP growth

## set up the cut-off point
neg_cut <- 0.0025 # negative cut-off point
pov_cut <- 0.9975 # positive cut-off point


# LP conditional on year
LP_Levy_list <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = 2, var_ind = 5, c_names = year_names, cut_num = 1000, neg_cut = neg_cut, pov_cut = pov_cut)

# LP_change conditional on year
LP_g_Levy_list <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = 2, var_ind = 6, c_names = year_names, cut_num = 1000, neg_cut = neg_cut, pov_cut = pov_cut)

# Zeta  conditional on year
Zeta_g_Levy_list <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = 2, var_ind = 7, c_names = year_names, cut_num = 1000, neg_cut = neg_cut, pov_cut = pov_cut)

save(LP_Levy_list, LP_g_Levy_list, Zeta_g_Levy_list, file="Levy_Fit_list_3d_samples.Rda")

plot_scatter(LP_Levy_list, "alpha", "beta", "scatterplot_Levy_Fit_LP.pdf")
plot_scatter(LP_g_Levy_list, "alpha", "beta", "scatterplot_Levy_Fit_LP_g.pdf")
plot_scatter(Zeta_g_Levy_list, "alpha", "beta", "scatterplot_Levy_Fit_Zeta_g.pdf")
