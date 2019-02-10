# Script to produce tables and plots for the paper.
# The script proceeds as follows:
#    0. Basic setup
#    1. Create LaTeX table of numbers of observations
#    2. Create plots of 1. Firm Size and 2. Industry proportions
#    3. Create semi-log plots for LP, LP growth, TFP growth by Year, Size, and Industry for sample of five countries
#    4. Create log-log plots for tails for LP, LP growth, TFP growth by Year, Size, and Industry for sample of five countries

############ 0. Basic Set up ############ 
## 0.1 loading of required libraries
if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(RColorBrewer,dplyr,xtable,tidyr)

##  0.2 loading of required data and cleaning
load("All_list_Cleaned.Rda") ## load the data file created from "Productivity_Analysis_Data.Rmd"

country_names <- c("Albania", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina", 
                   "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", 
                   "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland",
                   "Italy", "Kosovo", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", 
                   "Macedonia, FYR", "Malta", "Monaco", "Montenegro", "Netherlands", "Norway",
                   "Poland", "Portugal", "Moldova", "Romania", "Russian Federation", "Serbia",
                   "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "Ukraine", 
                   "United Kingdom")

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


## 0.3 Setting the class names: Year, Size, Industry, 

# yaer index
year_names <- c(2006:2015) 

# industry index
ind_name_table <- data.frame(ind_names = unique(All_list_Cleaned_cut[[15]]$NACE_CAT), 
                             ind_names_alphabet = c("J", "L", "G", "C", "I", "H", "S", "P", "M", "K", "B", "N", "A", "Q", "R", "F", "E", "O", "D", "T"))

ind_name_table <- ind_name_table[order(ind_name_table$ind_names_alphabet),]

size_names <- c("S", "M", "L", "V") # size index
names(size_names) <- 1:4 # create the numerical name for the size index


############ 1. a brief summary in a latex form: the number of obs for VA and EMPL variable ############ 
## 1.1. summary_1: record the number of obs for VA and EMPL variable 
summary_1 <- list()
for (k in 1:length(All_list_Cleaned_cut)) {
  zz <- All_list_Cleaned_cut[[k]] %>%
    select(Year, IDNR, EMPL, VA) %>%
    na.omit() %>%
    group_by(Year) %>%
    summarise(n = n())

  summary_1[[k]] <- zz
}


## 1.2. arrange summary_1 to make it suitable for latex form
summary <- list()
for (k in 1:length(summary_1)) {
  print(k)
  if (nrow(summary_1[[k]]) < 10) {
    create_this <- year_names[which(!year_names %in% summary_1[[k]]$Year)] # to fill the value of the missing years with 0

    summary[[k]] <- data.frame(Year = c(summary_1[[k]]$Year, create_this), n = c(summary_1[[k]]$n, rep(0, length(create_this)))) # fill the missing year with 0

    summary[[k]] <- summary[[k]] %>%
      arrange(Year)

    summary[[k]] <- summary[[k]][[2]] # get the value only
  } else {
    summary_1[[k]] <- summary_1[[k]] %>%
      arrange(Year)

    summary[[k]] <- summary_1[[k]][[2]] # get the value only
  }
}

summary_table <- do.call("cbind", summary) # from list to data frame
colnames(summary_table) <- country_names # names of the column: country
rownames(summary_table) <- year_names # names of the row: year
summary_table <- format(t(summary_table), big.mark = ",") # make the numeric values separated by a comma at every 1000

summary_table <- xtable(summary_table, digits = rep(0, 11)) # latex form


############ 2. Diagnostics for the size and industry proportion ############
## 2.1. Function set up
# 2.1.1 "fun_diagnostic_1" function to get the proportion of each class in the sample. It returns Size and Sector proportion for each country sample in the list of the list forms.

fun_diagnostic_1 <- function(dat) {
  result_list <- list()
  for (k in 1:length(dat)) { # k is the country index 
    print(k)

    zz <- dat[[k]]


    ## SIZE

    Size_p <- zz %>%
      select(Year, COMPCAT_one, EMPL) %>%
      na.omit() %>%
      group_by(Year) %>%
      summarise( # the proportion of each size category
        S_p = length(COMPCAT_one[COMPCAT_one == "S"]) / n(), # Small
        M_p = length(COMPCAT_one[COMPCAT_one == "M"]) / n(), # Medium
        L_p = length(COMPCAT_one[COMPCAT_one == "L"]) / n(), # Large
        VL_p = length(COMPCAT_one[COMPCAT_one == "V"]) / n(), # Very Large
        Self_emp = length(EMPL[EMPL == 1]) / n(),
        n = n()
      )

    
    ### Industry

    Ind_p <- zz %>%
      select(Year, NACE_CAT, LP_g) %>%
      na.omit() %>%
      group_by(Year, NACE_CAT) %>%
      summarise(n = n()) %>%
      mutate(freq = n / sum(n)) %>% # I take a different approach here to caculate the proportion since industry has many categories. I use this to avoid using the table function in R, which I do not like too much. I first take the relative frequency for each year and industry.
      group_by(Year)

    n <- Ind_p %>%
      group_by(Year) %>%
      summarise(n = sum(n)) # the total number for each year

    Ind_p <- spread(Ind_p[-c(3)], NACE_CAT, freq) # a long format to a wide format
    Ind_p$n <- n[[2]]


    result_list[[k]] <- list(Size_p = Size_p, Ind_p = Ind_p) # each list has the information on Size and Industry of each country
  }
  return(result_list)
}


# 2.1.1 "fun_plot_diag" function: plot function for proportion diagnostics: data type should be the one generated from fun_diagnostic_1 function. 
fun_plot_diag <- function(pdf_name, title, dat, var_num, var_ind) { # this function has 5 arguments. 1) the name of the pdf file, 2) the title of the figure, 3) the data file that is generated from the fun_diagnostic_1 function, 4) variable number: 1:Size, 2: Industry, 5) variable index: either "size" or "ind"
  pdf(paste(pdf_name, ".pdf", sep = ""), height = 6, width = 8)
  par(mfrow = c(4, 4), mar = c(3, 2.5, 1, 1), mgp = c(1.5, .3, 0), tck = -.01, oma = c(0, 0, 4, 0))

  if (var_ind == "size") { # for the size proportion
    color_this <- brewer.pal(5, "Dark2") # five colors for each size type including the self-employed
    leg <- c("S", "M", "L", "VL", "Self") # for the labels in the legend

    plot(c(1:1), c(1:1), yaxt = "n", xaxt = "n", main = "", cex.main = 1.2, xlab = "", ylab = "", lty = "blank", pch = 20, cex = .0, bty = "n") # Empty plot

    legend("center", legend = leg, lty = rep(1, 5), lwd = 2, col = color_this, bty = "n", cex = 1)
  } else { # for industry proportion
    color_this <- c("grey", "blue", "green", "red", brewer.pal(n = 8, name = "Dark2"), brewer.pal(n = 8, name = "Set3")) # 20 colors for each industry type
    leg <- ind_name_table$ind_names_alphabet # the alphabetical index for industry type

    plot(c(1:1), c(1:1), yaxt = "n", xaxt = "n", main = "", cex.main = 1.2, xlab = "", ylab = "", lty = "blank", pch = 20, cex = .0, bty = "n") # empty plot

    legend("center", legend = leg, lty = rep(1, 20), lwd = 2, col = color_this, bty = "n", cex = .8, ncol = 3)
  }


  for (k in 1:length(dat)) {
    print(k)

    ok <- dat[[k]][[var_num]]

    if (var_ind == "size") { # for the size porportion
      plot(ok$Year, ok[[2]], col = color_this[1], yaxt = "n", xaxt = "n", main = paste(country_names[k]), cex.main = 1., xlab = "Year", ylab = "Proportion", lty = "blank", pch = 20, cex = .0, ylim = c(0, 1))

      axis(side = 1, lwd = 0.3, cex.axis = 0.6)
      axis(side = 2, lwd = 0.3, cex.axis = 0.6)

      for (y in 1:5) { # five types
        lines(ok$Year, ok[[c(y + 1)]], cex = 0.5, col = color_this[y], lwd = 1.5)
      }
    } else { # for the industry proportion

      plot(ok$Year, ok[[2]], col = color_this[1], yaxt = "n", xaxt = "n", main = paste(country_names[k]), cex.main = 1., xlab = "Year", ylab = "Proportion", lty = "blank", pch = 20, cex = .0, ylim = c(0, .4))

      axis(side = 1, lwd = 0.3, cex.axis = 0.9)
      axis(side = 2, lwd = 0.3, cex.axis = 0.9)

      for (y in 1:c(ncol(ok) - 2)) { # the number of types is the number of the data frame - 2 (the year column and the total obs column)

        uni_ind <- which(ind_name_table$ind_names %in% names(ok)[c(y + 1)]) # for the consistency in coloring

        lines(ok$Year, ok[[c(y + 1)]], cex = 0.5, col = color_this[uni_ind], lwd = 1.5)
      }
    }
  }


  mtext(title, side = 3, line = 1, outer = TRUE, cex = 1.2)
  dev.off()
}


## 2.2 generate a diagnostic data on size and industry 
diag_size_ind <- fun_diagnostic_1(All_list_Cleaned_cut)


## 2.3 generate plots using the data "diag_size_ind"

# plot for size proportion
fun_plot_diag(pdf_name = "Figure_Size_Proportion", title = "Size Proportion by Country", dat = diag_size_ind, var_num = 1, var_ind = "size") # var_num = 1 means size variable

# plot for industry proportion
fun_plot_diag(pdf_name = "Figure_Ind_Proportion", title = "Size Proportion by Industry", dat = diag_size_ind, var_num = 2, var_ind = "ind") # var_num = 1s means industry variable

# detach("package:tidyr", unload=TRUE)


############  3. Plots for the distributions for Top 5 countries ############
## 3.1. Data set up for top 5 countries

country_names_five <- c("France", "Germany", "Italy", "Spain", "United Kingdom") # name of the five largest EU countries 

Five_list_Cleaned <-
  All_list_Cleaned_cut[which(country_names %in% country_names_five)] # take the subsample of the the five largest EU countries 


## 3.2. Set up a plot function "fun_plot_marginal" for the distribution of the target variable conditional on each class
fun_plot_marginal <- function(pdf_name, title, cond_ind, var_ind, x_lab, c_names, neg_cut, pov_cut, cut_num, n_col) { # this plotting function is applied to the data object "Five_list_Cleaned". It has 10 arguments: 1) the name of the pdf file, 2) the title of the figure, 3) the index of the variable that is used as the conditional class, 4) the target variable, 5) the name of the x label 6) the name of 3) variable components, 7) the cutting point on the left tail, 8) the cutting point on the right tail, 9) the minimum number of observations for each class, 10) the number of columns in the legend

  pdf(paste(pdf_name, ".pdf", sep = ""), height = 2.7, width = 10)
  par(mfrow = c(1, 5), mar = c(3, 2.5, 1, 1), mgp = c(1.5, .3, 0), tck = -.01, oma = c(0, 0, 4, 0))

  for (k in 1:5) {
    print(k) # k is the country index


    dd <- Five_list_Cleaned[[k]] %>%
      select(IDNR, Year, COMPCAT_one, NACE_CAT, LP, LP_g, Zeta, EMPL) %>%
      filter(EMPL > 1) %>% # remove self-employed persons
      mutate(LP = LP / 1000) %>% # change the unit scale of the labor productivity by dividing it by 1000
      mutate(
        LP_g = LP_g * 100, # percentage unit for the growth variables 
        Zeta = Zeta * 100
      )

    dd <- as.data.frame(dd) ## change the format of the data to data.frame for convenience

    dd$Cond <- dd[, cond_ind] # create a new column of the class variable for convenience
    dd$Var <- dd[, var_ind] # create a new column of the value variable for convenience

    dd <- dd %>% # get the firm index, conditional class, and the target variable only
      select(IDNR, Cond, Var) %>%
      na.omit() %>%
      filter(Var > quantile(Var, neg_cut) & Var < quantile(Var, pov_cut)) %>% # cut extreme parts of tails
      group_by(Cond) %>% # group by class
      filter(length(IDNR) > cut_num) # the minimum number of obs is cut_num

    dd_info <- dd %>% ## to get the min and max of the axis for the plot: x-axis from the mids object from the hist function and y-axis from the density object frim the hist function
      group_by(Cond) %>%
      summarise(
        x_min = min(hist(Var, breaks = seq(min(Var), max(Var), l = 100 + 1), plot = F)$mids),
        x_max = max(hist(Var, breaks = seq(min(Var), max(Var), l = 100 + 1), plot = F)$mids),
        y_min = min(hist(Var, breaks = seq(min(Var), max(Var), l = 100 + 1), plot = F)$density[hist(Var, breaks = seq(min(Var), max(Var), l = 100 + 1), plot = F)$density > 0]), # the minimun density needs to be greater than 0 to make it sure that the axis is log convertiblke
        y_max = max(hist(Var, breaks = seq(min(Var), max(Var), l = 100 + 1), plot = F)$density)
      )

    x_min <- min(dd_info$x_min) 
    y_min <- min(dd_info$y_min)
    x_max <- max(dd_info$x_max)
    y_max <- max(dd_info$y_max)

    ## the following part will be repeated throughout the script. It is designed to make the coloring of plots consistent across different subsamples. How it works it the following. I have the names of all classes by year, size, and industry. Go to the section 0.3 to see the unique factor by three different categorization of firms. It is important to note that some samples do not have all the classes, e.g. Country A missing year 2007, size Small, and Industry 1,2,10. Therefore, I first assign unique pch and color to each member of all classes, e.g. pch = 1 and color "red" to year 2007 and use this coloring and shape of the plot for all classes in the subsample.
    c_uni <- unique(dd$Cond) # unique class 

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

     color_ind <- c(brewer.pal(n = 8, name = "Dark2"), brewer.pal(n = 8, name = "Set3"), "grey", "blue", "green", "red")[1:length(c_names)] # color index 

    plot(c(x_min, x_max), c(y_min, y_max), cex = 0, log = "y", yaxt = "n", xaxt = "n", cex.main = 1.2, xlab = x_lab, ylab = "Log-Density", main = country_names_five[k]) # empty plot 
    axis(side = 1, lwd = 0.3, cex.axis = 0.9)
    axis(side = 2, lwd = 0.3, cex.axis = .9)

    c_ind_all <- c()
    for (c in 1:length(c_uni_name)) {
      c_lp <- dd$Var[dd$Cond == c_uni_name[c]] # get the variable conditional on each class
      c_hist <- hist(c_lp, breaks = seq(min(c_lp), max(c_lp), l = 100 + 1), plot = F) # hist info for the target variable

      c_ind <- which(c_names %in% c_uni_name[c]) 
      points(c_hist$mids, c_hist$density, pch = c_ind, cex = 0.35, col = color_ind[c_ind])

      c_ind_all[c] <- c_ind
    }

    legend("topright", legend = c_uni_name_2, pch = c_ind_all, col = color_ind[c_ind_all], bty = "n", xpd = NA, cex = .8, ncol = n_col)
  }
  mtext(paste(title), side = 3, line = 1, outer = TRUE, cex = 1.1)
  dev.off()
}


## 3.3. plot the distribution of the target variable
## note the following index
# con_ind: 2: year, 3: size, 4: industry
# var_ind: 5: LP, 6: LP_change, 7: TFP growth

## set up the cut-off point
neg_cut <- 0.0025 # negative cut-off point
pov_cut <- 0.9975 # positive cut-off point


# cross-sectional plots of LP and LP\_change (country-year)

fun_plot_marginal(pdf_name = "Figure_Country_Year_LP", title = "Log Density of Labor Productivity", cond_ind = 2, var_ind = 5, x_lab = "LP", c_names = year_names, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 0, n_col = 2)

fun_plot_marginal(pdf_name = "Figure_Country_Year_LP_Growth", title = "Log Density of Labor Productivity Growth", cond_ind = 2, var_ind = 6, x_lab = "LP Growth(%)", c_names = year_names, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 0, n_col = 2)

fun_plot_marginal(pdf_name = "Figure_Country_Year_TFP_Growth", title = "Log Density of TFP Growth", cond_ind = 2, var_ind = 7, x_lab = "TFP (%)", c_names = year_names, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 0, n_col = 2)

# cross-sectional plots of LP and LP\_change (country-size)

fun_plot_marginal(pdf_name = "Figure_Country_Size_LP", title = "Log Density of Labor Productivity", cond_ind = 3, var_ind = 5, x_lab = "LP", c_names = size_names, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000, n_col = 1)

fun_plot_marginal(pdf_name = "Figure_Country_Size_LP_Growth", title = "Log Density of Labor Productivity Growth", cond_ind = 3, var_ind = 6, x_lab = "LP Growth(%)", c_names = size_names, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000, n_col = 1)

fun_plot_marginal(pdf_name = "Figure_Country_Size_TFP_Growth", title = "Log Density of TFP Growth", cond_ind = 3, var_ind = 7, x_lab = "TFP Growth(%)", c_names = size_names, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000, n_col = 1)

# cross-sectional plots of LP and LP\_change (country-industry)

fun_plot_marginal(pdf_name = "Figure_Country_Industry_LP", title = "Log Density of Labor Productivity", cond_ind = 4, var_ind = 5, x_lab = "LP", c_names = ind_name_table$ind_names, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000, n_col = 3)

fun_plot_marginal(pdf_name = "Figure_Country_Industry_LP_Growth", title = "Log Density of Labor Productivity Growth", cond_ind = 4, var_ind = 6, x_lab = "LP Growth(%)", c_names = ind_name_table$ind_names, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000, n_col = 3)

fun_plot_marginal(pdf_name = "Figure_Country_Industry_TFP_Growth", title = "Log Density of TFP Growth", cond_ind = 4, var_ind = 7, x_lab = "TFP Growth(%)", c_names =  ind_name_table$ind_names, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000, n_col = 3)


############ 4. the distribution of the right tail parts ############
## 4.1. function
fun_plot_marginal_tail <- function(pdf_name, title, cond_ind, var_ind, x_lab, c_names, leg_pos, tail_size, neg_cut, pov_cut, cut_num, n_col, left_tail=FALSE) {
  # Function to plot the negative tail in log-log scale
  # Arguments:
  #   pdf_name: string, name of output pdf
  #   title: string, name of plot title
  #   cond_ind: int, Condition index (property that plots are grouped by) 
  #   var_ind: int, variable index (property the histogram of which is plotted)
  #   x_lab: string, x axis label
  #   c_names: list of strings, legend labels
  #   leg_pos:    IGNORED
  #   tail_size: float, tail size to be included (before removal of outliers)
  #   neg_cut: float, negative outlier cutoff point
  #   pov_cut: float, positive outlier cutoff point
  #   cut_num: int, minimum number of observations per category to be included
  #   n_col:     IGNORED
  #   left_tail: boolean, Indicator whether we want the left tail (or, default: the right tail)
  # Returns: n/a

  # Prepare pdf parameters
  pdf(paste(pdf_name, ".pdf", sep = ""), height = 2.7, width = 10)
  if(cond_ind == 4){    # Industry plot (cond_ind==4) requires 2 column index, therefore wider margin
    par(mfrow = c(1, 5), mar = c(3, 2.5, 1, 1), mgp = c(1.5, .3, 0), tck = -.01, oma = c(0, 0, 4, 4.7))  
  } else {
    par(mfrow = c(1, 5), mar = c(3, 2.5, 1, 1), mgp = c(1.5, .3, 0), tck = -.01, oma = c(0, 0, 4, 4))
  }
  
  # set respective tail cutoff point for left and right tails
  if (left_tail) {
    cut_tail = tail_size
  } else {
    cut_tail = 1. - tail_size
  }

  for (k in 1:5) {      # for each country
    #print(k)
    
    # prepare data
    dd <- Five_list_Cleaned[[k]] %>%
      select(IDNR, Year, COMPCAT_one, NACE_CAT, LP, LP_g, Zeta, EMPL) %>%
      filter(EMPL > 1) %>%
      mutate(LP = LP / 1000) %>%
      mutate(LP_g = LP_g * 100)

    dd <- as.data.frame(dd)

    dd$Cond <- dd[, cond_ind]
    dd$Var <- dd[, var_ind]

    dd <- dd %>%
      select(IDNR, Cond, Var) %>%
      na.omit() %>%
      filter(Var > quantile(Var, neg_cut) & Var < quantile(Var, pov_cut)) %>%
      group_by(Cond) %>%
      filter(length(IDNR) > cut_num)
    
    # get mode from histogram over all years
    dd_hist <- hist(dd$Var, breaks = seq(min(dd$Var), max(dd$Var), l = 100 + 1), plot = F) # hist info for the target variable
    Var_mode <- dd_hist$mids[which.max(dd_hist$density)]
    
    # obtain plot margins 
    if (left_tail) {        # left tail
      dd_info <- dd %>%
        group_by(Cond) %>%
        filter(Var < quantile(Var, cut_tail)) %>%
        mutate(Var = Var_mode - Var) %>%
        filter(Var > 0) %>%
        summarise(
          x_min = min(hist(log(Var), breaks = seq(min(log(Var)), max(log(Var)), l = 25 + 1), plot = F)$mids),
          x_max = max(hist(log(Var), breaks = seq(min(log(Var)), max(log(Var)), l = 25 + 1), plot = F)$mids),
          y_min = min(hist(log(Var), breaks = seq(min(log(Var)), max(log(Var)), l = 25 + 1), plot = F)$density[hist(log(Var), breaks = seq(min(log(Var)), max(log(Var)), l = 25 + 1), plot = F)$density > 0]),
          y_max = max(hist(log(Var), breaks = seq(min(log(Var)), max(log(Var)), l = 25 + 1), plot = F)$density)
        )
    } else {                # right tail
      dd_info <- dd %>%
        group_by(Cond) %>%
        filter(Var > quantile(Var, cut_tail)) %>%
        summarise(
          x_min = min(hist(log(Var), breaks = seq(min(log(Var)), max(log(Var)), l = 25 + 1), plot = F)$mids),
          x_max = max(hist(log(Var), breaks = seq(min(log(Var)), max(log(Var)), l = 25 + 1), plot = F)$mids),
          y_min = min(hist(log(Var), breaks = seq(min(log(Var)), max(log(Var)), l = 25 + 1), plot = F)$density),
          y_max = max(hist(log(Var), breaks = seq(min(log(Var)), max(log(Var)), l = 25 + 1), plot = F)$density)
        )
    }        

    x_min <- min(dd_info$x_min)
    y_min <- min(dd_info$y_min[dd_info$y_min > 0])
    x_max <- max(dd_info$x_max)
    y_max <- max(dd_info$y_max)

    # prepare labels
    c_uni <- unique(dd$Cond)
    
    c_uni_name <- c()
    c_uni_num <- c() 
    c_uni_name_2 <- c()
    
    for (i in 1:length(c_uni)) {
      c_uni_num[i] <- which(c_names %in% c_uni[i])
    }
    
    c_uni_num <- sort(c_uni_num)
    c_uni_name <- c_names[c_uni_num]
    
    color_ind <- c(brewer.pal(n = 8, name = "Dark2"), brewer.pal(n = 8, name = "Set3"), "grey", "blue", "green", "red")[1:length(c_names)]

    # create plot canvas
    plot(c(x_min, x_max), c(y_min, y_max), cex = 0, log = "y", yaxt = "n", xaxt = "n", cex.main = 1.2, xlab = x_lab, ylab = "Log-Density", main = country_names_five[k])

    # prepare axis ticks labels
    if (left_tail) {
      # for left tails use log of mode for pivot of the axis shift, unless that would be too large a shift (abs(log(x)) for x<1 becomes huge). In that case replace by zero.
      if ( Var_mode <=1 ) {
        log_Var_mode_position_shift = 0
      } else {
        log_Var_mode_position_shift = log(Var_mode)
      }
      
      # maniputalte ticks positions and labels
      xaxt_positions = log_Var_mode_position_shift - round(log_Var_mode_position_shift - axTicks(1))
      xaxt_labels = round(log_Var_mode_position_shift - axTicks(1))
    } else {
      # for right tails use standard values
      xaxt_positions = axTicks(1)
      xaxt_labels = axTicks(1)
    }
    
    # apply axis labels
    axis(side = 1, at=xaxt_positions, labels=xaxt_labels, lwd = 0.3, cex.axis = 0.9)
    axis(side = 2, lwd = 0.3, cex.axis = .9)

    c_ind_all <- c()
    
    # include scatter plot
    for (c in 1:length(c_uni_name)) {
      c_lp <- dd$Var[dd$Cond == c_uni_name[c]]

      if (left_tail) {
        c_lp <- c_lp[c_lp < quantile(c_lp, cut_tail)]   # cut the tail
        c_lp <- Var_mode - c_lp                         # apply transformation using mode for scatter plot
        c_lp <- c_lp[c_lp > 0]                          # remove possibly present negative and zero values
      } else {
        c_lp <- c_lp[c_lp > quantile(c_lp, cut_tail)]   # cut the tail 
      }

      c_hist <- hist(log(c_lp), breaks = seq(min(log(c_lp)), max(log(c_lp)), l = 50 + 1), plot = F)
      c_ind <- which(c_names %in% c_uni_name[c])
      points(c_hist$mids, c_hist$density, pch = c_ind, cex = 0.35, col = color_ind[c_ind])

      c_ind_all[c] <- c_ind
    }

  }

  # legend for last plot and to the right outside the plotting area. 
  if(cond_ind == 4){    # legend must be two column for industry plots (cond_ind==4)
    c_uni_name_2 <- ind_name_table$ind_names_alphabet[c_uni_num] # only industry letter code for industry plots
    n_col <- 2
    legend("topright", inset=c(-0.525,0), legend = c_uni_name_2, pch = c_ind_all, col = color_ind[c_ind_all], bty = "n", xpd = NA, cex = 1.2, ncol = n_col)
  } else{
    c_uni_name_2 <- c_uni_name
    n_col <- 1
    legend("topright", inset=c(-0.4,0), legend = c_uni_name_2, pch = c_ind_all, col = color_ind[c_ind_all], bty = "n", xpd = NA, cex = 1.2, ncol = n_col)
  }

  # title text
  mtext(paste(title), side = 3, line = 1, outer = TRUE, cex = 1.)
  dev.off()
}

## 4.2. Positive tail plots
# cut_tail is set to be 0.9

##cross-sectional plots of LP and LP\_change (country-year)
fun_plot_marginal_tail(pdf_name = "Figure_Country_Year_LP_pov_tail", title = "Right Tail of Log Density of Labor Productivity", cond_ind = 2, var_ind = 5, x_lab = "log(LP)", c_names = year_names, leg_pos = "topright", tail_size = 0.05, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 0, n_col = 2)

fun_plot_marginal_tail(pdf_name = "Figure_Country_Year_LP_Growth_pov_tail", title = "Right Tail of Log Density of Labor Productivity Growth", cond_ind = 2, var_ind = 6, x_lab = "log(LP Growth)", c_names = year_names, leg_pos = "topright", tail_size = 0.05, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 0, n_col = 2)

fun_plot_marginal_tail(pdf_name = "Figure_Country_Year_TFP_Growth_pov_tail", title = "Right Tail of Log Density of TFP Growth", cond_ind = 2, var_ind = 7, x_lab = "log(TFP Growth)", c_names = year_names, leg_pos = "topright", tail_size = 0.05, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 0, n_col = 2)

#cross-sectional plots of LP and LP\_change (country-size)
fun_plot_marginal_tail(pdf_name = "Figure_Country_Size_LP_pov_tail", title = "Right Tail of Log Density of Labor Productivity", cond_ind = 3, var_ind = 5, x_lab = "log(LP)", c_names = size_names, leg_pos = "bottomleft", tail_size = 0.05, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000, n_col = 1)

fun_plot_marginal_tail(pdf_name = "Figure_Country_Size_LP_Growth_pov_tail", title = "Right Tail of Log Density of Labor Productivity Growth", cond_ind = 3, var_ind = 6, x_lab = "log(LP Growth)", c_names = size_names, leg_pos = "bottomleft", tail_size = 0.05, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000, n_col = 1)

fun_plot_marginal_tail(pdf_name = "Figure_Country_Size_TFP_Growth_pov_tail", title = "Right Tail of Log Density of TFP Growth", cond_ind = 3, var_ind = 7, x_lab = "log(TFP Growth)", c_names = size_names, leg_pos = "bottomleft", tail_size = 0.05, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000, n_col = 1)

# cross-sectional plots of LP and LP\_change (country-industry)
fun_plot_marginal_tail(pdf_name = "Figure_Country_Industry_LP_pov_tail", title = "Right Tail of Log Density of Labor Productivity", cond_ind = 4, var_ind = 5, x_lab = "log(LP)", c_names = ind_name_table$ind_names, leg_pos = "bottomleft", tail_size = 0.05, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000, n_col = 3)

fun_plot_marginal_tail(pdf_name = "Figure_Country_Industry_LP_Growth_pov_tail", title = "Right Tail of Log Density of Labor Productivity Growth", cond_ind = 4, var_ind = 6, x_lab = "log(LP Growth)", c_names = ind_name_table$ind_names, leg_pos = "bottomleft", tail_size = 0.05, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000, n_col = 3)

fun_plot_marginal_tail(pdf_name = "Figure_Country_Industry_TFP_Growth_pov_tail", title = "Right Tail of Log Density of TFP Growth", cond_ind = 4, var_ind = 7, x_lab = "log(TFP Growth)", c_names = ind_name_table$ind_names, leg_pos = "bottomleft", tail_size = 0.05, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000, n_col = 3)


## 4.3. Negative tail plots
# cross-sectional plots of LP and LP\_change (country-year)
fun_plot_marginal_tail(pdf_name = "Figure_Country_Year_LP_neg_tail", title = "Left Tail of Log Density of Labor Productivity", cond_ind = 2, var_ind = 5, x_lab = "log(LP)", c_names = year_names, leg_pos = "topleft", tail_size = 0.05, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 10000, n_col = 2, left_tail=TRUE)

fun_plot_marginal_tail(pdf_name = "Figure_Country_Year_LP_Growth_neg_tail", title = "Left Tail of Log Density of Labor Productivity Growth", cond_ind = 2, var_ind = 6, x_lab = "log(LP Growth)", c_names = year_names, leg_pos = "topleft", tail_size = 0.05, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 10000, n_col = 2, left_tail=TRUE)

fun_plot_marginal_tail(pdf_name = "Figure_Country_Year_TFP_Growth_neg_tail", title = "Left Tail of Log Density of TFP Growth", cond_ind = 2, var_ind = 7, x_lab = "log(TFP Growth)", c_names = year_names, leg_pos = "topleft", tail_size = 0.05, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 10000, n_col = 2, left_tail=TRUE)

#cross-sectional plots of LP and LP\_change (country-size)
fun_plot_marginal_tail(pdf_name = "Figure_Country_Size_LP_neg_tail", title = "Left Tail of Log Density of Labor Productivity", cond_ind = 3, var_ind = 5, x_lab = "log(LP)", c_names = size_names, leg_pos = "topleft", tail_size = 0.05, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000, n_col = 1, left_tail=TRUE)

fun_plot_marginal_tail(pdf_name = "Figure_Country_Size_LP_Growth_neg_tail", title = "Left Tail of Log Density of Labor Productivity Growth", cond_ind = 3, var_ind = 6, x_lab = "log(LP Growth)", c_names = size_names, leg_pos = "topleft", tail_size = 0.05, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000, n_col = 1, left_tail=TRUE)

fun_plot_marginal_tail(pdf_name = "Figure_Country_Size_TFP_Growth_neg_tail", title = "Left Tail of Log Density of TFP Growth", cond_ind = 3, var_ind = 7, x_lab = "log(TFP Growth)", c_names = size_names, leg_pos = "topleft", tail_size = 0.05, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000, n_col = 1, left_tail=TRUE)

# cross-sectional plots of LP and LP\_change (country-industry)
fun_plot_marginal_tail(pdf_name = "Figure_Country_Industry_LP_neg_tail", title = "Left Tail of Log Density of Labor Productivity", cond_ind = 4, var_ind = 5, x_lab = "log(LP)", c_names = ind_name_table$ind_names, leg_pos = "topleft", tail_size = 0.05, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000, n_col = 3, left_tail=TRUE)

fun_plot_marginal_tail(pdf_name = "Figure_Country_Industry_LP_Growth_neg_tail", title = "Left Tail of Log Density of Labor Productivity Growth", cond_ind = 4, var_ind = 6, x_lab = "log(LP Growth)", c_names = ind_name_table$ind_names, leg_pos = "topleft", tail_size = 0.05, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000, n_col = 3, left_tail=TRUE)

fun_plot_marginal_tail(pdf_name = "Figure_Country_Industry_TFP_Growth_neg_tail", title = "Left Tail of Log Density of TFP Growth", cond_ind = 4, var_ind = 7, x_lab = "log(TFP Growth)", c_names = ind_name_table$ind_names, leg_pos = "topleft", tail_size = 0.05, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000, n_col = 3, left_tail=TRUE)
