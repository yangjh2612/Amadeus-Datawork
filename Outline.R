## ------------------------------------------------------------------------
library(dplyr)
library(RColorBrewer)

load("All_list_Cleaned.Rda") ## load the data file created from "Productivity_Analysis_Data.Rmd"


country_names  <- c('Albania', 'Austria', 'Belarus', 'Belgium', 'Bosnia and Herzegovina', 'Bulgaria', 'Croatia', 'Cyprus', 'Czech Republic', 'Denmark', 'Estonia', 'Finland', 'France', 'Germany', 'Greece', 'Hungary', 'Iceland', 'Ireland', 'Italy', 'Kosovo', 'Latvia', 'Liechtenstein', 'Lithuania', 'Luxembourg', 'Macedonia, FYR', 'Malta', 'Monaco', 'Montenegro', 'Netherlands', 'Norway', 'Poland', 'Portugal', 'Moldova', 'Romania', 'Russian Federation', 'Serbia', 'Slovakia', 'Slovenia', 'Spain', 'Sweden', 'Switzerland', 'Turkey', 'Ukraine', 'United Kingdom')

 All_list_Cleaned_cut <- list()
for(k in 1:length(All_list_Cleaned)){
  print(k)
  All_list_Cleaned_cut[[k]] <- All_list_Cleaned[[k]] %>%
    select(IDNR, Year, COMPCAT, NACE_CAT, LP, LP_g, EMPL, VA, Zeta) %>%
    mutate(COMPCAT_one = substr(COMPCAT, 1, 1)) %>%
    group_by(Year) %>%
    filter(length(IDNR) > 10000) %>% # Note that 23 countries out of 44 do not have VA info. Out of 21 countries, 15 countries have enough info for anlaysis (more than 5 years with 10,000 firms )
    group_by() 
}

 
take_this <- which(unlist(lapply(All_list_Cleaned_cut, function(x) length(unique(x$Year)) > 5))) # Countries with more than 5 year obs with 10,000 firms

All_list_Cleaned_cut <- All_list_Cleaned_cut[take_this]
country_names <- country_names[take_this]

rm(All_list_Cleaned)


## ------------------------------------------------------------------------
year_names <- c(2006:2015)
ind_names <- unique(All_list_Cleaned_cut[[15]]$NACE_CAT)
names(ind_names) <- 1:length(ind_names)

size_names <- c("S", "M", "L", "V")
names(size_names) <- 1:4


## ------------------------------------------------------------------------
## a brief summary part: the number of obs for VA variable

summary_1 <- list()
for(k in 1:length(All_list_Cleaned_cut)){

      zz <- All_list_Cleaned_cut[[k]]  %>%
        select(Year, IDNR, EMPL, VA) %>%
        na.omit() %>%
        group_by(Year) %>%
        summarise(n = n())
     
  summary_1[[k]] <- zz
}
 

summary <- list()
  for(k in 1:length(summary_1)){
  print(k)
  if(nrow(summary_1[[k]]) < 10){
    create_this <- year_names[which(!year_names%in%summary_1[[k]]$Year)] # to fill the missing year VA with 0 
    
    summary[[k]] <- data.frame(Year = c(summary_1[[k]]$Year, create_this), n =  c(summary_1[[k]]$n, rep(0, length(create_this))))
    
    summary[[k]] <- summary[[k]] %>%
      arrange(Year)
    
    summary[[k]] <- summary[[k]][[2]]

  } else{
    summary_1[[k]] <- summary_1[[k]] %>%
      arrange(Year)
    
      summary[[k]] <- summary_1[[k]][[2]]
  }
    
}

  summary_table <- do.call("cbind", summary)
  colnames(summary_table) <- country_names 
  rownames(summary_table) <- year_names
  summary_table <- format(t(summary_table), big.mark = ",")
  
  library(xtable)
  
  summary_table <- xtable(summary_table, digits=rep(0,11)) # latex form

## ------------------------------------------------------------------------
## Diagnostics for the size and industry proportion 
library(tidyr)
fun_diagnostic_1 <- function(dat){

result_list <- list()
for(k in 1:length(dat)){
  print(k)
  
  zz <- dat[[k]]

      
    ## SIZE 

      Size_p <- zz %>%
        select(Year, COMPCAT_one, EMPL) %>%
        na.omit() %>%
        group_by(Year) %>%
        summarise(S_p = length(COMPCAT_one[COMPCAT_one == "S"])/n(), # the proportion of each size category
                  M_p = length(COMPCAT_one[COMPCAT_one == "M"])/n(),
                  L_p = length(COMPCAT_one[COMPCAT_one == "L"])/n(),
                  VL_p = length(COMPCAT_one[COMPCAT_one == "V"])/n(),
                  Self_emp = length(EMPL[EMPL == 1])/n(),
                  n = n())
      
    ## Industry
      
       Ind_p <- zz %>%
        select(Year, NACE_CAT, LP_g) %>%
        na.omit() %>%
        group_by(Year, NACE_CAT) %>%
        summarise (n = n()) %>% 
        mutate(freq = n / sum(n)) %>% # I took a different approach since industry has many categories
        group_by(Year)

       n <- Ind_p %>%
         group_by(Year) %>%
         summarise(n = sum(n))
       Ind_p <-  spread(Ind_p[-c(3)], NACE_CAT, freq) # 
       Ind_p$n <- n[[2]]
       

   result_list[[k]] <- list(Size_p = Size_p,  Ind_p =  Ind_p)

  }
return(result_list)
}

##
diag_size_ind <- fun_diagnostic_1(All_list_Cleaned_cut)

## ------------------------------------------------------------------------
year_names <- c(2006:2015)
ind_names <- unique(All_list_Cleaned_cut[[15]]$NACE_CAT)
names(ind_names) <- 1:length(ind_names)

size_names <- c("S", "M", "L", "V")
names(size_names) <- 1:4

#### plot function for proportion diagnostics
fun_plot_diag <- function(pdf_name, title, dat, var_num, var_ind){
 
  
 pdf(paste(pdf_name, ".pdf", sep = ""), height = 6, width = 8)
  par(mfrow=c(4,4), mar=c(3, 2.5, 1, 1), mgp=c(1.5,.3,0), tck=-.01, oma=c(0,0,4,0))
  
  if(var_ind == "size"){
     color_this <- brewer.pal(5, "Dark2")
     leg = c("S", "M", "L", "VL", "Self")
     
     plot(c(1:1), c(1:1),  yaxt = "n", xaxt = "n",main = "", cex.main = 1.2, xlab = "", ylab = "", lty="blank", pch = 20, cex = .0, bty = "n")

 legend("center", legend = leg, lty = rep(1, 5), lwd = 2, col = color_this,  bty = "n", cex = 1)
 
  } else {
     color_this <- c("grey", "blue","green","red",brewer.pal(n = 8, name = 'Dark2'), brewer.pal(n = 8, name = 'Set3'))
     leg = names(ind_names)
    
      plot(c(1:1), c(1:1),  yaxt = "n", xaxt = "n",main = "", cex.main = 1.2, xlab = "", ylab = "", lty="blank", pch = 20, cex = .0, bty = "n")

      legend("center", legend = leg, lty = rep(1, 20), lwd = 2, col = color_this,  bty = "n", cex = .8,  ncol = 3)
 
  } 
  

   for(k in 1:length(dat)){
  print(k)
   
  ok <- dat[[k]][[var_num]]
  
  ok <- ok %>%
   filter(n > 10000)
 

  
  if(var_ind == "size"){
   plot(ok$Year, ok[[2]], col = color_this[1], yaxt = "n", xaxt = "n",main = paste(country_names[k]), cex.main = 1., xlab = "Year", ylab = "Proportion", lty="blank", pch = 20, cex = .0, ylim = c(0, 1))
    
  axis(side = 1, lwd = 0.3, cex.axis=0.6)
  axis(side = 2, lwd = 0.3, cex.axis=0.6)

for(y in 1:5){
  lines(ok$Year, ok[[c(y+1)]], cex = 0.5, col = color_this[y], lwd = 1.5)  
} 
  } else {
        
       
      
       plot(ok$Year, ok[[2]], col = color_this[1], yaxt = "n", xaxt = "n",main = paste(country_names[k]), cex.main = 1., xlab = "Year", ylab = "Proportion", lty="blank", pch = 20, cex = .0, ylim = c(0, .4))
    
    axis(side = 1, lwd = 0.3, cex.axis=0.9)
    axis(side = 2, lwd = 0.3, cex.axis=0.9)

for(y in 1:c(ncol(ok)-2)){
  
  uni_ind <- which(ind_names%in%names(ok)[c(y+1)]) # for the consistency in coloring
  
  lines(ok$Year, ok[[c(y+1)]], cex = 0.5, col = color_this[uni_ind], lwd = 1.5)  
}
  } 
  
  }


mtext(title, side=3, line=1, outer=TRUE,cex= 1.2)
  dev.off()

   
}




setwd("~/Desktop/Cleaned Rda/Productivity/Figures")
###
fun_plot_diag(pdf_name = "Figure_Size_Proportion", title = "Size Proportion by Country", diag_size_ind, var_num = 1, var_ind = "size")

fun_plot_diag(pdf_name = "Figure_Ind_Proportion", title = "Size Proportion by Industry", diag_size_ind, var_num = 2, var_ind = "ind")

#detach("package:tidyr", unload=TRUE)

## ------------------------------------------------------------------------
## Top 5 countries 
country_names_five  <- c('France', 'Germany', 'Italy', 'Spain', 'United Kingdom')


Five_list_Cleaned <- 
All_list_Cleaned_cut[which(country_names%in%country_names_five)]
  

## Plots for marginal distributions
fun_plot_marginal <- function(pdf_name, title, cond_ind, var_ind, x_lab, c_names, neg_cut, pov_cut, cut_num, n_col){
  ### LP_change
  
  
    pdf(paste(pdf_name, ".pdf", sep = ""), height = 2.7, width = 10)
  par(mfrow=c(1,5), mar=c(3, 2.5, 1, 1), mgp=c(1.5,.3,0), tck=-.01, oma=c(0,0,4,0))

  for(k in 1:5){
  print(k)
 
  
  dd <- Five_list_Cleaned[[k]] %>% 
    select(IDNR, Year, COMPCAT_one, NACE_CAT, LP, LP_g, Zeta, EMPL) %>%
    filter(EMPL > 1) %>%
    mutate(LP = LP/1000)%>%
    mutate(LP_g = LP_g * 100,
           Zeta = Zeta * 100) 
    
  dd <- as.data.frame(dd)
  
  dd$Cond <- dd[, cond_ind] # creating a new column of the class variable for convenience
  dd$Var <- dd[, var_ind]# creating a new column of the value variable for convenience

  dd <- dd %>%
    select(IDNR, Cond, Var) %>%
    na.omit() %>%
    filter(Var > quantile(Var, neg_cut) & Var < quantile(Var, pov_cut)) %>% # cut extreme parts of tails
    group_by(Cond) %>%
    filter(length(IDNR) > cut_num)
  
  dd_info <- dd %>% ## to get the min and max of the axis for the plot
    group_by(Cond) %>%
    summarise(x_min = min(hist(Var, breaks = seq(min(Var), max(Var),l= 100+1), plot = F)$mids),
              x_max = max(hist(Var, breaks = seq(min(Var), max(Var),l= 100+1), plot = F)$mids),
              y_min = min(hist(Var, breaks = seq(min(Var), max(Var),l= 100+1), plot = F)$density[hist(Var, breaks = seq(min(Var), max(Var),l= 100+1), plot = F)$density > 0]),
              y_max = max(hist(Var, breaks = seq(min(Var), max(Var),l= 100+1), plot = F)$density)
              )
   
  x_min <- min(dd_info$x_min)
  y_min <- min(dd_info$y_min)
  x_max <- max(dd_info$x_max)
  y_max <- max(dd_info$y_max) 
  
  c_uni <- unique(dd$Cond)
  
  c_uni_name <- c(); c_uni_num<- c() ## this part will be repeated throughout the script. It is designed to make the coloring of plots consistent across different subsamples. How it works it the following. I have the names of all classes by year, size, and industry. Go line 143 - 148 to see the unique factor by three different categorization of firms. It is important to note that some samples do not have all the classes, e.g. Country A missing year 2007, size Small, and Industry 1,2,10. Therefore, I first assign unique pch and color to each member of all classes, e.g. pch = 1 and color "red" to year 2007 and use this coloring and shape of the plot for all classes in the subsample. 
  
  if(is.numeric(c_uni)){
    c_uni_num <- sort(c_uni)
    c_uni_name <-  c_uni_num
  }else{
    for(i in 1:length(c_uni)){
      c_uni_num[i] <- which(c_names%in%c_uni[i])
    
    }
    c_uni_num <- sort(c_uni_num) 
    c_uni_name <- c_names[c_uni_num]
  }

  color_ind <- c(brewer.pal(n = 8, name = 'Dark2'), brewer.pal(n = 8, name = 'Set3'),"grey", "blue","green","red")[1:length(c_names)]
   
  
  
  plot(c(x_min, x_max), c(y_min, y_max), cex = 0,  log = "y", yaxt = "n", xaxt = "n", cex.main = 1.2, xlab = x_lab, ylab = "Log-Density", main = country_names_five[k])
  axis(side = 1, lwd = 0.3, cex.axis=0.9)
  axis(side = 2, lwd = 0.3, cex.axis=.9)
  
  c_ind_all <- c()
   for(c in 1:length(c_uni_name)){
     
   c_lp <- dd$Var[dd$Cond == c_uni_name[c]] # get the variable conditional on each class
   c_hist <- hist(c_lp, breaks = seq(min(c_lp), max(c_lp),l= 100+1), plot = F)  
   
   c_ind <- which(c_names%in%c_uni_name[c])
  points(c_hist$mids, c_hist$density, pch =  c_ind , cex = 0.35, col = color_ind[c_ind])
  
  c_ind_all[c] <- c_ind

   }

    legend("topright", legend = c_uni_num, pch = c_ind_all, col = color_ind[c_ind_all], bty='n', xpd=NA, cex = .8,ncol = n_col)


  }
    mtext(paste(title), side=3, line=1, outer=TRUE,cex= 1.1)
  dev.off()
  
}


## ------------------------------------------------------------------------
neg_cut <- 0.0025
pov_cut <- 0.9975
## cross-sectional plots of LP and LP\_change (country-year)

setwd("~/Desktop/Cleaned Rda/Productivity/Figures")


fun_plot_marginal(pdf_name = "Figure_Country_Year_LP", title = "Log Density of Labor Productivty", cond_ind = 2, var_ind = 5, x_lab = "LP", c_names = year_names, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 0, n_col = 2)

fun_plot_marginal(pdf_name = "Figure_Country_Year_LP_Growth", title = "Log Density of Labor Productivty Growth", cond_ind = 2, var_ind = 6, x_lab = "LP Growth(%)", c_names = year_names,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 0, n_col = 2)

fun_plot_marginal(pdf_name = "Figure_Country_Year_TFP_Growth", title = "Log Density of TFP Growth", cond_ind = 2, var_ind = 7, x_lab = "TFP (%)", c_names = year_names,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 0, n_col = 2)

## cross-sectional plots of LP and LP\_change (country-size)

fun_plot_marginal(pdf_name = "Figure_Country_Size_LP", title = "Log Density of Labor Productivty", cond_ind = 3, var_ind = 5, x_lab = "LP", c_names = size_names, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000, n_col = 1)

fun_plot_marginal(pdf_name = "Figure_Country_Size_LP_Growth", title = "Log Density of Labor Productivty Growth", cond_ind = 3, var_ind = 6, x_lab = "LP Growth(%)", c_names = size_names,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000, n_col = 1)

fun_plot_marginal(pdf_name = "Figure_Country_Size_TFP_Growth", title = "Log Density of TFP Growth", cond_ind = 3, var_ind = 7, x_lab = "TFP Growth(%)", c_names = size_names,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000, n_col = 1)

## cross-sectional plots of LP and LP\_change (country-industry)

fun_plot_marginal(pdf_name = "Figure_Country_Industry_LP", title = "Log Density of Labor Productivty", cond_ind = 4, var_ind = 5, x_lab = "LP", c_names = ind_names,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 2000, n_col = 3)

fun_plot_marginal(pdf_name = "Figure_Country_Industry_LP_Growth", title = "Log Density of Labor Productivty Growth", cond_ind = 4, var_ind = 6, x_lab = "LP Growth(%)", c_names = ind_names,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 2000, n_col = 3)

fun_plot_marginal(pdf_name = "Figure_Country_Industry_TFP_Growth", title = "Log Density of TFP Growth", cond_ind = 4, var_ind = 7, x_lab = "TFP Growth(%)", c_names = ind_names,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 2000, n_col = 3)

## ------------------------------------------------------------------------


fun_plot_marginal_tail <- function(pdf_name, title, cond_ind, var_ind, x_lab, c_names, leg_pos, cut_tail, neg_cut, pov_cut, cut_num, n_col){
  ### LP_change
  
  #color_ind <- brewer.pal(length(c_names), "Paired")
  
    pdf(paste(pdf_name,".pdf", sep = ""), height = 2.7, width = 10)
  par(mfrow=c(1,5), mar=c(3, 2.5, 1, 1), mgp=c(1.5,.3,0), tck=-.01, oma=c(0,0,4,0))

  for(k in 1:5){
  print(k)
  dd <- Five_list_Cleaned[[k]] %>%
    select(IDNR, Year, COMPCAT_one, NACE_CAT, LP, LP_g, Zeta, EMPL) %>%
    filter(EMPL > 1) %>%
    mutate(LP = LP/1000)%>%
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

  dd_info <- dd %>%
    group_by(Cond) %>%
    filter(Var > quantile(Var, cut_tail)) %>%
    summarise(x_min = min(hist(log(Var), breaks = seq(min(log(Var)), max(log(Var)),l= 50+1), plot = F)$mids),
              x_max = max(hist(log(Var), breaks = seq(min(log(Var)), max(log(Var)),l= 50+1), plot = F)$mids),
              y_min = min(hist(log(Var), breaks = seq(min(log(Var)), max(log(Var)),l= 50+1), plot = F)$density),
              y_max = max(hist(log(Var), breaks = seq(min(log(Var)), max(log(Var)),l= 50+1), plot = F)$density)
              )
   
  x_min <- min(dd_info$x_min)
  y_min <- min(dd_info$y_min[dd_info$y_min > 0])
  x_max <- max(dd_info$x_max)
  y_max <- max(dd_info$y_max) 
  
  c_uni <- unique(dd$Cond)
  
  c_uni_name <- c(); c_uni_num <- c()
  if(is.numeric(c_uni)){
    c_uni_num <- sort(c_uni)
    c_uni_name <-  c_uni_num
  }else{
    for(i in 1:length(c_uni)){
      c_uni_num[i] <- which(c_names%in%c_uni[i])
    
    }
    c_uni_num <- sort(c_uni_num)
    c_uni_name <- c_names[c_uni_num]
  }
  
   color_ind <- c(brewer.pal(n = 8, name = 'Dark2'), brewer.pal(n = 8, name = 'Set3'),"grey", "blue","green","red")[1:length(c_names)]
   

   
  plot(c(x_min, x_max), c(y_min, y_max), cex = 0,  log = "y", yaxt = "n", xaxt = "n", cex.main = 1.2, xlab = x_lab, ylab = "Log-Density", main = country_names_five[k])
  axis(side = 1, lwd = 0.3, cex.axis=0.9)
  axis(side = 2, lwd = 0.3, cex.axis=.9)
  

     c_ind_all <- c()
   
   for(c in 1:length(c_uni_name)){
     
   c_lp <- dd$Var[dd$Cond == c_uni_name[c]]
   c_lp <- c_lp[c_lp > quantile(c_lp, cut_tail)]
     c_hist <- hist(log(c_lp), breaks = seq(min(log(c_lp)), max(log(c_lp)),l= 50+1), plot = F)   
   c_ind <- which(c_names%in%c_uni_name[c])
  points(c_hist$mids, c_hist$density, pch =  c_ind , cex = 0.35, col = color_ind[c_ind])
  
  c_ind_all[c] <- c_ind

   }

legend(leg_pos, legend = c_uni_num, pch = c_ind_all, col = color_ind[c_ind_all], bty='n', xpd=NA, cex = .8,ncol = n_col)


  }
    mtext(paste(title), side=3, line=1, outer=TRUE,cex= 1.)
  dev.off()
  
}


## ------------------------------------------------------------------------

## cross-sectional plots of LP and LP\_change (country-year)


fun_plot_marginal_tail(pdf_name = "Figure_Country_Year_LP_pov_tail", title = "Right Tail of Log Density of Labor Productivty", cond_ind = 2, var_ind = 5, x_lab = "log(LP)", c_names = year_names, leg_pos = "topright", cut_tail = 0.9, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 0, n_col = 2)

fun_plot_marginal_tail(pdf_name = "Figure_Country_Year_LP_Growth_pov_tail", title = "Right Tail of Log Density of Labor Productivty Growth", cond_ind = 2, var_ind = 6, x_lab = "log(LP Growth)", c_names = year_names, leg_pos = "topright", cut_tail = 0.9, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 0, n_col = 2)

fun_plot_marginal_tail(pdf_name = "Figure_Country_Year_TFP_Growth_pov_tail", title = "Right Tail of Log Density of TFP Growth", cond_ind = 2, var_ind = 7, x_lab = "log(TFP Growth)", c_names = year_names, leg_pos = "topright", cut_tail = 0.9, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 0, n_col = 2)

## 
fun_plot_marginal_tail(pdf_name = "Figure_Country_Size_LP_pov_tail", title = "Right Tail of Log Density of Labor Productivty", cond_ind = 3, var_ind = 5, x_lab = "log(LP)", c_names = size_names, leg_pos = "bottomleft", cut_tail = 0.9, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000, n_col = 1)

fun_plot_marginal_tail(pdf_name = "Figure_Country_Size_LP_Growth_pov_tail", title = "Right Tail of Log Density of Labor Productivty Growth", cond_ind = 3, var_ind = 6, x_lab = "log(LP Growth)", c_names = size_names, leg_pos = "bottomleft", cut_tail = 0.9, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000, n_col = 1)

fun_plot_marginal_tail(pdf_name = "Figure_Country_Size_TFP_Growth_pov_tail", title = "Right Tail of Log Density of TFP Growth", cond_ind = 3, var_ind = 6, x_lab = "log(TFP Growth)", c_names = size_names, leg_pos = "bottomleft", cut_tail = 0.9, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000, n_col = 1)

## cross-sectional plots of LP and LP\_change (country-industry)

fun_plot_marginal_tail(pdf_name = "Figure_Country_Industry_LP_pov_tail", title = "Right Tail of Log Density of Labor Productivty", cond_ind = 4, var_ind = 5, x_lab = "log(LP)", c_names = ind_names, leg_pos = "bottomleft", cut_tail = 0.9, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 2000, n_col = 3)

fun_plot_marginal_tail(pdf_name = "Figure_Country_Industry_LP_Growth_pov_tail", title = "Right Tail of Log Density of Labor Productivty Growth", cond_ind = 4, var_ind = 6, x_lab = "log(LP Growth)", c_names = ind_names, leg_pos = "bottomleft", cut_tail = 0.9, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 2000, n_col = 3)

fun_plot_marginal_tail(pdf_name = "Figure_Country_Industry_TFP_Growth_pov_tail", title = "Right Tail of Log Density of TFP Growth", cond_ind = 4, var_ind = 6, x_lab = "log(TFP Growth)", c_names = ind_names, leg_pos = "bottomleft", cut_tail = 0.9, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 2000, n_col = 3)


## ------------------------------------------------------------------------


fun_plot_marginal_tail_neg <- function(pdf_name, title, cond_ind, var_ind, x_lab, c_names,  leg_pos, cut_tail, neg_cut, pov_cut, cut_num, n_col){
  ### LP_change
  
 
    pdf(paste(pdf_name,".pdf", sep = ""), height = 2.7, width = 10)
  par(mfrow=c(1,5), mar=c(3, 2.5, 1, 1), mgp=c(1.5,.3,0), tck=-.01, oma=c(0,0,4,0))

  for(k in 1:5){
  print(k)
  dd <- Five_list_Cleaned[[k]] %>%
    select(IDNR, Year, COMPCAT_one, NACE_CAT, LP, LP_g, Zeta, EMPL) %>%
    filter(EMPL > 1) %>%
    mutate(LP = LP/1000)%>%
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


  dd_info <- dd %>%
    group_by(Cond) %>%
    filter(Var < quantile(Var, cut_tail)) %>%
    mutate(Var = Var - min(Var)) %>%
    filter(Var > 0) %>%
    summarise(x_min = min(hist(log(Var), breaks = seq(min(log(Var)), max(log(Var)),l= 50+1), plot = F)$mids),
              x_max = max(hist(log(Var), breaks = seq(min(log(Var)), max(log(Var)),l= 50+1), plot = F)$mids),
              y_min = min(hist(log(Var), breaks = seq(min(log(Var)), max(log(Var)),l= 50+1), plot = F)$density[hist(log(Var), breaks = seq(min(log(Var)), max(log(Var)),l= 50+1), plot = F)$density>0]),
              y_max = max(hist(log(Var), breaks = seq(min(log(Var)), max(log(Var)),l= 50+1), plot = F)$density)
              )
   
  x_min <- min(dd_info$x_min)
  y_min <- min(dd_info$y_min)
  x_max <- max(dd_info$x_max)
  y_max <- max(dd_info$y_max) 
  
  
  c_uni <- unique(dd$Cond)
  
  c_uni_name <- c(); c_uni_num <- c()
  if(is.numeric(c_uni)){
    c_uni_num <- sort(c_uni)
    c_uni_name <-  c_uni_num
  }else{
    for(i in 1:length(c_uni)){
      c_uni_num[i] <- which(c_names%in%c_uni[i])
    
    }
    c_uni_num <- sort(c_uni_num)
    c_uni_name <- c_names[c_uni_num]
  }
  
  color_ind <- c(brewer.pal(n = 8, name = 'Dark2'), brewer.pal(n = 8, name = 'Set3'),"grey", "blue","green","red")[1:length(c_names)]
 
  
  plot(c(x_min, x_max), c(y_min, y_max), cex = 0,  log = "y", yaxt = "n", xaxt = "n", cex.main = 1.2, xlab = x_lab, ylab = "Log-Density", main = country_names_five[k])
  axis(side = 1, lwd = 0.3, cex.axis=0.9)
  axis(side = 2, lwd = 0.3, cex.axis=.9)
  
   c_ind_all <- c()
   
   for(c in 1:length(c_uni_name)){
     
   c_lp <- dd$Var[dd$Cond == c_uni_name[c]]
   c_lp <- c_lp[c_lp < quantile(c_lp, cut_tail)]
   c_lp <- c_lp - min(c_lp)
   c_lp <- c_lp[c_lp > 0]
 
   c_hist <- hist(log(c_lp), breaks = seq(min(log(c_lp)), max(log(c_lp)),l= 50+1), plot = F)   
   c_ind <- which(c_names%in%c_uni_name[c])
  points(c_hist$mids, c_hist$density, pch =  c_ind , cex = 0.35, col = color_ind[c_ind])
  
  c_ind_all[c] <- c_ind

   }

legend(leg_pos, legend = c_uni_num, pch = c_ind_all, col = color_ind[c_ind_all], bty='n', xpd=NA, cex = .8,ncol = n_col)


  }
    mtext(paste(title), side=3, line=1, outer=TRUE,cex= 1.)
  dev.off()
  
}


## ------------------------------------------------------------------------

## cross-sectional plots of LP and LP\_change (country-year)

fun_plot_marginal_tail_neg(pdf_name = "Figure_Country_Year_LP_neg_tail", title = "Left Tail of Log Density of Labor Productivty", cond_ind = 2, var_ind = 5, x_lab = "log(LP)", c_names = year_names,  leg_pos = "topleft", cut_tail = 0.1, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 10000, n_col = 2)

fun_plot_marginal_tail_neg(pdf_name = "Figure_Country_Year_LP_Growth_neg_tail", title = "Left Tail of Log Density of Labor Productivty Growth", cond_ind = 2, var_ind = 6, x_lab = "log(LP Growth)", c_names = year_names, leg_pos = "topleft", cut_tail = 0.1, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 10000, n_col = 2)

fun_plot_marginal_tail_neg(pdf_name = "Figure_Country_Year_TFP_Growth_neg_tail", title = "Left Tail of Log Density of TFP Growth", cond_ind = 2, var_ind = 6, x_lab = "log(TFP Growth)", c_names = year_names, leg_pos = "topleft", cut_tail = 0.1, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 10000, n_col = 2)

## 
fun_plot_marginal_tail_neg(pdf_name = "Figure_Country_Size_LP_neg_tail", title = "Left Tail of Log Density of Labor Productivty", cond_ind = 3, var_ind = 5, x_lab = "log(LP)", c_names = size_names, leg_pos = "topleft", cut_tail = 0.1, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000, n_col = 1)

fun_plot_marginal_tail_neg(pdf_name = "Figure_Country_Size_LP_Growth_neg_tail", title = "Left Tail of Log Density of Labor Productivty Growth", cond_ind = 3, var_ind = 6, x_lab = "log(LP Growth)", c_names = size_names,  leg_pos = "topleft", cut_tail = 0.1, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000, n_col = 1)

fun_plot_marginal_tail_neg(pdf_name = "Figure_Country_Size_TFP_Growth_neg_tail", title = "Left Tail of Log Density of TFP Growth", cond_ind = 3, var_ind = 6, x_lab = "log(TFP Growth)", c_names = size_names,  leg_pos = "topleft", cut_tail = 0.1, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000, n_col = 1)

## cross-sectional plots of LP and LP\_change (country-industry)

fun_plot_marginal_tail_neg(pdf_name = "Figure_Country_Industry_LP_neg_tail", title = "Left Tail of Log Density of Labor Productivty", cond_ind = 4, var_ind = 5, x_lab = "log(LP)", c_names = ind_names,  leg_pos = "topleft", cut_tail = 0.1, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 2000, n_col = 3)

fun_plot_marginal_tail_neg(pdf_name = "Figure_Country_Industry_LP_Growth_neg_tail", title = "Left Tail of Log Density of Labor Productivty Growth", cond_ind = 4, var_ind = 6, x_lab = "log(LP Growth)", c_names = ind_names, leg_pos = "topleft", cut_tail = 0.1, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 2000, n_col = 3)

fun_plot_marginal_tail_neg(pdf_name = "Figure_Country_Industry_TFP_Growth_neg_tail", title = "Left Tail of Log Density of TFP Growth", cond_ind = 4, var_ind = 6, x_lab = "log(TFP Growth)", c_names = ind_names, leg_pos = "topleft", cut_tail = 0.1, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 2000, n_col = 3)


## ------------------------------------------------------------------------
library(knitr)
setwd("~/Desktop/Cleaned Rda/Productivity")
purl("Outline.Rmd")  

