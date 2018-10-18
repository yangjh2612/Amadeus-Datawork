## ------------------------------------------------------------------------
load("/Users/janghoyang/Desktop/Cleaned Rda/First Report/Cleaned_up_info_list.Rda")

## ------------------------------------------------------------------------
#dat_load <- EMPL_change_mean_info
library(colorspace)
fun_plot_gen_log_pdf <- function(dat_load, var_name, var_name_title, leg_loc, log_ind){
  
  pdf(paste("Figure_",var_name, "_","log_pdf.pdf", sep = ""), height = 15, width = 12)
  par(mfrow=c(6,5), mar=c(3, 2.5, 1, 1), mgp=c(1.5,.3,0), tck=-.01, oma=c(0,0,4,0))

  # drop the empty elements
  drop_ind <- which(lapply(dat_load[[1]], function(x) sum(!is.na(x))) == 0)
  if(length(drop_ind) > 0){
  dat_load[[1]] <- dat_load[[1]][-c(drop_ind)]
  dat_load[[2]] <- dat_load[[2]][-c(drop_ind)]
  dat_load[[3]] <- dat_load[[3]][-c(drop_ind)]
  } else{
    dat_load <- dat_load
  }
  
  
  country_ind <- dat_load[[3]]
  
  #country_ind <- country_ind[country_ind%in%c(country_names[take_con])]
  
  for(c_number in 1:length(country_ind)){
#c_number <- 26
  year_ind = unlist(dat_load[[2]][c_number])
  country_ind =   country_ind
  dat_list = dat_load[[1]][[c_number]]

  
  # Take out those years that have zero obs
  error_ind <- sum(lapply(dat_list, function(x) length(x)) == 1)
  
    if(error_ind == 0){
       dat_list <- dat_list
       year_ind <- year_ind
    } else{
      error_ind <- which(lapply(dat_list, function(x) length(x)) == 1)
       dat_list <- dat_list[-c(error_ind)]
       year_ind <- year_ind[-c(error_ind)]
    }

 colores_c <- rainbow_hcl(length(year_ind), end=300)
  
 # calucate the min and max of ylim and xlim of all the yearly distrbutions
  y_max <- max(unlist(lapply(dat_list, function(x) max(x[[2]]/sum(x[[2]])))))
  y_min_b <- lapply(dat_list, function(x) x[[2]]/sum(x[[2]]))
  y_min_b <- lapply(y_min_b, function(x) x[x>0])
  y_min <-   min(unlist(lapply(y_min_b, function(x) sort(x,partial=2)[2])))
  
  x_max <- max(unlist(lapply(dat_list, function(x) max(x[[4]]))))
  x_min <- min(unlist(lapply(dat_list, function(x) min(x[[4]]))))
 
  
  y <- 1
  
  # p_rep is the normalized dnesity 
  p_rep <- dat_list[[y]][[2]]/sum(dat_list[[y]][[2]])
  p_rep[p_rep==0] <- NA
  p_length <- length(p_rep)

  # log_ind means taking log() in the x_lab
  if(log_ind == 1){
    xlab_n = paste("log(",var_name,")")
  } else{
    xlab_n = paste(var_name)
  }

  plot(dat_list[[y]][[4]],  p_rep, pch = 20, cex = 0,  log = "y", col = colores_c[y], yaxt = "n", xaxt = "n", cex.main = 1.2, xlab = xlab_n, ylab = "Log-Density", main = country_ind[c_number], ylim  = c(y_min, y_max),  xlim  = c(x_min, x_max))
  axis(side = 1, lwd = 0.3, cex.axis=0.6)
  axis(side = 2, lwd = 0.3, cex.axis=0.6)
  
   for(y in 1:length(year_ind)){
   p_rep <- dat_list[[y]][[2]]/sum(dat_list[[y]][[2]])
   p_rep[p_rep==0] <- NA
   p_length <- length(p_rep)


  points(dat_list[[y]][[4]][-c(p_length)],  p_rep[-c(p_length)], pch = y, cex = 0.5, col = colores_c[y])

     }
  legend(leg_loc, legend =   year_ind, pch = 1:length(  year_ind), col = colores_c[1:length(  year_ind)], bty='n', xpd=NA, cex = .7)

  }
    mtext(paste("Log Density of ",var_name_title), side=3, line=1, outer=TRUE,cex= 2.)
  dev.off()
}


#####
#####
#####
fun_plot_gen_log_pdf_mean <- function(dat_load, var_name, var_name_title, leg_loc, log_ind){
  
  pdf(paste("Figure_Mean_",var_name, "_","log_pdf.pdf", sep = ""), height = 15, width = 12)
  par(mfrow=c(6,5), mar=c(3, 2.5, 1, 1), mgp=c(1.5,.3,0), tck=-.01, oma=c(0,0,4,0))

  #dat_load <- CP_change_info
  #leg_loc <- "topright"
  #var_name <- "Sale"
  #log_ind <- 1
  
  # Take those countries that have at least one year obs
  drop_ind <- which(lapply(dat_load[[1]], function(x) sum(!is.na(x))) == 0)
  if(length(drop_ind) > 0){
  dat_load[[1]] <- dat_load[[1]][-c(drop_ind)]
  dat_load[[2]] <- dat_load[[2]][-c(drop_ind)]
  } else{
    dat_load <- dat_load
  }
  
  country_ind <- dat_load[[2]]
  #country_ind <- country_ind[country_ind%in%c(country_names[take_con])]

  
  
  colores_c <- rainbow_hcl(length(  country_ind ), end=300)
 
  for(c_number in 1:length(country_ind)){
#c_number <- 26
 # year_ind = unlist(dat_load[[2]][c_number])
  country_ind =   country_ind
  dat_list = dat_load[[1]][[c_number]]

  
  # Take out those years that have zero obs
  error_ind <- sum(lapply(dat_list, function(x) length(x)) == 1)
  
    if(error_ind == 0){
       dat_list <- dat_list
      
    } else{
      error_ind <- which(lapply(dat_list, function(x) length(x)) == 1)
       dat_list <- dat_list[-c(error_ind)]
      
    }

  
 # calucate the min and max of ylim and xlim of all the yearly distrbutions

  
  # p_rep is the normalized dnesity 
  p_rep <- dat_list[[2]]/sum(dat_list[[2]])
  p_rep[p_rep==0] <- NA
  p_length <- length(p_rep)

  # log_ind means taking log() in the x_lab
  if(log_ind == 1){
    xlab_n = paste("log(",var_name,")")
  } else{
    xlab_n = paste(var_name)
  }

  plot(dat_list[[4]],  p_rep, pch = 20, cex = 1,  log = "y", col = colores_c[c_number], yaxt = "n", xaxt = "n", cex.main = 1.2, xlab = xlab_n, ylab = "Log-Density", main = country_ind[c_number])
  axis(side = 1, lwd = 0.3, cex.axis=0.6)
  axis(side = 2, lwd = 0.3, cex.axis=0.6)

  }
    mtext(paste("Log Density of ",var_name_title), side=3, line=1, outer=TRUE,cex= 2.)
  dev.off()
}


## ------------------------------------------------------------------------

# this is to get the countries that have at least 5 year obs. 
#fun_cut_5 <- function(dat_info){
 # ok_ind <- which(unlist(lapply(dat_info[[1]], function(x) sum(!is.na(x)) >= 5)))
  #dat_info[[1]] <- dat_info[[1]][c(ok_ind)]
  #dat_info[[2]] <- dat_info[[2]][c(ok_ind)]
  #dat_info[[3]] <- dat_info[[3]][c(ok_ind)]
  #return(dat_info)
#}


#Profitability
setwd("~/Desktop/Cleaned Rda/First Report/Profitability")
fun_plot_gen_log_pdf(Profitability_info_list[[1]], "RoC", "Returns on Total Asset", "topleft", log_ind = 0)
fun_plot_gen_log_pdf(Profitability_info_list[[2]], "RoC_fix", "Returns on Fixed Asset", "topright", log_ind = 0)

fun_plot_gen_log_pdf(Profitability_info_list[[3]], "RoC_RCEM", "Returns on Capital Employed", "topleft", log_ind = 0)

#Productivity
setwd("~/Desktop/Cleaned Rda/First Report/Productivity")
fun_plot_gen_log_pdf(Productivity_info_list[[1]], "LP", "Labor Productivity", "topright", log_ind = 0) # change the loc of legend
fun_plot_gen_log_pdf(Productivity_info_list[[2]], "CP","Capital Productivity", "topright", log_ind = 0)
fun_plot_gen_log_pdf(Productivity_info_list[[3]], "LP_change", "Labor Productivity Change", "topleft", log_ind = 0)
fun_plot_gen_log_pdf(Productivity_info_list[[4]], "CP_change","Capital Productivity Change",  "topleft", log_ind = 0)
fun_plot_gen_log_pdf(Productivity_info_list[[5]], "Zeta","Rate of Cost Reduction", "topleft", log_ind = 0)


#Firm size
setwd("~/Desktop/Cleaned Rda/First Report/Firm_size")

fun_plot_gen_log_pdf(Firm_size_info_list[[4]], "SALE_change","Growth Rate of Sales", "topright", log_ind = 0)
fun_plot_gen_log_pdf(Firm_size_info_list[[5]], "TOAS_change","Growth Rate of Total Asset", "topright", log_ind = 0)


#Cost structure
setwd("~/Desktop/Cleaned Rda/First Report/Cost_structure")

fun_plot_gen_log_pdf(Cost_structure_info_list[[1]], "WS","Wage Share", "topright", log_ind = 0)
fun_plot_gen_log_pdf(Cost_structure_info_list[[2]], "PW_ratio","Profit-Wage Ratio", "topright", log_ind = 0)

## Mean distribution 
setwd("~/Desktop/Cleaned Rda/First Report/Mean")
fun_plot_gen_log_pdf_mean(mean_info_list[[1]], "EMPL_m_change","Employment change", "topright", log_ind = 0)
fun_plot_gen_log_pdf_mean(mean_info_list[[2]], "TOAS_m_change","TOAS change", "topright", log_ind = 0)
fun_plot_gen_log_pdf_mean(mean_info_list[[3]], "LP_m_change","LP change", "topright", log_ind = 0)
fun_plot_gen_log_pdf_mean(mean_info_list[[4]], "Zeta_m","Zeta", "topright", log_ind = 0)
fun_plot_gen_log_pdf_mean(mean_info_list[[5]], "RoC_m","RoC", "topright", log_ind = 0)
fun_plot_gen_log_pdf_mean(mean_info_list[[6]], "RoC_RCEM_m","RoC_RCEM", "topright", log_ind = 0)

