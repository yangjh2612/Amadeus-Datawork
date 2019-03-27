##################  Important: Run this file after Prod_Fitting_Levy.R ##################

############ 0. Basic Set up ############
## loading of required libraries
# loading of libraries
if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(colorspace,RColorBrewer, msir)

# loading of data
load("Year_Levy_list_boot.Rda")
load("Size_Levy_list_boot.Rda")
load("Industry_Levy_list_boot.Rda")

load("Labels.Rda")
load("All_list_Cleaned_cut.Rda")

############ 1. Plot the distribution with the Levy fitted lines ############
## Basically the same plot function but augumented with the fitter line from the Levy distribution. For detailed explanations, see the function detail of "Descriptive_Analysis.R" script.

## 1.1. function
fun_plot_levy_fit <- function(pdf_name, title, dat_list, c_names, c_names_leg, x_lab, n_col_f, n_col, cex_leg_f, cex_leg) {

  five_ind <- which(country_names %in% country_names_five)


  pdf(paste(pdf_name, ".pdf", sep = ""),  height = 5.5, width = 8.5)
  par(mfrow = c(2, 3), mar = c(3, 2.5, 1, 1), mgp = c(1.5, .3, 0), tck = -.01, oma = c(0, 0, 4, 0))

  color_ind <- c(brewer.pal(n = 8, name = "Dark2"), brewer.pal(n = 8, name = "Set3"), "grey", "blue", "green", "red")[1:length(c_names_leg)] # color index 
  
  plot(1,1, cex = 0, yaxt = "n", xaxt = "n", xlab ="", ylab = "", main = "", bty = "n") # empty plot 
  
  legend("topleft", legend = c_names_leg, pch = 1:length(c_names_leg), col = color_ind[1:length(c_names_leg)], bty = "n", xpd = NA, cex =  cex_leg_f, ncol = n_col_f)
  
  
  
  for (k in five_ind) {
    print(k)
    con_temp <- dat_list[[1]][[k]]

    c_uni <- dat_list[[2]][[k]]

    levy_soofi <- round(unlist(lapply(con_temp, function(x) x$levy_soofi)), 0)

    if(sum(is.na(levy_soofi)) > 0 | sum(levy_soofi < 50) > 0 ){
      ok_ind <- which(is.na(levy_soofi) | levy_soofi< 50)
      con_temp <- con_temp[-ok_ind]
      c_uni <- c_uni[-ok_ind]
      levy_soofi <- levy_soofi[-ok_ind]
    }
    
    
    y_max <- max(unlist(lapply(con_temp, function(x) max(x$data_p, x$levy_q))))
    y_min <- min(unlist(lapply(con_temp, function(x) min(x$data_p[x$data_p > 0], x$levy_q[x$levy_q > 0]))))

    
    plot(con_temp[[1]]$data_mid, con_temp[[1]]$data_p, cex = 0, log = "y", yaxt = "n", xaxt = "n", cex.main = 1.2, xlab = x_lab, ylab = "Log-Density", main = country_names[k], ylim = c(y_min, y_max))
    axis(side = 1, lwd = 0.3, cex.axis = 0.9)
    axis(side = 2, lwd = 0.3, cex.axis = .9)

    c_ind_all <- c()

    for (y in 1:length(con_temp)) {
      c_ind <- which(c_names %in% c_uni[y])
      points(con_temp[[y]]$data_mid, con_temp[[y]]$data_p, pch = c_ind, cex = 0.35, col = color_ind[c_ind])

      lines(con_temp[[y]]$data_mid, con_temp[[y]]$levy_q, col = color_ind[c_ind], lwd = 1., lty = 1) # Levy fit

      c_ind_all[y] <- c_ind
    }


    legend("topright",  legend = paste(levy_soofi), pch = c_ind_all, col = color_ind[c_ind_all], bty = "n", xpd = NA, cex = cex_leg, ncol = n_col)
  }

  mtext(paste(title), side = 3, line = 1, outer = TRUE, cex = 1.)
  dev.off()
}


## 1.2. plot

## Year class
# LP conditional on year (year class)
fun_plot_levy_fit(pdf_name = "Figure_LP_Year_Levy_Fit", title = "Labor Productivity by Year with Levy Fit", dat_list = LP_year_Levy_list_boot, c_names = year_names, c_names_leg = year_names, x_lab = "LP", n_col_f = 2, n_col = 2, cex_leg_f = 1.4, cex_leg = 1)

# LP_change conditional on year
fun_plot_levy_fit(pdf_name = "Figure_LP_Change_Year_Levy_Fit", title = "Labor Productivity Change by Year with Levy Fit", dat_list = LP_Change_year_Levy_list_boot, c_names = year_names,  c_names_leg = year_names[-c(1)], x_lab = "LP_Change", n_col_f = 2,  n_col = 2, cex_leg_f = 1.4, cex_leg = 1)



## Size class
# LP conditional on size
fun_plot_levy_fit(pdf_name = "Figure_LP_Size_Levy_Fit", title = "Labor Productivity by Size with Levy Fit", dat_list = LP_size_Levy_list_boot, c_names = size_names_long, c_names_leg = size_names_long, x_lab = "LP",  n_col_f = 1, n_col = 1, cex_leg_f = 1.4, cex_leg = 1)

# LP_change conditional on size
fun_plot_levy_fit(pdf_name = "Figure_LP_Change_Size_Levy_Fit", title = "Labor Productivity Growth by Size with Levy Fit", dat_list = LP_Change_size_Levy_list_boot, c_names = size_names_long, c_names_leg = size_names_long, x_lab = "LP_Change",  n_col_f = 1, n_col = 1, cex_leg_f = 1.4, cex_leg = 1)




## Industry class
# LP conditional on sector
fun_plot_levy_fit(pdf_name = "Figure_LP_Ind_Levy_Fit", title = "Labor Productivity by Industry with Levy Fit", dat_list = LP_ind_Levy_list_boot, c_names = ind_name_table$ind_names, c_names_leg = ind_name_table$ind_names_short, x_lab = "LP",  n_col_f = 2, n_col = 3, cex_leg_f = 1.2, cex_leg = 0.75)

# LP_change conditional on sector
fun_plot_levy_fit(pdf_name = "Figure_LP_Change_Ind_Levy_Fit", title = "Labor Productivity Change by Industry with Levy Fit", dat_list = LP_Change_ind_Levy_list_boot, c_names = ind_name_table$ind_names, c_names_leg = ind_name_table$ind_names_short, x_lab = "LP_Change",  n_col_f = 2, n_col = 3, cex_leg_f = 1.2, cex_leg = 0.75)


############ 2. Plots for Estimated parameters ############


## 2.1 function (for five countries )

fun_levy_par <- function(pdf_name, title, dat_list, x_value, x_value_name, x_lab, leg_pos) { # this function has 7 arguments. 1) the name of the pdf file, 2) the title of the figure, 3) the data file that is generated from the fun_fit_levy function in "Fitting_Levy_Boot.R" script, 4) all class names, 5) unique class name for each subsample, 6) the name of x label, 7) the position of the legend
  colores_this <- c(brewer.pal(n = 8, name = "Dark2"), brewer.pal(n = 7, name = "Set1"), brewer.pal(n = 7, name = "Set3"))

  five_ind <- which(country_names %in% country_names_five) # index for the top five countries


  c_uni_list <- list(); levy_par <- list(); levy_par_sd <- list()
  for (k in five_ind) {
    c_uni_list[[k]] <- dat_list[[3]][[k]] # the numeric value of the unique class
    levy_par[[k]] <- lapply(dat_list[[1]][[k]], function(x) x$levy_para) # the corresponding levy parameters for each class
    levy_par_sd[[k]] <- lapply(dat_list[[1]][[k]], function(x) apply(x$est_levy_std_error$t, 2, sd)) # the corresponding standard deviation of levy parameters for each class
  }

  par_a <- list(); par_a_sd <- list()
  par_b <- list(); par_b_sd <- list()
  par_g <- list(); par_g_sd <- list()
  par_d <- list(); par_d_sd <- list()

  for (k in five_ind) {
    par_a[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[1])) # parameter \alpha
    par_a_sd[[k]] <- unlist(lapply(levy_par_sd[[k]], function(x) x[1])) # sd of parameter \alpha
    par_b[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[2])) # parameter \beta
    par_b_sd[[k]] <- unlist(lapply(levy_par_sd[[k]], function(x) x[2]))
    par_g[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[3])) # parameter \gamma
    par_g_sd[[k]] <- unlist(lapply(levy_par_sd[[k]], function(x) x[3]))
    par_d[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[4])) # parameter \delta
    par_d_sd[[k]] <- unlist(lapply(levy_par_sd[[k]], function(x) x[4]))
  }


  par_list <- list(par_a, par_b, par_g, par_d)
  par_sd_list <- list(par_a_sd, par_b_sd, par_g_sd, par_d_sd)
  # parameter names
  par_name_list <- list(
    expression(paste(alpha, ": tail")),
    expression(paste(beta, ": skewness")),
    expression(paste(gamma, ": scale")),
    expression(paste(delta, ": location"))
  )
  ylab_list <- list(expression(paste(alpha)), expression(paste(beta)), expression(paste(gamma)), expression(paste(delta))) ## for the y_lab

  # lw_list <- list(lw_par_a, lw_par_b, lw_par_g, lw_par_d)


  ###
  pdf(paste(pdf_name, ".pdf", sep = ""), height = 2.5, width = 9.)
  par(mfrow = c(1, 4), mar = c(3, 1.2, 1, 1), mgp = c(1.5, .3, 0), tck = -.01, oma = c(0, 0, 4, 04))

  l <- 1 # first parameter
  plot(x_value, x_value, yaxt = "n", xaxt = "n", main = par_name_list[[l]], cex.main = 1.2, xlab = x_lab, ylab = ylab_list[[l]], cex.lab = 1.1, cex = 0, ylim = c(min(unlist(par_list[[l]]) - unlist(par_sd_list[[l]])), max(unlist(par_list[[l]]) + unlist(par_sd_list[[l]])) * 1.0)) # Empty plot for the first parameter: x_value is the numeric value of all classes


  axis(side = 1, at = x_value, label = x_value_name, lwd = 0.3, cex.axis = 0.85) # x-axis
  axis(side = 2, lwd = 0.3, cex.axis = 0.85) # y-axis


  for (k in five_ind) {
    points(c_uni_list[[k]], par_list[[l]][[k]], col = colores_this[k], cex = 0.7, pch = 20, type = "b", lty = 1, lwd = 1.3) # c_uni_list is the numeric value of the unique classes in the subsample

    polygon.x <- c(c_uni_list[[k]], rev(c_uni_list[[k]])) # error bar
    polygon.y <- c(c(par_list[[l]][[k]] - par_sd_list[[l]][[k]]), rev(c(par_list[[l]][[k]] + par_sd_list[[l]][[k]])))

    polygon(x = polygon.x, y = polygon.y, col = adjustcolor(colores_this[k], alpha.f = 0.4), border = NA)
  }

  for (l in 2:4) { # 2nd to 4th parameters
    plot(x_value, x_value, yaxt = "n", xaxt = "n", main = par_name_list[[l]], cex.main = 1.2, xlab = x_lab, ylab = ylab_list[[l]], cex.lab = 1.1, cex = 0, ylim = c(min(unlist(par_list[[l]]) - unlist(par_sd_list[[l]])), max(unlist(par_list[[l]]) + unlist(par_sd_list[[l]])))) # Empty plot for the first parameter: x_value is the numeric value of all classes

    axis(side = 1, at = x_value, label = x_value_name, lwd = 0.3, cex.axis = 0.85)
    axis(side = 2, lwd = 0.3, cex.axis = 0.85)

    for (k in five_ind) {
      points(c_uni_list[[k]], par_list[[l]][[k]], col = colores_this[k], cex = 0.7, pch = 20, type = "b", lty = 1, lwd = 1.3) # c_uni_list is the numeric value of the unique classes in the subsample

      # error bars
      polygon.x <- c(c_uni_list[[k]], rev(c_uni_list[[k]]))
      polygon.y <- c(c(par_list[[l]][[k]] - par_sd_list[[l]][[k]]), rev(c(par_list[[l]][[k]] + par_sd_list[[l]][[k]])))

      polygon(x = polygon.x, y = polygon.y, col = adjustcolor(colores_this[k], alpha.f = 0.4), border = NA)
    }
  }

  # legend outside the main plots
  legend("topright", inset=c(-0.35,0), legend = c("FRA", "GER", "ITA", "SPA", "UK"), col = colores_this[five_ind], pch = rep(20, 5), bty = "n", xpd = NA, cex = 1.4, ncol = 1)

  mtext(paste(title), side = 3, line = 1, outer = TRUE, cex = 1.)
  dev.off()
}


## 2.1.2 plot

## Estimated Parameter for Year class

fun_levy_par(pdf_name = "Figure_Levy_Para_LP_Year", title = "Parameters of Levy Distribution: LP by Year", dat_list = LP_year_Levy_list_boot, x_value = 1:10, x_value_name = year_names, x_lab = "Year", leg_pos = "topleft")


# LP_Change
fun_levy_par(pdf_name = "Figure_Levy_Para_LP_Change_Year", title = "Parameters of Levy Distribution: LP Change by Year", dat_list = LP_Change_year_Levy_list_boot, x_value = 2:10, x_value_name = year_names[-c(1)], x_lab = "Year", leg_pos = "bottomright")


## Estimated Parameter for Size class

# LP
fun_levy_par(pdf_name = "Figure_Levy_Para_LP_Size", title = "Parameters of Levy Distribution: LP by Size", dat_list = LP_size_Levy_list_boot, x_value = c(1:4), x_value_name = size_names, x_lab = "Size", leg_pos = "topright")

# LP_Change
fun_levy_par(pdf_name = "Figure_Levy_Para_LP_Change_Size", title = "Parameters of Levy Distribution: LP Change by Size", dat_list = LP_Change_size_Levy_list_boot, x_value = c(1:4), x_value_name = size_names, x_lab = "Year", leg_pos = "bottomright")


############ 3. Productivity by ind chart ############

# the code is very similar to the above one except that the x and y axes are switched
fun_levy_par_ind <- function(pdf_name, title, dat_list, y_value, y_value_name, leg_pos) { # # this function has 7 arguments. 1) the name of the pdf file, 2) the title of the figure, 3) the data file that is generated from the fun_fit_levy function in "Fitting_Levy_Boot.R" script, 4) all class names, 5) unique class name for each subsample, 6) the name of x label, 7) the position of the legend
  colores_this <- c(brewer.pal(n = 8, name = "Dark2"), brewer.pal(n = 7, name = "Set1"), brewer.pal(n = 7, name = "Set3"))
  
  five_ind <- which(country_names %in% country_names_five) # index for the top five countries
  
  
  
  c_uni_list <- list(); levy_par <- list(); levy_par_sd <- list()
  
  for (k in five_ind) {
    
    con_temp <- dat_list[[1]][[k]] # the levy parameters for each class
    c_uni_num <- dat_list[[3]][[k]] # the numeric value of the unique class
    levy_soofi <- round(unlist(lapply(con_temp, function(x) x$levy_soofi)), 0) # soofi ID
    
    # remove those distribution whose Soofi ID is below 50
    if(sum(is.na(levy_soofi)) > 0 | sum(levy_soofi < 50) > 0 ){
      ok_ind <- which(is.na(levy_soofi) | levy_soofi< 50)
      con_temp <- con_temp[-ok_ind]
      c_uni_num <- c_uni_num[-ok_ind]
      levy_soofi <- levy_soofi[-ok_ind]
    }
    
    
    c_uni_list[[k]] <- c_uni_num # the numeric value of the unique class
    levy_par[[k]] <- lapply(con_temp, function(x) x$levy_para) # the corresponding levy parameters for each class
    levy_par_sd[[k]] <- lapply(con_temp, function(x) apply(x$est_levy_std_error$t, 2, sd)) # the corresponding standard deviation of levy parameters for each class
  }
  
  par_a <- list(); par_a_sd <- list()
  par_b <- list(); par_b_sd <- list()
  par_g <- list(); par_g_sd <- list()
  par_d <- list(); par_d_sd <- list()
  
  for (k in five_ind) {
    par_a[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[1])) # parameter \alpha
    par_a_sd[[k]] <- unlist(lapply(levy_par_sd[[k]], function(x) x[1])) # sd of parameter \alpha
    par_b[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[2])) # parameter \beta
    par_b_sd[[k]] <- unlist(lapply(levy_par_sd[[k]], function(x) x[2]))
    par_g[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[3])) # parameter \gamma
    par_g_sd[[k]] <- unlist(lapply(levy_par_sd[[k]], function(x) x[3]))
    par_d[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[4])) # parameter \delta
    par_d_sd[[k]] <- unlist(lapply(levy_par_sd[[k]], function(x) x[4]))
  }
  
  
  par_list <- list(par_a, par_b, par_g, par_d)
  par_sd_list <- list(par_a_sd, par_b_sd, par_g_sd, par_d_sd)
  # parameter names
  par_name_list <- list(
    expression(paste(alpha, ": tail")),
    expression(paste(beta, ": skewness")),
    expression(paste(gamma, ": scale")),
    expression(paste(delta, ": location"))
  )
  ylab_list <- list(expression(paste(alpha)), expression(paste(beta)), expression(paste(gamma)), expression(paste(delta))) ## for the y_lab
  
  # lw_list <- list(lw_par_a, lw_par_b, lw_par_g, lw_par_d)
  
  
  ###
  pdf(paste(pdf_name, ".pdf", sep = ""), height = 7, width = 7.)
  par(mfrow = c(2, 2), mar = c(3, 2.7, 1, 1), mgp = c(1.5, .3, 0), tck = -.01, oma = c(0, 0, 4, 0))
  
  l <- 1 # first parameter
  plot(y_value, y_value, yaxt = "n", xaxt = "n", main = par_name_list[[l]], cex.main = 1.2, xlab = ylab_list[[l]], ylab = "", cex.lab = 1.1, cex = 0, xlim = c(min(unlist(par_list[[l]]) - unlist(par_sd_list[[l]])), max(unlist(par_list[[l]]) + unlist(par_sd_list[[l]])) * 1.0)) # Empty plot for the first parameter: x_value is the numeric value of all classes
  axis(side = 2, at = rev(y_value), label = y_value_name, lwd = 0.3, cex.axis = 0.85, las = 1)
  axis(side = 1, lwd = 0.3, cex.axis = 0.85)
  
  abline(h = y_value, lty = 3, col = "grey")
  
  #Add vertical grid
  abline(v = seq(min(unlist(par_list[[l]]) - unlist(par_sd_list[[l]])), max(unlist(par_list[[l]]) + unlist(par_sd_list[[l]])) * 1.0, length.out = 10),  lty = 3, col = "grey")

  
  for (k in five_ind) {
      # note taht y axis needs to be reversed
    for(z in 1:length(par_list[[l]][[k]])){
      lines(c(c(par_list[[l]][[k]][z] - par_sd_list[[l]][[k]][z]), c(par_list[[l]][[k]][z] + par_sd_list[[l]][[k]][z])), c(21-c_uni_list[[k]][z], 21-c_uni_list[[k]][z]),  lty = 1, lwd = 1.1, col = colores_this[k])
    }
    
    points(par_list[[l]][[k]], 21-c_uni_list[[k]], col = colores_this[k], cex = 1.2, pch = 20,  lty = 2, lwd = .8) # c_uni_list is the numeric value of the unique classes in the subsample
    
    
    }
  
  legend(leg_pos, legend = c("FRA", "GER", "ITA", "SPA", "UK"), col = colores_this[five_ind], pch = rep(20, 5), ncol = 1, cex = 1, bty = "n")
  
  for (l in 2:4) { # 2nd to 4th parameters
    plot(y_value, y_value, yaxt = "n", xaxt = "n", main = par_name_list[[l]], cex.main = 1.2, xlab = ylab_list[[l]], ylab = "", cex.lab = 1.1, cex = 0, xlim = c(min(unlist(par_list[[l]]) - unlist(par_sd_list[[l]])), max(unlist(par_list[[l]]) + unlist(par_sd_list[[l]])) * 1.0)) # Empty plot for the first parameter: x_value is the numeric value of all classes
    axis(side = 2, at = rev(y_value), label = y_value_name, lwd = 0.3, cex.axis = 0.85, las = 1)
    axis(side = 1, lwd = 0.3, cex.axis = 0.85)
    
    abline(h = y_value, lty = 3, col = "grey")
    #Add vertical grid
    abline(v = seq(min(unlist(par_list[[l]]) - unlist(par_sd_list[[l]])), max(unlist(par_list[[l]]) + unlist(par_sd_list[[l]])) * 1.0, length.out = 10),  lty = 3, col = "grey")
    
    
    for (k in five_ind) {
      
      
      for(z in 1:length(par_list[[l]][[k]])){
        lines(c(c(par_list[[l]][[k]][z] - par_sd_list[[l]][[k]][z]), c(par_list[[l]][[k]][z] + par_sd_list[[l]][[k]][z])), c(21-c_uni_list[[k]][z], 21-c_uni_list[[k]][z]),  lty = 1, lwd = 1.1, col = colores_this[k])
      }
      
      points(par_list[[l]][[k]], 21-c_uni_list[[k]], col = colores_this[k], cex = 1.2, pch = 20,  lty = 2, lwd = .8) # c_uni_list is the numeric value of the unique classes in the subsample
      
      
    }
  }  
  #legend("topright", inset=c(-0.35,0), legend = c("FRA", "GER", "ITA", "SPA", "UK"), col = colores_this[five_ind], pch = rep(20, 5), bty = "n", xpd = NA, cex = 1.4, ncol = 1)
  
  mtext(paste(title), side = 3, line = 1, outer = TRUE, cex = 1.)
  dev.off()
}



## Estimated Parameter for Industry class
fun_levy_par_ind(pdf_name = "Figure_Levy_Para_LP_Ind", title = "Parameters of Levy Distribution: LP by Industry", dat_list = LP_ind_Levy_list_boot, y_value = c(1:19), y_value_name = ind_name_table$ind_names_short , leg_pos = "topleft")

# LP_Change
fun_levy_par_ind(pdf_name = "Figure_Levy_Para_LP_g_Ind", title = "Parameters of Levy Distribution: LP Change by Industry", dat_list = LP_Change_ind_Levy_list_boot,  y_value = c(1:19), y_value_name = ind_name_table$ind_names_short , leg_pos = "topleft")


