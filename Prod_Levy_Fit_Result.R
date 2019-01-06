##################  Important: Run this file after Prod_Fitting_Levy.R ################## 

############ 0. Basic Set up ############ 
## loading of required libraries
library(colorspace)
library(RColorBrewer)
library(msir)

load("Year_Levy_list_boot.Rda")
load("Size_Levy_list_boot.Rda")
load("Industry_Levy_list_boot.Rda")


############ 1. Plot the distribution with the Levy fitted lines ############ 
##Basically the same plot function but augumented with the fitter line from the Levy distribution. For detailed explanations, see the function detail of "fun_plot_marginal" in Outline.R" script. 

## 1.1. function 
fun_plot_levy_fit <- function(pdf_name, title, dat_list, c_names, x_lab, n_col, cex_leg) {

  # take_this <- which(unlist(lapply(dat_list[[1]], function(x) length(is.na(x)))) > 1)
  # country_name_c <- country_names[take_this]

  five_ind <- which(country_names %in% country_names_five)


  pdf(paste(pdf_name, ".pdf", sep = ""), height = 2.7, width = 10)
  par(mfrow = c(1, 5), mar = c(3, 2.5, 1, 1), mgp = c(1.5, .3, 0), tck = -.01, oma = c(0, 0, 4, 0))


  for (k in five_ind) {
    print(k)
    con_temp <- dat_list[[1]][[k]]

    c_uni <- dat_list[[2]][[k]]

    levy_soofi <- round(unlist(lapply(con_temp, function(x) x[6])), 0)


    y_max <- max(unlist(lapply(con_temp, function(x) max(x[[3]], x[[4]]))))
    y_min <- min(unlist(lapply(con_temp, function(x) min(x[[3]][x[[3]] > 0], x[[4]][x[[4]] > 0]))))

    plot(con_temp[[1]]$data_mid, con_temp[[1]]$data_p, cex = 0, log = "y", yaxt = "n", xaxt = "n", cex.main = 1.2, xlab = x_lab, ylab = "Log-Density", main = country_names[k], ylim = c(y_min, y_max))
    axis(side = 1, lwd = 0.3, cex.axis = 0.9)
    axis(side = 2, lwd = 0.3, cex.axis = .9)

    color_ind <- c(brewer.pal(n = 8, name = "Dark2"), brewer.pal(n = 8, name = "Set3"), "grey", "blue", "green", "red")[1:length(c_names)]


    c_ind_all <- c()

    for (y in 1:length(con_temp)) {
      c_ind <- which(c_names %in% dat_list[[2]][[k]][y])
      points(con_temp[[y]]$data_mid, con_temp[[y]]$data_p, pch = y, cex = 0.35, col = color_ind[c_ind])

      lines(con_temp[[y]]$data_mid, con_temp[[y]]$levy_q, col = color_ind[c_ind], lwd = 1., lty = 1) # Levy fit

      c_ind_all[y] <- c_ind
    }


    leg <- paste(dat_list[[4]][[k]], "(", levy_soofi, ")", sep = "")

    legend("topright", legend = leg, pch = c_ind_all, col = color_ind[c_ind_all], bty = "n", xpd = NA, cex = cex_leg, ncol = n_col)
  }

  mtext(paste(title), side = 3, line = 1, outer = TRUE, cex = 1.)
  dev.off()
}


## 1.2. plot

## Year class
# LP conditional on year (year class)
fun_plot_levy_fit(pdf_name = "Figure_LP_Year_Levy_Fit", title = "Labor Productivity by Year with Levy Fit", dat_list = LP_year_Levy_list, c_names = year_names, x_lab = "LP", n_col = 2, cex_leg = 0.75)

# LP_change conditional on year
fun_plot_levy_fit(pdf_name = "Figure_LP_g_Year_Levy_Fit", title = "Labor Productivity Growth by Year with Levy Fit", dat_list = LP_g_year_Levy_list, c_names = year_names, x_lab = "LP_growth", n_col = 1, cex_leg = 0.75)

# Zeta  conditional on year
fun_plot_levy_fit(pdf_name = "Figure_TFP_g_Year_Levy_Fit", title = "TFP Growth by Year with Levy Fit", dat_list = Zeta_g_year_Levy_list, c_names = year_names, x_lab = "TFP_growth", n_col = 1, cex_leg = 0.75)


## Size class
# LP conditional on size 
fun_plot_levy_fit(pdf_name = "Figure_LP_Size_Levy_Fit", title = "Labor Productivity by Size with Levy Fit", dat_list = LP_size_Levy_list, c_names = size_names, x_lab = "LP", n_col = 1, cex_leg = 0.75)

# LP_change conditional on size 
fun_plot_levy_fit(pdf_name = "Figure_LP_g_Size_Levy_Fit", title = "Labor Productivity Growth by Size with Levy Fit", dat_list = LP_g_size_Levy_list, c_names = size_names, x_lab = "LP_growth", n_col = 1, cex_leg = 0.75)

# Zeta  conditional on size
fun_plot_levy_fit(pdf_name = "Figure_TFP_g_Size_Levy_Fit", title = "TFP Growth by Size with Levy Fit", dat_list = Zeta_g_size_Levy_list, c_names = size_names, x_lab = "TFP_growth", n_col = 1, cex_leg = 0.75)


## Industry class
# LP conditional on sector
fun_plot_levy_fit(pdf_name = "Figure_LP_Ind_Levy_Fit", title = "Labor Productivity by Industry with Levy Fit", dat_list = LP_ind_Levy_list, c_names = ind_name_table$ind_names, x_lab = "LP", n_col = 3, cex_leg = 0.65)

# LP_change conditional on sector 
fun_plot_levy_fit(pdf_name = "Figure_LP_g_Ind_Levy_Fit", title = "Labor Productivity Growth by Industry with Levy Fit", dat_list = LP_g_ind_Levy_list, c_names = ind_name_table$ind_names, x_lab = "LP_growth", n_col = 2, cex_leg = 0.65)

# Zeta conditional on sector 
fun_plot_levy_fit(pdf_name = "Figure_TFP_g_Ind_Levy_Fit", title = "TFP Growth by Industry with Levy Fit", dat_list = Zeta_g_ind_Levy_list, c_names = ind_name_table$ind_names, x_lab = "TFP_growth", n_col = 2, cex_leg = 0.65)


############ 2. Plots for Estimated parameters ############ 
## I separate the level variables from the growth variables to put the \gamma and \delta variables in log scale. I could include the conditional statement to achieve this but I do not want to make the plotting function too complicated. The only difference between the level and growth plotting function is that the last two parameters are in log scale in the level plotting function.

### 2.1. Growth variables
## 2.1.1 function (for five countries )

fun_levy_par <- function(pdf_name, title, dat_list, x_value, x_value_name, x_lab, leg_pos) {  # this function has 5 arguments. 1) the name of the pdf file, 2) the title of the figure, 3) the data file that is generated from the fun_fit_levy function in "Prod_Fitting_Levy.R" script, 4) variable number: 1:Size, 2: Industry, 5) variable index: either "size" or "ind"
  colores_this <- c(brewer.pal(n = 8, name = "Dark2"), brewer.pal(n = 7, name = "Set1"), brewer.pal(n = 7, name = "Set3"))

  five_ind <- which(country_names %in% country_names_five) # index for the top five countries


  c_uni_list <- list() 
  levy_par <- list()
  levy_par_sd <- list()
  for (k in five_ind) {
    c_uni_list[[k]] <- dat_list[[3]][[k]] # the numeric value of the unique class
    levy_par[[k]] <- lapply(dat_list[[1]][[k]], function(x) x$levy_para) # the corresponding levy parameters for each class
    levy_par_sd[[k]] <- lapply(dat_list[[1]][[k]], function(x) apply(x$est_levy_qt_std_error$t, 2, sd)) # the corresponding standard deviation of levy parameters for each class
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
  pdf(paste(pdf_name, ".pdf", sep = ""), height = 2.2, width = 9.5)
  par(mfrow = c(1, 4), mar = c(3, 2.5, 1, 1), mgp = c(1.5, .3, 0), tck = -.01, oma = c(0, 0, 4, 0))

  l <- 1 # first parameter
  plot(x_value, x_value, yaxt = "n", xaxt = "n", main = par_name_list[[l]], cex.main = 1., xlab = x_lab, ylab = ylab_list[[l]], cex = 0, ylim = c(min(unlist(par_list[[l]]) - unlist(par_sd_list[[l]])), max(unlist(par_list[[l]]) + unlist(par_sd_list[[l]]))*1.0 )) #Empty plot for the first parameter: x_value is the numeric value of all classes


  axis(side = 1, at = x_value, label = x_value_name, lwd = 0.3, cex.axis = 0.55)
  axis(side = 2, lwd = 0.3, cex.axis = 0.6)


  for (k in five_ind) {
    points(c_uni_list[[k]], par_list[[l]][[k]], col = colores_this[k], cex = 0.6, pch = 20, type = "b", lty = 1, lwd = 1.2) # c_uni_list is the numeric value of the unique classes in the subsample
    
    polygon.x <- c(c_uni_list[[k]], rev(c_uni_list[[k]]))
    polygon.y <- c(c(par_list[[l]][[k]] - par_sd_list[[l]][[k]]), rev(c(par_list[[l]][[k]] + par_sd_list[[l]][[k]])))
    
    polygon(x=polygon.x, y=polygon.y, col = adjustcolor(colores_this[k],  alpha.f=0.4), border = NA)
    
  }

  legend(leg_pos, legend = c("FRA", "GER", "ITA", "SPA", "UK"), col = colores_this[five_ind], lty = rep(1, 5), ncol = 2, cex = 0.6, bty = "n")


  for (l in 2:4) { # 2nd to 4th parameters
    plot(x_value, x_value, yaxt = "n", xaxt = "n", main = par_name_list[[l]], cex.main = 1., xlab = x_lab, ylab = ylab_list[[l]], cex = 0, ylim = c(min(unlist(par_list[[l]]) - unlist(par_sd_list[[l]])), max(unlist(par_list[[l]]) + unlist(par_sd_list[[l]])) )) #Empty plot for the first parameter: x_value is the numeric value of all classes
    
    axis(side = 1, at = x_value, label = x_value_name, lwd = 0.3, cex.axis = 0.55)
    axis(side = 2, lwd = 0.3, cex.axis = 0.6)
    
    for (k in five_ind) {
      points(c_uni_list[[k]], par_list[[l]][[k]], col = colores_this[k], cex = 0.6, pch = 20, type = "b", lty = 1, lwd = 1.2) # c_uni_list is the numeric value of the unique classes in the subsample
      
      # error bars
      polygon.x <- c(c_uni_list[[k]], rev(c_uni_list[[k]]))
      polygon.y <- c(c(par_list[[l]][[k]] - par_sd_list[[l]][[k]]), rev(c(par_list[[l]][[k]] + par_sd_list[[l]][[k]])))
      
      polygon(x=polygon.x, y=polygon.y, col = adjustcolor(colores_this[k],  alpha.f=0.4), border = NA)
    }
  }


  mtext(paste(title), side = 3, line = 1, outer = TRUE, cex = 1.)
  dev.off()
}


## 2.1.2 plot

## Estimated Parameter for Year class
#LP_Change
fun_levy_par(pdf_name = "Figure_Levy_Para_LP_g_Year", title = "Parameters of Levy Distribution: LP Change by Year", dat_list = LP_g_year_Levy_list_boot, x_value = 2:10, x_value_name = year_names[-c(1)], x_lab = "Year", leg_pos = "bottomright")

#Zeta
fun_levy_par(pdf_name = "Figure_Levy_Para_TFP_g_Year", title = "Parameters of Levy Distribution: TFP Change by Year", dat_list = Zeta_g_year_Levy_list_boot, x_value = 2:10, x_value_name = year_names[-c(1)], x_lab = "Year", leg_pos = "bottomright")


## Estimated Parameter for Size class
#LP_Change
fun_levy_par(pdf_name = "Figure_Levy_Para_TFP_g_Size", title = "Parameters of Levy Distribution: TFP Change by Size", dat_list = Zeta_g_size_Levy_list_boot, x_value = c(1:4), x_value_name  = size_names, x_lab = "Size", leg_pos = "topright")

#Zeta
fun_levy_par(pdf_name = "Figure_Levy_Para_LP_g_Size", title = "Parameters of Levy Distribution: LP Change by Size", dat_list = LP_g_size_Levy_list_boot, x_value = c(1:4), x_value_name  = size_names, x_lab = "Size", leg_pos = "topright")


## Estimated Parameter for Industry class
#LP_Change
fun_levy_par(pdf_name = "Figure_Levy_Para_LP_g_Ind", title = "Parameters of Levy Distribution: LP Change by Industry", dat_list = LP_g_ind_Levy_list_boot, x_value = c(1:20), x_value_name  = ind_name_table$ind_names_alphabet, x_lab = "Industry", leg_pos = "topright")

#Zeta
fun_levy_par(pdf_name = "Figure_Levy_Para_TFP_g_Ind", title = "Parameters of Levy Distribution: TFP Change by Industry", dat_list = Zeta_g_ind_Levy_list_boot, x_value = c(1:20), x_value_name  = ind_name_table$ind_names_alphabet, x_lab = "Industry", leg_pos = "topright")

##
fun_levy_par(pdf_name = "Figure_Levy_Para_LP_Year", title = "Parameters of Levy Distribution: LP by Year", dat_list = LP_year_Levy_list_boot, x_value = 1:10, x_value_name = year_names, x_lab = "Year", leg_pos = "topleft")

## Estimated Parameter for Size class
#LP
fun_levy_par(pdf_name = "Figure_Levy_Para_LP_Size", title = "Parameters of Levy Distribution: LP by Size", dat_list = LP_size_Levy_list_boot, x_value = c(1:4), x_value_name = size_names, x_lab = "Size", leg_pos = "topright")

## Estimated Parameter for Industry class
#LP
fun_levy_par(pdf_name = "Figure_Levy_Para_LP_Ind", title = "Parameters of Levy Distribution: LP by Industry", dat_list = LP_ind_Levy_list_boot, x_value = c(1:20), x_value_name = ind_name_table$ind_names_alphabet, x_lab = "Industry", leg_pos = "bottomleft")



############ 3. Summary Statistics  ############



