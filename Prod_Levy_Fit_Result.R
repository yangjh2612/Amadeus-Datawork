##################  Important: Run this file after Prod_Fitting_Levy.R ################## 

############ 0. Basic Set up ############ 
## loading of required libraries
library(colorspace)
library(msir)

############ 1. Plot the distribution with the Levy fitted lines ############ 
##Basically the same plot function but augumented with the fitter line from the Levy distribution. For detailed explanations, see the function detail of "fun_plot_marginal" in Outline.R" script. 

## 1.1. function 
fun_plot_levy_fit <- function(pdf_name, title, dat_list, c_names, x_lab, n_col) {

  # take_this <- which(unlist(lapply(dat_list[[1]], function(x) length(is.na(x)))) > 1)
  # country_name_c <- country_names[take_this]

  five_ind <- which(country_names %in% country_names_five)


  pdf(paste(pdf_name, ".pdf", sep = ""), height = 2.7, width = 10)
  par(mfrow = c(1, 5), mar = c(3, 2.5, 1, 1), mgp = c(1.5, .3, 0), tck = -.01, oma = c(0, 0, 4, 0))


  for (k in five_ind) {
    print(k)
    con_temp <- dat_list[[1]][[k]]

    c_uni <- dat_list[[2]][[k]]

    c_uni_name <- c()
    c_uni_num <- c()
    if (is.numeric(c_uni)) {
      c_uni_num <- sort(c_uni)
      c_uni_name <- c_uni_num
    } else {
      for (i in 1:length(c_uni)) {
        c_uni_num[i] <- which(c_names %in% c_uni[i])
      }
      c_uni_num <- sort(c_uni_num)
      c_uni_name <- c_names[c_uni_num]
    }

    levy_soofi <- round(unlist(lapply(con_temp, function(x) x[6])), 0)


    y_max <- max(unlist(lapply(con_temp, function(x) max(x[[3]], x[[4]]))))
    y_min <- min(unlist(lapply(con_temp, function(x) min(x[[3]][x[[3]] > 0], x[[4]][x[[4]] > 0]))))

    plot(con_temp[[1]]$data_mid, con_temp[[1]]$data_p, cex = 0, log = "y", yaxt = "n", xaxt = "n", cex.main = 1.2, xlab = x_lab, ylab = "Log-Density", main = country_names[k], ylim = c(y_min, y_max))
    axis(side = 1, lwd = 0.3, cex.axis = 0.9)
    axis(side = 2, lwd = 0.3, cex.axis = .9)

    color_ind <- c(brewer.pal(n = 8, name = "Dark2"), brewer.pal(n = 8, name = "Set3"), "grey", "blue", "green", "red")[1:length(c_names)]


    c_ind_all <- c()

    for (y in 1:length(con_temp)) {
      c_ind <- which(c_names %in% c_uni_name[y])
      points(con_temp[[y]]$data_mid, con_temp[[y]]$data_p, pch = y, cex = 0.35, col = color_ind[c_ind])

      lines(con_temp[[y]]$data_mid, con_temp[[y]]$levy_q, col = color_ind[c_ind], lwd = 1., lty = 1) # Levy fit

      c_ind_all[y] <- c_ind
    }


    leg <- paste(c_uni_num, "(", levy_soofi, ")", sep = "")

    legend("topright", legend = leg, pch = c_ind_all, col = color_ind[c_ind_all], bty = "n", xpd = NA, cex = .75, ncol = n_col)
  }

  mtext(paste(title), side = 3, line = 1, outer = TRUE, cex = 1.)
  dev.off()
}


## 1.2. plot

## Year class
# LP conditional on year (year class)
fun_plot_levy_fit(pdf_name = "Figure_LP_Year_Levy_Fit", title = "Labor Productivity by Year with Levy Fit", dat_list = LP_year_Levy_list, c_names = year_names, x_lab = "LP", n_col = 2)

# LP_change conditional on year
fun_plot_levy_fit(pdf_name = "Figure_LP_g_Year_Levy_Fit", title = "Labor Productivity Growth by Year with Levy Fit", dat_list = LP_g_year_Levy_list, c_names = year_names, x_lab = "LP_growth", n_col = 1)

# Zeta  conditional on year
fun_plot_levy_fit(pdf_name = "Figure_TFP_g_Year_Levy_Fit", title = "TFP Growth by Year with Levy Fit", dat_list = Zeta_g_year_Levy_list, c_names = year_names, x_lab = "TFP_growth", n_col = 1)


## Size class
# LP conditional on size 
fun_plot_levy_fit(pdf_name = "Figure_LP_Size_Levy_Fit", title = "Labor Productivity by Size with Levy Fit", dat_list = LP_size_Levy_list, c_names = size_names, x_lab = "LP", n_col = 1)

# LP_change conditional on size 
fun_plot_levy_fit(pdf_name = "Figure_LP_g_Size_Levy_Fit", title = "Labor Productivity Growth by Size with Levy Fit", dat_list = LP_g_size_Levy_list, c_names = size_names, x_lab = "LP_growth", n_col = 1)

# Zeta  conditional on size
fun_plot_levy_fit(pdf_name = "Figure_TFP_g_Size_Levy_Fit", title = "TFP Growth by Size with Levy Fit", dat_list = Zeta_g_size_Levy_list, c_names = size_names, x_lab = "TFP_growth", n_col = 1)


## Industry class
# LP conditional on sector
fun_plot_levy_fit(pdf_name = "Figure_LP_Ind_Levy_Fit", title = "Labor Productivity by Industry with Levy Fit", dat_list = LP_ind_Levy_list, c_names = ind_names, x_lab = "LP", n_col = 2)

# LP_change conditional on sector 
fun_plot_levy_fit(pdf_name = "Figure_LP_g_Ind_Levy_Fit", title = "Labor Productivity Growth by Industry with Levy Fit", dat_list = LP_g_ind_Levy_list, c_names = ind_names, x_lab = "LP_growth", n_col = 2)

# Zeta conditional on sector 
fun_plot_levy_fit(pdf_name = "Figure_TFP_g_Ind_Levy_Fit", title = "TFP Growth by Industry with Levy Fit", dat_list = Zeta_g_ind_Levy_list, c_names = ind_names, x_lab = "TFP_growth", n_col = 2)


############ 2. Plots for Estimated parameters ############ 
## I separate the level variables from the growth variables to put the \gamma and \delta variables in log scale. I could include the conditional statement to achieve this but I do not want to make the plotting function too complicated. The only difference between the level and growth plotting function is that the last two parameters are in log scale in the level plotting function.

### 2.1. Growth variables
## 2.1.1 function (for five countries )

fun_levy_par_growth <- function(pdf_name, title, dat_list, x_value_name, x_value, y_value, x_lab) {  # this function has 5 arguments. 1) the name of the pdf file, 2) the title of the figure, 3) the data file that is generated from the fun_fit_levy function in "Prod_Fitting_Levy.R" script, 4) variable number: 1:Size, 2: Industry, 5) variable index: either "size" or "ind"
  colores_this <- c(brewer.pal(n = 8, name = "Dark2"), brewer.pal(n = 7, name = "Set1"), brewer.pal(n = 7, name = "Set3"))

  five_ind <- which(country_names %in% country_names_five) # index for the top five countries


  c_uni_list <- list() 
  levy_par <- list()
  for (k in five_ind) {
    c_uni_list[[k]] <- dat_list[[3]][[k]] # the numeric value of the unique class
    levy_par[[k]] <- lapply(dat_list[[1]][[k]], function(x) x[5]) # the corresponding levy parameters for each class
  }

  par_a <- list()
  par_b <- list()
  par_g <- list()
  par_d <- list()
  for (k in five_ind) {
    par_a[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[[1]][1])) # parameter \alpha
    par_b[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[[1]][2])) # parameter \beta
    par_g[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[[1]][3])) # parameter \gamma
    par_d[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[[1]][4])) # parameter \delta
  }

  # a_f <- data.frame(y = as.numeric(unlist(c_uni_list)), a = unlist(par_a))
  # lw_par_a <- loess.sd(a_f, nsigma = 1.)

  # b_f <- data.frame(y = as.numeric(unlist(c_uni_list)), b = unlist(par_b))
  # lw_par_b <- loess.sd(b_f, nsigma = 1.)

  # g_f <- data.frame(y = as.numeric(unlist(c_uni_list)), g = unlist(par_g))
  # lw_par_g <- loess.sd(g_f, nsigma = 1.)

  # d_f <- data.frame(y = as.numeric(unlist(c_uni_list)), d = unlist(par_d))
  # lw_par_d <- loess.sd(d_f, nsigma = 1.)


  par_list <- list(par_a, par_b, par_g, par_d)
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
  pdf(paste(pdf_name, ".pdf", sep = ""), height = 2., width = 8)
  par(mfrow = c(1, 4), mar = c(3, 2.5, 1, 1), mgp = c(1.5, .3, 0), tck = -.01, oma = c(0, 0, 4, 0))

  l <- 1 # first parameter
  plot(x_value, y_value, yaxt = "n", xaxt = "n", main = par_name_list[[l]], cex.main = 1., xlab = x_lab, ylab = ylab_list[[l]], cex = 0, ylim = c(min(unlist(par_list[[l]])), max(unlist(par_list[[l]])) * (1.05))) #Empty plot for the first parameter: x_value is the numeric value of all classes


  axis(side = 1, at = x_value, label = x_value, lwd = 0.3, cex.axis = 0.6)
  axis(side = 2, lwd = 0.3, cex.axis = 0.6)


  for (k in five_ind) {
    points(c_uni_list[[k]], par_list[[l]][[k]], col = colores_this[k], cex = 0.6, pch = 20, type = "o", lty = 1, lwd = 1.2) # c_uni_list is the numeric value of the unique classes in the subsample
  }

  legend("topleft", legend = c("FRA", "GER", "ITA", "SPA", "UK"), col = colores_this[five_ind], lty = rep(1, 5), horiz = TRUE, cex = 0.45, bty = "n")


  for (l in 2:4) { # 2nd to 4th parameters
    plot(x_value, y_value, yaxt = "n", xaxt = "n", main = par_name_list[[l]], cex.main = 1., xlab = x_lab, ylab = ylab_list[[l]], cex = 0, ylim = c(min(unlist(par_list[[l]])), max(unlist(par_list[[l]]))))


    axis(side = 1, at = x_value, label = x_value, lwd = 0.3, cex.axis = 0.6)
    axis(side = 2, lwd = 0.3, cex.axis = 0.6)

    for (k in five_ind) {
      points(c_uni_list[[k]], par_list[[l]][[k]], col = colores_this[k], cex = 0.6, pch = 20, type = "o", lty = 1, lwd = 1.2)
    }
  }


  mtext(paste(title), side = 3, line = 1, outer = TRUE, cex = 1.)
  dev.off()
}


## 2.1.2 plot

## Estimated Parameter for Year class
#LP_Change
fun_levy_par_growth(pdf_name = "Figure_Levy_Para_LP_g_Year", title = "Parameters of Levy Distribution: LP Change by Year", dat_list = LP_g_year_Levy_list, x_value = c(2007:2015), y_value = rep(1, 9), x_lab = "Year")

#Zeta
fun_levy_par_growth(pdf_name = "Figure_Levy_Para_TFP_g_Year", title = "Parameters of Levy Distribution: TFP Change by Year", dat_list = Zeta_g_year_Levy_list, x_value = c(2007:2015), y_value = rep(1, 9), x_lab = "Year")


## Estimated Parameter for Size class
#LP_Change
fun_levy_par_growth(pdf_name = "Figure_Levy_Para_TFP_g_Size", title = "Parameters of Levy Distribution: TFP Change by Size", dat_list = Zeta_g_size_Levy_list, x_value = c(1:4), y_value = rep(1, 4), x_lab = "Size")

#Zeta
fun_levy_par_growth(pdf_name = "Figure_Levy_Para_LP_g_Size", title = "Parameters of Levy Distribution: LP Change by Size", dat_list = LP_g_size_Levy_list, x_value = c(1:4), y_value = rep(1, 4), x_lab = "Size")


## Estimated Parameter for Industry class
#LP_Change
fun_levy_par_growth(pdf_name = "Figure_Levy_Para_LP_g_Ind", title = "Parameters of Levy Distribution: LP Change by Industry", dat_list = LP_g_ind_Levy_list, x_value = c(1:20), y_value = rep(1, length(ind_names)), x_lab = "Industry")

#Zeta
fun_levy_par_growth(pdf_name = "Figure_Levy_Para_TFP_g_Ind", title = "Parameters of Levy Distribution: TFP Change by Industry", dat_list = Zeta_g_ind_Levy_list, x_value = c(1:20), y_value = rep(1, length(ind_names)), x_lab = "Industry")


### 2.2. Level variables
## 2.2.1 function (for five countries )

fun_levy_par_level <- function(pdf_name, title, dat_list, x_value, y_value, x_lab) {
  colores_this <- c(brewer.pal(n = 8, name = "Dark2"), brewer.pal(n = 7, name = "Set1"), brewer.pal(n = 7, name = "Set3"))

  five_ind <- which(country_names %in% country_names_five)




  c_uni_list <- list()
  levy_par <- list()
  for (k in five_ind) {
    c_uni_list[[k]] <- dat_list[[3]][[k]]
    levy_par[[k]] <- lapply(dat_list[[1]][[k]], function(x) x[5])
  }


  par_a <- list()
  par_b <- list()
  par_g <- list()
  par_d <- list()
  for (k in five_ind) {
    par_a[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[[1]][1]))
    par_b[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[[1]][2]))
    par_g[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[[1]][3]))
    par_d[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[[1]][4]))
  }

  # a_f <- data.frame(y = as.numeric(unlist(c_uni_list)), a = unlist(par_a))
  # lw_par_a <- loess.sd(a_f, nsigma = 1.)

  # b_f <- data.frame(y = as.numeric(unlist(c_uni_list)), b = unlist(par_b))
  # lw_par_b <- loess.sd(b_f, nsigma = 1.)

  # g_f <- data.frame(y = as.numeric(unlist(c_uni_list)), g = unlist(par_g))
  # lw_par_g <- loess.sd(g_f, nsigma = 1.)

  # d_f <- data.frame(y = as.numeric(unlist(c_uni_list)), d = unlist(par_d))
  # lw_par_d <- loess.sd(d_f, nsigma = 1.)


  par_list <- list(par_a, par_b, par_g, par_d)
  par_name_list <- list(
    expression(paste(alpha, ": tail")),
    expression(paste(beta, ": skewness")),
    expression(paste(gamma, ": scale")),
    expression(paste(delta, ": location"))
  )
  ylab_list <- list(expression(paste(alpha)), expression(paste(beta)), expression(paste(gamma)), expression(paste(delta)))

  # lw_list <- list(lw_par_a, lw_par_b, lw_par_g, lw_par_d)


  ###
  pdf(paste(pdf_name, ".pdf", sep = ""), height = 2., width = 8)
  par(mfrow = c(1, 4), mar = c(3, 2.5, 1, 1), mgp = c(1.5, .3, 0), tck = -.01, oma = c(0, 0, 4, 0))

  l <- 1
  plot(x_value, y_value, yaxt = "n", xaxt = "n", main = par_name_list[[l]], cex.main = 1., xlab = x_lab, ylab = ylab_list[[l]], cex = 0, ylim = c(min(unlist(par_list[[l]])), max(unlist(par_list[[l]])) * (1.05)))


  axis(side = 1, at = x_value, label = x_value, lwd = 0.3, cex.axis = 0.6)
  axis(side = 2, lwd = 0.3, cex.axis = 0.6)


  for (k in five_ind) {
    points(c_uni_list[[k]], par_list[[l]][[k]], col = colores_this[k], cex = 0.6, pch = 20, type = "o", lty = 1, lwd = 1.2)
  }

  legend("topleft", legend = c("FRA", "GER", "ITA", "SPA", "UK"), col = colores_this[five_ind], lty = rep(1, 5), horiz = TRUE, cex = 0.45, bty = "n")

  l <- 2
  plot(x_value, y_value, yaxt = "n", xaxt = "n", main = par_name_list[[l]], cex.main = 1., xlab = x_lab, ylab = ylab_list[[l]], cex = 0, ylim = c(min(unlist(par_list[[l]])), max(unlist(par_list[[l]])) * (1.05)))


  axis(side = 1, at = x_value, label = x_value, lwd = 0.3, cex.axis = 0.6)
  axis(side = 2, lwd = 0.3, cex.axis = 0.6)


  for (k in five_ind) {
    points(c_uni_list[[k]], par_list[[l]][[k]], col = colores_this[k], cex = 0.6, pch = 20, type = "o", lty = 1, lwd = 1.2)
  }



  for (l in 3:4) {
    plot(x_value, y_value, yaxt = "n", xaxt = "n", main = par_name_list[[l]], cex.main = 1., xlab = x_lab, ylab = ylab_list[[l]], cex = 0, ylim = c(log(min(unlist(par_list[[l]])[unlist(par_list[[l]]) > 0])), max(log(unlist(par_list[[l]])))))


    axis(side = 1, at = x_value, label = x_value, lwd = 0.3, cex.axis = 0.6)
    axis(side = 2, lwd = 0.3, cex.axis = 0.6)

    for (k in five_ind) {
      points(c_uni_list[[k]], log(par_list[[l]][[k]]), col = colores_this[k], cex = 0.6, pch = 20, type = "o", lty = 1, lwd = 1.2)
    }
  }


  mtext(paste(title), side = 3, line = 1, outer = TRUE, cex = 1.)
  dev.off()
}


## 2.2.2 Plot
## Estimated Parameter for Year class
#LP

fun_levy_par_level(pdf_name = "Figure_Levy_Para_LP_Year", title = "Parameters of Levy Distribution: LP by Year", dat_list = LP_year_Levy_list, x_value = c(2006:2015), y_value = rep(1, 10), x_lab = "Year")

## Estimated Parameter for Size class
#LP
fun_levy_par_level(pdf_name = "Figure_Levy_Para_LP_Size", title = "Parameters of Levy Distribution: LP by Size", dat_list = LP_size_Levy_list, x_value = c(1:4), y_value = rep(1, 4), x_lab = "Size")

## Estimated Parameter for Industry class
#LP
fun_levy_par_level(pdf_name = "Figure_Levy_Para_LP_Ind", title = "Parameters of Levy Distribution: LP by Industry", dat_list = LP_ind_Levy_list, x_value = c(1:20), y_value = rep(1, 20), x_lab = "Industry")

