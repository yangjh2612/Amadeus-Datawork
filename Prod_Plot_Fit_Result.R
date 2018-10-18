## ------------------------------------------------------------------------
setwd("~/Desktop/Cleaned Rda/Productivity")

load("All_list_for_prod.Rda")

load("LP_change_list.Rda")
load("CP_change_list.Rda")
load("Zeta_list.Rda")



## ------------------------------------------------------------------------

library(colorspace)
colores_10 <- rainbow_hcl(10, end=300)
year_names <- c(2007:2015)



## ------------------------------------------------------------------------
# density with the fitted lines

fun_plot_fit <- function(dat_list, var_name){
 
  take_this <- which(unlist(lapply(dat_list, function(x) sum(is.na(x)))) < 10)

country_name_c <- country_names[take_this]
dat_list_c <- dat_list[take_this]


 pdf(paste(var_name,"_fit.pdf", sep = ""), height = 15, width = 12)
  par(mfrow=c(6,5), mar=c(3, 2.5, 1, 1), mgp=c(1.5,.3,0), tck=-.01, oma=c(0,0,4,0))

  
  for(k in 1:length(dat_list_c)){
    con_temp <- dat_list_c[[k]]

year_name_c <- year_names[!is.na(con_temp)]
con_temp_c <- con_temp[!is.na(con_temp)]

levy_soofi <- round(unlist(lapply(con_temp, function(x) x[9])),0)
sub_soofi <- round(unlist(lapply(con_temp, function(x) x[10])),0)

levy_soofi <- levy_soofi[!is.na(con_temp)]
sub_soofi <- sub_soofi[!is.na(con_temp)]  
    
y_max <- max(unlist(lapply(con_temp_c, function(x) max(x[[3]],x[[4]]))))
y_min <- min(unlist(lapply(con_temp_c, function(x) min(x[[3]][x[[3]] > 0],x[[4]][x[[4]] >0]))))

plot(con_temp_c[[1]]$data_mid, con_temp_c[[1]]$data_p, col = colores_10[1], yaxt = "n", xaxt = "n",main = paste(country_name_c[k]), log = "y", cex.main = 1.2, xlab = "Growth", ylab = "Density", lty="blank", pch = 20, cex = .0, ylim = c(y_min,y_max))

axis(side = 1, lwd = 0.3, cex.axis=0.6)
axis(side = 2, lwd = 0.3, cex.axis=0.6)

for(y in 1:length(con_temp_c)){
  points(con_temp_c[[y]]$data_mid, con_temp_c[[y]]$data_p, pch = 20, cex = 0.5, col = colores_10[y])  
  lines(con_temp_c[[y]]$data_mid, con_temp_c[[y]]$levy_q, col = colores_10[y], lwd = 1.2, lty = 1)
}


leg <- paste(year_name_c, "(", levy_soofi, ")", sep = "")

legend("topright", legend = leg, pch = rep(20, length(year_name_c)), col = colores_10,  bty = "n", cex = 0.8)

}


mtext(paste(var_name, ": Levy Fit"), side=3, line=1, outer=TRUE,cex= 2.)
  dev.off()

   
}


## ------------------------------------------------------------------------
setwd("~/Desktop/Cleaned Rda/Productivity/Figures")
fun_plot_fit(dat_list = LP_change_list, var_name = "LP_change")
fun_plot_fit(dat_list = CP_change_list, var_name = "CP_change")
fun_plot_fit(dat_list = Zeta_list, var_name = "Zeta")

## ------------------------------------------------------------------------

# time series plot for the recovered levy parameters
fun_levy_par <- function(dat_list, var_name){
 
take_this <- which(unlist(lapply(dat_list, function(x) sum(is.na(x)))) < 10)

country_name_c <- country_names[take_this]
dat_list_c <- dat_list[take_this]

year_name_c <- list(); levy_par <- list()
for(k in 1:length(dat_list_c)){
  names(dat_list_c[[k]]) <- year_names[1:length(dat_list_c[[k]])] 
  
  con_temp <- dat_list_c[[k]]
  con_temp_c <- con_temp[!is.na(con_temp)]
   
  year_name_c[[k]] <- names(con_temp_c)
  levy_par[[k]] <- lapply(con_temp_c, function(x) x[6])

    # stupid denmark 
}


 par_a <- list();   par_b <- list();   par_g <- list();   par_d <- list(); 
for(k in 1:length(dat_list_c)){
  
par_a[[k]] <-  unlist(lapply(levy_par[[k]], function(x) x[[1]][1]))
par_b[[k]] <-  unlist(lapply(levy_par[[k]], function(x) x[[1]][2]))
par_g[[k]] <-  unlist(lapply(levy_par[[k]], function(x) x[[1]][3]))
par_d[[k]] <-  unlist(lapply(levy_par[[k]], function(x) x[[1]][4]))

}
 
a_f <- data.frame(y = as.numeric(unlist(year_name_c)), a = unlist(par_a)) 
lw_par_a <- loess.sd(a_f, nsigma = 1.)

b_f <- data.frame(y = as.numeric(unlist(year_name_c)), b = unlist(par_b)) 
lw_par_b <- loess.sd(b_f, nsigma = 1.)

g_f <- data.frame(y = as.numeric(unlist(year_name_c)), g = unlist(par_g)) 
lw_par_g <- loess.sd(g_f, nsigma = 1.)

d_f <- data.frame(y = as.numeric(unlist(year_name_c)), d = unlist(par_d)) 
lw_par_d <- loess.sd(d_f, nsigma = 1.)


colores_this <- rainbow_hcl(length(dat_list_c), start = 20, end=300)

pdf(paste("Levy_Par_Trend_",var_name,".pdf",sep = ""), height = 7, width = 8)
par(mfrow=c(2,2), mar=c(3, 2.5, 1, 1), mgp=c(1.5,.3,0), tck=-.01, oma=c(0,0,4,0))

 
par_list <- list(par_a, par_b, par_g, par_d)
par_name_list <- list(expression(paste(alpha, ": tail")),
                      expression(paste(beta, ": skewness")),
                      expression(paste(gamma, ": scale")),
                      expression(paste(delta, ": location")))
ylab_list <- list(expression(paste(alpha)), expression(paste(beta)), expression(paste(gamma)), expression(paste(delta)))

lw_list <- list(lw_par_a, lw_par_b, lw_par_g, lw_par_d)

for(l in 1:4){
plot(2007:2015, rep(1,9),  col = colores_this[1], yaxt = "n", xaxt = "n", main = par_name_list[[l]],  cex.main = 1.2, xlab = "Year", ylab = ylab_list[[l]], cex = 0, ylim = c(min(unlist(par_list[[l]])), max(unlist(par_list[[l]]))))
  axis(side = 1, lwd = 0.3, cex.axis=0.6)
  axis(side = 2, lwd = 0.3, cex.axis=0.6)

for(k in 1:length(par_list[[l]])){
 points(year_name_c[[k]], par_list[[l]][[k]] ,  col = colores_this[k], cex = 0.6, pch = 20, type = "o", lty = 2, lwd = 0.7)
      
}
  

lines(lw_list[[l]]$x, lw_list[[l]]$y, lty=1, lwd = 1.5)
#lines(lw_list[[l]]$x, lw_list[[l]]$upper, lty=2, lwd = 1.5)
#lines(lw_list[[l]]$x, lw_list[[l]]$lower, lty=2, lwd = 1.5)

}

mtext(paste("Time Trend of Levy Parameters: ",var_name, sep = ""), side=3, line=1, outer=TRUE,cex=1.3)
dev.off()

}


## ------------------------------------------------------------------------
setwd("~/Desktop/Cleaned Rda/Productivity/Figures")
fun_levy_par(dat_list = LP_change_list, var_name = "LP_change")
fun_levy_par(dat_list = CP_change_list, var_name = "CP_change")
fun_levy_par(dat_list = Zeta_list, var_name = "Zeta")


## ------------------------------------------------------------------------
library(knitr)
purl("Prod_Fitting.Rmd")  

