rm(list = ls()) # clear environment
gc() # garbage collection

pacman::p_load(stabledist,tidyverse,foreach,doParallel,reshape,xtable) # load packages

# n <- 1000000
# b <- n/10
# 
# levy <- rstable(n, 1.7, 0, gamma = 1, delta = 0, pm = 0)
# levy_cut <- levy[which(levy < (mean(levy) + 3 * sd(levy)) & levy > (mean(levy) - 3 * sd(levy)))]
# norm <- rnorm(n, mean = mean(levy), sd = sd(levy))
# norm_cut <- rnorm(n, mean = mean(levy_cut), sd = sd(levy_cut))
# 
# par(mfrow=c(2,2))
# hist(levy, breaks = b)
# hist(norm, breaks = b)
# hist(levy_cut, breaks = b)
# hist(norm_cut, breaks = b)
# mtext(paste("n = ", n, sep = ''), side = 3, line = -25, outer = TRUE)


# Using Variance
if(!file.exists('C:/Users/JulianW/Documents/Work/Productivity Dispersion/testsd.Rda')){ # I run the simulations once, then save the result, because it takes too long to run
  alpha <- 1.7 # Levy parameters
  beta <- 0
  gamma <- 1
  delta <- 0
  n <- 8 # how many samples sizes do you want to consider? 10^n
  runs <- iter(1:1000) # how many interations of each sample size
  
  cl <- makePSOCKcluster(detectCores()) # register parallel backend
  registerDoParallel(cl)
  
  start_time <- Sys.time() # time the code
  testsd <- foreach(i = runs, .combine = rbind, .packages = 'stabledist') %dopar% { # foreach loop the returns one set of simulated samples, of size 1 to n
    c <- as.list(seq(1,n,1)) # vector sample sizes
    c <- lapply(c, function(x) {
      sd(rstable(10^x, alpha, beta, gamma, delta, pm = 0)) # standard deviation of the sample of draws from the levy
    })
    unlist(c) # return the vector, which foreach combines into a df
  }
  end_time <- Sys.time()
  stopCluster(cl) # close the backend
  time <- end_time - start_time # 1h48m
  
  save(testsd, file = 'C:/Users/JulianW/Documents/Work/Productivity Dispersion/testsd.Rda') # save results
  rm(alpha, beta, gamma, delta, i, start_time, end_time, time)
} else {
  load('C:/Users/JulianW/Documents/Work/Productivity Dispersion/testsd.Rda') # if the results exist already, just load them to save time
}

sd_sum <- as.data.frame(testsd) # create DF
sd_sum <- melt(sd_sum) # reshape into long format
class(sd_sum$variable) <- 'character' # turn into character
sd_sum <- sd_sum %>% # take the relevant statistics
  group_by(variable) %>%
  mutate(mean = mean(value),
         median = median(value),
         topquint = quantile(value, 0.95),
         botquint = quantile(value, 0.05))

sd_sum_srt <- sd_sum %>% # keep the statistics we are interested in
  group_by(variable) %>%
  slice(1) %>%
  ungroup() %>%
  select(-value)
  
# Plot the standard deviations
pdf('C:/Users/JulianW/Documents/Work/Productivity Dispersion/sample_std_dev_alpha_17.pdf', height = 6, width = 8)
par(mfcol = c(1,1))

plot(range(sd_sum_srt$variable), 
     range(sd_sum_srt$topquint, sd_sum_srt$botquint), 
     xlab = 'Sample Size, log base 10 scale', 
     ylab = 'Sample Standard Deviation',
     cex.axis = 1.3,
     cex.lab = 1.3,
     type = 'n')
polygon(c(sd_sum_srt$variable, rev(sd_sum_srt$variable)), 
        c(sd_sum_srt$topquint, rev(sd_sum_srt$botquint)),
        col = "grey90", border = NA)
lines(sd_sum_srt$variable,
      sd_sum_srt$mean,
      lty = 1,
      col = 'red',
      lwd = 2)
lines(sd_sum_srt$variable,
      sd_sum_srt$median,
      lty = 2,
      col = 'blue',
      lwd = 2)
legend('topleft',
       c('Mean', 'Median'),
       bty = 'n',
       cex = 1.3, 
       col = c('red','blue'), 
       lty = c(1, 2), lwd = 2)

dev.off()

# IQR as measure of dispersion

iqr_tab <- as.data.frame(rbind(c(0.95,0.05,1.7,0,1,0), # create the parameters for the relevant Levy distributions
                               c(0.95,0.05,1.5,0,1,0),
                               c(0.9,0.1,1.7,0,1,0),
                               c(0.9,0.1,1.5,0,1,0),
                               c(0.75,0.25,1.7,0,1,0),
                               c(0.75,0.25,1.5,0,1,0),
                               c(0.95,0.05,1.5,0,1,0),
                               c(0.95,0.05,1.5,0.5,1,0),
                               c(0.9,0.1,1.5,0,1,0),
                               c(0.9,0.1,1.5,0.5,1,0),
                               c(0.75,0.25,1.5,0,1,0),
                               c(0.75,0.25,1.5,0.5,1,0)))
colnames(iqr_tab) <- c('Top', 'Bottom', 'a', 'b', 'g', 'd')

iqr_tab <- iqr_tab %>%
  rowwise() %>%
  mutate(IQR = qstable(Top, a, b, g, d) - qstable(Bottom, a, b, g, d)) %>% # find the relevant IQRs for the given parameters
  ungroup() %>%
  mutate(pc_IQR = ifelse((row_number() %% 2) == 0, (IQR - lag(IQR,1))/lag(IQR,1), NA),
         pc_IQR = round(100*pc_IQR, digit = 2)) # express as %

print(xtable(iqr_tab, caption = 'Interquartile Range as a Measure of Dispersion'), include.rownames = FALSE) # paste table into latex
