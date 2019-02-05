# This file runs the simulation for Interquartile Ranges

rm(list = ls()) # clear environment
gc() # garbage collection

pacman::p_load(tidyverse,xtable) # load packages

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