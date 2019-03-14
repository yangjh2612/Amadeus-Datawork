# Script to produce plots comparing goodness of Levy stable fit using methods GMM and McCulloch QT. 
#           Uses files saved by Prod_Fitting_Levy.R (namely Levy_list_GMM.Rda and Levy_list_QT.Rda

# 0. Import modules
if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(ggplot2,gridExtra,reshape2)


# 1. Load and prepare data
# Load GMM fits
load("Levy_list_GMM.Rda",verbose=T)

# Combine and use only the first of four elements of each list of fits
GMM = list(LP_year_Levy_list[[1]], LP_g_year_Levy_list[[1]], Zeta_g_year_Levy_list[[1]], LP_size_Levy_list[[1]], LP_g_size_Levy_list[[1]], 
                                Zeta_g_size_Levy_list[[1]], LP_ind_Levy_list[[1]], LP_g_ind_Levy_list[[1]], Zeta_g_ind_Levy_list[[1]])

# Load McCulloch QT fits. Note that this overwrites the variables loaded for GMM fits.
load("Levy_list_QT.Rda",verbose=T)

# Combine and use only the first of four elements of each list of fits
QT = list(LP_year_Levy_list[[1]], LP_g_year_Levy_list[[1]], Zeta_g_year_Levy_list[[1]], LP_size_Levy_list[[1]], LP_g_size_Levy_list[[1]], 
                                Zeta_g_size_Levy_list[[1]], LP_ind_Levy_list[[1]], LP_g_ind_Levy_list[[1]], Zeta_g_ind_Levy_list[[1]])

# Extract Soofi indices
soofiGMM <- unlist(lapply(GMM, function(z) lapply(z, function(y) lapply(y, function(x) x[[6]]))))
soofiQT <- unlist(lapply(QT, function(z) lapply(z, function(y) lapply(y, function(x) x[[6]]))))


# 2. Create scatter plots by parameter (QT vs GMM and parameter difference vs. Soofi index)
paralist = c("alpha", "beta", "gamma", "delta")     # Parameter names
plist <- list()                                     # List of QT vs. GMM plots
plist2 <- list()                                    # List of parameter difference vs Soofi index plots
for (i in 1:4) {                                    # Execute loop for each parameter
    # Extract parameter values
    seriesGMM <- unlist(lapply(GMM, function(z) lapply(z, function(y) lapply(y, function(x) x[[5]][[i]]))))    # With this excessive number of brackets, this feels a bit like writing perl code.
    seriesQT <- unlist(lapply(QT, function(z) lapply(z, function(y) lapply(y, function(x) x[[5]][[i]]))))
    
    # Compute difference
    diff_series <- abs(seriesGMM - seriesQT)
    
    # Create plotting data frame
    plotdf <- data.frame(seriesGMM, seriesQT, diff_series, soofiQT, soofiGMM)
    
    # Create QT vs. GMM plot
    plist[[i]] <- ggplot(plotdf, aes(seriesGMM, seriesQT)) + geom_point(size=.1,shape=16) +
            xlab(paste(paralist[[i]], "GMM", sep=" ")) + ylab(paste(paralist[[i]], "QT", sep=" "))
    
    # Create parameter difference vs. QT Soofi index plot
    plist2[[i]] <- ggplot(plotdf, aes(diff_series, soofiQT)) + geom_point(size=.1,shape=16) + 
            xlab(paste("Difference", paralist[[i]], sep=" ")) + ylab("Soofi index QT")

    # Create parameter difference vs. GMM Soofi index plot
    plist2[[4+i]] <- ggplot(plotdf, aes(diff_series, soofiGMM)) + geom_point(size=.1,shape=16) +
            xlab(paste("Difference", paralist[[i]], sep=" ")) + ylab("Soofi index GMM")    
}

# Save plots to pdf
ggsave("QT_GMM_fit_correspondence.pdf", do.call(grid.arrange,plist))                                                    # QT vs GMM scatter plots
ggsave("QT_GMM_fit_soofi_comparison.pdf", do.call(grid.arrange,c(plist2,ncol=4,respect=TRUE)), width = 10, height = 5)  # parameter difference vs. Soofi index scatter plots


# 3. Create histograms plots of Soofi indices by fitting method (by type of series fitted (separated because sample size is different) and again for all fits).
# 3.1 By type of series
series_list <- c("by years", "by size", "by industry")  # Series names
plist <- list()                                         # List of by series histogram plots
for (i in 1:3) {                                        # Execute loop for each series
    # The lists QT and GMM are arranged such that the first 3 elements are by-year fits, the next 3 elements are by size fits, 
    #          the next 3 elements are by-industry fits (or equivalently more elements if the number of variables is changed).
    # Find first element index for series in QT, GMM lists
    start = 1 + round(length(GMM)/3) * (i-1)
    
    # Extract current series' elements
    GMMs <- GMM[seq(start, start + round(length(GMM)/3), 1)]
    QTs <- QT[seq(start, start + round(length(QT)/3), 1)]

    # Extract Soofi index values
    soofiGMM <- unlist(lapply(GMMs, function(z) lapply(z, function(y) lapply(y, function(x) x[[6]]))))
    soofiQT <- unlist(lapply(QTs, function(z) lapply(z, function(y) lapply(y, function(x) x[[6]]))))
    
    # Create plotting data frame
    plotdf <- data.frame(soofiQT, soofiGMM)
    names(plotdf) <- c("QT", "GMM")     # Column names are used for the legend, therefore revised to understandable strings.
    plotdf_melted <- melt(plotdf)       # Prepare df for histogram: one column for type (QT or GMM), one column for values
    
    # Create histogram plots
    plist[[i]] <- ggplot(plotdf_melted, aes(x=value, fill=variable)) + geom_density(alpha=0.4) +
                    scale_fill_manual(values=c("#9999FF", "#671100"))  + xlim(80, 100) + ylim(0, .5) +
                    xlab(paste("Soofi index", series_list[[i]], sep=" ")) + ylab("Density") + guides(fill=guide_legend(title=""))

    if (i == 1) {   # Only the first panel has a legend, therefore conditional.
        plist[[i]] = plist[[i]] + theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                                                text = element_text(size=10), legend.position = c(0.2, 0.8))
    } else {
        plist[[i]] = plist[[i]] + theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                                                text = element_text(size=10), legend.position = "none")
    }
}
# Save plot to pdf
ggsave("QT_GMM_soofi_density_by_series.pdf", do.call(grid.arrange,c(plist,ncol=3,respect=TRUE)), width = 9, height = 3)


# 3.2 Histogram plot of soofi indices for all fits.
# Clear plot list
plist <- list()
# Extract Soofi indices
soofiGMM <- unlist(lapply(GMM, function(z) lapply(z, function(y) lapply(y, function(x) x[[6]]))))
soofiQT <- unlist(lapply(QT, function(z) lapply(z, function(y) lapply(y, function(x) x[[6]]))))

# Create plotting data frame
plotdf <- data.frame(soofiQT, soofiGMM)
names(plotdf) <- c("QT", "GMM")     # Column names are used for the legend, therefore revised to understandable strings.
plotdf_melted <- melt(plotdf)       # Prepare df for histogram: one column for type (QT or GMM), one column for values

# Create histogram plot
plist[[1]] <- ggplot(plotdf_melted, aes(x=value, fill=variable)) + geom_density(alpha=0.4) +
        scale_fill_manual(values=c("#9999FF", "#671100")) + xlim(80, 100) +
        theme(legend.position = c(0.2, 0.8), panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                text = element_text(size=20)) +
        xlab("Soofi index") + ylab("Density") + guides(fill=guide_legend(title=""))

# Save plot to pdf
ggsave("QT_GMM_soofi_density_all.pdf", do.call(grid.arrange,plist))
