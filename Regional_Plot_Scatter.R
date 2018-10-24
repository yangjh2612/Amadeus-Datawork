library(ggplot2)
library(gridExtra)

# plotting function
plot_desc_stats_CP_vs_RoC <- function(desc_stats) {
    ggplot(desc_stats, aes(CP_median, RoC_median, colour=Country)) + geom_point(size=1,shape=".") + geom_rug(size=0.1) + theme(legend.title = element_blank(), legend.position="bottom") + facet_wrap(~Year) + coord_fixed(ratio=4) + labs(x="Capital Productivity", y="Return on Total Assets")
    pdf_file_name = paste(paste("Reg_NUTS", nuts_level, sep="_"), "_scatterplots_CP_vs_RoC.pdf", sep="_")
    ggsave(file = pdf_file_name)
}

# main entry point
nuts_level <- 2

# load data
stats_file_name <- paste(paste("Reg_NUTS", nuts_level, sep="_"), "desc_stats.Rda", sep="_")
load(file=stats_file_name)   # loads frame desc_stats

# clear unrealistic values
desc_stats <- desc_stats[desc_stats$CP_num_obs > 15 & desc_stats$RoC_num_obs > 15, ]

# plot
plot_desc_stats_CP_vs_RoC(desc_stats)
