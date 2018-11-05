library(ggplot2)
library(gridExtra)

# plotting function
plot_desc_stats_CP_vs_RoC <- function(desc_stats, axis1, axis2) {
    #ggplot(desc_stats, aes(CP_median, RoC_median, colour=Country)) + geom_point(size=1,shape=".") + geom_rug(size=0.1) + theme(legend.title = element_blank(), legend.position="bottom") + facet_wrap(~Year) + coord_fixed(ratio=4) + labs(x="Capital Productivity", y="Return on Total Assets")
    ggplot(desc_stats, aes(get(axis1), get(axis2), colour=Country)) + geom_point(size=1,shape=".") + geom_rug(size=0.1) + theme(legend.title = element_blank(), legend.position="bottom") + facet_wrap(~Year) + coord_fixed(ratio=4) + labs(x=axis1, y=axis2)
    pdf_file_name = paste(paste(paste(paste(paste(paste("Reg_NUTS", nuts_level, sep="_"), "scatterplots", sep="_"), axis1, sep="_"), "vs", sep="_"), axis2, sep="_"), "pdf", sep=".")
    ggsave(file = pdf_file_name)
}

# main entry point
nuts_level <- 1

# load data
stats_file_name <- paste(paste("Reg_NUTS", nuts_level, sep="_"), "desc_stats.Rda", sep="_")
load(file=stats_file_name)   # loads frame desc_stats

# clear unrealistic values
desc_stats <- desc_stats[desc_stats$CP_num_obs > 15 & desc_stats$RoC_num_obs > 15, ]

# plot
plot_desc_stats_CP_vs_RoC(desc_stats, "CP_median", "RoC_median")
plot_desc_stats_CP_vs_RoC(desc_stats, "CP_median", "LP_median")
plot_desc_stats_CP_vs_RoC(desc_stats, "PW_ratio_median", "LP_median")
plot_desc_stats_CP_vs_RoC(desc_stats, "PW_ratio_median", "PW_ratio_sd")
plot_desc_stats_CP_vs_RoC(desc_stats, "PW_ratio_median", "PW_ratio_entropy")
plot_desc_stats_CP_vs_RoC(desc_stats, "PW_ratio_median", "RoC_median")
