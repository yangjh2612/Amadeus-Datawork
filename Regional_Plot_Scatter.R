library(ggplot2)
library(gridExtra)

# plotting function
plot_desc_stats_CP_vs_RoC <- function(desc_stats) {
    ggplot(desc_stats, aes(CP_median, RoC_median, colour=Country)) + geom_point(size=1,shape=".") + geom_rug(size=0.1) + theme(legend.title = element_blank(), legend.position="bottom") + facet_wrap(~Year) + coord_fixed(ratio=4) + labs(x="Capital Productivity", y="Return on Total Assets")
    ggsave(file = "Reg_NUTS_2_scatterplots_CP_vs_RoC.pdf")
}

# main entry point
load(file="Reg_NUTS_2_CP_RoC_desc_stats.Rda")   # loads frame desc_stats

# clear unrealistic values
desc_stats <- desc_stats[desc_stats$CP_num_obs > 15 & desc_stats$RoC_num_obs > 15, ]
plot_desc_stats_CP_vs_RoC(desc_stats)
