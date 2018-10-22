library(ggplot2)
library(gridExtra)

# plotting function (inconvenient and complicated; use the other below, if at all possible)
plot_desc_stats_CP_vs_RoC_old <- function(desc_stats) {
    plots <- list()
    lpos = c('none','none','none','none','none','none','none','none','none','bottom')
    for (year in 2006:2015) {
        idx = year - 2005
        year_results = desc_stats[desc_stats$Year==year, ]
        #scatterplot(year_results$CP_median, year_results$RoC_median, reg.line=FALSE, xlab="Capital Productivity", ylab="Return on Total Assets")
        plots[[idx]] <- ggplot(year_results, aes(CP_median, RoC_median, colour=Country)) + geom_point() + geom_rug(size=0.1) + theme(legend.title = element_blank(), legend.position=lpos[[idx]], text = element_text(size=10))
    }
    do.call(grid.arrange, c(plots, ncol=3))
    ggsave(file = "Reg_NUTS_2_scatterplots_CP_vs_RoC.pdf", arrangeGrob(grobs = plots, ncol = 3))

    #  ggplot(iris, aes(Sepal.Width, Petal.Width, colour=Species)) +
    #    geom_point() +
    #    geom_smooth(alpha=0.2) +
    #    theme(legend.position="top"),
}

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
