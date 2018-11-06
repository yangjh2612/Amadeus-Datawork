library(ggplot2)
library(gridExtra)

# function to obtain the correct column name in the desc_stats data frame that holds the number of observations for a given variable
get_number_observations_variable_name <- function(variable_name) {
    variable_name <- gsub(variable_name, pattern="weighted.", replacement="")
    variable_name_num_obs <- paste(sub("_[^_]+$", "", variable_name), "num_obs", sep="_")
    return(variable_name_num_obs)
}

# function to clean outliers. Not very generic. TODO: Make more generic
clean_out_outliers <- function (desc_stats, axis1, axis2, min_observations) {
    # clear unrealistic values
    axis1_no <- get_number_observations_variable_name(axis1)
    axis2_no <- get_number_observations_variable_name(axis2) 
    desc_stats <- desc_stats[desc_stats[axis1_no] > min_observations & desc_stats[axis2_no] > min_observations, ]

    # Cyprus has absurd values for RoC
    if (axis1 == "RoC_median" | axis2 == "RoC_median") {
        desc_stats <- desc_stats[desc_stats$RoC_median < 1.0, ]
    }

    # Latvia has absurd values for C_com
    if (axis1 == "C_com_median" | axis2 == "C_com_median") {
        desc_stats <- desc_stats[desc_stats$C_com_median < 10, ]
    }
    
    # Some countries have absurd values for PW_ratio
    if (axis1 == "PW_ratio_median" | axis2 == "PW_ratio_median") {
        desc_stats <- desc_stats[desc_stats$PW_ratio_median < 1.5 & desc_stats$PW_ratio_median > -1, ]
    }

    # no absurd dispersion values
    if (substring(axis1,nchar(axis1)+1-7)=="_entrop") {
        desc_stats <- desc_stats[desc_stats[axis1] < 50 & desc_stats[axis1] > -25, ]
    }
    if (substring(axis2,nchar(axis2)+1-7)=="_entrop") {
        desc_stats <- desc_stats[desc_stats[axis2] < 50 & desc_stats[axis2] > -25, ]
    }
    if (substring(axis1,nchar(axis1)+1-6)=="_stdev") {
        desc_stats <- desc_stats[desc_stats[axis1] < 200, ]
    }
    if (substring(axis2,nchar(axis2)+1-6)=="_stdev") {
        desc_stats <- desc_stats[desc_stats[axis2] < 200, ]
    }
    
    return(desc_stats)
}

# plotting function
plot_desc_stats_CP_vs_RoC <- function(desc_stats, axis1, axis2, min_observations=15) {
    desc_stats_reduced <- clean_out_outliers (desc_stats, axis1, axis2, min_observations)
    aspect_ratio_this_plot = (max(desc_stats_reduced[axis1], na.rm=T) - min(desc_stats_reduced[axis1], na.rm=T)) / (max(desc_stats_reduced[axis2], na.rm=T) - min(desc_stats_reduced[axis2], na.rm=T))
    ggplot(desc_stats_reduced, aes(get(axis1), get(axis2), colour=Country)) + geom_point(size=1,shape=".") + geom_rug(size=0.1) + theme(legend.title = element_blank(), legend.position="bottom") + facet_wrap(~Year) + coord_fixed(ratio=aspect_ratio_this_plot) + labs(x=axis1, y=axis2)
    pdf_file_name = paste(paste(paste(paste(paste(paste("Reg_NUTS", nuts_level, sep="_"), "scatterplots", sep="_"), axis1, sep="_"), "vs", sep="_"), axis2, sep="_"), "pdf", sep=".")
    ggsave(file = pdf_file_name)
}

# main entry point
nuts_level <- 2

# load data
stats_file_name <- paste(paste("Reg_NUTS", nuts_level, sep="_"), "desc_stats.Rda", sep="_")
load(file=stats_file_name)   # loads frame desc_stats

# hard coded factor level reordering #TODO: Change to more generic coding (hard coded lists are bad and break whenever the country list changes)
desc_stats$Country = factor(desc_stats$Country,levels(factor(desc_stats$Country))[c(28,18,3,20,15,32,23,8,29,11,10,12,2,30,9,17,24,7,27,14,4,26,5,22,21,25,16,1,6,13,19,31)])

# plot
plot_desc_stats_CP_vs_RoC(desc_stats, "CP_median", "RoC_median")
plot_desc_stats_CP_vs_RoC(desc_stats, "CP_median", "LP_median")
plot_desc_stats_CP_vs_RoC(desc_stats, "C_com_median", "CP_median")
plot_desc_stats_CP_vs_RoC(desc_stats, "C_com_median", "RoC_median")
plot_desc_stats_CP_vs_RoC(desc_stats, "LP_median", "RoC_median")
plot_desc_stats_CP_vs_RoC(desc_stats, "C_com_median", "LP_median")
plot_desc_stats_CP_vs_RoC(desc_stats, "PW_ratio_median", "LP_median")
plot_desc_stats_CP_vs_RoC(desc_stats, "C_com_entrop", "LP_entrop")
plot_desc_stats_CP_vs_RoC(desc_stats, "PW_ratio_median", "PW_ratio_stdev")
plot_desc_stats_CP_vs_RoC(desc_stats, "PW_ratio_median", "RoC_median")
plot_desc_stats_CP_vs_RoC(desc_stats, "PW_ratio_median", "PW_ratio_entrop")
plot_desc_stats_CP_vs_RoC(desc_stats, "C_com_median", "C_com_entrop")
plot_desc_stats_CP_vs_RoC(desc_stats, "CP_median", "CP_entrop")
plot_desc_stats_CP_vs_RoC(desc_stats, "LP_median", "LP_entrop")
plot_desc_stats_CP_vs_RoC(desc_stats, "RoC_median", "RoC_entrop")
