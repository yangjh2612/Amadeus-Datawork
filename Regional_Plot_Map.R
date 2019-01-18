# load libraries
if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(ggplot2, maptools, broom, rgdal, dplyr, rgeos)
#library("gpclib")

# function to obtain the correct column name in the desc_stats data frame that holds the number of observations for a given variable
get_number_observations_variable_name <- function(variable_name) {
    variable_name <- gsub(variable_name, pattern="weighted.", replacement="")
    variable_name_num_obs <- paste(sub("_[^_]+$", "", variable_name), "num_obs", sep="_")
    return(variable_name_num_obs)
}

# function to clean outliers. Not very generic. TODO: Make more generic
clean_out_outliers <- function (desc_stats, axis1, min_observations) {
    # clear unrealistic values
    axis1_no <- get_number_observations_variable_name(axis1)
    desc_stats <- desc_stats[desc_stats[axis1_no] > min_observations, ]
    
    # return if table is empty; will otherwise somehow result in an error
    if(nrow(desc_stats)==0){
        return(desc_stats)
    }
    
    # Cyprus has absurd values for RoC
    if (axis1 == "RoC_median") {
        desc_stats <- desc_stats[desc_stats$RoC_median < 1.0, ]
    }

    # Latvia has absurd values for C_com
    if (axis1 == "C_com_median") {
        desc_stats <- desc_stats[desc_stats$C_com_median < 10, ]
    }
    
    # Some countries have absurd values for PW_ratio
    if (axis1 == "PW_ratio_median") {
        desc_stats <- desc_stats[desc_stats$PW_ratio_median < 1.5 & desc_stats$PW_ratio_median > -1, ]
    }
    
    # no absurd dispersion values
    if (substring(axis1,nchar(axis1)+1-7)=="_entrop") {
        desc_stats <- desc_stats[desc_stats[axis1] < 50 & desc_stats[axis1] > -25, ]
    }
    if (substring(axis1,nchar(axis1)+1-6)=="_stdev") {
        desc_stats <- desc_stats[desc_stats[axis1] < 200, ]
    }
    
    return(desc_stats)
}

map_plot_by_var <- function(property, nuts_code="NUTS_2", min_observations=15) {
    
    for(year in 2006:2015) {
        
        # reduce to this year and clean out outliers
        desc_stats_yr <- desc_stats[desc_stats$Year == year, ]
        desc_stats_yr <- clean_out_outliers(desc_stats_yr, property, min_observations)
        
        # match regions in shapefile and data
        shape <- nutsshape
        #shape$prop <- desc_stats_yr[,property][match(shape$id, desc_stats_yr$NUTS_2)]
        shape$prop <- desc_stats_yr[,property][match(shape$id, desc_stats_yr[[nuts_code]])]
        
        shape$Year <- year
        
        # separate regions with and without data
        shape_data <- subset(shape, !is.na(shape$prop))
        shape_nodata <- subset(shape, is.na(shape$prop))
 
        # remove French overseas departements (they would cause Europe to be much smaller in the map)
        shape_nodata <- shape_nodata[!(shape_nodata$id %in% c("FRY1", "FRY2", "FRY3", "FRY4", "FRY5", "FRY")), ]
        if(exists("all_shape_data")) {
            all_shape_data <- rbind(all_shape_data, shape_data)
            all_shape_nodata <- rbind(all_shape_nodata, shape_nodata)
        } else {
            all_shape_data <- shape_data
            all_shape_nodata <- shape_nodata
        }
    }
    
    #print(median(all_shape_data$prop, na.rm=T))
 
    # plot                                                    
    ggplot() + 
        geom_polygon(data=all_shape_nodata, aes(long, lat, group = group), 
                     fill="grey90", color = "black", size = .1) +                        # regions for which we do not have data in grey
        geom_polygon(data=all_shape_data, aes(long, lat, fill=prop, group = group),      # regions for which we have data                                  
                     color = "black", size = .1) +                                       # borders in black, narrow borders
        facet_wrap(~Year) +                                                              # panels by year
        theme_classic() + theme(axis.line=element_blank(),axis.text.x=element_blank(),
              axis.text.y=element_blank(),axis.ticks=element_blank(),
              axis.title.x=element_blank(), axis.title.y=element_blank(), 
              legend.title = element_blank(), legend.position="bottom") +                # classic layout with axes, labels removed
        scale_fill_gradient2(low="#00FF00",mid="#0000FF",high="#FF0000",
              midpoint=median(all_shape_data$prop, na.rm=T)) +                           # gradient between green, red and blue
        coord_fixed(ratio=1)                                                             # force aspect ratio 1, i.e. no grotesque distortions 

    # save
    outputfilename = paste(paste(paste(paste("NUTS", nuts_level, sep="_"), "map", sep="_"), property, sep="_"), ".pdf", sep="")
    ggsave(file = outputfilename)
}


# main entry point
nuts_level <- 2
nuts_code <- paste("NUTS", nuts_level, sep="_")

# read shapefile and prepare as data.frame
# shapefile is from https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts
# ... the NUTS2016, 1:3M, SHP version.
# ... zip must be unpacked and placed in the directory "shapefile" which must be a subdirectory of cwd (current working directory).
eushp <- readOGR(dsn = "shapefile", layer = "NUTS_RG_03M_2016_3035")
eu_nuts <- subset(eushp, LEVL_CODE == nuts_level)
nutsshape <- tidy(eu_nuts, region="NUTS_ID")

# load and prepare data
stats_file_name <- paste(paste("Reg_NUTS", nuts_level, sep="_"), "desc_stats.Rda", sep="_")
load(file=stats_file_name)   # loads frame desc_stats

# variables to be plotted
plotlist = list("CP_median", "RoC_median", "CP_entrop", "RoC_entrop", "PW_ratio_median", "TOAS_median", "LP_median", "CP_change_median", "C_com_median", "PW_ratio_entrop", "TOAS_entrop", "LP_entrop", "CP_change_entrop", "C_com_entrop")
#plotlist = list("CP_median")

# commence plotting
for (var in plotlist) {
    print(paste("Commencing", var, sep=" "))
    map_plot_by_var(var, nuts_code)
}
