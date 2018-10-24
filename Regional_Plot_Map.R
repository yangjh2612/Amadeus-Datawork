# load libraries
library("ggplot2")
library("maptools")
library("broom")
library("rgdal")
library("dplyr")


map_plot_by_var <- function(property, nuts_code="NUTS_2") {
    
    for(year in 2006:2015) {
        desc_stats_yr <- desc_stats[desc_stats$Year == year, ]
    
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
nuts_level <- 1
nuts_code <- paste("NUTS", nuts_level, sep="_")

# read shapefile and prepare as data.frame
# shapefile is from https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts
# ... the NUTS2016, 1:3M, SHP version.
# ... zip must be unpacked and placed in the directory "shapefile" which must be a subdirectory of cwd (current working directory).
eushp <- readOGR(dsn = "shapefile/", layer = "NUTS_RG_03M_2016_3035")
eu_nuts <- subset(eushp, LEVL_CODE == nuts_level)
nutsshape <- tidy(eu_nuts, region="NUTS_ID")

# load and prepare data
stats_file_name <- paste(paste("Reg_NUTS", nuts_level, sep="_"), "desc_stats.Rda", sep="_")
load(file=stats_file_name)   # loads frame desc_stats
desc_stats <- desc_stats[desc_stats$CP_num_obs > 15 & desc_stats$RoC_num_obs > 15, ]

# variables to be plotted
plotlist = list("CP_median", "RoC_median", "CP_entrop", "RoC_entrop")

# commence plotting
for (var in plotlist) {
    print(paste("Commencing", var, sep=" "))
    map_plot_by_var(var, nuts_code)
}
