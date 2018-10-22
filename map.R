# load libraries
library("ggplot2")
library("maptools")
library("broom")
library("rgdal")
library("dplyr")


map_plot_by_var <- function(property) {
    
    for(year in 2006:2015) {
        desc_stats_yr <- desc_stats[desc_stats$Year == year, ]
    
        # match regions in shapefile and data
        shape <- nuts2shape
        #shape$CP_median <- desc_stats$CP_median[match(shape$id, desc_stats$NUTS_2)]
        shape$prop <- desc_stats_yr[,property][match(shape$id, desc_stats_yr$NUTS_2)]
        
        shape$Year <- year
        
        # separate regions with and without data
        shape_data <- subset(shape, !is.na(shape$prop))
        shape_nodata <- subset(shape, is.na(shape$prop))
        # remove French overseas departements (they would cause Europe to be much smaller in the map)
        shape_nodata <- shape_nodata[!(shape_nodata$id %in% c("FRY1", "FRY2", "FRY3", "FRY4", "FRY5")), ]
        if(exists("all_shape_data")) {
            all_shape_data <- rbind(all_shape_data, shape_data)
            all_shape_nodata <- rbind(all_shape_nodata, shape_nodata)
        } else {
            all_shape_data <- shape_data
            all_shape_nodata <- shape_nodata
        }
    }
    
    print(median(all_shape_data$prop, na.rm=T))
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
    outputfilename = paste(paste("NUTS_2_map_", property, sep=""), ".pdf", sep="")
    ggsave(file = outputfilename)
}




# main entry point

# read shapefile and prepare as data.frame
# shapefile is from https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts
# ... the NUTS2016, 1:3M, SHP version.
# ... zip must be unpacked and placed in the directory "shapefile" which must be a subdirectory of cwd (current working directory).
eushp <- readOGR(dsn = "shapefile/", layer = "NUTS_RG_03M_2016_3035")
eu_nuts2 <- subset(eushp, LEVL_CODE == 2)
nuts2shape <- tidy(eu_nuts2, region="NUTS_ID")

# load and prepare data
load("NUTS_2_CP_RoC_desc_stats.Rda", verbose=T)
desc_stats <- desc_stats[desc_stats$CP_num_obs > 15 & desc_stats$RoC_num_obs > 15, ]

# variables to be plotted
plotlist = list("CP_median", "RoC_median", "CP_entrop", "RoC_entrop")

# commence plotting
for (var in plotlist) {
    print(paste("Commencing", var, sep=" "))
    map_plot_by_var(var)
}
