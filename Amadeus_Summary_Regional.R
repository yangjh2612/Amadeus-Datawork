trace(stop, quote(print(sys.calls())))
require(entropy)

# auxiliary functions for aggregation

# Shannon entropy
entrop <- function(dat, na.rm="ignored") {
     ee <- entropy.empirical(dat)
     return(ee)
}

# Number of non NA observations
num_obs <- function(dat, na.rm="ignored") {
    non_na <- sum(!is.na(dat))
    return(non_na)
}

# Compute descriptive statistics of capital productivity and profitability (returns on capital) by NUTS2 region
desc_stat_by_file <- function(nuts_code, cfile, country) {
    print(paste("Commencing", country, sep=" "))
    
    # load data file
    load(cfile, verbose=F)
    
    # will catch cases with empty data files
    if(nrow(Cleaned_dat_INDEX)==0) {
        return(NA)
    }
    
    # remove what we do not need
    rm(Cleaned_dat_cost_structure)
    rm(Cleaned_dat_RD)
    rm(Cleaned_dat_firm_size)
    Cleaned_dat_INDEX <- subset(Cleaned_dat_INDEX, select = c(IDNR, Year, get(nuts_code)))
    Cleaned_dat_Profitability <- subset(Cleaned_dat_Profitability, select = c(IDNR, Year, RoC))
    Cleaned_dat_Productivity <- subset(Cleaned_dat_Productivity, select = c(IDNR, Year, CP))
    
    # merge into one frame
    Cleaned_dat_INDEX <- merge(Cleaned_dat_INDEX, Cleaned_dat_Productivity, c("IDNR", "Year"))
    Cleaned_dat_INDEX <- merge(Cleaned_dat_INDEX, Cleaned_dat_Profitability, c("IDNR", "Year"))
    Cleaned_dat_INDEX$IDNR <- NULL
    
    # compute statistics by region
    for(func in list("mean", "median", "sd", "entrop", "num_obs")) {
        agg <-aggregate(.~get(nuts_code)+Year, Cleaned_dat_INDEX, FUN=func, na.rm=T, na.action=NULL)
        agg <- agg[!(agg[,nuts_code] == ""),]
        colnames(agg) <- c(nuts_code, "Year", paste("CP", func, sep="_"), paste("RoC", func, sep="_"))
        #colnames(agg) <- c("NUTS_2", "Year", "CP_mean", "RoC_mean")
        if(exists("all_results")) {
            all_results <- merge(all_results, agg, c(nuts_code, "Year"))
        } else {
            all_results <- agg
        }
    }
    
    # will catch cases in which the results frame has no elements (presumably because of too few observations for each region)
    if(nrow(all_results)==0) {
        return(NA)
    }
    
    # add country to results and return
    all_results$Country <- country
    return(all_results)
}

# handle iteration over files list, call function to compute descriptive statistics for all, merge results
desc_stat_all_files <- function (nuts_code, filenames, country_names) {

    nfiles = length(filenames)
    
    for(i in 1:nfiles) {
        cfile = filenames[[i]]
        country = country_names[[i]]
        res <- desc_stat_by_file(nuts_code, cfile, country)
        if(!is.na(res)) {
            if(exists("all_results")) {
                all_results <- rbind(all_results, res)
            } else {
                all_results <- res
            }
        }
    }
    
    return(all_results)
    
}

nuts_level <- 2
nuts_code <- paste("NUTS", nuts_level, sep="_")

filenames = c("panels_J!&Albania.Rda", "panels_J!&Austria.Rda", "panels_J!&Belarus.Rda", "panels_J!&Belgium.Rda", "panels_J!&Bulgaria.Rda", "panels_J!&Croatia.Rda", "panels_J!&Cyprus.Rda", "panels_J!&Czech Republic.Rda", "panels_J!&Denmark.Rda", "panels_J!&Estonia.Rda", "panels_J!&Finland.Rda", "panels_J!&France.Rda", "panels_J!&Germany.Rda", "panels_J!&Greece.Rda", "panels_J!&Hungary.Rda", "panels_J!&Iceland.Rda", "panels_J!&Ireland.Rda", "panels_J!&Italy.Rda", "panels_J!&Kosovo.Rda", "panels_J!&Latvia.Rda", "panels_J!&Liechtenstein.Rda", "panels_J!&Lithuania.Rda", "panels_J!&Luxembourg.Rda", "panels_J!&Malta.Rda", "panels_J!&Moldova.Rda", "panels_J!&Monaco.Rda", "panels_J!&Montenegro.Rda", "panels_J!&Netherlands.Rda", "panels_J!&Norway.Rda", "panels_J!&Poland.Rda", "panels_J!&Portugal.Rda", "panels_J!&Russian Federation.Rda", "panels_J!&Serbia.Rda", "panels_J!&Slovakia.Rda", "panels_J!&Spain.Rda", "panels_J!&Sweden.Rda", "panels_J!&Switzerland.Rda", "panels_J!&Turkey.Rda", "panels_J!&United Kingdom.Rda")
country_names = c("Albania", "Austria", "Belarus", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Kosovo", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta", "Moldova", "Monaco", "Montenegro", "Netherlands", "Norway", "Poland", "Portugal", "Russian Federation", "Serbia", "Slovakia", "Spain", "Sweden", "Switzerland", "Turkey", "United Kingdom")
#filenames = c("panels_J!&Austria.Rda", "panels_J!&Serbia.Rda")
#country_names = c("Austria", "Serbia")

desc_stats <- desc_stat_all_files(nuts_code, filenames, country_names)
#print(desc_stats)
output_file_name = paste(paste("Reg", nuts_code, sep="_"), "CP_RoC_desc_stats.Rda", sep="_")
save(desc_stats, file=output_file_name)

