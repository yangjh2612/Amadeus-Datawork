#trace(stop, quote(print(sys.calls())))
require(entropy)        # for entropy
require(acid)           # for weighted entropy
require(dplyr)

require(matrixStats)   # for weighted sd and median
# Note that instead of using radiant.data.weighted.sd we might want to correct the sd for the sample size 
#    after considering weights (i.e. in terms of employees not in terms of number of firms). This function does
#    not do that. But acid.weighted.moments is able to. 

# auxiliary functions for aggregation

# Shannon entropy from entropy
entrop <- function(dat, w = "ignored", na.rm="ignored") {
     dat <- na.omit(dat)
     ee <- entropy.empirical(dat)
     return(ee)
}

# Weighted entropy from acid
# Problem: This results in many NaN values because it includes logs of dat which are undefined where dat<0.
weighted.entrop <- function(dat, w, na.rm="ignored") {
     df <- na.omit(data.frame(dat = dat, w = w))
     df <- na.omit(df)
     ee <- weighted.entropy(df$dat, w=df$w)
#     if(is.nan(ee)){
#         print(ee)
#         print(head(df))
#         print(head(dat))
#         print(head(w))
#         save(df,file="tester.Rda")
#     }
     return(ee)
}

# sd does not accept additional unused arguments, hence we must wrap it to be able to supply unused weights.
# Instead, we could use ("weights" %in% formalArgs(func)), to distinguish the two cases, but if this is the
#   only function that cannot handle unused arguments, this is not necessary and would complicate the code.
stdev <- function(dat, na.rm=F, w = ignored) {
    sd <- sd(dat, na.rm=na.rm)
    return(sd)
}

# weighted sd from weightedVar in matrixStats
weighted.stdev <- function (dat, w, na.rm) {
    wvar <- weightedVar(dat, w=w, na.rm=na.rm)
    wsd <- (wvar)^.5
    return(wsd)
}

# weighted median without interpolation from matrixStats
# Note that interpolated weighted medians have terribly many implementations in R some of which produce different
#    results. See: https://stackoverflow.com/q/2748725/1735215 
#    The common implementation is the weighted percentile method see wikipedia: https://en.wikipedia.org/
#    wiki/Percentile#Weighted_percentile but that seems to not be the only one. 
#    For now, we just use non-interpolated weighted medians.
weighted.median <- function (dat, w, na.rm) {
    wm <- weightedMedian(dat, w, interpolate=F, na.rm=na.rm)
    return(wm)
}

# Number of non NA observations
num_obs <- function(dat, w = "ignored", na.rm="ignored") {
    non_na <- sum(!is.na(dat))
    return(non_na)
}

# Compute descriptive statistics of capital productivity and profitability (returns on capital) by NUTS2 region
desc_stat_by_file <- function(nuts_code, cfile, country, stat_variables = c("CP", "RoC")) {
    print(paste("Commencing", country, sep=" "))
    
    # load data file
    load(cfile, verbose=F)
    
    # will catch cases with empty data files
    if(nrow(Cleaned_dat_INDEX)==0) {
        return(NA)
    }
    
    # remove what we do not need
    Cleaned_dat_INDEX <- subset(Cleaned_dat_INDEX, select = c(IDNR, Year, get(nuts_code)))
    
    # merge into one frame
    Cleaned_dat_INDEX <- merge(Cleaned_dat_INDEX, Cleaned_dat_Productivity, c("IDNR", "Year"))
    rm(Cleaned_dat_Productivity)
    Cleaned_dat_INDEX <- merge(Cleaned_dat_INDEX, Cleaned_dat_Profitability, c("IDNR", "Year"))
    rm(Cleaned_dat_Profitability)
    Cleaned_dat_INDEX <- merge(Cleaned_dat_INDEX, Cleaned_dat_cost_structure, c("IDNR", "Year"))
    rm(Cleaned_dat_cost_structure)
    Cleaned_dat_INDEX <- merge(Cleaned_dat_INDEX, Cleaned_dat_firm_size, c("IDNR", "Year"))
    rm(Cleaned_dat_firm_size)
    unique_columns <- !(colnames(Cleaned_dat_RD) %in% colnames(Cleaned_dat_INDEX))
    unique_columns[match(c("IDNR", "Year"), colnames(Cleaned_dat_RD))] <- TRUE
    Cleaned_dat_INDEX <- merge(Cleaned_dat_INDEX, Cleaned_dat_RD[unique_columns], c("IDNR", "Year"))
    rm(Cleaned_dat_RD)
    
    #print(colnames(Cleaned_dat_INDEX))
    retained_columns <- c("Year", nuts_code, "EMPL", stat_variables)
    Cleaned_dat_INDEX <- subset(Cleaned_dat_INDEX, select = retained_columns)
    colnames(Cleaned_dat_INDEX) <- c("Year", "nuts_code", "EMPL", stat_variables)
    
    Cleaned_dat_INDEX_weights <- Cleaned_dat_INDEX[!is.na(Cleaned_dat_INDEX$EMPL),]
    Cleaned_dat_INDEX$EMPL <- NULL
    
    # compute statistics by region
    for(func in list("mean", "median", "stdev", "entrop", "num_obs")) {
        agg <-aggregate(.~nuts_code+Year, Cleaned_dat_INDEX, FUN=func, na.rm=T, na.action=NULL)
        agg <- agg[!(agg[,"nuts_code"] == ""),]
        colnames(agg) <- c(nuts_code, "Year", paste(stat_variables, func, sep="_"))
        print(colnames(agg))
        if(exists("all_results")) {
            all_results <- merge(all_results, agg, c(nuts_code, "Year"))
        } else {
            all_results <- agg
        }
    }
    
    # compute statistics by region for dplyr (for variables that require weights)
    dplyr_flist = list(weighted.mean, weighted.median, weighted.stdev, weighted.entrop)
    dplyr_fnames = list("weighted.mean", "weighted.median", "weighted.stdev", "weighted.entrop")
    for(i in 1:length(dplyr_flist)) {
        func = dplyr_flist[[i]]
        func_name = dplyr_fnames[[i]]
        print("FUUU")
        print(length(Cleaned_dat_INDEX_weights$EMPL))
        agg <- Cleaned_dat_INDEX_weights %>% group_by(nuts_code, Year) %>% summarise_at(vars(-EMPL,-Year,-nuts_code),funs(func(., EMPL, na.rm=T)))
        agg <- agg[!(agg[,"nuts_code"] == ""),]
        print("FUUUYYYY")
        colnames(agg) <- c(nuts_code, "Year", paste(stat_variables, func_name, sep="_"))
        print(colnames(agg))
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
desc_stat_all_files <- function (nuts_code, filenames, country_names, stat_variables = c("CP", "RoC")) {

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

nuts_level <- 1
nuts_code <- paste("NUTS", nuts_level, sep="_")

filenames = c("panels_J!&Albania.Rda", "panels_J!&Austria.Rda", "panels_J!&Belarus.Rda", "panels_J!&Belgium.Rda", "panels_J!&Bulgaria.Rda", "panels_J!&Croatia.Rda", "panels_J!&Cyprus.Rda", "panels_J!&Czech Republic.Rda", "panels_J!&Denmark.Rda", "panels_J!&Estonia.Rda", "panels_J!&Finland.Rda", "panels_J!&France.Rda", "panels_J!&Germany.Rda", "panels_J!&Greece.Rda", "panels_J!&Hungary.Rda", "panels_J!&Iceland.Rda", "panels_J!&Ireland.Rda", "panels_J!&Italy.Rda", "panels_J!&Kosovo.Rda", "panels_J!&Latvia.Rda", "panels_J!&Liechtenstein.Rda", "panels_J!&Lithuania.Rda", "panels_J!&Luxembourg.Rda", "panels_J!&Malta.Rda", "panels_J!&Moldova.Rda", "panels_J!&Monaco.Rda", "panels_J!&Montenegro.Rda", "panels_J!&Netherlands.Rda", "panels_J!&Norway.Rda", "panels_J!&Poland.Rda", "panels_J!&Portugal.Rda", "panels_J!&Russian Federation.Rda", "panels_J!&Serbia.Rda", "panels_J!&Slovakia.Rda", "panels_J!&Spain.Rda", "panels_J!&Sweden.Rda", "panels_J!&Switzerland.Rda", "panels_J!&Turkey.Rda", "panels_J!&United Kingdom.Rda")
country_names = c("Albania", "Austria", "Belarus", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Kosovo", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta", "Moldova", "Monaco", "Montenegro", "Netherlands", "Norway", "Poland", "Portugal", "Russian Federation", "Serbia", "Slovakia", "Spain", "Sweden", "Switzerland", "Turkey", "United Kingdom")
filenames = c("panels_J!&Austria.Rda", "panels_J!&Serbia.Rda")
country_names = c("Austria", "Serbia")

desc_stats <- desc_stat_all_files(nuts_code, filenames, country_names)
print(desc_stats)
output_file_name = paste(paste("Reg", nuts_code, sep="_"), "CP_RoC_desc_stats.Rda", sep="_")
save(desc_stats, file=output_file_name)

