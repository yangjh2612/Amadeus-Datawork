rm(list=ls()) # clear environment
gc() # clear memory

if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org') # get this package if you don't have it
pacman::p_load(tidyverse,data.table) # load these packages, install if necessary

'%!in%' <- function(x,y)!('%in%'(x,y)) # a function I use for convenience

# setwd("~/Work/DEF_KLEMS_2017ii") # working directory here

# VA deflators
klems_va <- fread(file = 'ALL_output_17ii.txt') %>% filter(var == 'VA_P') # load the KLEMS file, only keep the VA price indices for now
klems_va$new_c <- klems_va$code # I duplicate this variable so that I can work with it
kl <- data.table(klems_va[klems_va$country == 'FR', ] %>% select(code, new_c)) # I isolate the codes
kl_n <- kl[!grepl('[[:alpha:]]', kl$code)] # take the numerical categories in isolation
kl_n$new_c <- str_replace(kl_n$new_c,"-",",") # replace - with a comma
kl_n$new_c[grepl(',', kl_n$new_c)] <- paste("seq(",kl_n$new_c[grepl(',', kl_n$new_c)],",by=1)") # this way we can replace each cell with a function that can be read by R - in this case, take the sequence of numbers between the numbers in a given cell
kl_n$new_c <- map(kl_n$new_c, ~ eval(parse(text=.))) # the commands are then evaluated here, creating lists of the codes that are comprised in a given category (which is still reported in 'code')
kl_n <- unnest(kl_n) # this command expands the lists, so that the elements themselves become observations
kl_n$parent <- as.character(kl_n$new_c) # these two digit codes will be use to merge in with the rest of the data

nace2 <- read_csv("nace2.csv")
nace2$Code <- gsub('\\.', '', nace2$Code) # clean-up the codes from the file
nace2$Code[nace2$Level == 2] <- paste(nace2$Code[nace2$Level == 2], '00', sep = '') # I turn everything into 4-digit codes
nace2$Code[nace2$Level == 3] <- paste(nace2$Code[nace2$Level == 3], '0', sep = '')
nace2 <- nace2 %>% select(Code) # idolate the codes
nace2$head <- gsub('[[:digit:]]*', '', nace2$Code) # for each code we want the header (big category)
while(any(grepl('^[:blank:]*$', nace2$head) == T)){ # the headers are then assigned depending on which group the code falls under
  nace2$head <- ifelse(grepl('^[:blank:]*$', nace2$head), lag(nace2$head), nace2$head)
}
nace2$parent <- substr(nace2$Code, 1, 2) # take out the 2-digit code
nace2 <- nace2 %>% left_join(kl_n %>% select(def_cd = code, parent), by = 'parent') # join the whole thing with the KLEMS concordance

# use headers for rest, since not all codes are numerical
kl_a <- kl[grepl('[[:alpha:]]', kl$code)] # isolate the alphabetical codes this time
kl_a <- kl_a[ , list(new_c = unlist(strsplit(new_c , '-'))), by = code] # same as before
kl_a <- kl_a %>% filter(new_c %!in% kl_a$new_c[duplicated(kl_a$new_c)] | new_c == code)# I check for duplicates, since some categories are aggreagated. I keep the finer ones.
kl_a$head <- as.character(kl_a$new_c)

nace2 <- nace2 %>% left_join(kl_a %>% select(def_cd_h = code, head), by = 'head') %>% # join this, and assign the categories where the 4-digit codes are unassigned
  mutate(def_cd = ifelse(is.na(def_cd), def_cd_h, def_cd)) %>%
  select(-c(def_cd_h, head, parent))

nace2 <- unique(nace2) # remove duplicates

def_list <- list() # deflators by country

for(i in unique(klems_va$country)){ # assign country codes
  def_list[[i]] <- nace2 %>% mutate(ctry = i) # create country code variable
}

all_id <- rbindlist(def_list) # full list of possible nace2 codes, klems categories, country codes

all_p_ind <- all_id %>% # now we can join all the KLEMS data
  left_join(klems_va %>% select(-var, -new_c) %>% rename(def_cd = code, ctry = country), by = c('ctry', 'def_cd'))
all_p_ind <- melt(all_p_ind, id = c('Code', 'def_cd', 'ctry')) # put into long format
all_p_ind <- all_p_ind %>%  # rename the variables appropriately
  rename(nace2 = Code, year = variable, p_ind_va = value) %>%
  mutate(year = as.numeric(gsub('_', '', year)))

rm(kl, kl_a, kl_n, klems_va, def_list, nace2)

# GO deflators
klems_go <- fread(file = 'ALL_output_17ii.txt') %>% filter(var == 'GO_P') # same for Gross Output deflators

all_p_ind_go <- all_id %>% 
  left_join(klems_go %>% select(-var) %>% rename(def_cd = code, ctry = country), by = c('ctry', 'def_cd'))
all_p_ind_go <- melt(all_p_ind_go, id = c('Code', 'def_cd', 'ctry'))
all_p_ind_go <- all_p_ind_go %>% 
  rename(nace2 = Code, year = variable, p_ind_go = value) %>%
  mutate(year = as.numeric(gsub('_', '', year)))

all_p_ind <- all_p_ind %>% left_join(all_p_ind_go, by = c('nace2', 'def_cd', 'ctry', 'year'))
rm(all_p_ind_go, klems_go)

# Capital deflators
klems_cp <- fread(file = 'ALL_capital_17i.txt', quote = '', colClasses = 'character') # issue in original file, had to edit line manually
colnames(klems_cp) <- gsub('\"', '', colnames(klems_cp)) # some cleaning required
klems_cp$country <- gsub('\"', '', klems_cp$country)
klems_cp$`_2015` <- gsub('\"', '', klems_cp$`_2015`)
klems_cp <- klems_cp %>% filter(var == 'Ip_GFCF') # keep this deflator

klems_va <- fread(file = 'ALL_output_17ii.txt') %>% filter(var == 'VA_P') # import again to check whether code categories are the same
identical(klems_cp$code[klems_cp$country == 'AT'], klems_va$code[klems_va$country == 'AT']) # True, so don't need to re-do concordance
rm(klems_va)

all_p_ind_cp <- all_id %>% # join in, just like before
  left_join(klems_cp %>% select(-var) %>% rename(def_cd = code, ctry = country), by = c('ctry', 'def_cd'))
all_p_ind_cp <- melt(all_p_ind_cp, id = c('Code', 'def_cd', 'ctry'))
all_p_ind_cp <- all_p_ind_cp %>% 
  rename(nace2 = Code, year = variable, p_ind_cp = value) %>%
  mutate(year = as.numeric(gsub('_', '', year)),
         p_ind_cp = as.numeric(p_ind_cp))

all_p_ind <- all_p_ind %>% left_join(all_p_ind_cp, by = c('nace2', 'def_cd', 'ctry', 'year'))
rm(all_p_ind_cp, klems_cp)

all_p_ind <- all_p_ind %>% filter(!(is.na(p_ind_cp) & is.na(p_ind_va) & is.na(p_ind_go))) # remove observations where no deflators exist - saves space
save(all_p_ind, file = 'DEF_KLEMS_2017ii.Rda')
