# get all species from GOAIERP

# need to read data from all survey areas. Just read them and bind them together
library(tidyverse)
library(data.table)

# import data
load("data/goaierp_data.Rdata")

all_species <- goaierp_data %>% select(CommonName) %>% distinct()

# try and fill this with the mapping we have already done, by species name
race_species <- read.csv("data/RACE_species_goa_Atlantis.csv")
race_species <- race_species %>% dplyr::select(Atlantis.group,Common.Name,Scientific.Name)

# base matching on scientific name, as common names may vary. This will not get all of them (some of the names were missing in RACE too, some will be spelled different, etc)

match_name <- function(CommonName) {
  At_gr <- race_species[grep(CommonName,race_species$Common.Name,ignore.case = TRUE),]$Atlantis.group
  if(length(At_gr)==0) {
    At_gr <- race_species[grep(CommonName,race_species$Scientific.Name,ignore.case = TRUE),]$Atlantis.group
  }
  At_gr <- At_gr[!is.na(At_gr)]
  if(length(unique(factor(At_gr)))==1) {
    At_gr <- At_gr[1]
  } else {
    At_gr <- paste(unique(factor(At_gr)),sep = " ", collapse = " ")
  }
  return(At_gr)
}

all_species <- all_species %>% rowwise() %>% mutate(Atlantis.group=match_name(CommonName))

# need to finish the rest manually (also to check that everything is sensible)

write.csv(all_species,"data/goaierp_species.csv",row.names=FALSE)
