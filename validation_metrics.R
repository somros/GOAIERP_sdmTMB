# code to stitch together the validation metrics for AFSC and DFO
library(tidyverse)
library(data.table)

goaierp_files <- list.files("output/validation_tables/", full.names = TRUE)

goaierp_titles <- list.files("output/validation_tables/", full.names = FALSE)

goaierp_titles <- gsub("_GOAIERP.csv","",goaierp_titles)

all_tabs <- vector(mode="list",length=length(goaierp_files))

for (i in 1:length(goaierp_titles)){
  tab <- read.csv(goaierp_files[i])
  tab <- tab %>% mutate(Group = goaierp_titles[i])
  all_tabs[[i]] <- tab
}

goaierp_val <- rbindlist(all_tabs)

#write out

write.csv(goaierp_val,"goaierp_validation.csv",row.names = FALSE)
