# From DOI to bib
# Renata Muylaert - 2024
#-------------------------------------------------------------------------------------------------------------

# Packages
#install.packages(c("rcrossref", "igraph", "dplyr"))

library(rcrossref)
library(dplyr)
library(purrr)
library(stringr)

# Get doi list from master table (to be populated)

doi <-  c(
  "10.1038/s41559-019-1002-3",
  "10.3390/v16071154",
  "10.1007/s40823-024-00096-3",
  "10.1371/journal.pcbi.1010362")

refs <- unlist(cr_cn(dois = doi, format = "bibtex"))

refs

# Bib map references to populate a string table in main text or supplements

cite_keys <- refs %>% 
  map_chr(~ str_extract(.x, pattern = '\\@article\\{[A-Za-z_]*[0-9]+')) %>% 
  map_chr(~ str_extract(.x, pattern = '[A-Za-z_]*[0-9]+'))

cite_keys

# Export 

setwd('bib')

write(refs, file = "bibliography.bib")

#----------------------------------------------------------------------------------