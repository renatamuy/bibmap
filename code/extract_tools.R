# Network tools usage table
# Muylaert et al. 

require(tidyverse)
#install.packages("writexl")  
library(writexl)

setwd("data")
getwd()
list.files()

df <- read.csv("bibmap_variables.csv")

df$Network_tools
df$Number

tool_counts <- df %>%
  mutate(Network_tools = str_replace_all(Network_tools, ",\\s*|\\s*,\\s*", " , ")) %>% # Standardize separators
  mutate(Network_tools = str_split(Network_tools, " , ")) %>%
  unnest(Network_tools) %>%
  mutate(Network_tools = str_trim(Network_tools)) %>%
  distinct(Number, Network_tools) %>%
  count(Network_tools, sort = TRUE, name = "n_mentions") %>%
  mutate(percentage = round(100 * n_mentions / sum(n_mentions), 1))

setwd('../figures')

table(tool_counts$Network_tools)
tail(tool_counts)

colnames(tool_counts) <- c("Network tools",	"Number of mentions",	"Percentage (%)")

write_xlsx(tool_counts, path = "Table_tool_counts.xlsx")

#------------------------------------------------