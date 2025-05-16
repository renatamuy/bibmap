# bibmap
# This script is a supplement to the manuscript:
# Muylaert et al., in prep. Connections in the Dark: Network Science and 
# Social-Ecological Networks as Tools for Bat Conservation and Public Health.
# Global Union of Bat Diversity Networks (GBatNet).
# See README for further info:
# https://github.com/renatamuy/bibmap

#Load or install the required packages
packages <- c("tidyverse", "writexl")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

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