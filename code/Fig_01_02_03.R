# bibmap
# This repo is a supplement to the manuscript:
# Muylaert et al., in prep. Connections in the Dark: Network Science and 
# Social-Ecological Networks as Tools for Bat Conservation and Public Health.
# Global Union of Bat Diversity Networks (GBatNet).
# See README for further info:
# https://github.com/renatamuy/bibmap/blob/main/README.md


# Packages
if(!require(devtools)){
  install.packages("devtools")
  library(devtools)
}

if(!require(forcats)){
  install.packages("forcats")
  library(forcats)
}

if(!require(here)){
  install.packages("here")
  library(here)
}

devtools::install_github("G-Thomson/Manu")
library(Manu)

if(!require(RColorBrewer)){
  install.packages("RColorBrewer")
  library(RColorBrewer)
}

if(!require(rnaturalearth)){
  install.packages("rnaturalearth")
  library(rnaturalearth)
}

if(!require(rnaturalearthdata)){
  install.packages("rnaturalearthdata")
  library(rnaturalearthdata)
}

if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list= ls())

setwd("../data")
getwd()
list.files()

df <- xlsx::read.xlsx("bibmap_variables.xlsx", sheetIndex = 1, startRow=1)

tail(df[1:133,])

df[1,1:16]

df <- df[1:133, 1:16] #valid rows

table(df$Reference)

tail(df)

# Studies summary

table(df$Weight)

datasum <- df %>% distinct(Number, .keep_all = TRUE)

length(unique(datasum$Number))

# double checking data content and format

studies <- datasum %>%
  group_by(Number) %>%
  summarise(Count = n())

table(datasum$Global.South)

table(df$Mode)

table(df$Weight)
# Most nextwork types evaluated were weighted, followed by binary of both

df$Links_EE <- fct_infreq(df$Links_EE)


########################## FIGURE 2 ############################################


setwd('../figures')

table(df$Links_detail)

df$Category <- with(df, case_when(
  grepl("virus|viral|microbiome|parasite|rabies|Hendra|ectoparasite", Links_detail, ignore.case = TRUE) ~ "Host-pathogen \n interactions",
  grepl("species|genotype|genetic|evolutionary", Links_detail, ignore.case = TRUE) ~ "Metacommunities",
  grepl("frugivory|nectarivory|seed dispersal|pollination|feeding", Links_detail, ignore.case = TRUE) ~ "Mutualistic \n interactions",
  grepl("social|behavior|roosts|shared|foraging|reproduction", Links_detail, ignore.case = TRUE) ~ "Bat societies",
  grepl("use of|landscape|resource|corridors|tents|roost", Links_detail, ignore.case = TRUE) ~ "Use of space",
  grepl("coauthorship|transmission|ecological|network|physical contact", Links_detail, ignore.case = TRUE) ~ "Social-ecological \n networks",
  grepl("predation", Links_detail, ignore.case = TRUE) ~ "Predation",
  grepl("brain", Links_detail, ignore.case = TRUE) ~ "Brain function",
  TRUE ~ NA_character_ # exception
))

data.frame(df$Links_detail, df$Category)

df1 <- df %>% filter(!is.na(Global.South))

df1 <- df1 %>% filter(Global.South != 'NA')

df1 <- df1 %>%  filter(!is.na(Links_EE))

df1 <- df1 %>% 
  filter(!is.na(Mode), !is.na(Category))

#--- export fig

jpeg(filename = 'Figure_02.jpg', res = 400, units = 'cm', width = 20, height = 14)

 ggplot(df1, aes(x = Category, fill = Mode)) +
  geom_bar() + 
  coord_flip() +
  labs(
    title = "Ecological Network Studies",
    x = "Topic",
    y = "Number of studies",
    fill = "Network type" 
  ) +
   scale_fill_manual(values = get_pal("Kakapo") ) +
   #scale_fill_viridis(discrete = TRUE) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

dev.off()


########################## FIGURE 1 ############################################


dfy <- data.frame(Number = df$Number, Year= df$Year)

dfy <- dfy %>% distinct(Number, Year, .keep_all = TRUE)

studies_per_year <- dfy %>%
  group_by(Year) %>%
  summarise(Count = n())

studies_per_2years <- dfy %>%
  mutate(Year = (Year %/% 2) * 2) %>%  
  group_by(Year) %>%                  
  summarise(Count = n())  

# cumulative
df$Year_Binned <- cut(df$Year, breaks = seq(2006, 2024, by = 2))
                      
year_counts <- table(df$Year_Binned)

# Create cumulative counts
cumulative_counts <- cumsum(year_counts)

# Create the data frame for plotting
plot_data <- data.frame(
  Year_Range = names(cumulative_counts),
  Cumulative_Count = cumulative_counts
)

# Option - cumulative bar plot
jpeg(filename = 'Figure_01_cumulative.jpg', res = 400, units = 'cm', width = 14, height = 10 )

ggplot(plot_data, aes(x = Year_Range, y = Cumulative_Count)) +
  geom_bar(stat = "identity", fill = "#7D9D33", color = "#7D9D33") + 
  #It's better to make it consistent with the Kakapo palette used in Fig2
  labs(title = "", x = "Year", y = "Cumulative number of studies") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

dev.off()


# Figure 1 - noncumulative
setwd('../figures')

studies_per_2years$lab <- c('2006-2007','2008-2009', '2010-2011',
                            '2012-2013', '2014-2015', 
                            '2016-2017', '2018-2019', '2020-2021', '2022-2023', '2024')
  
jpeg(filename = 'Figure_01.jpg', res = 400, units = 'cm', width = 14, height = 10 )

ggplot(studies_per_2years, aes(x = lab, y = Count)) +
  geom_bar(stat = "identity", fill = "#7D9D33") + #Kakaperized too  
  labs(
    title = "",
    x = "Year",
    y = "Number of studies"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

dev.off()


########################## FIGURE 3 ############################################


table(df$Country_or_Region)

unique(df$Country_or_Region)

df <- df %>%
  mutate(Geo_general = ifelse(grepl(" and ", Country_or_Region), "Multicountry", Country_or_Region))

# go on with recoding 

df <- df %>%
  mutate(Geo_general = ifelse(grepl("Neotropical", Geo_general), "Multicountry", Geo_general))


# Recode and remove duplicate records for multinetwork studies
dfg <- df %>%
  mutate(Geo_general = ifelse(grepl("North Africa", Geo_general), "Multicountry", Geo_general))%>% 
  distinct(Number, .keep_all = TRUE)


# check
unique(dfg$Geo_general)

# other option for countries

dfg <- df %>%
  mutate(
    Geo = case_when(
      Country_or_Region == "USA" ~ "United States of America",
      Country_or_Region == "England" ~ "United Kingdom",
      Country_or_Region == "Ucrane and Russia and Azerbaijan" ~ "Ukraine, Russia, Azerbaijan",
      Country_or_Region == "Bahamas and Greater Antilles and Lesser Antilles" ~ "Caribbean",
      Country_or_Region == "Neotropical" ~ "Neotropics",
      Country_or_Region == "Neotropics and Europe" ~ "Neotropics, Europe",
      Country_or_Region == "Costa Rica and Panama" ~ "Costa Rica, Panama",
      Country_or_Region == "Colombia and Venezuela" ~ "Colombia, Venezuela",
      Country_or_Region == "Global" ~ "World",
      Country_or_Region == "Global " ~ "World", 
      Country_or_Region == "Brazil and Bolivia and Paraguay" ~ "Brazil, Bolivia, Paraguay",
      Country_or_Region == "Romania and Hungary" ~ "Romania, Hungary",
      Country_or_Region == "Netherlands and Belgium and Hungary and Romania" ~ "Netherlands, Belgium, Hungary, Romania",
      is.na(Country_or_Region) ~ NA_character_, 
      TRUE ~ Country_or_Region 
    )
  )


dfc <- dfg %>%
  group_by(Geo_general) %>%
  summarise(Count = n()) %>% 
  arrange(desc(Count))


dfc <- dfc %>% 
  filter(!is.na(Geo_general)) %>% 
  arrange(Count)

# export table 
xlsx::write.xlsx(dfc, "Table_studies_per_country.xlsx")

# Figure03

dfc$Geo_general <- reorder(dfc$Geo_general, -dfc$Count)

# export 
jpeg(filename = 'Figure_03.jpg', res = 400, units = 'cm', width = 24, height = 20 )

dfc$Geo_general

dfc <- dfc %>%
  mutate(
    fill_color = ifelse(Geo_general %in% c("Global", "Multicountry", "In silico"), "#DCC949", "#7D9D33")
  )


ggplot(dfc, aes(x = Geo_general, y = Count, fill = fill_color)) +
  geom_bar(
    stat = "identity", 
    show.legend = FALSE
  ) + 
  scale_fill_identity() +  # Directly maps the fill_color column
  coord_flip() + 
  labs(
    title = "",
    x = "Country or Region",
    y = "Number of studies"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14)
  )

dev.off()


########################## SUPPLEMENT FIGURE S4 ################################


# Map - removing studies with global or vague regional mentions (keeping only named countries)

head(df)

country_hits <- df %>%
  distinct(Number, .keep_all = TRUE) %>% 
  filter(!is.na(Geo_general)) %>% 
  separate_rows(Geo_general, sep = ", ") %>% 
  filter(!Geo_general %in% c('In silico', "Global", "Neotropical", "Neotropics and Europe", "North Africa", "Europe")) %>% # Exclude vague regions
  group_by(Geo_general) %>%
  summarize(Hits = n()) %>%
  ungroup()

head(country_hits)
tail(country_hits)

world <- ne_countries(scale = "medium", returnclass = "sf")

world$name

map_data <- world %>%
  left_join(country_hits, by = c("name" = "Geo_general"))

head(map_data)

# Export fig
jpeg(filename = 'Figure_S4.jpg', res = 400, units = 'cm', width = 14, height = 10 )

ggplot(map_data) +
  geom_sf(aes(fill = Hits), color = "gray70", size = 0.2) +
  scale_colour_gradient2(
    low = "#7D9D33",   #Kakaperized
    mid = "#DCC949",
    high = "#775B24",
    midpoint = 10,
    space = "Lab",
    guide = "colourbar",
    aesthetics = "fill",
    na.value = "lightgray",
    name = "Number of studies"
  ) +
  theme_minimal() +
  labs(
    title = "",
    subtitle = "",
    caption = ""
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 10)
  )

dev.off()
#-------------------------------------------
