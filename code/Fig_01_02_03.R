# Muylaert et al 
# bat-network research review
# R version 4.4.1 
# Race for Your Life  

require(RColorBrewer)
require(tidyverse)
library(forcats)
require(Manu)
library(rnaturalearth)
library(rnaturalearthdata)
library(here)        

setwd(here())

setwd('data')

list.files()

df <- xlsx::read.xlsx("bibmap_variables_prelim4.xlsx", sheetIndex = 1, startRow=1)

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


# Figure 1

df$Links_EE <- fct_infreq(df$Links_EE)

# Figure 2 

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

# Figure 1- growth

dfy <- data.frame(Number = df$Number, Year= df$Year)

dfy <- dfy %>% distinct(Number, Year, .keep_all = TRUE)

studies_per_year <- dfy %>%
  group_by(Year) %>%
  summarise(Count = n())

studies_per_2years <- dfy %>%
  mutate(Year = (Year %/% 2) * 2) %>%  
  group_by(Year) %>%                  
  summarise(Count = n())  


# Fig 1
setwd('../figures')

jpeg(filename = 'Figure_01.jpg', res = 400, units = 'cm', width = 14, height = 10 )

ggplot(studies_per_2years, aes(x = Year, y = Count)) +
  geom_bar(stat = "identity", fill = "royalblue") +
  scale_x_continuous(
    breaks = seq(min(studies_per_year$Year), max(studies_per_year$Year), by = 2)
  ) +
  labs(
    title = "",
    x = "Year",
    y = "Number of Studies"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
dev.off()


# -------------

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
    fill_color = ifelse(Geo_general %in% c("Global", "Multicountry", "In silico"), "#BCA888", "royalblue")
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

# Map - removing studies with global or vague regional mentions (keeping only named countries)

country_hits <- df %>%
  distinct(Number, .keep_all = TRUE) %>% 
  filter(!is.na(Geo)) %>% 
  separate_rows(Geo, sep = ", ") %>% 
  filter(!Geo %in% c('In silico', "Global", "Neotropical", "Neotropics and Europe", "North Africa", "Europe")) %>% # Exclude vague regions
  group_by(Geo) %>%
  summarize(Hits = n()) %>%
  ungroup()

tail(country_hits)


world <- ne_countries(scale = "medium", returnclass = "sf")

world$name

map_data <- world %>%
  left_join(country_hits, by = c("name" = "Geo"))

# Export fig
jpeg(filename = 'Figure_S4.jpg', res = 400, units = 'cm', width = 14, height = 10 )

ggplot(map_data) +
  geom_sf(aes(fill = Hits), color = "gray70", size = 0.2) +
  scale_fill_viridis_c(
    option = "plasma",
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