# From DOI to network
# Renata Muylaert - 2024
#-------------------------------------------------------------------------------------------------------------

# Packages
#install.packages(c("rcrossref", "igraph", "dplyr"))

# Load
library(rcrossref)
library(igraph)
library(dplyr)
require(here)
require(ggrepel)
require(ggraph)
require(here)

setwd(here())

setwd('data')

list.files()

df <- xlsx::read.xlsx("bibmap_variables_prelim4.xlsx", sheetIndex = 1, startRow=1)

df <- df[1:133, 1:16]

head(df)

windowsFonts()

# Cumstom functions ------------------------------------------------------------------------------------------

# Retrieve author data from a DOI!
get_authors_from_doi <- function(doi) {
  # Use cr_works to retrieve metadata of the DOI
  article_metadata <- cr_works(dois = doi)
  
  # Check if there are authors listed
  if (!is.null(article_metadata$data$author)) {
    # Extract author names
    authors <- article_metadata$data$author
    
    # Return a vector of author names
    return(sapply(authors, function(a) paste(a$given, a$family)))
  } else {
    return(NULL)
  }
}

#  last name only (optional for aesthetics)
extract_last_name <- function(name) {
  sapply(strsplit(name, " "), tail, 1)
}

# last name and initials only
extract_last_name_initials <- function(name) {
  parts <- strsplit(name, " ")[[1]]                       
  initials <- paste(substr(parts[1:(length(parts) - 1)], 1, 1), collapse = ".")  
  last_name <- tail(parts, 1)                            
  return(paste(last_name, paste0(initials, "."), sep = " "))       
}

#-------------------------------------------------------------------------------------------------------------


# DOI vector input

dois <- df$DOI

dois

# create receiving object
author_edges <- list()

for (doi in dois) {
  authors <- get_authors_from_doi(doi)
  
  # If there are at least two authors, create edges
  if (!is.null(authors) && length(authors) > 1) {
    # Create all possible pairs of authors for this article
    pairs <- combn(authors, 2)
    
    # Append pairs to edges list
    author_edges <- append(author_edges, split(pairs, col(pairs)))
  }
}

# Check edges list (characters)
str(author_edges)
author_edges

#  edges to df
edges_df <- do.call(rbind, lapply(author_edges, function(edge) {
  data.frame(from = edge[1], to = edge[2])
}))

# Correct names
edges_df$from <- sapply(edges_df$from, extract_last_name_initials)
edges_df$to <- sapply(edges_df$to, extract_last_name_initials)

# Check manually

view(edges_df)

xlsx::write.xlsx(edges_df, "edges_df.xlsx")

# Mello M.A.


# df to graph
author_network <- graph_from_data_frame(d = edges_df, directed = FALSE)

# Check 
author_network


# plot network
set.seed(123)

# Export
setwd('../figures')

# bad viz with reg plot 

plot(author_network, 
  vertex.size = 8,    
  vertex.shape = "circle",
  vertex.label.cex = 0.9,                 
  vertex.label.color = "black",         
  vertex.color = "lightblue",   
  vertex.label.family= "Arial", vertex.label.cex= .55,
  vertex.frame.color = "white",             
  edge.color = adjustcolor("gray", alpha.f = 0.5), 
  edge.width = 2, 
  edge.curved=0.3,
  layout = layout.fruchterman.reingold, #layout_nicely,                    
  main = "Author Collaborations")

dev.off()

#cluster

cluster <- cluster_louvain(author_network)

length(unique(cluster$membership))

V(author_network)$color <- cluster$membership

degree_values <- degree(author_network)  

table(degree_values)
summary(degree_values)

# Collaboration number as colour
#V(author_network)$color <- ifelse(degree_values > 7, "firebrick", "steelblue")

# repel and gggraph is better

setwd('../figures')

set.seed(123)

jpeg(filename = 'Figure_04_louvain_clusters.jpg', res = 400, units = 'cm', width = 20, height = 20 )

ggraph(author_network, layout = "fr") +  
  geom_edge_link(aes(edge_alpha = 1), show.legend = FALSE) +  
  geom_node_point(aes(color = color), size = 5) +  # Color nodes based on degree
  geom_text_repel(aes(x = x, y = y, label = name),  
                  size = 2, box.padding = 0.5, point.padding = 0.5) +
  scale_color_identity() +  # Use the colors defined earlier
  theme_void() +  
  labs(title = "Author collaboration network")

dev.off()


#dd ---------------------------------------------------------------------------------------

degree_df <- data.frame(degree = degree_values)

arrange(degree_df,degree)

summary(degree_df$degree)

jpeg(filename = 'Figure_dd.jpg', res = 400, units = 'cm', width = 20, height = 20 )

ggplot(degree_df, aes(x = degree)) + 
  geom_histogram(binwidth = 1, fill = "royalblue", color = "black", alpha = 0.7) +
  labs(title = "", x = "Individual author collaborations", y = "Frequency") +
  theme_minimal()

dev.off()

#-------------------------------------------------------------------------------------
