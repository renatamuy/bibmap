# From DOI to network
# Renata Muylaert - 2024
#-------------------------------------------------------------------------------------------------------------

# Packages
#install.packages(c("rcrossref", "igraph", "dplyr"))

# Load
library(rcrossref)
library(igraph)
library(dplyr)

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

dois <- c(
  "10.1038/s41559-019-1002-3",
  "10.3390/v16071154",
  "10.1007/s40823-024-00096-3",
  "10.1371/journal.pcbi.1010362")


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

# Check 
edges_df

# df to graph
author_network <- graph_from_data_frame(d = edges_df, directed = FALSE)

# Check 
author_network

# plot network
set.seed(123)

# Export

png(filename = 'figures/collab_network.png', res = 300, units = 'cm', width = 20, height = 20 )

plot(author_network, 
  vertex.size = 10,    
  vertex.shape = "circle",
  vertex.label.cex = 0.9,                 
  vertex.label.color = "black",         
  vertex.color = "lightblue",   
  vertex.label.family= "Arial", vertex.label.cex= .55,
  vertex.frame.color = "white",             
  edge.color = adjustcolor("gray", alpha.f = 0.5), 
  edge.width = 2, 
  edge.curved=0.3,
  layout = layout_nicely,                    
  main = "Author Collaborations")

dev.off()

#-------------------------------------------------------------------------------------
