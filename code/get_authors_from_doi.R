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