# bibmap
# This repo is a supplement to the manuscript:
# Muylaert et al., in prep. Connections in the Dark: Network Science and 
# Social-Ecological Networks as Tools for Bat Conservation and Public Health.
# Global Union of Bat Diversity Networks (GBatNet).
# See README for further info:
# https://github.com/renatamuy/bibmap/blob/main/README.md


# Renata 2024
# Pre-screening to plan screening strategy
# Sreening strategy proposed: start with narrower, then go broader


#Let's get ready for running the code provided here. 

#Set the working directory to the source of this script file.   
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#Delete all previous objects.
rm(list= ls())

#Load or install the required packages
if(!require(devtools)){
  install.packages("devtools")
  library(devtools)
}

if(!require(htmlwidgets)){
  install.packages("htmlwidgets")
  library(htmlwidgets)
}

if(!require(RColorBrewer)){
  install.packages("RColorBrewer")
  library(RColorBrewer)
}

if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}

if(!require(tm)){
  install.packages("tm")
  library(tm)
}

if(!require(tm)){
  install.packages("tm")
  library(tm)
}

if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}

if(!require(webshot)){
  install.packages("webshot")
  library(webshot)
}

if(!require(wordcloud2)){
  install.packages("wordcloud2")
  library(wordcloud2)
}


# comprehensive search using bat OR Chiroptera AND network* OR
# graph* (1,856 articles)
broader <- read.csv("../data/pre-screening/Scopus_26_09_2024.csv", sep=';') 

nrow(broader) 

# socio* OR socia* OR ecolog* AND bat OR chiroptera AND network* OR graph*
# (322 Scopus), and a complementary comprehensive search using bat OR
# Chiroptera AND network* OR graph* (1,856 articles)
narrower <- read.csv("../data/pre-screening/Scopus_27_09_2024_with_social_keywords.csv", sep=';') 

nrow(narrower)

length(narrower$DOI %in% broader$DOI)

broader$Title[!broader$DOI %in% narrower$DOI]

broader_not_in_narrower <- data.frame(Title = broader$Title[!broader$DOI %in% 
                                                        narrower$DOI])

write.table(broader_not_in_narrower, file="../data/pre-screening/titles_broader_not_in_narrower.txt", row.names=F)

# Wordcould of broader

# Create a text corpus

# remove special characters from title

broader$Title <- gsub("[^[:alnum:][:space:]]", "", broader$Title)

# utf8

broader$Title <- iconv(broader$Title, from = "latin1", to = "UTF-8", sub = "")

# check title
broader$Title 

# corpus build
corpus <- Corpus(VectorSource(broader$Title))

# rm lowercase, remove punctuation, numbers, and stopwords
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))

# term-document matrix
tdm <- TermDocumentMatrix(corpus)

# matrix
m <- as.matrix(tdm)

# word frequencies
word_freqs <- sort(rowSums(m), decreasing=TRUE)

# df this
word_data <- data.frame(word = names(word_freqs), freq = word_freqs)

# word cloud
wordcloud2(word_data, color = brewer.pal(8, "Dark2"))

# export
wordcloud <- wordcloud2(word_data, color = brewer.pal(8, "Dark2"))
saveWidget(wordcloud, "../figures/broader_wordcloud.html", selfcontained = TRUE)

# repeat workflow for narrower

narrower$Title <- gsub("[^[:alnum:][:space:]]", "", narrower$Title)

narrower$Title <- iconv(narrower$Title, from = "latin1", to = "UTF-8", sub = "")

corpus <- Corpus(VectorSource(narrower$Title))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))

tdm <- TermDocumentMatrix(corpus)

m <- as.matrix(tdm)

word_freqs <- sort(rowSums(m), decreasing = TRUE)

word_data <- data.frame(word = names(word_freqs), freq = word_freqs)

wordcloud2(word_data, color = brewer.pal(8, "Dark2"))

wordcloud <- wordcloud2(word_data, color = brewer.pal(8, "Dark2"))
saveWidget(wordcloud, "../figures//narrower_wordcloud.html",
           selfcontained = TRUE)

################################################################################