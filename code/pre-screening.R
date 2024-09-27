# Renata 2024
# Support for starting with narrow screening

# comprehensive search using bat OR Chiroptera AND network* OR graph* (1,856 articles)

broader <- read.csv('data//pre-screening/Scopus_26_09_2024.csv', sep=';') 

nrow(broader) 

# socio* OR socia* OR ecolog* AND bat OR chiroptera AND network* OR graph* (322 Scopus), and a complementary comprehensive search using bat OR Chiroptera AND network* OR graph* (1,856 articles)

narrower <- read.csv('data//pre-screening/Scopus_27_09_2024_with_social_keywords.csv', sep=';') 

nrow(narrower)

length(narrower$DOI %in% broader$DOI)

broader$Title

# Wordcould of broader

install.packages("wordcloud2")
install.packages("tm")         # For text mining
install.packages("RColorBrewer") # For colors

library(wordcloud2)
library(tm)
library(RColorBrewer)


# Create a text corpus

# Remove special char from title

broader$Title <- gsub("[^[:alnum:][:space:]]", "", broader$Title)

# Convert to utf8

broader$Title <- iconv(broader$Title, from = "latin1", to = "UTF-8", sub = "")

# corpus build

corpus <- Corpus(VectorSource(broader$Title))

# Convert text to lowercase, remove punctuation, numbers, and stopwords
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))

# Create a term-document matrix
tdm <- TermDocumentMatrix(corpus)

# Convert to a matrix
m <- as.matrix(tdm)

# Get word frequencies
word_freqs <- sort(rowSums(m), decreasing=TRUE)

# Create a data frame with words and frequencies
word_data <- data.frame(word = names(word_freqs), freq = word_freqs)


# Generate the word cloud
wordcloud2(word_data, color = brewer.pal(8, "Dark2"))

# Export

#install.packages("webshot")
#install.packages("htmlwidgets")
#webshot::install_phantomjs()  

library(wordcloud2)
library(htmlwidgets)

# Save the word cloud as an HTML file
wordcloud <- wordcloud2(word_data, color = brewer.pal(8, "Dark2"))
saveWidget(wordcloud, "figures/broader_wordcloud.html", selfcontained = TRUE)

# Repeat workflow for narrower

narrower$Title <- gsub("[^[:alnum:][:space:]]", "", narrower$Title)

narrower$Title <- iconv(narrower$Title, from = "latin1", to = "UTF-8", sub = "")

# Create a text corpus from the Title column of narrower
corpus <- Corpus(VectorSource(narrower$Title))

# Convert text to lowercase, remove punctuation, numbers, and stopwords
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))

# Create a term-document matrix
tdm <- TermDocumentMatrix(corpus)

# Convert the term-document matrix to a matrix
m <- as.matrix(tdm)

# Get word frequencies by summing up occurrences
word_freqs <- sort(rowSums(m), decreasing = TRUE)

# Create a data frame with words and frequencies
word_data <- data.frame(word = names(word_freqs), freq = word_freqs)

# Generate the word cloud
wordcloud2(word_data, color = brewer.pal(8, "Dark2"))

# Save the word cloud as an HTML file
wordcloud <- wordcloud2(word_data, color = brewer.pal(8, "Dark2"))
saveWidget(wordcloud, "figures/narrower_wordcloud.html", selfcontained = TRUE)

#-------------------------------------------