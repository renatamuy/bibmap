# Renata 2024
# Pre-screening to plan screening strategy
# Sreening strategy proposed: start with narrower, then go broader
#-------------------------------------------------------------------------------

# comprehensive search using bat OR Chiroptera AND network* OR graph* (1,856 articles)

broader <- read.csv('data//pre-screening/Scopus_26_09_2024.csv', sep=';') 

nrow(broader) 

# socio* OR socia* OR ecolog* AND bat OR chiroptera AND network* OR graph* (322 Scopus), and a complementary comprehensive search using bat OR Chiroptera AND network* OR graph* (1,856 articles)

narrower <- read.csv('data//pre-screening/Scopus_27_09_2024_with_social_keywords.csv', sep=';') 

nrow(narrower)

length(narrower$DOI %in% broader$DOI)

broader$Title[!broader$DOI %in% narrower$DOI]

broader_not_in_narrower <- data.frame(Title = broader$Title[!broader$DOI %in% narrower$DOI])

setwd('data/pre-screening')

write.table(broader_not_in_narrower, file='titles_broader_not_in_narrower.txt', row.names=F)

# Wordcould of broader

install.packages("wordcloud2")
install.packages("tm")         # For text mining
install.packages("RColorBrewer") # For colors

library(wordcloud2)
library(tm)
library(RColorBrewer)

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

#install.packages("webshot")
#install.packages("htmlwidgets")
#webshot::install_phantomjs()  

library(wordcloud2)
library(htmlwidgets)

wordcloud <- wordcloud2(word_data, color = brewer.pal(8, "Dark2"))
saveWidget(wordcloud, "figures/broader_wordcloud.html", selfcontained = TRUE)

# repeat workflow for narrower

narrower$Title <- gsub("[^[:alnum:][:space:]]", "", narrower$Title)

narrower$Title <- iconv(narrower$Title, from = "latin1", to = "UTF-8", sub = "")

corpus <- Corpus(VectorSource(narrower$Title))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))

# term matrix
tdm <- TermDocumentMatrix(corpus)

# matrix
m <- as.matrix(tdm)

# word frequencies
word_freqs <- sort(rowSums(m), decreasing = TRUE)

# df
word_data <- data.frame(word = names(word_freqs), freq = word_freqs)

# word cloud
wordcloud2(word_data, color = brewer.pal(8, "Dark2"))

# export
wordcloud <- wordcloud2(word_data, color = brewer.pal(8, "Dark2"))
saveWidget(wordcloud, "figures/narrower_wordcloud.html", selfcontained = TRUE)

#------------------------------------------- :)