# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

# ---------------- Lists the files ------------------------
file_paths <- list.files(path = 'files/',pattern = ".txt", full.names = TRUE)
file_paths

# ---------------- Loads the portuguese stopwords ------------------
pt_stopwords = readLines('util/stopwords.txt')

# ---------------- Fixes paths of file names -----------------------
file_paths = lapply(file_paths, function (x) sub(x = x, pattern = '//', replacement = '/'))
file_paths

# ---------------- Reads files content------------------------------
file_contents = lapply(X = file_paths, readLines)
file_contents

# ----------------- Gets the artists names -------------------------
artists = sapply(file_contents,function(x){return (x[2])})
artists

# ----------------- Get the songs ----------------------------------
songs = sapply(file_contents,function(x){return (x[1])})
songs

# ------------- Creates data frame with artists and songs ----------
top50_df = data.frame(Artist = artists, Song= songs)
top50_df
# Other way to create a data frame
# top50_df = data.frame(Artist = sapply(file_contents,function(x){return (x[2])}), Song = sapply(file_contents,function(x){return (x[1])}))

# ------------- Transform file_contents to vector --------------
text = unlist(file_contents)

# ------------ Creates a Corpus --------------------------------
corpusTexts <- Corpus(VectorSource(text))

# ------------ Documents transformations -----------------------
# To Lower Case
corpusTexts <- tm_map(corpusTexts, content_transformer(tolower))

# Remove whitespaces
corpusTexts <- tm_map(corpusTexts, stripWhitespace)

# Remove numbers
corpusTexts <- tm_map(corpusTexts, removeNumbers)

# Remove Punctuation
corpusTexts <- tm_map(corpusTexts, removePunctuation)

# Remove custom portuguese stopwords
corpusTexts <- tm_map(corpusTexts, removeWords, pt_stopwords)

# Remove defaults stopwords
corpusTexts <- tm_map(corpusTexts, removeWords, stopwords('pt'))
corpusTexts <- tm_map(corpusTexts, removeWords, stopwords('english'))

# Remove stem words
corpusTexts <- tm_map(corpusTexts, stemDocument)

# --------------- Generates the frequency table of the words -----------------
tdmTexts <- TermDocumentMatrix(corpusTexts)
matrixTexts <- as.matrix(tdmTexts)
vectorTextFrequence <- sort(rowSums(matrixTexts),decreasing=TRUE)
dfFrequence <- data.frame(word = names(vectorTextFrequence), freq = vectorTextFrequence)

# ---------------- Generates the wordcloud -----------------------------------
wordcloud(words = dfFrequence$word, freq = dfFrequence$freq,  max.words = 100, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))

# Other way to create a wordcloud
#wordcloud(corpusTexts, max.words = 100, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))


