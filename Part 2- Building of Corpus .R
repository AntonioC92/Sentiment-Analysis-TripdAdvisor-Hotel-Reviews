
library(tidyverse)
library(xml2)
library(rvest)
library(stringr)
library(rebus)
library(lubridate)
library(dplyr)
library(readr)


# Read web page
webpage <- read_html("https://www.tripadvisor.ie/Hotel_Review-g186605-d212567-Reviews-Mespil_Hotel-Dublin_County_Dublin.html") 
webpage
# Extract records info
results <- webpage %>% html_nodes(".oETBfkHU , ._3hDPbqWO")
results
# Building the dataset
records <- vector("list", length = length(results))
records
for (i in seq_along(results)) {
  dateOfStay <- str_c(results[i] %>% 
                        html_nodes("._34Xs-BQm") %>% 
                        html_text())
  reviewTitle <- str_sub(results[i] %>%
                           html_nodes(".glasR4aX")%>%
                           html_text())
  review <- str_sub(results[i] %>% 
                      html_nodes(".IRsGHoPm") %>% 
                      html_text())
  records[[i]] <- data_frame(dateOfStay = dateOfStay, reviewTitle = reviewTitle, review = review)
}
# use bind_rows function to add the rows in the records list generated to a dataframe
MespilHotel_DF <- bind_rows(records)
# Remove "Date of Stay: " from dateOfStay column
MespilHotel_DF$dateOfStay
MespilHotel_DF$dateOfStay <- str_sub(MespilHotel_DF$dateOfStay, 15, -1)
MespilHotel_DF

# Transform to datetime format - Is adding a date of the 20th to all for some reason?
MespilHotel_DF$dateOfStay <- mdy(MespilHotel_DF$dateOfStay)
mdy(MespilHotel_DF$dateOfStay)
ymd(MespilHotel_DF$dateOfStay)
# Export to csv
write_csv(MespilHotel_DF, "MespilHotel.csv")




##SECOND PART :  Build a search engine for Mespil hotel  http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("tm")


#1st step: imported the text file


text <- readLines(file.choose("MespilHotel.csv"))

#removed irrelevant line of the first line comprising headers
text
finaltext <- text[-1]
finaltext

review1 <-finaltext[1]
review2 <-finaltext[2]
review3 <-finaltext[3]
review4 <-finaltext[4]
review5 <-finaltext[5]


# loaded the data as a corpus


review.list <- list(review1,review2,review3, review4, review5)
N.reviews <- length(review.list) #5
names(review.list) <- paste0("doc", c(1:N.reviews))


review.list #view the list
######

#set a pretend query

query <- "Family friendly hotel dublin city centre"

docs_Mespil <- VectorSource(c(review.list, query))

docs_Mespil$Names <- c(names(review.list), "query")

mespil_corpus <- Corpus(docs_Mespil)

inspect(mespil_corpus)



docs_Mespil #query added to the corpus



#2nd step: text transformation with the tmap function

getTransformations()



## ------------------------------------------------------------------------
#removePumctuation
mespil_corpus <- tm_map(mespil_corpus, removePunctuation)

mespil_corpus

## If we don't want to count similar words as  separate words, we will use the stem document
#transformation to implement the porter stemmer algorithm


mespil_corpus <- tm_map(mespil_corpus, stemDocument)



## ------------------------------------------------------------------------
mespil_corpus <- tm_map(mespil_corpus, removeNumbers)
mespil_corpus <- tm_map(mespil_corpus, tolower)
mespil_corpus <- tm_map(mespil_corpus, stripWhitespace)
mespil_corpus <- tm_map(mespil_corpus, removeWords, stopwords("english"))
mespil_corpus <- tm_map(mespil_corpus, removeWords, c("and", "are", "...")) 
mespil_corpus <- tm_map(mespil_corpus, removePunctuation)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
mespil_corpus <- tm_map(mespil_corpus, toSpace,"\U0001f4ab")

inspect(mespil_corpus)  #cleaning completed



#########3 APPLICATION OF VECTOR SPACE MODEL 

# represent each document as a vector and use the angle between  the vectors as a similarity measure.
#Rank by the similarity of each document to the query and you have a search engine.



## 
#One of the simplest things we can do is to count words within documents.
#This naturally forms a two dimensional structure, the term document matrix,
#with rows corresponding to the words and the columns corresponding to the documents. 
#As with any matrix, we may think of a term document matrix as a collection of column vectors existing in a space defined by the rows. 
#The query lives in this space as well, though in practice we wouldn’t know it beforehand.


term.doc.matrix.stm <- TermDocumentMatrix(mespil_corpus)
inspect(term.doc.matrix.stm[0:14, ]) #thw
#The matrices in tm are of type Simple Triplet Matrix, where only the triples are stored for non-zero values
#<<TermDocumentMatrix (terms: 14, documents: 6)
#Non-/sparse entries: 24/60
#Sparsity: 71%
#Maximal term length: 14
#Weighting          : term frequency (tf)


term.doc.matrix <- as.matrix(term.doc.matrix.stm)

options(max.print = 10000)
term.doc.matrix 
#the dimension of the document space are simple term frequencies (tf)
#rather than a linear increase in the term frequency (tf), 
#perhaps (sqrt(tf) or log(tf) would provide a more reasonable diminishing returns 
#on word counts within documents.

cat("Dense matrix representation costs", object.size(term.doc.matrix), "bytes.\n", 
    "Simple triplet matrix representation costs", object.size(term.doc.matrix.stm), 
    "bytes.") 
# Not considering document 6 which is the query, the word hotel appears in 3 reviews, the word room in 4, staff as well, dublin in 2


#a word's document frequency df is the number of documents that contain it,
# and a natural choice is to weigh those words inversely proportional to their (df)s
# Choice and implementation of Weights

#For both the document and query, we choose tfidf weights of ((1 + log2(tf)) ∗ log2(N/df)), which are defined to be (0) if (tf = 0).
#Note that whenever a term does not occur in a specific document, or when it appears in every document, its weight is zero. 
#We implement this weighting function across entire rows of the term document matrix, 
#and therefore our tfidf function must take a term frequency vector and a document frequency scalar as inputs.




get.tf.idf.weights <- function(tf.vec, df) {
  # Computes tfidf weights from a term frequency vector and a document
  # frequency scalar
  weight = rep(0, length(tf.vec))
  weight[tf.vec > 0] = (1 + log2(tf.vec[tf.vec > 0])) * log2(N.reviews/df)
  weight
}


cat("A word appearing in 3 of 5 documents, occurring 2,1 and 1 time respectively: \n", 
    get.tf.idf.weights(c(2, 0, 1, 0, 1), 3)) #to be impproved once we have all the docs for the hotels
#output :  Hotel A word appearing in 3 of 6 documents, occurring 3,1,3 and 1 times respectively, with a weighted assigned of -4.097069 0 -1.584963 -4.097069 0 -1.584963


# Using apply, we run the tfidf weighting function on every row of the term document matrix.
#The document frequency is easily derived from each row by the counting the non-zero entries (not including the query).

get.weights.per.term.vec <- function(tfidf.row) {
  term.df <- sum(tfidf.row[1:N.reviews] > 0)
  tf.idf.vec <- get.tf.idf.weights(tfidf.row, term.df)
  return(tf.idf.vec)
}

tfidf.matrix <- t(apply(term.doc.matrix, c(1), FUN = get.weights.per.term.vec))
colnames(tfidf.matrix) <- colnames(term.doc.matrix)

tfidf.matrix[0:3, ]


## We can see that the cosine decreases from its maximum value of (1.0) as the angle departs from zero in either direction.
angle <- seq(-pi, pi, by = pi/16)
plot(cos(angle) ~ angle, type = "b", xlab = "angle in radians", main = "Cosine similarity by angle")


#We may furthermore normalize each column vector in our tfidf matrix so that its norm is one. 
#Now the dot product is (cos θ).
tfidf.matrix <- scale(tfidf.matrix, center = FALSE, scale = sqrt(colSums(tfidf.matrix^2)))
tfidf.matrix[0:3, ]


## Matrix Multiplication: a dot product machine by assuming the query was not there


query.vector <- tfidf.matrix[, (N.reviews + 1)]
tfidf.matrix <- tfidf.matrix[, 1:N.reviews]


## With the query vector and the set of document vectors in hand, it is time to go after the cosine similarities. 
#These are simple dot products as our vectors have been normalized to unit length. 

#Recall that matrix multiplication is really just a sequence of vector dot products. 
#The matrix operation below returns values of (cos θ) for each document vector and the query vector.
doc.scores <- t(query.vector) %*% tfidf.matrix


## #With scores in hand, rank the documents by their cosine similarities with the query vector.
results.df <- data.frame(doc = names(review.list), score = t(doc.scores), text = unlist(review.list))
results.df <- results.df[order(results.df$score, decreasing = TRUE), ]


## results as a basic search engine

options(width = 2000)
print(results.df, row.names = FALSE, right = FALSE, digits = 2)

#Interpretation:  
#the best “best” document, at least in an intuitive sense,is the first one with a value of 0.17 which contained almost all the keywords of the query and repeateadly the word dublin,
#, followed by review 3 with 0.09 which containes the word frim the query fewer times. Rareness of the other word make the others rank lower dueto che choice to 
#incorporate the inverse document frequency weighting for both documents and query. Fortunately, the uninformative document with only the word dublin  ranked last

#### END OF SEARCH ENGINE





