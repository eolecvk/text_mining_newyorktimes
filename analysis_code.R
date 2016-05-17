# Text mining and topic analysis of New York Times articles

# Make sure you run the commands in the 
# file 20151104_TextMining_functions.R
# before you run these commands. 

# Load the required libraries
library(cluster)
library(RCurl)
library(RJSONIO)
library(rlist)
library(stringr)
library(dplyr)
library(magrittr)
library(RTextTools)
library(tm)
library(ngram)

setwd("/home/eolus/Dropbox/MA710 Perso/ASSG2/Workspace")
load("base_functions.RData")
load("custom_functions.RData")
load("article_raw.RData")
load('drop_word.RData')


SOURCE_LIST <- c(
  'headline',        #1
  'snippet',         #2
  'lead_paragraph',  #3
  'abstract'         #4
)

### DATA SOURCING

data_source = SOURCE_LIST[2]

if (data_source == SOURCE_LIST[1]){
  docs = article.df$headline
} else if (data_source == SOURCE_LIST[2]){
  docs = article.df$snippet
} else if (data_source == SOURCE_LIST[3]){
  docs = article.df$lead_paragraph
} else if(data_source == SOURCE_LIST[4]){
  docs = article.df$abstract
} else {
  stopifnot(1 ==0)
}


### SOURCE CLEANING
### ****************************

#                                snippet:
# Remove empty, NULL, blank cells: length(docs) // 1010 => 998
docs <- docs[empty.doc.logical(docs)]

# Remove duplicate documents:      length(docs) // 998 => 931
docs <- dedupe(docs)

# Remove irrelevant cells: elements with followin sub-strings
#                                length(docs) // 931 => 916
if (data_source == SOURCE_LIST[1]){
  drops_strings = headline_drops_strings
} else if (data_source == SOURCE_LIST[2]){
  drops_strings = snippet_drops_strings
}

docs <- drop(drops_strings, docs)

# Perform synonym expansion
docs.clean <- synonym.expansion.documents(docs)

# Remove punctuation, apostrophes and blanks
docs.clean <- clean.document.punctuation(docs.clean)

# Vectorize the documents
# document.vector <- vectorize.documents(docs)
#document.vector <- vectorize.documents(docs)
document.vector <- remove.stop.words(docs.clean, STOP_WORDS)

document.vector <- remove.short.word(document.vector)

document.vector <- stem.words(document.vector)




document.vector <- vec.to.string(document.vector)




# Cleaning round#1
# Remove stop words // Collections =  (SMART) + (Custom)
#document.vector <- remove.stop.words(document.vector, STOP_WORDS)
# Remove words less than 2 char long
#document.vector <- remove.short.word(document.vector)
# Perform stemming
#document.vector <- stem.words(document.vector)

# Cleaning round#2 (post stemming)
# Remove stop words // Collections =  (SMART) + (Custom)
document.vector <- remove.stop.words(document.vector, STOP_WORDS)
# Remove words less than 2 char long
document.vector <- remove.short.word(document.vector)
# Remove empty terms: eg: c("","","house") -> c("house")
document.vector <- remove.empty.terms(document.vector)
# If a document contains only one term change value of term to: "".
document.vector <- nullify.one.word.document(document.vector)
# Remove empty terms:
document.vector <- remove.empty.terms(document.vector)

# Convert document vectors to document strings
document.vector <- vec.to.string(document.vector)
# Perform synonym expansion
document.vector <- unlist(synonym.expansion.documents(document.vector))

# Remove empty documents from document.vector and update docs accordingly
non.empty <- empty.doc.logical(document.vector)

document.vector <- document.vector[non.empty]
docs.clean <- docs.clean[non.empty]
docs <- docs[non.empty]


### DOCUMENT REPRESENTATION
### ***************************************************

# Create the document matrix
doc.matrix <- 
  create_matrix(unlist(document.vector), 
                weighting=tm::weightTf   # OPTION: weighting (see below)
  )

# Weighting OPTIONS:
# tm::weightTfIdf - term frequency-inverse document frequency
# tm::weightTf    - term frequency
# To use binary weighting use tm::weightTf and create a "binary matrix" below.


# Create the document-term matrix
dtm = as.matrix(doc.matrix) 

# OPTION: create a binary matrix
# in order to use binary weighting.
# DO NOT run this code if you 
# DO NOT want to use binary weighting.
# Only use with parameter
#     weighting=tm::weightTf 
# All positive frequencies become 1,
# indicating only the presence of a word  
# in a  document. 
# Uncomment the following line to use
# this code if you decide to use it. 
dtm[dtm>1]=1 

# This may not make much of a difference 
# as nearly all document-word frequencies 
# are equal to 1. Most duplicate words are
# stopwords, and those have been removed. 

# Check the distribution of document-word frequencies 
# if you created a binary document-word matrix above
table(dtm)

# Check the distribution of word frequencies 
# table(colSums(dtm))

# This gives the distribution of word frequencies 
# for the entire collection of articles

# OPTION: frequency threshold
# Keep words from the document term matrix
# that occur at least the number of times
# indicated by the `freq.threshold` parameter 

dtm =reduce.dtm(dtm,freq.threshold=2) 


save(file = '/home/eolus/Dropbox/MA710 Perso/ASSG2/Workspace/cleaned_data.RData',
     list = c('dtm','docs'))

load('/home/eolus/Dropbox/MA710 Perso/ASSG2/Workspace/cleaned_data.RData')
# **************************************************
# ********** CLUSTER PARAMETERS ********************
# **************************************************

library(clValid)
 
dtm.clValid = clValid(dtm.grams[,],
                        nClust=c(20, 30, 50, 70, 80, 90),
                        clMethods=c("kmeans","pam"),
                        validation='internal',
                        maxitems = 1000
                        )
  
 
cluster_count_analysis <- summary(dtm.clValid)




# Check the number of columns/words 
# remaining in the document-term matrix
ncol(dtm)

# OPTION: number of clusters to find
k = 30

load('/home/eolus/Dropbox/MA710 Perso/ASSG2/Workspace/clusters_centers.RData')
# x1 <- kmeans(dtm, k)$center
# x2 <- pam(dtm, k)$center

x1 = kmeans(dtm, 50)$center
x2 = kmeans(dtm, 30)$center
# OPTION: cluster algorithm 
cluster_kmeans = kmeans(dtm, centers=x1)$cluster
cluster_kmeans2 = kmeans(dtm, centers=x2)$cluster

# cluster_pam = pam(dtm, k)$cluster
# hclust.res = hclust(dist(dtm))
# cluster = cutree(hclust.res,k)

# EVALUATE the clusters using `table` 
# to check the cluster sizes
# pam_cluster_freq <- as.data.frame(table(cluster_pam))


# EVALUATE the clusters using `check.cluster` 
# to look at the common words in each cluster
# The second parameter is the minimum number of
# rows that a cluster must have to be displayed.
options(warn=-1)
check.clusters(cluster_kmeans.grams, 10) 
options(warn=0)


# EVALUATE the clusters by looking 
# at the documents in the clusters

view.cluster(1)
TopWords(dtm, cluster_kmeans, 1)



# End

# Saving outputs
save(file = '/home/eolus/Dropbox/MA710 Perso/ASSG2/Workspace/clusters_centers.RData',
     list = c('x1','x2'))
save(file = '/home/eolus/Dropbox/MA710 Perso/ASSG2/Workspace/outputs.RData',
     list = c('cluster_kmeans', 'cluster_kmeans2','dtm'))
