library(bitops)
library(RCurl)
library(RJSONIO)
library(rlist)
library(stringr)
library(dplyr)
library(magrittr)
library(RTextTools)
library(tm)
library(ngram)

options(dplyr.print_min=100)

## Define functions

# Create a single row data frame from a list
dfrow.from.list = function(aList) { 
  data.frame(rbind(unlist(aList)),
             stringsAsFactors=FALSE)
}

# Retrieve a single page (with 10 articles) from the New York Times
get.nyt.page = function(page=0,          # page number (default: 0)
                        query.string="", # single word only (REQUIRED)
                        begin.date="",   # yyyymmdd (REQUIRED)
                        end.date=""      # yyyymmdd (REQUIRED)
) { # page=0; query.string="sports"; begin.date="20010101"; end.date="20151231"
  str_c(# create query string to send to NYT
    "http://api.nytimes.com", 
    "/svc/search/v2/articlesearch.json",
    "?api-key=",    articlesearch.key,
    "&q=",          str_replace_all(query.string," ","%20"),
    "&begin_date=", begin.date,
    "&end_date=",   end.date,
    "&page=",       page
  ) %>%
  {Sys.sleep(0.1); .} %>%    # wait 0.1s (rate limit 10/s)
    getURL() %>%             # retreive data from NYT
    fromJSON() %>%           # convert from JSON to an R list
    { .$response$docs } %>%  # retrieve only the documents
    list.select(             # keep only these four fields
      headline=as.character(headline["main"]), 
      snippet, 
      lead_paragraph, 
      abstract,
      pub_date) %>% 
    lapply(dfrow.from.list) %>% # convert each list item to a dataframe
    bind_rows                   # create a single dataframe
}

get.nyt.articles = function(pages=0, # vector of page numbers
                            query.string="", # single word only (REQUIRED)
                            begin.date="",   # yyyymmdd (REQUIRED)
                            end.date=""      # yyyymmdd (REQUIRED)
) { 
  lapply(pages, 
         get.nyt.page, 
         query.string=query.string,
         begin.date=begin.date,
         end.date=end.date
  ) %>% 
    bind_rows()
}


# Clean documents
clean.documents = function (document.vector) {
  
  document.vector %>% # document.vector = docs[93:94]
    tolower() %>%                           # change to lower case
    str_replace_all("'s","")            %>% # remove "'s"
    str_replace_all("’s","")            %>% # remove "’s"
    str_replace_all("\\.","")           %>% # remove periods
    str_replace_all("[[:digit:]]+"," ") %>% # change numbers to a space
    str_replace_all("[[:punct:]]"," ")  %>% # change punctuation to a space
    str_replace_all("[[:blank:]]+"," ") %>% # change white space to a space
    str_trim(side = "both")             %>% # remove spaces at the ends
    
    {. } -> clean_doc
  
  return(clean_doc)
}

# Create strings of n-grams
modify.words = function(document.vector, stem.words=FALSE, ngram.vector=1, stop.words=c()) {
  document.vector %>% # document.vector = docs.clean
    str_split("[[:space:]]") %>%            
    lapply(function(x) setdiff(x,stop.words)) %>%
    { if(stem.words) lapply(., wordStem) 
      else . 
    } %>% 
    lapply(function(x) { 
      ngrams(x,ngram.vector) %>%
        lapply( function(x) paste(x,collapse=".")) %>% 
        paste(collapse=" ") 
    })
}

# create.ngrams.remove.stopwords = function(document.vector, ngram.vector, stop.words) {
#   document.vector %>% # document.vector = docs.clean
#     str_split("[[:space:]]") %>%            # string to vector of words
#     lapply(wordStem) %>%                    # stem words (OPTION)
#     lapply(function(word.vector) {          # for each word.vector
#       word.vector %>%
#         match(stop.words,nomatch=FALSE) %>% # remove stop words
#         `!` %>%                             # remove stop words
#         { word.vector[.] } %>%              # remove stop words
#         ngrams(ngram.vector) %>%            # create n-grams
#         sapply(function(word.vec) {         # create n-gram words
#           paste(word.vec,collapse=".") 
#         }) %>%
#         paste(collapse=" ")                 # create string of n-grams
#     }) %>%
#     as.character() 
# }

reduce.dtm = function (dtm, freq.threshold) {
  word.counts=colSums(dtm)
  new.columns = names(word.counts)[freq.threshold<=word.counts]
  dtm[,new.columns]
}

# List the ten most common words in cluster i
TopWords = function (dtm, clusters, i) { # clusters=res$cluster; i=1
  dtm_names = colnames(dtm)
  row_count = sum(clusters==i)
  dtm_csums =
    apply(matrix(dtm[clusters==i,], nrow=row_count),
          2,
          mean)
  names(dtm_csums) = dtm_names
  dtm_ndx = order(dtm_csums, decreasing=TRUE)[1:10]
  bind_rows(
    data.frame(word=paste(c("[cluster ",formatC(i, format="f", digits=0),"]"), collapse=""),avg=NA),
    data.frame(word=paste(c("[",formatC(row_count, format="f", digits=0)," records]"), collapse=""),avg=NA),
    data.frame(word=dtm_names[dtm_ndx], avg=dtm_csums[dtm_ndx])
  )
}

check.clusters = function(cluster, count.min) { # cluster=res$cluster; count.min=3
  cluster.counts = table(cluster)
  as.numeric(names(cluster.counts)[cluster.counts >= count.min]) %>%
    lapply(function(clnum) { # clnum=1
      TopWords(dtm,cluster,clnum) 
    }) %>%
    bind_cols()
} 

view.dtm = function(cluster.number) {
  docs[res$cluster==cluster.number]
}

view.cluster = function(cluster.number) {
  docs[cluster_kmeans2==cluster.number]
}

#
# Define functions (END)
base_functions <- c(
  'dfrow.from.list',
  'get.nyt.page',
  'get.nyt.articles',
  'clean.documents',
  'modify.words', 
  'reduce.dtm',
  'TopWords',
  'check.clusters',
  'view.dtm',
  'view.cluster'
)

setwd("/home/eolus/Dropbox/MA710 Perso/ASSG2/Workspace")
save(file = '/home/eolus/Dropbox/MA710 Perso/ASSG2/Workspace/base_functions.RData',
     list = base_functions)


# INPUT_TYPE: List of documents
# Drops all the elements in `from_doc` containing any of the sub-strings
# listed in the `drops_string` vector
drop <- function(drops_string, from_doc){
  drops_regex <- paste("(",drops_string[1], sep="")
  for(i in drops_string[2:length(drops_string)] ){
    drops_regex <- paste(drops_regex, i, sep = ")|(")
  }
  drops_regex <- paste(drops_regex,")", sep="") 
  lapply(from_doc, function(x) grepl(drops_regex, x, ignore.case = TRUE)) %>%
    unlist() %>%
    { . } -> drops_position
  return(from_doc[!drops_position])
}


# INPUT_TYPE: List of documents
# Punctuation, blanks and apostrophe
clean.document.punctuation = function (documents) {
  
    documents                           %>%
    tolower()                           %>% # change to lower case
    str_replace_all("'s","")            %>% # remove "'s"
    str_replace_all("’s","")            %>% # remove "’s"
    str_replace_all("\\.","")           %>% # remove periods
    str_replace_all("[[:digit:]]+"," ") %>% # change numbers to a space
    str_replace_all("[[:punct:]]"," ")  %>% # change punctuation to a space
    str_replace_all("[[:blank:]]+"," ") %>% # change white space to a space
    str_trim(side = "both")             %>% # remove spaces at the ends
    
    {. } -> clean_doc
  
  return(clean_doc)
}


# INPUT_TYPE: List of documents
# Dedupe the entries
dedupe = function(documents){
  documents[!duplicated(documents)] %>%
  na.omit() %>%
  { . } -> deduped.docs
  
  return(deduped.docs)
}

# INPUT_TYPE: List of documents
# Transform vector of document string into vector of vectors of terms
vectorize.documents = function(documents){
  documents %>%
    tolower()  %>% 
    str_split("[[:space:]]") %>% 
    { . } -> document.vector
  
  return(document.vector)
}


# INPUT_TYPE: List of vectors
# Filter out stop words from vectors of terms
remove.stop.words = function(document.vector, stop.words){
  document.vector %>%
    tolower() %>%
    str_split("[[:space:]]") %>% 
  lapply(function(x) setdiff(x, stop.words))  %>% 
  { . } -> document.vector.no.stop.words

return(document.vector.no.stop.words)
}


# INPUT_TYPE: List of documents
synonym.expansion.documents <- function (document.vector) {
  
  document.vector %>%
    lapply(.,  function(x){
      x %>%
        tolower() %>%                           # change to lower case
        
        # Cybernation semantic
        str_replace_all("automation","automat")         %>%
        str_replace_all("automatic","automat")          %>%
        str_replace_all("automated","automat")          %>%
        str_replace_all("automates","automat")           %>%
        str_replace_all("automate","automat")           %>%
        str_replace_all("automating","automat")         %>%
        str_replace_all("automatization","automat")     %>%
        str_replace_all("automatize","automat")         %>%
        str_replace_all("automatizing","automat")       %>%
        str_replace_all("automatd","automat")           %>%
        str_replace_all("automatons","automat")         %>%
        str_replace_all("automaton","automat")          %>%
        str_replace_all("autonomy","autonom")           %>%
        str_replace_all("autonomously","autonom")       %>%
        str_replace_all("autonomous","autonom")         %>%
        str_replace_all("smart","intelligent")          %>%
        str_replace_all("clever","intelligent")         %>%
        str_replace_all("brilliant","intelligent")      %>%
        str_replace_all("intelligence","intelligent")   %>%
        str_replace_all("brainy","intelligent")         %>%
        str_replace_all("humanlike","android")          %>%
        str_replace_all("humanlik","android")          %>%
        
        
        # Cities, countries
        str_replace_all("new york times","nyt")         %>%
        str_replace_all("new york city","nyc")          %>%
        str_replace_all("new york","nyc")               %>%
        str_replace_all("york","nyc")                   %>%
        str_replace_all("chinese","china")              %>%
        str_replace_all("chines","china")               %>%
        str_replace_all("japanese","japan")             %>%
        str_replace_all("tokyo","japan")                %>%
        str_replace_all("beijing","china")              %>%
        str_replace_all("americans","usa")              %>%
        str_replace_all("american","usa")               %>%
        str_replace_all("america","usa")                %>%
        str_replace_all("malaysian","malaysia")         %>%
        
        # Entertainment
        str_replace_all("animated","animation")         %>%
        str_replace_all("trailer","cinema")             %>%
        str_replace_all("movies","cinema")               %>%
        str_replace_all("movie","cinema")                %>%
        str_replace_all("films","cinema")               %>%
        str_replace_all("film","cinema")                %>%
        str_replace_all("theater","cinema")             %>%
        str_replace_all("superhero","hero")             %>%
        str_replace_all("reader","book")                %>%
        str_replace_all("kids","child")                 %>%
        str_replace_all("kid","child")                  %>%
        str_replace_all("children","child")             %>%
        
        # Industry
        str_replace_all("jobs","labor")                 %>%
        str_replace_all("job","labor")                  %>%
        str_replace_all("businesses","business")        %>%
        str_replace_all("compani","business")           %>%
        str_replace_all("corporation","business")       %>%
        str_replace_all("corp","business")              %>%
        
        str_replace_all("alphabet","google")            %>%
        str_replace_all("amazoncom","amazon")           %>%
        
        # Science
        str_replace_all("researcher","scientist")       %>%
        str_replace_all("studi","research")             %>%
        
        # Car
        str_replace_all("automakers","car manufacturer")%>%
        str_replace_all("automaker","car manufacturer") %>%
        str_replace_all("vehicles","car")               %>%
        str_replace_all("vehicle","car")                %>%
        str_replace_all("vehicl","car")                 %>%
        str_replace_all('selfdriving',"selfdrive")     %>%
        str_replace_all("self driving","selfdrive")     %>%
        str_replace_all("driverless","selfdrive")       %>%
        str_replace_all("driver","drive")               %>%
        str_replace_all("driving","drive")              %>%
        
        # Sea
        str_replace_all("oceanic","marine")             %>%
        str_replace_all("ocean","marine")               %>%
        str_replace_all("underwater","marine")          %>%
        
        # Space
        str_replace_all("international space station","iss")  %>%
        str_replace_all("space station","iss")          %>%
        
        #Misc
        str_replace_all("empathetic","empathy")         %>%
        
        {. } -> synonymized.doc
      
      return(synonymized.doc)
      
    }) %>%
    { . } -> synomized.doc.collection
      
    return(unlist(synomized.doc.collection))
}

# INPUT_TYPE: List of vectors
# Change document-vector to document-vector w n-grams
make.grams = function(docs, ngram.vector=1){
  docs %>%
    str_split("[[:space:]]") %>%  
    lapply(function(x) { 
      ngrams(x,ngram.vector) %>%
        lapply( function(x) paste(x,collapse=".")) %>% 
        paste(collapse=" ")
    }) %>%
    { . } -> document.vector.grams
  
  return(document.vector.grams)
}

# Stem terms of document vector
stem.words = function(document.vector){
  document.vector %>%
    lapply(., wordStem) %>%
    { . } -> document.vector.stemmed
  
  return(document.vector.stemmed)
}


vec.to.string = function(document.vector){
  document.vector <- lapply(document.vector, function(x) paste0(x, sep=" ", collapse=""))
  unlist(document.vector) %>%
    str_replace_all("[[:blank:]]+"," ") %>% # change white space to a space
    str_trim(side = "both")             %>% # remove spaces at the ends
    { . } -> document.string
  
  return(document.string)
}


# INPUT_TYPE: List of documents
empty.doc.logical = function(docs){
  docs %>%
    str_replace_all("[[:blank:]]"," ") %>%
    str_trim(side = "both")           %>%
    { . } -> docs
  
  non.empty.doc <-  !(docs %in% c(""," ", "NULL", NULL, NA))
  return(non.empty.doc)
}



# INPUT_TYPE: List of vectors
# Remove one-term document
nullify.one.word.document = function(document.vector){
  lapply(document.vector, function(x){if (length(x) > 1) {return(x)} else {return("")}}) %>%
    as.vector() %>%
    { . } -> document.vector
  
  return(document.vector)
}

remove.empty.terms = function(document.vector){
  document.vector %>%
    vec.to.string() %>%
    vectorize.documents() %>%
    {.} -> document.vector
  
  return(document.vector)
}


# INPUT_TYPE: List of vectors
# From each vector. remove terms than are less than x char in length
remove.short.word = function(document.vector, lengthMin = 3){
  document.vector %>%
    lapply(., function(x){unlist(lapply(x,function(y){
      if(nchar(y) >= lengthMin) {return(y)} else {return("")}
    }))}) %>%
    { . } -> document.vector
  
  return(document.vector)
}



# Define functions (END)
custom_functions <- c('clean.document.punctuation',
                      'dedupe',
                      'drop',
                      'make.grams',
                      'remove.stop.words',
                      'remove.short.word',
                      'remove.empty.terms',
                      'stem.words',
                      'nullify.one.word.document',
                      'synonym.expansion.documents',
                      'vectorize.documents',
                      'empty.doc.logical',
                      'vec.to.string')

setwd("/home/eolus/Dropbox/MA710 Perso/ASSG2/Workspace")
save(file = '/home/eolus/Dropbox/MA710 Perso/ASSG2/Workspace/custom_functions.RData',
     list = custom_functions)