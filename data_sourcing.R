# Set this so we can see all columns when printing dataframes
options(dplyr.width=Inf)

# New York Times developer site http://developer.nytimes.com
articlesearch.key = "OOOPS"
#
# Query parameter
QUERY_STRING  = "robot"
START_DATE    = "20140101"
END_DATE      = "20160320"

# Create a dataframe of articles from the New York Times
article.df = get.nyt.articles(query.string  = QUERY_STRING,     # OPTION
                              begin.date    = START_DATE,    # OPTION
                              end.date      = END_DATE,    # OPTION
                              pages         = 0:100)         # maximum (do not change)


# Check number of articles returned
num.articles = nrow(article.df)
num.articles

# Check a random sample of 5 articles
doc.ndx = sample(1:num.articles,5)
article.df[doc.ndx,]


# Save
setwd("/home/eolus/Dropbox/MA710 Perso/ASSG2/Workspace")
save(file = "/home/eolus/Dropbox/MA710 Perso/ASSG2/Workspace/article_raw.RData",
     list = c('article.df'))