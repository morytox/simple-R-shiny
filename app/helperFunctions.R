library( twitteR )  # for twitter connection
library( tm )       # for text analysis
library( ggplot2 )  # for graphics
library( plotly )   # for interactive graphics

# each twitter app needs authentication via OAuth
"oauth_tw_app" <- function()
{
  consumer_key    <- "8m86Q9irIuDl8bOGTCHZGgbsV"
  
  consumer_secret <- "pVAh2vkpZwFxg4YO3QHwW5cVlzvWPx2G3FnYUVyQMstEZ1dlG8"
  
  access_token    <- "844516894996598786-HcM5QKDxCGlYlv2bi6yvXZVeMYpqa7g"
  
  access_secret   <- "auHEMSG7F3PidplfaeYmaRyKgYW1suv0YqTWoRT2brz5z"
  
  setup_twitter_oauth( consumer_key = consumer_key, consumer_secret = consumer_secret,
                       access_token = access_token, access_secret = access_secret )
}

# remove all URLs
"removeURL" <- function( x ) 
{
  gsub( "http[^[:space:]]*", "", x )
}

# remove anything other than latin letters or space
"removeNumPunct" <- function( x ) 
{
  gsub( "[^[:alpha:][:space:]]*", "", x )
}

# stem completion
## Needs some work, as the stem completion might give out words not used 
## that often
"stemCompletion2" <- function( x, dictionary ) 
{
  x <- unlist( strsplit( as.character( x ), " " ) )
  x <- x[x != ""]
  x <- stemCompletion( x, dictionary = dictionary )
  x <- paste( x, sep = "", collapse = " ")

  return( x )
}

# searchs twitter by search word aand converts to data.frame
"search_twitter" <- function( searchterm, n = 100, lang = "en" )  
{
  # search twitter for searchterm
  tweets <- searchTwitter( searchterm, n = n, lang = lang )
  # convert to data.frame
  tweets.df <- twListToDF( tweets )
  return( tweets.df )
}

# cleans data and transforms texts
"transform_tweets" <- function( tweets.df, lang = "en" )
{
  # build a corpus 
  myCorpus <- Corpus( VectorSource( tweets.df$text ) )
  # remove URLs 
  # has to removed first
  myCorpus <- tm_map( myCorpus, content_transformer( removeURL ) )
  # remove anything other than latin letters or space
  # e.g. emoticons
  myCorpus <- tm_map( myCorpus, content_transformer( removeNumPunct ) )
  # convert to lower case
  # has to make last otherwise there are symbols that cannot be transformed
  myCorpus <- tm_map( myCorpus, content_transformer( tolower ) )
  # remove stopwords
  myStopwords <- c( stopwords( lang ), "via", "rt",
                    "use", "see", "used", "amp" ) 
  myStopwords <- setdiff( myStopwords, c( "r", "big" ) )
  myCorpus <- tm_map( myCorpus, removeWords, myStopwords )
  # remove extra whitespace
  myCorpus <- tm_map( myCorpus, stripWhitespace )
  # create a copy 
  myCorpusCopy <- myCorpus
  # stem words
  myCorpus <- tm_map( myCorpus, stemDocument ) 
  myCorpus <- lapply( myCorpus, stemCompletion2, dictionary = myCorpusCopy )
  myCorpus <- SimpleCorpus( VectorSource( myCorpus ) )
  
  return( myCorpus )
}  

# counts word frequence
"wordFreq" <- function( corpus, word ) 
{
  results <- lapply( corpus, 
                     function( x ) { grep( as.character( x ), pattern = paste0( "\\<", word ) )} )
  sum( unlist( results ) )
}

# find top frequent terms
"find_freq_terms" <- function( tdm, lowfreq = 20 )
{
  term.freq <- rowSums( as.matrix( tdm ) )
  term.freq <- subset( term.freq, term.freq >= lowfreq )
  term.freq <- data.frame( term = names( term.freq ), freq = term.freq )
  
  return( term.freq )
}

