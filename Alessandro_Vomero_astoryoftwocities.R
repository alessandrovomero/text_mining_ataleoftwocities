#set the work directory 
setwd("C:/Users/Alessandro/Dropbox/SDSU FALL 2018/MIS620 - Electronic Business and Big Data Infrastructures/text mining assig")

library(NLP)
library(tm) #for corpus and term document matrix creation/processing
library(SnowballC) #for stemming
library(RColorBrewer)
library(wordcloud)
library(cluster)
library(rpart)

#read in the Dicken's "A tale of wo cities" book
#136673 words distributed on 549 pages (foot notes were not included)
#saved as rdata file to save space and decrease loading times
twocities <- read.csv("astoryoftwocities.csv")
 
#convert text to character, read.csv defaults to Factor
twocities$Text<-as.character(twocities$Text)
twocities$Text
str(twocities)

#convert to text multibyte encoding to UTF form
#this was neccesary after importing on Ubuntu Server, but might not be for you
#encoding differences will often need to reconciled between platforms and editors
twocities$Text <- iconv(enc2utf8(twocities$Text),sub="byte")
twocities$Text <- iconv(twocities$Text, to="utf-8",sub="")


                  ##regular expression
## remove letters, digits, and punctuation characters starting 
#with @ remove usernames and replace with "USER"
twocities$Text <- gsub("\w*\\@\\w*"," USER",twocities$Text)
twocities$Text

##Remove website links and replace with "WEBADDRESS"
twocities$Text  <- gsub("http[[:alnum:][:punct:]]*"," WEBADDRESS",   tolower(twocities$Text))
twocities$Text  <- gsub("www[[:alnum:][:punct:]]*"," WEBADDRESS",   tolower(twocities$Text))
twocities$Text

#remove html entitties like &quot; starting with 
#not perfect but we will remove remaining punctation at later step
twocities$Text<-gsub("\\&\\w*;","", twocities$Text)


#remove any letters repeated more than twice (eg. hellooooooo -> helloo)
twocities$Text  <- gsub('([[:alpha:]])\\1+', '\\1\\1', twocities$Text)

#additional cleaning removing leaving only letters numbers or spaces
twocities$Text <- gsub("[^a-zA-Z0-9 ]","",twocities$Text)

#review the text noow
head(twocities$Text,20)

#list of stopwords
stopwords("english")


#create corpus and clean up text before creating document term matrix
twocities_Corpus <- Corpus(VectorSource(twocities$Text))

twocities_Corpus <- tm_map(twocities_Corpus, stemDocument)
twocities_Corpus <- tm_map(twocities_Corpus, removePunctuation)
twocities_Corpus <- tm_map(twocities_Corpus , removeNumbers)
twocities_Corpus <- tm_map(twocities_Corpus, removeWords, stopwords("english"))
twocities_Corpus <- tm_map(twocities_Corpus, stripWhitespace)  



#create term document matrix (terms as rows, documents as columns)
tdm <- TermDocumentMatrix(twocities_Corpus)
inspect(tdm)

#count row (i.e, terms)
#must convert to matrix to work with as dtm is stored as a memory 
#efficient sparse matrix doesn't store empty fields
tdm$nrow #7069 
tdm$ncol #12354
#inspect the term document matrix, make sure to subset it is very large 
inspect(tdm[1:44, 1:12])

#there are over 7074 terms and high sparsity lets trim down and remove terms
#remove words that are over 98% sparse (i.e., do not appear in 98.9% of documents)
tdm <- removeSparseTerms(tdm, 0.989)
tdm$nrow #now 55 terms
tdm$ncol #12354 
inspect(tdm[1:55, 1:12354])

inspect(tdm)


#now thats its manageable in size (the original dtm saved as a regular matrix requires 32GB of memory)

# define tdm as matrix
m = as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
d #lets see frequency of words

# plot wordcloud
set.seed(1)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=45, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#said (code for  @said included in tweet most frequent lets see what it is associated with)
#, look, one, and upon
findAssocs(tdm, terms = c("said", "look", "one","upon"), corlimit = .05) 

#normally set a limit for correlations to a reasonable r size, bui this is sparse data 
#and we trimmed terms

#lets make a bar chart of frequent words
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")


#lets cluster the documents, but first find optimal k
wss <- numeric(15) 
wss
for (k in 1:10) wss[k] <- sum(kmeans(tdm, centers=k)$withinss)
plot(wss, type="b")

twocities.kmeans <- kmeans(tdm,2)
twocities.kmeans$cluster

###################################################################################