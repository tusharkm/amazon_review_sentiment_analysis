#Word2Vec
setwd("D:/Big Data Analytics/Project")
library(text2vec)
library(devtools)
library(rword2vec)
install.packages("rvest")
library(stringr)
library(SnowballC)
library(text2vec)
library(leaflet)
library(tm)
library(tmap)
#########Scraping

basehtml <- "https://www.amazon.com/Amazon-Tap-Portable-Wireless-Bluetooth-Speaker-with-WiFi-Alexa/product-reviews/B01BH83OOM/ref=cm_cr_arp_d_paging_btm_2?ie=UTF8&reviewerType=avp_only_reviews&showViewpoints=1&sortBy=recent&pageNumber="
amazonReview <- read_html(paste(basehtml,1,sep = ""))
review <- amazonReview %>%
  html_nodes(".review-text") %>%
  html_text()

for(level in c(2:344)){
  amazonReview <- read_html(paste(basehtml,level,sep = ""))
  review <- c(review,amazonReview %>%
                html_nodes(".review-text") %>%
                html_text())
  print(level)
  Sys.sleep(2)
}

review <- as.data.frame(review,stringsAsFactors=FALSE)
dim(review)

write.csv(review, file = "AmazonReview.csv")

amazon<-read.csv("AmazonReview.csv")

amazon$X<-NULL
amazon$PositiveWordsCount<-NULL
amazon$NegativeWordsCount<-NULL
amazon$Sentiment_Positive<-NULL
amazon$Sentiment_Negative<-NULL
amazon$result<-NULL
sentence<-amazon

pos_word=scan("D:/Big Data Analytics/Positive.txt", what="character",comment.char=";")
neg_word=scan("D:/Big Data Analytics/Negative.txt", what="character",comment.char=";")


datalist = list()
for(i in 1:nrow(sentence))
{myCorpus<- Corpus(VectorSource(sentence[i,"review"]))
datalist[i]<-myCorpus

}

new_corpus <- as.VCorpus(datalist)
?tm_map

myCorpus <- tm_map(new_corpus, tolower)
myCorpus = tm_map(myCorpus,(removePunctuation))
myCorpus = tm_map(myCorpus,(removeNumbers))
myStopwords <- c(stopwords('english'))

myCorpus <- tm_map(myCorpus, removeWords,myStopwords)  

as.character(myCorpus[1])
dictCorpus <- myCorpus
myCorpus <- tm_map(myCorpus, stemDocument)
#as.character(dictCorpus)
myCorpus <- tm_map(myCorpus, PlainTextDocument)
#myCorpus <- tm_map(myCorpus, stemCompletion, dictionary=dictCorpus)
as.character(myCorpus[1])

scoreGenerator=function(z,com)
{print(z)
  x<- unlist(z)
  pos.matches= match(x,pos_word)
  neg.matches=match(x,neg_word)
  neg.matches
  pos.matches
  pos.matches = !is.na(pos.matches)
  neg.matches = !is.na(neg.matches)
  pos.matches
  pos.num=sum(pos.matches)
  neg.num=sum(neg.matches)
  score = pos.num-neg.num
  total=pos.num+neg.num
  pos.per=pos.num/total*100
  neg.per=neg.num/total*100
  scores.df<-(data.frame(com,sum(neg.matches),sum(pos.matches),score,total,pos.per,neg.per))
  # View(scores.df)
  return(scores.df)
}


sentence_comment1 = list()
finalvar1<-data.frame()
t<-myCorpus
test<-as.data.frame(myCorpus)
writeLines(as.character(myCorpus), con="mycorpus.txt")
for(i in 1:length(myCorpus))
{
  sentence_comment1[i]<-(str_split(myCorpus[i],"\\s+")) 
  final1<-( ((scoreGenerator(sentence_comment1[i],sentence[i,"review"]))))
  finalvar1<-rbind(finalvar1,final1)
}

#write.csv(datalistcorpus,file="corpus1.csv")
sentence_comment1[1]
write.csv(finalvar1, file="Output.csv")

#wordvecadd=cbind(finalvar1,dataframe)
myCorpusTokenized <- lapply(myCorpus, scan_tokenizer)
#dataframe <- as.data.frame(myCorpus)


dataframe<-data.frame(text=unlist(sapply(myCorpus, `[`, "content")), 
                      stringsAsFactors=F)

wordvecadd=cbind(finalvar1,dataframe)

write.csv(wordvecadd, file="OutputWordVec1.csv")

wordstoremove<-"listcontent" 
(dat1 <- as.data.frame(sapply(dataframe, function(x) 
  gsub(paste(wordstoremove, collapse = '|'), '', x))))

wordvecadd=cbind(finalvar1,dat1)

#datalistcorpus = list()
#for(i in 1:nrow(sentence))
#{
#datalistcorpus[i]<-myCorpus

#}

for(i in 1:nrow(wordvecadd))
{if(wordvecadd$sum.pos.matches.[i] >= wordvecadd$sum.neg.matches.[i]){
 result101[i] <- "Positive"
} else {
 result101[i] <- "Negative"
}}
result101
wordvecadd=cbind(finalvar1,dat1,result101)
write.csv(wordvecadd, file="OutputWordVec2.csv")


####################Word2Vec
tryTolower = function(northeastern_class)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(northeastern_class), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(northeastern_class)
  
  
  # result
  return(y)
}

listofText<- sapply(wordvecadd$text, (tryTolower))

it <- itoken(listofText, tolower, word_tokenizer, chunks_number = 10, progessbar = F)
vocab <- create_vocabulary(it, ngram = c(1L, 1L), stopwords = stopwords(kind = "english"))
str(vocab, nchar.max = 20, width = 80, strict.width = 'wrap')

vectorizer = vocab_vectorizer(vocab)
dtm = create_dtm(it, vectorizer)
dim(dtm)

#prun vocab
pruned_vocab = prune_vocabulary(vocab, 
                                term_count_min = 3, 
                                doc_proportion_max = 0.5,
                                doc_proportion_min = 0.0001)
vectorizer = vocab_vectorizer(pruned_vocab)
# create dtm_train with new pruned vocabulary vectorizer
dtm  = create_dtm(it, vectorizer)
dim(dtm)

# define tfidf model
tfidf = TfIdf$new()
# fit model to  data and transform  data with fitted model
dtm_tfidf = fit_transform(dtm, tfidf)


#df <- as.numeric(factor(wordvecadd$result101,levels = c("Positive","Negative"))) - 1
df.pos <- as.numeric(factor(wordvecadd$result101,levels = c("Positive")))
df.neg<-as.numeric(factor(wordvecadd$result101,levels = c("Negative")))
df.neg[is.na(df.neg)] <- 0
df.pos[is.na(df.pos)] <- 0
data <- cbind(dtm_tfidf,df.pos,df.neg)
data1<-as.matrix(data)
write.csv(file="finalop2.csv",data1)
dim(data)#2907
View(data1)

##########Deep Beleief network
library(deepnet)
str(data)
ncol.print <- function(data) print(matrix(as.matrix(data),ncol=ncol(data),dimnames=NULL),quote=F)
ncol.print(data)
dim(data)
#Divide train and test data.
set.seed(23)
smp_size <- floor(0.7 * nrow(data))
train_ind <- sample(seq_len(nrow(data)),size = smp_size)
#data.train <- data[train_ind,]
#data.test <- data[-train_ind,]
#data.train.x. <- data.matrix(data.train[,1:609])
#data.train.y <- data.matrix(data.train[,610:611])
#data.test.x <- data.matrix(data.test[,1:609])
#data.test.y <- data.matrix(data.test[,610:611])
data.train <- data[train_ind,]
data.test <- data[-train_ind,]
data.train.x <- data.matrix(data.train[,1:2905])
data.train.y <- data.matrix(data.train[,2906:2907])

data.test.x <- data.matrix(data.test[,1:2905])
data.test.y <- data.matrix(data.test[,2906:2907])
data.train.x[2,]
data.train.y[-1]
?dbn.dnn.train
data.train.x
dnn <- dbn.dnn.train(data.train.x, data.train.y, hidden = c(100,100,2), numepochs = 3, cd = 2)
x<-data.train.x[-1,]
y<-data.train.y[-1,]
dnn <- dbn.dnn.train(x,y, hidden = c(30,30,30), numepochs = 15, cd = 15,
                     learningrate=0.01,output="softmax")

err.dnn <- nn.test(dnn, data.test.x, data.test.y)
preds1 <- nn.predict(dnn, data.test.x)


#write.csv(data,file="wordfinal.csv")
preds1
data.test.x[1,]

library(caret)
confusionMatrix(round(preds1),round( data.test.y))

(preds1)
data.test.y


####################SVM

model <- svm(data.train.y~data.train, kernel="linear", cost=1, gamma = 0.02, data = data.train.x)
prediction = predict(model, data.test.y)
table(data.test.y, prediction)
dim(dtm)