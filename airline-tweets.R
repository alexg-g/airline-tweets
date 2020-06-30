setwd("/Users/alexg/Desktop/Rlesson")

library(tm) #for corpus and term document matrix creation/processing
library(SnowballC) #for stemming
library(wordcloud)
library(cluster)
library(rpart)
library(topicmodels)
library(tidyverse)
library(ggplot2)
library(ROCR)
library(caret)
library(dplyr)
library(data.table)
library(neuralnet)



#read in subset of twitter

twitter.sub <- read.csv("project_twitter1.csv")
str(twitter.sub)

#convert tweet text to character, read.csv defaults to Factor
twitter.sub$Text <- as.character(twitter.sub$text)

str(twitter.sub)


#gsub tutorial
#gsub looks for pattern and replaces gsub(pattern, replacement, text)
gsub("!", "", c("hi!", "hi hi hi!!!"))



head(twitter.sub)
names(twitter.sub)



head(twitter.sub$text, 20)

#convert to texttext multibyte encoding to UTF form
#this was neccesary after importing on Ubuntu Server, but might not be for you
#encoding differences will often need to reconciled between platforms and editors
twitter.sub$Text <- iconv(twitter.sub$text, to="utf-8",sub="")


##regular expression
#Remove duplicate retweets and replace with RETWEET
twitter.sub$Text <- gsub("Our fleet's on fleek"," RETWEET",   twitter.sub$Text)
twitter.sub$Text <- gsub("@JetBlue's CEO battles to appease passengers and Wall Street"," RETWEET",   twitter.sub$Text)
twitter.sub$Text <- gsub("@JetBlue's new CEO seeks the right balance to please passengers and Wall"," RETWEET",   twitter.sub$Text)
twitter.sub$Text <- gsub("New airline expected to make its way to MEM"," RETWEET",   twitter.sub$Text)
twitter.sub$Text <- gsub("@JetBlue CEO battles to appease passengers, investors"," RETWEET",   twitter.sub$Text)
twitter.sub$Text <- gsub("@JetBlue's CEO Battles to Appease Passengers and Wall Street"," RETWEET",   twitter.sub$Text)
twitter.sub$Text <- gsub("@VirginAmerica has getaway deals through May, from $59 one-way. Lots of cool cities"," RETWEET",   twitter.sub$Text)



#for some reason it appears the phrase "Cancelled Flightled" was substituted for "cancelled" in the original dataset. 
#I'm reverting all instances of "Cancelled Flightled" to go back to "cancelled"
twitter.sub$Text <- gsub("Cancelled Flightled", "cancelled",   twitter.sub$Text)
twitter.sub$Text <- gsub("cancelled flightled", "cancelled",   twitter.sub$Text)
twitter.sub$Text <- gsub("Cancelled Flighted", "cancelled",   twitter.sub$Text)
twitter.sub$Text <- gsub("cancelled flighted", "cancelled",   twitter.sub$Text)

#Same for the phrase "Cancelled Flightlations" and "Cancelled Flightlation" which was substituted for "cancellations" and "cancellation", so I'll revert that as well.
twitter.sub$Text <- gsub("Cancelled Flightlations", "cancellations",   twitter.sub$Text)
twitter.sub$Text <- gsub("cancelled flightlations", "cancellations",   twitter.sub$Text)
twitter.sub$Text <- gsub("Cancelled Flightlation", "cancellation",   twitter.sub$Text)
twitter.sub$Text <- gsub("cancelled flightlation", "cancellation",   twitter.sub$Text)


#Also for the phrase "cancelled flightling" which was substituted for "cancelling"
twitter.sub$Text <- gsub("Cancelled Flightling", "cancelling",   twitter.sub$Text)
twitter.sub$Text <- gsub("cancelled flightling", "cancelling",   twitter.sub$Text)


#Found another one "late flightr" was substituted for "later"
twitter.sub$Text <- gsub("Late Flightr", "later",   twitter.sub$Text)
twitter.sub$Text <- gsub("late flightr", "later",   twitter.sub$Text)
twitter.sub$Text <- gsub("Late FlightR", "later",   twitter.sub$Text)


##Remove website links and replace with "URL"
twitter.sub$Text  <- gsub("http[[:alnum:][:punct:]]*"," WEBADDRESS",   tolower(twitter.sub$Text ))
twitter.sub$Text  <- gsub("www[[:alnum:][:punct:]]*"," WEBADDRESS",   tolower(twitter.sub$Text ))


#Remove stopwords before removing puncutation
#list of stopwords
stopwords("english")
twitter.sub$Text <- removeWords(twitter.sub$Text, stopwords("english"))


#creating a new dataframe that will preserve unique names for branded accounts but also
#have all the text transformations from before

twitter.brand <- twitter.sub
twitter.brand$Text.brand <- twitter.sub$Text
twitter.brand$Text <- NULL


## remove letters, digits, and punctuation characters starting with @ remove usernames and replace with "USER"
twitter.sub$Text <- gsub("@\\w*", "USER",   twitter.sub$Text)


#Transforming user handles for Virgin America, United, US Airways, Southwest, Delta, Jet Blue and American
twitter.brand$Text.brand <- gsub("@virginamerica", "virginair",   twitter.brand$Text.brand)
twitter.brand$Text.brand <- gsub("@united", "unitedair",   twitter.brand$Text.brand)
twitter.brand$Text.brand <- gsub("@southwestair", "southwestair",   twitter.brand$Text.brand)
twitter.brand$Text.brand <- gsub("@jetblue", "jetblueair",   twitter.brand$Text.brand)
twitter.brand$Text.brand <- gsub("@usairways", "usairways",   twitter.brand$Text.brand)
twitter.brand$Text.brand <- gsub("@americanair", "americanair",   twitter.brand$Text.brand)
twitter.brand$Text.brand <- gsub("@delta", "deltaair",   twitter.brand$Text.brand)

#Now we can replace @usernames with USER for the branded data frame
twitter.brand$Text <- gsub("@\\w*", "USER",   twitter.brand$Text.brand)


#remove html entitties like &quot; starting with 
twitter.sub$Text<-gsub("\\&\\w*;","", twitter.sub$Text)

#also from the branded column
twitter.brand$Text.brand<-gsub("\\&\\w*;","", twitter.brand$Text.brand)

#remove any letters repeated more than twice (eg. hellooooooo -> helloo)
twitter.sub$Text  <- gsub('([[:alpha:]])\\1+', '\\1\\1', twitter.sub$Text)

#again from the branded column
#remove any letters repeated more than twice (eg. hellooooooo -> helloo)
twitter.brand$Text.brand  <- gsub('([[:alpha:]])\\1+', '\\1\\1', twitter.brand$Text.brand)

#additional cleaning removing special characters leaving only letters numbers or spaces
twitter.sub$Text <- gsub("[^a-zA-Z0-9 ]","",twitter.sub$Text)

#from the branded column
twitter.brand$Text.brand <- gsub("[^a-zA-Z0-9 ]","",twitter.brand$Text.brand)


#review tweets now
head(twitter.sub$Text,20)
head(twitter.brand$Text.brand,20)

nrow(twitter.sub)
nrow(twitter.brand)

#create corpus and clean up text before creating document term matrix
Twitter_Corpus <- Corpus(VectorSource(twitter.sub$Text))
Twitter_Corpus <- tm_map(Twitter_Corpus, stemDocument)
Twitter_Corpus <- tm_map(Twitter_Corpus, removePunctuation)
Twitter_Corpus <- tm_map(Twitter_Corpus, removeNumbers)
Twitter_Corpus <- tm_map(Twitter_Corpus, stripWhitespace)  



#create corpus using branded column
Twitter_Corpus_Brands <- Corpus(VectorSource(twitter.brand$Text.brand))
Twitter_Corpus_Brands <- tm_map(Twitter_Corpus_Brands, stemDocument)
Twitter_Corpus_Brands <- tm_map(Twitter_Corpus_Brands, removePunctuation)
Twitter_Corpus_Brands <- tm_map(Twitter_Corpus_Brands, removeNumbers)
Twitter_Corpus_Brands <- tm_map(Twitter_Corpus_Brands, stripWhitespace)  


#create term document matrix (terms as rows, documents as columns)
tdm <- TermDocumentMatrix(Twitter_Corpus)
tdm

#branded term document matrix
tdm.branded <- TermDocumentMatrix(Twitter_Corpus_Brands)


inspect(tdm)
inspect(tdm.branded)
tdm$nrow
tdm$ncol

tdm.branded$nrow
tdm.branded$ncol

?removeSparseTerms

??TermDocumentMatrix

tdm.branded <- removeSparseTerms(tdm.branded, 0.99)
tdm.branded$nrow

#count row (i.e, terms)
#must convert to matrix to work with as dtm is stored as a memory efficient sparse matrix doesn't store
#empty fields
tdm$nrow 

#inspect the term document matrix, make sure to subset it is very large 
inspect(tdm[1:30, 1:30])

#remove words that are over 98% sparse (i.e., do not appear in 98% of documents)
tdm <- removeSparseTerms(tdm, 0.98)
tdm$nrow #now 66 terms
tdm$ncol #14,640 tweets
inspect(tdm[1:46, 1:3])

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
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


#remove user from tdm because it appears in every tweet
m.rem = as.matrix(tdm[-1,])
v.rem <- sort(rowSums(m.rem),decreasing=TRUE)
d.rem <- data.frame(word = names(v.rem),freq=v.rem)
d.rem #lets see frequency of words
#wordcloud without user
set.seed(1)
wordcloud(words = d.rem$word, freq = d.rem$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))



#Turning branded tdm into a matrix with 98% sparcity
#remove words that are over 98% sparse (i.e., do not appear in 98% of documents)
tdm.branded <- removeSparseTerms(tdm.branded, 0.98)
tdm.branded$nrow #now 159 terms
tdm.branded$ncol #14,640 tweets
inspect(tdm.branded[1:46, 1:3])

m.branded = as.matrix(tdm.branded)
v.branded <- sort(rowSums(m.branded),decreasing=TRUE)
d.branded <- data.frame(word = names(v.branded),freq=v.branded)
d.branded #lets see frequency of words

#wordcloud using branded matrix
set.seed(1)
wordcloud(words = d.branded$word, freq = d.branded$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


#normally set a limit for correlations to a reasonable r size, bui this is sparse data and we trimmed terms

#lets make a bar chart of frequent words excluding USER since it appears in every tweet
barplot(d[2:11,]$freq, las = 2, names.arg = d[2:11,]$word,
        col ="lightblue", main ="Most frequent words",
        xlab = "Frequency", horiz = TRUE, las = 2)

d[2:11,]


#lets cluster the documents, but first find optimal k
set.seed(1)
wss <- numeric(15) 
for (k in 1:10) wss[k] <- sum(kmeans(tdm, centers=k)$withinss)
plot(wss, type="b") #seems like 2 or 3 will cover it

set.seed(1)
twitter.kmeans <- kmeans(tdm, 3, iter.max = 200, nstart = 100)
twitter.kmeans$cluster #lets looks at cluster membership

set.seed(1)
tdm$cluster <- twitter.kmeans$cluster
table(tdm$cluster)

#66 words in tdm are classified into 3 clusters :cluster 1-1 word, cluster 2- 1 word , cluster 3 -62 words

#this is strange. 
#I'm stumped on this too. I tried changing nstart to 1000 and it still ends up with 
#only 1 term in all the other groups. Also tried increasing iter.max to 200 and 
#still had the same results. Perhaps changing the sparcity to 98% will make a difference.


#Trying k means analysis using branded tdm
set.seed(1)
wss.branded <- numeric(15) 
for (k in 1:10) wss.branded[k] <- sum(kmeans(tdm.branded, centers=k)$withinss)
plot(wss.branded, type="b") #wss falls to zero at k=11. This doesn't seem right. Curious to see what LDA analysis reveals.

set.seed(1)
twitter.kmeans.branded <- kmeans(tdm.branded, 5, iter.max = 200, nstart = 100)
twitter.kmeans.branded$cluster #lets looks at cluster membership

set.seed(1)
tdm.branded$cluster <- twitter.kmeans.branded$cluster
table(tdm.branded$cluster)

dtm.branded <- DocumentTermMatrix(Twitter_Corpus_Brands)
dtm.branded <- removeSparseTerms(dtm.branded, 0.99) #remove sparse terms
dtm.branded
inspect(dtm.branded[1:4,1:10])

ui.branded = unique(dtm.branded$i)
dtm.branded.nonempty <- dtm.branded[ui.branded,] #createing new document term matrix with only tweets with words
ldaOut.branded <-LDA(dtm.branded.nonempty,k=5, method="Gibbs")

ldaOut.branded.terms <- as.matrix(terms(ldaOut.branded,10))
ldaOut.branded.terms



#lets do some sentiment analysis with each word as a column
#document term matrix, 



#create Document Term Matrix (so terms are columns or attributes)
dtm <- DocumentTermMatrix(Twitter_Corpus)
dtm <- removeSparseTerms(dtm, 0.98) #remove sparse terms
dtm
inspect(dtm[1:4,1:10])

#before looking at classifications, lets do some basic LDA topic modeling with gibbs sampling

library(topicmodels)

#filter tweets that have no words after processing
ui = unique(dtm$i)
dtm.nonempty <- dtm[ui,] #creating new document term matrix with only tweets with words
nrow(dtm.nonempty)

#Run LDA using Gibbs sampling
ldaOut <-LDA(dtm.nonempty,k=5, method="Gibbs")


#top 10 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,10))
ldaOut.terms


#CLASSIFICATION

#convert to matrix

names(twitter.sub)
nrow(twitter.sub)
nrow(dtm)
#appending sentiment to the tdm docs (since rows are docs or tweets and columns are freq, we are adding a new column for sentiment )
labeledTerms <- as.data.frame(as.matrix(dtm))  
labeledTerms$Sentiment <- twitter.sub$airline_sentiment #merge with labels
table(labeledTerms$Sentiment)

nrow(labeledTerms)
ncol(labeledTerms)
#there are three levels


#model training on training set (first 14000 rows are balanced training set)
library(rpart)
library(rpart.plot)
library(pROC)

#train decision tree on sentiment
nrow(twitter.sub)
ncol(twitter.sub)
sentiment.rpart = rpart(Sentiment ~., data=labeledTerms[1:11712,], method="class" , model=TRUE)
rpart.plot(sentiment.rpart, extra=2, clip.right.labs=FALSE, varlen=0, faclen=3)

names(labeledTerms)
#evaluate performance on test set

#performance testing on test set---------------
#prediction on of test set 
p.rpart <- predict(sentiment.rpart, labeledTerms[11713:14640,-67])
str(p.rpart)


score <- p.rpart[, c("positive")]
actual_class <- labeledTerms[11713:14640,]$Sentiment == 'positive'
pred <- prediction(score, actual_class)
perf <- performance(pred, "tpr", "fpr")

plot(perf, lwd=2, xlab="False Positive Rate (FPR)",
     ylab="True Positive Rate (TPR)")
abline(a=0, b=1, col="gray50", lty=3)

## corresponding AUC score
auc <- performance(pred, "auc")
auc <- unlist(slot(auc, "y.values"))
auc #not bad AUC of .75

#lets make a confusion matrix using caret
library(caret)
library(doParallel)

#removing neutral

labeledTerms1<-labeledTerms[!labeledTerms$Sentiment=="neutral",]
labeledTerms1$Sentiment

summary(labeledTerms1$Sentiment)

labeledTerms1=droplevels(labeledTerms1)
summary(labeledTerms1$Sentiment)
labeledTerms1

str(labeledTerms1)
ncol(labeledTerms1)
y <- labeledTerms1$Sentiment#keep your DV away from all processing and filtering to avoid overfitting!
x <- labeledTerms1[,1:66] 

set.seed(543)
inTrain<-createDataPartition(y=y, p=.85, list=FALSE)

#lets split out using index of training and test sets created above, uses row index

y.train <- y[inTrain] #ouput
x.train <- x[inTrain,]  #input

y.test<- y[-inTrain]
x.test <- x[-inTrain,]

#check composition

table(y.train)
table(y.test)



##lets start modeling

#some parameters to control the sampling during parameter tuning and testing
#5 fold crossvalidation, using 5-folds instead of 10 to reduce computation time in class demo, use 10 and with more computation to spare use
#repeated cv
ctrl <- trainControl(method="cv", number=10,
                     classProbs=TRUE,
                     #function used to measure performance
                     summaryFunction = twoClassSummary, #multiClassSummary for non binary
                     allowParallel =  TRUE) #am disabling allowParallel because of bug in caret 6.0-77
#example models are small enough that parallel not important but set to TRUE! when working with large datasets  
ctrl_down <-trainControl(method="cv", number=10,
                         classProbs=TRUE,sampling = "down",
                         #function used to measure performance
                         summaryFunction = twoClassSummary, #multiClassSummary for non binary
                         allowParallel =  TRUE)
ctrl_up <-trainControl(method="cv", number=10,
                       classProbs=TRUE,sampling = "up",
                       #function used to measure performance
                       summaryFunction = twoClassSummary, #multiClassSummary for non binary
                       allowParallel =  TRUE)
ctrl_smote <-trainControl(method="cv", number=10,
                          classProbs=TRUE,sampling = "smote",
                          #function used to measure performance
                          summaryFunction = twoClassSummary, #multiClassSummary for non binary
                          allowParallel =  TRUE)
ctrl_rose<-trainControl(method="cv", number=10,
                        classProbs=TRUE,sampling = "rose",
                        #function used to measure performance
                        summaryFunction = twoClassSummary, #multiClassSummary for non binary
                        allowParallel =  TRUE)

ctrl_svm <- trainControl(method="cv", number=5,
                         classProbs=TRUE,
                         #function used to measure performance
                         summaryFunction = twoClassSummary, #multiClassSummary for non binary
                         allowParallel =  TRUE) 

ctrl_rose_svm<-trainControl(method="cv", number=5,
                            classProbs=TRUE,sampling = "rose",
                            #function used to measure performance
                            summaryFunction = twoClassSummary, #multiClassSummary for non binary
                            allowParallel =  TRUE)

ctrl_svm_merge<-trainControl(method="cv", number=5,
                             classProbs=TRUE,sampling = "rose",
                             #function used to measure performance
                             summaryFunction = twoClassSummary, #multiClassSummary for non binary
                             allowParallel =  TRUE)

#twoClassSummary is built in function with ROC, Sensitivity and Specificity
#rpart
set.seed(192)
m.rpart.15 <- train(y=y.train, x=x.train,
                    trControl = ctrl,
                    metric = "ROC", #using AUC to find best performing parameters
                    tuneLength=15, #search through 15 different complexity parameters for pruning (150 models, 10 CV x 15 parameters)
                    method = "rpart")
m.rpart.15
m.rpart.15 <- readRDS("m-rpart-15.Rds")
saveRDS(file = "m-rpart-15.rds")

getTrainPerf(m.rpart.15)
plot(m.rpart.15)

p.rpart<- predict(m.rpart.15,x.test)
p.rpart
confusionMatrix(p.rpart,y.test) 

##Naive Bayes
modelLookup("nb") #we have some paramters to tune such as laplace correction
set.seed(192)
m.nb <- train(y=y.train, x=x.train,
              trControl = ctrl,
              metric = "ROC",tuneLength=15, #using AUC to find best performing parameters
              method = "nb")
saveRDS(m.nb, file = "m-nb.rds")
m.nb
m.nb <- readRDS("m-nb.Rds")

getTrainPerf(m.nb)
plot(m.nb)
p.nb<- predict(m.nb,x.test)
confusionMatrix(p.nb,y.test) #calc accuracies with confusion matrix on test set



##Logistic Regression (no parameters here, but will get cross validated perfomrance measures)
modelLookup("glm")
set.seed(192)
m.glm<- train(y=y.train, x=x.train,
              trControl = ctrl,
              metric = "ROC", tuneLength=15,#using AUC to find best performing parameters
              method = "glm")
saveRDS(m.glm, file = "m-glm.rds")
m.glm <- readRDS("m-glm.Rds")

m.glm
getTrainPerf(m.glm)
p.glm<- predict(m.glm,x.test)
confusionMatrix(p.glm,y.test)



library(ipred)
set.seed(192)
modelLookup("treebag")
m.bag <- train(y=y.train, x=x.train,
               trControl = ctrl,
               metric = "ROC",tuneLength=15, #using AUC to find best performing parameters
               method = "treebag")
saveRDS(m.bag, file = "m-bag.rds")
m.bag <- readRDS("m-bag.Rds")
m.bag
p.bag<- predict(m.bag,x.test)
confusionMatrix(p.bag,y.test)



library(randomForest)
set.seed(192)
modelLookup("rf")
m.rf <- train(y=y.train, x=x.train,
              trControl = ctrl,
              metric = "ROC", #using AUC to find best performing parameters
              tuneLength=9,
              method = c("rf") )
saveRDS(m.rf, file = "m-rf.rds")
m.rf <- readRDS("m-rf.Rds")
p.rf<- predict(m.rf,x.test)
confusionMatrix(p.rf,y.test)



library(e1071)
m.svm <- train(y=y.train, x=x.train,
               trControl = ctrl_svm,
               metric = "ROC", tuneLength=15, #Accuracy over all classes
               method = "svmRadial")
saveRDS(m.svm, file = "m-svm.rds")
m.svm <- readRDS("m-svm.Rds")
m.svm
plot(m.svm)
p.svm<- predict(m.svm, x.test)
confusionMatrix(p.svm, y.test)



#combining results

rValues <- resamples(list(rpart=m.rpart.15, naivebayes=m.nb, logistic=m.glm , bag=m.bag,rf=m.rf, SVM=m.svm))

str(m.rpart.15)
rValues_test <- resamples(list(rpart=p.rpart, naivebayes=p.nb, logistic=p.glm , bag=p.bag, SVM=p.svm))
names(p.rpart)
str(p.rpart)

summary(rValues_test)

#need probability predictions not labels, let's predict again
rpart.prob <- predict(m.rpart.15, x.test, type="prob")
nb.prob <- predict(m.nb, x.test, type="prob")
glm.prob <- predict(m.glm, x.test, type="prob")
bag.prob <-predict(m.bag, x.test, type="prob")
rf.prob<-predict(m.rf, x.test, type="prob")
svm.prob <-predict(m.svm, x.test, type="prob")


names(rpart.prob)
rpart.prob$negative

??roc
#using no probability as positive class
rpart.roc<- roc(y.test, rpart.prob$negative)
nb.roc<- roc(y.test, nb.prob$negative)
glm.roc<- roc(y.test, glm.prob$negative)
bag.roc<- roc(y.test, bag.prob$negative)
rf.roc<- roc(y.test, rf.prob$negative)
svm.roc<- roc(y.test, svm.prob$negative)

#lets see auc
auc(rpart.roc)
auc(nb.roc)
auc(glm.roc)
auc(bag.roc)
auc(rf.roc)
auc(svm.roc)


#let's create an ROC plot with all combined
plot(rpart.roc, col="black")
plot(nb.roc, add=T, col="red")
plot(glm.roc, add=T, col="blue")
plot(bag.roc, add=T, col="pink")
plot(rf.roc, add=T, col="orange")
plot(svm.roc, add=T, col="green")

legend(x=.34, y=.3, cex=.5, legend=c("rpart","Naive Bayes", "Logistic","bag","rf", "SVM"), col=c("black", "red", "blue","pink","orange","green"), lwd=5)

#pro-tip use smooth function if your curves are to rough from small test set size
plot(smooth(rpart.roc), col="black")
plot(smooth(nb.roc), add=T, col="red")
plot(smooth(glm.roc), add=T, col="blue")
plot(smooth(bag.roc), add=T, col="pink")
plot(smooth(rf.roc), add=T, col="orange")
plot(smooth(svm.roc), add=T, col="green")
legend(x=.34, y=.3, cex=1, legend=c("rpart","Naive Bayes", "Logistic","bag","rf","SVM"), col=c("black", "red", "blue","pink","orange","green"), lwd=5)



#compare the performance of all models trained today
#have to add random forest and ada after executing 

#######################################################################################################
#rose 
set.seed(192)
m.rpart.15.rose <- train(y=y.train, x=x.train,
                         trControl = ctrl_rose,
                         metric = "ROC", #using AUC to find best performing parameters
                         tuneLength=15, #search through 15 different complexity parameters for pruning (150 models, 10 CV x 15 parameters)
                         method = "rpart")
saveRDS(m.rpart.15.rose, file = "m-rpart-15-rose.rds")
m.rpart.15.rose <- readRDS("m-rpart-15-rose.Rds")
m.rpart.15.rose
getTrainPerf(m.rpart.15.rose)
plot(m.rpart.15.rose)
p.rpart.rose<- predict(m.rpart.15.rose, x.test)
y.test
confusionMatrix(p.rpart.rose,y.test) 


##Naive Bayes
modelLookup("nb") #we have some paramters to tune such as laplace correction
set.seed(192)
m.nb.rose <- train(y=y.train, x=x.train,
                   trControl = ctrl_rose,
                   metric = "ROC",tuneLength=15, #using AUC to find best performing parameters
                   method = "nb")
saveRDS(m.nb.rose, file = "m-nb-rose.rds")
m.nb.rose <- readRDS("m-nb-rose.Rds")
m.nb.rose
getTrainPerf(m.nb.rose)
plot(m.nb.rose)
p.nb.rose<- predict(m.nb.rose,x.test)
confusionMatrix(p.nb.rose,y.test) #calc accuracies with confusion matrix on test set



##Logistic Regression (no parameters here, but will get cross validated perfomrance measures)
modelLookup("glm")
set.seed(192)
m.glm.rose<- train(y=y.train, x=x.train,
                   trControl = ctrl_rose,
                   metric = "ROC", tuneLength=15,#using AUC to find best performing parameters
                   method = "glm")
saveRDS(m.glm.rose, file = "m-glm-rose.rds")
m.glm.rose <- readRDS("m-glm-rose.Rds")
m.glm.rose
getTrainPerf(m.glm.rose)
p.glm.rose<- predict(m.glm.rose,x.test)
confusionMatrix(p.glm.rose,y.test)



library(ipred)
set.seed(192)
modelLookup("treebag")
m.bag.rose <- train(y=y.train, x=x.train,
                    trControl = ctrl_rose,
                    metric = "ROC",tuneLength=15, #using AUC to find best performing parameters
                    method = "treebag")
saveRDS(m.bag.rose, file = "m-bag-rose.rds")
m.bag.rose <- readRDS("m-bag-rose.Rds")
m.bag.rose
p.bag.rose<- predict(m.bag.rose,x.test)
confusionMatrix(p.bag.rose,y.test)



library(randomForest)
set.seed(192)
modelLookup("rf")
m.rf.rose <- train(y=y.train, x=x.train,
                   trControl = ctrl_rose,
                   metric = "ROC", #using AUC to find best performing parameters
                   tuneLength=9,
                   method = c("rf") )
saveRDS(m.rf.rose, file = "m.rf.rose.rds")
m.rf.rose <- readRDS("m-rf-rose.Rds")
m.rf.rose
p.rf.rose<- predict(m.rf.rose,x.test)
confusionMatrix(p.rf.rose,y.test)

####additional models - svm 
library(e1071)
set.seed(543)
m.svm.rose <- train(y=y.train, x=x.train,
                    trControl = ctrl_rose_svm,
                    metric = "ROC", tuneLength=15, #Accuracy over all classes
                    method = "svmRadial")
saveRDS(m.svm.rose, file = "m-svm-rose.rds")
m.svm.rose <- readRDS("m-svm-rose.Rds")
m.svm.rose
plot(m.svm.rose)
p.svm.rose<- predict(m.svm.rose,x.test)
confusionMatrix(p.svm.rose,y.test)

####additional models - neural network






################################################################################


rValues_rose <- resamples(list(rpart=m.rpart.15.rose, naivebayes=m.nb.rose, logistic=m.glm.rose , bag=m.bag.rose, rf=m.rf.rose, SVM=m.svm.rose))

summary(rValues_rose)

#create plot comparing them
bwplot(rValues_rose, metric="ROC")
bwplot(rValues_rose, metric="Sens") #Sensitvity
bwplot(rValues_rose, metric="Spec")

####Visualizations for original data set
twitter.dt <- as.data.table(twitter.sub) 
twitter.dt <- tbl_df(twitter.dt)
glimpse(twitter.dt)

summary(twitter.sub$airline)


d
words.counts <- as.data.frame(d)
words.counts
names(words.counts)[1]='Word'
words.counts


ggplot(data=words.counts, aes(x=Word, y = freq))+
  geom_bar(stat="identity")+coord_flip()



ggplot(data=twitter.sub, aes(x=airline, fill=airline_sentiment))+
  geom_bar(stat="count", position="stack")




identity(twitter.sub$airline)

delta <- subset(twitter.sub, airline == "Delta")
american <- subset(twitter.sub, airline == "American")
virgin <- subset(twitter.sub, airline == "Virgin America")
southwest <- subset(twitter.sub, airline == "Southwest")
united <- subset(twitter.sub, airline == "United")
us.airways <- subset(twitter.sub, airline == "US Airways")


delta.total <- nrow(delta)
american.total <- nrow(american)
virgin.total <- nrow(virgin)
southwest.total <- nrow(southwest)
united.total <- nrow(united)
us.airways.total <- nrow(us.airways)



delta.total 
american.total 
virgin.total
southwest.total 
united.total
us.airways.total 




delta.neg <- sum(delta$airline_sentiment == "negative")
american.neg <- sum(american$airline_sentiment == "negative")
virgin.neg <- sum(virgin$airline_sentiment == "negative")
southwest.neg <- sum(southwest$airline_sentiment == "negative")
united.neg <- sum(united$airline_sentiment == "negative")
us.airways.neg <- sum(us.airways$airline_sentiment == "negative")


delta.neut <- sum(delta$airline_sentiment == "neutral")
american.neut <- sum(american$airline_sentiment == "neutral")
virgin.neutg <- sum(virgin$airline_sentiment == "neutral")
southwest.neut <- sum(southwest$airline_sentiment == "neutral")
united.neut <- sum(united$airline_sentiment == "neutral")
us.airways.neut <- sum(us.airways$airline_sentiment == "neutral")

delta.neut 
american.neut
virgin.neutg
southwest.neut 
united.neut
us.airways.neut 


delta.pos <- sum(delta$airline_sentiment == "positive")
american.pos <- sum(american$airline_sentiment == "positive")
virgin.pos <- sum(virgin$airline_sentiment == "positive")
southwest.pos <- sum(southwest$airline_sentiment == "positive")
united.pos <- sum(united$airline_sentiment == "positive")
us.airways.pos <- sum(us.airways$airline_sentiment == "positive")

delta.pos
american.pos
virgin.pos
southwest.pos
united.pos
us.airways.pos



delta.ratio <- delta.neg/delta.total
american.ratio <- american.neg/american.total
virgin.ratio <- virgin.neg/virgin.total
southwest.ratio <- southwest.neg/southwest.total
united.ratio <- united.neg/united.total
us.airways.ratio <- us.airways.neg/us.airways.total

ratios.comb <- c(american.ratio, delta.ratio, southwest.ratio, united.ratio, us.airways.ratio, virgin.ratio)
ratios.comb <- ratios.comb*100
ratios.comb <- transpose(as.data.frame(ratios.comb))
names(ratios.comb) <- c("American", "Delta", "Southwest", "United", "US Airways", "Virgin America")
ratios.comb
ratios.comb <- t(as.data.frame(ratios.comb))
ratios.comb <- as.data.frame(ratios.comb)
ratios.comb

ggplot(ratios.comb, aes(x=rownames(ratios.comb), V1, fill = V1)) + geom_bar(stat='identity') +
  xlab("Airline") + ylab("% Negative Tweets") 

virgin.neg
virgin.ratio

##############################################################################
#plotting and comparing all of the models


rpart.prob <- predict(m.rpart.15, x.test, type="prob")
nb.prob <- predict(m.nb, x.test, type="prob")
glm.prob <- predict(m.glm, x.test, type="prob")
bag.prob <-predict(m.bag, x.test, type="prob")
rf.prob<-predict(m.rf, x.test, type="prob")
p.svm<- predict(p.svm, x.test, type="prob")



p.rpart.rose<- predict(m.rpart.15.rose,x.test, type="prob")
p.nb.rose<- predict(m.nb.rose,x.test, type="prob")
p.glm.rose<- predict(m.glm.rose,x.test, type="prob")
p.bag.rose<- predict(m.bag.rose,x.test, type="prob")
p.rf.rose<- predict(m.rf.rose,x.test, type="prob")
p.svm.rose<- predict(m.svm.rose, x.test, type="prob")



rpart.rose.roc<- roc(y.test, p.rpart.rose$negative)
nb.rose.roc<- roc(y.test, p.nb.rose$negative)
glm.rose.roc<- roc(y.test, p.glm.rose$negative)
bag.rose.roc<- roc(y.test, p.bag.rose$negative)
rf.rose.roc<- roc(y.test, p.rf.rose$negative)
svm.rose.roc<- roc(y.test, p.svm.rose$negative)



plot(rpart.rose.roc, col="black")
plot(nb.rose.roc, add=T, col="red")
plot(glm.rose.roc, add=T, col="blue")
plot(bag.rose.roc, add=T, col="pink")
plot(rf.rose.roc, add=T, col="orange")
plot(svm.rose.roc, add=T, col="green")
legend(x=.34, y=.3, cex=.5, legend=c("rpart","Naive Bayes", "Logistic","bag","rf", "SVM"), col=c("black", "red", "blue","pink","orange","green"), lwd=5)


rpart.roc
nb.roc
glm.roc
bag.roc
rf.roc

rpart.rose.roc
nb.rose.roc
glm.rose.roc
bag.rose.roc
rf.rose.roc


################################################################################
#create a new dataset with neutral included in positive class

dtm.merge <- dtm
twitter.sub.merge <- twitter.sub
twitter.sub.merge$airline_sentiment

levels(twitter.sub.merge$airline_sentiment)[levels(twitter.sub.merge$airline_sentiment)%in%c("positive","neutral")] <- "positive"

twitter.sub.merge$airline_sentiment

labeledTerms.merge <- as.data.frame(as.matrix(dtm.merge))  
labeledTerms.merge$Sentiment <- twitter.sub.merge$airline_sentiment #merge with labels
table(labeledTerms.merge$Sentiment)

labeledTerms.merge=droplevels(labeledTerms.merge)


str(labeledTerms.merge)
ncol(labeledTerms.merge)
y.merge <- labeledTerms.merge$Sentiment
x.merge <- labeledTerms.merge[,1:66] 


set.seed(543)
inTrain.merge <- createDataPartition(y=y.merge, p=.85, list=FALSE)

inTrain.merge
y.train.merge <- y.merge[inTrain.merge] #ouput
x.train.merge <- x.merge[inTrain.merge,]  #input

y.test.merge <- y.merge[-inTrain.merge]
x.test.merge <- x.merge[-inTrain.merge,]

#check composition

table(y.train.merge)
table(y.test.merge)


##modeling for the merged dataset where neutral and positive are considered positive
ctrl_merge <- trainControl(method="cv", number=10,
                           classProbs=TRUE,
                           #function used to measure performance
                           summaryFunction = twoClassSummary, #multiClassSummary for non binary
                           allowParallel =  TRUE) #am disabling allowParallel because of bug in caret 6.0-77


#Run the models using the merged train and test sets

#rpart
set.seed(543)
m.rpart.merge <- train(y=y.train.merge, x=x.train.merge,
                       trControl = ctrl_merge,
                       metric = "ROC", #using AUC to find best performing parameters
                       tuneLength=15, #search through 15 different complexity parameters for pruning (150 models, 10 CV x 15 parameters)
                       method = "rpart")
m.rpart.merge
saveRDS(m.rpart.merge, file = "m-rpart-merge.rds")
m.rpart.merge <- readRDS("m-rpart-merge.Rds")
getTrainPerf(m.rpart.merge)
plot(m.rpart.merge)
str(p.rpart.merge)
p.rpart.merge <- predict(m.rpart.merge, x.test.merge)
confusionMatrix(p.rpart.merge, y.test.merge) 


##Naive Bayes
modelLookup("nb") #we have some paramters to tune such as laplace correction
set.seed(192)
m.nb.merge <- train(y=y.train.merge, x=x.train.merge,
                    trControl = ctrl_merge,
                    metric = "ROC",tuneLength=15, #using AUC to find best performing parameters
                    method = "nb")
saveRDS(m.nb.merge, file = "m-nb-merge.rds")
m.nb.merge <- readRDS("m-nb-merge.Rds")
m.nb.merge
getTrainPerf(m.nb.merge)
plot(m.nb.merge)
p.nb.merge <- predict(m.nb.merge, x.test.merge)
confusionMatrix(p.nb.merge, y.test.merge) #calc accuracies with confuction matrix on test set



##Logistic Regression (no parameters here, but will get cross validated perfomrance measures)
modelLookup("glm")
set.seed(543)
m.glm.merge<- train(y=y.train.merge, x=x.train.merge,
                    trControl = ctrl_merge,
                    metric = "ROC", tuneLength=15,#using AUC to find best performing parameters
                    method = "glm")
saveRDS(m.glm.merge, file = "m-glm-merge.rds")
m.glm.merge <- readRDS("m-glm-merge.Rds")
m.glm.merge
getTrainPerf(m.glm)
p.glm.merge<- predict(m.glm.merge,x.test.merge)
confusionMatrix(p.glm.merge,y.test.merge)



library(ipred)
set.seed(192)
modelLookup("treebag")
m.bag.merge <- train(y=y.train.merge, x=x.train.merge,
                     trControl = ctrl_merge,
                     metric = "ROC",tuneLength=15, #using AUC to find best performing parameters
                     method = "treebag")
saveRDS(m.bag.merge, file = "m-bag-merge.rds")
m.bag.merge <- readRDS("m-bag-merge.Rds")
m.bag.merge
p.bag.merge<- predict(m.bag.merge,x.test.merge)
confusionMatrix(p.bag.merge,y.test.merge)



library(randomForest)
set.seed(192)
modelLookup("rf")
m.rf.merge <- train(y=y.train.merge, x=x.train.merge,
                    trControl = ctrl_merge,
                    metric = "ROC", #using AUC to find best performing parameters
                    tuneLength=9,
                    method = c("rf") )
m.rf.merge
saveRDS(m.rf.merge, file = "m-rf-merge.rds")
m.rf.merge <- readRDS("m-rf-merge.Rds")
p.rf.merge<- predict(m.rf.merge,x.test.merge)
confusionMatrix(p.rf.merge,y.test.merge)



library(e1071)
m.svm.merge <- train(y=y.train.merge, x=x.train.merge,
                     trControl = ctrl_svm_merge,
                     metric = "ROC", tuneLength=15, #Accuracy over all classes
                     method = "svmRadial")
saveRDS(m.svm.merge, file = "m-svm-merge.rds")
m.svm.merge <- readRDS("m-svm-merge.Rds")
m.svm.merge
plot(m.svm.merge)
p.svm.merge<- predict(m.svm.merge, x.test.merge)
confusionMatrix(p.svm.merge, y.test.merge)

rValues <- resamples(list(rpart=m.rpart.merge, naivebayes=m.nb.merge, logistic=m.glm.merge, bag=m.bag.merge,rf=m.rf.merge, SVM=m.svm.merge))

str(m.rpart.merge)
rValues_test_merge <- resamples(list(rpart=p.rpart.merge, naivebayes=p.nb.merge, logistic=p.glm.merge, bag=p.bag.merge, rf=p.rf.merge, SVM=p.svm.merge))


summary(rValues_test_merge)



#need probability predictions not labels, let's predict again
rpart.prob.merge <- predict(m.rpart.merge, x.test.merge, type="prob")
nb.prob.merge <- predict(m.nb.merge, x.test.merge, type="prob")
glm.prob.merge <- predict(m.glm.merge, x.test.merge, type="prob")
bag.prob.merge <-predict(m.bag.merge, x.test.merge, type="prob")
rf.prob.merge<-predict(m.rf.merge, x.test.merge, type="prob")
svm.prob.merge <-predict(m.svm.merge, x.test.merge, type="prob")


??roc
#using no probability as positive class
rpart.roc.merge<- roc(y.test.merge, rpart.prob.merge$negative)
nb.roc.merge<- roc(y.test.merge, nb.prob.merge$negative)
glm.roc.merge<- roc(y.test.merge, glm.prob.merge$negative)
bag.roc.merge<- roc(y.test.merge, bag.prob.merge$negative)
rf.roc.merge<- roc(y.test.merge, rf.prob.merge$negative)
svm.roc.merge<- roc(y.test.merge, svm.prob.merge$negative)

#lets see auc
auc(rpart.roc.merge)
auc(nb.roc.merge)
auc(glm.roc.merge)
auc(bag.roc.merge)
auc(rf.roc.merge)
auc(svm.roc.merge)


#let's create an ROC plot with all combined
plot(rpart.roc.merge, col="black")
plot(nb.roc.merge, add=T, col="red")
plot(glm.roc.merge, add=T, col="blue")
plot(bag.roc.merge, add=T, col="pink")
plot(rf.roc.merge, add=T, col="orange")
plot(svm.roc.merge, add=T, col="green")

legend(x=.34, y=.3, cex=.5, legend=c("rpart","Naive Bayes", "Logistic","bag","rf", "SVM"), col=c("black", "red", "blue","pink","orange","green"), lwd=5)



####Visualizations for data set where neutral and positive are merged


ggplot(data=twitter.sub.merge, aes(x=airline, fill=airline_sentiment))+
  geom_bar(stat="count", position="stack")

