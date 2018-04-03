setwd("C:\\Users\\shreyas\\Desktop\\WorkSpace\\IVY-R\\WorkDir\\SMS-Spam")
library(tm)
sms_raw <- read.csv("sms_spam.csv" , stringsAsFactors = FALSE)

str(sms_raw)

names(sms_raw)

sms_raw <- sms_raw[,1:2]
View(sms_raw)

colnames(sms_raw) <- c("Type" , "Text")

str(sms_raw)
sms_raw$Type <- factor(sms_raw$Type)

str(sms_raw$Type)

#frequency table os ham and spam
table(sms_raw$Type)

prop.table(table(sms_raw$Type))
#86.5% are ham :: 13.4% are spam

#cleaning and standardizing text data using vcorpus()

sms_corpus <- VCorpus(x= VectorSource(sms_raw$Text))

#count characters in each document
inspect(sms_corpus[1:5])

#View the actual message

as.character(sms_corpus[[1]])
lapply(sms_corpus , as.character)

#clean- remove numbers and punctuations

Corpus_clean <- sms_corpus
Corpus_clean <- tm_map(x = Corpus_clean , FUN = removeNumbers)
Corpus_clean <- tm_map(x = Corpus_clean, FUN = tolower) #convert to lower
Corpus_clean <- tm_map(x= Corpus_clean , FUN = removePunctuation)

#remove stop words
stopwords() #174 stopwords
Corpus_clean <- tm_map(x = sms_corpus ,FUN =removeWords, stopwords())

#stemming
install.packages("SnowballC")
library(SnowballC)

Corpus_clean <-tm_map(x = Corpus_clean , FUN = stemDocument)

#Remove extra white spaces
Corpus_clean <- tm_map(x = Corpus_clean , FUN = stripWhitespace)

#differentiate data before and after cleaning
as.character(sms_corpus[[1]])
as.character(Corpus_clean[[1]])

#Create a Document Term Matrix (DTM)

dtm <- DocumentTermMatrix(Corpus_clean)
dtm

#Data preparation - creating training and test datasets
dtm_train <- dtm[1:4169, ]
dtm_test <- dtm[4170:5559, ]


# Create vectors with labels for the training and test set
train_labels <- sms_raw[1:4169, ]$Type
test_labels <- sms_raw[4170:5559, ]$Type

str(train_labels)
str(DTM_train)
# Check proportion of ham and spam is similar on the training and test set
prop.table(table(train_labels))
prop.table(table(test_labels))

#Visualzing text data - WORD CLOUDS
install.packages("wordcloud")
library(wordcloud)
wordcloud(Corpus_clean , min.freq = 100 , random.order = FALSE , colors = c("red","blue","green","cyan","black" ,"yellow" , "pink" ,"orange" ) )

#subsetting data
spam <- subset(sms_raw , Type == "spam")
spam
ham <- subset(sms_raw , Type == "ham")
ham

wordcloud(spam$Text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$Text, max.words = 40, scale = c(3, 0.5))

#creating indicator features forfrequent words
sms_freq_words <- findFreqTerms(dtm_train , lowfreq = 5)

# Filter DTM to only have most frequent words
sms_freq_words_train <- dtm_train[,sms_freq_words]
sms_freq_words_test <- dtm_test[,sms_freq_words]

dim(sms_freq_words_train)

# Convert numeric values into categorical values
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

sms_train <- apply(sms_freq_words_train, MARGIN = 2,
                   convert_counts)
sms_test <- apply(sms_freq_words_train, MARGIN = 2,
                   convert_counts)

str(sms_test)
#training a model on the data
library(e1071)

unlist(sms_train)
sms_classifier <- naiveBayes(sms_train , train_labels)
class(sms_train)
typeof(sms_test_pred)

#evaluate model performance
sms_test_pred <- predict(sms_classifier , sms_test , type = "class")
length(sms_test_pred)
length(test_labels)
class(sms_test_pred)
                            
library(gmodels)
CrossTable(sms_test_pred[1:1390] , test_labels , prop.chisq = FALSE , prop.t = FALSE, dnn = c('Predicted' , 'Actual'))

#improving model performance

sms_classifier2 <-naiveBayes(sms_train , train_labels , laplace = 1)

sms_test_pred2 <- predict(sms_classifier2 , sms_test)

CrossTable(sms_test_pred2[1:1390] , test_labels , prop.chisq = FALSE , 
           prop.t = FALSE , dnn = c('Predicted' ,'Actual'))


