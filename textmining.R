#required packages
library(tm)
library(SnowballC)
library(dplyr)
library(qdap)

##upload the dataset 
ibis=read.csv("ibishotel.csv", stringsAsFactors = FALSE)
View(ibis)
str(ibis)

##frequency of the words in the Review column 
fr=freq_terms(ibis)
fr
plot(fr)

##Creat documents
ibiscr=Corpus(VectorSource(ibis$Reviews))
ibiscr[[2]]$content


#Preprocessing
#1. trun all words to lower case
ibiscr= ibiscr %>%
  tm_map(tolower)
ibiscr[[2]]$content

#2. Remove punctuations
ibiscr = ibiscr %>%
  tm_map(removePunctuation)
ibiscr[[2]]$content

#3. removing the stopwords 
stopwords("english")
ibiscr =ibiscr %>%
  tm_map(removeWords, c("hotel", "koblenz", "ibis", stopwords("english")))
ibiscr[[2]]$content

#4. Remove numbers
ibiscr = ibiscr %>%
  tm_map(removeNumbers)
ibiscr[[2]]$content

#5. stripping the white space
ibiscr= ibiscr %>%
  tm_map(stripWhitespace)
ibiscr[[2]]$content


#6. Stemming
ibiscr = ibiscr %>%
  tm_map(stemDocument)
ibiscr[[2]]$content

fr1=freq_terms(ibiscr, 5)   #5 most frequent words
plot(fr1)


#Feature Extraction 
ibisfreq=DocumentTermMatrix(ibiscr)

dim(ibisfreq)    #39 rows and 289 variables

inspect(ibisfreq)

inspect(ibisfreq)[1:10, 1:10]   #document 1:4 columns 1:3

findFreqTerms(ibisfreq)
l=findFreqTerms(ibisfreq, lowfreq = 8)
l
length(l)
##we have many features with too many zeros, high sparsity
ibissparse=removeSparseTerms(ibisfreq, 0.80)  
##keep only the terms that appears in 20% or more of the feedback/documents/columns
dim(ibissparse)

inspect(ibissparse)

##convert it to dataframe 
ibis_review=as.data.frame(as.matrix(ibissparse))
View(ibis_review)
dim(ibis_review)

###visualize the freq of terms

ibis_names=colnames(ibis_review)


ibis_freq=c()

for (i in 1:5){
  ibis_freq[i]=sum(ibis_review[,i])
}
ibis_freq
barplot(ibis_freq,
        col=rainbow(5), 
        names.arg = ibis_names, 
        ylim=c(0,20))

#Supervised
####Sentiment analysis 
#create your sentiment variable
View(ibis)
rate=ibis$Rating
View(ibis_review)
ibis_review= ibis_review %>%
  mutate(y=ifelse(rate>3, "Positive","Negative"))
View(ibis_review)

##which terms derive positive rating
#Decision tree Model
library(rpart)
library(rpart.plot)
ibis_sent=rpart(y~.,
                data = ibis_review,
                method = "class")
#prp(ibis_sent)
rpart.plot(ibis_sent)



