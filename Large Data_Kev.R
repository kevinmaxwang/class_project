#Package
library(readr)
library(tidyverse)
library(tidytext)
library(textdata)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(tm)
library(SnowballC)
library(viridisLite)
library(topicmodels)
library(quanteda)

# Import data
setwd("~/Desktop")
cs = read.csv("Consumer_Complaints.csv",header=T, na.strings=c("","NA"))

cs_words = cs[!is.na(cs$Consumer.complaint.narrative),]


#Split data
set.seed(1111)
sample_size <- floor(0.75 * nrow(df))
train_ind <- sample(nrow(df), size = sample_size)
train <- df[train_ind, ]
test <- df[-train_ind, ]

### Explore
df = cs[sample(nrow(cs), 10000), ]

df = df[!is.na(df$Consumer.complaint.narrative),]

summary(df)
sort(table(df$Issue))
sort(table(df$Sub.issue))
table((df$Issue))
sum(is.na(df$Company.public.response))
sum(is.na(df$Company.response.to.consumer))

#Combine products 
df$Product[df$Product == "Credit reporting"] = "Credit reporting, credit repair services, or other personal consumer reports"
df$Product[df$Product == "Credit card"] = "Credit card or prepaid card"
df$Product[df$Product == "Bank account or service"] = "Checking or savings account"
df$Product[df$Product == "Prepaid card"] = "Credit card or prepaid card"
df$Product[df$Product == "Money transfers"] = "Money transfer, virtual currency, or money service"
sort(table(df$Product),decreasing = TRUE)

new_df = as.data.frame(head(sort(table(df$Product),decreasing = TRUE),10))

# See what products most people complain
ggplot(new_df, aes(x=Var1, y = Freq)) + 
  geom_bar(stat="identity")+ xlab("Products") + ylab("Counts")+
  coord_flip() +ggtitle("Products Problems") 


##Missing data
table(is.na(cs$Consumer.complaint.narrative))# 949728 a lot of missing 30%

table(is.na(cs$Company.public.response))

table(is.na(cs$Company.response.to.consumer))

## Function change text to cleancorpus
ct_corpus <- function(text) {
  corpus <- Corpus(VectorSource(text))
  corpus = tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, words = c(stopwords("en"),"xxxx","xxxxxxxx"))
  corpus = tm_map(corpus, stripWhitespace)
  return(corpus)
}

### WordCloud
corpus = ct_corpus(df$Consumer.complaint.narrative)
matrix = as.matrix(TermDocumentMatrix(corpus))
Term_f = rowSums(matrix)
word_f = data.frame(term= names(Term_f), num = Term_f)
color_pal<- cividis(5)
wordcloud(word_f$term, word_f$num,max.words = 100, color = color_pal)



##Commonality.cloud
### Word Freq
class(Term_f)
term_freq = sort(Term_f,decreasing = TRUE)
class(term_freq)


###tokenization
#clean and get ready
# Stem the tokens

custom <- add_row(stop_words, word = c("xxxx",'xx'), lexicon = "custom")

df$Consumer.complaint.narrative = as.character(df$Consumer.complaint.narrative)
words_tokens <- df %>%
  tidytext::unnest_tokens(output = "word", token = "words", input = Consumer.complaint.narrative) %>%
  anti_join(custom) %>%
  mutate(word = wordStem(word))

words_tokens %>%
  count(word, sort = TRUE)

# Create a document term matrix 
words_matrix <- words_tokens %>%
  count(Complaint.ID, word) %>%
  cast_dtm(document = Complaint.ID, term = word,
           value = n, weighting = tm::weightTf)

# Print the matrix details 

less_sparse_matrix <-
  removeSparseTerms(words_matrix, sparse =0.99)


##LDA
sentence_lda <-
  LDA(words_matrix, k = 5, method = 'Gibbs', control = list(seed = 1111))

# Extract the beta matrix 
sentence_betas <- tidy(sentence_lda, matrix = "beta")

# Topic #2
sentence_betas %>%
  filter(topic == 2) %>%
  arrange(-beta)
# Topic #3
sentence_betas %>%
  filter(topic == 3) %>%
  arrange(-beta)

# Topic #4
sentence_betas %>%
  filter(topic == 4) %>%
  arrange(-beta)


# Extract the beta and gamma matrices
sentence_betas <- tidy(sentence_lda, matrix = "beta")
sentence_gammas <- tidy(sentence_lda, matrix = "gamma")

# Explore Topic 5
sentence_betas %>%
  filter(topic == 5) %>%
  arrange(-beta)

# Explore Topic 5 Gammas
sentence_gammas %>%
  filter(topic == 5) %>%
  arrange(-gamma)

sample_size <- floor(0.90 * nrow(words_matrix))
set.seed(1111)
train_ind <- sample(nrow(words_matrix), size = sample_size)
train <- words_matrix[train_ind, ]
test <- words_matrix[-train_ind, ]

sentence_lda <-
  LDA(words_matrix, k = 25, method = 'Gibbs', control = list(seed = 1111))

# Train
perplexity(sentence_lda, newdata = train) 
# Test
perplexity(sentence_lda, newdata = test) 

values = c()
for(i in c(2:50)){
  sentence_lda <-
    LDA(words_matrix, k = i, method = 'Gibbs', 
        control = list(iter = 25, seed = 1111))
    values = c(values, perplexity(sentence_lda,newdata = test))
}

plot(c(2:50), values, main= "Perplexity for Topics",
     xlab = "Number of Topics", ylab = "Perplexity")


## pick 24 topics

###
# Focus on geographic info
# Focus on time and seasonality
# Focus on prediction of issue
# Focus on prediction of timeliness of response 


###Sentiment Analysis
sentiments
get_sentiments("nrc")
#AFINN: score from -5 to 5
# bing: positive / negative label for all words
# nrc: labels words as fear, joy, anger

# which product contains the most negativity
words_tokens %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(Product)%>%
  summarise(sentiment = mean(value)) %>%
  arrange(sentiment)


# Which issue contains the most negativity
words_tokens %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(Issue)%>%
  summarise(sentiment = sum(value)) %>%
  arrange(sentiment)

# Which Company contains the most negativity
words_tokens %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(Company)%>%
  summarise(sentiment = sum(value)) %>%
  arrange(sentiment)

###  Number of negative words and proportion
word_total = words_tokens %>%
  group_by(Issue) %>%
  count()
words_tokens %>%
  inner_join(get_sentiments("bing"))%>%
  group_by(Issue)%>%
  count(sentiment)%>%
  filter(sentiment == 'negative')
  transform(p = n/word_total$n) %>%
  arrange(desc(p))

word_total = words_tokens %>%
  group_by(Product) %>%
  count()
words_tokens %>%
  inner_join(get_sentiments("bing"))%>%
  group_by(Product)%>%
  count(sentiment)%>%
  transform(p = n/word_total$n) %>%
  arrange(desc(p))



## Word emdeddings 
#word 2vec 
#words with similar meaning will close together
library(h2o)
h2o.init()
h2o_object = as.h2o(df)
words <-h2o.tokenize()

### meeting
#explore trend in this issue 
#explore possible connection 
#not only focus on 30%
#decide to use topic model
#business use case
#EDA end next week 
#show story 


library(lubridate)
df$month = month(as.POSIXlt(df$Date.received, format="%m/%d/%Y"))
df$year = year(as.POSIXlt(df$Date.received, format="%m/%d/%Y"))


df$month <- factor(df$month, ordered = TRUE, levels = c(1:12))
df$year <- factor(df$year, ordered = TRUE, 
                  levels = c(2011 : 2019))

var = as.data.frame(head(sort(table(df$Issue),decreasing = TRUE),10))
issue = var$Var1 = as.character(var$Var1)
df$Issue = as.character(df$Issue)

df_issue = df[df$Issue %in% issue, ]


ggplot(df_issue, aes(x= month, fill = Issue)) + 
  geom_bar(position="dodge")

ggplot(df_issue, aes(x= year, fill = Issue)) + 
  geom_bar(position="dodge")


install.packages("flexdashboard")



#You can go back and recategorize it later as patterns emerge in the rest of the data)