##############################################
############ Text data analysis #############
############################################

###Useful textbook: https://www.tidytextmining.com/index.html 

## Note: there are several approaches on how to do text analysis in R
#Here I show one such approach, which is most consistent with Tidyverse commands for data analysis

### Setting working directory 
#Use Session - set working directory - choose directory
#or use setwd() command

###Installing packages (only need to do once)
#install.packages('tidytext') 
#install.packages('tidyverse') 
#install.packages('stopwords') 
#install.packages('wordcloud') 
#install.packages('wordcloud2')
#install.packages('topicmodels') 
#install.packages('igraph')
#install.packages('ggraph')
#install.packages('Rcpp')
#install.packages('lubridate')
#install.packages('textdata')
#install.packages('htmlwidgets')
###Loading necessary packages 

library(tidytext) #To perform different operations with text data
library(tidyverse) #General data cleaning
library(stopwords) #Stopwords dictionary
library(wordcloud) #One way to plot wordclouds
library(wordcloud2) #Alternative way of producing wordcloud
library(lubridate) #To work with dates
library(topicmodels) #For topic modelling
library(igraph) #To plot network of words
library(ggraph) #To plot network of words
library(Rcpp) #technical package to plot graph
library(textdata)
library(htmlwidgets) #To save wordcloud2 as html file
###Loading data
#### Data source: https://www.kaggle.com/datasets/corrieaar/disinformation-articles
##This data comes from EUvsDisinfo project which explores misinformation shared in the EU
## and Eastern Partnership countries, primarily by pro-russian sources
#Time period between January 2015 and January 2020
#The data contains both the quotes from the articles with the fake information they published
#As well as the explanation of why this claim is fake
articles <- read_csv('5.Semantic Analysis/data.csv')



### (If you don't have a standard dataset) Loading text data with readLines command
## And then transforming it to the dataset
articles_txt <- readLines("5.Semantic Analysis/data.txt")
### And then you can convert this into dataset format
articles_txt_df <- tibble(lines = 1:12467, text = articles_txt)
remove(articles_txt, articles_txt_df)

#### Exploring data ###
names(articles)
View(articles)

#### Separating data into words
### We use 'bag of words' method, meaning that we perform analysis at the level of individual words
### First, we separate the text into words for each of the claims
#Also use lowercase letters so that the same word written with uppercase and with lowercase letter is treated the same
?unnest_tokens
articles_unnested <- articles |> unnest_tokens(word, claim_reviewed, to_lower = TRUE)

####Now we have a very large dataset, where there is a separate observation for each word in each review
### With this dataset we can perform further analysis

#### Stopwords ####
### Before we proceed with the further analysis we need to remove stopwords
##Stopwords are commonly used words that do not bring any additional value to the meaning of the text
#such as "the', 'and', 'or', etc. At the same time they pollute data with unnecessary information
#And also make the dataset much bigger and more timely to process
#So, before the analysis it is better to remove such words.
#Luckily, there are existing dictionaries with the list of such words, so you do not have to come up with the list yourself
#But given on the specific text you are working with, you may want to choose to add additional words to this list

##Creating a vector of stopwords
#There are stopwords available in different lanaguages, and for some languages there are several sources
#For more information see helpfile for stopwords command
stop_words <- stopwords(language = "en",source = "marimo")
stop_words

###We have a vector of stopwords, but in order to use it to remove them from our data, we need
#to convert it from vector to dataframe using tibble command
stop_words_df <- tibble(lines = 1:237, word = stop_words)

####Next, we remove the stopwords from the dataset, using the anti_join function
?anti_join
#It removes all rows from our initial dataset that are matched 
articles_cleaned <- articles_unnested |> anti_join(stop_words_df, by = "word")

### What if you want to expand this list & add additional words?
#You simply add the additional words to the to the stopwords vector that we have already created
#Additional stop words in this case are geographical names (eg countries, continents)
#since they are very commonly used, and we would like to analyse data both with and without them separately
stop_words_countries <- c(stop_words, 'europe', 'russia', 'eu', 'russian', 'united', 'states',
                          'american', 'usa', "syria", "ukraine", "kyiv", 'donbass',
                          'crimea', 'belarus', 'poland', 'western', 'us', 'ukrainian',
                          'eastern', 'west', 'donbas', 'moscow', 'european', 'germany', 'georgia', 'ukrainians', 'Union', 'belarusain')

stop_words_countries_df <- tibble(lines = 1:265, word = stop_words_countries)

#As before, removing these stop words from our data
articles_no_countries <- articles_unnested |> anti_join(stop_words_countries_df, by = "word")


### Exploring text data ###
#### What can we do with our text data now?
### 1) Let's see which are the most commonly used words
top_words <- articles_cleaned |> count(word) |> arrange(desc(n))
top_words_no_countries <- articles_no_countries |> count(word) |> arrange(desc(n))


####Wordclouds (2 ways)
### We can plot those words as a wordcloud
#Wordcloud represents the most common words in a certain text. 
#The size of the word represents its relative frequency
#there are 2 main packages of how to produce wordcloud
#1) Wordcloud package - for it you need to calculate the frequency of words
#before plotting
#You can also customize things like color, rotation, maximum number of words, check the help file
#Random order - whether the most common words are in the center
?wordcloud
wordcloud(
  words = top_words$word,
  freq = top_words$n,
  max.words = 100,
  random.order = FALSE
)
##Similiarly without country names 
#(here we use random order just to show the difference)
wordcloud(
  words = top_words_no_countries$word,
  freq = top_words_no_countries$n,
  max.words = 100,
  random.order = TRUE
)
##Alternative way is using wordcloud 2 package
#Here you just need to provide it with the dataframe
#It also produces the gif, but you can save it either as image or as webpage
#Another benefit here is that if you hover over the word it shows its frequency
#you can also change shape of the image, colors, font etc.
?wordcloud2
wordcloud_2 <- wordcloud2(top_words)
saveWidget(wordcloud_2, "wordcloud_2.html")
browseURL( "wordcloud_2.html")
wordcloud_2_no_count <- wordcloud2(top_words_no_countries)
saveWidget(wordcloud_2_no_count, "wordcloud_2_no_count.html")
browseURL( "wordcloud_2_no_count.html")


### Alternatively, you can also plot most frequent words with a more traditional bar chart
#First, let's limit only to top10 words
top_words_10 <- top_words |> top_n(10)
top_words_no_countries_10 <- top_words_no_countries |> top_n(10)


ggplot(top_words_10, aes(x = fct_reorder(word,n), y = n)) +
  geom_col() +
  coord_flip()




### Sentiment analysis ####
### Sentiment analysis allows you to track general sentiments of your text
## It does so by using the pre-existing dictionary, where each word is associated with a certain sentiment
##There are several dictionaries available in R, and they have different types of sentiments available
#From simply 'positive' and 'negative' to a wider range of sentiments
##Here we use the data without geographical names, since they do not have 
#sentiments associated to them
?get_sentiments
## We will use 3 different dictionaries just to see the different options
#The options we use are:
# bing  = only has positive/negative sentiments
#nrc = has a range of emotions
#afinn = has a quantitative scoring system

#First, we join these dictionaries to our dataset 
#Since we only want to keep those words from those dictionaries that are present in our dataset, we use left_join()
articles_no_countries <- articles_no_countries |> 
  left_join(get_sentiments("nrc"), by = "word") |> 
  rename(sentiment_nrc = sentiment) |> 
  left_join(get_sentiments('bing'), by = 'word') |> 
  rename(sentiment_bing = sentiment) |> 
  left_join(get_sentiments('afinn'), by = 'word') |> 
  rename(sentiment_afinn = value)

### Let's see what types of sentiments there are for each dictionary
unique(articles_no_countries$sentiment_nrc) #10 different sentiments
unique(articles_no_countries$sentiment_bing) #positive or negative sentiments
unique(articles_no_countries$sentiment_afinn) #Numeric values

##What can we do with these sentiments?
##Some ideas:
# 1) What are the most commonly used sentiments in this dataset?
sentiment_count <- articles_no_countries |>
  group_by(sentiment_nrc) |> 
  summarize(count = n()) |> 
  filter(!is.na(sentiment_nrc))

ggplot(sentiment_count, aes(x = fct_reorder(sentiment_nrc,count), y = count)) +
  geom_col() +
  coord_flip()

sentiment_count <- articles_no_countries |>
  group_by(sentiment_bing) |> 
  summarize(count = n()) |> 
  filter(!is.na(sentiment_bing))

ggplot(sentiment_count, aes(x = fct_reorder(sentiment_bing,count), y = count)) +
  geom_col() +
  coord_flip()

#When you use the dictionary with many categories, it seems like there are predominantly
#positive sentiments. But when you only use positive and negative sentiments, negative sentiment dominate
#because of many types of negative sentiments and different classification
#so be careful which dictionary you use


# 2) What are the most common words with positive and negative sentiments?
summary_word_sentiment <- articles_no_countries |> 
  group_by(sentiment_bing, word) |> 
  summarize(count = n()) |> 
  filter(!is.na(sentiment_bing)) |> 
  top_n(10)

ggplot(summary_word_sentiment, aes(x = fct_reorder(word,count), y = count, fill = sentiment_bing)) +
  geom_col() +
  coord_flip()


# 3) What is the average numeric value of sentiment?
mean(articles_no_countries$sentiment_afinn, na.rm = TRUE)

# 4) How did the level of sentiment change with time?
## We group our data by month & we calculate the average level of sentiment by article for each month
#Create new variables  -month and year of when the article was published
articles_no_countries <- articles_no_countries |> mutate(month = month(claim_published),
                                                          year = year(claim_published))
##Grouping by the month and year & calculating the average level of sentiment for each month
summary_by_date <- articles_no_countries |> group_by(month, year) |> summarise(mean_sent = mean(sentiment_afinn, na.rm = TRUE))

##Creating new variable of the date type, so that we can plot a graph
summary_by_date$date<-as.Date(with(summary_by_date,paste(year,month,"01",sep="-")),"%Y-%m-%d")

##Plotting the results
ggplot(summary_by_date, aes(x=date, y=mean_sent)) +
  geom_line() + 
  xlab("")+scale_x_date(date_labels = "%m-%Y", date_breaks = "4 month")


### Topic modelling
## Trying to uncover what are the different topics that are discussed in the text
#To do it, this algorithm looks at which words are most often used together (in the same document)
## For each topic we get the words that are most commonly associated with that topic
#Then, based on those words we can try to figure out what the topic is

## Before performing topic modelling we need to reformat our data
##Taking the full dataset (with country names)
## We need to transform it to Matrix format, which will display the frequency of each word used in each article
#This data format is called document term matrix or dtm
#Note that such data in this format may take more memory and is slower to process
#...1 here is article id variable 

articles_dtm <- articles_cleaned |>
  count(word, `...1`) |>
  cast_dtm(`...1`, word, n) |> 
  as.matrix()
#So, as a result we have a very large matrix (805 mb) 
#Let's look at the part of it, since it's too big to open fully
articles_dtm[1:6, 1000:1010]
#Very sparse matrix - most words are not present in most documents
#To run topic modelling run LDA command
#LDA = Latent Dirichlet allocation
#To read more about LDA 
#more technical details, formulas behind it check e.g. https://towardsdatascience.com/latent-dirichlet-allocation-lda-9d1cd064ffa2 
#Or check topic modelling chapter in the textbook suggested at the top of the script.
#We choose the number of topics we want to create ourselves

?LDA

### We set a seed (just any number) so that the output of the model
#is the same each time you run it
lda_model <- LDA(
  articles_dtm,
  k = 2,
  method =
    "Gibbs"
  ,
  control = list(seed = 16)
)

###You can extract 'betas' from the model  - per-topic-per-word probabilities
#Basically how strongly each word is associated with each topic
topics <- tidy(lda_model, matrix = "beta")
#We have 28 k unique word-topic combinations, but it makes sense to plot only those that are most important

top_terms <- topics |>
  group_by(topic) |>
  slice_max(beta, n = 10) |> 
  ungroup() |>
  arrange(topic, -beta)

top_terms |>
  mutate(term = reorder_within(term, beta, topic)) |>
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

### Note, that many terms here are repeated in both topics
#So perhaps, it is better to plot the log ratios of betas, so that we
#know which topic is more associated with which words

beta_wide <- topics |>
  mutate(topic = paste0("topic", topic)) |>
  pivot_wider(names_from = topic, values_from = beta) |> 
  filter(topic1 > .001 | topic2 > .001) |>
  mutate(log_ratio = log2(topic2 / topic1))

### We then select top terms with the most negative and the most positive values and plot them
beta_wide |>
  group_by(direction = log_ratio > 0) |>
  slice_max(abs(log_ratio), n = 10) |> 
  ungroup() |>
  mutate(term = reorder(term, log_ratio)) |>
  ggplot(aes(log_ratio, term)) +
  geom_col() +
  labs(x = "Log2 ratio of beta in topic 2 / topic 1", y = NULL)

##The words which are most distinct about first topic are related to europe/eu/west
##while the words in the second topics are seem to be related to the war in Syria

## Bi-grams and n-grams
## So far our analysis focused on individual words. However, sometimes looking at individual 
#words is not enough, as some phrases have a different meaning (e.g. "gross domestic product" vs just "gross")
#Probably the most common example were combination of words changes the meaning as compared to individual words
#is negation terms (eg "no", "not", etc.). Presence of such negation terms change the meaning
#of the word completely, which is especially important for sentiment analysis

#Therefore, let's look how we can perform analysis at the level of pairs (or more) of words

#To do it we use the same unnest_token command as we used before, but specify that we 
#want to look at pairs of words ('ngrams') rather than individual words
articles_bigrams <- articles |> unnest_tokens(word, claim_reviewed, token = "ngrams", n = 2, to_lower = TRUE)

#Now the word column has 2 words rather than 1 for each row. It takes each pair of words
#that are in a row in a text and creates a new row for each such pair. 
#(So that each word appears in 2 observations with both words that are adjacent to it)
View(articles_bigrams)
## As we did previously we can look at the most popular bigrams
#Most common of them are not meaningful - we did not filter stopwords
articles_bigrams |>
  count(word, sort = TRUE)

### Can we filter stopwords? Yes, but there is no dictionary (to the best of my knowledge)
#with the pairs of words. Therefore, instead, we can separate each pair into separate words
#and remove those observations for which at least one of the words is in the list of the stopwords
#We separate articles
articles_bigrams_separated <- articles_bigrams |>
  separate(word, c("word1", "word2"), sep = " ")
#We filter out observations for which at least one words is in the stopwords list
bigrams_filtered <- articles_bigrams_separated |>
  filter(!word1 %in% stop_words_df$word) |>
  filter(!word2 %in% stop_words_df$word)

#### Now we recount the most frequent bigrams
#And we see that the top bigrams are more meaningful
bigram_counts <- bigrams_filtered |> 
  count(word1, word2, sort = TRUE)

View(bigram_counts)

#### Why use bigrams vs individual words? 
#1) See what are the most common words that are used in conjunction with a particular word of interest
#(Let's say you want to see whether the word us is more commonly used meaning the USA or word 'us")
bigrams_filtered |>
  filter(word1 == "us") |>
  count(word2, sort = TRUE)
#2) Correct mistakes in a sentiment analysis
#since we consider individual words, we ignore that when word is preceeded by "not" it has
#an opposite meaning
#Let's see whether it substantially affected our sentiment analysis results
#We use quantitative sentiments dictionary, so that we can quantify the effect of 
#our errors

##First, we select word pairs which start with the word "not"
#Then we attach the sentiment based on the second word of the pair
#Finally we calculate the number of observations for each unique pair
not_words <- articles_bigrams_separated |>
  filter(word1 == "not") |>
  inner_join(get_sentiments('afinn'), by = c(word2 = "word")) |>
  count(word2, value, sort = TRUE)

#### Next we calculate the "contribution" to the error in our sentiment analysis of each word pair
#This is done by simply multiplying the frequency of each pair of words that start by "not" by the intensity of the sentiment of the second word of a pair
#Then we select only 20 words with the highest absolute level of contribution & plot the results
not_words |>
  mutate(contribution = n * value) |>
  arrange(desc(abs(contribution))) |>
  head(20) |>
  mutate(word2 = reorder(word2, contribution)) |>
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"")

##You can also calculate whether overall your sentiment analysis was biased to the positive or to the negative side

not_words <- not_words |> mutate(contribution = n*value)
sum(not_words$contribution)
## THe result is positive -> so sentiment analysis showed that text was more positive than it was in reality

#You can also repeat the same exercise with other negation words

### Another thing we can do with bigrams is to plot graphs
# With graphs we can visually illustrate which words each other word is most commonly used together

#We use bigram counts dataset, select only most popular words and convert dataset to the proper format for 
#plotting graphs
bigram_graph <- bigram_counts |>
  filter(n > 20) |>
  graph_from_data_frame()

bigram_graph


##Finally we plot the graph
#Set seed for replicability
set.seed(321)

?ggraph
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)



####Exercises ##
## Find a text dataset that interests you 
## e.g. on Kaggle or use your own data!
# and perform textual analysis

##Find Ukrainian stop-words 