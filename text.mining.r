library(tidyverse)
library(wordcloud)
library(qdap) # text mining: freq_terms
library(tm) # text mining: converts vectors to Source objects; preprocessing/cleaning functions
library(tidytext) # text mining: tokenizes easily
devtools::install_github("JBGruber/rwhatsapp")
library("rwhatsapp")
library(SnowballC) #provides a wordStem() function which
#provides a wordStem() function, which for a character
# vector, returns the same vector of terms in its root form. For example, the function
# correctly stems the variants of the word learn, as described previously
 
#load the whatsapp text file
texts <- readLines("whatsapp.txt")
texts2 <- readLines("whatsapp.txt")

#create the corpus using tm
docs <- Corpus(VectorSource(texts))

docs[[2]]$content # double brackets to get metadata on second tweet; $content (or single brackets)


#Common preporocessing functions
#---------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
#applying preprocessing steps to a corpus using tm package
# using tm_map() makes it easier to apply cleaning functions to an entire corpus

# create a custom function so that you can apply the same functions over multiple corpora
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower)) # use content_transformer when using base R functions like tolower()
  corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stemDocument)
  return(corpus)
}

#apply preprocessing function to textCorpus (corpus of qual data)
clean_corp <- clean_corpus(docs)

## see how it's cleaned up one of the stories
clean_corp[[22]]$content

# content_transformer function modify the content of an R object.
# content_transformer is used to create a wrapper to get and set the content of
#text documents
trans <- content_transformer(function(x, pattern) gsub(pattern, "", x))
clean_corp <- tm_map(clean_corp, trans, "/")
clean_corp <- tm_map(clean_corp, trans, "@")
clean_corp <- tm_map(clean_corp, trans, "\\|")

#-------------------------------------------------------------------------------
## create TermDocumentMatrix (TDM) or DocumentTermMatrix (DTM) to get frequency matrices of corpus
qual_tdm <- TermDocumentMatrix(clean_corp)

## convert dtm to matrix so that it's easier to look at and manipulate
qualtdm_matrix <- as.matrix(qual_tdm)
qualtdm_matrix[1:4,1:10]

#calculate total frequency of terms and sort in decreasing order
freqTerms <- rowSums(qualtdm_matrix) %>% sort(., decreasing = TRUE)
head(freqTerms)

# turn into a dataframe
freqTermsdf <- freqTerms %>% data.frame()
names(freqTermsdf) <- "number"
freqTermsdf <- freqTermsdf %>% mutate(terms = row.names(.)) %>% arrange(desc(number))

library(ggthemes)
#create bar chart, reorder and labelling
ggplot(freqTermsdf[1:20,], aes(x = reorder(terms,number), y=number)) + geom_bar(stat = "identity") +
  geom_text(aes(label = number),size = 3, hjust = 2, color = "white") + coord_flip() +
  labs(title = "Words with greatest frequency", x = "Number of mentions", y = "terms") +
  theme_tufte(base_size = 11)
  
#-------------------------------------------------------------------------------
##Simple word cloud
library(wordcloud)

# create word cloud with three colors (red and gray provide a good contrast for high and low freq words)
wordcloud(freqTermsdf$term, 
          freqTermsdf$num,
          max.words = 25,
          scale = c(3,.9), # indicates the range of the size of the words
          colors = brewer.pal(8, "Dark2"))

# load the viridisLite package. The viridisLite color schemes are perceptually-uniform, both in regular form and when converted to black-and-white.
# The colors are also designed to be perceived by readers with color blindness.
library(viridisLite)
color_pal <- viridis(n=3)
wordcloud(term_frequency$term, 
          term_frequency$num, 
          max.words = 20,
          colors = color_pal)

---------------------------------------------------------------------------------
#Find common words
#Say you want to visualize common words across multiple documents. You can do this with commonality.cloud().
#Each of our coffee and chardonnay corpora is composed of many individual tweets.
#To treat the coffee tweets as a single document and likewise for chardonnay, 
#you paste() together all the tweets in each corpus along with the parameter collapse = " ".
#This collapses all tweets (separated by a space) into a single vector. Then you
#can create a single vector containing the two collapsed documents.

library(tidyverse)
sf <- read.csv("progs4.2018.csv")
sfjoin <- full_join(sf, sfqual, by = "Program.Name") %>% filter(!is.na(Comments))

qualCG <- sfjoin %>% filter(Practice.Area == "Communities and Governance")
qualEd <- sfjoin %>% filter(Practice.Area == "Education")

qualCG <- paste(qualCG$Comments, collapse = " ")
qualEd <- paste(qualEd$Comments, collapse = " ")

all_Comments <- c(qualCG, qualEd)


# Convert to a vector source
all_Comments <- VectorSource(all_Comments)

# Create all_corpus
all_corpus <- VCorpus(all_Comments)

#-------------------------------------------------------

# Create a commonality cloud to look at common words across documents
## try this across impact stories for each practice

# Create all_coffee
all_coffee <- paste(coffee_tweets$text, collapse = " ")

rwa_read("whatsapp.txt")

# Create all_chardonnay
all_chardonnay <- paste(chardonnay_tweets$text, collapse = " ")

# Create all_tweets
all_tweets <- c(all_coffee, all_chardonnay)

# Convert to a vector source
all_tweets <- VectorSource(all_tweets)

# Create all_corpus
all_corpus <- VCorpus(all_tweets)

# Clean the corpus
all_clean <- clean_corpus(all_corpus)

# Create all_tdm
all_tdm <- TermDocumentMatrix(all_clean)

# Create all_m
all_m <- as.matrix(all_tdm)

# Print a commonality cloud
commonality.cloud(all_m, max.words = 100, colors = "steelblue1")

# DISSIMILAR COMPARISON CLOUD
##Say you want to visualize the words not in common. To do this, you can also use comparison.cloud() 
#and the steps are quite similar with one main difference.
#Like when you were searching for words in common, you start by unifying the tweets into distinct corpora 
#and combining them into their own VCorpus() object. Next apply a clean_corpus() function and organize it into a TermDocumentMatrix.
#To keep track of what words belong to coffee versus chardonnay, you can set the column names of the TDM 

# polarized tag cloud -----------------------------------------------------

1+1
# POLARIZED TAG CLOUD (pyramid.plot)

# Commonality clouds show words that are shared across documents. One interesting 
#thing that they can't show you is which of those words appear more commonly in 
#one document compared to another. For this, you need a pyramid plot; these can 
#be generated using pyramid.plot() from the plotrix package.

#First, some manipulation is required to get the data in a suitable form. This is most easily 
#done by converting it to a data frame and using dplyr. Given a matrix of word counts, 
#as created by as.matrix(tdm), you need to end up with a data frame with three columns:
  
  # The words contained in each document.
  # The counts of those words from document 1.
  # The counts of those words from document 2

top25_df <- all_tdm_m %>%
  # Convert to data frame
  as_data_frame(rownames = "word") %>% 
  # Keep rows where word appears everywhere
  filter_all(all_vars(. > 0)) %>% 
  # Get difference in counts
  mutate(difference = chardonnay - coffee) %>% 
  # Keep rows with biggest difference
  top_n(25, wt = difference) %>% 
  # Arrange by descending difference
  arrange(desc(difference))

pyramid.plot(
  # Chardonnay counts
  top25_df$chardonnay, 
  # Coffee counts
  top25_df$coffee, 
  # Words
  labels = top25_df$word, 
  top.labels = c("Chardonnay", "Words", "Coffee"), 
  main = "Words in Common", 
  unit = NULL,
  gap = 8,
)

# ----------------------------------------------------------------------------------

# Visualize WOR NETWORKS

#Another way to view word connections is to treat them as a network, similar to a social network. 
#Word networks show term association and cohesion. A word of caution: these visuals can become very dense and hard to interpret visually.

#In a network graph, the circles are called nodes and represent individual terms, 
#while the lines connecting the circles are called edges and represent the connections between the terms.

#For the over-caffeinated text miner, qdap provides a shorcut for making word networks. 
#The word_network_plot() and word_associate() functions both make word networks easy!
  
#The sample code constructs a word network for words associated with "Marvin".

# Word association
word_associate(coffee_tweets$text, match.string = "barista", 
               stopwords = c(Top200Words, "coffee", "amp"), 
               network.plot = TRUE, cloud.colors = c("gray85", "darkred"))

# Add title
title(main = "Barista Coffee Twee Associations") %>% %>% 
 text <- filter(x=5)

##------------------------------
#use tidytext
library(lubridate) # works well with dates
chat <- rwa_read("whatsapp.txt")  

# peaks and valleys of whatsapp messaging
chat %>%
  mutate(day = date(time)) %>%
  count(day)  %>%
  ggplot(aes(x = day, y = n)) +
  geom_bar(stat = "identity") +
  ylab("") + xlab("")

# Number of messages per author
chat %>% filter(author != "NA") %>%
  mutate(day = date(time)) %>%
  count(author) %>%
  ggplot(aes(x = reorder(author, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ggtitle("Number of messages") +
  geom_text(aes(label = n),size = 4, hjust = 2, color = "white") + coord_flip() +
  geom_hline(yintercept = 0, size =1, colour = "#9ba7a7") +
  theme_minimal() +
  theme(axis.title.y=element_blank(),
        plot.title = element_text(size = 12, hjust = -.3),
        axis.text.x=element_blank(),# axis titles
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(size = 12)) +
  theme(panel.grid.major = element_line(color = "#d8e6e6" ),
        panel.grid.minor = element_line("#d8e6e6"))

#Hannah has sent about 62% more text messages than me or over 3,000 more messages! 

https://camo.githubusercontent.com/2b8330206cd79c425a2596e67dbf7032f756d4db/68747470733a2f2f692e696d6775722e636f6d2f6f3345415733782e706e67

https://github.com/sudharsan13296/Whatsapp-analytics/blob/master/WhatsappAnalysis.R

https://rpubs.com/lgeorge2651/151203