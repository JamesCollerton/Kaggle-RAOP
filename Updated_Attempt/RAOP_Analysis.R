#Sets working directory to the relevant space and brings up the appropriate library.

# required_packages <- c("tm", "rjson", "KernSmooth", "SnowballC", "wordcloud")

# install.packages(required_packages)

library('tm')
library("rjson")
library("KernSmooth")
library('SnowballC')
library('wordcloud')

setwd("~/Documents/Programming/Git_Folders/Kaggle-RAOP/Updated_Attempt/Data")

#Reads in the relevant .json file

meta_data <- fromJSON(file = "train.json")

#This part just makes a nice dataframe and gives nice titles.

meta_dataframe <- data.frame("giver_username_if_known" = unlist(lapply(meta_data, function(l) l[[1]])),                             
                             "number_of_downvotes_of_request_at_retrieval" = unlist(lapply(meta_data, function(l) l[[2]])),         
                             "number_of_upvotes_of_request_at_retrieval" = unlist(lapply(meta_data, function(l) l[[3]])),           
                             "post_was_edited" = unlist(lapply(meta_data, function(l) l[[4]])),                                     
                             "request_id" = unlist(lapply(meta_data, function(l) l[[5]])),                                          
                             "request_number_of_comments_at_retrieval" = unlist(lapply(meta_data, function(l) l[[6]])),             
                             "request_text" = unlist(lapply(meta_data, function(l) l[[7]])),                                        
                             "request_text_edit_aware" = unlist(lapply(meta_data, function(l) l[[8]])),                             
                             "request_title" = unlist(lapply(meta_data, function(l) l[[9]])),                                       
                             "requester_account_age_in_days_at_request" = unlist(lapply(meta_data, function(l) l[[10]])),            
                             "requester_account_age_in_days_at_retrieval" = unlist(lapply(meta_data, function(l) l[[11]])),          
                             "requester_days_since_first_post_on_raop_at_request" = unlist(lapply(meta_data, function(l) l[[12]])),  
                             "requester_days_since_first_post_on_raop_at_retrieval" = unlist(lapply(meta_data, function(l) l[[13]])),
                             "requester_number_of_comments_at_request" = unlist(lapply(meta_data, function(l) l[[14]])),             
                             "requester_number_of_comments_at_retrieval" = unlist(lapply(meta_data, function(l) l[[15]])),           
                             "requester_number_of_comments_in_raop_at_request" = unlist(lapply(meta_data, function(l) l[[16]])),     
                             "requester_number_of_comments_in_raop_at_retrieval" = unlist(lapply(meta_data, function(l) l[[17]])),   
                             "requester_number_of_posts_at_request" = unlist(lapply(meta_data, function(l) l[[18]])),                
                             "requester_number_of_posts_at_retrieval" = unlist(lapply(meta_data, function(l) l[[19]])),              
                             "requester_number_of_posts_on_raop_at_request" = unlist(lapply(meta_data, function(l) l[[20]])),        
                             "requester_number_of_posts_on_raop_at_retrieval" = unlist(lapply(meta_data, function(l) l[[21]])),      
                             "requester_number_of_subreddits_at_request" = unlist(lapply(meta_data, function(l) l[[22]])),           
                             "requester_received_pizza" = unlist(lapply(meta_data, function(l) l[[23]])),                               
                             "requester_upvotes_minus_downvotes_at_request" = unlist(lapply(meta_data, function(l) l[[25]])),        
                             "requester_upvotes_minus_downvotes_at_retrieval" = unlist(lapply(meta_data, function(l) l[[26]])),      
                             "requester_upvotes_plus_downvotes_at_request" = unlist(lapply(meta_data, function(l) l[[27]])),         
                             "requester_upvotes_plus_downvotes_at_retrieval" = unlist(lapply(meta_data, function(l) l[[28]])),                                       
                             "requester_username" = unlist(lapply(meta_data, function(l) l[[30]])),                                  
                             "unix_timestamp_of_request" = unlist(lapply(meta_data, function(l) l[[31]])),                           
                             "unix_timestamp_of_request_utc" = unlist(lapply(meta_data, function(l) l[[32]])))


###################################################################
#Exploratory analysis##############################################
###################################################################

###################################################################
#The first trend to be explored is if having a lot of downvotes would lead to a dimished
#probability of recieving pizza.
###################################################################

check_voting_impact <- function(vote_type)
{
	votes <- data.frame(meta_dataframe$requester_received_pizza, 
                      vote_type)

	vote_table <- table(votes)

	xvals <- unique(as.numeric(colnames(vote_table)))
	yvals <- vote_table["TRUE",]/(vote_table["TRUE",] + vote_table["FALSE",])
  
  xvals <- xvals[yvals > 0]
	yvals <- yvals[yvals > 0]
  
	plot(xvals, yvals, col = "dodgerblue", xlab = "Number of Downvotes",
     	 ylab = "Proportion of Successful Pizza Requests", main = "Corellation between Number of Votes
     	 and Proportion of Successful Pizza Requests", pch = 16)

	#Fits a linear model to the data whilst ignoring all values that jet off to infinity.

	lm_fit_df <- data.frame(xvals, yvals)
	lm_fit_df <- lm_fit_df[which(lm_fit_df$yvals != Inf),]

	fit <- lm(lm_fit_df$yvals ~ lm_fit_df$xvals)
	abline(data.frame(summary(fit)$coefficients)$Estimate, col = "firebrick1", lwd = 1.5)

	# Finally look at Pearson's.

	cor(yvals, xvals, method = "pearson")
}

check_voting_impact(meta_dataframe$number_of_downvotes_of_request_at_retrieval)
check_voting_impact(meta_dataframe$number_of_upvotes_of_request_at_retrieval)

#Admittedly this is a little surprising, I thought there would be a corellation between
#up or downvotes, but I wonder if this is actually what I should expect. I think that if
#a post recieves a lot of attention then it will get a pizza.

#To be honest this seems like just fucking around for absolutely no reason. I think the most sensible
#trends to bother exploring are:
#1) Requester upvotes + downvotes = Total attention.
#2) Requester number of comments
#3) Timestamp, see what time of day it was at and if that bears any relation to anything.

total_attention <- meta_dataframe$requester_upvotes_plus_downvotes_at_retrieval +
  meta_dataframe$request_number_of_comments_at_retrieval

total_attention_df <- data.frame(total_attention, meta_dataframe$requester_received_pizza)

plot(total_attention_df$meta_dataframe.requester_received_pizza, total_attention_df$total_attention)
plot(total_attention_df$total_attention, total_attention_df$meta_dataframe.requester_received_pizza)

#This sort of shows the same thing, that there's no clear corellation between the two. I think
#the most productive way forward may be text mining.





















#The strategy going forward will be to seperate out the requests into successful and unsuccessful
#and then build up frequency tables of each.

#Titles text mining.

#Successful

successful_requests <- meta_dataframe[which(meta_dataframe$requester_received_pizza == TRUE),]

successful_titles <- successful_requests$request_title
new_successful_titles <- list()

for (i in 1:length(successful_titles)){
  new_successful_titles[[i]] <- unlist(strsplit(as.vector(successful_titles[i]), " "))
}

all_successful_title_words = unlist(new_successful_titles)
all_successful_title_words_table_df <- data.frame(table(all_successful_title_words))
all_successful_title_words_table_df <- all_successful_title_words_table_df[order(all_successful_title_words_table_df$Freq),]

#So now we have the frequencies of all the words used in the title. Next step is to remove the most
#common ones in the English language.

#Stripped from the web.

#Trying to find most common words in English language

words_page <- "https://www.englishclub.com/vocabulary/common-words-100.htm"
words_page <- readLines(words_page)

useful_words = words_page[92:191]

for (i in 1:length(useful_words)){
  useful_words[i] = strsplit(useful_words[i], "<")[[1]][1]
  useful_words[i] = strsplit(useful_words[i], " ")[[1]][2]
}

useful_words = c(useful_words, "is", "Pizza", "pizza", "[Request]", "[REQUEST]", "[request]", "-")

#Strip away all most useful words.

all_successful_title_words_table_df <- all_successful_title_words_table_df[-which(all_successful_title_words_table_df$all_successful_title_words %in% useful_words),]
all_successful_title_words_table_df <- all_successful_title_words_table_df[order(all_successful_title_words_table_df$Freq),]

#This gives us something to look at and play with. Now we try using the text mining package to
#create a word cloud.

for_word_cloud_corpus <- Corpus(VectorSource(all_successful_title_words_table_df$all_successful_title_words))

#cleaning

for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, stripWhitespace)
for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, tolower)
for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, removeWords, stopwords("english"))
for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, stemDocument)
for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, removeWords, c("[request]", "request"))

wordcloud(for_word_cloud_corpus, scale = c(4, 1), max.words = 100, colors = brewer.pal(8, "Dark2"))

#Failed

failed_requests <- meta_dataframe[which(meta_dataframe$requester_received_pizza == FALSE),]

failed_titles <- failed_requests$request_title
new_failed_titles <- list()

for (i in 1:length(failed_titles)){
  new_failed_titles[[i]] <- unlist(strsplit(as.vector(failed_titles[i]), " "))
}

all_failed_title_words = unlist(new_failed_titles)
all_failed_title_words_table_df <- data.frame(table(all_failed_title_words))
all_failed_title_words_table_df <- all_failed_title_words_table_df[order(all_failed_title_words_table_df$Freq),]

#So now we have the frequencies of all the words used in the title. Next step is to remove the most
#common ones in the English language.

#Stripped from the web.

#Trying to find most common words in English language

words_page <- "https://www.englishclub.com/vocabulary/common-words-100.htm"
words_page <- readLines(words_page)

useful_words = words_page[92:191]

for (i in 1:length(useful_words)){
  useful_words[i] = strsplit(useful_words[i], "<")[[1]][1]
  useful_words[i] = strsplit(useful_words[i], " ")[[1]][2]
}

useful_words = c(useful_words, "is", "Pizza", "pizza", "[Request]", "[REQUEST]", "[request]", "-")

#Strip away all most useful words.

all_failed_title_words_table_df <- all_failed_title_words_table_df[-which(all_failed_title_words_table_df$all_failed_title_words %in% useful_words),]
all_failed_title_words_table_df <- all_failed_title_words_table_df[order(all_failed_title_words_table_df$Freq),]

#This gives us something to look at and play with. Now we try using the text mining package to
#create a word cloud.

for_word_cloud_corpus <- Corpus(VectorSource(all_failed_title_words_table_df$all_failed_title_words))

#cleaning

for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, stripWhitespace)
for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, tolower)
for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, removeWords, stopwords("english"))
for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, stemDocument)
for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, removeWords, c("[request]", "request"))

wordcloud(for_word_cloud_corpus, scale = c(4, 1), max.words = 100, colors = brewer.pal(8, "Dark2"))

#In reality this didn't really show a lot so we need to strip away the words that are quite common in both
#fields. We look at the top 20 words that occur in both and then try removing any that are the same.

top_failed_words <- all_failed_title_words_table_df[order(all_failed_title_words_table_df$Freq),]
top_successful_words <- all_successful_title_words_table_df[order(all_successful_title_words_table_df$Freq),]
top_failed_words <- top_failed_words[(nrow(top_failed_words)-nrow(top_successful_words)+1):nrow(top_failed_words),]

top_common_words_df <- data.frame(top_failed_words, top_successful_words)

#This didn't actually show that much, I think it would be much better to look at these ideas as proportions.

number_of_successful_requests = nrow(successful_requests)
number_of_failed_requests = nrow(failed_requests)

top_common_words_df$Freq = paste(top_common_words_df$Freq, number_of_failed_requests, sep = "/")
top_common_words_df$Freq.1 = paste(top_common_words_df$Freq.1, number_of_successful_requests, sep = "/")

#We can see from this that the titles don't actually carry a lot of pertinent information as they mainly seem
#to focus on the same sort of ideas.






















#Now we look at the actual text of posts and see if there's anything pertinent in there.

#The strategy going forward will be to seperate out the requests into successful and unsuccessful
#and then build up frequency tables of each.

#Texts text mining.

#Successful

successful_requests <- meta_dataframe[which(meta_dataframe$requester_received_pizza == TRUE),]

successful_texts <- successful_requests$request_text
new_successful_texts <- list()

for (i in 1:length(successful_texts)){
  new_successful_texts[[i]] <- unlist(strsplit(as.vector(successful_texts[i]), " "))
}

all_successful_text_words = unlist(new_successful_texts)
all_successful_text_words_table_df <- data.frame(table(all_successful_text_words))
all_successful_text_words_table_df <- all_successful_text_words_table_df[order(all_successful_text_words_table_df$Freq),]

#So now we have the frequencies of all the words used in the text. Next step is to remove the most
#common ones in the English language.

#Stripped from the web.

#Trying to find most common words in English language

words_page <- "https://www.englishclub.com/vocabulary/common-words-100.htm"
words_page <- readLines(words_page)

useful_words = words_page[92:191]

for (i in 1:length(useful_words)){
  useful_words[i] = strsplit(useful_words[i], "<")[[1]][1]
  useful_words[i] = strsplit(useful_words[i], " ")[[1]][2]
}

useful_words = c(useful_words, "is", "Pizza", "pizza", "[Request]", "[REQUEST]", "[request]", "-")

#Strip away all most useful words.

all_successful_text_words_table_df <- all_successful_text_words_table_df[-which(all_successful_text_words_table_df$all_successful_text_words %in% useful_words),]
all_successful_text_words_table_df <- all_successful_text_words_table_df[order(all_successful_text_words_table_df$Freq),]

#This gives us something to look at and play with. Now we try using the text mining package to
#create a word cloud.

for_word_cloud_corpus <- Corpus(VectorSource(all_successful_text_words_table_df$all_successful_text_words))

#cleaning

for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, stripWhitespace)
for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, tolower)
for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, removeWords, stopwords("english"))
for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, stemDocument)
for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, removeWords, c("[request]", "request"))

wordcloud(for_word_cloud_corpus, scale = c(4, 1), max.words = 100, colors = brewer.pal(8, "Dark2"))

#Failed

failed_requests <- meta_dataframe[which(meta_dataframe$requester_received_pizza == FALSE),]

failed_texts <- failed_requests$request_text
new_failed_texts <- list()

for (i in 1:length(failed_texts)){
  new_failed_texts[[i]] <- unlist(strsplit(as.vector(failed_texts[i]), " "))
}

all_failed_text_words = unlist(new_failed_texts)
all_failed_text_words_table_df <- data.frame(table(all_failed_text_words))
all_failed_text_words_table_df <- all_failed_text_words_table_df[order(all_failed_text_words_table_df$Freq),]

#So now we have the frequencies of all the words used in the text. Next step is to remove the most
#common ones in the English language.

#Stripped from the web.

#Trying to find most common words in English language

words_page <- "https://www.englishclub.com/vocabulary/common-words-100.htm"
words_page <- readLines(words_page)

useful_words = words_page[92:191]

for (i in 1:length(useful_words)){
  useful_words[i] = strsplit(useful_words[i], "<")[[1]][1]
  useful_words[i] = strsplit(useful_words[i], " ")[[1]][2]
}

useful_words = c(useful_words, "is", "Pizza", "pizza", "[Request]", "[REQUEST]", "[request]", "-")

#Strip away all most useful words.

all_failed_text_words_table_df <- all_failed_text_words_table_df[-which(all_failed_text_words_table_df$all_failed_text_words %in% useful_words),]
all_failed_text_words_table_df <- all_failed_text_words_table_df[order(all_failed_text_words_table_df$Freq),]

#This gives us something to look at and play with. Now we try using the text mining package to
#create a word cloud.

for_word_cloud_corpus <- Corpus(VectorSource(all_failed_text_words_table_df$all_failed_text_words))

#cleaning

for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, stripWhitespace)
for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, tolower)
for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, removeWords, stopwords("english"))
for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, stemDocument)
for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, removeWords, c("[request]", "request"))

wordcloud(for_word_cloud_corpus, scale = c(4, 1), max.words = 100, colors = brewer.pal(8, "Dark2"))

#In reality this didn't really show a lot so we need to strip away the words that are quite common in both
#fields. We look at the top 20 words that occur in both and then try removing any that are the same.

top_failed_words <- all_failed_text_words_table_df[order(all_failed_text_words_table_df$Freq),]
top_successful_words <- all_successful_text_words_table_df[order(all_successful_text_words_table_df$Freq),]
top_failed_words <- top_failed_words[(nrow(top_failed_words)-nrow(top_successful_words)+1):nrow(top_failed_words),]

top_common_words_df <- data.frame(top_failed_words, top_successful_words)

#This didn't actually show that much, I think it would be much better to look at these ideas as proportions.

number_of_successful_requests <- nrow(successful_requests)
number_of_failed_requests <- nrow(failed_requests)

top_common_words_df$Freq <- paste(top_common_words_df$Freq, number_of_failed_requests, sep = "/")
top_common_words_df$Freq.1 <- paste(top_common_words_df$Freq.1, number_of_successful_requests, sep = "/")