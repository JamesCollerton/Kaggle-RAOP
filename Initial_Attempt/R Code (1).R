#Sets working directory to the relevant space and brings up the appropriate library.

library('tm')
library("rjson")
library("KernSmooth")
library('SnowballC')
library('wordcloud')
setwd("C:/Users/James/Documents/R/KagglePizzaProblem")

#Reads in the relevant .json file

meta_data <- fromJSON(file = "train.json")

#Initialises all the necessary vectors. This was just miles easier to do in Sublime Text.

giver_username_if_known <- c()
number_of_downvotes_of_request_at_retrieval <- c()
number_of_upvotes_of_request_at_retrieval <- c()
post_was_edited <- c()
request_id <- c()
request_number_of_comments_at_retrieval <- c()
request_text <- c()
request_text_edit_aware <- c()
request_title <- c()
requester_account_age_in_days_at_request <- c()
requester_account_age_in_days_at_retrieval <- c()
requester_days_since_first_post_on_raop_at_request <- c()
requester_days_since_first_post_on_raop_at_retrieval <- c()
requester_number_of_comments_at_request <- c()
requester_number_of_comments_at_retrieval <- c()
requester_number_of_comments_in_raop_at_request <- c()
requester_number_of_comments_in_raop_at_retrieval <- c()
requester_number_of_posts_at_request <- c()
requester_number_of_posts_at_retrieval <- c()
requester_number_of_posts_on_raop_at_request <- c()
requester_number_of_posts_on_raop_at_retrieval <- c()
requester_number_of_subreddits_at_request <- c()
requester_received_pizza <- c()
requester_subreddits_at_request <- c()
requester_upvotes_minus_downvotes_at_request <- c()
requester_upvotes_minus_downvotes_at_retrieval <- c()
requester_upvotes_plus_downvotes_at_request <- c()
requester_upvotes_plus_downvotes_at_retrieval <- c()
requester_user_flair <- c()
requester_username <- c()
unix_timestamp_of_request <- c()
unix_timestamp_of_request_utc <- c()

#Loop to turn all the information in the lists into something more usable.

for (i in 1:length(meta_data)){
  giver_username_if_known[i]= meta_data[[i]]$giver_username_if_known
  number_of_downvotes_of_request_at_retrieval[i] = meta_data[[i]]$number_of_downvotes_of_request_at_retrieval
  number_of_upvotes_of_request_at_retrieval[i] = meta_data[[i]]$number_of_upvotes_of_request_at_retrieval
  post_was_edited[i] = meta_data[[i]]$post_was_edited
  request_id[i] = meta_data[[i]]$request_id
  request_number_of_comments_at_retrieval[i] = meta_data[[i]]$request_number_of_comments_at_retrieval
  request_text[i] = meta_data[[i]]$request_text
  request_text_edit_aware[i] = meta_data[[i]]$request_text_edit_aware
  request_title[i] = meta_data[[i]]$request_title
  requester_account_age_in_days_at_request[i] = meta_data[[i]]$requester_account_age_in_days_at_request
  requester_account_age_in_days_at_retrieval[i] = meta_data[[i]]$requester_account_age_in_days_at_retrieval
  requester_days_since_first_post_on_raop_at_request[i] = meta_data[[i]]$requester_days_since_first_post_on_raop_at_request
  requester_days_since_first_post_on_raop_at_retrieval[i] = meta_data[[i]]$requester_days_since_first_post_on_raop_at_retrieval
  requester_number_of_comments_at_request[i] = meta_data[[i]]$requester_number_of_comments_at_request
  requester_number_of_comments_at_retrieval[i] = meta_data[[i]]$requester_number_of_comments_at_retrieval
  requester_number_of_comments_in_raop_at_request[i] = meta_data[[i]]$requester_number_of_comments_in_raop_at_request
  requester_number_of_comments_in_raop_at_retrieval[i] = meta_data[[i]]$requester_number_of_comments_in_raop_at_retrieval
  requester_number_of_posts_at_request[i] = meta_data[[i]]$requester_number_of_posts_at_request
  requester_number_of_posts_at_retrieval[i] = meta_data[[i]]$requester_number_of_posts_at_retrieval
  requester_number_of_posts_on_raop_at_request[i] = meta_data[[i]]$requester_number_of_posts_on_raop_at_request
  requester_number_of_posts_on_raop_at_retrieval[i] = meta_data[[i]]$requester_number_of_posts_on_raop_at_retrieval
  requester_number_of_subreddits_at_request[i] = meta_data[[i]]$requester_number_of_subreddits_at_request
  requester_received_pizza[i] = meta_data[[i]]$requester_received_pizza
  requester_subreddits_at_request[i] = length(meta_data[[i]]$requester_subreddits_at_request)
  requester_upvotes_minus_downvotes_at_request[i] = meta_data[[i]]$requester_upvotes_minus_downvotes_at_request
  requester_upvotes_minus_downvotes_at_retrieval[i] = meta_data[[i]]$requester_upvotes_minus_downvotes_at_retrieval
  requester_upvotes_plus_downvotes_at_request[i] = meta_data[[i]]$requester_upvotes_plus_downvotes_at_request
  requester_upvotes_plus_downvotes_at_retrieval[i] = meta_data[[i]]$requester_upvotes_plus_downvotes_at_retrieval
  requester_user_flair[i] = length(meta_data[[i]]$requester_user_flair)
  requester_username[i] = meta_data[[i]]$requester_username
  unix_timestamp_of_request[i] = meta_data[[i]]$unix_timestamp_of_request
  unix_timestamp_of_request_utc[i] = meta_data[[i]]$unix_timestamp_of_request_utc
}

#This part just makes a nice dataframe and gives nice titles.

meta_dataframe <- data.frame("giver_username_if_known" = unlist(giver_username_if_known),                             
                             "number_of_downvotes_of_request_at_retrieval" = unlist(number_of_downvotes_of_request_at_retrieval),         
                             "number_of_upvotes_of_request_at_retrieval" = unlist(number_of_upvotes_of_request_at_retrieval),           
                             "post_was_edited" = unlist(post_was_edited),                                     
                             "request_id" = unlist(request_id),                                          
                             "request_number_of_comments_at_retrieval" = unlist(request_number_of_comments_at_retrieval),             
                             "request_text" = unlist(request_text),                                        
                             "request_text_edit_aware" = unlist(request_text_edit_aware),                             
                             "request_title" = unlist(request_title),                                       
                             "requester_account_age_in_days_at_request" = unlist(requester_account_age_in_days_at_request),            
                             "requester_account_age_in_days_at_retrieval" = unlist(requester_account_age_in_days_at_retrieval),          
                             "requester_days_since_first_post_on_raop_at_request" = unlist(requester_days_since_first_post_on_raop_at_request),  
                             "requester_days_since_first_post_on_raop_at_retrieval" = unlist(requester_days_since_first_post_on_raop_at_retrieval),
                             "requester_number_of_comments_at_request" = unlist(requester_number_of_comments_at_request),             
                             "requester_number_of_comments_at_retrieval" = unlist(requester_number_of_comments_at_retrieval),           
                             "requester_number_of_comments_in_raop_at_request" = unlist(requester_number_of_comments_in_raop_at_request),     
                             "requester_number_of_comments_in_raop_at_retrieval" = unlist(requester_number_of_comments_in_raop_at_retrieval),   
                             "requester_number_of_posts_at_request" = unlist(requester_number_of_posts_at_request),                
                             "requester_number_of_posts_at_retrieval" = unlist(requester_number_of_posts_at_retrieval),              
                             "requester_number_of_posts_on_raop_at_request" = unlist(requester_number_of_posts_on_raop_at_request),        
                             "requester_number_of_posts_on_raop_at_retrieval" = unlist(requester_number_of_posts_on_raop_at_retrieval),      
                             "requester_number_of_subreddits_at_request" = unlist(requester_number_of_subreddits_at_request),           
                             "requester_received_pizza" = unlist(requester_received_pizza),                            
                             "requester_subreddits_at_request" = unlist(requester_subreddits_at_request),                     
                             "requester_upvotes_minus_downvotes_at_request" = unlist(requester_upvotes_minus_downvotes_at_request),        
                             "requester_upvotes_minus_downvotes_at_retrieval" = unlist(requester_upvotes_minus_downvotes_at_retrieval),      
                             "requester_upvotes_plus_downvotes_at_request" = unlist(requester_upvotes_plus_downvotes_at_request),         
                             "requester_upvotes_plus_downvotes_at_retrieval" = unlist(requester_upvotes_plus_downvotes_at_retrieval),       
                             "requester_user_flair" = unlist(requester_user_flair),                                
                             "requester_username" = unlist(requester_username),                                  
                             "unix_timestamp_of_request" = unlist(unix_timestamp_of_request),                           
                             "unix_timestamp_of_request_utc" = unlist(unix_timestamp_of_request_utc))


###################################################################
#Exploratory analysis##############################################
###################################################################

###################################################################
#The first trend to be explored is if having a lot of downvotes would lead to a dimished
#probability of recieving pizza.
###################################################################

downvotes <- data.frame(meta_dataframe$requester_received_pizza, 
                        meta_dataframe$number_of_downvotes_of_request_at_retrieval)

#Now what we would expect to see is that as the number of downvotes increases the proportion of
#people recieving pizza = (People who recieved pizza)/ (People who did not recieve pizza) begins 
#to decrease

#Creates a tabular version of the data.

downvote_table <- table(downvotes)

xvals <- unique(downvotes$meta_dataframe.number_of_downvotes_of_request_at_retrieval)
yvals <- downvote_table[2,]/downvote_table[1,]
plot(xvals, yvals, col = "dodgerblue", xlab = "Number of Downvotes",
     ylab = "Proportion of Successful Pizza Requests", main = "Corellation between Number of Downvotes
     and Proportion of Successful Pizza Requests", pch = 16)

#Fits a linear model to the data whilst ignoring all values that jet off to infinity.

lm_fit_df <- data.frame(xvals, yvals)
lm_fit_df <- lm_fit_df[which(lm_fit_df$yvals != Inf),]

fit <- lm(lm_fit_df$yvals ~ lm_fit_df$xvals)
abline(0.438249, -0.009727, col = "firebrick1", lwd = 1.5)

#Attempt at kernel smoothing the data with a plug-in method optimised parameter but shows very little.

hhat <- dpik(lm_fit_df$xvals)
kern <- ksmooth(lm_fit_df$xvals, lm_fit_df$yvals, bandwidth = hhat)
lines(kern, lwd=2, col= "cadetblue")

#To be honest there doesn't seem to be much of a corellation between the two factors which
#is surprising.

###################################################################
#The second trend to be explored is if having a lot of upvotes would lead to a risen
#probability of recieving pizza.
###################################################################

upvotes <- data.frame(meta_dataframe$requester_received_pizza, 
                      meta_dataframe$number_of_upvotes_of_request_at_retrieval)

#Now what we would expect to see is that as the number of upvotes increases the proportion of
#people recieving pizza = (People who recieved pizza)/ (People who did not recieve pizza) begins 
#to decrease

#Creates a tabular version of the data.

upvote_table <- table(upvotes)

xvals <- unique(upvotes$meta_dataframe.number_of_upvotes_of_request_at_retrieval)
yvals <- upvote_table[2,]/upvote_table[1,]
plot(xvals, yvals, col = "dodgerblue", xlab = "Number of upvotes",
     ylab = "Proportion of Successful Pizza Requests", main = "Corellation between Number of upvotes
     and Proportion of Successful Pizza Requests", pch = 16)

#Fits a linear model to the data whilst ignoring all values that jet off to infinity.

lm_fit_df <- data.frame(xvals, yvals)
lm_fit_df <- lm_fit_df[which(lm_fit_df$yvals != Inf),]

fit <- lm(lm_fit_df$yvals ~ lm_fit_df$xvals)
abline(0.653607, -0.001204, col = "firebrick1", lwd = 1.5)

#Attempt at kernel smoothing the data with a plug-in method optimised parameter but shows very little.

hhat <- 20
kern <- ksmooth(lm_fit_df$xvals, lm_fit_df$yvals, bandwidth = hhat)
lines(kern, lwd=2, col= "cadetblue")

#Admittedly this is a little surprising, I thought there would be a corellation between
#up or downvotes, but I wonder if this is actually what I should expect. I think that if
#a post recieves a lot of attention then it will get a pizza.

#To be honest this seems like just fucking around for absolutely no reason. I think the most sensible
#trends to bother exploring are:
#1) Requester upvotes + downvotes = Total attention.
#2) Requester number of comments
#TO BE HONEST THE BEST WAY TO DEAL WITH THIS IS TO SUM THE TWO ABOVE.
#3) Timestamp, see what time of day it was at and if that bears any relation to anything.

total_attention <- meta_dataframe$requester_upvotes_plus_downvotes_at_retrieval +
                  meta_dataframe$request_number_of_comments_at_retrieval

total_attention_df <- data.frame(total_attention, meta_dataframe$requester_received_pizza)

plot(total_attention_df$meta_dataframe.requester_received_pizza, total_attention_df$total_attention)

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