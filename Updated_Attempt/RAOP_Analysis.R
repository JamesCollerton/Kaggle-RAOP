#Sets working directory to the relevant space and brings up the appropriate library.

# required_packages <- c("tm", "rjson", "KernSmooth", "SnowballC", "wordcloud", "httr", "XML", "RCurl")

# install.packages(required_packages)

install.packages("RCurl")

library('tm')
library("rjson")
library("KernSmooth")
library('SnowballC')
library('wordcloud')
library('httr')    
library('XML')
library('RCurl')

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

check_voting_impact <- function(vote_type, sufficient_data_threshold)
{
	votes <- data.frame(meta_dataframe$requester_received_pizza, 
                      vote_type)

	vote_table <- table(votes)
  
  false_sufficient_data <- which(vote_table["FALSE",] > sufficient_data_threshold)
  true_sufficient_data <- which(vote_table["TRUE",] > sufficient_data_threshold)
  
  sufficient_data <- intersect(false_sufficient_data, true_sufficient_data)
  vote_table <- vote_table[,sufficient_data]

	xvals <- unique(as.numeric(colnames(vote_table)))
	yvals <- vote_table["TRUE",]/(vote_table["TRUE",] + vote_table["FALSE",])
  
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

check_voting_impact(meta_dataframe$number_of_downvotes_of_request_at_retrieval, 7)
check_voting_impact(meta_dataframe$number_of_upvotes_of_request_at_retrieval, 8)

# So now we have shown that, when you ignore posts with sparse information, there is
# a strong correlation between upvotes and pizza, and a weak one between downvotes.

# ------------------------------------------------------------------------------------------------

create_word_cloud <- function(titles){

#   titles <- requests$request_title
  new_titles <- list()

  for (i in 1:length(titles)){
    new_titles[[i]] <- unlist(strsplit(as.vector(titles[i]), " "))
  }

  all_title_words = unlist(new_titles)
  all_title_words_table_df <- data.frame(table(all_title_words))
  all_title_words_table_df <- all_title_words_table_df[order(all_title_words_table_df$Freq),]

  words_page <- getURL("https://en.wikipedia.org/wiki/Most_common_words_in_English")
  tables <- readHTMLTable(words_page)
  useful_words <- tables$'NULL'$V2

  useful_words = c(useful_words, "is", "Pizza", "pizza", "[Request]", "[REQUEST]", "[request]", "-")

  all_title_words_table_df <- all_title_words_table_df[-which(all_title_words_table_df$all_title_words %in% useful_words),]
  all_title_words_table_df <- all_title_words_table_df[order(all_title_words_table_df$Freq),]

  #This gives us something to look at and play with. Now we try using the text mining package to
  #create a word cloud.

  for_word_cloud_corpus <- Corpus(VectorSource(all_title_words_table_df$all_title_words))

  #cleaning

  for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, stripWhitespace)
  for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, tolower)
  for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, PlainTextDocument)
  for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, removeWords, stopwords("english"))
  for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, stemDocument)
  for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, removeWords, c("[request]", "request"))

  wordcloud(for_word_cloud_corpus, scale = c(4, 1), max.words = 40, colors = brewer.pal(8, "Dark2"))

}

successful_requests_titles <- meta_dataframe[which(meta_dataframe$requester_received_pizza == TRUE),]$request_title
failed_requests_titles <- meta_dataframe[which(meta_dataframe$requester_received_pizza == FALSE),]$request_title

successful_requests_text <- meta_dataframe[which(meta_dataframe$requester_received_pizza == TRUE),]$request_text
failed_requests_text <- meta_dataframe[which(meta_dataframe$requester_received_pizza == FALSE),]$request_text

create_word_cloud(successful_requests_titles)
create_word_cloud(failed_requests_titles)
create_word_cloud(successful_requests_text)
create_word_cloud(failed_requests_text)

# ------------------------------------------------------------------------------------------------