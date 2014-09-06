#Sets working directory to the relevant space and brings up the appropriate library.

library("rjson")
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

# Precursory look at corellations.

for (i in 1:length(meta_dataframe[1,])){
  