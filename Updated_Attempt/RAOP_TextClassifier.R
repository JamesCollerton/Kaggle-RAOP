# Sets working directory to the relevant space and brings up the appropriate library.

# This approach will try to use text classification in R to see what makes a successful post.

# required_packages <- c("tm", "rjson", "KernSmooth", "SnowballC", "wordcloud", "httr", "XML", "RCurl", "RTextTools")

# install.packages(required_packages)

library('tm')
library('rjson')
library('KernSmooth')
library('SnowballC')
library('wordcloud')
library('httr')    
library('XML')
library('RCurl')
library('RTextTools')

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

createCoreWords <- function(plainText) {
  for_word_cloud_corpus <- Corpus(VectorSource(plainText))
  for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, stripWhitespace)
  for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, tolower)
  for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, PlainTextDocument)
  for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, removeWords, stopwords("english"))
  for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, stemDocument)
  for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, removeWords, c("[request]", "request"))
  for_word_cloud_corpus[[1]]$content
}

# Creates a datadrame of the text, and if it was successful. Then splits it into two parts. The first is
# the training data and the second is the validation data.
textOutputDataFrame <- data.frame(meta_dataframe$request_text, meta_dataframe$requester_received_pizza)

trainingTextOutputDataFrame <- textOutputDataFrame[1:(0.3*nrow(textOutputDataFrame)),]
requestText <- sapply(trainingTextOutputDataFrame$meta_dataframe.request_text, createCoreWords)
requesterRecievedPizza <- trainingTextOutputDataFrame$meta_dataframe.requester_received_pizza
trainingTextOutputDataFrame <- data.frame(requestText, requesterRecievedPizza)

validationTextOutputDataFrame <- textOutputDataFrame[(0.75*nrow(textOutputDataFrame)):(nrow(textOutputDataFrame)),]
validationTextOutputDataFrame$meta_dataframe.request_text <- sapply(validationTextOutputDataFrame$meta_dataframe.request_text, createCoreWords)

documentTermMatrix <- create_matrix(trainingTextOutputDataFrame$requestText)
trainingLength <- length(trainingTextOutputDataFrame$requesterRecievedPizza)
container <- create_container(documentTermMatrix, 
                              trainingTextOutputDataFrame$requesterRecievedPizza, 
                              trainSize = 1:(trainingLength/2),
                              testSize = (trainingLength/2 + 1):trainingLength,
                              virgin=FALSE)
model <- train_model(container, "SLDA", kernel="linear", cost=1)

# create_matrix_edited <- edit(create_matrix)
predictionMatrix <- create_matrix_edited(validationTextOutputDataFrame$meta_dataframe.request_text, originalMatrix=documentTermMatrix)
predSize = length(validationTextOutputDataFrame$meta_dataframe.request_text);
predictionContainer <- create_container(predictionMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)
results <- classify_model(predictionContainer, model)

classifiedTrue <- which(results$SLDA_LABEL == TRUE)
actuallyTrue <- which(validationTextOutputDataFrame$meta_dataframe.requester_received_pizza == TRUE)
numCorrectClassifications <- length(intersect(classifiedTrue, actuallyTrue))
numRequestersRecievedPizza <- length(actuallyTrue)
percentageCorrect <- numCorrectClassifications / numRequestersRecievedPizza
print(percentageCorrect)
