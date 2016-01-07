#The strategy going forward will be to seperate out the requests into successful and unsuccessful
#and then build up frequency tables of each.

#Titles text mining.

#Successful

#This pulls out all the requests that were successful.
successful_requests <- meta_dataframe[which(meta_dataframe$requester_received_pizza == TRUE),]

#Vector of all titles and initialises new list for unlisting into.
successful_titles <- successful_requests$request_title
new_successful_titles <- list()

#Unlists all the elements of the title to be used more easily.
for (i in 1:length(successful_titles)){
  new_successful_titles[[i]] <- unlist(strsplit(as.vector(successful_titles[i]), " "))
}

new_successful_titles <- unlist(new_successful_titles)

for_word_cloud_corpus <- Corpus(VectorSource(new_successful_titles))

#Cleaning (removes stop words, whitespace, caps etc.)

for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, stripWhitespace)
for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, tolower)
for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, removeWords, stopwords("english"))
for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, removePunctuation)
for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, stemDocument)
for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, removeWords, c("[request]", "request"))

wordcloud(for_word_cloud_corpus, scale = c(4, 1), max.words = 100, colors = brewer.pal(8, "Dark2"))

#Now we create a table to examine.

successful_title_words_vec <- unlist(inspect(for_word_cloud_corpus))
successful_title_words_table <- data.frame(table(successful_title_words_vec))
successful_title_words_table <- successful_title_words_table[order(successful_title_words_table$Freq),]

#The strategy going forward will be to seperate out the requests into failed and unfailed
#and then build up frequency tables of each.

#Titles text mining.

#failed

#This pulls out all the requests that were failed.
failed_requests <- meta_dataframe[which(meta_dataframe$requester_received_pizza == FALSE),]

#Vector of all titles and initialises new list for unlisting into.
failed_titles <- failed_requests$request_title
new_failed_titles <- list()

#Unlists all the elements of the title to be used more easily.
for (i in 1:length(failed_titles)){
  new_failed_titles[[i]] <- unlist(strsplit(as.vector(failed_titles[i]), " "))
}

new_failed_titles <- unlist(new_failed_titles)

for_word_cloud_corpus <- Corpus(VectorSource(new_failed_titles))

#Cleaning (removes stop words, whitespace, caps etc.)

for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, stripWhitespace)
for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, tolower)
for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, removeWords, stopwords("english"))
for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, removePunctuation)
for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, stemDocument)
for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, removeWords, c("[request]", "request"))

wordcloud(for_word_cloud_corpus, scale = c(4, 1), max.words = 100, colors = brewer.pal(8, "Dark2"))

#Now we create a table to examine.

failed_title_words_vec <- unlist(inspect(for_word_cloud_corpus))
failed_title_words_table <- data.frame(table(failed_title_words_vec))
failed_title_words_table <- failed_title_words_table[order(failed_title_words_table$Freq),]

##### Text ######

#The strategy going forward will be to seperate out the requests into successful and unsuccessful
#and then build up frequency tables of each.

#text text mining.

#Successful

#This pulls out all the requests that were successful.
successful_requests <- meta_dataframe[which(meta_dataframe$requester_received_pizza == TRUE),]

#Vector of all text and initialises new list for unlisting into.
successful_text <- successful_requests$request_text
new_successful_text <- list()

#Unlists all the elements of the text to be used more easily.
for (i in 1:length(successful_text)){
  new_successful_text[[i]] <- unlist(strsplit(as.vector(successful_text[i]), " "))
}

new_successful_text <- unlist(new_successful_text)

for_word_cloud_corpus <- Corpus(VectorSource(new_successful_text))

#Cleaning (removes stop words, whitespace, caps etc.)

for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, stripWhitespace)
for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, tolower)
for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, removeWords, stopwords("english"))
for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, removePunctuation)
for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, stemDocument)
for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, removeWords, c("[request]", "request"))

wordcloud(for_word_cloud_corpus, scale = c(4, 1), max.words = 100, colors = brewer.pal(8, "Dark2"))

#Now we create a table to examine.

successful_text_words_vec <- unlist(inspect(for_word_cloud_corpus))
successful_text_words_table <- data.frame(table(successful_text_words_vec))
successful_text_words_table <- successful_text_words_table[order(successful_text_words_table$Freq),]

#The strategy going forward will be to seperate out the requests into failed and unfailed
#and then build up frequency tables of each.

#text text mining.

#failed

#This pulls out all the requests that were failed.
failed_requests <- meta_dataframe[which(meta_dataframe$requester_received_pizza == FALSE),]

#Vector of all text and initialises new list for unlisting into.
failed_text <- failed_requests$request_text
new_failed_text <- list()

#Unlists all the elements of the text to be used more easily.
for (i in 1:length(failed_text)){
  new_failed_text[[i]] <- unlist(strsplit(as.vector(failed_text[i]), " "))
}

new_failed_text <- unlist(new_failed_text)

for_word_cloud_corpus <- Corpus(VectorSource(new_failed_text))

#Cleaning (removes stop words, whitespace, caps etc.)

for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, stripWhitespace)
for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, tolower)
for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, removeWords, stopwords("english"))
for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, removePunctuation)
for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, stemDocument)
for_word_cloud_corpus <- tm_map(for_word_cloud_corpus, removeWords, c("[request]", "request"))

wordcloud(for_word_cloud_corpus, scale = c(4, 1), max.words = 100, colors = brewer.pal(8, "Dark2"))

#Now we create a table to examine.

failed_text_words_vec <- unlist(inspect(for_word_cloud_corpus))
failed_text_words_table <- data.frame(table(failed_text_words_vec))
failed_text_words_table <- failed_text_words_table[order(failed_text_words_table$Freq),]
