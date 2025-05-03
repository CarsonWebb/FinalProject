#Libraries
library(dplyr)
library(plyr)
library(tibble)

#As part of my analysis, I need to split up the lyrics into a list of each word in a given song.
split_lyrics <- function(x) {
  #cast to lowercase, cut out special characters, and then split
  #This combines (some) plurals and possessives (cow's, cows)
  strsplit(gsub("[[:punct:]]","",tolower(x)), " ")
}

#Tabulate a given emotion
tabulate_emotion <- function(emo_name) {
  #Import the data, then prune it to the given emotion
  data <- read.csv("C:\\Users\\thepe\\Desktop\\School Docs\\2025 Spring\\ITCS\\Final Project\\spotify_dataset.csv")
  data <- data %>% select(text, emotion) %>% filter(emotion == emo_name) %>% slice_sample(n = 1000) #Select N random rows with that emotion
  
  data$text <- sapply(data$text, split_lyrics) #Replace the text column with the split text column
  
  #Creating our data table
  wordlist <- c()
  for (i in 1:nrow(data)) {
    wordlist <- append(wordlist, unlist(data[i,1]))
  }
  rm(data)
  gc()
  
  emo <- table(wordlist)            #Produce contingency table
  emo <- as.data.frame(emo)      #Convert to dataframe
  emo <- emo %>% arrange(desc(Freq)) %>% slice_head(n = 1000) #Cut the data down to a top N words
  rownames(emo) <- emo[,1]          #Turn the wordlist into a data label, instead of value
  emo[,2] <- as.numeric(emo[,2])    #Convert frequency to numeric
  emo <- emo %>% select(Freq)       #Select just the frequency row
  emo <- t(emo)
  
  return(emo)
}

anger_df <- tabulate_emotion("anger")
gc()
fear_df <- tabulate_emotion("fear")
gc()
joy_df <- tabulate_emotion("joy")
gc()
love_df <- tabulate_emotion("love")
gc()
sadness_df <- tabulate_emotion("sadness")
gc()
surprise_df <- tabulate_emotion("surprise")
gc()

#Overly rare emotions: angry, True, thirst, confusion, pink, interest, Love

#Merging the data
emotions_df <- rbind.fill(as.data.frame(anger_df), as.data.frame(fear_df))
emotions_df <- rbind.fill(emotions_df, as.data.frame(joy_df))
emotions_df <- rbind.fill(emotions_df, as.data.frame(love_df))
emotions_df <- rbind.fill(emotions_df, as.data.frame(sadness_df))
emotions_df <- rbind.fill(emotions_df, as.data.frame(surprise_df))
rownames(emotions_df) <- c("anger","fear","joy","love","sadness","surprise")
emotions_df[is.na(emotions_df)] <- 0 #Turn all NAs into 0s

#Turn the data into proportions, rather than counts
emotions_df <- as.data.frame(t(emotions_df)) %>% mutate_all(proportions)

#Create a dataframe of just the words present in every emotion
omniwords_df <- emotions_df %>% filter_all(all_vars(. > 0)) %>% t()


#Word Analysis
frequencies <- apply(omniwords_df, 2, sum)

plot(1:length(frequencies),sort(frequencies, decreasing=TRUE),xlab="words",ylab="frequency") #Overall word frequency distribution

#Heatmap of pronouns (adjusting for frequency)
pronouns <- c("i","you","he","she","it","they")
heatmap(sweep(omniwords_df[,pronouns], 2, frequencies[pronouns], "/"), scale= "none")

#Heatmap of top 10 nouns (excluding swears)
top10nouns <- c("verse","chorus","man","love","time","right","baby","need","money","life")
heatmap(sweep(omniwords_df[,top10nouns], 2, frequencies[top10nouns], "/"), scale= "none")