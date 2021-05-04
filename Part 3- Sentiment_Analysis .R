# Load packages
#install.packages("tidyverse", dependencies = TRUE)
library(rvest)
library(stringr)
library(dplyr)
library(lubridate)
library(readr)
library(Rcrawler)
library(zoo)
library(tm)
library(splitstackshape)
library(ggplot2)
library(tidytext)
library(reshape2)
library(wordcloud)
library(SnowballC)
library(cowplot)
#IMPORT FILE
#################################################################################################################################3
#
#
#
hotels <- read.csv("3CombinedHotels.csv")
str(hotels)
hotels$dateOfStay <- gsub('January ', '01-', hotels$dateOfStay)
hotels$dateOfStay <- gsub('February ', '02-', hotels$dateOfStay)
hotels$dateOfStay <- gsub('March ', '03-', hotels$dateOfStay)
hotels$dateOfStay <- gsub('April ', '04-', hotels$dateOfStay)
hotels$dateOfStay <- gsub('May ', '05-', hotels$dateOfStay)
hotels$dateOfStay <- gsub('June ', '06-', hotels$dateOfStay)
hotels$dateOfStay <- gsub('July ', '07-', hotels$dateOfStay)
hotels$dateOfStay <- gsub('August ', '08-', hotels$dateOfStay)
hotels$dateOfStay <- gsub('September ', '09-', hotels$dateOfStay)
hotels$dateOfStay <- gsub('October ', '10-', hotels$dateOfStay)
hotels$dateOfStay <- gsub('November ', '11-', hotels$dateOfStay)
hotels$dateOfStay <- gsub('December ', '12-', hotels$dateOfStay)


hotels$dateOfStay_month <- substr(hotels$dateOfStay, start = 1, stop = 3)
hotels$dateOfStay_month <- str_replace(hotels$dateOfStay_month,"-","")

hotels$dateOfStay_year <- substr(hotels$dateOfStay, start = 4, stop = 7)

hotels <- subset(hotels, select = -c(1))

hotels$date <- as.Date(paste(hotels$dateOfStay_year, hotels$dateOfStay_month, sep="-"), "%Y-%M")
hotels$date <- as.yearmon(paste(hotels$dateOfStay_year, hotels$dateOfStay_month), "%Y %m")

class(hotels$date)



#CLEANING
#################################################################################################################################3
#
#
#
#Start cleaning - https://www.youtube.com/watch?v=jCrQYOsAcv4
#Remove punctuation
hotels$review_clean <- gsub(pattern = "\\W", replace = " ", hotels$review)

#Remove digits
hotels$review_clean <- gsub(pattern = "\\d", replace = " ", hotels$review_clean)

#Change to lower case
hotels$review_clean <- tolower(hotels$review_clean)

#Remove spop words
hotels$review_clean <- removeWords(hotels$review_clean, stopwords("english"))

#Remove worlds of lenght 1
hotels$review_clean <- gsub(pattern = "\\b[A-z]\\b{1}",replace = " ", hotels$review_clean)

#Remove extra white spaces
hotels$review_clean <- stripWhitespace(hotels$review_clean)

#Stem words
hotels$review_clean <- wordStem(hotels$review_clean,language = "english")

#Check the diference between before and after cleaning
#Before cleaning
hotels$review
#After cleaning
hotels$review_clean







#SENTIMENT ANALYSIS
#################################################################################################################################3
#
#
#Sentiment https://www.youtube.com/watch?v=jt4WzWoSCyo
#https://www.kaggle.com/rtatman/tutorial-sentiment-analysis-in-r
#https://www.youtube.com/watch?v=BqNTcewq0k0



#Drop column for title and review, leave just the date and review clean
hotels2 <- hotels[,-c(1,3,4)]

hotels2

#Change shape and leave each word in a row
hotels2 <- cSplit(hotels2, "review_clean", sep = " ", direction = "long")

#Rename column from review_clean to words
hotels2 <- hotels2 %>% rename("word" = "review_clean")


#Convert to Tibble
hotels_T <- as_tibble(hotels2)
class(hotels_T)
hotels_T




#subset for each of the hotels
#################################################################################################################################3
#
#
ashling <- subset(hotels_T, hotelName == "The Ashling Hotel")
mespil <- subset(hotels_T, hotelName == "The Mespil Hotel")
marker <- subset(hotels_T, hotelName == "The Marker Hotel")




#COUNT POPULAR WORDS
#################################################################################################################################3
#
#
#popular word for all the hotels
hotels_T%>%
  count(word, sort = TRUE)

#popular word for ashling
ashling%>%
  count(word, sort = TRUE)

#popular word for mespil
mespil%>%
  count(word, sort = TRUE)

#popular word for marker
marker%>%
  count(word, sort = TRUE)






#USING AFIN BY DATE
#################################################################################################################################3
#
#
#Use afin to get the sentiment analysis
hotels_T_afinn <- hotels_T %>%
  inner_join(get_sentiments("afinn"))

#Count total number of positives and negatives by month-year
hotels_T_afinn2_sentiment <- hotels_T_afinn%>%
  group_by(date, hotelName)%>%
  summarise(mean = mean(value))

hotels_T_afinn2_sentiment


#Plot to see the sentiment by month
ggplot(hotels_T_afinn2_sentiment, aes (x = date, y = mean, group = hotelName))+
  labs(title = "Sentiment Analysis by Hotel and Date of Stay", x = "Month and Year of Stay",y="Sentiment Value AFFIN Mean")+
  geom_line(aes(color = hotelName))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values=c("#424242","#FFD700","#5782A7"))


hotels_T_afinn

ggplot(hotels_T_afinn, aes (x = hotelName, y = value, color = hotelName))+
  labs(title = "Overall Sentiment Analysis by Hotel", x =element_blank(),y="Sentiment Value AFFIN")+
  stat_boxplot(fill = NA, show.legend = FALSE)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values=c("#424242","#FFD700","#5782A7"))




#USING BING BY DATE
#################################################################################################################################3
#

#ASHLING
#Use Bing to get the sentiment analysis
ashlingbing <- ashling %>%
  inner_join(get_sentiments("bing"))

ashlingbing

#Count total number of positives and negatives by month-year
ashling_sentiment_bing <- ashlingbing%>%
  group_by(date,sentiment)%>%
  count(sentiment)

ashling_sentiment_bing
#Plot to see the sentiment by month
ashlingbingplot <- ggplot(ashling_sentiment_bing, aes (x = date, y = n, fill = sentiment))+
  labs(title = "Sentiment Analysis The Aishling Hotel by Date of Stay", x = "Date of Stay",y="Word Count",fill="Sentiment")+
  geom_col(aes(fill = sentiment), alpha = 0.5)+
  scale_color_manual(values = c("#990000","#0B6623"))+
  scale_fill_manual(values = c("#990000","#0B6623"))+
  theme(plot.title = element_text(hjust = 0.5))


#MESPIL
#Use Bing to get the sentiment analysis
mespilbing <- mespil %>%
  inner_join(get_sentiments("bing"))

mespilbing

#Count total number of positives and negatives by month-year
mespil_sentiment_bing <- mespilbing%>%
  group_by(date,sentiment)%>%
  count(sentiment)

mespil_sentiment_bing
#Plot to see the sentiment by month
mespilbingplot <- ggplot(mespil_sentiment_bing, aes (x = date, y = n, fill = sentiment))+
  labs(title = "Sentiment Analysis The Mespil Hotel by Date of Stay", x = "Date of Stay",y="Word Count",fill="Sentiment")+
  geom_col(aes(fill = sentiment), alpha = 0.5)+
  scale_color_manual(values = c("#990000","#0B6623"))+
  scale_fill_manual(values = c("#990000","#0B6623"))+
  theme(plot.title = element_text(hjust = 0.5))



#MARKER
#Use Bing to get the sentiment analysis
markerbing <- marker %>%
  inner_join(get_sentiments("bing"))

markerbing

#Count total number of positives and negatives by month-year
marker_sentiment_bing <- markerbing%>%
  group_by(date,sentiment)%>%
  count(sentiment)

marker_sentiment_bing
#Plot to see the sentiment by month
markerbingplot <- ggplot(marker_sentiment_bing, aes (x = date, y = n, fill = sentiment))+
  labs(title = "Sentiment Analysis The Marker Hotel by Date of Stay", x = "Date of Stay",y="Word Count",fill="Sentiment")+
  geom_col(aes(fill = sentiment), alpha = 0.5)+
  scale_color_manual(values = c("#990000","#0B6623"))+
  scale_fill_manual(values = c("#990000","#0B6623"))+
  theme(plot.title = element_text(hjust = 0.5))

plot_grid(ashlingbingplot,mespilbingplot,markerbingplot,labels = "AUTO", ncol = 2)


#USING NRC BY DATE
#################################################################################################################################3
#
#ASHLING
#Use ncr to get the sentiment analysis
ashlingnrc <- ashling %>%
  inner_join(get_sentiments("nrc"))

ashlingnrc
#Count total number of positives and negatives by month-year
ashling_sentiment_nrc <- ashlingnrc%>%
  group_by(date,sentiment)%>%
  count(sentiment)

#positive
ashling_sentiment_nrc_pos <- subset(ashling_sentiment_nrc, sentiment  == "anticipation"|sentiment  == "joy"|sentiment  == "positive"|
                                               sentiment == "surprise"| sentiment == "trust")

ashling_sentiment_nrc_pos

#Plot to see the positive sentiment
positiveashling <- ggplot(ashling_sentiment_nrc_pos, aes (x = date, y = n, color = sentiment))+
  labs(title = "Positive Sentiment The Aishling Hotel by Date of Stay", x = "Date of Stay",y="Word Count",fill="Sentiment")+
  geom_line()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette="Greens")



#negative
ashling_sentiment_nrc_neg <- subset(ashling_sentiment_nrc, sentiment  == "anger"| sentiment  == "disgust"| sentiment  == "fear"|
                                               sentiment  == "negative"| sentiment  =="sadness")

ashling_sentiment_nrc_neg

#Plot to see the negative sentiment
negativeashling <- ggplot(ashling_sentiment_nrc_neg, aes (x = date, y = n, color = sentiment))+
  labs(title = "Negative Sentiment The Aishling Hotel by Date of Stay", x = "Date of Stay",y="Word Count",fill="Sentiment")+
  geom_line()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette="Reds")



#MESPIL
#Use ncr to get the sentiment analysis
mespilnrc <- mespil %>%
  inner_join(get_sentiments("nrc"))

mespilnrc
#Count total number of positives and negatives by month-year
mespil_sentiment_nrc <- mespilnrc%>%
  group_by(date,sentiment)%>%
  count(sentiment)

#positive
mespil_sentiment_nrc_pos <- subset(mespil_sentiment_nrc, sentiment  == "anticipation"|sentiment  == "joy"|sentiment  == "positive"|
                                      sentiment == "surprise"| sentiment == "trust")

mespil_sentiment_nrc_pos

#Plot to see the positive sentiment
positivemespil <- ggplot(mespil_sentiment_nrc_pos, aes (x = date, y = n, color = sentiment))+
  labs(title = "Positive Sentiment The Mespil Hotel by Date of Stay", x = "Date of Stay",y="Word Count",fill="Sentiment")+
  geom_line()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette="Greens")



#negative
mespil_sentiment_nrc_neg <- subset(mespil_sentiment_nrc, sentiment  == "anger"| sentiment  == "disgust"| sentiment  == "fear"|
                                      sentiment  == "negative"| sentiment  =="sadness")

mespil_sentiment_nrc_neg

#Plot to see the negative sentiment
negativemespil <- ggplot(mespil_sentiment_nrc_neg, aes (x = date, y = n, color = sentiment))+
  labs(title = "Negative Sentiment The Mespil Hotel by Date of Stay", x = "Date of Stay",y="Word Count",fill="Sentiment")+
  geom_line()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette="Reds")



#MARKER
#Use ncr to get the sentiment analysis
markernrc <- marker %>%
  inner_join(get_sentiments("nrc"))

markernrc
#Count total number of positives and negatives by month-year
marker_sentiment_nrc <- markernrc%>%
  group_by(date,sentiment)%>%
  count(sentiment)

#positive
marker_sentiment_nrc_pos <- subset(marker_sentiment_nrc, sentiment  == "anticipation"|sentiment  == "joy"|sentiment  == "positive"|
                                      sentiment == "surprise"| sentiment == "trust")

marker_sentiment_nrc_pos

#Plot to see the positive sentiment
positivemarker <- ggplot(marker_sentiment_nrc_pos, aes (x = date, y = n, color = sentiment))+
  labs(title = "Positive Sentiment The Marker Hotel by Date of Stay", x = "Date of Stay",y="Word Count",fill="Sentiment")+
  geom_line()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette="Greens")



#negative
marker_sentiment_nrc_neg <- subset(marker_sentiment_nrc, sentiment  == "anger"| sentiment  == "disgust"| sentiment  == "fear"|
                                      sentiment  == "negative"| sentiment  =="sadness")

marker_sentiment_nrc_neg

#Plot to see the negative sentiment
negativemarker <- ggplot(marker_sentiment_nrc_neg, aes (x = date, y = n, color = sentiment))+
  labs(title = "Negative Sentiment The Marker Hotel by Date of Stay", x = "Date of Stay",y="Word Count",fill="Sentiment")+
  geom_line()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette="Reds")


plot_grid(positiveashling, positivemespil,positivemarker,labels = "AUTO", ncol = 2)
plot_grid(negativeashling, negativemespil, negativemarker,labels = "AUTO", ncol = 2)


#USING AFIN OVERALL
#################################################################################################################################3
#
#
#Use afinn to get the sentiment analysis
hotels_T_afinn2_sentiment
hotels_T_afinn


hotels_T_afinn%>%
  group_by(hotelName)%>%
  summarize(sum(value))


hotels_T_afinn%>%
  group_by(hotelName)%>%
  summarize(mean(value))




#USING BING OVERALL
#################################################################################################################################3
#
#
#ASHLING
ashlingbing

ashlingbing_overall <- ashlingbing%>%
  group_by(sentiment)%>%
  count(sentiment)


ashlingbing_overall

#Plot to see the sentiment by month
chart_ashling <- ggplot(ashlingbing_overall, aes (x = "",y = n, fill = sentiment))+
  labs(title = "Overall Sentiment Analysis The Aishling Hotel",x = element_blank(), y="Word Count",fill="Sentiment")+
  geom_bar(width = 1, stat = "identity", alpha = 0.5)+
  scale_color_manual(values = c("#990000","#0B6623"))+
  scale_fill_manual(values = c("#990000","#0B6623"))+
  theme(plot.title = element_text(hjust = 0.5))


pie_ashling <- chart_ashling  + coord_polar("y", start=0)
pie_ashling 






#MESPIL
mespilbing

mespilbing_overall <- mespilbing%>%
  group_by(sentiment)%>%
  count(sentiment)


mespilbing_overall

#Plot to see the sentiment by month
chart_mespil <- ggplot(mespilbing_overall, aes (x = "",y = n, fill = sentiment))+
  labs(title = "Overall Sentiment Analysis The Mespil Hotel",x = element_blank(), y="Word Count",fill="Sentiment")+
  geom_bar(width = 1, stat = "identity", alpha = 0.5)+
  scale_color_manual(values = c("#990000","#0B6623"))+
  scale_fill_manual(values = c("#990000","#0B6623"))+
  theme(plot.title = element_text(hjust = 0.5))


pie_mespil <- chart_mespil  + coord_polar("y", start=0)
pie_mespil 





#MARKER
markerbing

markerbing_overall <- markerbing%>%
  group_by(sentiment)%>%
  count(sentiment)


markerbing_overall

#Plot to see the sentiment by month
chart_marker <- ggplot(markerbing_overall, aes (x = "",y = n, fill = sentiment))+
  labs(title = "Overall Sentiment Analysis The Marker Hotel",x = element_blank(), y="Word Count",fill="Sentiment")+
  geom_bar(width = 1, stat = "identity", alpha = 0.5)+
  scale_color_manual(values = c("#990000","#0B6623"))+
  scale_fill_manual(values = c("#990000","#0B6623"))+
  theme(plot.title = element_text(hjust = 0.5))


pie_marker <- chart_marker  + coord_polar("y", start=0)
pie_marker 




plot_grid(pie_ashling, pie_mespil, pie_marker,labels = "AUTO", ncol = 2)



#most popular words ashling
ashling_words <- ashlingbing%>%
  group_by(word)%>%
  count(sentiment)


ashling_words %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE, alpha = 0.5) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment - The Ashling Hotel",
       x = NULL) +
  coord_flip()+
  scale_fill_manual(values = c("#990000","#0B6623"))




#most popular words mespil
mespil_words <- mespilbing%>%
  group_by(word)%>%
  count(sentiment)


mespil_words %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE,alpha = 0.5) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment - The Mespil Hotel",
       x = NULL) +
  coord_flip()+
  scale_fill_manual(values = c("#990000","#0B6623"))





#most popular words marker
marker_words <- markerbing%>%
  group_by(word)%>%
  count(sentiment)


marker_words %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE,alpha = 0.5) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment - The Marker Hotel",
       x = NULL) +
  coord_flip()+
  scale_fill_manual(values = c("#990000","#0B6623"))






#wordcloud positive and negative ashling
ashling_words %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#990000", "#0B6623"),
                   max.words = 100)


#wordcloud positive and negative mespil
mespil_words %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#990000", "#0B6623"),
                   max.words = 100)


#wordcloud positive and negative marker
marker_words %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#990000", "#0B6623"),
                   max.words = 100)

#USING NRC OVERALL
#################################################################################################################################3
#
#
#ashling
theAishlingHotel_sentiment_nrc_pos_overall <- ashling_sentiment_nrc_pos %>% 
  group_by(sentiment)%>%
  count(sentiment)

theAishlingHotel_sentiment_nrc_pos_overall


posnrcashling <- ggplot(theAishlingHotel_sentiment_nrc_pos_overall, aes (x = "",y = n, fill = sentiment))+
  labs(title = "Overall Positive Sentiment Analysis The Aishling Hotel",x = element_blank(), y="Word Count",fill="Sentiment")+
  geom_bar(width = 1, stat = "identity", alpha = 0.5)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette="Greens")


theAishlingHotel_sentiment_nrc_neg_overall <- ashling_sentiment_nrc_neg %>% 
  group_by(sentiment)%>%
  count(sentiment)

theAishlingHotel_sentiment_nrc_neg_overall

negnrcashling <- ggplot(theAishlingHotel_sentiment_nrc_neg_overall, aes (x = "",y = n, fill = sentiment))+
  labs(title = "Overall Negative Sentiment Analysis The Aishling Hotel",x = element_blank(), y="Word Count",fill="Sentiment")+
  geom_bar(width = 1, stat = "identity", alpha = 0.5)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette="Reds")



#mespil
theMespilHotel_sentiment_nrc_pos_overall <- mespil_sentiment_nrc_pos %>% 
  group_by(sentiment)%>%
  count(sentiment)

theMespilHotel_sentiment_nrc_pos_overall


posnrcmespil <- ggplot(theMespilHotel_sentiment_nrc_pos_overall, aes (x = "",y = n, fill = sentiment))+
  labs(title = "Overall Positive Sentiment Analysis The Mespil Hotel",x = element_blank(), y="Word Count",fill="Sentiment")+
  geom_bar(width = 1, stat = "identity", alpha = 0.5)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette="Greens")


theMespilHotel_sentiment_nrc_neg_overall <- mespil_sentiment_nrc_neg %>% 
  group_by(sentiment)%>%
  count(sentiment)

theMespilHotel_sentiment_nrc_neg_overall

negnrcmespil <- ggplot(theMespilHotel_sentiment_nrc_neg_overall, aes (x = "",y = n, fill = sentiment))+
  labs(title = "Overall Negative Sentiment Analysis The Mespil Hotel",x = element_blank(), y="Word Count",fill="Sentiment")+
  geom_bar(width = 1, stat = "identity", alpha = 0.5)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette="Reds")




#marker
theMarkerHotel_sentiment_nrc_pos_overall <- marker_sentiment_nrc_pos %>% 
  group_by(sentiment)%>%
  count(sentiment)

theMarkerHotel_sentiment_nrc_pos_overall


posnrcmarker <- ggplot(theMarkerHotel_sentiment_nrc_pos_overall, aes (x = "",y = n, fill = sentiment))+
  labs(title = "Overall Positive Sentiment Analysis The Marker Hotel",x = element_blank(), y="Word Count",fill="Sentiment")+
  geom_bar(width = 1, stat = "identity", alpha = 0.5)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette="Greens")


theMarkerHotel_sentiment_nrc_neg_overall <- marker_sentiment_nrc_neg %>% 
  group_by(sentiment)%>%
  count(sentiment)

theMarkerHotel_sentiment_nrc_neg_overall

negnrcmarker <- ggplot(theMarkerHotel_sentiment_nrc_neg_overall, aes (x = "",y = n, fill = sentiment))+
  labs(title = "Overall Negative Sentiment Analysis The Marker Hotel",x = element_blank(), y="Word Count",fill="Sentiment")+
  geom_bar(width = 1, stat = "identity", alpha = 0.5)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette="Reds")


plot_grid(posnrcashling, posnrcmespil, posnrcmarker,labels = "AUTO", ncol = 2)
plot_grid(negnrcashling, negnrcmespil, negnrcmarker,labels = "AUTO", ncol = 2)



