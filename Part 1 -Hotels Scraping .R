library(rvest)
library(stringr)
library(dplyr)
library(readr)
## Get the page Number

# Mespil Hotel - https://www.tripadvisor.ie/Hotel_Review-g186605-d212567-Reviews-Mespil_Hotel-Dublin_County_Dublin.html
# Ashling Hotel - https://www.tripadvisor.ie/Hotel_Review-g186605-d212690-Reviews-Ashling_Hotel-Dublin_County_Dublin.html
# Marker Hotel - https://www.tripadvisor.ie/Hotel_Review-g186605-d4062795-Reviews-The_Marker_Hotel-Dublin_County_Dublin.html

########################################
# Get the number of pages with reviews #
########################################
webpageForPgNum <- read_html("https://www.tripadvisor.ie/Hotel_Review-g186605-d212690-Reviews-Ashling_Hotel-Dublin_County_Dublin.html")
webpageForPgNum

pageNumber <- str_sub(webpageForPgNum %>%
                        html_nodes(".pageNumbers") %>%
                        html_text())

pageNumber <- substring(pageNumber, 8)
pageNumber <- as.numeric(pageNumber)

#URL's Pagenumbers for FOR Loop
URLpageNumber <- pageNumber*5

#####################################
# Generating the URLs to be scraped #
#####################################
#creating empty list
webpage_list <- vector(mode = "list")
webpage_list


#Links for scrapings
# Ashling Hotel - https://www.tripadvisor.ie/Hotel_Review-g186605-d212690-Reviews-or{n}-Ashling_Hotel-Dublin_County_Dublin.html#REVIEWS
# Mespil Hotel - https://www.tripadvisor.ie/Hotel_Review-g186605-d212567-Reviews-or{n}-Mespil_Hotel-Dublin_County_Dublin.html#REVIEWS
# Marker Hotel - https://www.tripadvisor.ie/Hotel_Review-g186605-d4062795-Reviews-or{n}-The_Marker_Hotel-Dublin_County_Dublin.html#REVIEWS

for(n in seq(from=5, to=URLpageNumber, by=5)){
  webpage_list[[n]] <- glue::glue("https://www.tripadvisor.ie/Hotel_Review-g186605-d212690-Reviews-or{n}-Ashling_Hotel-Dublin_County_Dublin.html#REVIEWS")
}
webpage_list

#dropping the empty values
webpage_list[sapply(webpage_list,is.null)] <- NULL
tail(webpage_list, 10)
# make the list object an unlisted object
webpage_list2 <- unlist(webpage_list)
class(webpage_list2)
# convert to a character value
webpage_list<- as.character(webpage_list)
for (o in 1:n) {
  webpage_list[o]<-as.character(webpage_list[o])
}
# create a records list the same size as the webpage_list urls
for(p in webpage_list){
  records <- list(length = (length(webpage_list)))
}
records
###############################################################
# Scrape each URL for the review and date of stay information #
###############################################################

for (x in seq_along(webpage_list2)) {
  url <- read_html(webpage_list2[x])
  dateOfStay <- str_c(url %>% 
                        html_nodes("._34Xs-BQm") %>% 
                        html_text())
  # reviewTitle <- str_sub(url %>%
  #                          html_nodes(".glasR4aX")%>%
  #                          html_text())
  review <- str_sub(url %>%
                      html_nodes(".IRsGHoPm") %>%
                      html_text())
  records[[x]] <- data_frame(dateOfStay = dateOfStay, review = review)#, reviewTitle = reviewTitle
}

#############################################
# Build Data Frame with scraped information #
#############################################
#Build theMarkerHotel_DF
theMarkerHotel_DF <- bind_rows(records)
theMarkerHotel_DF
# Remove "Date of Stay: " from dateOfStay column
theMarkerHotel_DF$dateOfStay
theMarkerHotel_DF$dateOfStay <- str_sub(theMarkerHotel_DF$dateOfStay, 15, -1)
theMarkerHotel_DF

#Build theMespilHotel_DF
theMespilHotel_DF <- bind_rows(records)
theMespilHotel_DF
# Remove "Date of Stay: " from dateOfStay column
theMespilHotel_DF$dateOfStay
theMespilHotel_DF$dateOfStay <- str_sub(theMespilHotel_DF$dateOfStay, 15, -1)
theMespilHotel_DF

#Build theAshlingHotel_DF
theAshlingHotel_DF <- bind_rows(records)
theAshlingHotel_DF
# Remove "Date of Stay: " from dateOfStay column
theAshlingHotel_DF$dateOfStay
theAshlingHotel_DF$dateOfStay <- str_sub(theAshlingHotel_DF$dateOfStay, 15, -1)
theAshlingHotel_DF

############################
# Export dataframe to .csv #
############################
write_csv(theMarkerHotel_DF, "TheMarkerHotel.csv")
write_csv(theMespilHotel_DF, "TheMespilHotel.csv")
write_csv(theAshlingHotel_DF, "TheAshlingHotel.csv")
