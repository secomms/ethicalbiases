# Set Bearer Token <- TwitterAPI
Sys.setenv(Bearer_Token = "XXXX")

install.packages("rtweet") 
install.packages("viewtweets")
library(rtweet)

# Collection of 100 tweet for each user: World Health Organization (WHO), World Trade Organization (WTO), International Monetary Fund (IMF), North Atlantic Treaty Organization (NATO)
rt<-get_timeline(user=c("WHO", "wto", "IMFNews", "NATO"), n=100)
textid = c (1:length (rt$full_text))

Tweets = data.frame(text_id = textid, full_text = rt$full_text)

install.packages("writexl")
library("writexl")
#set the path
write_xlsx(Tweets,"/Users/..../Desktop/Tweets.xlsx")