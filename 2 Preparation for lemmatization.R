
library(dplyr)
library(stringr)


#Ïîäãîòîâêà äàííûõ

tweets = read.csv("C:/Users/User/Desktop/DATA/ÖÏÊ/tweets.csv")
tweets$Time = str_sub(tweets$Time, 1, 10)
tweets$Time = as.Date(tweets$Time)
tweets$Tweet = gsub("@\\w+ *", "", tweets$Tweet)
tweets$Tweet = str_replace_all(tweets$Tweet, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")
tweets = tweets%>%filter(Time>='2022-01-01')
tweets = tweets%>%mutate(doc_id=rownames(tweets))

before = tweets%>%filter(Time<'2022-02-24')
after = tweets%>%filter(Time>='2022-02-24')

write.csv(before, file='beforenolemma.csv', fileEncoding ='UTF-8', row.names=FALSE)
write.csv(after, file='afternolemma.csv', fileEncoding ='UTF-8', row.names=FALSE)

