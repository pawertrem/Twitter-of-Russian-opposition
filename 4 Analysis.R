library(udpipe)
library(readxl)
library(tidytext)
library(stopwords)
library(quanteda)
library(ggplot2)
library(ggwordcloud)
library(wordcloud)
library(wordcloud2)
library(ggpubr)
library(readxl)
library(dplyr)
library(stringr)
library(data.table)
library(LDAvis)
library(topicmodels)
library(tidyr)
library(readr)
library(cowplot)
library(xlsx)
library(hrbrthemes)

#Import of data

beforelemmas = read.csv("~/before.csv", fileEncoding = 'UTF-8')
afterlemmas = read.csv("~/after.csv", fileEncoding = 'UTF-8')

#Rest of preparation of data for analysis (filtering nouns and proper nouns, cleaning of stop-words that has not been removed earlier and of nonsensical characters)

beforelemmas$X=rownames(beforelemmas)
beforelemmas$Tweet=gsub("\\[|\\]|'|»|«|‘|¦|’|\\.\\.\\.|\"|-", "", beforelemmas$Tweet)
beforelemmas=beforelemmas%>%filter(Tweet!="")
beforelemmas$Tweet = iconv(beforelemmas$Tweet, to = 'UTF-8')
Encoding(beforelemmas$Tweet)
udmodel <- udpipe_download_model(language = "russian")
udmodel <- udpipe_load_model(file = udmodel$file_model)
beforelemma = udpipe(x = beforelemmas$Tweet, object='russian')
beforelemma = beforelemma %>% filter(upos == 'NOUN'|upos == 'PROPN')%>%select(doc_id, token)
colnames(beforelemma) = c('doc_id', 'lemma')
stopw = c(stopwords(language = "ru"), 'ýòî', 'êîòîðûé')
stopwen = stopwords(language = "en")
stoptw = beforelemma %>% filter(lemma %in% stopw | lemma %in% stopwen)
beforelemma = anti_join(beforelemma, stoptw, by = "lemma")
beforelemma$doc_id = str_replace_all(beforelemma$doc_id, 'doc', '')
beforelemma$doc_id = as.numeric(beforelemma$doc_id)
beforelemma=beforelemma%>%filter(lemma!='ìî÷ü' & lemma!='ãîä' & lemma!='áàò' & lemma!='íàø' & nchar(lemma)>2)
beforelemma$lemma=gsub('”','',beforelemma$lemma)

afterlemmas$X=rownames(afterlemmas)
afterlemmas$Tweet=gsub("\\[|\\]|'|»|«|‘|¦|’|\\.\\.\\.|\"|-|”", "", afterlemmas$Tweet)
afterlemmas=afterlemmas%>%filter(Tweet!="")
afterlemmas$Tweet = iconv(afterlemmas$Tweet, to = 'UTF-8')
Encoding(afterlemmas$Tweet)
afterlemma = udpipe(x = afterlemmas$Tweet, object='russian')
afterlemma = afterlemma %>% filter(upos == 'NOUN'|upos == 'PROPN')%>%select(doc_id, token)
colnames(afterlemma) = c('doc_id', 'lemma')
stoptw = afterlemma %>% filter(lemma %in% stopw | lemma %in% stopwen)
afterlemma = anti_join(afterlemma, stoptw, by = "lemma")
afterlemma$doc_id = str_replace_all(afterlemma$doc_id, 'doc', '')
afterlemma$doc_id = as.numeric(afterlemma$doc_id)
afterlemma=afterlemma%>%filter(lemma!='ìî÷ü' & lemma!='ãîä' & lemma!='áàò' & lemma!='íàø' & nchar(lemma)>2)

#Word Clouds

warcloudbefore = beforelemma %>%
  dplyr::count(lemma, sort=TRUE) %>% 
  top_n(100, n)
warcloudafter = afterlemma %>%
  dplyr::count(lemma, sort=TRUE) %>% 
  top_n(100, n)
wordcloud2(data = warcloudbefore, shuffle = FALSE, color='random-light', rotateRatio=0)
wordcloud2(data = warcloudafter, shuffle = FALSE, color='random-light', rotateRatio = 0)

#LDA

#Before 24.02

word_count = beforelemma%>%count(doc_id, lemma, sort = TRUE)%>%ungroup()
dtm = word_count %>% cast_dtm(doc_id, lemma, n)
lda1 <- LDA(dtm, k = 20, control = list(seed = 12345))
lda1

lda_topics <- tidy(lda1, matrix = "beta")

before_top <- lda_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

before_top %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  xlab('Ëåììà')+
  ylab('per-topic-per-word probabilities')+
  coord_flip()

#After 24.02

word_count = afterlemma%>%count(doc_id, lemma, sort = TRUE)%>%ungroup()
dtm = word_count %>% cast_dtm(doc_id, lemma, n)
lda2 <- LDA(dtm, k = 40, control = list(seed = 12345))

lda_topics <- tidy(lda2, matrix = "beta")

after_top <- lda_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

after_top %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free")  +
  xlab('Ëåììà')+
  ylab('per-topic-per-word probabilities')+
  coord_flip()

#Log-Likelihood

g2 = function(a, b) {
  c = sum(a)
  d = sum(b)
  E1 = c * ((a + b) / (c + d))
  E2 = d * ((a + b) / (c + d))
  return(2*((a*log(a/E1+1e-7)) + (b*log(b/E2+1e-7))))
}

logratio = function(a, b) {
  return(log2((a/(sum(a)+1e-7)/(b/(sum(b)+1e-7)))))
}

afterlemman2=afterlemma%>%mutate(period='after')
beforelemman2=beforelemma%>%mutate(period='before')
afterbefore=rbind(afterlemman2, beforelemman2)
Rep_Dem = afterbefore %>%
  dplyr::count(lemma, period) %>%
  spread(period, n, fill = 0)%>%dplyr::filter(after > 5 | before > 5)


Rep_Dem_G2 = Rep_Dem %>% 
  mutate(g2=g2(before, after)) %>%
  arrange(desc(g2)) %>%
  mutate(g2 = round(g2, 2))

Rep_Dem_LR = Rep_Dem_G2 %>%
  mutate(logratio = logratio(before, after))

x = Rep_Dem_LR %>%
  filter(before > 0 & after > 0) %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(lemma = reorder(lemma, logratio)) %>%
  ggplot(aes(lemma, logratio, fill = logratio > 0)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  ylab("log odds ratio (Äî 24.02/Ïîñëå 24.02)") +
  xlab("Ëåììà")+
  scale_fill_discrete(name = "", labels = c("Ïîñëå 24.02", "Äî 24.02"))

x

#Sentiment Analysis

dict <- readr::read_csv("https://raw.githubusercontent.com/text-machine-lab/sentimental/master/sentimental/word_list/russian.csv")
beforesent=beforelemma%>%inner_join(dict, by=c('lemma'='word'))
aftersent=afterlemma%>%inner_join(dict, by=c('lemma'='word'))
mean(beforesent$score)
mean(aftersent$score)
sent$period=as.factor(sent$period)
ggplot(sent)+geom_boxplot(aes(x=period, y=score))
a=ggplot() +
  geom_histogram(data = beforesent, aes(x = score, y=..density..), fill="#008080", col="#483D8B", alpha = 0.5)+ggtitle('Äî 24.02')+xlab('Ýìîöèîíàëüíàÿ îêðàñêà (îò íåãàòèâíîé ê ïîëîæèòåëüíîé)')+ylab('Ïëîòíîñòü ðàñïðåäåëåíèÿ')+ylim(0,2)
b=ggplot() +
  geom_histogram(data = aftersent, aes(x = score, y=..density..), fill="#008080", col="#483D8B", alpha = 0.5)+ggtitle('Ïîñëå 24.02')+xlab('Ýìîöèîíàëüíàÿ îêðàñêà (îò íåãàòèâíîé ê ïîëîæèòåëüíîé)')+ylab('Ïëîòíîñòü ðàñïðåäåëåíèÿ')
SCATS <- plot_grid(a, b)
SCATS

warcloudbefore = beforesent %>%
  dplyr::count(lemma, sort=TRUE) %>% 
  top_n(50, n)
warcloudafter = aftersent %>%
  dplyr::count(lemma, sort=TRUE) %>% 
  top_n(50, n)
wordcloud2(data = warcloudbefore, shuffle = FALSE, color='random-light', rotateRatio=0)
wordcloud2(data = warcloudafter, shuffle = FALSE, color='random-light', rotateRatio = 0, size=0.5)

beforesentmonths = beforesent%>%left_join(beforenolemmas, by='doc_id')
aftersentmonths = aftersent%>%left_join(afternolemmas, by='doc_id')
beforesentmonths = beforesentmonths%>%mutate(month=months(as.Date(Time)))
aftersentmonths = aftersentmonths%>%mutate(month=months(as.Date(Time)))
beforesentmonths = beforesentmonths%>%filter(score<0)%>%select(lemma, month)%>%group_by(month)%>%count()
aftersentmonths = aftersentmonths%>%filter(score<0)%>%select(lemma, month)%>%group_by(month)%>%count()
beforesentmonths$month = ifelse(beforesentmonths$month=='Ôåâðàëü', 'Ôåâðàëü\n(äî 24.02)', ifelse(beforesentmonths$month=='Èþíü', 'Èþíü\n(äî 9-ãî)', beforesentmonths$month))
aftersentmonths$month = ifelse(aftersentmonths$month=='Ôåâðàëü', 'Ôåâðàëü\n(ïîñëå 24.02)', ifelse(aftersentmonths$month=='Èþíü', 'Èþíü\n(äî 9-ãî)', aftersentmonths$month))
year22sent = rbind(beforesentmonths, aftersentmonths)
year22sent$month=factor(year22sent$month, levels = c('ßíâàðü', 'Ôåâðàëü\n(äî 24.02)', 'Ôåâðàëü\n(ïîñëå 24.02)', 'Ìàðò', 'Àïðåëü', 'Ìàé', 'Èþíü\n(äî 9-ãî)'))
year22sent = year22sent%>%filter(month!='Èþíü\n(äî 9-ãî)')

ggplot(data=year22sent, aes(x=month, y=n)) +
  geom_line(aes(group=1), color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
  theme_ipsum() +
  ggtitle("Äèíàìèêà èñïîëüçîâàíèÿ íåãàòèâíîé ñòèëèñòè÷åñêè îêðàøåííîé ëåêñèêè")+
  xlab('')+
  ylab('')



