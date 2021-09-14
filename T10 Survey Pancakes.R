##
#  Survey, Business Insights, and Shiny App
#
#  Group Presentation (A2), Text Analytics
#  Author:      Philipp Domenig,
#               Serena Dougan,
#               Carolina Jativa,
#               Dachao Sun
#               -- Team 10
#
#  Email:       dsun2020@student.hult.edu
#  Institution: Hult Intl Business School
#  Date:        February 15, 2021
#  
############################################
#############Loading Libraries##############
############################################
library(tidyverse)
library(tidytext)
library(textdata)
library(dplyr)
library(widyr)
library(tidyr)
library(stringr)
library(scales)
library(twitteR)
library(rtweet)
library(tm)
library(ggplot2)
library(igraph)
library(ggraph)
library(reshape2)
library(wordcloud)
library(readr)

############################################
##########Loading Survey Data ##############
############################################

#"/Users/domenig/Documents/_Shared folders/_Philipp Shared/Dual Degree MsBA/2nd Term/DAT-5317_Text Analytics and NLP/Assignments/Team/T10 Group Assignment/Questions txt/Question 1.txt", 
"\t"

Question_1 <- read_delim("~/Documents/Hult Master Spring 2020/Text Analytics/T10 Group Assignment/Questions txt/Question 1.txt", 
                         "\t", escape_double = FALSE, col_names = FALSE, 
                         trim_ws = TRUE)


Question_2 <- read_delim("~/Documents/Hult Master Spring 2020/Text Analytics/T10 Group Assignment/Questions txt/Question 2.txt", 
                         "\t", escape_double = FALSE, col_names = FALSE, 
                         trim_ws = TRUE)

Question_3 <- read_delim("~/Documents/Hult Master Spring 2020/Text Analytics/T10 Group Assignment/Questions txt/Question 3.txt", 
                         "\t", escape_double = FALSE, col_names = FALSE, 
                         trim_ws = TRUE)

Question_4 <- read_delim("~/Documents/Hult Master Spring 2020/Text Analytics/T10 Group Assignment/Questions txt/Question 4.txt", 
                         "\t", escape_double = FALSE, col_names = FALSE, 
                         trim_ws = TRUE)

Question_5 <- read_delim("~/Documents/Hult Master Spring 2020/Text Analytics/T10 Group Assignment/Questions txt/Question 5.txt", 
                         "\t", escape_double = FALSE, col_names = FALSE, 
                         trim_ws = TRUE)

Question_6 <- read_delim("~/Documents/Hult Master Spring 2020/Text Analytics/T10 Group Assignment/Questions txt/Question 6.txt", 
                         "\t", escape_double = FALSE, col_names = FALSE, 
                         trim_ws = TRUE)


Question_7 <- read_delim("~/Documents/Hult Master Spring 2020/Text Analytics/T10 Group Assignment/Questions txt/Question 7.txt", 
                         "\t", escape_double = FALSE, col_names = FALSE, 
                         trim_ws = TRUE)

Question_8 <- read_delim("~/Documents/Hult Master Spring 2020/Text Analytics/T10 Group Assignment/Questions txt/Question 8.txt", 
                         "\t", escape_double = FALSE, col_names = FALSE, 
                         trim_ws = TRUE)

#Assigning question number to each row
Question_1$question <- "Q1"
Question_2$question <- "Q2"
Question_3$question <- "Q3"
Question_4$question <- "Q4"
Question_5$question <- "Q5"
Question_6$question <- "Q6"
Question_7$question <- "Q7"
Question_8$question <- "Q8"

#Creating a data frame with survey data
survey_df <- rbind.data.frame(Question_1,
                              Question_2,
                              Question_3,
                              Question_4,
                              Question_5,
                              Question_6,
                              Question_7,
                              Question_8)
#calling the stop words library
data(stop_words)

#creating an object with a txt file of custom stop words
to_keep  <-  read.delim(file="/Users/carolinajativa/Documents/Hult Master Spring 2020/Text Analytics/T10 Group Assignment/tokeep.txt", 
                        header=FALSE, sep = "\n") %>%
                        rename(word = V1)

#creating my own stop_words
custom_stop_words <- tribble(
                              ~word, ~lexicon,
                              "yeah", "CUSTOM",
                              "pancakes", "CUSTOM",
                              "pancake", "CUSTOM",
                              "eat", "CUSTOM",
                              "prefer", "CUSTOM",
                              "feel", "CUSTOM",
                              "favorite", "CUSTOM",
                              "toppings", "CUSTOM",
)



#joining the custom stop words to the stop words
stop_words2 <- stop_words  %>%
  anti_join(to_keep) %>% 
  bind_rows(custom_stop_words)

survey_counts <- survey_df %>%
  unnest_tokens(word, X1) %>%
  anti_join(stop_words2)%>%
  count(word, sort=TRUE)

#deleting numbers from tokens
#nums <- tokens_clean %>% 
#  filter(str_detect(word, "^[0-9]")) %>% 
#  select(word) %>% 
#  unique()

#survey_counts <- survey_counts %>% 
#anti_join(nums, by = "word")

############################################
########Tokenizing Survey Data #############
############################################
tok_Q1 <- survey_df %>% 
  filter(question == "Q1") %>% 
  unnest_tokens(word, X1) %>%
  anti_join(stop_words2)%>%
  count(word, sort = TRUE)

tok_Q2 <- survey_df %>% 
  filter(question == "Q2") %>% 
  unnest_tokens(word, X1) %>%
  anti_join(stop_words2)

tok_Q3 <- survey_df %>% 
  filter(question == "Q3") %>% 
  unnest_tokens(word, X1) %>%
  anti_join(stop_words2)

tok_Q4 <- survey_df %>% 
  filter(question == "Q4") %>% 
  unnest_tokens(word, X1) %>%
  anti_join(stop_words2)

tok_Q5 <- survey_df %>% 
  filter(question == "Q5") %>% 
  unnest_tokens(word, X1) %>%
  anti_join(stop_words2)

tok_Q6 <- survey_df %>% 
  filter(question == "Q6") %>% 
  unnest_tokens(word, X1) %>%
  anti_join(stop_words2)

tok_Q7 <- survey_df %>% 
  filter(question == "Q7") %>% 
  unnest_tokens(word, X1) %>%
  anti_join(stop_words2)

tok_Q8 <- survey_df %>% 
  filter(question == "Q8") %>% 
  unnest_tokens(word, X1)


############################################
#########Token frequency histograms#########
############################################

freq_survey <-survey_counts %>%
  filter(n > 10) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n))+
  geom_col()+
  labs(title = "Survey Token Frequencies")+
  coord_flip()
print(freq_survey)


############################################
################ Question 1 ################
############################################

############Question 1: Frequency###########
freq_Q1 <- tok_Q1 %>%
  count(word, sort=TRUE) %>% 
  filter(n > 1) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n))+
  geom_col()+
  labs(title = "Frequencies: Do you like pancakes?")+
  coord_flip()
print(freq_Q1)


##########################################################
################ tf_idf Q1 #########################
##########################################################

tf_idf_Q1 <- tok_Q1 %>%
  unnest_tokens(word, X1) %>%
  anti_join(stop_words2) %>%
  count(author, word, sort=TRUE) %>%
  ungroup()

total_words_Q1 <- tf_idf_Q1 %>%
  group_by(author) %>%
  summarize(total=sum(n))

questions_words_Q1 <- left_join(tf_idf_Q1, total_words_Q1)

print(questions_words_Q1)

ggplot(questions_words, aes(n/total, fill = author))+
  geom_histogram(show.legend=FALSE)+
  xlim(NA, 0.1) +
  facet_wrap(~author, ncol=2, scales="free_y")

######################################
########## ZIPF's law ################
######################################

freq_by_rank_Q1 <- questions_words_Q1 %>%
  group_by(author) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)
freq_by_rank_Q1

# plot ZIPF's Law
freq_by_rank_Q1 %>%
  ggplot(aes(rank, `term frequency`, color=author))+
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()

###################################################
################# TF_IDF ##########################
###################################################
questions_words_idf_Q1 <-questions_words_Q1 %>%
  bind_tf_idf(word, author, n) 

#reorganize the table
questions_words_idf_Q1 %>%
  arrange(desc(tf_idf_Q1))

#graphical approach
questions_words_idf_Q1 %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(author) %>%
  top_n(5) %>% #top highest tfidf tokens
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=author))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~author, ncol=2, scales="free")+
  coord_flip()


#######Question 1: Bing Sentiment###########
bing_Q1 <- tok_Q1 %>%
  inner_join(get_sentiments("bing")) %>%  
  count(word, sentiment, sort=TRUE) %>% 
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "grey80"),
                   max.words=500, scale=c(2, 2),
                   title.size=3)

#######Question 1: Afinn Sentiment###########
afinn_Q1 <- tok_Q1 %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(word) %>%
  summarise(sentiment = sum(value)) %>%
  mutate(method = "AFINN")

afinn_Q1_plot <- afinn_Q1 %>%
  ggplot(aes(word, sentiment, fill = method)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~method, ncol = 1, scales = "free_y")+
  labs(x = "Question 1", y = NULL, title = "AFINN Sentiment: Do you like pancakes?")

print(afinn_Q1_plot )


##########Question 1: Bigrams#############
Q1_bigrams <- survey_df %>% 
  filter(question == "Q1") %>% 
  unnest_tokens(bigram, X1, token = "ngrams", n=2)

Q1_bigrams #We want to see the bigrams (words that appear together, "pairs")

Q1_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
Q1_separated <- Q1_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

Q1_filtered <- Q1_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
Q1_counts <- Q1_filtered %>%
  count(word1, word2, sort = TRUE) %>% 
  filter(n > 1)
#want to see the new bigrams
Q1_counts


############################################
################ Question 2 ################
############################################

############Question 2: Frequency###########
freq_Q2 <- tok_Q2 %>%
  count(word, sort=TRUE) %>%
  filter(n > 4) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n))+
  geom_col()+
  labs(title = "Frequencies: At what time do you prefer to eat pancakes?")+
  coord_flip()
print(freq_Q2)

############################################
################ Question 3 ################
############################################

############Question 3: Frequency###########
freq_Q3 <- tok_Q3 %>%
  count(word, sort=TRUE) %>%
  filter(n > 4) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n))+
  geom_col()+
  labs(title = "Frequencies: How often do you eat pancakes?")+
  coord_flip()
print(freq_Q3)

##########Question 3: Bigrams#############
custom_stop_words1 <- tribble(
  ~word, ~lexicon,
  "pancakes", "CUSTOM"
)


Q3_bigrams <- survey_counts %>% 
  filter(question == "Q3") %>% 
  unnest_tokens(bigram, X1, token = "ngrams", n=2)%>% 
  anti_join(custom_stop_words1)

Q3_bigrams #We want to see the bigrams (words that appear together, "pairs")

Q3_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
Q3_separated <- Q3_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

Q3_filtered <- Q3_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
Q3_counts <- Q3_filtered %>%
  count(word1, word2, sort = TRUE) %>%
  anti_join(custom_stop_words1)

#want to see the new bigrams
Q3_counts


######################################################
####### VISUALISING A BIGRAM NETWORK #################
######################################################

Q3_bigram_graph <- Q3_counts %>%
  graph_from_data_frame()

Q3_bigram_graph

ggraph(Q3_bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)



############################################
################ Question 4 ################
############################################

############Question 4: Frequency###########
freq_Q4 <- tok_Q4 %>%
  count(word, sort=TRUE) %>%
  filter(n > 2) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n))+
  geom_col()+
  labs(title = "Frequencies: Do you prefer savory or sweet pancakes?")+
  coord_flip()
print(freq_Q4)


############################################
################ Question 5 ################
############################################
custom_stop_words2 <- tribble(
  ~word, ~lexicon,
  "would", "CUSTOM"
)

############Question 5: Frequency###########
freq_Q5 <- tok_Q5 %>%
  count(word, sort=TRUE) %>%
  anti_join(custom_stop_words2) %>%
  filter(n > 4) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n))+
  geom_col()+
  labs(title = "Frequencies: What are your three favorite pancake toppings?")+
  coord_flip()
print(freq_Q5)

############################################
################ Question 6 ################
############################################

############Question 6: Frequency###########
custom_stop_words3 <- tribble(
  ~word, ~lexicon,
  "like", "CUSTOM"
)

freq_Q6 <- tok_Q6 %>%
  count(word, sort=TRUE) %>%
  anti_join(custom_stop_words3)%>%
  filter(n > 3) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n))+
  geom_col()+
  labs(title = "Frequencies: Describe why you prefer thin or fluffy pancakes?")+
  coord_flip()
print(freq_Q6)

############Question 6: Bing Sentiment ########### #NOT WORKING!!!!!!!
#IF YOU RUN UNTIL COUNT YOU ONLY SEE POSITIVE WORDS

nrc_Q6 <- subset(tok_Q6, word != "batter") %>%
  inner_join(get_sentiments("nrc")) %>% 
  count(word, sentiment, sort=TRUE) 
#  acast(word ~sentiment, value.var="n", fill=0) %>%
#  comparison.cloud(colors = c("grey20", "grey80"),
#                  max.words=500, scale=c(1, 1),
#                   title.size=1.5)



nrc_plot_Q6 <- nrc_Q6 %>%
  group_by(sentiment) %>% 
  slice_max(n, n = 5, with_ties = FALSE) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(n, word)) +
  geom_col() +
  facet_wrap(~sentiment, scales = "free") +
  labs(x = "Question 6", y = NULL)

print(nrc_plot_Q6)

#######Question 6: Afinn Sentiment###########
afinn_Q6 <- tok_Q6 %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(word) %>%
  summarise(sentiment = sum(value)) %>%
  mutate(method = "AFINN")

afinn_Q6_plot <- afinn_Q6 %>%
  ggplot(aes(word, sentiment, fill = method)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~method, ncol = 1, scales = "free_y")+
  labs(x = "Question 6", y = NULL, title = "AFINN Sentiment:  Describe why you prefer thin or fluffy pancakes?")

print(afinn_Q6_plot )

############################################
################ Question 7 ################
############################################

############Question 7: Frequency###########
freq_Q7 <- tok_Q7 %>%
  count(word, sort=TRUE) %>%
  filter(n > 4) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n))+
  geom_col()+
  labs(title = "Frequencies: Describe how pancakes make you feel?")+
  coord_flip()
print(freq_Q7)

############Question 7: Bing Sentiment ###########
bing_Q7 <- tok_Q7 %>%
  inner_join(get_sentiments("bing")) %>%  
  count(word, sentiment, sort=TRUE) %>% 
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "grey80"),
                   max.words=500, scale=c(2, 2),
                   title.size=3)

#######Question 7: Afinn Sentiment###########
afinn_Q7 <- tok_Q7 %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(word) %>%
  summarise(sentiment = sum(value)) %>%
  mutate(method = "AFINN")

afinn_Q7_plot <- afinn_Q7 %>%
  ggplot(aes(word, sentiment, fill = method)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~method, ncol = 1, scales = "free_y")+
  labs(x = "Question 7", y = NULL, title = "AFINN Sentiment: Describe how pancakes make you feel?")

print(afinn_Q7_plot )


############################################
################ Question 8 ################
############################################

#######Question 8: Frequency###########
freq_Q8 <- tok_Q8 %>%
  count(word, sort=TRUE) # we need this to eliminate all the low count words
  
  
plot_Q8 <- ggplot(freq_Q8, aes(x="", y=n, fill=word))+
  geom_bar(stat="identity", width = 1, color="white")+
  labs(title = "Frequencies: Do you prefer pancakes or waffles?")+
  coord_polar("y", start = 0) +
  theme_void()
print(plot_Q8)


###############TFIDF ALL QUESTIONS######################
questions <- bind_rows(mutate(Question_1, author = "Question 1"),
                      mutate(Question_2, author = "Question 2"),
                      mutate(Question_3, author = "Question 3"),
                      mutate(Question_4, author = "Question 4"),
                      mutate(Question_5, author = "Question 5"),
                      mutate(Question_6, author = "Question 6"),
                      mutate(Question_7, author = "Question 7"),
                      mutate(Question_8, author = "Question 8"),)
questions_tokens <- questions %>%
  unnest_tokens(word, X1) %>%
  anti_join(stop_words2) %>%
  count(word, sort=T) 

#############################################
###### N-grams and tokenizing ###############
#############################################

questions_bigrams <- questions %>%
  unnest_tokens(bigram, X1, token = "ngrams", n=2)%>%
  filter(!is.na(bigram))

questions_bigrams #We want to see the bigrams (words that appear together, "pairs")

questions_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
questions_separated <- questions_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

questions_filtered <- questions_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
questions_counts <- questions_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
questions_counts

###########################################################
###### We can also apply the tf_idf framework  ############
################### on our bigram  ########################
###########################################################

questions_united <- questions_filtered %>%
  unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section

questions_bigram_tf_idf <- questions_united %>%
  count(author, bigram) %>%
  bind_tf_idf(bigram, author, n) %>%
  arrange(desc(tf_idf))

questions_bigram_tf_idf


######################################################
####### VISUALISING A BIGRAM NETWORK #################
######################################################

questions_bigram_graph <- questions_counts %>%
  filter(n>1) %>%
  graph_from_data_frame()

questions_bigram_graph

ggraph(questions_bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

##########################################################
################ tf_idf Analysis#########################
##########################################################

tf_idf_questions <- bind_rows(mutate(Question_1, author = "Question 1"),
                              mutate(Question_2, author = "Question 2"),
                              mutate(Question_3, author = "Question 3"),
                              mutate(Question_4, author = "Question 4"),
                              mutate(Question_5, author = "Question 5"),
                              mutate(Question_6, author = "Question 6"),
                              mutate(Question_7, author = "Question 7"),
                              mutate(Question_8, author = "Question 8")) %>%
  unnest_tokens(word, X1) %>%
  anti_join(stop_words2) %>%
  count(author, word, sort=TRUE) %>%
  ungroup()

total_words <- tf_idf_questions %>%
  group_by(author) %>%
  summarize(total=sum(n))

questions_words <- left_join(tf_idf_questions, total_words)

print(questions_words)

ggplot(questions_words, aes(n/total, fill = author))+
  geom_histogram(show.legend=FALSE)+
  xlim(NA, 0.1) +
  facet_wrap(~author, ncol=2, scales="free_y")

######################################
########## ZIPF's law ################
######################################

freq_by_rank <- questions_words %>%
  group_by(author) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)
freq_by_rank

# plot ZIPF's Law
freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color=author))+
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()

###################################################
################# TF_IDF ##########################
###################################################
questions_words_idf <-questions_words %>%
  bind_tf_idf(word, author, n) 

#reorganize the table
questions_words_idf %>%
  arrange(desc(tf_idf))

#graphical approach
questions_words_idf %>%
  anti_join(stop_words2) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(author) %>%
  top_n(5) %>% #top highest tfidf tokens
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=author))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~author, ncol=2, scales="free")+
  coord_flip()








