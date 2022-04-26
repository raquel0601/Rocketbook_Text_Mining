# importing libraries
library(textreadr)
library(magrittr)
library(rvest)
library("twitteR")
library("tm")
library(ggplot2)
library(tidytext)
library(tidytuesdayR)
library(tidyverse)
library(dplyr)
library(tidyr)
library(rtweet)
library(widyr)
library(stringr)
library(tidyselect)
library(knitr)
library(readxl)
library(tibble)


# creating a dataframe with tokens of reviews

  rocket<- rocket1%>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>% 
  anti_join(custom_stopwords) %>% 
  count(word,sort=TRUE)
 
  # dropping NAs from dataframe.
  rocket %>% 
    drop_na()
  
  # counting number of reviews by rating.
  rocket1 %>% 
    count(rating)%>% 
    group_by(rating)
  
#Let's visualize most common words,
  
  Top_rocket <- rocket %>% 
    filter(n>581)
  
# Bar Graph with most used words.
  Top_rocket %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col(fill="darkgoldenrod1") +
    xlab(NULL)+
    coord_flip()+
    labs(x="",
         y="",
         caption="Retrieved from Amazon Reviews")+
    theme(axis.text = element_text(family="AppleGothic"))+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(axis.text = element_text(size = 12))+
    theme(plot.title = element_text(size = 13)) 

  #############################################################
  ####################QUADROGRAM################################
  
  #installing visualization packages
  
  library(igraph)
  library(ggraph)
  
  rocket_quadrogram <- rocket1 %>%
    unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
    separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>%
    filter(!word3 %in% stop_words$word) %>%
    filter(!word4 %in% stop_words$word) 
  
  View(rocket_quadrogram)
  
  rocket_qcounts <- rocket_quadrogram %>%
    count(word1, word2,word3,word4, sort = TRUE) %>% 
    filter(n>2 & n<7)
  
  View(rocket_qcounts)
  
  # graph of quadrograms
  ggraph(rocket_qcounts, layout = "fr") +
    geom_edge_link()+
    geom_node_point()+
    geom_node_text(aes(label=name), vjust =1, hjust=1)
  
  
  #############################################################
  #################### BIGRAM ################################
  
  
  rocket_bigram <- rocket1 %>%
    unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
    separate(bigram, c("word1", "word2"), sep=" ") %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) 
  
  rocket_bcounts <- rocket_bigram %>%
    count(word1, word2, sort = TRUE) %>% 
    filter(n>69)

   
  # graph of bigram
  ggraph(rocket_bcounts, layout = "fr") +
    geom_edge_link()+
    geom_node_point()+
    geom_node_text(aes(label=name), vjust =1, hjust=1)
  
  ################### Improved Bigram  ###############################
  ####################################################################
  library(igraph)
  rbigram_graph <- rocket_bcounts %>%
    graph_from_data_frame()
  
  #creating arrows and changing colors.
  arrow <- grid::arrow(type = "open", length = unit(.15, "inches"))
  
  ggraph(rbigram_graph, layout = "fr") + 
    geom_edge_link(aes(alpha = n), show.legend = F, 
                   arrow = arrow, end_cap = circle(0.07, "inches")) + 
    geom_node_point(color = "lightblue", size = 5) + 
    geom_node_text(aes(label = name), vjust = 0.8, hjust = 0.8)
  
  
  ###################tf-idf ################################
  
  bigram_idf <- rocket_bigram %>%
    unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section
  
  bigram_tf_idf <- bigram_idf%>%
    count(rating, bigram) %>%
    bind_tf_idf(bigram, rating, n) %>%
    arrange(desc(tf_idf))
  
  View(bigram_tf_idf)

  
  #####################################################
  ### Running LDA per document
  ######################################################
  #removing other stopwords
  custom_stopwords <- data_frame(word=c("https", "t.co","rocketbook"),
                                 lexicon=rep("cust", each=3))
  tidy_rocket <- rocket1 %>%
    group_by(rating) %>% 
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    anti_join(custom_stopwords) %>% 
    count(word)%>%
    cast_dtm(rating,word, n)
  
  library(topicmodels)
  
  lda <- LDA(tidy_rocket,k=2, control = list(seed=123))
  
  gamma <- tidy(lda, matrix="gamma")
  
  ##########################################################
  ### Running LDA per token
  ##########################################################
  library(tidytext)
  rocket_topics <- tidy(lda, matrix="beta")
  
  print(eco_topics)
  
  library(ggplot2)
  library(dplyr)
  
  
  top_terms <- rocket_topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  top_terms
  
  #lets plot the term frequencies by topic
  top_terms %>%
    mutate(term=reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend=FALSE) +
    facet_wrap(~topic, scales = "free") +
    coord_flip()  
  
  
  # nothing insightful
  
  ############################################
  #### Sentiment Analysis 
  #############################################
  
  afin<- get_sentiments("afin")
  bing <- get_sentiments("bing")
  nrc <- get_sentiments("nrc")
  
  #bing
  rocket_bing <-rocket %>% 
    inner_join(get_sentiments("bing")) %>% 
    count(word,sentiment,sort=TRUE) %>% 
    ungroup()
  
  
  rocket_bing_count<- rocket_bing %>% 
    group_by(sentiment) %>% 
    count(sentiment,sort=TRUE)
  
    rocket_bing %>% 
    ggplot(aes(x = reorder(sentiment, n),y = n,fill=sentiment)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    labs(x = NA,
         y = "",
         title = "Number of words by sentiment")+
    theme(axis.text = element_text(family="AppleGothic"))+
    theme(axis.title.y = element_blank())+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(axis.text = element_text(size = 12))+
    theme(plot.title = element_text(size = 13)) 
  
  #nrc
  rocket_nrc <-rocket %>% 
    inner_join(get_sentiments("nrc")) %>% 
    count(word,sentiment,sort=TRUE) %>% 
    ungroup()
  
  rocket_nrc_count<- rocket_nrc %>% 
    group_by(sentiment) %>% 
    count(sentiment,sort=TRUE)
  
  sentiment_plot <- rocket_nrc_count %>% 
    ggplot(aes(x = reorder(sentiment, n),y = n,fill=sentiment)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    labs(x = NA,
         y = "",
         title = "Number of words by sentiment")+
    theme(axis.text = element_text(family="AppleGothic"))+
    theme(axis.title.y = element_blank())+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(axis.text = element_text(size = 12))+
    theme(plot.title = element_text(size = 13)) 

############### EMOJIS ##################
  #Most used emojis with eco-friednly
  #install.packages("emo")
  
  library(emoji)
  
  count_emojis<- rocket1 %>%
    mutate(emoji = emoji_detect(text)) %>%
    unnest(cols = c(emoji)) %>%
    count(emoji, sort = TRUE) 
  #693 emojis recognized. Now let's check which ones are at the top.
  
  rocket_emojis<- rocket1 %>%
    mutate(emoji = emoji_extract_all(text)) %>%
    unnest(cols = c(emoji)) %>%
    count(emoji, sort = TRUE) %>%
    top_n(10)
 
  #nothing insightful on emojis.
  
  ######## NEW DATAFRAME #####################################
 ############################################################## 
  ### Customer with  3 or less ratings.#######################
  bad_reviews <- rocket1%>% 
    filter(rocket1$rating <= 3)
  
  
  # creating a dataframe with tokens of bad reviews
  
  bad_reviews1<- rocket1%>% 
    unnest_tokens(word,text) %>% 
    count(word,sort=TRUE)
  
  #############################################################
  #################### BIGRAM ################################
  

  bad_bigram <- bad_reviews %>%
    unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
    separate(bigram, c("word1", "word2"), sep=" ") %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) 
  
  bad_bcounts <- bad_bigram  %>%
    count(word1, word2, sort = TRUE) %>% 
    filter(n>3)
  
  # graph of bigram
  ggraph(bad_bcounts, layout = "fr") +
    geom_edge_link()+
    geom_node_point()+
    geom_node_text(aes(label=name), vjust =1, hjust=1)
  
  trigram <- bad_reviews %>%
    unnest_tokens(trigram, text, token = "ngrams", n=3) %>%
    separate(trigram, c("word1", "word2","word3"), sep=" ") %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>%
    filter(!word3 %in% stop_words$word)
  
  ################### another bigram format ###########################
  ####################################################################
  library(igraph)
  bbigram_graph <- bad_bcounts %>%
    graph_from_data_frame()
  
  #creating arrows and changing colors.
  arrow <- grid::arrow(type = "open", length = unit(.15, "inches"))
  
  ggraph(bbigram_graph, layout = "fr") + 
    geom_edge_link(aes(alpha = n), show.legend = F, 
                   arrow = arrow, end_cap = circle(0.07, "inches")) + 
    geom_node_point(color = "lightblue", size = 5) + 
    geom_node_text(aes(label = name), vjust = 0.8, hjust = 0.8)

  
  #filtering trigram
  strigram <- trigram  %>%
    count(word1, word2,word3, sort = TRUE) %>% 
    filter(n>1)
  
  # graph of trigram
  ggraph(strigram, layout = "fr") +
    geom_edge_link()+
    geom_node_point()+
    geom_node_text(aes(label=name), vjust =1, hjust=1)
  
  
  
  
  