library(tidytext)
library(harrypotter)
library(stringr)
library(plyr)
library(widyr)
library(tidyverse)
library(irlba)
library(broom)

# OPTIONS
options(stringsAsFactors = F, # do not convert upon loading
        scipen = 999, # do not convert numbers to e-values
        max.print = 200) # stop printing after 200 values

# LOAD IN BOOKS
corpus <- list(
  philosophers_stone = philosophers_stone
) %>%  
  ldply(rbind) %>% # bind all chapter text to dataframe columns
  mutate(book = factor(seq_along(.id), labels = .id)) %>% # identify associated book
  select(-.id) %>% # remove ID column
  gather(key = 'chapter', value = 'text', -book) %>% # gather chapter columns to rows
  filter(!is.na(text)) %>% # delete the rows/chapters without text
  mutate(chapter = as.integer(chapter)) # chapter id to numeric

# DROP BOOK and put chapter on the right hand side
philo <- corpus[,!(names(corpus) == "book")]

# We want to use original tweets, not retweets:
# elected_no_retweets <- elected_official_tweets %>%
#   filter(is_retweet == F) %>%
#   select(c("text"))

#create tweet id
#elected_no_retweets$postID<-row.names(elected_no_retweets)

#create context window with length n
tidy_skipgrams <- philo %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 30) %>%
  dplyr::mutate(ngramID = row_number()) %>%
  tidyr::unite(skipgramID, chapter, ngramID) %>%
  unnest_tokens(word, ngram)

#calculate unigram probabilities (used to normalize skipgram probabilities later)
unigram_probs <- philo %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  mutate(p = n / sum(n))

#calculate probabilities
skipgram_probs <- tidy_skipgrams %>%
  pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
  mutate(p = n / sum(n))

#normalize probabilities
normalized_prob <- skipgram_probs %>%
  filter(n > 20) %>%
  rename(word1 = item1, word2 = item2) %>%
  left_join(unigram_probs %>%
              select(word1 = word, p1 = p),
            by = "word1") %>%
  left_join(unigram_probs %>%
              select(word2 = word, p2 = p),
            by = "word2") %>%
  mutate(p_together = p / p1 / p2)

normalized_prob %>% 
  filter(word1 == "harry") %>%
  arrange(-p_together)

pmi_matrix <- normalized_prob %>%
  mutate(pmi = log10(p_together)) %>%
  cast_sparse(word1, word2, pmi)

#remove missing data
pmi_matrix@x[is.na(pmi_matrix@x)] <- 0
#run SVD
pmi_svd <- irlba(pmi_matrix, 256, maxit = 500)
#next we output the word vectors:
word_vectors <- pmi_svd$u
rownames <- rownames(pmi_matrix)
rownames(word_vectors) <- what

#' Julia Silge Function
#' search_synonyms <- function(word_vectors, #' selected_vector) {
#'   
#'   similarities <- word_vectors %*% selected_vector %>%
#'     tidy() %>%
#'     as_tibble() %>%
#'     plyr::rename(token = .rownames,
#'            similarity = unrowname.x.)
#'   
#'   similarities %>%
#'     arrange(-similarity)    
#' }

#' @comment: code from JS renames column called 'rownames' as token
#' but i miss the part where i'm supposed to make a rowname column
#' what is function *rename* and what is token needed for?
#' also what is *unrowname.x.*

selected_vector <- word_vectors["hermione",]

similarities <- word_vectors %*% selected_vector 

rownames_only_or_someting <- similarities[,-1] #tokens
b <- similarities[,1]
pres_synonym <- data.frame(b)
colnames(pres_synonym)[1] <- "similarity"

head(pres_synonym %>% arrange(-similarity))









