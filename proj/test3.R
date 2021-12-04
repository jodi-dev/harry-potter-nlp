library(tidyverse)
library(tidytext)
library(harrypotter)
library(textnets)
library(stringi)

set.seed(1234)

# names of each book
hp_books <- c(
  "philosophers_stone"
  #"chamber_of_secrets",
  #"prisoner_of_azkaban", 
  #"goblet_of_fire",
  #"order_of_the_phoenix", 
  #"half_blood_prince",
  #"deathly_hallows"
)

# combine books into a list
hp_words <- list(
  philosophers_stone
  #chamber_of_secrets
  #prisoner_of_azkaban,
  #goblet_of_fire,
  #order_of_the_phoenix,
  #half_blood_prince,
  #deathly_hallows
) %>%
  # name each list element
  set_names(hp_books) %>%
  # convert each book to a data frame and merge into a single data frame
  map_df(as_tibble, .id = "book") %>%
  # convert book to a factor
  mutate(book = factor(book, levels = hp_books)) %>%
  # remove empty chapters
  drop_na(value) %>%
  # create a chapter id column
  group_by(book) %>%
  mutate(chapter = row_number(book)) %>%
  ungroup()

lst <- hp_words %>%
  filter(chapter == 17)

#' https://www.sparknotes.com/lit/harrypotter/characters/
characters <- c(
  "Harry",
  "Hermione",
  "Ron",
  "Hagrid",
  "Dumbledore",
  "Voldemort",
  "Malfoy",
  "Neville",
  "Mcgonagall",
  "Snape",
  "Quirrell",
  "Ginny"
)

#' @references:
#' https://stackoverflow.com/questions/29508943/r-regular-expression-isolate-a-string-between-quotes
hp_quotes <- stri_extract_all_regex(lst$value, '.{15}"[^"]*".{20}')[[1]]

#' Regex to match name in some number of words window after \"

sample_lst <- c(
  # make a sample and text regex, then apply to whole list
  # in the end add a new column to the quotes with plausible char name, if nothing
  # then NA
  hp_quotes[4],
  hp_quotes[5],
  hp_quotes[61],
  hp_quotes[113],
  hp_quotes[96],
  hp_quotes[133],
  hp_quotes[135],
  hp_quotes[137]
)
sample_lst <- as.data.frame(sample_lst)
names(sample_lst)[1] <- "quotes"

#' @references: 
#' https://stackoverflow.com/questions/60942627/exact-match-from-list-of-words-from-a-text-in-r
# pattern_words  <- paste0('\\b', characters, '\\b', collapse = "|")
# sample_lst$result <- c('Not Found', 'Found')[str_detect(sample_lst, pattern_words) + 1]

sample_lst %>% 
  rowwise() %>%
  mutate(animals = paste(list_of_words[unlist(
    lapply(list_of_words, function(x) grepl(x, text, ignore.case = T)))], collapse=",")) %>%
  data.frame()

list_of_words <- c(
  "Harry",
  "Hermione",
  "Ron",
  "Hagrid",
  "Dumbledore",
  "Voldemort",
  "Malfoy",
  "Neville",
  "Mcgonagall",
  "Snape",
  "Quirrell",
  "Ginny"
)

df <- tibble::tibble(#page=c(12,6,9,18,2,15,81,65),
                     text=c(hp_quotes[4],
                            hp_quotes[5],
                            hp_quotes[61],
                            hp_quotes[113],
                            hp_quotes[96],
                            hp_quotes[133],
                            hp_quotes[135],
                            hp_quotes[137]))

characters <- c("dog,hen", "lion,tiger", "horse", FALSE, "dog", "tiger", "lion", FALSE)

df %>% 
  rowwise() %>%
  mutate(characters = paste(list_of_words[unlist(
    lapply(list_of_words, function(x) grepl(x, text, ignore.case = T)))], collapse=",")) %>%
  data.frame()

#####

prepped_hp <- PrepText(lst, groupvar = "book", textvar = "value", node_type = "groups", tokenizer = "words", pos = "nouns", remove_stop_words = TRUE, compound_nouns = TRUE)

hp_text_network <- CreateTextnet(prepped_hp)

# VisTextNet(hp_text_network, label_degree_cut = 0)
