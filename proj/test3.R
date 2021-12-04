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
  "harry",
  "hermione",
  "ron",
  "hagrid",
  "dumbledore",
  "voldemort",
  "malfoy",
  "neville",
  "mcgonagall",
  "snape",
  "quirrell"
)

#' @references:
#' https://stackoverflow.com/questions/29508943/r-regular-expression-isolate-a-string-between-quotes
hp_quotes <- stri_extract_all_regex(lst$value, '.{15}"[^"]*".{15}')[[1]]

#' Regex to match name in some number of words window after \"

sample_lst <- list(
  # make a sample and text regex, then apply to whole list
  # in the end add a new column to the quotes with plausible char name, if nothing
  # then NA
  hp_quotes[14]
)

#####

prepped_hp <- PrepText(lst, groupvar = "book", textvar = "value", node_type = "groups", tokenizer = "words", pos = "nouns", remove_stop_words = TRUE, compound_nouns = TRUE)

hp_text_network <- CreateTextnet(prepped_hp)

# VisTextNet(hp_text_network, label_degree_cut = 0)
