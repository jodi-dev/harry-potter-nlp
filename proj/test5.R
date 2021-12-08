# names of each book
hp_books <- c(
  #"philosophers_stone",
  #"chamber_of_secrets"
  # "prisoner_of_azkaban", 
  # "goblet_of_fire",
  # "order_of_the_phoenix", 
  # "half_blood_prince",
  "deathly_hallows"
)

# combine books into a list
hp_words <- list(
  #philosophers_stone,
  #chamber_of_secrets
  # prisoner_of_azkaban,
  # goblet_of_fire,
  # order_of_the_phoenix,
  # half_blood_prince,
  deathly_hallows
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
  filter(chapter == 31)

#' @references:
#' https://stackoverflow.com/questions/29508943/r-regular-expression-isolate-a-string-between-quotes
hp_quotes <- stri_extract_all_regex(lst$value, '.{15}"[^"]*".{20}')[[1]]

#' Regex to match name in some number of words window after \"


# sample_lst <- c(
#   # make a sample and text regex, then apply to whole list
#   # in the end add a new column to the quotes with plausible char name, if nothing
#   # then NA
#   hp_quotes[5],
#   hp_quotes[8],
#   hp_quotes[66],
#   hp_quotes[69],
#   hp_quotes[72],
#   hp_quotes[75],
#   hp_quotes[79],
#   hp_quotes[80],
#   hp_quotes[82],
#   hp_quotes[89],
#   hp_quotes[90],
#   hp_quotes[91],
#   hp_quotes[95],
#   hp_quotes[96],
#   hp_quotes[98],
#   hp_quotes[101],
#   hp_quotes[102],
#   hp_quotes[105],
#   hp_quotes[118],
#   hp_quotes[124],
#   hp_quotes[132]
# )
# sample_lst <- as_tibble(sample_lst)
# names(sample_lst)[1] <- "quotes"

#' @references: 
#' https://stackoverflow.com/questions/60942627/exact-match-from-list-of-words-from-a-text-in-r
# pattern_words  <- paste0('\\b', characters, '\\b', collapse = "|")
# sample_lst$result <- c('Not Found', 'Found')[str_detect(sample_lst, pattern_words) + 1]

# sample_lst %>% 
#   rowwise() %>%
#   mutate(animals = paste(list_of_words[unlist(
#     lapply(list_of_words, function(x) grepl(x, text, ignore.case = T)))], collapse=",")) %>%
#   data.frame()

list_of_words <- c(
  "Harry",
  "Hermione",
  "Ron",
  "Hagrid",
  "Neville",
  "Mcgonagall",
  "Ginny",
  "Tonks"
)

df <- tibble::tibble(#page=c(12,6,9,18,2,15,81,65),
  text=c(hp_quotes[5],
         hp_quotes[8],
         hp_quotes[66],
         hp_quotes[69],
         hp_quotes[72],
         hp_quotes[75],
         hp_quotes[79],
         hp_quotes[80],
         hp_quotes[82],
         hp_quotes[89],
         hp_quotes[90],
         hp_quotes[91],
         hp_quotes[95],
         hp_quotes[96],
         hp_quotes[98],
         hp_quotes[101],
         hp_quotes[102],
         hp_quotes[105],
         hp_quotes[118],
         hp_quotes[124],
         hp_quotes[132]))

characters <- vector(mode="character", length=21)

df2 <- df %>% 
  rowwise() %>%
  mutate(characters = paste(list_of_words[unlist(
    lapply(list_of_words, function(x) grepl(x, text, ignore.case = T)))], collapse=",")) %>%
  data.frame()

df2 <- lapply(df2, gsub, pattern="NA,", replacement="")

df2 <- as.data.frame(df2)

df2 <- df2 %>%
  filter(characters != "")
  #filter(!str_detect(characters, ",")) # only single chars


#####

library(textnets)

prepped_hp <- PrepText(df2, groupvar = "characters", textvar = "text", node_type = "groups", tokenizer = "words", pos = "nouns", remove_stop_words = TRUE, compound_nouns = TRUE)

hp_text_network <- CreateTextnet(prepped_hp)

VisTextNet(hp_text_network, label_degree_cut = 0)

