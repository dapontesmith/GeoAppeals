# Goal of this file - 
# To do analyses that show patterns of place-name use, beyond mere counts 

#load relevant libraries 
library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(quanteda.sentiment)
library(quanteda.tidy)
library(tidytext)
library(BSDA)

setwd("C:/Users/dapon/Dropbox/Harvard/GeoAppeals")
#setwd("/Users/nod086/Desktop/GeoAppeals/GeoAppeals")

# set up the corpus 
news <- read_csv("data/all_newsletters.csv")

#take sample 
news <- news %>% sample_n(size = nrow(news)/20)

news$doc_id <- seq(1, nrow(news), 1)
quanteda_options(threads = 6)

bios <- read_csv("data/legislator_table.csv") %>% 
  select(-session)

bios_unique <- bios %>% 
  group_by(id.bioguide) %>% 
  mutate(id_num = seq(1, n(), 1)) %>% 
  filter(id_num == 1) 

# join news and bioguides, by id 
news <- left_join(news, bios_unique, 
                  by = c("bioguide_id" = "id.bioguide")) %>% 
  left_join(as_tibble(cbind(state.name, state.abb)), 
            by = c("state" = "state.abb"))


#Clean up the corpus a little bit - 
#remove addresses 

#create vector of zip codes
zips <- as.character(seq(1, 99999, 1)) 


corpus_orig <- corpus(news, text_field = "Message",
                 docnames = news$doc_id) %>%
  mutate(number_tokens = ntoken(corpus, remove_punct = TRUE),
         date = news$Date, 
         bioguide_id = news$bioguide_id, 
         state = news$state.name) 

# convert to sentences, for more precise filtering
corp_sentences <- corpus_reshape(corpus_orig, to = "sentences")
# filter out sentence-addresses
corp_sentences <- corp_sentences %>% 
  corpus_subset(str_detect(., str_c("\\b(", str_c(zips, collapse = "|"), ")\\b")) == FALSE,
                str_detect(., "Office Building") == FALSE) 

corpus <- corpus_reshape(corp_sentences, to = "documents")


############################################################
# DETECT WHETHER PLACE-NAME APPEARS IN DOCUMENT 
############################################################

placenames <- c("Washington","Washington, D.C.","washington")

#text_field = character, name of text column in df
#corpus = corpus (created above) 
#placenames = vector of placenames to c ount
places <- as.list(placenames)
names(places) <- placenames
places_dict <- dictionary(places)




#make into a tokens object
tokens <- tokens(corpus, remove_punct = TRUE,
                 remove_numbers = FALSE,
                 remove_url = TRUE, 
                 remove_symbols = FALSE)


#remove stopwords via tokens_select
toks_no_stop <- tokens_select(tokens,
                              pattern = stopwords(language="en",source="marimo"),
                              selection = 'remove')

#get word count per document
dfm <- dfm(tokens)
#trim the dfm so it doesn't break - these parameters may need tuning
dfmtrimmed <- dfm_trim(dfm, min_docfreq = min_docfreq,
                       min_termfreq = min_termfreq, verbose = TRUE)

dfm <- convert(dfmtrimmed, to = "data.frame") %>%
  as_tibble() %>%
  dplyr::select(-doc_id)

#get number of mentions of places per document in the corpus
dfm_dict_toks <- dfm(tokens_lookup(toks_no_stop,
                                   places_dict))
#turn dfm into tibble, then do rowsums
dict_tibble <- dfm_dict_toks %>%
  as_tibble() %>% 
  dplyr::select(-doc_id)

#assign variables 
corpus <- corpus %>% 
  mutate(total_words = rowSums(dfm), # total words variable
         place_mentions = rowSums(dict_tibble), # number of place mentions
         place_mentions_prop = place_mentions / total_words) # % of all words that are place mentiions



# SENTIEMENT ANALYSIS - tokenize and apply dictionary
sent_dict <- corpus %>%
  tokens() %>%
  tokens_lookup(dictionary = data_dictionary_LSD2015)
# transform to a dfm and convert to dataframe
dfmat_dict <- dfm(sent_dict) 
dict_output <- convert(dfmat_dict, to = "data.frame")

#create sentiment score (this is from benoit slides)
dict_output$sent_score <- log((dict_output$positive +
                                 dict_output$neg_negative + 0.5) /
                                (dict_output$negative +
                                   dict_output$neg_positive+ 0.5))
dict_output <- cbind(dict_output, docvars(corpus))
























