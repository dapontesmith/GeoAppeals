# Goal of this file - 
# To do analyses that show patterns of place-name use, beyond mere counts 

# contains functions that count place-name mentions
# and get sentiment scores for every newsletter in the corpus 

#load relevant libraries 
library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(quanteda.sentiment)
library(quanteda.tidy)
library(tidytext)
library(stats)
library(BSDA)

setwd("C:/Users/dapon/Dropbox/Harvard/GeoAppeals")
#setwd("/Users/nod086/Desktop/GeoAppeals/GeoAppeals")

# set up the corpus 
news <- read_csv("data/all_newsletters.csv")

#take sample 
news <- news %>% sample_n(size = nrow(news))

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
                 docnames = news$doc_id)
corpus_orig <- corpus_orig %>% 
  mutate(number_tokens = ntoken(corpus_orig, remove_punct = TRUE),
         date = news$Date, 
         bioguide_id = news$bioguide_id, 
         state = news$state.name) 

# convert to sentences, for more precise filtering
corp_sentences <- corpus_reshape(corpus_orig, to = "sentences")

# filter out sentence-addresses
corp_sentences <- corp_sentences %>% 
  corpus_subset(str_detect(., str_c("\\b(", str_c(zips, collapse = "|"), ")\\b")) == FALSE,
                str_detect(., "Office Building") == FALSE,
                str_detect(., "WASHINGTON") == FALSE,
                str_detect(., "Contact Information") == FALSE, 
                str_detect(., "Washington Journal") == FALSE,
                str_detect(., "Washington Times") == FALSE,
                str_detect(., "Washington, D.C. Office") == FALSE, 
                str_detect(., "Washington, DC Office") == FALSE,
                str_detect(., "Office Locations Washington D.C.") == FALSE,
                str_detect(., "Office Locations Washington, D.C.") == FALSE)

# save sentence corpus and document variables as csv
sentences_df <- as_tibble(cbind(
  as_tibble(corp_sentences),
  docvars(corp_sentences)
)) %>% 
  rename(text = value)

write.csv(sentences_df, "data/sentences_newsletters_clean.csv")
                

corpus <- corpus_reshape(corp_sentences, to = "documents")
corpus_df <- as_tibble(cbind(
  as_tibble(corpus),
  docvars(corpus)
)) %>% 
  rename(text = value)

write.csv(corpus_df, "data/newsletters_clean.csv")


############################################################
# FUNCTIONS 
############################################################

# function to analyze the sentiments of documents
get_sentiments <- function(corpus,                        
                           text_field,
                                      placenames,
                                      min_docfreq = 50, 
                                      min_termfreq = 50,
                                      dictionary){
  # SENTIMENT ANALYSIS - tokenize and apply dictionary
  sent_dict <- corpus %>% 
    tokens() %>% 
    tokens_lookup(dictionary = dictionary)
  
  # transform to a dfm and convert to dataframe
  dfmat_dict <- dfm(sent_dict)
  dict_output <- convert(dfmat_dict, to = "data.frame")
  
  # create sentiment score (this is from benoit slides)
  
  dict_output$sentscore <- log((dict_output$positive + 
                                  dict_output$neg_negative + 0.5) / 
                                 (dict_output$negative + 
                                    dict_output$neg_positive + 0.5))
  dict_output <- cbind(dict_output, docvars(corpus)) 
  return(dict_output)
}

# define function for getting place-name mentions from documens
get_mentions <- function(corpus, text_field, placenames,
                         min_docfreq = 50, min_termfreq = 50){
  # define dictionary 
  places <- as.list(placenames)
  names(places) <- placenames
  places_dict <- dictionary(places)
  
  # make into a tokens object
  tokens <- tokens(corpus, remove_punct = TRUE,
                   remove_numbers = FALSE,
                   remove_url = TRUE,
                   remove_symbols = FALSE)
  

   #remove stopwords via tokens_select
   toks_no_stop <- tokens_select(tokens,
                                 pattern = stopwords(language="en",source="marimo"),
                                 selection = 'remove')
   
   # get word count per document
   dfm <- dfm(toks_no_stop)
   # trim the dfm so as to reduce computational needs
   dfmtrimmed <- dfm_trim(dfm, min_docfreq = 50,
                          min_termfreq = 50, verbost = TRUE)
   # convert to a dataframe
   dfm <- convert(dfmtrimmed, to = "data.frame") %>% 
     as_tibble() %>% 
     select(-doc_id)
   
   # get number of mentions of places per document in the corpus
   dfm_dict_toks <- dfm(tokens_lookup(
     toks_no_stop, places_dict
   ))
   
   # turn dfm into tibble, then do rowsums 
   dict_tibble <- dfm_dict_toks %>% as_tibble() %>%
     select(-doc_id)
   
   # assign variables- this syntax relies on quanteda.tidy
   corpus <- corpus %>%
     mutate(total_words = rowSums(dfm),
            place_mentions = rowSums(dict_tibble),
            place_mentions_prop = place_mentions / total_words)
   
   out <- as_tibble(cbind(corpus, docvars(corpus))) %>% 
     mutate(text = as.character(corpus)) %>% 
     select(-corpus) %>% 
     select(Date, Subject, text, everything()) %>% 
     # make binary variable for whether place is mentioned at all 
     mutate(mentions_binary = ifelse(place_mentions > 0, 1, 0))
   
   return(out)
  
}

# run the functions separately, then put back together 
mention_out <- get_mentions(corpus = corpus, 
                            text_field = "Message",
                            placenames = "Washington",
                            min_docfreq = 200, min_termfreq = 200)

sent_out <- get_sentiments(corpus = corpus,
                          text_field = "Message",
                          min_docfreq = 200, min_termfreq = 200,
                          dictionary = data_dictionary_LSD2015) 

# join together 
out <- left_join(mention_out, sent_out, by = c(
  "Date","Subject","bioguide_id", "name.first","name.last",
  "name.official_full", "type","state","district","party",
  "sex","Race","id_num", "state.name"
)) %>% 
  select(-starts_with("number")) 

# make binary variable for whether place is mentioned at all 


# make plot 
out %>% 
  filter(total_words > 500, 
         party %in% c("Republican","Democrat"),
         place_mentions_prop > 0.0001) %>% 
  ggplot(aes(y = sentscore, x = log(place_mentions_prop), color = party)) + 
  geom_point() + 
  geom_smooth() + 
  theme_minimal() + 
  labs(x = "Washington mentions proportion (log)",
       y = "Sentiment",
       title = "Washington mentions vs. sentiment, by party",
       subtitle = "Observations at newsletter-level, 
       only newsletters with >500 words and non-zero placename mentions")


# perhaps try with sentences, rather than by document

# things to figure out: 
# 1 . how to do the analysis at the sentence-level, not the document level

# MTURK with clouresearch approved respondents 




# save csv of top 500 newsletters by within-state mentions 

newsletters_state <- readRDS("data/newsletters_with_state_mentions.Rds")

top500states <- newsletters_state %>% 
  arrange(desc(place_mentions_prop)) %>% 
  select(text, state, district, name.official_full, party,
         everything()) %>% 
  head(n = 500)

write.csv(top500states, "data/top500_withinstate_mentions.csv")

# save csv of top 500 newsletters by washington count (from the sample)

top500washington <- mention_out %>% 
  arrange(desc(place_mentions_prop)) %>% 
  filter(state != "Washington") %>% 
  select(text, state, district, name.official_full, party, place_mentions_prop, 
         everything()) %>% 
  head(n = 500)

write.csv(top500washington, "data/top500_washington_sample.csv")













