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

# define function for drawing random sample from the newsletters
# and linking to bioguide information
create_sample_function <- function(size, filename){
  # @param size: size of sample to be drawn
  # @param filename: filepath in which to save csv of sample 
  

  
  #take sample 
  samp <- news %>% sample_n(500)
  
  samp$doc_id <- seq(1, nrow(samp), 1)
  quanteda_options(threads = 6)
  
  bios <- read_csv("data/legislator_table.csv") %>% 
    select(-session)
  
  bios_unique <- bios %>% 
    group_by(id.bioguide) %>% 
    mutate(id_num = seq(1, n(), 1)) %>% 
    filter(id_num == 1) 
  
  # join news and bioguides, by id 
  samp <- left_join(samp, bios_unique, 
                    by = c("bioguide_id" = "id.bioguide")) %>% 
    left_join(as_tibble(cbind(state.name, state.abb)), 
              by = c("state" = "state.abb"))
  
  
  #Clean up the corpus a little bit - 
  #remove addresses 
  
  #create vector of zip codes
  zips <- as.character(seq(1, 99999, 1)) 
  
  
  corpus_orig <- corpus(samp, text_field = "Message",
                        docnames = news$doc_id)
  corpus_orig <- corpus_orig %>% 
    mutate(number_tokens = ntoken(corpus_orig, remove_punct = TRUE),
           date = samp$Date, 
           bioguide_id = samp$bioguide_id, 
           state = samp$state.name) 
  
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
  
  write.csv(corpus_df, filename)

}

create_sample_function(size = 500, filename = "data/newletter_samples/coding_sample1.csv")
create_sample_function(size = 500, filename = "data/newletter_samples/coding_sample2.csv")
create_sample_function(size = 500, filename = "data/newletter_samples/coding_sample3.csv")

create_sample_function(size = 500, filename = "data/newsletter_samples/coding_sample_joint.csv")

create_sample_function(size = 200, 
                       filename = "data/newsletter_samples/coding_sample_may2022.csv")
