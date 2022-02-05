library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(quanteda.sentiment)
library(tidytext)
library(BSDA)

setwd("C:/Users/dapon/Dropbox/Harvard/GeoAppeals")
#setwd("C:/Users/nod086/Downloads/GeoAppeals")
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


#Clean up the corpus a little bit - 
#remove addresses 

corpus <- corpus(news, text_field = "Message",
                 docnames = news$doc_id)
corp_sentences <- corpus_reshape(corpus, to = "sentences")
docvars(corpus, "number_tokens") <- ntoken(corpus, remove_punct = TRUE)
docvars(corpus, "date") <- news$Date
docvars(corpus, "bioguide_id") <- news$bioguide_id

#create vector of zip codes
zips <- as.character(seq(1, 99999, 1))

#try to filter out building addresses
corp_sentences <- corp_sentences %>% 
  #this filters out zip codes 
  corpus_subset(str_detect(., str_c("\\b(", str_c(zips, collapse = "|"), ")\\b")) == FALSE,
                str_detect(., "Office Building") == FALSE,
                str_detect(., "WASHINGTON") == FALSE) # exclude all-caps since it's mostly datelines 

corpus <- corpus_reshape(corp_sentences, to = "documents")

#function for counting the number of mentions of places in a dataframe
#returns the original df, plus columns of # of place-metions and # of words 
#element 2 of return list is dfm of place-mentions across all docs in corpus
count_mentions <- function(corpus, text_field, placenames,
                           min_docfreq = 150, min_termfreq = 150){

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
  total_words <- rowSums(dfm)
  docvars(corpus, "total_words") <- total_words
  
  #get number of mentions of places per document in the corpus
  dfm_dict_toks <- dfm(tokens_lookup(toks_no_stop,
                                     places_dict))
  #turn dfm into tibble, then do rowsums
  dict_tibble <- dfm_dict_toks %>%
    as_tibble() %>% 
    dplyr::select(-doc_id)
  
  place_mentions <- rowSums(dict_tibble)
  docvars(corpus, "place_mentions") <- place_mentions
  
  #get number of place mentions as proportion of all words
  place_mentions_prop <- place_mentions/total_words
  docvars(corpus, "place_mentions_prop") <- place_mentions_prop
  
  
  return(list(corpus, dict_tibble))

}


#run function to count state-name mentions
mentions_df <- count_mentions(corpus = corpus, 
               text_field = 'Message',
               placenames = state.name)
states_df <- as_tibble(cbind(state.name, state.abb))

states_with_vars <- rbind(mentions_df[[1]], as_tibble(docvars(mentions_df[[1]]))) %>%
  left_join(., bios_unique, by = c("bioguide_id" = "id.bioguide"))


states_by_mp <- states_with_vars %>% 
  select(total_words:state, Race, party, name.official_full) %>% 
  mutate(total_words = as.numeric(total_words),
         place_mentions = as.numeric(place_mentions),
         place_mentions_prop = as.numeric(place_mentions_prop)) %>% 
  group_by(name.official_full, type, state, party, Race) %>% 
  summarize(total_words = sum(total_words, na.rm = TRUE), 
            state_mentions = sum(place_mentions, na.rm = TRUE)) %>% 
  mutate(state_mentions = state_mentions / total_words ) %>% 
  arrange(-state_mentions) %>% 
  #use only MPs with >500 words 
  filter(total_words > 500)


states_by_mp %>% 
  filter(party != "Independent") %>%
  mutate(Party = party) %>% 
  ggplot() + 
  geom_density(aes(x = state_mentions, color = Party)) + 
  theme_minimal() + 
  labs(x = "Proportion of all words that are state names",
       y = "Density",
       title = "State-name mentions, by party")

#get dataframe for counting how often "washington" is mentioned 
washington_df <- count_mentions(corpus = corpus, 
                                text_field = "Message",
                                placenames = c("washington", "Washington"))


#get the docvars, join with the bioguide df
wash_with_vars <- cbind(as_tibble(washington_df[[1]]), 
                        as_tibble(docvars(washington_df[[1]]))) %>% 
  left_join(., bios_unique, 
            by = c("bioguide_id" = "id.bioguide")) %>% 
  left_join(., states_df, by = c("state" = "state.abb")) %>% 
  mutate(state.name = tolower(state.name)) %>% 
  filter(state.name != "Washington") %>% 
  #get only newsletters with > 500 words
  filter(total_words > 500,
         state != "WA") %>%
  arrange(desc(place_mentions_prop)) %>% 
  as_tibble()


write.csv(wash_with_vars, "data/newsletters_with_washington_counts.csv")

mentions_by_mp <- wash_with_vars %>% 
  select(total_words:state, Race, party, name.official_full) %>% 
  mutate(total_words = as.numeric(total_words),
         place_mentions = as.numeric(place_mentions),
         place_mentions_prop = as.numeric(place_mentions_prop)) %>% 
  group_by(name.official_full, type, state, party, Race) %>% 
  summarize(total_words = sum(total_words, na.rm = TRUE), 
            wash_mentions = sum(place_mentions, na.rm = TRUE)) %>% 
  mutate(wash_mentions_prop = wash_mentions / total_words ) %>% 
  arrange(-wash_mentions_prop) %>% 
  #use people only with over 500 words
  filter(total_words > 500)

mentions_by_mp %>% 
  filter(party != "Independent") %>%
  mutate(Party = party) %>% 
  ggplot() + 
  geom_density(aes(x = wash_mentions_prop, color = Party)) + 
  theme_minimal() + 
  labs(x = "Proportion of all words that are Washington",
       y = "Density",
       title = "Washington mentions, by party")

#do z-test of the distributions of republicans and democrats 
gop <- mentions_by_mp %>% 
  filter(party == "Republican") %>% 
  pull(wash_mentions_prop)

dem <- mentions_by_mp %>% 
  filter(party == "Democrat") %>% 
  pull(wash_mentions_prop)

#republicans do mention washington more! 
z.test(gop, dem,
       sigma.x = sd(gop, na.rm = TRUE),
       sigma.y = sd(dem, na.rm = TRUE))

#make plot of % of total words that are washington, by state and party
washington_plot_by_state <- mentions_by_mp %>% 
   
  group_by(state, party) %>%
  summarize(mean_prop = mean(wash_mentions_prop, na.rm = TRUE)) %>% 
  pivot_wider( names_from = party, values_from = mean_prop) %>% 
  mutate(difference = Democrat-Republican) %>% 
  ggplot(aes(x = state, y = difference)) + 
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  annotate(geom = "text", x = 12, y = -0.008, label = "States where Republicans mention DC more") + 
  annotate(geom = "text", x = 12, y = 0.005, label = "States where Democrats mention DC more") + 
  theme_minimal() + 
  labs(x = "State", y = "Difference",
       title = "Difference between Dem and GOP propensity to mention Washington, as % of total words")
  
  
mentions_by_mp



ggplot() + 
  geom_point(aes(x = mean_prop, y = state, color = party))
  


#get count of how often each place is mentioned in the entire corpus
mentions_by_state <- colSums(mentions_df[[2]])
mentions_by_state <- cbind(names(mentions_by_state), 
                           mentions_by_state) %>% as_tibble() %>%
  rename(place = V1, 
         total_mentions_in_corpus = mentions_by_state) %>% 
  mutate(total_mentions_in_corpus = as.numeric(total_mentions_in_corpus)) %>% 
  arrange(desc(total_mentions_in_corpus))


ggplot(mentions_by_state) + 
  geom_col(aes(x = reorder(place, -total_mentions_in_corpus),
               y = total_mentions_in_corpus)) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1)) + 
  labs(x = "State", y = 'Number of mentions in all newsletters')





#read in placename data
pop_places <- read.table("data/placenames/POP_PLACES_20210825.txt", sep = "|",
                         fill = TRUE, header = TRUE) %>% as_tibble()

pop_places <- read.table("data/placenames/NHGIS_census_places_2019.txt", sep = "|",
                         fill = TRUE, header = TRUE) %>% as_tibble()

#create vector of place names
pop_places <- pop_places %>% 
  sample_n(size = nrow(pop_places)/20) %>% 
  pull(FEATURE_NAME) %>% as_tibble()

#run function on places vector 
mentions_df_places <- count_mentions(df = news, 
                              text_field = 'Message',
                              placenames = pop_places)






#get count of how often each place is mentioned in the entire corpus
total_mentions_by_place <- colSums(dict_tibble)
total_mentions_by_place <- cbind(names(total_mentions_by_place), 
      total_mentions_by_place) %>% as_tibble() %>%
  rename(place = V1, 
         total_mentions_in_corpus = total_mentions_by_place) %>% 
  arrange(desc(total_mentions_in_corpus))

hist(news$place_mentions_prop)


tail(corpus) %>% 
  textstat_polarity(dictionary = data_dictionary_geninqposneg)


# TO DO - 
# LOOK AT NEWSLETTERS BY DEMOCRATS AND REPUBLICANS THAT MENTION "WASHINGTON" - 
# PERHAPS TAKE THE ONES THAT MENTION WASHINGTON THE MOST, AND LOOK AT THOSE  


