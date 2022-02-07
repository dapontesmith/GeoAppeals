# SCRIPT FOR CALCULATING HOW OFTEN MPS MENTION THEIR OWN STATE 
# MUCH HERE IS QUITE SIMILAR TO THE COUNT_PLACES SCRIPT
library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(quanteda.sentiment)
library(tidytext)
library(BSDA)
library(stats)
library(sf)

setwd("C:/Users/dapon/Dropbox/Harvard/GeoAppeals")
#setwd("C:/Users/nod086/Downloads/GeoAppeals")
news <- read_csv("data/all_newsletters.csv")

#take sample 
news <- news %>% sample_n(size = nrow(news)/10)

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

corpus <- corpus(news, text_field = "Message",
                 docnames = news$doc_id)
corp_sentences <- corpus_reshape(corpus, to = "sentences")
docvars(corpus, "number_tokens") <- ntoken(corpus, remove_punct = TRUE)
docvars(corpus, "date") <- news$Date
docvars(corpus, "bioguide_id") <- news$bioguide_id
docvars(corpus, "state") <- news$state.name

#create vector of zip codes
zips <- as.character(seq(1, 99999, 1))

#try to filter out building addresses
corp_sentences <- corp_sentences %>% 
  #this filters out zip codes 
  corpus_subset(str_detect(., str_c("\\b(", str_c(zips, collapse = "|"), ")\\b")) == FALSE,
                str_detect(., "Office Building") == FALSE) # exclude all-caps since it's mostly datelines 

corpus <- corpus_reshape(corp_sentences, to = "documents")

# DEFINE FUNCTION FOR REPORTING NUMBER OF MENTIONS OF ONE'S OWN STATE
#element 2 of return list is dfm of place-mentions across all docs in corpus
count_mentions_in_state <- function(corpus, text_field, placenames,
                           min_docfreq = 50, min_termfreq = 50){
  
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

#function to run the above function, on a subsetted corpus
#returns dataframe of message, bioguide information, and place-counts and proportions

run_count_mentions_in_state <- function(corpus, state_name){
  corpus_state <- corpus %>% corpus_subset(state.name == state_name)
  out <- count_mentions_in_state(corpus = corpus_state,
                                 text_field = "Message",
                                 placenames = state_name )
  full <- cbind(as_tibble(out[[1]]), as_tibble(docvars(out[[1]])))
  return(full)
}


#run over all states 
full <- run_count_mentions_in_state(corpus = corpus, 
                                     state_name = state.name[1])
for(i in 2:length(state.name)){
  print(i)
  out <- run_count_mentions_in_state(corpus = corpus, 
                                     state_name = state.name[i]) 
  full <- rbind(full, out)
}

#save the resulting dataframe to workspace 
save(fill, file="data/newsletters_with_state_mentions.csv")

#do some mutation of variables 
test <- full %>% 
  rename(text = value) %>% 
 # mutate(total_words = as.numeric(total_words),
  #       place_mentions = as.numeric(place_mentions),
   #      place_mentions_prop = as.numeric(place_mentions_prop)) %>% 
  #filter(total_words > 500) %>% 
  select(-id_num, -text, -Subject) %>% 
  arrange(-place_mentions_prop)



test <- test %>% filter(total_words > 100) %>% 
  mutate(party_type = paste(party, type, sep = "-"))

# create summarized df of mentions by MP
mentions_by_mp <- test %>% 
  group_by(name.official_full, state, state.name, district, type, party, sex, Race) %>% 
  summarize(total_mentions = sum(place_mentions, na.rm = TRUE),
            total_words = sum(total_words, na.rm = TRUE), 
            prop_mentions = total_mentions / total_words) %>%
  arrange(-prop_mentions)

# create summarized df of mentions by state
mentions_by_state_party <- mentions_by_mp %>% 
  group_by(state, state.name, party ) %>% 
  summarize(total_mentions = sum(total_mentions, na.rm = TRUE),
            total_words = sum(total_words, na.rm = TRUE), 
            prop_mentions = total_mentions / total_words) %>%
  arrange(-prop_mentions)

# create a few density plots 
doc_newsletter_density_by_party <- ggplot(test) + 
  geom_density(aes(x = place_mentions_prop, color = party)) + 
  theme_minimal() + 
  labs(title = "Density plot of one's own state-name mentions",
      x = "% of words that are one's own state", 
      y = "Density")

doc_density_by_type <- ggplot(test) + 
  geom_density(aes(x = place_mentions_prop, color = party_type)) + 
  theme_minimal() + 
  labs(title = "Density plot of one's own state-name mentions",
       x = "% of words that are one's own state", 
       y = "Density")

mp_density_by_party <- mentions_by_mp %>% 
  ggplot() + 
  geom_density(aes(x = prop_mentions, color = party)) + 
  theme_minimal() + 
  labs(title = "Density plot of one's own state-name mentions, by MP",
       x = "% of words that are one's own state",
       y = "Density")

# prop_mentions by state and party 

ggplot(mentions_by_state_party) + 
  geom_point(aes(x = reorder(state, -prop_mentions),
               y = prop_mentions, color = party)) + 
  theme_minimal() + 
  labs(x = "State",
       y = "% total words own-state mentions", 
       title = "% total words own-state mentions, by state and party")



# read in shapefile of us states
shp <- st_read("data/shapefiles/cb_2018_us_state_500k.shp")


shp %>% 
  filter(NAME %in% state.name, NAME != "Hawaii", NAME!="Alaska") %>% 
  ggplot() + 
  geom_sf()

