library(tidyverse)
library(tidymodels)
library(textrecipes)

setwd("C:/Users/dapon/Dropbox/Harvard")

# read in full dataframe 
df <- read.csv("GeoAppeals/data/sentence_level_newsletter_dataset_with_annotations.csv")

# note - 0 = policy, 1 = symbolic, 99 = NA 

annotated <- df %>% 
  as_tibble() %>% 
  filter(is_annotated == 1) %>% 
  select(sentence_w_mention, majority_annotation, 
         ends_with("annotation"),
         is_training, is_validation) %>% 
   filter(majority_annotation %in% c(0, 99)) %>% 
  mutate(is_policy = as.factor(ifelse(majority_annotation == 0, 1, 0))) %>% 
  select(sentence = sentence_w_mention, class = is_policy)

split <- initial_split(annotated, strata = class)

train_data <- training(split)
test_data <- testing(split)

sentence_rec <- recipe(
  class ~ sentence, data = train_data) %>% 
  step_tokenize(sentence) %>% 
  # step_tokenfilter(sentence, max_tokens = 1e3) %>%
  step_tfidf(sentence)

# start with a naive bayes model
nb <- naive_Bayes() %>% 
  set_mode("classification") %>% 
  set_engine("naivebayes")

sentence_wf <- workflow() %>% 
  add_model(nb) %>% 
  add_recipe(sentence_rec)

nb_fit <- sentence_wf %>% 
  fit(data = train_data)


  
  