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
  # fill majority annotation with placeholder for now 
  mutate(docid = 1:n()) %>% 
  # get only snetneces in 1 of hte three categories
  filter(majority_annotation %in% c(0, 1, 99)) %>% 
  mutate(majority_annotation_cat = case_when(
    majority_annotation == 0 ~ "policy",
    majority_annotation == 1 ~ "symbolic",
    majority_annotation == 99 ~ "NA"
  ),
  majority_annotation_cat = as.factor(majority_annotation_cat)) %>% 
  select(sentence = sentence_w_mention, class = majority_annotation_cat)

split <- initial_split(annotated)

train_data <- training(split)
test_data <- testing(split)

sentence_rec <- recipe(
  class ~ sentence, data = train_data) %>% 
  step_tokenize(sentence) %>% 
  step_tokenfilter(sentence, max_tokens = 1e3) %>%
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



# now fit a model
logit <- logistic_reg() %>% 
  set_engine("glm")

# now build a workflow 
logit_workflow <- workflow() %>% #initialize workflow
  # add model to the workflow
  add_model(logit) %>% 
  # add recipe to the workflow - this will process the data
  add_recipe(first_recipe) 


  
  