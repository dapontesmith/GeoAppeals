setwd("C:/Users/dapon/Dropbox/Harvard")

library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textmodels)

# read in full dataframe 
df <- read_csv("GeoAppeals/data/sentence_level_newsletter_dataset_with_annotations.csv")

annotated <- df %>% 
  filter(is_annotated == 1) %>% 
  dplyr::select(sentence_w_mention, majority_annotation, 
                is_annotated, ends_with("annotation"),
                is_training, is_validation) %>% 
  # fill majority annotation with placeholder for now 
  mutate(docid = 1:n(),
         majority_annotation = case_when(
           docid %% 2 == 0 ~ "fizz",
           docid %% 2 == 1 ~ "buzz"
         ))

train <- annotated %>% 
  filter(is_training == 1) %>% 
  select(sentence = sentence_w_mention, class = majority_annotation, docid)
val <- annotated %>% 
  filter(is_validation == 1) %>% 
  select(sentence = sentence_w_mention, class = majority_annotation, docid)

create_dfm <- function(data, text_var, docnames_var,
                       class_var){
  corpus <- corpus(data[[text_var]], 
                   docnames = data[[docnames_var]])
  
  docvars(corpus, "class") <- data[[class_var]]
  
  token <- tokens(
    corpus, 
    split_hyphens = TRUE,
    remove_numbers = TRUE,
    remove_punct = TRUE, 
    remove_symbols = TRUE, 
    remove_url = TRUE, 
    include_docvars = TRUE
  )
  
  dfm <- dfm(token, 
             tolower = TRUE, 
             stem = TRUE, 
             remove = stopwords("english"))
  
  return(dfm)
  
}

train_dfm <- create_dfm(data = train, 
           text_var = "sentence", 
           docnames = "doc_id", 
           class_var = "class")

val_dfm <- create_dfm(data = val, 
                        text_var = "sentence", 
                        docnames = "doc_id", 
                        class_var = "class")


run_nb <- function(data, training_dfm, dfm, class = "class"){
  # arguments - 
  # data = a dataset
  #train_dfm = dfm to use to run the model on
  # dfm = dfm in which to make predictions
  
  model <- textmodel_nb(training_dfm, 
                        docvars(training_dfm, class),
                        prior = "docfreq")
  
  out <- data %>% 
    mutate(pred = predict(model, newdata = dfm, force = TRUE) )
  
  
  accuracy <- sum(out$class == out$pred)/nrow(out)
  
  return(list(out, accuracy))
  
}

# run function on train and validation 
val_out <- run_nb(val, train_dfm, dfm = val_dfm)
val <- val_out[[1]] ; acc_val <- val_out[[2]] 

train_out <- run_nb(data = train, training_dfm = train_dfm, dfm = train_dfm)
train <- train_out[[1]] ; acc_train <- train_out[[2]] 

acc_train > acc_val


