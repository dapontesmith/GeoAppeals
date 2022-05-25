setwd("C:/Users/dapon/Dropbox/Harvard")

library(tidyverse)
library(caret) # for confusion matrix
library(quanteda)
library(quanteda.textstats)
library(quanteda.textmodels)
library(quanteda.textplots)
library(patchwork) # for plots 

# read in full dataframe 
df <- read.csv("GeoAppeals/data/sentence_level_newsletter_dataset_with_annotations_[20220523].csv")

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
  ))

train <- annotated %>% 
  filter(is_training == 1) %>% 
  select(sentence = sentence_w_mention, class = majority_annotation_cat, docid)
val <- annotated %>% 
  filter(is_validation == 1) %>% 
  select(sentence = sentence_w_mention, class = majority_annotation_cat, docid)

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
             remove = c(stopwords("english"),
                        "fffc","u","â"))
  
  return(dfm)
  
}

train_dfm <- create_dfm(data = train, 
           text_var = "sentence", 
           docnames = "docid", 
           class_var = "class")

val_dfm <- create_dfm(data = val, 
                        text_var = "sentence", 
                        docnames = "docid", 
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

# function to plot the confusion matrix
plot_confusion <- function(data, class = "class", 
                           pred = "pred",
                           dataset_name){
  # takes as input the first element of output of run_nb function 
  
  title <- paste("Confusion matrix in", dataset_name, "data", sep = " ")
  table(data[[class]], data[[pred]]) %>% 
    as.data.frame() %>% 
    rename(actual = Var1, predicted = Var2) %>% 
  # Save confusion matrix as data frame 
  # plot matrix
    ggplot(., aes(x = predicted, y = actual, fill = Freq)) + 
    geom_tile() + 
    xlab("Predicted class") + 
    ylab("Actual class") +
    theme_minimal() + 
    scale_fill_distiller(palette = "Blues", direction = 1) + 
    ggtitle(title)
  
}

train_confusion <- plot_confusion(data = train, dataset_name = "train")
val_confusion <- plot_confusion(data = val, dataset_name = "validation")
train_confusion + val_confusion


topfeatures(
  train_dfm,
  n = 10,
  decreasing = TRUE,
  scheme = c("count", "docfreq"),
  groups = docvars(train_dfm)$class
)


textplot_wordcloud(train_dfm, min_count = 1, color ='red')  



