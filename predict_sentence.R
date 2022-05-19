setwd("C:/Users/dapon/Dropbox/Harvard")

library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textmodels)

# read in full dataframe 
df <- read_csv("GeoAppeals/data/sentence_level_newsletter_dataset_with_annotations.csv")

df %>% 
  filter(is_annotated == 1) %>% 
  dplyr::select(sentence_w_mention, majority_annotation, 
                is_annotated, ends_with("annotation"))
