library(tidyverse)
library(haven)
library(estimatr)
setwd("C:/Users/nod086/Downloads/")

df <- read_sav("HOP 220224 CAPS v1.sav")

df <- df %>% 
  mutate(white= ifelse(QRACE == 1, 1, 0),
         male = ifelse(D1 == 1, 1, 0),
         age = D2,
         region = H3, 
         education = H5,
         republican = ifelse(H4New %in% c(1, 3), 1, 0),
         income = H5A,
         income = ifelse(income == 7, NA, income),
         party = case_when(
           H4 == 1 ~ "GOP",
           H4 == 2 ~ "Dem",
           H4 == 3 ~ "Ind",
           H4 == 4 ~ "Other"),
         party2 = case_when(
           H4New %in% c(1, 3) ~ "GOP",
           H4New %in% c(2, 4) ~ "Dem"
         ))

# make summary table of Q1

first <- qs %>% 
  select(starts_with("Q1")) 

names(first) <- c("state","local", "middle_class",
                  "ethnicity","region",
                  "usa","job","religion")
holder <- NULL
for(i in 1:length(names(first))){
  holder[i] <- sum(first[,i] == 1)/nrow(first)
}



# figure of group belonging proportions 
cbind(names(first), holder) %>%
  as_tibble() %>% 
  rename(group = V1, 
         prop = holder) %>% 
  mutate(prop = as.numeric(prop)) %>% 
  ggplot(aes(x = reorder(group,-prop), y = prop)) + 
  geom_bar(stat = "identity")

# make plot of people's thermometer ratings, by area and party
ratings <- df %>% 
  select(record, party, party2, starts_with("Q2New")) %>% 
  rename(local = Q2Newr1, 
         major_city = Q2Newr2, 
         rural = Q2Newr3, 
         state = Q2Newr4,
         own_region = Q2Newr5, 
         other_regions = Q2Newr6,
         usa = Q2Newr7) %>% 
  pivot_longer(cols = local:usa,
               names_to = "area",
               values_to = "rating")
ratings %>% 
  filter(party %in% c("GOP","Dem")) %>% 
  ggplot() + 
  geom_density(aes(x = rating, color = party2)) +
  facet_wrap(~ area) +
  theme_minimal()





# who belongs to the local community? 
summary(lm(data = df, 
          Q1Newr2 ~ white + male + age + republican + education + income))
