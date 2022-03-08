library(tidyverse)
library(haven)
library(estimatr)
setwd("C:/Users/nod086/Downloads/")

df <- read_sav("caps_questions_feb2022.sav")

# read in the full harris poll 
harris <- read_csv("harris_feb_2022.csv") %>% 
  rename(urban_rural = QD16)

# join the full harris poll with hte CAPS questions
full <- df %>% 
  left_join(., 
            harris %>% select(-urban_rural) ,
            by = c("record" = "RECORD")) %>% 
  # ASK LIZ - IS THIS THE RIGHT VARIABLE ON WHICH TO MERGE? 
  # rename and clean some variables 
  rename(age = D2, 
         region = H3, 
         education = H4, 
         income = H5A) %>% 
  mutate(white = ifelse(QRACE.x == 1, 1, 0), 
         male = ifelse(D1 == 1, 1, 0),
         republican = ifelse(H4New %in% c(1, 3), 1, 0))

urban_rural <- harris %>% 
  select(RECORD, urban_rural) %>% 
  mutate(urban_rural_cat = case_when(
    urban_rural == 1 ~ "urban",
    urban_rural == 2 ~ "suburban",
    urban_rural == 3 ~ "rural"
  ))

full <- left_join(full, urban_rural, 
          by = c("record" = "RECORD")) 


# make summary table of Q1

first <- full %>% 
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
ratings <- full %>% 
  select(record, republican, urban_rural_cat,
         white, male, age, republican, education, income, starts_with("Q2New")) %>% 
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
  filter(area %in% c("major_city","rural"),
         !is.na(urban_rural_cat)) %>% 
  rename(`Area being rated` = area) %>% 
  ggplot() + 
  geom_density(aes(x = rating, color = `Area being rated`)) +
  facet_wrap(~ urban_rural_cat) +
  labs(title = "Thermometer ratings of areas, by place-type of residence")

rate_subset <- ratings %>%
  filter(area == "state")

summary(lm(data = rate_subset, 
           rating ~  white + male + age + 
             republican + education + income + 
             as.factor(urban_rural_cat)))

# who belongs to the local community? 
summary(lm(data = df, 
          Q1Newr2 ~ white + male + age + republican + education + income))
