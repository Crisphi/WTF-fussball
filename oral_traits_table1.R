library(data.table)
library(tidyverse)
library(tidytext)
library(NLP)
library(openNLP) # benötigt Java JDK: https://www.oracle.com/java/technologies/downloads/
library(corpustools)
library(topicmodels)
library(spacyr)

# --- load Dataframes ---
load("./weltfussball_liveticker/df_livetexts_1718_bis_2223.RData")
load("./weltfussball_liveticker/df_livetexts_1112_bis_1617.RData")

# bind Dataframes

df_livetexts_1112_bis_2223 <- rbind(df_livetexts_1112_bis_1617, df_livetexts_1718_bis_2223)

# --- compute average number of entries per game (epg) ---

count_epg_1112_bis_2223 <- df_livetexts_1112_bis_2223 %>% 
  group_by(Season) %>% 
  # group_by(game_ID) %>% 
  summarise(avg_entries_per_game = n() / 306)

print(count_epg_1112_bis_2223)

# --- compute avg. number of sentences per entry (spe) ---

spacy_initialize("de_core_news_sm")

# sentence counter function
count_sentences <- function(text) {
  tokens <- spacy_tokenize(text, what = "sentence")
  n_sentences <- length(unlist(tokens))
  return(n_sentences)
}

# apply sentence counter to dataframe, this will take a while...
df_livetexts_1112_bis_2223$Sentence_Count <- sapply(df_livetexts_1112_bis_2223$Text, count_sentences)
save(df_livetexts_1718_bis_2223, file = "./weltfussball_liveticker/counted_sentences_1112_bis_2223.RData")

# compute average number of sentences per entry grouped by season
count_spe_1112_bis_2223 <- df_livetexts_1112_bis_2223 %>% 
  group_by(Season) %>% 
  summarise(avg_spe = round(mean(Sentence_Count), digits = 3))
print(count_spe_1112_bis_2223)

# --- merge computed statistics into one table ---

merged_table1 <- inner_join(count_epg_1112_bis_2223,
                           count_spe_1112_bis_2223)
print(merged_table1)