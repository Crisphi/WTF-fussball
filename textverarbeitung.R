library(data.table)
library(tidyverse)
library(tidytext)
library(NLP)
library(openNLP) # benötigt Java JDK: https://www.oracle.com/java/technologies/downloads/
library(corpustools)
library(topicmodels)
library(ggwordcloud)
library(wordcloud)

#knüpft an datenbereinigung.R an

# --- für Saisons 17/18 bis 22/23 ---

# --- Load Dataframe and get Text ---
load("./weltfussball_liveticker/df_BL_1718_bis_2223_pt.RData")

# --- Remove Text-Elements that are currently not relevant to ease computation ---
df_livetexts_1718_bis_2223 <- df_BL_1718_bis_2223_pt %>% 
  filter(!grepl("^Spielende$", Text)) %>% 
  filter(Text != "Anpfiff 2. Halbzeit") %>% 
  filter(Text != "Anpfiff 2. Halbzeit ") %>%
  filter(Text != "Ende 1. Halbzeit") %>%
  filter(Text != "Spielbeginn") %>% 
  filter(!str_starts(Text, "Einwechslung")) %>% 
  filter(!str_starts(Text, "Auswechslung")) %>% 
  filter(!str_starts(Text, "Offizielle Nachspielzeit"))

# replace minutes, scores, numbers, quotes

df_livetexts_1718_bis_2223 <- df_livetexts_1718_bis_2223 %>% 
  mutate(Text = str_replace_all(Text, "\\d+\\. Minute", "@min@")) %>%
  mutate(Text = str_replace_all(Text, "\\(\\d+\\.\\)", "@br_min@")) %>%
  mutate(Text = str_replace_all(Text, "\\d+:\\d+", "@score@")) %>%
  mutate(Text = str_replace_all(Text, "\\s+\\d+\\s*\\.", " @other_num@")) %>% 
  mutate(Text = str_replace_all(Text, '\"', ' @quote@ ')) %>%
  mutate(Text = str_replace_all(Text, "\n" , " ")) %>% 
  mutate(Text = str_replace_all(Text, "Tooor für .*?, @score@ durch .*?\\s.*?\\s+", "@goal@ ")) %>% 
  mutate(Text = str_replace_all(Text, "Gelbe Karte für .*?\\)\\s+", "@yellow_card@ ")) %>% 
  mutate(Text = str_replace_all(Text, "Rote Karte für .*?\\)\\s+", "@red_card@ "))
# View(df_livetexts_1718_bis_2223)

# df_livetexts speichern
save(df_livetexts_1718_bis_2223, file = "./weltfussball_liveticker/df_livetexts_1718_bis_2223.RData")

texts <- head(df_livetexts_1718_bis_2223$Text, 10) #später head entfernen

# --- Sentence Annotation ---

sentence_annotator <- Maxent_Sent_Token_Annotator(language = "de")
txt <- texts %>%
  # str_replace_all("\\d+\\. Minute", "@min@") %>%
  # str_replace_all("\\(\\d+\\.\\)", "@br_min@") %>%
  # str_replace_all("\\d+:\\d+", "@score@") %>%
  # str_replace_all("\\s+\\d+\\s*\\.", " @other_num@") %>%
  # #TODO Teamnamen filtern
  # as.String() %>%
  # str_replace_all('\"', ' @quote@ ') %>%
  # str_replace_all("\n" , " ") %>%
  as.String()

sentence_annotation <- annotate(txt, sentence_annotator)
# txt[sentence_annotation]
str_count(txt, " @other_num@")
str_count(txt, " @min@")
str_count(txt, " @score@")
str_count(txt, " @br_min@")
str_count(txt, "@quote@")

# --- Tokenisation ---

token_annotator <- Maxent_Word_Token_Annotator()
token_annotation <- annotate(txt,
                             token_annotator,
                             sentence_annotation)

# --- POS Annotation ---

pos_tagger <- Maxent_POS_Tag_Annotator()
annotation <- annotate(txt, pos_tagger, token_annotation)
# annotation

tokens <- txt[annotation[annotation$type == "word"]]

pos_sequence <- as.character(unlist(
  annotation$features[
    annotation$type == "word"
  ]
))


livetexts_tibble <- tibble(ID = 1:length(tokens), Token = tokens, POS = pos_sequence)
livetexts_tibble

# --- Corpustools ---

# --- Create Corpus ---

# rename column Text, otherwise create_tcorpus() gives error: cannot find column "text" 
df_livetexts_1718_bis_2223 <- df_livetexts_1718_bis_2223 %>% rename(text = Text)

df_livetexts_1718_bis_2223$id <- sprintf("doc_%05d", 1:nrow(df_livetexts_1718_bis_2223))
corpus_1718_bis_2223 <- create_tcorpus(df_livetexts_1718_bis_2223,
                         doc_column = "id",
                         text_colums = "text",
                         split_sentences = TRUE,
                         verbose = TRUE)
corpus_1718_bis_2223

# --- Preprocessing ---

# keep copy of original corpus
corpus_1718_bis_2223_old <- corpus_1718_bis_2223$copy()

corpus_1718_bis_2223$preprocess(column = "token",
                  new_column = "token2",
                  lowercase = TRUE,
                  remove_punctuation = TRUE,
                  remove_stopwords = TRUE,
                  remove_numbers = FALSE,
                  use_stemming = FALSE,
                  language = "german")

corpus_1718_bis_2223$tokens

# --- Document Feature Matrix ---

corpus_1718_bis_2223$preprocess(column = "token",
                  new_column = "feature",
                  remove_stopwords = TRUE,
                  remove_numbers = FALSE,
                  use_stemming = TRUE,
                  min_docfreq = 3,
                  language = "german")
corpus_1718_bis_2223$tokens

dfm <- get_dfm(corpus_1718_bis_2223, "feature")
dfm

dfm_weighted <- get_dfm(corpus_1718_bis_2223, "feature", weight = "tfidf")
dfm_weighted

dfm_weighted %>%  tidy() %>% arrange(desc(count))

# Beispiel für Subset
# doc_30359 <- subset(corpus_1718_bis_2223, subset_meta = doc_id == "doc_30359")
# doc_30359$tokens

# query_result <- search_features(corpus_1718_bis_2223, 
#                                 feature = "token",
#                                 query = c("VAR", "Videoassistent", "Videoschiedsrichter"))
# table(as.character(query_result$hits$feature))
# 
# --- tidytext ---

livetexts_tidy <- as_tibble(corpus_1718_bis_2223$tokens) %>%
  inner_join(corpus_1718_bis_2223$meta, by = "doc_id") %>%
  # ggf. Spalten mit Metadaten ausblenden
  select(doc_id, sentence, token_id, token, token2, feature) %>%
  mutate(word = token2)
livetexts_tidy

# count words
# livetexts_tidy %>% count(word, sort = TRUE)

# wordcloud
# livetexts_tidy %>% count(word) %>% with(wordcloud(word, n, max.words=50))


# --- corups_old, corpus & livetexts_tidy speichern ---

save(corpus_1718_bis_2223, corpus_1718_bis_2223_old, livetexts_tidy,
     file = "./weltfussball_liveticker/corpus_1718_bis_2223.RData")


# ------------------------------------------------------------------------------

# --- nochmal für Saisons 11/12 bis 16/17 ---

# --- Load Dataframe and get Text ---
load("./weltfussball_liveticker/df_BL_1112_bis_1617_pt.RData")

# --- Remove Text-Elements that are currently not relevant to ease computation ---
df_livetexts_1112_bis_1617 <- df_BL_1112_bis_1617_pt %>% 
  filter(!grepl("^Spielende$", Text)) %>% 
  filter(Text != "Anpfiff 2. Halbzeit") %>% 
  filter(Text != "Anpfiff 2. Halbzeit ") %>%
  filter(Text != "Ende 1. Halbzeit") %>%
  filter(Text != "Spielbeginn") %>% 
  filter(!str_starts(Text, "Einwechslung")) %>% 
  filter(!str_starts(Text, "Auswechslung")) %>% 
  filter(!str_starts(Text, "Offizielle Nachspielzeit"))

# replace minutes, scores, numbers, quotes

df_livetexts_1112_bis_1617 <- df_livetexts_1112_bis_1617 %>% 
  mutate(Text = str_replace_all(Text, "\\d+\\. Minute", "@min@")) %>%
  mutate(Text = str_replace_all(Text, "\\(\\d+\\.\\)", "@br_min@")) %>%
  mutate(Text = str_replace_all(Text, "\\d+:\\d+", "@score@")) %>%
  mutate(Text = str_replace_all(Text, "\\s+\\d+\\s*\\.", " @other_num@")) %>% 
  mutate(Text = str_replace_all(Text, '\"', ' @quote@ ')) %>%
  mutate(Text = str_replace_all(Text, "\n" , " ")) %>% 
  mutate(Text = str_replace_all(Text, "Tooor für .*?, @score@ durch .*?\\s.*?\\s+", "@goal@ ")) %>% 
  mutate(Text = str_replace_all(Text, "Gelbe Karte für .*?\\)\\s+", "@yellow_card@ ")) %>% 
  mutate(Text = str_replace_all(Text, "Rote Karte für .*?\\)\\s+", "@red_card@ "))
# View(df_livetexts_1112_bis_1617)

# df_livetexts speichern
save(df_livetexts_1112_bis_1617, file = "./weltfussball_liveticker/df_livetexts_1112_bis_1617.RData")

texts <- head(df_livetexts_1112_bis_1617$Text, 10) #später head entfernen

# --- Sentence Annotation ---

sentence_annotator <- Maxent_Sent_Token_Annotator(language = "de")
txt <- texts %>%
  # str_replace_all("\\d+\\. Minute", "@min@") %>%
  # str_replace_all("\\(\\d+\\.\\)", "@br_min@") %>%
  # str_replace_all("\\d+:\\d+", "@score@") %>%
  # str_replace_all("\\s+\\d+\\s*\\.", " @other_num@") %>%
  # #TODO Teamnamen filtern
  # as.String() %>%
  # str_replace_all('\"', ' @quote@ ') %>%
  # str_replace_all("\n" , " ") %>%
  as.String()

sentence_annotation <- annotate(txt, sentence_annotator)
# txt[sentence_annotation]
str_count(txt, " @other_num@")
str_count(txt, " @min@")
str_count(txt, " @score@")
str_count(txt, " @br_min@")
str_count(txt, "@quote@")

# --- Tokenisation ---

token_annotator <- Maxent_Word_Token_Annotator()
token_annotation <- annotate(txt,
                             token_annotator,
                             sentence_annotation)

# --- POS Annotation ---

pos_tagger <- Maxent_POS_Tag_Annotator()
annotation <- annotate(txt, pos_tagger, token_annotation)
# annotation

tokens <- txt[annotation[annotation$type == "word"]]

pos_sequence <- as.character(unlist(
  annotation$features[
    annotation$type == "word"
  ]
))


livetexts_tibble <- tibble(ID = 1:length(tokens), Token = tokens, POS = pos_sequence)
livetexts_tibble

# --- Corpustools ---

# --- Create Corpus ---

# rename column Text, otherwise create_tcorpus() gives error: cannot find column "text" 
df_livetexts_1112_bis_1617 <- df_livetexts_1112_bis_1617 %>% rename(text = Text)

df_livetexts_1112_bis_1617$id <- sprintf("doc_%05d", 1:nrow(df_livetexts_1112_bis_1617))
corpus_1112_bis_1617 <- create_tcorpus(df_livetexts_1112_bis_1617,
                                       doc_column = "id",
                                       text_colums = "text",
                                       split_sentences = TRUE,
                                       verbose = TRUE)
corpus_1112_bis_1617

# --- Preprocessing ---

# keep copy of original corpus
corpus_1112_bis_1617_old <- corpus_1112_bis_1617$copy()

corpus_1112_bis_1617$preprocess(column = "token",
                                new_column = "token2",
                                lowercase = TRUE,
                                remove_punctuation = TRUE,
                                remove_stopwords = TRUE,
                                remove_numbers = FALSE,
                                use_stemming = FALSE,
                                language = "german")

corpus_1112_bis_1617$tokens

# --- Document Feature Matrix ---

corpus_1112_bis_1617$preprocess(column = "token",
                                new_column = "feature",
                                remove_stopwords = TRUE,
                                remove_numbers = FALSE,
                                use_stemming = TRUE,
                                min_docfreq = 3,
                                language = "german")
corpus_1112_bis_1617$tokens

dfm <- get_dfm(corpus_1112_bis_1617, "feature")
dfm

dfm_weighted <- get_dfm(corpus_1112_bis_1617, "feature", weight = "tfidf")
dfm_weighted

dfm_weighted %>%  tidy() %>% arrange(desc(count))

# Beispiel für Subset
# doc_30359 <- subset(corpus_1112_bis_1617, subset_meta = doc_id == "doc_30359")
# doc_30359$tokens

query_result <- search_features(corpus_1112_bis_1617, 
                                feature = "token",
                                query = c("VAR", "Videoassistent", "Videoschiedsrichter"))
table(as.character(query_result$hits$feature))

# --- tidytext ---

livetexts_tidy <- as_tibble(corpus_1112_bis_1617$tokens) %>% 
  inner_join(corpus_1112_bis_1617$meta, by = "doc_id") %>% 
# ggf. Spalten mit Metadaten ausblenden
  select(doc_id, sentence, token_id, token, token2, feature) %>% 
  mutate(word = token2)
livetexts_tidy

# count words
livetexts_tidy %>% count(word, sort = TRUE)

# wordcloud
livetexts_tidy %>% count(word) %>% with(wordcloud(word, n, max.words=50))


# --- corups_old, corpus & livetexts_tidy speichern ---

save(corpus_1112_bis_1617, corpus_1112_bis_1617_old, livetexts_tidy,
     file = "./weltfussball_liveticker/corpus_1112_bis_1617.RData")