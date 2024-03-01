library(data.table)
library(tidyverse)
library(tidytext)
library(NLP)
library(openNLP) # benötigt Java JDK: https://www.oracle.com/java/technologies/downloads/
library(corpustools)
library(topicmodels)
library(ggwordcloud)
library(wordcloud)

# --- Load Dataframe and get Text ---
load("./weltfussball_liveticker/df_BL_1718_bis_2223_pt")

# --- Remove Text-Elements that are currently not relevant to ease computation ---
df_livetexts <- df_BL_1718_bis_2223_pt %>% 
  filter(!grepl("^Spielende$", Text)) %>% 
  filter(Text != "Anpfiff 2. Halbzeit") %>% 
  filter(Text != "Anpfiff 2. Halbzeit ") %>%
  filter(Text != "Ende 1. Halbzeit") %>%
  filter(Text != "Spielbeginn") %>% 
  filter(!str_starts(Text, "Tooor")) %>% 
  filter(!str_starts(Text, "Einwechslung")) %>% 
  filter(!str_starts(Text, "Auswechslung")) %>% 
  filter(!str_starts(Text, "Gelbe Karte")) %>% 
  filter(!str_starts(Text, "Rote Karte")) %>% 
  filter(!str_starts(Text, "Offizielle Nachspielzeit"))

texts <- head(df_livetexts$Text, 10)
text <- texts[1]

# --- Basic String manipulations ---
#nchar(text)

#nchar(texts)

#substr(text, start = 11, stop = 16)

#strsplit(text, " ")  # split

#strsplit(text, " ") %>% unlist()

#strsplit(texts[1:2], " ")

#tolower(text)

#toupper(text)

#grep("[[:digit:]]", texts[3])

#grep("[[:digit:]]", texts)

#grep("[[:digit:]]", texts, value = TRUE)

#grepl("[[:digit:]]", texts)

#NFC Normalisation for umlaute
#stringi::stri_trans_isnfc(text)

# --- Sentence Annotation ---

sentence_annotator <- Maxent_Sent_Token_Annotator(language = "de")
txt <- texts %>%
  str_replace_all("\\d+\\. Minute", "@min@") %>%
  str_replace_all("\\(\\d+\\.\\)", "@br_min@") %>%
  str_replace_all("\\d+:\\d+", "@score@") %>%
  str_replace_all("\\s+\\d+\\s*\\.", " @other_num@") %>%
  #TODO Teamnamen filtern
  as.String() %>%
  str_replace_all('\"', ' @quote@ ') %>%
  str_replace_all("\n" , " ") %>%
  as.String()

sentence_annotation <- annotate(txt, sentence_annotator)
txt[sentence_annotation]
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


tibble(ID = 1:length(tokens), Token = tokens, POS = pos_sequence)