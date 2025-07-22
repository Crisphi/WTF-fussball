# weiterentwickelte Version von textverarbeitung.R

# knüpft an datenbereinigung.R an

# ---- load libraries ----
options(java.parameters = "-Xmx2g")

library(data.table)
library(tidyverse)
library(tidytext)
library(NLP)
library(rJava)
library(openNLP) # benötigt Java JDK: https://www.oracle.com/java/technologies/downloads/
library(corpustools)
library(topicmodels)
library(ggwordcloud)
library(wordcloud)
library(dplyr)
library(readr)
library(stringr)
library(progress)
library(progressr)
library(purrr)
library(stopwords)

# ---- data preparation for NLP ----


# ---- für Saisons 17/18 bis 22/23

# Load Dataframe and get Text
load("./weltfussball_liveticker/df_BL_1718_bis_2223_pt.RData")

# Remove Text-Elements that are currently not relevant to ease computation
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
  mutate(Text = str_replace_all(Text, "Rote Karte für .*?\\)\\s+", "@red_card@ ")) %>%
  
  # generate doc_IDs
  group_by(game_ID) %>% 
  mutate(doc_ID = paste0(
    str_replace_all(game_ID, "/", "_"),
    "_", sprintf("%03d", row_number())
  )) %>% 
  ungroup()

# View(df_livetexts_1718_bis_2223)

# df_livetexts speichern
save(df_livetexts_1718_bis_2223, file = "./weltfussball_liveticker/df_livetexts_1718_bis_2223.RData")


# ---- nochmal für Saisons 11/12 bis 16/17

# Load Dataframe and get Text
load("./weltfussball_liveticker/df_BL_1112_bis_1617_pt.RData")

# Remove Text-Elements that are currently not relevant to ease computation
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
  mutate(Text = str_replace_all(Text, "Rote Karte für .*?\\)\\s+", "@red_card@ ")) %>% 
  
  # generate doc_IDs
  group_by(game_ID) %>% 
  mutate(doc_ID = paste0(
    str_replace_all(game_ID, "/", "_"),
    "_", sprintf("%03d", row_number())
  )) %>% 
  ungroup()

# View(df_livetexts_1112_bis_1617)

# df_livetexts speichern
save(df_livetexts_1112_bis_1617, file = "./weltfussball_liveticker/df_livetexts_1112_bis_1617.RData")


# ---- Utility: progress bar for later functions ----

handlers(global = TRUE)
handlers("txtprogressbar")


# ---- Annotationsfunktion (sentence, word, POS, (NER)) mit openNLP ----

# annotation function (sentence, word, POS)
# Bearbeitung der Daten in Portionen (Einzeldateien), da POS-Annotation auf vielen Daten nicht mehr funktioniert (s. textverarbeitung.R)

annotate_liveticker_batch <- function(texts,
                                      doc_ids = NULL,
                                      output_dir = "annotation_output",
                                      skip_existing = TRUE,
                                      ner = FALSE) { # wenn NER benötigt, ner = TRUE setzen
  # Erzeuge Ordner zur Zwischenspeicherung
  if (!dir.exists(output_dir)) dir.create(output_dir)
  
  # Erstellung von Standard doc_IDs, falls keine an Funktion übergeben werden
  # tatsächlich werden später die game_IDs übergeben
  if (is.null(doc_ids)) {
    doc_ids <- paste0("doc_", sprintf("%05d", seq_along(texts)))
  }
  
  # Lade Annnotatoren
  sent_annotator <- Maxent_Sent_Token_Annotator(language = "de")
  word_annotator <- Maxent_Word_Token_Annotator(language = "de")
  pos_annotator <- Maxent_POS_Tag_Annotator(language = "de")
  
  if (ner) {
    ner_person <- Maxent_Entity_Annotator(kind = "person", language = "en")
    ner_org <- Maxent_Entity_Annotator(kind = "organization", language = "en")
    ner_loc <- Maxent_Entity_Annotator(kind = "location", language = "en")
  }
  
  # Fortschrittsanzeige
  pb <- progress_bar$new(
    format = "Annotiere :current/:total [:bar] :percent :eta",
    total = length(texts),
    width = 60
  )
  
  for (i in seq_along(texts)) {
    pb$tick()
    
    this_doc <- doc_ids[i]
    outfile <- file.path(output_dir, paste0(this_doc, ".rds"))
    
    # Falls schon gespeichert, überspringen
    if (skip_existing && file.exists(outfile)) {
      next
    }
    
    this_text <- texts[i]
    
    # leere Texte abfangen
    if (is.na(this_text) || str_trim(this_text) == "") {
      warning(paste("Leerer Text:", this_doc))
      next
    }
    
    txt <- as.String(this_text)
    
    # Annotation
    sent_ann <- annotate(txt, sent_annotator)
    word_ann <- annotate(txt, word_annotator, sent_ann)
    pos_ann <- annotate(txt, pos_annotator, word_ann)
    
    words <- subset(pos_ann, type == "word")
    tags <- sapply(words$features, `[[`, "POS")
    tokens <- txt[words]
    
    df <- tibble(
      doc_id = this_doc,
      token_id = seq_along(tokens),
      token = as.character(tokens),
      pos = tags
    )
    
    # Optional: Lemmatisierung
    
    # Optional: Named Entity Recognition
    
    if (ner) {
      ner_ann <- annotate(txt, list(ner_person, ner_org, ner_loc), word_ann)
      entities <- ner_ann[ner_ann$type == "entity"]
      
      if (length(entities) > 0) {
        entities_df <- tibble(
          doc_id = this_doc,
          entity = sapply(entities, function(e) substr(txt, e$start, e$end)),
          type = sapply(entities, function(e) e$features$kind)
        )
      } else {
        entities_df <- tibble(doc_id = character(), entity = character(), type = character())
      }
      
      saveRDS(list(tokens = df, entities = entities_df), outfile)
      
    } else {
      saveRDS(df, outfile)
    }
  }
  
  message("Annotation abgeschlossen")
}

# ---- Annotationsfunktion anwenden ----

# Funktion aufrufen für 17/18 bis 22/23
annotate_liveticker_batch(df_livetexts_1718_bis_2223$Text,
                          doc_ids = df_livetexts_1718_bis_2223$doc_ID,
                          output_dir = "annotation_output_1718_bis_2223",
                          skip_existing = FALSE)


# Einzeldateien 17/18 bis 22/23 zusammenfügen

annotation_files_1718_bis_2223 <- list.files(
  path = "annotation_output_1718_bis_2223",
  pattern = "\\.rds$",
  full.names = TRUE
)

tokens_df_1718_bis_2223 <- with_progress({
  p <- progressor(steps = length(annotation_files_1718_bis_2223))
  map_dfr(annotation_files_1718_bis_2223, function(f) {
    p()
    df <- readRDS(f)
    df <- df %>%
      mutate(across(where(~ inherits(.x, "String")), as.character))
    df
  })
})


# Funktion aufrufen für 11/12 bis 16/17
annotate_liveticker_batch(df_livetexts_1112_bis_1617$Text,
                          doc_ids = df_livetexts_1112_bis_1617$doc_ID,
                          output_dir = "annotation_output_1112_bis_1617",
                          skip_existing = TRUE)

# Einzeldateien 11/12 bis 16/17 zusammenfügen

annotation_files_1112_bis_1617 <- list.files(
  path = "annotation_output_1112_bis_1617",
  pattern = "\\.rds$",
  full.names = TRUE
)

tokens_df_1112_bis_1617 <- with_progress({
  p <- progressor(steps = length(annotation_files_1112_bis_1617))
  map_dfr(annotation_files_1112_bis_1617, function(f) {
    p()
    df <- readRDS(f)
    df <- df %>%
      mutate(across(where(~ inherits(.x, "String")), as.character))
    df
  })
})

# ---- Lemmatisierung (leer) ----

# fällt weg, da umständlich es nachträglich der Pipeline hinzuzufügen (besser von vorneherein alles mit udpipe machen)


# ---- Named Entity Recognition (nicht verwendet) ----
# (funktioniert technisch, aber ohne deutsches NER-Modell pratkisch unbrauchbar)

# NER Test

test_subset <- df_livetexts_1718_bis_2223 %>% 
  filter(game_ID == "game_2017/2018_001")

annotate_liveticker_batch(
  texts = test_subset$Text,
  doc_ids = test_subset$doc_ID,
  output_dir = "ner_test_output",
  skip_existing = FALSE,
  ner = TRUE
)

test_result <- readRDS("ner_test_output/game_2017_2018_001_001.rds")

# Test Annotation mit POS-Tagger auf Deutsch

annotate_liveticker_batch(
  texts = test_subset$Text,
  doc_ids = test_subset$doc_ID,
  output_dir = "pos_test_output",
  skip_existing = TRUE,
  ner = FALSE
)

pos_test_files <- list.files(
  path = "pos_test_output",
  pattern = "\\.rds$",
  full.names = TRUE
)

pos_test <- pos_test_files %>% 
  lapply(readRDS) %>% 
  bind_rows()

test_result <- readRDS("pos_test_output/game_2017_2018_001_002.rds")

# ---- Relevante Metadaten vorbereiten und mit tokens_df zusammenfügen ---- 

# für 17/18 bis 22/23 (postVAR)

meta_df_1718_bis_2223 <- df_livetexts_1718_bis_2223 %>% 
  dplyr::select(
    doc_id = doc_ID,
    game_id = game_ID,
    team1 = Team1,
    team2 = Team2,
    season = Season,
    matchday = Matchday,
    date = Date,
    kickoff = Kickoff,
    result = Result,
    minute = Time,
  ) %>% 
  distinct(doc_id, .keep_all = TRUE)

corpus_openNLP_postVAR <- tokens_df_1718_bis_2223 %>% 
  left_join(meta_df_1718_bis_2223, by = "doc_id") %>% 
  mutate(period = "postVAR")

# für 11/12 bis 16/17 (preVAR)

meta_df_1112_bis_1617 <- df_livetexts_1112_bis_1617 %>% 
  dplyr::select(
    doc_id = doc_ID,
    game_id = game_ID,
    team1 = Team1,
    team2 = Team2,
    season = Season,
    matchday = Matchday,
    date = Date,
    kickoff = Kickoff,
    result = Result,
    minute = Time,
  ) %>% 
  distinct(doc_id, .keep_all = TRUE)

corpus_openNLP_preVAR <- tokens_df_1112_bis_1617 %>% 
  left_join(meta_df_1112_bis_1617, by = "doc_id") %>% 
  mutate(period = "preVAR")


# ---- preVAR und postVAR Datensätze zusammenführen ----

corpus_openNLP_all <- bind_rows(corpus_openNLP_preVAR, corpus_openNLP_postVAR)

# ---- Korpora speichern ----

save(tokens_df_1112_bis_1617, tokens_df_1718_bis_2223, corpus_openNLP_postVAR, corpus_openNLP_preVAR, corpus_openNLP_all, file = "./weltfussball_liveticker/corpus_openNLP.RData")


# ---- Stoppwörter, Teamnamen entfernen ----

german_stops <- tibble(word = stopwords::stopwords("de"))

team_names <- corpus_openNLP_all %>% 
  select(team1, team2) %>% 
  pivot_longer(everything(), values_to = "team") %>% 
  distinct(team)

team_tokens <- team_names %>% 
  mutate(team = str_split(team, "\\s+")) %>% 
  unnest(team) %>% 
  distinct(team) %>% 
  rename(token = team)

custom_stopwords <- tibble(
  token = c("BVB", "Hamburg", "HSV", "FCK", "96er", "TSG", "Schalker", "Effzeh", "Wölfe", "SGE", "Breisgauer",
            "Berliner", "Club", "Borussen", "Arminen", "Fohlen", "Kölner", "Hessen", "Fohlenelf", "Werkself",
            "Hannoveraner",
            "Zieler", "Rebić", "Chandler", "Hrádecký", "Füllkrug",
            "Ball", "rechten", "rechts", "linken", "links", "Meter", "Metern", "Bundesliga",
            "Minuten", "Mitte", "Kugel", "Leder", "Pille", "Hausherren", "Gäste", "Seite", "Hälfte"
            )
)

# nach relevanten POS-Tags filtern
corpus_postVAR_filtered <- corpus_openNLP_postVAR %>% 
  filter(pos %in% c("NN",                               # Nomen OHNE NE
                    "ADJA", "ADJD", "ADV",              # Adjektive
                    "VVFIN", "VVIMP", "VVINF", "VVIZU",
                    "VVPP","VAFIN", "VAIMP", "VAINF",
                    "VAPP", "VMFIN", "VMINF", "VMPP"    # Verben
  )) %>% 
  anti_join(german_stops, by = c("token" = "word")) %>% 
  anti_join(team_tokens, by = "token") # %>% 
  # anti_join(custom_stopwords, by = "token")

corpus_preVAR_filtered <- corpus_openNLP_preVAR %>% 
  filter(pos %in% c("NN",
                    "ADJA", "ADJD", "ADV",              # Adjektive
                    "VVFIN", "VVIMP", "VVINF", "VVIZU",
                    "VVPP","VAFIN", "VAIMP", "VAINF",
                    "VAPP", "VMFIN", "VMINF", "VMPP"    # Verben
  )) %>% 
  anti_join(german_stops, by = c("token" = "word")) %>% 
  anti_join(team_tokens, by = "token") # %>% 
  # anti_join(custom_stopwords, by = "token")

corpus_all_filtered <- corpus_openNLP_all %>% 
  filter(pos %in% c("NN", "NNP")) %>% # Nomen OHNE NE
  anti_join(german_stops, by = c("token" = "word")) %>% 
  anti_join(team_tokens, by = "token") %>% 
  anti_join(custom_stopwords, by = "token")

#most_frequent_words <- corpus_preVAR_filtered %>% 
#  count(token, sort = TRUE) %>% 
#  slice_head(n = 30)
#View(most_frequent_words)


# ---- LDA Topic Modelling ausführen ----

# postVAR

# DTM (document-term matrix)
dtm_postVAR <- corpus_postVAR_filtered %>% 
  count(doc_id, token) %>% 
  cast_dtm(document = doc_id, term = token, value = n)

# Topic Modelling
lda_postVAR <- LDA(dtm_postVAR, k = 9, control = list(seed = 123))

# Top-Wörter pro Topic visualisieren
lda_postVAR_plot <- lda_postVAR %>% 
  tidy(matrix = "beta") %>% # beta -> Top-Wörter pro Topic, gamma -> Verteilung der Topics auf Dokumente
  group_by(topic) %>% 
  slice_max(beta, n = 10) %>% 
  ungroup() %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
  labs(title = "Top-Wörter je Topic", x = "β", y = NULL)
ggsave("./plots/lda_postVAR_plot.png", plot = lda_postVAR_plot, width = 10, height = 5.63, dpi = 300)


# preVAR

# DTM (document-term matrix)
dtm_preVAR <- corpus_preVAR_filtered %>% 
  count(doc_id, token) %>% 
  cast_dtm(document = doc_id, term = token, value = n)

# Topic Modelling
lda_preVAR <- LDA(dtm_preVAR, k = 9, control = list(seed = 123))

# Top-Wörter pro Topic visualisieren
lda_preVAR_plot <- lda_preVAR %>% 
  tidy(matrix = "beta") %>% # beta -> Top-Wörter pro Topic, gamma -> Verteilung der Topics auf Dokumente
  group_by(topic) %>% 
  slice_max(beta, n = 10) %>% 
  ungroup() %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
  labs(title = "Top-Wörter je Topic", x = "β", y = NULL)
ggsave("./plots/lda_preVAR_plot.png", plot = lda_preVAR_plot, width = 10, height = 5.63, dpi = 300)


# alle Saisons

# DTM (document-term matrix)
dtm_all <- corpus_all_filtered %>% 
  count(doc_id, token) %>% 
  cast_dtm(document = doc_id, term = token, value = n)

# Topic Modelling
lda_all <- LDA(dtm_all, k = 9, control = list(seed = 123))

# Top-Wörter pro Topic visualisieren
lda_all_plot <- lda_all %>% 
  tidy(matrix = "beta") %>% # beta -> Top-Wörter pro Topic, gamma -> Verteilung der Topics auf Dokumente
  group_by(topic) %>% 
  slice_max(beta, n = 10) %>% 
  ungroup() %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
  labs(title = "Top-Wörter je Topic", x = "β", y = NULL)
ggsave("./plots/lda_all_plot.png", plot = lda_all_plot, width = 10, height = 5.63, dpi = 300)


# ---- Topic Models speichern ----

save(lda_preVAR, lda_postVAR, lda_all, file = "./weltfussball_liveticker/lda_topic_models.RData")


# ---- Generelle Korpusstatistiken ----

# Saisonvergleich: durchschn. Einträge pro Spiel, durchschn. Wörter pro Eintrag

ticker_stats <- corpus_openNLP_all %>% 
  group_by(season, game_id, doc_id) %>% 
  summarise(n_tokens = n(), .groups = "drop") %>% 
  group_by(season, game_id) %>% 
  summarise(
    entries_per_game = n(),
    avg_tokens_per_entry = mean(n_tokens),
    .groups = "drop"
  ) %>% 
  group_by(season) %>% 
  summarise(
    avg_entries_per_game = round(mean(entries_per_game), 1),
    avg_tokens_per_entry = round(mean(avg_tokens_per_entry), 1),
    .groups = "drop"
  )

liveticker_length_plot <- ticker_stats %>% 
  pivot_longer(cols = starts_with("avg"), names_to = "metric", values_to = "value") %>% 
  ggplot(aes(x = season, y = value, group = metric, color = metric)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(title = "Entwicklung der Liveticker-Länge",
       y = "Durchschnitt pro Spiel bzw. Eintrag",
       x = "Saison",
       color = "Metrik") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
liveticker_length_plot

ggsave("./plots/liveticker_length_plot.png", plot = liveticker_length_plot, width = 10, height = 5.63, dpi = 300, bg = "white")


# Einträge pro Minute

docs_to_remove <-  corpus_openNLP_all %>% 
  group_by(doc_id) %>% 
  summarise(
    minute = first(minute),
    has_willkommen = any(str_detect(token, regex("willkommen", ignore_case = TRUE))),
    has_fazit = any(str_detect(token, regex("fazit", ignore_case = TRUE))),
    has_halbzeitfazit = any(str_detect(token, regex("halbzeitfazit", ignore_case = TRUE)))
  ) %>% 
  filter(
    (minute == "1'" & has_willkommen) |
      minute == "45'" & has_halbzeitfazit |
      minute == "90'" & has_fazit
  ) %>% 
  pull(doc_id)

corpus_cleaned_minute_stats <- corpus_openNLP_all %>% 
  filter(!doc_id %in% docs_to_remove) %>% 
  filter(minute != "")

ticker_minute_stats <- corpus_cleaned_minute_stats |>
  filter(!is.na(minute)) |>
  mutate(minute_numeric = parse_number(minute)) |>
  group_by(season, game_id, doc_id, minute_numeric) |>
  summarise(n_tokens = n(), .groups = "drop") |>
  group_by(minute_numeric) |>
  summarise(
    n_entries = n(),
    total_tokens = sum(n_tokens),
    avg_tokens_per_entry = round(total_tokens / n_entries, 1)
  )
#View(ticker_minute_stats)

# Skaliere avg_tokens_per_entry auf die Höhe von n_entries
max_entries <- max(ticker_minute_stats$n_entries, na.rm = TRUE)
max_tokens  <- max(ticker_minute_stats$avg_tokens_per_entry, na.rm = TRUE)
scale_factor <- max_entries / max_tokens

minute_stats_plot <- ggplot(ticker_minute_stats, aes(x = minute_numeric)) +
  geom_col(aes(y = n_entries), fill = "steelblue", alpha = 0.6) +
  geom_line(aes(y = avg_tokens_per_entry * scale_factor), color = "darkred", size = 1.1) +
  scale_y_continuous(
    name = "Anzahl Einträge pro Minute",
    sec.axis = sec_axis(~ . / scale_factor, name = "Ø Tokens pro Eintrag")
  ) +
  labs(
    title = "Ticker-Aktivität und Eintragslänge nach Spielminute (Saisons 11/12 bis 22/23 insgesamt)",
    x = "Spielminute"
  ) +
  theme_minimal()

ggsave("./plots/minute_stats_plot.png", plot = minute_stats_plot, width = 10, height = 5.63, dpi = 300, bg = "white")


# Vergleich bestimmter Worthäufigkeiten in den Datensätzen preVAR und postVAR
# Log-Odds-Ratio mit Glättung

# Funktion zur Berechnung der Log-Odds
compute_log_odds <- function(df1, df2, token_col = "token", alpha = 0.01) {
  
  df1_counts <- df1 %>% 
    count(!!sym(token_col), name = "n_1")
  
  df2_counts <- df2 %>% 
    count(!!sym(token_col), name = "n_2")
  
  full_join(df1_counts, df2_counts, by = token_col) %>% 
    mutate(
      n_1 = replace_na(n_1, 0), # sicherstellen, dass NA (Wort kommt gar nicht vor) als 0 gezählt wird
      n_2 = replace_na(n_2, 0),
      total_1 = sum(n_1), # Gesamtzahl der tokens pro Korpus
      total_2 = sum(n_2),
      p_1 = (n_1 + alpha) / (total_1 + alpha + n()), # Dirichlet-Glättung für Korpus 1, damit log sauber funktioniert
      p_2 = (n_2 + alpha) / (total_2 + alpha + n()), # Dirichlet-Glättung für Korpus 2
      log_odds = round(log(p_2 / (1 - p_2)) - log(p_1 / (1 - p_1)), 2),
      variance = round(1 / (n_1 + alpha) + 1 / (n_2 + alpha), 1),
      z_score = round(log_odds / sqrt(variance), 2) # Standardisierung für Signifikanz
    ) %>% 
    arrange(desc(abs(log_odds)))
}

# Funktionsaufruf
log_odds_pre_vs_post <- compute_log_odds(corpus_preVAR_filtered, corpus_postVAR_filtered, token_col = "token")

# unauffällige Wörter rausfiltern
filtered_log_odds_pre_vs_post <- log_odds_pre_vs_post %>% 
  filter(
    # Fall A: Wort kommt in beiden Korpora vor -> strenger Filter (muss signifikant sein)
    (n_1 > 0 & n_2 > 0 & n_1 + n_2 >= 20 & abs(z_score) >= 2 & abs(log_odds) >= 0.5) |
    # Fall B: Wort kommt nur in einem Korpus vor -> weicher Filter (aber log_odds muss hoch sein)
    ((n_1 == 0 | n_2 == 0) & n_1 + n_2 >= 20 & abs(log_odds) >= 4)
  ) %>% 
  # filter(n_1 + n_2 >= 20 & abs(log_odds) >= 3 & abs(z_score) >= 2) %>% # keine Einträge mit n = 0 in einem Korpus
  arrange(desc(abs(z_score))) # alternativ nach log_odds sortieren

#Manuelle Suche nach auffälligen Begriffen, da mit Spielernamen etc. verunreinigt
View(filtered_log_odsd_pre_vs_post)


# ---- LDA Topic Model Test ----

# Test-Set für LDA Topic Model erstellen:

lda_test_set <- corpus_postVAR_filtered %>% 
  filter(season == "2017/2018") # %>% 
# filter(game_id %in% unique(game_id)[1:9]) # erste 9 Spiele der Saison 17/18

# testen auf doc_id Ebene

dtm_doc <- lda_test_set %>% 
  count(doc_id, token) %>% 
  cast_dtm(document = doc_id, term = token, value = n)

lda_doc <- LDA(dtm_doc, k = 9, control = list(seed = 23))

lda_doc %>% 
  tidy(matrix = "beta") %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 10) %>% 
  ungroup() %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
  labs(title = "Top-Wörter je Topic (doc_id-Ebene)", x = "β", y = NULL)


# testen auf game_id Ebene

dtm_game <- lda_test_set %>% 
  count(game_id, token) %>% 
  cast_dtm(document = game_id, term = token, value = n)

lda_game <- LDA(dtm_game, k = 9, control = list(seed = 23))

# Visualisierung Top-Wörter
lda_game %>% 
  tidy(matrix = "beta") %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 10) %>% 
  ungroup() %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
  labs(title = "Top-Wörter je Topic (game_id-Ebene)", x = "β", y = NULL)

# Visualisierung Wahrscheinlichkeit Topic je Dokument
lda_game %>% 
  tidy(matrix = "gamma") %>% 
  group_by(topic) %>% 
  slice_max(gamma, n = 10) %>% 
  ungroup() %>% 
  mutate(document = reorder_within(document, gamma, topic)) %>% 
  ggplot(aes(gamma, document, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
  labs(title = "Top-Dokumente je Topic (game_id-Ebene)", x = "β", y = NULL)

# Unterschiede der Topic-Verteilung vergleichen

# gamma = Topic-Anteile je Dokument
gamma_doc <- tidy(lda_doc, matrix = "gamma") |>
  mutate(granularity = "doc_id")

gamma_game <- tidy(lda_game, matrix = "gamma") |>
  mutate(granularity = "game_id")

bind_rows(gamma_doc, gamma_game) |>
  ggplot(aes(x = factor(topic), y = gamma, fill = granularity)) +
  geom_boxplot() +
  labs(title = "Topic-Verteilung: doc_id vs. game_id",
       x = "Topic", y = "Gamma", fill = "Ebene")


# ---- Bugfixes ----


# Defekte Dateien finden

is_safe_df <- function(file) {
  df <- tryCatch(readRDS(file), error = function(e) return(NULL))
  if (is.null(df)) return(FALSE)
  if (!is.data.frame(df)) return(FALSE)
  
  # check if any column contains NLP::String objects
  has_bad_column <- any(sapply(df, function(col) inherits(col, "String")))
  return(!has_bad_column)
}

bad_files <- annotation_files_1718_bis_2223[!sapply(annotation_files_1718_bis_2223, is_safe_df)]
bad_files

bad_doc_ids <- tools::file_path_sans_ext(basename(bad_files))

df_to_fix <- df_livetexts_1718_bis_2223 %>%
  filter(doc_ID %in% bad_doc_ids)

annotate_liveticker_batch(
  texts   = df_to_fix$Text,
  doc_ids = df_to_fix$doc_ID,
  output_dir = "annotation_output_1718_bis_2223",
  skip_existing = FALSE  # damit sie sicher neu geschrieben werden
)


#