library(tidyverse)
library(dplyr)
library(ggplot2)
library(scales)
library(multcomp)


# --- Fragestellung: Gibt es den Heimovorteil?
# Vermutung: Mehr Heim- als Auswärtssiege

# import Output-Dateien von datenbereinigung.R

load("./weltfussball_liveticker/df_BL_1718_bis_2223.RData")
dataframe <- df_BL_1718_bis_2223

# Erstellen der Spalte result_category mit Werten "Heimsieg", "Unentschieden" und "Auswärtssieg"
dataframe$result_category <- ifelse(dataframe$Result_Home > dataframe$Result_Away, "Heimsieg",
                                    ifelse(dataframe$Result_Home == dataframe$Result_Away, "Unentschieden",
                                           "Auswärtssieg"))

result_category_counts <- table(dataframe$result_category)
dataframe$result_category <- factor(dataframe$result_category, levels = c("Heimsieg", "Unentschieden", "Auswärtssieg"))

summary_stats <- dataframe %>% 
  group_by(Season) %>% 
  summarize(
    HomeWinPercentage = round(sum(result_category == "Heimsieg") / n() * 100, 2),
    DrawPercentage = round(sum(result_category == "Unentschieden") / n() * 100, 2),
    AwayWinPercentage = round(sum(result_category == "Auswärtssieg") / n() * 100, 2),
    HomeAwayWinRatio = round(sum(result_category == "Heimsieg") / sum(result_category == "Auswärtssieg"), 2)) %>% 
  as.data.frame()


# ---Plotting---

# Vergleich der Verteilung von Heim-, Auswärtssiegen & Unentschieden pro Saison in absoluter Häufigkeit

abbreviations <- c("Heimsieg" = "HS", "Unentschieden" = "U", "Auswärtssieg" = "AS")

ggplot(dataframe, aes(x = result_category, fill = result_category)) +
  geom_bar() +
  labs(title = "Gibt es den Heimvorteil?",
       x = "Kategorie",
       y = "Anzahl") +
  scale_fill_manual(values = c("Heimsieg" = "green", "Unentschieden" = "yellow", "Auswärtssieg" = "red")) +
  scale_x_discrete(limits = c("Heimsieg", "Unentschieden", "Auswärtssieg"), labels = abbreviations) +
  facet_wrap(~Season, scales = "free_x") +
  guides(fill = "none")

dateipfad_1 <- "./plots/heimvorteil_plot.png"
ggsave(dateipfad_1, width = 10, height = 6, units = "in")

# Fazit: deutliche Tendenz zu mehr Heim- als Auswärtssiege im plot und anhand der HomeAwayWinRatio sichtbar


# --- Ergänzung Juli 2025:
# Kann man den Heimvorteil als statistisch signifikant nachweisen?

# Aus summary_stats ablesbar, dass Unentschieden insgesamt nur zu ~25% auftreten
# Unentschieden werden daher für die Tests an dieser Stelle ignoriert

# Aufgrund nur 2 statt 3 Kategorien --> Binomialtest statt Chi-Quadrat-Test
  # Nullhypothese H0: Der Anteil der Heimsiege unter den Siegen ist 50% (kein Heimvorteil)
  # Alternativhypothese H1: Der Anteil der Heimsiege unter den Siegen ist größer als 50%

df_filtered <- dataframe %>% 
  filter(result_category %in% c("Heimsieg", "Auswärtssieg"))

homewins <- sum(df_filtered$result_category == "Heimsieg")
total <- nrow(df_filtered)

binom.test(homewins, total, p = 0.5, alternative = "greater")

# --> Fazit: Ja, der Heimvorteil lässt sich signifikant nachweisen


# GLM zur Modellierung der Heimsieg-Wahrscheinlichkeiten über mehrere Saisons
# --> Wie verändert sich die Wahrscheinlichkeit eines Heimsieges über die Saisons hinweg?

# Vorbereitung binomialer Daten in result_category

df_modified_for_glm <- df_filtered %>% 
  mutate(
    is_home_win = ifelse(result_category == "Heimsieg", 1, 0),
    Season = as.factor(Season)
    # hier evtl. unwichtige Spalten rausfiltern
  )

model <- glm(is_home_win ~ Season, family = binomial, data = df_modified_for_glm)
summary(model)

# aus Log-Odds die Wahrscheinlichkeiten auf Heimsieg pro Saison berechnen
odds_per_season <- data.frame(Season = levels(df_modified_for_glm$Season))

odds_per_season$log_odds <- predict(model, newdata = odds_per_season, type = "link") # type = "response" ist auch möglich und gibt die Wahrscheinlichkeit direkt zurück, aber "link" für Berechnung der Konfidenzintervalle wichtig
odds_per_season$probability <- plogis(odds_per_season$log_odds) # bei type = "response" entfällt diese Zeile
print(odds_per_season)

# das gleiche nochmal, diesmal inkl. Berechnung der Konfidenzintervalle
# 95%-Konfidenzintervall auf der Logit-Skala

odds_per_season <- data.frame(Season = levels(df_modified_for_glm$Season))

pred <- predict(model, newdata = odds_per_season, type = "link", se.fit = TRUE)

odds_per_season <- odds_per_season %>% 
  mutate(
    log_odds = pred$fit,
    se = pred$se.fit,
    lower_logit = log_odds - 1.96 * se,
    upper_logit = log_odds + 1.96 * se,
    probability = plogis(log_odds),
    lower_prob = plogis(lower_logit),
    upper_prob = plogis(upper_logit)
  )

print(odds_per_season)

# Visualisierung der Wahrscheinlichkeiten mit Konfidenzintervallen

ggplot(odds_per_season, aes(x = Season, y = probability)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(
    aes(ymin = lower_prob, ymax = upper_prob),
    width = 0.2,
    linewidth = 0.9,
    color = "black"
  ) +
  geom_text(
    aes(label = scales::percent(probability, accuracy = 0.1)),
    y = 0.77,
    vjust = 0,
    size = 4
  ) +
  labs(
    title = "Geschätzte Heimsieg-Wahrscheinlichkeit pro Saison",
    subtitle = "Mit 95%-Konfidenzintervallen (binomiales GLM)",
    x = "Saison",
    y = "P(Heimsieg)"
  ) +
  ylim(0, 1) +
  theme_minimal(base_size = 13)

dateipfad_2 <- "./plots/heimvorteil_seasons_plot.png"
ggsave(dateipfad_2, width = 8, height = 5, dpi = 300, bg = "white")

# Post-hoc-Test: Weicht die Saison 19/20 auch von den anderen Saisons signifikant ab, nicht nur von der Referenzsaison 17/18?

# "Tukey" -> paarweise Vergleiche, mit Korrektur für Mehrfachtests
tukey_result <- glht(model, linfct = mcp(Season = "Tukey")) # general linear hypothesis test, multiple comparison procedure
summary(tukey_result)

# Interpretation: p-Werte im Tukey-Test sind adjustiert für Mehrfachvergleiche, d.h. streng konservativ
#                 die Saison 19/20 weicht in den Effekten (z value) ab, nach strenger Mehrfachkorrektur aber nicht signifikant
#                 --> deshalb nochmal nur die Saison 19/20 mit den anderen vergleichen

# Gezielter Kontrast der Saison 2019/2020 mit den anderen Saisons

df_modified_for_glm <- df_modified_for_glm %>% 
  mutate(
    season_category = factor(
      ifelse(Season == "2019/2020", "Corona-Saison", "Normal"), 
      levels = c("Normal", "Corona-Saison") # "Normal" als Referenzkategorie für GLM vorbereiten
    )
  )

model_2 <- glm(is_home_win ~ season_category, family = binomial, data = df_modified_for_glm)
summary(model_2)

# Hypothese: Der Heimvorteil hängt nicht von der Saison, sondern eigentlich von den Zuschauerzahlen (ja/nein/reduziert) ab.
#            Als während der Corona-Pandemie keine/weniger Zuschauer zugelassen waren, nahm der Heimvorteil signifikant ab.

# Informationen zur Zuschauerbeschränkung als Spalte public_allowed hinzufügen
df_modified_for_glm <- df_modified_for_glm %>% 
  mutate(Date = as.Date(Date)) %>% # Format des Datums in Date-Format umwandeln
  mutate(public_allowed = TRUE) %>% # erstmal alle Werte auf TRUE setzen
  mutate(
    public_allowed = ifelse(
      (Season == "2019/2020" & Date >= as.Date("2020-03-11")) |
      (Season == "2020/2021"), # alle Spiele der Saison 2020/21 gelten als Geisterspiele, auch wenn einzelne Spiele bis zu 20% Zuschauer zuließen
      FALSE,
      public_allowed
    )
  )

# GLM in Abhängigkeit von public_allowed
model_public_allowed <- glm(is_home_win ~ public_allowed, family = binomial, data = df_modified_for_glm)
summary(model_public_allowed)

# Wahrscheinlichkeiten und Konfidenzintervalle berechnen
odds_public <- data.frame(public_allowed = c(FALSE, TRUE))

pred <- predict(model_public_allowed, newdata = odds_public, type = "link", se.fit = TRUE)

odds_public <- odds_public %>% 
  mutate(
    log_odds = pred$fit,
    se = pred$se.fit,
    lower_logit = log_odds - 1.96 * se,
    upper_logit = log_odds + 1.96 * se,
    probability = plogis(log_odds),
    lower_prob = plogis(lower_logit),
    upper_prob = plogis(upper_logit)
  )

print(odds_public)

# Visualisierung odds_public

ggplot(odds_public, aes(x = public_allowed, y = probability)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(
    aes(ymin = lower_prob, ymax = upper_prob),
    width = 0.2,
    linewidth = 0.9,
    color = "black"
  ) +
  geom_text(
    aes(label = scales::percent(probability, accuracy = 0.1)),
    y = 0.77,
    vjust = 0,
    size = 4
  ) +
  labs(
    title = "Geschätzte Heimsieg-Wahrscheinlichkeit in Abh. von Zuschauern ja/nein",
    subtitle = "Mit 95%-Konfidenzintervallen (binomiales GLM)",
    x = "Zuschauer erlaubt",
    y = "P(Heimsieg)"
  ) +
  ylim(0, 1) +
  theme_minimal(base_size = 13)

dateipfad_3 <- "./plots/heimvorteil_public_plot.png"
ggsave(dateipfad_3, width = 8, height = 5, dpi = 300, bg = "white")

# ggf. am Ende noch Modellvergleich visualisieren mit jeweils Saison, Corona-Beschränkung (oder Nichts?) als Prädiktor
