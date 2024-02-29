library(tidyverse)
library(ggplot2)

#import Output-Dateien von datenbereinigung.R

load("./weltfussball_liveticker/df_BL_1718_bis_2223.RData")
dataframe <- df_BL_1718_bis_2223

#Erstellen der Spalte result_category mit Werten "Heimsieg", "Unentschieden" und "Auswärtssieg"
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


#---Plotting---

#Vergleich der Verteilung von Heim-, Auswärtssiegen & Unentschieden pro Saison in absoluter Häufigkeit

abbreviations <- c("Heimsieg" = "HS", "Unentschieden" = "U", "Auswärtssieg" = "AS")

ggplot(dataframe, aes(x = result_category, fill = result_category)) +
  geom_bar() +
  labs(title = "Gibt es den Heimvorteil?",
       x = "Kategorie",
       y = "Anzahl") +
  scale_fill_manual(values = c("Heimsieg" = "green", "Unentschieden" = "yellow", "Auswärtssieg" = "red")) +
  scale_x_discrete(limits = c("Heimsieg", "Unentschieden", "Auswärtssieg"), labels = abbreviations) +
  facet_wrap(~Season, scales = "free_x") +
  guides(fill = FALSE)

dateipfad <- "./plots/heimvorteil_plot.png"
ggsave(dateipfad, width = 10, height = 6, units = "in")
