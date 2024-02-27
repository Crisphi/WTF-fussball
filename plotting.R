library(tidyverse)
library(wordcloud2)
library(ggwordcloud)
library(psych)
library(GGally)
library(Hmisc)
library(ggcorrplot)
library(ggalt)
library(readxl)
library(scales)
library(zipfR)

# Boxplot von Toren der Heim-Teams
# TODO: Color funktioniert noch nicht so wie wir das wollen
df_weltfussball_season_mit_zeit %>%
  ggplot(aes(x = Team1, y = Result_Home, colour = "red")) +
  geom_boxplot(outlier.alpha = .5) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0))

# Boxplot von Toren der Away-Teams
df_weltfussball_season_mit_zeit %>%
  ggplot(aes(x = Team2, y = Result_Away, colour = "blue")) +
  geom_boxplot(outlier.alpha = .5) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0))

ggpairs(df_weltfussball_season_mit_zeit |> select(Result_Home, Result_Away))