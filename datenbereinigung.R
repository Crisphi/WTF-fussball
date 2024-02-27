library(tidyverse)

dataframe_files <- c(
  "./weltfussball_liveticker/df_weltfussball_1718_mit_zeit.RData",
  "./weltfussball_liveticker/df_weltfussball_1819_mit_zeit.RData",
  "./weltfussball_liveticker/df_weltfussball_1920_mit_zeit.RData",
  "./weltfussball_liveticker/df_weltfussball_2021_mit_zeit.RData",
  "./weltfussball_liveticker/df_weltfussball_2122_mit_zeit.RData",
  "./weltfussball_liveticker/df_weltfussball_2223_mit_zeit.RData"
)

dataframes <- list()

for(datei in dataframe_files) {
  load(datei)
  dataframes[[datei]] <- df_weltfussball_season_mit_zeit
}

clean_dataframe_names <- list(
  "./weltfussball_liveticker/df_BL_1718.RData",
  "./weltfussball_liveticker/df_BL_1819.RData",
  "./weltfussball_liveticker/df_BL_1920.RData",
  "./weltfussball_liveticker/df_BL_2021.RData",
  "./weltfussball_liveticker/df_BL_2122.RData",
  "./weltfussball_liveticker/df_BL_2223.RData"
)

for(i in 1:length(dataframes)) {
  
  #Title enthält Ergebnis, Saison, Wettbewerb und Spieltag, die in eigene Spalten überführt werden sollen
  dataframes[[i]] <- dataframes[[i]] %>% 
    mutate(Result = str_extract(Title, "\\d+:\\d+")) %>% 
    mutate(Season = str_extract(Title, "\\d{4}/\\d{4}")) %>% 
    mutate(Competition = str_extract(Title, "(?<=\\().+?(?=\\d{4}/\\d{4})")) %>% 
    mutate(Matchday = str_extract(Title, "\\d+\\. Spieltag")) %>% 
    mutate(Date = str_extract(Date, "\\d+-\\d+-\\d+"))
  
  #Spalten-Reihenfolge neu ordnen (ohne Title)
  neue_reihenfolge <- c("Team1", "Team2", "Matchday", "Competition", "Season", "Date", "Kickoff", "Result", "Text")
  dataframes[[i]] <- dataframes[[i]][, neue_reihenfolge]
  
  str(dataframes[[i]])
  
  #save.image()
  #save(dataframes[[i]], file = clean_dataframe_names[[i]])
}

df_BL_1718 <- dataframes[[1]]
df_BL_1819 <- dataframes[[2]]
df_BL_1920 <- dataframes[[3]]
df_BL_2021 <- dataframes[[4]]
df_BL_2122 <- dataframes[[5]]
df_BL_2223 <- dataframes[[6]]