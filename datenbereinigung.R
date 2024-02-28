library(tidyverse)

#dataframes importieren (Output-files von import_weltfussball.R)

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

#Dataframes bereinigen

for(i in 1:length(dataframes)) {
  
  #Title enthält Ergebnis, Saison, Wettbewerb und Spieltag, die in eigene Spalten überführt werden sollen
  dataframes[[i]] <- dataframes[[i]] %>% 
    mutate(Result = str_extract(Title, "\\d+:\\d+")) %>% 
    separate(Result, c("Result_Home", "Result_Away"), sep=":", remove=FALSE, convert=TRUE) %>% 
    mutate(Season = str_extract(Title, "\\d{4}/\\d{4}")) %>% 
    mutate(Competition = str_extract(Title, "(?<=\\().+?(?=\\d{4}/\\d{4})")) %>% 
    mutate(Matchday = str_extract(Title, "\\d+\\. Spieltag")) %>% 
    mutate(Date = str_extract(Date, "\\d+-\\d+-\\d+")) %>%
    
    #um Teams später als Factors verwenden zu können
    mutate_at(vars(Team1,Team2), factor)
  
  #Spalten-Reihenfolge neu ordnen (ohne Title)
  neue_reihenfolge <- c("Team1", "Team2", "Matchday", "Competition", "Season", "Date", "Kickoff", "Result", "Result_Home", "Result_Away", "Text")
  dataframes[[i]] <- dataframes[[i]][, neue_reihenfolge]
}

#bereinigte Dataframes speichern

df_BL_1718 <- dataframes[[1]]
save(df_BL_1718, file = "./weltfussball_liveticker/df_BL_1718.RData")
df_BL_1819 <- dataframes[[2]]
save(df_BL_1819, file = "./weltfussball_liveticker/df_BL_1819.RData")
df_BL_1920 <- dataframes[[3]]
save(df_BL_1920, file = "./weltfussball_liveticker/df_BL_1920.RData")
df_BL_2021 <- dataframes[[4]]
save(df_BL_2021, file = "./weltfussball_liveticker/df_BL_2021.RData")
df_BL_2122 <- dataframes[[5]]
save(df_BL_2122, file = "./weltfussball_liveticker/df_BL_2122.RData")
df_BL_2223 <- dataframes[[6]]
save(df_BL_2223, file = "./weltfussball_liveticker/df_BL_2223.RData")

#Zusammenfügen der 6 Saisons zu einem Dataframe

df_BL_1718_bis_2223 <- bind_rows(df_BL_1718, df_BL_1819, df_BL_1920,
                                 df_BL_2021, df_BL_2122, df_BL_2223)
save(df_BL_1718_bis_2223, file = "./weltfussball_liveticker/df_BL_1718_bis_2223")


#-------------------------------------------------------------------------------

#dataframes importieren (Output-files von import_weltfussball_as_single_entries.R)

pt_df_files <- c(
  "./weltfussball_liveticker/df_weltfussball_1718_pt.RData",
  "./weltfussball_liveticker/df_weltfussball_1819_pt.RData",
  "./weltfussball_liveticker/df_weltfussball_1920_pt.RData",
  "./weltfussball_liveticker/df_weltfussball_2021_pt.RData",
  "./weltfussball_liveticker/df_weltfussball_2122_pt.RData",
  "./weltfussball_liveticker/df_weltfussball_2223_pt.RData"
)

pt_dataframes <- list()
for(datei in pt_df_files) {
  load(datei)
  pt_dataframes[[datei]] <- df_gesamt
}

#Dataframes bereinigen

for(i in 1:length(pt_dataframes)) {
  
  #Title enthält Ergebnis, Saison, Wettbewerb und Spieltag, die in eigene Spalten überführt werden sollen
  pt_dataframes[[i]] <- pt_dataframes[[i]] %>% 
    mutate(Result = str_extract(Title, "\\d+:\\d+")) %>% 
    separate(Result, c("Result_Home", "Result_Away"), sep=":", remove=FALSE, convert=TRUE) %>% 
    mutate(Season = str_extract(Title, "\\d{4}/\\d{4}")) %>% 
    mutate(Competition = str_extract(Title, "(?<=\\().+?(?=\\d{4}/\\d{4})")) %>% 
    mutate(Matchday = str_extract(Title, "\\d+\\. Spieltag")) %>% 
    mutate(Date = str_extract(Date, "\\d+-\\d+-\\d+")) %>%
    
    #um Teams später als Factors verwenden zu können
    mutate_at(vars(Team1,Team2), factor)
  
  #Spalten-Reihenfolge neu ordnen (ohne Title)
  neue_reihenfolge <- c("Team1", "Team2", "Matchday", "Competition", "Season", "Date", "Kickoff", "Result", "Time", "Text")
  pt_dataframes[[i]] <- pt_dataframes[[i]][, neue_reihenfolge]
}

#bereinigte Dataframes speichern

df_BL_1718_pt <- pt_dataframes[[1]]
save(df_BL_1718_pt, file = "./weltfussball_liveticker/df_BL_1718_pt.RData")
df_BL_1819_pt <- pt_dataframes[[2]]
save(df_BL_1819_pt, file = "./weltfussball_liveticker/df_BL_1819_pt.RData")
df_BL_1920_pt <- pt_dataframes[[3]]
save(df_BL_1920_pt, file = "./weltfussball_liveticker/df_BL_1920_pt.RData")
df_BL_2021_pt <- pt_dataframes[[4]]
save(df_BL_2021_pt, file = "./weltfussball_liveticker/df_BL_2021_pt.RData")
df_BL_2122_pt <- pt_dataframes[[5]]
save(df_BL_2122_pt, file = "./weltfussball_liveticker/df_BL_2122_pt.RData")
df_BL_2223_pt <- pt_dataframes[[6]]
save(df_BL_2223_pt, file = "./weltfussball_liveticker/df_BL_2223_pt.RData")

#Zusammenfügen der 6 Saisons zu einem Dataframe

df_BL_1718_bis_2223_pt <- bind_rows(df_BL_1718_pt, df_BL_1819_pt, df_BL_1920_pt,
                                 df_BL_2021_pt, df_BL_2122_pt, df_BL_2223_pt)
save(df_BL_1718_bis_2223_pt, file = "./weltfussball_liveticker/df_BL_1718_bis_2223_pt")

print("done")