library(tidyverse)
library(readxl)
library(XML)
library(xml2)
library(methods)
library(purrr)

#Skript zum Import von Saison-Korpora als xml-Files von weltfussball.de

#---Variablen---

#Dateipfade der Rohdaten
files <- list(
    "./weltfussball_liveticker/bundesliga-2017-2018.xml", 
    "./weltfussball_liveticker/bundesliga-2018-2019.xml", 
    "./weltfussball_liveticker/bundesliga-2019-2020.xml", 
    "./weltfussball_liveticker/bundesliga-2020-2021.xml", 
    "./weltfussball_liveticker/bundesliga-2021-2022.xml", 
    "./weltfussball_liveticker/bundesliga-2022-2023.xml"
  )

#Dateipfade zum Export der Dataframes

#Text ohne Zeitmarken
fnames <- list(
  "./weltfussball_liveticker/df_weltfussball_1718.RData",
  "./weltfussball_liveticker/df_weltfussball_1819.RData",
  "./weltfussball_liveticker/df_weltfussball_1920.RData",
  "./weltfussball_liveticker/df_weltfussball_2021.RData",
  "./weltfussball_liveticker/df_weltfussball_2122.RData",
  "./weltfussball_liveticker/df_weltfussball_2223.RData"
)

#Text mit Zeitmarken
fnames_mit_zeit <- list(
  "./weltfussball_liveticker/df_weltfussball_1718_mit_zeit.RData",
  "./weltfussball_liveticker/df_weltfussball_1819_mit_zeit.RData",
  "./weltfussball_liveticker/df_weltfussball_1920_mit_zeit.RData",
  "./weltfussball_liveticker/df_weltfussball_2021_mit_zeit.RData",
  "./weltfussball_liveticker/df_weltfussball_2122_mit_zeit.RData",
  "./weltfussball_liveticker/df_weltfussball_2223_mit_zeit.RData"
)

#---Funktionen---

#Funktion zum extrahieren der Metadaten aus der XML-Struktur der Metadaten pro Spiel
extrahiere_metadaten <- function(text_element) {
  list(
    Title = xml_text(xml_find_first(text_element, ".//title")),
    Team1 = xml_text(xml_find_first(text_element, ".//team1")),
    Team2 = xml_text(xml_find_first(text_element, ".//team2")),
    Date = xml_text(xml_find_first(text_element, ".//date")),
    Kickoff = xml_text(xml_find_first(text_element, ".//kickoff"))
  )
}

#Funktion zum extrahieren der einzelnen Absätze der Livekommentare aus der XML Struktur pro Spiel ohne Zeitmarken
extrahiere_texte <- function(text_element) {
  p_tags <- xml_find_all(text_element, ".//p")
  #rev() bringt die Livekommentare in chronologische Reihenfolge zum Spielverlauf
  reversed_p_tags <- rev(p_tags)
  #Konkateniert die einzelenen Absätze eines Spiels zu einem String
  pasted_text <- paste(xml_text(reversed_p_tags), collapse = " ")
  return(pasted_text)
}

#Funktion zum extrahieren der einzelnen Absätze der Livekommentare aus der XML Struktur pro Spiel mit Zeitmarken
extrahiere_texte_mit_zeit <- function(text_element) {
  p_tags <- xml_find_all(text_element, ".//p | .//time")
  reversed_p_tags <- rev(p_tags)
  pasted_text <- paste(xml_text(reversed_p_tags), collapse = "||")
  return(pasted_text)
}

#---Ausführende Befehle---

#For-Schleife iteriert über alle angegebenen Rohdaten für einzelne Saisons und verarbeitet diese nacheinander 

for(i in 1:length(files)){
  print(i)
  print(files[[i]])
  #Einlesen der XML-File
  xml_weltfussball_season <- read_xml(files[[i]])
  
  #Extrahiere Metadaten für jedes Spiel der Saison
  metadaten_liste <- map(xml_find_all(xml_weltfussball_season, "//text"), extrahiere_metadaten)
  
  #Erstelle listen für jedes Metadatum aus den extrahierten Daten pro Spiel
  titel_liste <- sapply(metadaten_liste, function(x) x$Title, simplify = FALSE)
  datum_liste <- sapply(metadaten_liste, function(x) x$Date, simplify = FALSE)
  team1_liste <- sapply(metadaten_liste, function(x) x$Team1, simplify = FALSE)
  team2_liste <- sapply(metadaten_liste, function(x) x$Team2, simplify = FALSE)
  kickoff_liste <- sapply(metadaten_liste, function(x) x$Kickoff, simplify = FALSE)
  
  #Extrahiere Textdaten der Livekommentare für jedes Spiel der Saison
  texte_liste <- map(xml_find_all(xml_weltfussball_season, "//text"), extrahiere_texte)
  texte_liste_mit_zeit <- map(xml_find_all(xml_weltfussball_season, "//text"), extrahiere_texte_mit_zeit)
  
  df_weltfussball_season <- data.frame(
    Title = unlist(titel_liste),
    Date = unlist(datum_liste),
    Kickoff = unlist(kickoff_liste),
    Team1 = unlist(team1_liste),
    Team2 = unlist(team2_liste),
    Text = unlist(texte_liste)
  )
  
  df_weltfussball_season_mit_zeit <- data.frame(
    Title = unlist(titel_liste),
    Date = unlist(datum_liste),
    Kickoff = unlist(kickoff_liste),
    Team1 = unlist(team1_liste),
    Team2 = unlist(team2_liste),
    Text = unlist(texte_liste_mit_zeit)
  )
  
  head(df_weltfussball_season)
  #View(df_weltfussball_season_mit_zeit)
  
  save(df_weltfussball_season, file = fnames[[i]])
  save(df_weltfussball_season_mit_zeit, file = fnames_mit_zeit[[i]])
  
  print("done")
}

