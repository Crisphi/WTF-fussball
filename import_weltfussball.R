library(tidyverse)
library(readxl)
library(XML)
library(xml2)
library(methods)
library(purrr)

#Skript zum Import von Saison-Korpora als xml-Files von weltfussball.de

xml_weltfussball_1718 <- read_xml("./weltfussball_liveticker/bundesliga-2017-2018.xml")

extrahiere_metadaten <- function(text_element) {
  list(
    Title = xml_text(xml_find_first(text_element, ".//title")),
    Team1 = xml_text(xml_find_first(text_element, ".//team1")),
    Team2 = xml_text(xml_find_first(text_element, ".//team2")),
    Date = xml_text(xml_find_first(text_element, ".//date")),
    Kickoff = xml_text(xml_find_first(text_element, ".//kickoff"))
  )
}

metadaten_liste <- map(xml_find_all(xml_weltfussball_1718, "//text"), extrahiere_metadaten)
titel_liste <- sapply(metadaten_liste, function(x) x$Title, simplify = FALSE)
datum_liste <- sapply(metadaten_liste, function(x) x$Date, simplify = FALSE)
team1_liste <- sapply(metadaten_liste, function(x) x$Team1, simplify = FALSE)
team2_liste <- sapply(metadaten_liste, function(x) x$Team2, simplify = FALSE)
kickoff_liste <- sapply(metadaten_liste, function(x) x$Kickoff, simplify = FALSE)

extrahiere_texte <- function(text_element) {
  p_tags <- xml_find_all(text_element, ".//p")
  reversed_p_tags <- rev(p_tags)
  pasted_text <- paste(xml_text(reversed_p_tags), collapse = " ")
  return(pasted_text)
}

extrahiere_texte_mit_zeit <- function(text_element) {
  p_tags <- xml_find_all(text_element, ".//p | .//time")
  reversed_p_tags <- rev(p_tags)
  pasted_text <- paste(xml_text(reversed_p_tags), collapse = "||")
  return(pasted_text)
}

texte_liste <- map(xml_find_all(xml_weltfussball_1718, "//text"), extrahiere_texte)
texte_liste_mit_zeit <- map(xml_find_all(xml_weltfussball_1718, "//text"), extrahiere_texte_mit_zeit)

df_weltfussball_1718 <- data.frame(
  Title = unlist(titel_liste),
  Date = unlist(datum_liste),
  Kickoff = unlist(kickoff_liste),
  Team1 = unlist(team1_liste),
  Team2 = unlist(team2_liste),
  Text = unlist(texte_liste)
)

df_weltfussball_1718_mit_zeit <- data.frame(
  Title = unlist(titel_liste),
  Date = unlist(datum_liste),
  Kickoff = unlist(kickoff_liste),
  Team1 = unlist(team1_liste),
  Team2 = unlist(team2_liste),
  Text = unlist(texte_liste_mit_zeit)
)

#View(df_weltfussball_1718)
#View(df_weltfussball_1718_mit_zeit)

save(df_weltfussball_1718, file = "./weltfussball_liveticker/df_weltfussball_1718.RData")
save(df_weltfussball_1718_mit_zeit, file = "./weltfussball_liveticker/df_weltfussball_1718_mit_zeit.RData")
