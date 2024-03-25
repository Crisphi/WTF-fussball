library(dplyr)
library(XML)
library(xml2)
library(readxl)

#Skript zum Import von Saison-Korpora als xml-Files von weltfussball.de
#Output: Dateien mit Dataframes, pro Saison ein df mit je einer Zeile pro p-Tag mit zugehörigem time-Tag

#---Variablen---

#Dateipfade der Rohdaten
files <- list(
  "./weltfussball_liveticker/bundesliga-2011-2012.xml",
  "./weltfussball_liveticker/bundesliga-2012-2013.xml", 
  "./weltfussball_liveticker/bundesliga-2013-2014.xml", 
  "./weltfussball_liveticker/bundesliga-2014-2015.xml", 
  "./weltfussball_liveticker/bundesliga-2015-2016.xml", 
  "./weltfussball_liveticker/bundesliga-2016-2017.xml",
  
  "./weltfussball_liveticker/bundesliga-2017-2018.xml", 
  "./weltfussball_liveticker/bundesliga-2018-2019.xml", 
  "./weltfussball_liveticker/bundesliga-2019-2020.xml", 
  "./weltfussball_liveticker/bundesliga-2020-2021.xml", 
  "./weltfussball_liveticker/bundesliga-2021-2022.xml", 
  "./weltfussball_liveticker/bundesliga-2022-2023.xml"
)

#Dateipfade zum Export der Dataframes

fnames <- list(
  "./weltfussball_liveticker/df_weltfussball_1112_pt.RData",
  "./weltfussball_liveticker/df_weltfussball_1213_pt.RData",
  "./weltfussball_liveticker/df_weltfussball_1314_pt.RData",
  "./weltfussball_liveticker/df_weltfussball_1415_pt.RData",
  "./weltfussball_liveticker/df_weltfussball_1516_pt.RData",
  "./weltfussball_liveticker/df_weltfussball_1617_pt.RData",
  
  "./weltfussball_liveticker/df_weltfussball_1718_pt.RData",
  "./weltfussball_liveticker/df_weltfussball_1819_pt.RData",
  "./weltfussball_liveticker/df_weltfussball_1920_pt.RData",
  "./weltfussball_liveticker/df_weltfussball_2021_pt.RData",
  "./weltfussball_liveticker/df_weltfussball_2122_pt.RData",
  "./weltfussball_liveticker/df_weltfussball_2223_pt.RData"
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

#---Skript---

for (i in 1:length(files)) {
  
  print("File start")
  
  xml_file <- read_xml(files[[i]])
  
  text_tags <- xml_find_all(xml_file, "//text")
  
  df_gesamt <- data.frame()
  
  iteration_count <- 0
  
  for (text_tag in text_tags) {
    
    iteration_count <- iteration_count + 1
    
    metadata <- extrahiere_metadaten(text_tag)
    
    p_tags <- xml_find_all(text_tag, ".//p")
    p_text <- xml_text(p_tags)
    
    time_tags <- xml_find_all(text_tag, ".//time")
    time_text <- xml_text(time_tags)
    
    df <- data.frame(Title = metadata$Title,
                     Date = metadata$Date,
                     Kickoff = metadata$Kickoff,
                     Team1 = metadata$Team1,
                     Team2 = metadata$Team2,
                     Time = time_text,
                     Text = p_text)
    
    df_gesamt <- bind_rows(df_gesamt, df)
    cat(iteration_count, " done\n")
    
  }
  
  save(df_gesamt, file = fnames[[i]])
  
  print("File done")
  
}
