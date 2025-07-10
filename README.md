# WTF-fussball

Ein Repository für das abschließende Projekt im Seminar Wörter-Texte & Frequenzen (WTF) im Wintersemester 2023-24 an der FAU. 
von Cristian Ortega Singer und Jan-Oliver Reincke

Ziel des Projekts ist die Anwendung statistischer Analysemethoden in R anhand entsprechender Fragestellungen auf ein selbst gewähltes Korpus von Textdaten.
Das Korpus enthält Text- und Metadaten aus Livetickern von weltfussball.de aus den Saisons 17/18 bis 22/23 der Herrenfußball-Bundesliga.

Die Daten von weltfussball.de wurden mithilfe eines Scraping-Skripts von Prof. Simon Meier-Vieracker (aka fußballinguist) als XML-Dateien extrahiert.
  Quelle: https://github.com/fussballlinguist/livetext


Reihenfolge der Skripte:

1. weltfussball_livetext.pl

   Perl-Skript von Prof. Simon-Meier Vieracker
   extrahiert die Liveticker-Daten der Bundelsiga-Ticker von www.weltfussball.de
   für die Saisons 11/12 - 22/23 jeweils ausgeführt (line 24 jeweils angepasst)


2. import_weltfussball_as_single_entries.R

   R-Skript, das die Daten aus den XML-Files aus 1. zu extrahieren und strukturiert als Dataframe aufzubereiten
   import_weltfussball.R war das ursprüngliche Skript, in dem die Textdaten pro Spiel zusammengefasst waren und nicht als eigene Einträge im df aufgeführt wurden
   
3. datenbereinigung.R

   ...
   
4. textverarbeitung.R

   ...