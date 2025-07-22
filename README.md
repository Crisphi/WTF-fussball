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

   Extrahiert Metadaten aus "Title" als eigene Spalten in passendem Format
   Fasst die Meta- und Textdaten in zwei Dataframes zusammen, jeweils für 11/12 - 16/17 und 17/18 - 22/23
   Zwei Dataframes deshalb, weil der VAR zur Saison 17/18 eingeführt wurde und Veränderungen im Text im Zusammenhang mit dem VAR zu erwarten sind
   Vergleich der beiden Zeiträume dadurch möglich
   
4. heimvorteil.R

   Skript mit einer statistischen Auwertung der Metadaten zur Heimvorteil-Hypothese, basierend auf den Saisons 2017/18 bis 2022/23
   Beinhaltet Binomialtest, versch. GLM-Modelle (Prädiktoren: Saison & Publikum), Likelihood-Ratio-Test und Visualisierungen
   
5. textverarbeitung.R bzw. *NEU* 2025_textverarbeitung.R

   Bereinigt Liveticker-Daten zur Vorbereitung der Annotation, entfernt oder ersetzt irrelevante Elemente (z.B. Spielende, Einwechslung, Minutenangaben, Spielstände, etc.)
   Satzannotation, Tokenisierung, POS-Annotation
   Korpuserstellung, Preprocessing
   Document Feature Matrix, word count, wordcloud
   Topic Modelling
   Generelle Korpus Statistiken