# Datenaufbereitung
library(readxl)
library(dplyr)

# ---- Laden von Rohdaten und Mappings ----

# Bestandsdaten einlesen und obersten 4 Zeilen löschen
bestandsdaten <- read_excel("01_Daten/MV_Kfz-Bestand_052025.xlsx", skip = 4)

# Mapping zur Datenaufbereitung einlesen
mapping <- read_excel("01_Daten/mapping_datenaufbereitung.xlsx")


# ---- Initiale Datenbereinigung ----


# ---- Join Prämien nach Stufung ----


# ---- Ableitung und Umformungen von Variablen ----


# ---- Anwendung TP Funktion ----


# ---- Speichern der aufbereiteten daten ----
