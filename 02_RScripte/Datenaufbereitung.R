# Datenaufbereitung
library(readxl)
library(dplyr)
library(lubridate)

# ---- Einstellungen ----

# Referenzdatum für Alters/Jahresvariablen
referenzdatum <- as.Date("2025-01-01")

# ---- Laden von Rohdaten und Mappings ----

# Bestandsdaten einlesen und obersten 4 Zeilen löschen
bestandsdaten <- read_excel("01_Daten/MV_Kfz-Bestand_052025.xlsx", skip = 4, guess_max = 10000)

# Mapping zur Datenaufbereitung einlesen
mapping <- read_excel("01_Daten/mapping_datenaufbereitung.xlsx")


# ---- Initiale Datenbereinigung ----

# Ableitung der Variablen für die Bestimmung des TP auf Basis des GDV
df <- bestandsdaten %>% 
  mutate(
    
    # Differenziertes Nutzeralter
    
    # Kilometerleistung
    FL <- KM, 
    # -> ToDo: noch als numeric? Auf STufen von GDV bringen oder später?
    
    # Fahrzeugalter bei Erwerb
    # Differenz in Monaten; 0 wenn Erwerb leer ist
    FZAE_monate_exakt = case_when(
      is.na(ERWERB) ~ 0L,
      TRUE ~ interval(ymd(as.character(ERSTZULASS)), ymd(as.character(ERWERB))) %/% months(1)
    ),
    # Gruppierung in Schrittweiten
    FZAE = cut(
      FZAE_monate_exakt,
      breaks = c(-Inf, 11, 23, 35, 59, 119, Inf), # TODO: Schrittweiten anpassen
      labels = c("0–11", "12–23", "24–35", "36–59", "60–119", "120+"),
      right = TRUE
    ),
    
    # Fahrzeughalter
    
    # Regionalklasse
    
    # Tarifgruppe
    TARGRH_Buchstabe = substr(`TARGR-H`, 1, 1),
    TARIF = case_when(
      TARGRH_Buchstabe == "A" ~ 50,
      TARGRH_Buchstabe == "B" ~ 60,
      TARGRH_Buchstabe == "D" ~ 90, # ToDo absprechen, ob die Zuordnung stimmt
      TARGRH_Buchstabe == "N" ~ 10,
      TARGRH_Buchstabe == "R" ~ 10,
      TRUE ~ NA_integer_  # Fallback für andere oder fehlende
    ),
    
    # Typklasse / Wagnisstärke / zulässige Gesamtmasse
    WSTK_KH = case_when(
      # Typklasse falls WKZ 112
      WKZ %in% c("112") ~ `TYPKL-KRH`, 
      # Kategorisierung anhand von Motorstärke, falls WKZ 251
      WKZ %in% c("251") ~ cut(as.numeric(`WT-WST-KRH`), 
                              breaks = c(-Inf, 30, 38, 45, 52, 63, 76, 85, 97, 150, 213, Inf),
                              labels = as.character(c(1,30, 38, 45, 52, 63, 76, 85, 97, 150, 213)),
                              right = FALSE),
      # Kategorisierung anhand von Motorstärke, falls WKZ 351
      WKZ %in% c("351") ~ cut(as.numeric(`WT-WST-KRH`), 
                              breaks = c(-Inf, 52, 76, 97, 150, 213, 267, 291, 318, Inf),
                              labels = as.character(c(1,52, 76, 97, 150, 213, 267, 291, 318)),
                              right = FALSE),
      # Kategorisierung anhand von zulässiger Gesamtmasse, falls WKZ 581
      # ToDo: anpassen
      WKZ %in% c("581") ~ cut(as.numeric(`WT-WST-KRH`), 
                              breaks = c(-Inf, 52, 76, 97, 150, 213, 267, 291, 318, Inf),
                              labels = as.character(c(1,52, 76, 97, 150, 213, 267, 291, 318)),
                              right = FALSE),
      TRUE ~ "NA"
    ),
    
    # Deckung TK
    # zurodnung von fällen die nicht beim gdv abgebildet sind zu den am besten passenden kategorien
    DECKUNG = case_when(
      `DA-K` %in% c("76", "77") ~ "74",
      TRUE ~ `DA-K`
    ),
    
    # Nutzerkreis
    
    # Selbstbehalt VK und TK in VK
    VK_Sb = case_when(
      `DA-K` %in% c("21") ~ 1, # kein VK SB
      `DA-K` %in% c("32", "42") ~ 2, # 150 VK SB
      `DA-K` %in% c("33", "43", "63") ~ 3, # 300 VK SB
      `DA-K` %in% c("34", "44", "64") ~ 4, # 500 VK SB
      `DA-K` %in% c("36", "46", "66", "38", "68") ~ 6, # 1000 VK SB; 1500 VK SB wird hier ebenfalls zugeordnet
      `DA-K` %in% c("37", "47", "67") ~ 7,
      TRUE ~ NA
    ),
    TK_Sb = case_when(
      `DA-K` %in% c("32", "33", "34", "35", "36", "37", "38") ~ 3, # 150 TK SB oder höher
      `DA-K` %in% c("21", "42", "43", "44", "46", "47") ~ 4, # TK ohne SB
      `DA-K` %in% c("63", "64", "66", "67", "68") ~ 6, # Falls VK SB gleich TK SB
      TRUE ~ NA
      # falls eine Kombination aus VK und TK Selbstbehalt in mehrere Gruppen fällt z,B, 150/150 SB wurde die erste Gruppe gewählt
    ),
    
    # Aufbauart
    AUFBAU = case_when(
      `SC-AUFBAUART-KR` %in% c("91", "92") ~ "90",
      TRUE ~ `SC-AUFBAUART-KR`
    )
    
    # Gefahrgut, keine Umformung notwendig
  )

# ---- Join Prämien nach Stufung ----


# ---- Ableitung und Umformungen von Variablen ----


# ---- Anwendung TP Funktion ----


# ---- Speichern der aufbereiteten daten ----
