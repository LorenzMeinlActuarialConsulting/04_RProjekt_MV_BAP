# 📦 Benötigte Pakete
library(mgcv)
library(writexl)  # für Excel-Export
library(dplyr)

# 🧠 Beispielmodell laden oder selbst definieren:
# model <- gam(y ~ s(x1) + Geschlecht + Region, data = df)
# Stornomodell -> später löschen
model <- readRDS("01_Daten/03_model_2025-06-05.rds")

# 🧱 1. Variable: alle Modellterme
variablen <- attr(terms(model), "term.labels")

# 🧱 2. Faktor-Levels extrahieren
model_factors <- model.frame(model) %>%
  select(where(is.factor))

faktoren_liste <- lapply(model_factors, levels)

# 🧾 3. Design-Matrix für Dummy-Namen
dummy_spalten <- colnames(model.matrix(model))

# 🧪 4. Ausgabe vorbereiten

## A. Variable-Tabelle
df_variablen <- data.frame(Variable = variablen)

## B. Faktor-Ausprägungen als Liste in Data Frame
df_auspraegungen <- do.call(rbind, lapply(names(faktoren_liste), function(name) {
  data.frame(
    Variable = name,
    Auspraegung = faktoren_liste[[name]],
    stringsAsFactors = FALSE
  )
}))

## C. Dummy-Spaltennamen
df_dummies <- data.frame(Dummy_Kodierung = dummy_spalten)

# 💾 5. Export nach Excel
write_xlsx(
  list(
    "Modell-Variablen" = df_variablen,
    "Faktor-Ausprägungen" = df_auspraegungen,
    "Design-Matrix-Spalten" = df_dummies
  ),
  path = "modellstruktur_export.xlsx"
)


variablen_storno <- attr(terms(model), "term.labels")
print(variablen_storno)
