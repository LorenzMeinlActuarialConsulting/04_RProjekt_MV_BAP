# HIlfsfunktion die anhand eines Mappings neu Spalten und Ausprägungen erstellt
mappe_daten_mit_mapping <- function(df, mapping_df) {
  merkmale <- unique(mapping_df$Merkmal)
  
  for (merk in merkmale) {
    # Filter für dieses Merkmal
    map_df <- mapping_df %>% filter(Merkmal == merk)
    zielspalte <- unique(map_df$Zielspalte)
    
    if (length(zielspalte) != 1) {
      stop(paste("Mehrdeutiger Zielspaltenname für Merkmal:", merk))
    }
    
    # Join & neue Spalte erzeugen
    df <- df %>%
      left_join(map_df, by = setNames("Original", merk)) %>%
      rename(!!zielspalte := Neu) %>%
      select(-Merkmal, -Zielspalte)
  }
  
  return(df)
}