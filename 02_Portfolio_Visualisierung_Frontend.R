# libraries ladens
library(tidyverse)  # Beinhaltet ggplot2 (Plotting), dplyr (Datenmanipulation), readr (CSV-Import)
library(scales)     # für bessere Achsenbeschriftungen
library(ggrepel)    # Text und Plots separat darstellen (keine visuelle Überschneidung)
library(viridis)    # erweiterte Farbpaletten
library(readxl)     # Excel-Reader

# Daten laden
df_weights <- read_csv2("data/Portfolio_Gewichte_Master.csv")
df_summary <- read_csv2("data/Portfolio_Summary_Master.csv")
df_vola <- read_csv2("data/Portfolio_Volatilitat_Master.csv")
df_momentum <- read_csv2("data/Portfolio_Momentum_Master.csv")
df_expected_returns <- read_csv2("data/Portfolio_ExpectedReturns_Master.csv")
df_expected_returns$Jahr <- as.character(df_expected_returns$Jahr)
meta_2010 <- read_excel("data/S&P500 FROM 2010 TO 2025.xlsx", sheet = "2010 META")
meta_2015 <- read_excel("data/S&P500 FROM 2010 TO 2025.xlsx", sheet = "2015 META")
meta_2020 <- read_excel("data/S&P500 FROM 2010 TO 2025.xlsx", sheet = "2020 META")
meta_2025 <- read_excel("data/S&P500 FROM 2010 TO 2025.xlsx", sheet = "2025 META")

# 'X(MV)~USD in MV umbenennen (Sonderzeichen Problem)
# "Jahr"-Spalte hinzufügen für das spätere Zuordnen nach dem merge (+ numeric erzwingen wg NAs)
meta_2010 <- meta_2010 %>% 
  rename(MV = `X(MV)~USD`) %>% 
  mutate(Jahr = "2010", MV = as.numeric(MV), MTBV = as.numeric(MTBV))
meta_2015 <- meta_2015 %>% 
  rename(MV = `X(MV)~USD`) %>% 
  mutate(Jahr = "2015", MV = as.numeric(MV), MTBV = as.numeric(MTBV))
meta_2020 <- meta_2020 %>% 
  rename(MV = `X(MV)~USD`) %>% 
  mutate(Jahr = "2020", MV = as.numeric(MV), MTBV = as.numeric(MTBV))
meta_2025 <- meta_2025 %>% 
  rename(MV = `X(MV)~USD`) %>% 
  mutate(Jahr = "2025", MV = as.numeric(MV), MTBV = as.numeric(MTBV))

# alle 4 Metadaten in einem dataframe zusammenfassen
df_fundamentals <- bind_rows(meta_2010, meta_2015, meta_2020, meta_2025)

# Datenvorbereitung:
# mutate wegen Leerzeichenproblem bei Variablen!
# Kehrwert von MTB, da mit dem BTM ausgewertet wird
df_fundamentals <- df_fundamentals %>%
  mutate(
    # Leerzeichenproblemlösung
    Aktie = make.names(NAME), 
    
    # Kehrwert MTB => BTM
    BookToMarket = 1 / as.numeric(MTBV)
  )

# Jahr-Spalten der dfs in gleiches Format für den merge umwandeln
df_weights$Jahr <- as.character(df_weights$Jahr)
df_fundamentals$Jahr <- as.character(df_fundamentals$Jahr)
df_vola$Jahr <- as.character(df_vola$Jahr)
df_momentum$Jahr <- as.character(df_momentum$Jahr)

# Titel aus der Vola-Tabelle mit den Fundamentals versehen
# (diese enthält noch alle "gültigen" Aktien!)
df_universe <- df_vola %>%
  left_join(df_fundamentals, by = c("Jahr", "Aktie")) %>%

  left_join(df_momentum, by = c("Jahr", "Aktie"))

# Aus den vier Universen werden die Verteilungsparameter ermittelt 
df_universe_zscores <- df_universe %>%
  # Gruppierung nach JAHR
  group_by(Jahr) %>%
  mutate(
    # Size: MV
    Z_Size = as.numeric(scale(MV)),
    
    # Z-Score Value: BookToMarket
    Z_Value = as.numeric(scale(BookToMarket)),
    
    # Z-Score Low Volatility: (annualisierte) Volatilität 
    Z_LowVol = as.numeric(scale(Volatilitat)) * (-1),
    
    # Z-Score Momentum: 12m-1m
    Z_Momentum = as.numeric(scale(Momentum))
  ) %>%
  ungroup()

# Portfoliogewichte mit entsprechendem Universum zusammenfügen
df_merged <- df_weights %>%
  # left join: wichtig, da eine Aktie in mehreren Portfolios im Jahr sein kann!
  left_join(df_universe_zscores, by = c("Jahr", "Aktie"))

# =========================================================================
# DYNAMISCHE PORTFOLIO-REIHENFOLGE (Nach Vola sortiert)
# =========================================================================
# Portfolionamen aus summary ermitteln und nach vol sortieren
portfolio_order <- df_summary %>%
  group_by(Portfolio_Typ) %>%
  summarise(mean_vola = mean(Vola_Prozent), .groups = "drop") %>%
  arrange(mean_vola) %>%
  pull(Portfolio_Typ)

cat("\nSortier-Reihenfolge für Plots erkannt:\n")
print(portfolio_order)

# df für Stilfaktoren
df_exposures_styles <- df_merged %>%
  group_by(Jahr, Portfolio_Typ) %>%
  summarise(
    Exp_Size = sum(Gewicht * Z_Size, na.rm = TRUE),
    Exp_Value = sum(Gewicht * Z_Value, na.rm = TRUE),
    Exp_LowVol = sum(Gewicht * Z_LowVol, na.rm = TRUE),
    Exp_Momentum = sum(Gewicht * Z_Momentum, na.rm = TRUE),
    .groups = "drop"
  )

# df für Branchen
df_exposures_branche <- df_merged %>%
  group_by(Jahr, Portfolio_Typ, TR3N) %>%
  summarise(
    Gewicht_Prozent = sum(Gewicht, na.rm = TRUE),
    .groups = "drop"
  )

# df für Regionen
df_exposures_region <- df_merged %>%
  group_by(Jahr, Portfolio_Typ, GEOGN) %>%
  summarise(
    Gewicht_Prozent = sum(Gewicht, na.rm = TRUE),
    .groups = "drop"
  )

# VISUALISIERUNGEN

# =========================================================================
# 1. TOP 4 EINZELTITEL (UNTERNEHMEN)
# =========================================================================

# Benchmark-Gewichte (S&P 500 Index) für ALLE Aktien berechnen
df_benchmark_aktie <- df_universe %>%
  group_by(Jahr) %>%
  mutate(Index_Gewicht = MV / sum(MV, na.rm = TRUE)) %>%
  select(Jahr, Aktie, Index_Gewicht) %>%
  ungroup()

# Top 4 Aktien aus den Portfolios PRO JAHR ermitteln (über alle Portfolio-Typen hinweg)
top4_aktien_per_year <- df_merged %>%
  group_by(Jahr, Aktie) %>%
  # Wir schauen, welche Aktien in Summe über alle 5 Portfolios am stärksten gewichtet sind
  summarise(Gesamt_Portfolio_Gewicht = sum(Gewicht, na.rm = TRUE), .groups = "drop_last") %>%
  slice_max(order_by = Gesamt_Portfolio_Gewicht, n = 4) %>%
  select(Jahr, Aktie) %>%
  mutate(Kategorie = Aktie) 

# Benchmark-Gewichte für den Plot vorbereiten (Top 4 + Sonstige)
df_bench_plot_aktie <- df_benchmark_aktie %>%
  left_join(top4_aktien_per_year, by = c("Jahr", "Aktie")) %>%
  mutate(Kategorie = replace_na(Kategorie, "Sonstige")) %>%
  group_by(Jahr, Kategorie) %>%
  summarise(Index_Gewicht = sum(Index_Gewicht, na.rm = TRUE), .groups = "drop")

# Portfoliogewichte für den Plot vorbereiten (Top 4 + Sonstige)
df_port_plot_aktie <- df_merged %>%
  left_join(top4_aktien_per_year, by = c("Jahr", "Aktie")) %>%
  mutate(Kategorie = replace_na(Kategorie, "Sonstige")) %>%
  group_by(Jahr, Portfolio_Typ, Kategorie) %>%
  summarise(Gewicht_Prozent = sum(Gewicht, na.rm = TRUE), .groups = "drop")

# Daten für den Plot mergen (Benchmark + Portfolios)
plot_data_aktie <- expand_grid(
  df_bench_plot_aktie,
  Portfolio_Typ = unique(df_port_plot_aktie$Portfolio_Typ)
) %>%
  left_join(df_port_plot_aktie, by = c("Jahr", "Kategorie", "Portfolio_Typ")) %>%
  mutate(Gewicht_Prozent = replace_na(Gewicht_Prozent, 0))

# Dynamische Sortierung anwenden
plot_data_aktie$Portfolio_Typ <- factor(plot_data_aktie$Portfolio_Typ, levels = portfolio_order)

# Dynamische Sortierung der X-Achse (Top-Titel nach links, Sonstige nach rechts)
plot_data_aktie <- plot_data_aktie %>%
  group_by(Jahr, Kategorie) %>%
  mutate(Summe_Gewicht_Jahr = sum(Gewicht_Prozent, na.rm = TRUE)) %>%
  ungroup()

# "Sonstige" zwingend ans Ende setzen
plot_data_aktie <- plot_data_aktie %>%
  mutate(Summe_Gewicht_Jahr = ifelse(Kategorie == "Sonstige", -Inf, Summe_Gewicht_Jahr))

# Hilfsspalte für das Reordering im Grid
plot_data_aktie <- plot_data_aktie %>%
  mutate(Kategorie_Facet = paste(Jahr, Kategorie, sep = "__"))

plot_data_aktie$Kategorie_Facet <- reorder(plot_data_aktie$Kategorie_Facet, -plot_data_aktie$Summe_Gewicht_Jahr)

# Der finale Plot
plot_aktie_grid <- ggplot(plot_data_aktie, aes(x = Kategorie_Facet)) +
  
  # Balken für die Portfolios
  geom_col(aes(y = Gewicht_Prozent, fill = Portfolio_Typ), 
           position = position_dodge(width = 0.85), 
           color = "black", linewidth = 0.2, alpha = 0.9) +
  
  # Rote Linie für das Index-Gewicht (S&P 500 Anteil dieser Aktie)
  geom_errorbar(aes(ymin = Index_Gewicht, ymax = Index_Gewicht), 
                color = "red", linewidth = 1, width = 0.85) +
  
  facet_wrap(~ Jahr, ncol = 1, scales = "free_x") +
  
  # Namen säubern: Jahr-Präfix weg, "DEAD" entfernen, Punkte in Leerzeichen umwandeln und umbrechen
  scale_x_discrete(labels = function(x) str_wrap(gsub("\\.", " ", gsub("\\.+DEAD", "", gsub("^.*__", "", x))), width = 12)) +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_viridis_d(option = "viridis") + 
  
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold"),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 14),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    title = "Top 4 Einzeltitel (Unternehmen) vs. Index",
    subtitle = "Die 4 größten Positionen im Portfolio pro Jahr (Rote Linie = Indexgewicht)",
    x = NULL,
    y = "Anteil am Portfolio / Index",
    fill = "Portfolio:"
  )

print(plot_aktie_grid)

# Dateiexport
ggsave("data/Plot_Einzeltitel_Grid.png", plot = plot_aktie_grid, width = 9, height = 10, dpi = 300)

# =========================================================================
# 2. BRANCHEN
# =========================================================================

# Benchmark-Gewichte vom Index für ALLE Branchen nach MV berechnen (für Sortierung)
df_benchmark_branche <- df_universe %>%
  group_by(Jahr, TR3N) %>%
  # MV pro Jahr und Branche addieren
  summarise(Branche_MV = sum(MV, na.rm = TRUE), .groups = "drop_last") %>%
  # Branchen-MV durch Summe ergibt den relativen Anteil einer Branche pro Jahr
  mutate(Index_Gewicht = Branche_MV / sum(Branche_MV, na.rm = TRUE)) %>%
  ungroup()

# Top 4 Branchen aus den Portfolios PRO JAHR GESAMT ermitteln
top4_branchen_per_year <- df_exposures_branche %>%
  group_by(Jahr, TR3N) %>%
  # Summen pro Jahr und Branche bilden
  summarise(Gesamt_Portfolio_Gewicht = sum(Gewicht_Prozent, na.rm = TRUE), .groups = "drop_last") %>%
  # die 4 stärksten Branchen nach Anteil absteigend ordnen
  slice_max(order_by = Gesamt_Portfolio_Gewicht, n = 4) %>%
  select(Jahr, TR3N) %>%
  mutate(Kategorie = TR3N) 

# Gesamtgewichte zu Branchen zuordnen und "Sonstige" (=alles nach den 4 größten) einfügen
df_bench_plot_branche <- df_benchmark_branche %>%
  # alle die nicht zu den Top 4 gehören werden als NA bzw. Sonstige bezeichnet
  left_join(top4_branchen_per_year, by = c("Jahr", "TR3N")) %>%
  mutate(Kategorie = replace_na(Kategorie, "Sonstige")) %>%
  group_by(Jahr, Kategorie) %>%
  summarise(Index_Gewicht = sum(Index_Gewicht, na.rm = TRUE), .groups = "drop")

# nochmal Gesamtgewichte zu Branchen zuordnen für Portfolios
df_port_plot_branche <- df_exposures_branche %>%
  left_join(top4_branchen_per_year, by = c("Jahr", "TR3N")) %>%
  mutate(Kategorie = replace_na(Kategorie, "Sonstige")) %>%
  group_by(Jahr, Portfolio_Typ, Kategorie) %>%
  summarise(Gewicht_Prozent = sum(Gewicht_Prozent, na.rm = TRUE), .groups = "drop")

# Gewichte mit Jahr versehen damit 4 Gruppen/Summen gebildet werden können
plot_data_branche <- expand_grid(
  df_bench_plot_branche,
  Portfolio_Typ = unique(df_port_plot_branche$Portfolio_Typ)
) %>%
  left_join(df_port_plot_branche, by = c("Jahr", "Kategorie", "Portfolio_Typ")) %>%
  mutate(Gewicht_Prozent = replace_na(Gewicht_Prozent, 0))

# Sortierungen für das Diagramm festlegen
plot_data_branche$Portfolio_Typ <- factor(plot_data_branche$Portfolio_Typ, levels = portfolio_order)

# Dynamische Sortierung PRO JAHR
# Zuerst die absolute Summe der Portfoliogewichte pro Branche und Jahr berechnen
plot_data_branche <- plot_data_branche %>%
  group_by(Jahr, Kategorie) %>%
  mutate(Summe_Gewicht_Jahr = sum(Gewicht_Prozent, na.rm = TRUE)) %>%
  ungroup()

# "Sonstige" zwingend ans Ende setzen (fiktiv sehr negative Summe)
plot_data_branche <- plot_data_branche %>%
  mutate(Summe_Gewicht_Jahr = ifelse(Kategorie == "Sonstige", -Inf, Summe_Gewicht_Jahr))

# Um pro Jahr individuell sortieren zu können, wird die Hilfsspalte: "Jahr__Kategorie" hinzugefügt
plot_data_branche <- plot_data_branche %>%
  mutate(Kategorie_Facet = paste(Jahr, Kategorie, sep = "__"))

# nach Hilfsspalte absteigend sortieren
plot_data_branche$Kategorie_Facet <- reorder(plot_data_branche$Kategorie_Facet, -plot_data_branche$Summe_Gewicht_Jahr)

# finale Plot
plot_branche_grid <- ggplot(plot_data_branche, aes(x = Kategorie_Facet)) +
  
  geom_col(aes(y = Gewicht_Prozent, fill = Portfolio_Typ), 
           position = position_dodge(width = 0.85), 
           color = "black", linewidth = 0.2, alpha = 0.9) +
  
  geom_errorbar(aes(ymin = Index_Gewicht, ymax = Index_Gewicht), 
                color = "red", linewidth = 1, width = 0.85) +
  
  facet_wrap(~ Jahr, ncol = 1, scales = "free_x") +
  
  # gsub schneidet das Jahr ab, str_wrap bricht den Text nach ca. 15 Zeichen in eine neue Zeile um
  scale_x_discrete(labels = function(x) str_wrap(gsub("^.*__", "", x), width = 20)) +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_viridis_d(option = "viridis") + 
  
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, face = "bold"),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 14),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    title = "Sektorale Portfolio-Allokation vs. Index",
    subtitle = "Top 4 Branchen im Portfolio + 'Sonstige' (Sortiert nach Portfoliogewicht)",
    x = NULL,
    y = "Anteil am Portfolio / Index",
    fill = "Portfolio:"
  )

print(plot_branche_grid)
# Dateiexport
# width schmaler machen (passend für die Word-Seitenbreite) und height massiv erhöhen
ggsave("data/Plot_Branchen_Grid.png", plot = plot_branche_grid, width = 9, height = 10, dpi = 300)     

# =========================================================================
# 3. SIZE
# =========================================================================

# Daten isolieren
plot_data_size <- df_exposures_styles %>%
  select(Jahr, Portfolio_Typ, Exp_Size)

# Portfolios sortieren (Vola aufsteigend)
plot_data_size$Portfolio_Typ <- factor(plot_data_size$Portfolio_Typ, levels = portfolio_order)

# Der Size-Plot
plot_size <- ggplot(plot_data_size, aes(x = Portfolio_Typ, y = Exp_Size, fill = Portfolio_Typ)) +
  
  # Balken einzeichnen
  geom_col(color = "black", linewidth = 0.2, alpha = 0.9) +
  
  # Die Nulllinie (Benchmark/Marktdurchschnitt) stark rot hervorheben
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  
  # Wieder das 4-Jahres-Grid im Hochformat
  facet_wrap(~ Jahr, ncol = 1) +
  
  scale_fill_viridis_d(option = "viridis") + 
  
  theme_minimal(base_size = 14) +
  theme(
    # Text leicht schräg, damit Beschreibung/Text leserlich
    axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, face = "bold"),
    legend.position = "none", # Keine Legende nötig, da Namen schon auf der X-Achse stehen
    strip.text = element_text(face = "bold", size = 14),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    title = "Stilfaktor-Exposure: Size (Marktkapitalisierung)",
    subtitle = "Z-Scores (Rote Linie = 0 = Mittelwert | > 0 Large Cap | < 0 Small Cap)",
    x = NULL,
    y = "Exposure (Z-Score)"
  )

print(plot_size)
# Dateiexport im gleichen Hochformat wie Branchen
ggsave("data/Plot_Factor_Size.png", plot = plot_size, width = 9, height = 10, dpi = 300)

# =========================================================================
# 4. VALUE
# =========================================================================
plot_data_value <- df_exposures_styles %>% select(Jahr, Portfolio_Typ, Exp_Value)
plot_data_value$Portfolio_Typ <- factor(plot_data_value$Portfolio_Typ, levels = portfolio_order)

plot_value <- ggplot(plot_data_value, aes(x = Portfolio_Typ, y = Exp_Value, fill = Portfolio_Typ)) +
  geom_col(color = "black", linewidth = 0.2, alpha = 0.9) +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  facet_wrap(~ Jahr, ncol = 1) +
  scale_fill_viridis_d(option = "viridis") + 
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, face = "bold"),
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 14),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    title = "Stilfaktor-Exposure: Value (Book-to-Market)",
    subtitle = "Z-Scores (Rote Linie = 0 = Mittelwert | > 0 Value-Fokus | < 0 Growth-Fokus)",
    x = NULL, y = "Exposure (Z-Score)"
  )

print(plot_value)
ggsave("data/Plot_Factor_Value.png", plot = plot_value, width = 9, height = 10, dpi = 300)

# =========================================================================
# 5. Momentum
# =========================================================================
plot_data_mom <- df_exposures_styles %>% select(Jahr, Portfolio_Typ, Exp_Momentum)
plot_data_mom$Portfolio_Typ <- factor(plot_data_mom$Portfolio_Typ, levels = portfolio_order)

plot_mom <- ggplot(plot_data_mom, aes(x = Portfolio_Typ, y = Exp_Momentum, fill = Portfolio_Typ)) +
  geom_col(color = "black", linewidth = 0.2, alpha = 0.9) +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  facet_wrap(~ Jahr, ncol = 1) +
  scale_fill_viridis_d(option = "viridis") + 
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, face = "bold"),
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 14),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    title = "Stilfaktor-Exposure: Momentum (12m - 1m)",
    subtitle = "Z-Scores (Rote Linie = 0 = Mittelwert | > 0 Gewinner-Aktien | < 0 Verlierer-Aktien)",
    x = NULL, y = "Exposure (Z-Score)"
  )

print(plot_mom)
ggsave("data/Plot_Factor_Momentum.png", plot = plot_mom, width = 9, height = 10, dpi = 300)

# =========================================================================
# 6. Low Volatility
# =========================================================================
plot_data_lowvol <- df_exposures_styles %>% select(Jahr, Portfolio_Typ, Exp_LowVol)
plot_data_lowvol$Portfolio_Typ <- factor(plot_data_lowvol$Portfolio_Typ, levels = portfolio_order)

plot_lowvol <- ggplot(plot_data_lowvol, aes(x = Portfolio_Typ, y = Exp_LowVol, fill = Portfolio_Typ)) +
  geom_col(color = "black", linewidth = 0.2, alpha = 0.9) +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  facet_wrap(~ Jahr, ncol = 1) +
  scale_fill_viridis_d(option = "viridis") + 
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, face = "bold"),
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 14),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    title = "Stilfaktor-Exposure: Low Volatility",
    subtitle = "Z-Scores (Rote Linie = 0 = Mittelwert | > 0 Risikoärmer als Mittelwert | < 0 Riskanter)",
    x = NULL, y = "Exposure (Z-Score)"
  )

print(plot_lowvol)
ggsave("data/Plot_Factor_LowVol.png", plot = plot_lowvol, width = 9, height = 10, dpi = 300)

# =========================================================================
# 7. EFFIZIENZGRENZEN (NUR PORTFOLIOS, OHNE EINZELAKTIEN)
# =========================================================================

# Daten für die 5 Portfolios (Effizienzgrenze) vorbereiten
plot_data_frontier <- df_summary %>%
  mutate(Jahr = as.character(Jahr))
plot_data_frontier$Portfolio_Typ <- factor(plot_data_frontier$Portfolio_Typ, levels = portfolio_order)

# Das Minimum-Varianz-Portfolio (Startpunkt) isolieren (als optischer Diamant)
plot_data_minvar <- plot_data_frontier %>%
  filter(Portfolio_Typ == portfolio_order[1])

# Der finale Plot
plot_frontier <- ggplot() +
  
  # A) Die Linien der berechneten Effizienzgrenzen
  geom_line(data = plot_data_frontier, 
            aes(x = Vola_Prozent, y = Rendite_Prozent, color = Jahr, group = Jahr), 
            linewidth = 1.2, alpha = 0.8) +
  
  # B) Die Standard-Portfolios (Punkte)
  geom_point(data = plot_data_frontier, 
             aes(x = Vola_Prozent, y = Rendite_Prozent, color = Jahr, shape = Portfolio_Typ), 
             size = 4, stroke = 1.2) +
  
  # C) Die Minimum-Varianz-Portfolios als Diamanten (Farbe passend zum Jahr)
  geom_point(data = plot_data_minvar, 
             aes(x = Vola_Prozent, y = Rendite_Prozent, fill = Jahr), 
             shape = 23, color = "black", size = 5, stroke = 1.2) +
  
  # Farben, Legenden und Formen
  scale_color_viridis_d(option = "viridis", direction = -1) +
  scale_fill_viridis_d(option = "viridis", direction = -1, guide = "none") +
  # MVP aus der Shape-Legende ausblenden, da es durch den farbigen Diamanten schon hervorgehoben ist
  scale_shape_manual(values = c(16, 15, 17, 18, 8), breaks = portfolio_order[-1]) +
  scale_x_continuous(labels = scales::percent_format(scale = 1, accuracy = 0.1)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1, accuracy = 0.1)) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Verschiebung der Effizienzgrenzen (2010 - 2025)",
    x = "Volatilität (Risiko)",
    y = "Erwartete Rendite E(r)",
    color = "Anlagejahr:",
    shape = "Portfolio-Typ:"
  )

print(plot_frontier)
ggsave("data/Plot_Efficient_Frontier_Clean.png", plot = plot_frontier, width = 10, height = 7, dpi = 300)








