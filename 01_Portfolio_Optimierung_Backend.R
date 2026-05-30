# libraries laden
library(corpcor)
library(readxl)
library(tidyverse)
library(quadprog)

# VARIABLEN

# Ziel-Volatilitäten
target_vol_1 <- 0.15
target_vol_2 <- 0.18
target_vol_3 <- 0.21
target_vol_4 <- 0.24

# Schwellenewert für die relative Anzahl von Renditen pro Aktie die vorhanden sein müssen
thresh_valid_returns <- 0.4

# Mindestgewicht für Aktien, die in den finalen Portfolios aufgenommen werden sollen
min_share_weight <- 0.0001

# Daten laden
meta_2010 <- read_excel("data/S&P500 FROM 2010 TO 2025.xlsx", sheet = "2010 META")
meta_2015 <- read_excel("data/S&P500 FROM 2010 TO 2025.xlsx", sheet = "2015 META")
meta_2020 <- read_excel("data/S&P500 FROM 2010 TO 2025.xlsx", sheet = "2020 META")
meta_2025 <- read_excel("data/S&P500 FROM 2010 TO 2025.xlsx", sheet = "2025 META")
return_2010 <- read_excel("data/S&P500 FROM 2010 TO 2025.xlsx", sheet = "2010 RETURN")
return_2015 <- read_excel("data/S&P500 FROM 2010 TO 2025.xlsx", sheet = "2015 RETURN")
return_2020 <- read_excel("data/S&P500 FROM 2010 TO 2025.xlsx", sheet = "2020 RETURN")
return_2025 <- read_excel("data/S&P500 FROM 2010 TO 2025.xlsx", sheet = "2025 RETURN")

# Datumspalte (=erste Spalte) entfernen
return_2010 <- return_2010[, -1]
return_2015 <- return_2015[, -1] 
return_2020 <- return_2020[, -1] 
return_2025 <- return_2025[, -1] 

# sämtliche Spalten in numerische umwandeln
return_2010 <- data.frame(lapply(return_2010, as.numeric))
return_2015 <- data.frame(lapply(return_2015, as.numeric))
return_2020 <- data.frame(lapply(return_2020, as.numeric))
return_2025 <- data.frame(lapply(return_2025, as.numeric))

# Prozentzahlen in Dezimalzahlen umwandeln
return_2010 <- return_2010 / 100
return_2015 <- return_2015 / 100
return_2020 <- return_2020 / 100
return_2025 <- return_2025 / 100

# Kopien der Renditedaten VOR der 40%-Filterung erstellen
# Dadurch kann später verglichen werden, wie viele Aktien entfernt wurden.
return_2010_raw <- return_2010
return_2015_raw <- return_2015
return_2020_raw <- return_2020
return_2025_raw <- return_2025

# Funktion zum Entfernen von Aktien (Spalten) mit zu wenig Datenpunkten
filter_stocks_by_na <- function(df, threshold) {
  # relativen Anteil an gesamten Return ermitteln um mit dem Schwellenwert vergleichen zu können
  valid_ratio <- colSums(!is.na(df)) / nrow(df)
  
  # Informativ: Zusammenfassung der Aktien, die entfernt wurden
  dropped_stocks <- names(df)[valid_ratio < threshold]
  
  # Konsolenausgabe der entfernten Aktien
  if(length(dropped_stocks) > 0) {
    cat("Entfernte Aktien (weniger als", threshold * 100, "% Daten):\n")
    cat(paste(dropped_stocks, collapse = ", "), "\n\n")
  }
  
  # Behalte nur die Spalten, die den Schwellenwert erreichen oder überschreiten
  df_filtered <- df[, valid_ratio >= threshold]
  
  return(df_filtered)
}

# Funktion zum Extrahieren und Säubern der Portfolio-Gewichte
extract_weights <- function(portfolio_result, return_df, year_label, port_label) {
  
  # Sicherheitsprüfung, falls das Portfolio nicht berechnet werden konnte
  if(is.na(portfolio_result$risk[1])) return(NULL)
  
  # Tabelle bauen aus Spaltennamen (Aktien) und Gewichten
  df <- data.frame(
    Jahr = year_label,
    Portfolio_Typ = port_label,
    Aktie = colnames(return_df),
    Gewicht = round(portfolio_result$weights, 4) # Runden auf 4 Nachkommastellen
  )
  
  # Es werden nur Aktien behalten, die ein Gewicht von mind. "min_share_weight" haben.
  df <- df[df$Gewicht >= min_share_weight, ]
  
  # Ergebnisse nach Gewicht absteigend sortieren
  df <- df[order(-df$Gewicht), ]
  
  # Zeilennamen resetten
  rownames(df) <- NULL
  
  return(df)
}

# Funktion zur Berechnung des 12M-1M Momentum bei DAILY RETURNS
calculate_momentum <- function(return_df) {
  
  n_days <- nrow(return_df)
  
  # letzte Zeile ist jüngstes Datum
  # 252 - "heute" = 251
  start_row <- max(1, n_days - 251) 
  
  # letzten 21 Tage (=1 Monat) wird ausgeschlossen
  end_row <- max(1, n_days - 21)    
  
  # Dieser Zeitraum wird abgetrennt...
  mom_data <- return_df[start_row:end_row, , drop = FALSE]
  
  # ... und davon die kummulierte Rendite berechnet = Momentum
  mom_scores <- apply(mom_data, 2, function(x) {
    prod(1 + x[!is.na(x)]) - 1
  })
  
  return(mom_scores)
}

# Alle NA/0 - Funktionen auf alle vier Datensätze anwenden
return_2010 <- filter_stocks_by_na(return_2010, thresh_valid_returns)
return_2015 <- filter_stocks_by_na(return_2015, thresh_valid_returns)
return_2020 <- filter_stocks_by_na(return_2020, thresh_valid_returns)
return_2025 <- filter_stocks_by_na(return_2025, thresh_valid_returns)

# =========================================================================
# ÜBERSICHT: ANZAHL DER AKTIEN VOR UND NACH DER 40%-FILTERUNG
# =========================================================================

filter_summary_stocks <- data.frame(
  Jahr = c("2010", "2015", "2020", "2025"),
  
  Aktien_vor_Filter = c(
    ncol(return_2010_raw),
    ncol(return_2015_raw),
    ncol(return_2020_raw),
    ncol(return_2025_raw)
  ),
  
  Aktien_nach_Filter = c(
    ncol(return_2010),
    ncol(return_2015),
    ncol(return_2020),
    ncol(return_2025)
  )
)

# Anzahl der entfernten Aktien berechnen
filter_summary_stocks$Entfernte_Aktien <- 
  filter_summary_stocks$Aktien_vor_Filter - filter_summary_stocks$Aktien_nach_Filter

# Prozentualen Anteil der entfernten Aktien berechnen
filter_summary_stocks$Entfernte_Aktien_Prozent <- round(
  filter_summary_stocks$Entfernte_Aktien / filter_summary_stocks$Aktien_vor_Filter * 100,
  2
)

# Tabelle in der Konsole anzeigen
print("--- ÜBERSICHT: AKTIEN VOR UND NACH DER 40%-FILTERUNG ---")
print(filter_summary_stocks)

# Tabelle als CSV speichern
write.csv2(
  filter_summary_stocks,
  "data/Filter_Summary_Aktien.csv",
  row.names = FALSE
)

# Daten bereinigen: Tage mit mind. 1xNA werden gelöscht (Für Complete Case!)
return_2010_cc <- na.omit(return_2010)
return_2015_cc <- na.omit(return_2015)
return_2020_cc <- na.omit(return_2020)
return_2025_cc <- na.omit(return_2025)

# Übersichtstabelle für den cc-Datenverlust erstellen
robustness_summary <- data.frame(
  Jahr = c("2010", "2015", "2020", "2025"),
  Verbleibende_Tage = c(nrow(return_2010_cc), nrow(return_2015_cc), nrow(return_2020_cc), nrow(return_2025_cc)),
  Gesamte_Tage = c(nrow(return_2010), nrow(return_2015), nrow(return_2020), nrow(return_2025))
)

# Prozentualen Verlust berechnen (cc)
robustness_summary$Verlust_Prozent <- round(100 - (robustness_summary$Verbleibende_Tage / robustness_summary$Gesamte_Tage * 100), 2)

# Tabelle in der Konsole anzeigen (und im Environment von RStudio abrufbar)
print("--- ROBUSTHEITSCHECK: DATENVERLUST DURCH NA.OMIT ---")
print(robustness_summary)

# Alternative (na.omit) Kovarianzmatrizen berechnen
cov_matrix_2010_cc <- cov(return_2010_cc) * 252
cov_matrix_2015_cc <- cov(return_2015_cc) * 252
cov_matrix_2020_cc <- cov(return_2020_cc) * 252
cov_matrix_2025_cc <- cov(return_2025_cc) * 252

# Kovarianzmatrizen nur mit "kompletten/vorhandenen" Zahlenpaare berechnen (annualisiert)
cov_matrix_2010 <- cov(return_2010, use = "pairwise.complete.obs") * 252
cov_matrix_2015 <- cov(return_2015, use = "pairwise.complete.obs") * 252
cov_matrix_2020 <- cov(return_2020, use = "pairwise.complete.obs") * 252
cov_matrix_2025 <- cov(return_2025, use = "pairwise.complete.obs") * 252

# Positive Definitheit erzwingen (für solve.QP)
cov_matrix_2010 <- make.positive.definite(cov_matrix_2010)
cov_matrix_2015 <- make.positive.definite(cov_matrix_2015)
cov_matrix_2020 <- make.positive.definite(cov_matrix_2020)
cov_matrix_2025 <- make.positive.definite(cov_matrix_2025)

# historische, diskrete, annualisierte Erwartungswerte sämtlicher Aktien
exp_return_2010 <- colMeans(return_2010, na.rm = TRUE) * 252
exp_return_2015 <- colMeans(return_2015, na.rm = TRUE) * 252
exp_return_2020 <- colMeans(return_2020, na.rm = TRUE) * 252
exp_return_2025 <- colMeans(return_2025, na.rm = TRUE) * 252

# Funktion für eine erwartete Portfoliorendite
portfolio_return <- function(w, mu) { sum(w * mu)}

# Funktion für eine Portfoliovolatilität
portfolio_risk <- function(w, Sigma) {sqrt(as.numeric(t(w) %*% Sigma %*% w))}

# Funktion für ein Min.Var.-Portfolio
min_var_portfolio <- function(mu, Sigma) {
  
  # Anzahl Assets
  n <- length(mu)
  
  # Varianzminimierung
  Dmat <- 2 * Sigma 
  dvec <- rep(0, n)  
  
  # Nebenbedingungen: Summe Gewichte = 1 UND keine Short Sales w >= 0
  Amat <- cbind(
    rep(1, n),   
    diag(n)      
  )
  
  bvec <- c(
    1,          
    rep(0, n)  
  )
  
  # meq = 1 bedeutet:  erste Bedingung ist Gleichung (sum(w) = 1) rest sind Ungleichungen (>=)
  result <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
  
  # optimale Gewichte w
  w <- result$solution
  
  # Rückgabe als Liste
  list(
    weights = w,
    expected_return = portfolio_return(w, mu),
    risk = portfolio_risk(w, Sigma)
  )
}

# Funktion für ein Portfolio mit vorgegebener Zielrendite
target_return_portfolio <- function(mu, Sigma, target_ret) {
  
  n <- length(mu)
  Dmat <- 2 * Sigma  
  dvec <- rep(0, n)  
  
  # Sicherheitsprüfung ob die Renditeerwartung möglich ist
  max_possible_ret <- max(mu, na.rm = TRUE)
  if(target_ret > max_possible_ret) {
    warning(paste("Zielrendite", round(target_ret*100,2), "% ist mathematisch unmöglich. Setze auf maximal mögliche Rendite von", round(max_possible_ret*100,2), "%."))
    target_ret <- max_possible_ret
  }
  
  # Nebenbedingungen: Summe Gewichte = 1 UND keine Short Sales w >= 0
  Amat <- cbind(
    rep(1, n),   
    mu,          
    diag(n)      
  )
  
  bvec <- c(
    1,           
    target_ret,  
    rep(0, n)    
  )
  
  # solve.QP durchführen (eingepackt in tryCatch, falls es unlösbare Konstellationen gibt)
  result <- try(solve.QP(Dmat, dvec, Amat, bvec, meq = 1), silent = TRUE)
  
  # Fehlermeldung falls keine Lösung gefunden wurde
  if (inherits(result, "try-error")) {
    return(list(weights = rep(NA, n), expected_return = NA, risk = NA))
  }
  
  w <- result$solution
  
  list(
    weights = w,
    expected_return = portfolio_return(w, mu),
    risk = portfolio_risk(w, Sigma)
  )
}

# Funktion für ein Portfolio mit exakt vorgegebenem Zielrisiko (Bisektionsverfahren)
target_vola_portfolio <- function(mu, Sigma, target_vol, tol = 1e-4, max_iter = 100) {
  
  # 1. Start- und Endpunkte ermitteln
  mvp <- min_var_portfolio(mu, Sigma)
  max_ret <- max(mu, na.rm = TRUE)
  max_port <- target_return_portfolio(mu, Sigma, max_ret)
  
  # Sicherheitsprüfung: Ist das Zielrisiko überhaupt möglich?
  if(target_vol <= mvp$risk) {
    warning(paste("Zielrisiko ist zu gering! Minimum ist", round(mvp$risk*100,2), "%. Gebe Minimum-Varianz-Portfolio zurück."))
    return(mvp)
  }
  if(!is.na(max_port$risk) && target_vol >= max_port$risk) {
    warning(paste("Zielrisiko ist zu hoch! Maximum ist", round(max_port$risk*100,2), "%. Gebe Max-Return-Portfolio zurück."))
    return(max_port)
  }
  
  # 2. Bisektionsverfahren
  low_ret <- mvp$expected_return
  high_ret <- max_ret
  
  for (i in 1:max_iter) {
    # Mitte der Rendite berechnen
    mid_ret <- (low_ret + high_ret) / 2
    port_mid <- target_return_portfolio(mu, Sigma, mid_ret)
    
    # Notausstieg falls solve.QP fehlschlägt
    if (is.na(port_mid$risk)) break 
    
    # Abweichung vom Zielrisiko
    diff <- port_mid$risk - target_vol
    
    # Abweichung
    if (abs(diff) < tol) {
      return(port_mid)
    }
    
    # Intervall anpassen
    if (diff < 0) {
      # Risiko ist zu klein -> mehr Rendite (untere Grenze anheben)
      low_ret <- mid_ret
    } else {
      # Risiko ist zu groß -> weniger Rendite (obere Grenze absenken)
      high_ret <- mid_ret
    }
  }
  
  # Falls max_iter erreicht wird, gib die bestmögliche Näherung zurück
  return(target_return_portfolio(mu, Sigma, (low_ret + high_ret) / 2))
}

# =========================================================================
# ZIEL-PORTFOLIOS BERECHNEN: ALLE JAHRE mit Schleife
# =========================================================================

# 1. Listen vorbereiten, um mit einer Schleife durch alle Jahre zu iterieren
years <- c("2010", "2015", "2020", "2025")

list_mu <- list("2010" = exp_return_2010, "2015" = exp_return_2015, "2020" = exp_return_2020, "2025" = exp_return_2025)
list_sigma <- list("2010" = cov_matrix_2010, "2015" = cov_matrix_2015, "2020" = cov_matrix_2020, "2025" = cov_matrix_2025)
list_returns <- list("2010" = return_2010, "2015" = return_2015, "2020" = return_2020, "2025" = return_2025)

# Leere Listen für die finalen Ergebnisse erstellen
all_summaries <- list()
all_weights <- list()

# 2. Die Schleife führt die Schritte nun für jedes Jahr automatisch durch
for (y in years) {
  
  # Daten für das jeweilige Jahr aus den Listen abrufen
  mu <- list_mu[[y]]
  Sigma <- list_sigma[[y]]
  ret_df <- list_returns[[y]]
  
  # Portfolios berechnen
  mvp <- min_var_portfolio(mu, Sigma)
  p2 <- target_vola_portfolio(mu, Sigma, target_vol_1)
  p3 <- target_vola_portfolio(mu, Sigma, target_vol_2)
  p4 <- target_vola_portfolio(mu, Sigma, target_vol_3)
  p5 <- target_vola_portfolio(mu, Sigma, target_vol_4)
  
  # Labels generieren (Name in der Tabelle)
  label_p2 <- paste0("Target Vol ", round(target_vol_1 * 100, 2), "%")
  label_p3 <- paste0("Target Vol ", round(target_vol_2 * 100, 2), "%")
  label_p4 <- paste0("Target Vol ", round(target_vol_3 * 100, 2), "%")
  label_p5 <- paste0("Target Vol ", round(target_vol_4 * 100, 2), "%")
  
  # Summary-Tabelle für dieses Jahr erstellen
  summ <- data.frame(
    Jahr = y,
    Portfolio_Typ = c("Min Variance", label_p2, label_p3, label_p4, label_p5),
    Rendite_Prozent = round(c(mvp$expected_return, p2$expected_return, p3$expected_return, p4$expected_return, p5$expected_return) * 100, 2),
    Vola_Prozent = round(c(mvp$risk, p2$risk, p3$risk, p4$risk, p5$risk) * 100, 2)
  )
  
  cat("\n--- SUMMARY", y, "---\n")
  print(summ)
  
  # Summary in der Liste speichern
  all_summaries[[y]] <- summ
  
  # Gewichte extrahieren und aneinanderhängen
  w_mvp <- extract_weights(mvp, ret_df, y, "Min Variance")
  w_p2  <- extract_weights(p2, ret_df, y, label_p2)
  w_p3  <- extract_weights(p3, ret_df, y, label_p3)
  w_p4  <- extract_weights(p4, ret_df, y, label_p4)
  w_p5  <- extract_weights(p5, ret_df, y, label_p5)
  
  # Gewichte in der Liste speichern
  all_weights[[y]] <- rbind(w_mvp, w_p2, w_p3, w_p4, w_p5)
}

# 3. Alle Jahres-Ergebnisse in die Master-Tabellen zusammenfügen
master_summary <- do.call(rbind, all_summaries)
rownames(master_summary) <- NULL

master_weights <- do.call(rbind, all_weights)
rownames(master_weights) <- NULL

# Vola aus Kovarianzmatrix ermitteln
vola_2010 <- data.frame(Jahr = "2010", Aktie = colnames(cov_matrix_2010), Volatilitat = sqrt(diag(cov_matrix_2010)))
vola_2015 <- data.frame(Jahr = "2015", Aktie = colnames(cov_matrix_2015), Volatilitat = sqrt(diag(cov_matrix_2015)))
vola_2020 <- data.frame(Jahr = "2020", Aktie = colnames(cov_matrix_2020), Volatilitat = sqrt(diag(cov_matrix_2020)))
vola_2025 <- data.frame(Jahr = "2025", Aktie = colnames(cov_matrix_2025), Volatilitat = sqrt(diag(cov_matrix_2025)))

# Alle Vola-Jahre zusammenfügen
master_vola <- rbind(vola_2010, vola_2015, vola_2020, vola_2025)
rownames(master_vola) <- NULL

# Momentum berechnen (Funktion auf alle Jahre)
mom_2010 <- data.frame(Jahr = "2010", Aktie = colnames(return_2010), Momentum = as.numeric(calculate_momentum(return_2010)))
mom_2015 <- data.frame(Jahr = "2015", Aktie = colnames(return_2015), Momentum = as.numeric(calculate_momentum(return_2015)))
mom_2020 <- data.frame(Jahr = "2020", Aktie = colnames(return_2020), Momentum = as.numeric(calculate_momentum(return_2020)))
mom_2025 <- data.frame(Jahr = "2025", Aktie = colnames(return_2025), Momentum = as.numeric(calculate_momentum(return_2025)))

# Alle Momentum-Jahre zusammenfügen
master_momentum <- rbind(mom_2010, mom_2015, mom_2020, mom_2025)
rownames(master_momentum) <- NULL

# als CSV speichern
write.csv2(master_weights, "data/Portfolio_Gewichte_Master.csv", row.names = FALSE)
write.csv2(master_summary, "data/Portfolio_Summary_Master.csv", row.names = FALSE)
write.csv2(master_vola, "data/Portfolio_Volatilitat_Master.csv", row.names = FALSE)
write.csv2(master_momentum, "data/Portfolio_Momentum_Master.csv", row.names = FALSE)

# Abschlussmeldung
cat("Berechnung abgeschlossen. Daten wurden erfolgreich exportiert!\n")

# Available-Case vs. Complete-Case (na.omit)
# Exemplarisch für das min-var-portfolio aus 2025
cat("\nStarte Gewichtevergleich für 2025...\n")

# neue Erwartungswerte für die CC-Datene berechnen
exp_return_2025_cc <- colMeans(return_2025_cc) * 252

# Min-Var-Portfolio mit CC-Daten versuchen zu berechnen (in 'try' gewrappt)
mvp_2025_cc_result <- try(min_var_portfolio(exp_return_2025_cc, cov_matrix_2025_cc), silent = TRUE)

# Prüfen, ob der Optimierer mit der na.omit-Matrix abstürzt
if(!inherits(mvp_2025_cc_result, "try-error") && !is.na(mvp_2025_cc_result$risk[1])) {
  
  # Gewichte extrahieren
  w_mvp_2025_cc <- extract_weights(mvp_2025_cc_result, return_2025_cc, "2025", "Min Variance (CC)")
  
  # Gewichte mit der normalen Methode mergen (wir filtern sie direkt aus master_weights)
  w_mvp_2025_normal <- master_weights[master_weights$Jahr == "2025" & master_weights$Portfolio_Typ == "Min Variance", ]
  
  vergleich_gewichte <- merge(
    x = w_mvp_2025_normal[, c("Aktie", "Gewicht")],
    y = w_mvp_2025_cc[, c("Aktie", "Gewicht")],
    by = "Aktie",
    all = TRUE,
    suffixes = c("_Normal", "_na_omit")
  )
  
  # NAs durch 0 ersetzen (Aktien, die in der einen Methode gekauft wurden, in der anderen aber nicht)
  vergleich_gewichte[is.na(vergleich_gewichte)] <- 0
  
  # Absolute Differenz berechnen
  vergleich_gewichte$Differenz_absolut <- abs(vergleich_gewichte$Gewicht_Normal - vergleich_gewichte$Gewicht_na_omit)
  
  # Nach größter Differenz absteigend sortieren
  vergleich_gewichte <- vergleich_gewichte[order(-vergleich_gewichte$Differenz_absolut), ]
  
  # Konsole ausgeben und exportieren
  cat("\n--- GEWICHTEVERGLEICH: NORMAL VS. NA.OMIT (MVP 2025) ---\n")
  print(head(vergleich_gewichte, 15)) # Zeigt die Top 15 Abweichungen
  write.csv2(vergleich_gewichte, "data/Gewichtevergleich_MVP_2025.csv", row.names = FALSE)
  cat("Der Vergleich wurde als 'Gewichtevergleich_MVP_2025.csv' gespeichert!\n")
  
} else {
  # Prüfung ob na.omit möglich
  cat("\n=========================================================================\n")
  cat("ACHTUNG: Der Gewichtevergleich konnte nicht durchgeführt werden!\n")
  cat("Grund: Die na.omit Matrix für 2025 ist mathematisch kollabiert (nicht positiv definit).\n")
  cat("Complete-Case Analysis hier nicht anwendbar ist!\n")
  cat("=========================================================================\n")
}

# =========================================================================
# EXPORT DER ERWARTETEN RENDITEN (für Effizienzgrenzendarstellung)
# =========================================================================

# Vektoren in ein Dataframe umwandeln und direkt als CSV speichern
df_expected_returns <- bind_rows(
  enframe(exp_return_2010, name = "Aktie", value = "Exp_Return") %>% mutate(Jahr = "2010"),
  enframe(exp_return_2015, name = "Aktie", value = "Exp_Return") %>% mutate(Jahr = "2015"),
  enframe(exp_return_2020, name = "Aktie", value = "Exp_Return") %>% mutate(Jahr = "2020"),
  enframe(exp_return_2025, name = "Aktie", value = "Exp_Return") %>% mutate(Jahr = "2025")
)

write_csv2(df_expected_returns, "data/Portfolio_ExpectedReturns_Master.csv")
