# Setze den Seed für Reproduzierbarkeit
set.seed(123)

# Generiere die unabhängige Variable 'indep'
indep <- runif(500, min = 1, max = 10)

# Definiere die Parameter der linearen Funktion
intercept <- 10       # Intercept ungleich 0
slope <- 1           # Steigung
residual_sd <- 2     # Streuung der Residuen

# Generiere die abhängige Variable 'depend'
depend <- intercept + slope * indep + rnorm(1000, mean = 0, sd = residual_sd)

# Kombiniere die Variablen in einen Dataframe
data <- data.frame(indep = indep, depend = depend)
data <- na.omit(data)  # Entferne Zeilen mit NA-Werten

# Daten anzeigen
head(data)


# Erstelle einen Scatterplot der Daten
plot(data$indep, data$depend,
     main = "Scatterplot der generierten Daten",
     xlab = "Independent Variable (indep)",
     ylab = "Dependent Variable (depend)",
     pch = 19, col = "blue")

# Füge die theoretische Regressionslinie hinzu
abline(a = intercept, b = slope, col = "red", lwd = 2)

# Transformationen für die Variable "indep"
data$indep_inv <- 1 / data$indep
data$indep_inv_sqrt <- 1 / sqrt(data$indep)
data$indep_inv_sq <- 1 / (data$indep^2)
data$indep_inv_cube <- 1 / (data$indep^3)
data$indep_exp <- 1.3^(data$indep)
data$indep_log <- log(data$indep)
data$indep_sqrt <- sqrt(data$indep)
data$indep_sq <- data$indep^2
#data$indep_cube <- data$indep^3

# Transformationen für die Variable "depend"
data$depend_inv <- 1 / data$depend
data$depend_inv_sqrt <- 1 / sqrt(data$depend)
data$depend_inv_sq <- 1 / (data$depend^2)
data$depend_inv_cube <- 1 / (data$depend^3)
data$depend_log <- log(data$depend)
data$depend_exp <- 1.3^(data$depend)
data$depend_sqrt <- sqrt(data$depend)
data$depend_sq <- data$depend^2
#data$depend_cube <- data$depend^3

# Liste der Transformationen für 'indep' und 'depend'
indep_transforms <- c("indep", "indep_inv", "indep_inv_sqrt", "indep_inv_sq",
                      "indep_inv_cube", "indep_exp", "indep_log", "indep_sqrt", "indep_sq")
depend_transforms <- c("depend", "depend_inv", "depend_inv_sqrt", "depend_inv_sq",
                       "depend_inv_cube", "depend_exp", "depend_log", "depend_sqrt", "depend_sq")
indep_transforms_lab <- list("x", expression(1/x), expression(1/x^2), expression(1/sqrt(x)),
                          expression(1/x^{1/3}), expression(log(x)), expression(e^x), expression(x^2), expression(sqrt(x)))
depend_transforms_lab <- c("y", expression(1/y), expression(1/y^2), expression(1/sqrt(y)),
                          expression(1/y^{1/3}), expression(log(y)), expression(e^y), expression(y^2), expression(sqrt(y)))

# # Anzahl der Transformationen
n_indep <- length(indep_transforms)
n_depend <- length(depend_transforms)
#
# # Zufällige Stichprobe von 100 Werten
# set.seed(123)  # Für Reproduzierbarkeit
# sample_indices <- sample(1:nrow(data), 100)
# sample_data <- data[sample_indices, ]
#
# # Erstelle die Grafikmatrix mit Beschriftungen
# par(mfrow = c(n_depend, n_indep), mar = c(2, 2, 2, 2), oma = c(4, 4, 2, 2)) # Rasterlayout und Platz für Beschriftungen
#
# for (i in seq_along(depend_transforms)) {
#   for (j in seq_along(indep_transforms)) {
#     # Streudiagramm für die aktuelle Kombination von 'indep' und 'depend' (mit 100 Werten)
#     plot(sample_data[[indep_transforms[j]]], sample_data[[depend_transforms[i]]],
#          xlab = "", ylab = "",
#          main = "", pch = 19, cex = 0.5, col = "blue")
#
#     # Beschriftung für die Spalten (oben)
#     if (i == 1) {
#       mtext(side = 3, text = indep_transforms[j], line = 0.5, cex = 0.8)
#     }
#
#     # Beschriftung für die Zeilen (links)
#     if (j == 1) {
#       mtext(side = 2, text = depend_transforms[i], line = 0.5, cex = 0.8, las = 1)
#     }
#   }
# }
#
# # Globale Achsentitel
# mtext("Transformierte 'indep'-Variablen", side = 1, line = 2, outer = TRUE, cex = 1.2)
# mtext("Transformierte 'depend'-Variablen", side = 2, line = 2, outer = TRUE, cex = 1.2)



# Funktion zur Berechnung eines geglätteten Medians
calculate_smoothed_median <- function(x, y, bandwidth = 8) {
  # Sortiere die Daten nach x
  sorted_indices <- order(x)
  x <- x[sorted_indices]
  y <- y[sorted_indices]

  # Gleitende Mediane berechnen und glätten
  smoothed <- ksmooth(x, y, kernel = "normal", bandwidth = bandwidth)

  # Rückgabe der geglätteten Werte
  return(data.frame(x = smoothed$x, y = smoothed$y))
}


# Funktion zur Berechnung eines adaptiven 90%-Vertrauensintervalls
calculate_adaptive_ci <- function(x, y, num_bins = 50) {
  # Sortiere x und y nach x
  sorted_indices <- order(x)
  x <- x[sorted_indices]
  y <- y[sorted_indices]

  # Berechne adaptive Bins
  bin_size <- floor(length(x) / num_bins)
  bin_indices <- rep(1:num_bins, each = bin_size, length.out = length(x))

  # Initialisiere Ergebnisse
  bin_centers <- numeric(num_bins)
  lower_bound <- numeric(num_bins)
  upper_bound <- numeric(num_bins)

  # Berechne CI für jeden Bin
  for (i in seq_len(num_bins)) {
    in_bin <- bin_indices == i
    if (sum(in_bin) > 1) {
      bin_centers[i] <- mean(x[in_bin], na.rm = TRUE)
      lower_bound[i] <- quantile(y[in_bin], 0.10, na.rm = TRUE)
      upper_bound[i] <- quantile(y[in_bin], 0.90, na.rm = TRUE)
    } else {
      bin_centers[i] <- NA
      lower_bound[i] <- NA
      upper_bound[i] <- NA
    }
  }

  # Zusammenfügen der Ergebnisse in ein Dataframe
  ci <- data.frame(x = bin_centers, lwr = lower_bound, upr = upper_bound)
  return(ci)
}

# Erstelle die Grafikmatrix mit geglätteten CI-Bändern und adaptivem Binning
par(mfrow = c(n_depend, n_indep), mar = c(0, 0, 0, 0), oma = c(2.5, 2.5, 0, 0)) # Rasterlayout und Platz für Beschriftungen

for (i in seq_along(depend_transforms)) {
  for (j in seq_along(indep_transforms)) {
    x <- data[[indep_transforms[j]]]
    y <- data[[depend_transforms[i]]]

    valid <- complete.cases(x, y)
    x <- x[valid]
    y <- y[valid]

    if (length(x) > 1) {
      ci <- calculate_adaptive_ci(x, y, num_bins = 9)
      smoothed_median <- calculate_smoothed_median(x, y, bandwidth = diff(range(x)) / 16)  # Berechne geglätteten Median
    }

    # Streudiagramm
    plot(x, y, xlab = "", ylab = "", main = "", pch = 20, cex = 0.5, col = "blue", axes = FALSE)
    box()


    # Zeichne das adaptiv geglättete CI, falls berechnet
    if (exists("ci")) {
      polygon(c(ci$x, rev(ci$x)), c(ci$lwr, rev(ci$upr)), col = adjustcolor("red", alpha.f = 0.6), border = NA)
    }

    # Zeichne die geglättete Median-Linie
    if (exists("smoothed_median")) {
      lines(smoothed_median$x, smoothed_median$y, col = "green", lwd = 2)
    }

    if (i == length(indep_transforms)) {
      mtext(side = 1, text = indep_transforms_lab[[j]], line = 1.2, cex = 0.8)
      # Hinzufügen der Achsenticks ohne Beschriftungen
      axis(1, labels = FALSE, tck = -0.02)  # X-Achse
      axis(1, labels = TRUE, line = -1, lwd = 0)

    }
    if (j == 1) {
      mtext(side = 2, text = depend_transforms_lab[[i]], line = 1.2, cex = 0.8, las = 0)
      axis(2, labels = FALSE, tck= -0.02)  # Y-Achse
      axis(2, labels = TRUE, line= -1, lwd = 0)
    }
  }
}

# mtext("Transformierte 'indep'-Variablen", side = 1, line = 2, outer = TRUE, cex = 1.2)
# mtext("Transformierte 'depend'-Variablen", side = 2, line = 2, outer = TRUE, cex = 1.2)
