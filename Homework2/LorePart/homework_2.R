# Inizializzazione
rm(list=ls(all=TRUE))

#install.packages("ks")
#install.packages("LPCM")
library(MASS)
library(ks)
data1d <- geyser[, 1]  # Old Faithful: waiting times

par(mfrow = c(1, 3))
for (bw in c(0.2, 4, 15)) {
  plot(density(data1d, bw = bw),
       main = paste("h =", bw),
       col = "steelblue", lwd = 2,
       ylim = c(0, 0.05))
  rug(data1d, col = "gray50")
}

data1d <- geyser[, 1]

h_silverman <- hns(data1d)      # regola di Silverman
h_cv        <- hlscv(data1d)    # cross-validation
h_plugin    <- hpi(data1d)      # plug-in

plot(density(data1d, bw = h_silverman), col = "dodgerblue",
     lwd = 2, main = "Confronto bandwidth selectors",
     ylim = c(0, 0.045))
lines(density(data1d, bw = h_cv),     col = "limegreen",  lwd = 2)
lines(density(data1d, bw = h_plugin), col = "firebrick",   lwd = 2)
legend("topleft",
       legend = c(paste("Silverman:", round(h_silverman, 2)),
                  paste("CV:",        round(h_cv, 2)),
                  paste("Plug-in:",   round(h_plugin, 2))),
       col = c("dodgerblue", "limegreen", "firebrick"), lwd = 2)


# --- Confidence band bootstrap ---
library(MASS)
data1d <- geyser[, 1]
h1 <- 4; n1 <- length(data1d)
kde0 <- density(data1d, bw = h1, from = 40, to = 110)

n_BT <- 500
sup_dev <- numeric(n_BT)
for (j in 1:n_BT) {
  bt <- data1d[sample(n1, n1, replace = TRUE)]
  kde_bt <- density(bt, bw = h1, from = 40, to = 110)
  sup_dev[j] <- max(abs(kde_bt$y - kde0$y))
}
t_band <- quantile(sup_dev, 0.95)

plot(kde0, lwd = 2, col = "purple",
     main = "95% Confidence Band (Bootstrap)",
     ylim = c(-0.005, 0.05))
polygon(c(kde0$x, rev(kde0$x)),
        c(kde0$y + t_band, rev(kde0$y - t_band)),
        col = adjustcolor("plum1", 0.5), border = NA)
lines(kde0, lwd = 2, col = "purple")
abline(h = 0)

# --- KDE 2D con contour ---
library(KernSmooth)
data2d <- cbind(data1d[1:(n1-1)], data1d[2:n1])
kde2d  <- bkde2D(data2d, bandwidth = 5.5)
contour(kde2d$x1, kde2d$x2, kde2d$fhat,
        col = "blue", lwd = 1.5,
        xlab = "Current waiting time",
        ylab = "Next waiting time",
        main = "2D KDE — Old Faithful")
points(data2d, pch = 16, cex = 0.5)

library(MASS)
set.seed(42)

# Vera densità: mixture di due gaussiane
n <- 200
x_true <- c(rnorm(100, mean = 0), rnorm(100, mean = 4))

# Griglia di bandwidth da testare
h_grid <- seq(0.1, 3, length.out = 50)

# Calcola MISE approssimato via integrazione numerica
mise <- numeric(length(h_grid))
x_eval <- seq(-4, 8, length.out = 500)

# vera densità
p_true <- 0.5 * dnorm(x_eval, 0, 1) + 0.5 * dnorm(x_eval, 4, 1)

for (i in seq_along(h_grid)) {
  kde_i <- density(x_true, bw = h_grid[i], 
                   from = -4, to = 8, n = 500)
  mise[i] <- mean((kde_i$y - p_true)^2)
}

plot(h_grid, mise, type = "l", lwd = 2, col = "firebrick",
     xlab = "Bandwidth h", ylab = "MISE approssimato",
     main = "Bias-Variance Tradeoff")
abline(v = h_grid[which.min(mise)], lty = 2, col = "blue")
legend("topright",
       legend = paste("h ottimale =", round(h_grid[which.min(mise)], 2)),
       col = "blue", lty = 2)

data1d <- geyser[, 1]

par(mfrow = c(1, 2))

# Istogramma classico
hist(data1d, breaks = 20, freq = FALSE,
     main = "Istogramma", col = "lightblue",
     xlab = "Waiting time")

# KDE
plot(density(data1d, bw = bw.SJ(data1d)),
     main = "KDE (Sheather-Jones)",
     col = "steelblue", lwd = 2)
rug(data1d, col = "gray50")


par(mfrow = c(2, 2))

# vera densità: mixture gaussiana
p_true <- function(x) 0.5*dnorm(x,0,1) + 0.5*dnorm(x,4,1)

for (n in c(50, 200, 500, 2000)) {
  x <- c(rnorm(n/2, 0, 1), rnorm(n/2, 4, 1))
  kde <- density(x, bw = bw.SJ(x), from = -4, to = 8)
  
  plot(kde, main = paste("n =", n),
       col = "steelblue", lwd = 2,
       ylim = c(0, 0.25), xlab = "x")
  
  # vera densità in rosso
  curve(p_true(x), from = -4, to = 8,
        col = "red", lwd = 2, lty = 2, add = TRUE)
  
  legend("topright",
         legend = c("KDE", "Vera densità"),
         col = c("steelblue", "red"),
         lwd = 2, lty = c(1, 2), cex = 0.7)
}
# Homework effettivo

# set seed
set.seed(42)

#DGP
true_dens <- function(x){
  0.4* dnorm(x, -2, 1) + 0.6*dnorm(x, 3, 0.5)
}

# Genera campione dalla mistura
n <- 500
B <- 1000                    # Numero di simulazioni Monte Carlo
u <- runif(n)

# Funzione per generare un nuovo campione dalla mistura (necessaria per il MISE effettivo)
generate_mixture <- function(n) {
  u <- runif(n)
  x <- ifelse(u < 0.4, 
              rnorm(n, mean = -2, sd = 1), 
              rnorm(n, mean = 3, sd = 0.5))
  return(x)
}

x_sample <- generate_mixture(n)

# Griglia per plottare la vera densità
x_grid <- seq(-6, 6, length.out = 1000)

# Plot
plot(x_grid, true_dens(x_grid),
     type = "l", lwd = 2, col = "black",
     main = "Mistura Bimodale — Vera Densità",
     xlab = "x", ylab = "Densità",
     ylim = c(0, 0.55))
rug(x_sample, col = "gray50")

# Confronto sparando a caso valori di h

# Tre bandwidth da confrontare
bandwidths <- c(0.01, 0.3, 2)
titoli <- c("h = 0.01 (undersmoothing)",
            "h = 0.3 (buona scelta)",
            "h = 2.0 (oversmoothing)")

for (i in seq_along(bandwidths)) {
  
  kde_i <- density(x_sample, bw = bandwidths[i],
                   from = -6, to = 6, n = 1000)
  
  # area sotto la KDE (shading)
  plot(kde_i, main = titoli[i],
       col  = "steelblue", lwd  = 2,
       ylim = c(0, 0.65),
       xlab = "x", ylab = "Densità",
       zero.line = FALSE)
  
  polygon(c(kde_i$x, rev(kde_i$x)),
          c(kde_i$y, rep(0, length(kde_i$y))),
          col    = adjustcolor("steelblue", alpha.f = 0.15),
          border = NA)
  
  # vera densità in rosso sopra
  lines(x_grid, true_dens(x_grid),
        col = "red", lwd = 2, lty = 2)
  
  rug(x_sample, col = adjustcolor("gray40", 0.3))
  
  legend("topleft",
         legend = c("KDE", "Vera densità"),
         col    = c("steelblue", "red"),
         lwd    = 2, lty = c(1, 2),
         cex    = 0.85, bty = "n")
  
}


#Valutiamo il MISE in base al variare al valore h (possiamo farlo solo perché sappiamo il GDP)


par(mfrow = c(1, 1))  # ripristina il layout standard: un grafico per finestra

x_eval <- seq(-6, 6, length.out = 1000)
p_true <- true_dens(x_eval)
# Griglia di bandwidth da testare
h_grid <- seq(0.01, 2.5, length.out = 100)

# Matrice per salvare gli ISE (Integrated Squared Error)
# Ogni riga è una simulazione, ogni colonna è un valore di h
ise_matrix <- matrix(0, nrow = B, ncol = length(h_grid))

# --- 2. CICLO MONTE CARLO PER IL MISE EFFETTIVO ---
cat("Inizio simulazioni Monte Carlo...\n")

for (b in 1:B) {
  # Genero un NUOVO campione a ogni ciclo
  x_sample_b <- generate_mixture(n)
  
  for (i in seq_along(h_grid)) {
    # Stima KDE con l'h corrente
    kde_i <- density(x_sample_b, bw = h_grid[i], from = -6, to = 6, n = 1000)
    
    # Calcolo ISE: media degli errori al quadrato tra stima e verità
    ise_matrix[b, i] <- mean((kde_i$y - p_true)^2)
  }
  
  if(b %% 20 == 0) cat("Completate", b, "simulazioni su", B, "\n")
}

# Calcolo del MISE (Media degli ISE sulle righe)
mise <- colMeans(ise_matrix)

# h ottimale = quello che minimizza il MISE
h_opt <- h_grid[which.min(mise)]

# altri metodi di selezione bandwidth
h_sm <- bw.nrd0(x_sample)   # Silverman
h_sc <- bw.nrd(x_sample)    # Scott
h_sj <- bw.SJ(x_sample)     # Sheather-Jones
h_cv <- bw.ucv(x_sample)    # cross-validation

cat("h ottimale (MISE):  ", round(h_opt, 4), "\n")
cat("h Silverman:        ", round(h_sm,  4), "\n")
cat("h Scott:            ", round(h_sc, 4), "\n")
cat("h Sheather-Jones:   ", round(h_sj,  4), "\n")
cat("h Cross-Validation: ", round(h_cv,  4), "\n")

# Creazione del data.frame
tabella_h <- data.frame(
  Metodo = c("Ottimale (MISE)", "Silverman", "Scott","Sheather-Jones", "Cross-Validation"),
  Bandwidth = c(h_opt, h_sm, h_sc, h_sj, h_cv)
)

# Arrotondamento per una visualizzazione pulita
tabella_h$Bandwidth <- round(tabella_h$Bandwidth, 4)

# Visualizzazione
print(tabella_h)


plot(h_grid, mise, type = "l", lwd = 2, col = "gray20",
     xlab = "Bandwidth h", ylab = "MISE approssimato",
     main = "Selezione del Bandwidth")

# linee verticali per ogni metodo
abline(v = h_opt, col = "red",       lwd = 2, lty = 1)
abline(v = h_sm,  col = "blue",      lwd = 2, lty = 2)
abline(v = h_sc, col = "purple", lwd = 2, lty = 3)
abline(v = h_sj,  col = "darkgreen", lwd = 2, lty = 4)
abline(v = h_cv,  col = "orange",    lwd = 2, lty = 5)

legend("bottomright",
       legend = c(paste("h opt (MISE) =", round(h_opt, 3)),
                  paste("h Silverman  =", round(h_sm,  3)),
                  paste("h Scott =", round(h_sc, 3)),
                  paste("h S-J        =", round(h_sj,  3)),
                  paste("h CV         =", round(h_cv,  3))),
       col    = c("red", "blue", "purple","darkgreen", "orange"),
       lwd    = 2, lty = 1:4, bty = "n")
