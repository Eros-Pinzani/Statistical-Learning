# Script per il primo homework

rm(list = ls()) # per pulire l'ambiente

# Obiettivo: problema della variabile omessa. Noi valutiamo come la correlazione tra due variabili aleatorie nel nostro caso Gaussiane
# influiscano nella generazione del modello (lm) e l'assenza di tale generazione crei un disallineamento della stima effettiva di beta1 

library(MASS)

# Parametri 
n    <- 1000 #numerosità campionaria
nsim <- 2000 # numero di simulazioni

beta0 <- 1
beta1 <- 2
beta2 <- -2

Sigma1 <- matrix(c(1,   0.5,
                   0.5, 1),  nrow = 2, ncol = 2)  # variabili correlate
Sigma2 <- matrix(c(1,   0,
                   0,   1),  nrow = 2, ncol = 2)  # variabili indipendenti

# ── Funzioni ───────────────────────────────────────────────────────────────────
genData <- function(sigma, n) {
  x    <- mvrnorm(n = n, mu = c(0, 0), Sigma = sigma)
  epsi <- rnorm(n = n, 0, 1)
  x1   <- x[, 1]
  x2   <- x[, 2]
  y    <- beta0 + beta1*x1 + beta2*x2 + epsi
  return(list(y = y, x1 = x1, x2 = x2)) # generazione dati
}

genModel <- function(data) {
  m1 <- lm(data$y ~ data$x1)              # modello con variabile omessa
  m2 <- lm(data$y ~ data$x1 + data$x2)   # modello completo
  return(list(m1 = m1, m2 = m2)) #generazione dati
}

# ── Simulazione Monte Carlo ────────────────────────────────────────────────────
beta1_omitted_s1 <- c()
beta1_full_s1    <- c()
beta1_omitted_s2 <- c()
beta1_full_s2    <- c()

for (i in 1:nsim) {
  data1 <- genData(Sigma1, n)
  data2 <- genData(Sigma2, n)
  
  mods1 <- genModel(data1) #modell1 scenario 1
  mods2 <- genModel(data2) #modelli scenario 2
  
  beta1_omitted_s1[i] <- coef(mods1$m1)["data$x1"]
  beta1_full_s1[i]    <- coef(mods1$m2)["data$x1"]
  beta1_omitted_s2[i] <- coef(mods2$m1)["data$x1"]
  beta1_full_s2[i]    <- coef(mods2$m2)["data$x1"]
}

# ── Medie ──────────────────────────────────────────────────────────────────────
mean(beta1_omitted_s1)
mean(beta1_full_s1)
mean(beta1_omitted_s2)
mean(beta1_full_s2)


results <- data.frame(
  Scenario = c("Sigma1 (correlate)", "Sigma2 (indipendenti)"),
  Omitted  = round(c(mean(beta1_omitted_s1), mean(beta1_omitted_s2)), 4),
  Full     = round(c(mean(beta1_full_s1),    mean(beta1_full_s2)),    4),
  True_beta = c(beta1, beta1)
)

results
# ── Plot ───────────────────────────────────────────────────────────────────────
col_fill_omit <- "#D3EDEC"
col_fill_full <- "#D3EDEC"
col_dens_omit <- "#990F3D"
col_dens_full <- "#0D7680"
col_true      <- "#593380"

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# Scenario 1 — Omitted Variable
hist(beta1_omitted_s1, probability = TRUE,
     main = "Scenario 1: Omitted Variable",
     xlab = expression(hat(beta)[1]),
     col = col_fill_omit, border = "white", breaks = 30,
     xlim = c(min(beta1_omitted_s1), max(beta1 + 0.2, max(beta1_omitted_s1))))
lines(density(beta1_omitted_s1), col = col_dens_omit, lwd = 2.5)
abline(v = beta1,                col = col_true,      lwd = 2, lty = 2)
abline(v = mean(beta1_omitted_s1), col = col_dens_omit, lwd = 2, lty = 1)
legend("topright",
       legend = c("Densità", expression(beta[1] == 1), expression(E(hat(beta)[1]))),
       col = c(col_dens_omit, col_true, col_dens_omit),
       lty = c(1, 2, 1), lwd = 2, bty = "n", cex = 0.8)

# Scenario 1 — Full Model
hist(beta1_full_s1, probability = TRUE,
     main = "Scenario 1: Full Model",
     xlab = expression(hat(beta)[1]),
     col = col_fill_full, border = "white", breaks = 30)
lines(density(beta1_full_s1), col = col_dens_full, lwd = 2.5)
abline(v = beta1,              col = col_true,      lwd = 2, lty = 2)
abline(v = mean(beta1_full_s1), col = col_dens_full, lwd = 2, lty = 1)
legend("topright",
       legend = c("Densità", expression(beta[1] == 1), expression(E(hat(beta)[1]))),
       col = c(col_dens_full, col_true, col_dens_full),
       lty = c(1, 2, 1), lwd = 2, bty = "n", cex = 0.8)

# Scenario 2 — Omitted Variable
hist(beta1_omitted_s2, probability = TRUE,
     main = "Scenario 2: Omitted Variable",
     xlab = expression(hat(beta)[1]),
     col = col_fill_omit, border = "white", breaks = 30)
lines(density(beta1_omitted_s2), col = col_dens_omit, lwd = 2.5)
abline(v = beta1,                col = col_true,      lwd = 2, lty = 2)
abline(v = mean(beta1_omitted_s2), col = col_dens_omit, lwd = 2, lty = 1)
legend("topright",
       legend = c("Densità", expression(beta[1] == 1), expression(E(hat(beta)[1]))),
       col = c(col_dens_omit, col_true, col_dens_omit),
       lty = c(1, 2, 1), lwd = 2, bty = "n", cex = 0.8)

# Scenario 2 — Full Model
hist(beta1_full_s2, probability = TRUE,
     main = "Scenario 2: Full Model",
     xlab = expression(hat(beta)[1]),
     col = col_fill_full, border = "white", breaks = 30)
lines(density(beta1_full_s2), col = col_dens_full, lwd = 2.5)
abline(v = beta1,              col = col_true,      lwd = 2, lty = 2)
abline(v = mean(beta1_full_s2), col = col_dens_full, lwd = 2, lty = 1)
legend("topright",
       legend = c("Densità", expression(beta[1] == 1), expression(E(hat(beta)[1]))),
       col = c(col_dens_full, col_true, col_dens_full),
       lty = c(1, 2, 1), lwd = 2, bty = "n", cex = 0.8)


# valutiamo mantenendo i valori di beta ottenuti uguali come varia il bias in base alla correlazione della matrice
# Calcolo mean (beta1-omitted) - beta1
# non usiamo bias teorico, ma ottenuto in base alle simulazioni 

par(mfrow = c(1, 1))  # ripristina il layout standard: un grafico per finestra

# Griglia di correlazioni
rhos <- seq(-0.95, 0.95, by = 0.05)

# griglia di beta2
betas2 <- c(-2, -1, 1, 2)

results <- vector("list", length(betas2)) # ci salveremo i risultati della bias per cambio di beta2


for (k in seq_along(betas2)) {
  beta2 <<- betas2[k]
  bias_omitted <- c()
  for (j in seq_along(rhos)) {
    
    rho <- rhos[j]
    
    Sigma_rho <- matrix(c(1,   rho,
                          rho, 1), nrow = 2, ncol = 2)
    
    beta1_omitted_rho <- c()
    
    for (i in 1:nsim) {
      data_rho <- genData(Sigma_rho, n)
      mods_rho <- genModel(data_rho)
      beta1_omitted_rho[i] <- coef(mods_rho$m1)["data$x1"]
    }
    
    bias_omitted[j] <- mean(beta1_omitted_rho) - beta1
  }
  results[[k]] <- bias_omitted # salva l'intero vettore
}


#notiamo che l'andamento è lineare all'aumentare diminuisce il bias raggiunge lo zero esclusivamente a p=0

#Palette colori per i 4 valori di beta2
palette_b2 <- c("-2" = "#990F3D",   # rosso scuro
                "-1" = "#D4542A",   # arancio
                "1"  = "#0D7680",   # teal
                "2"  = "#593380")   # viola

lty_b2     <- c("-2" = 1, "-1" = 2, "1" = 3, "2" = 4)

#  Calcolo ylim globale su tutti i bias
all_bias <- unlist(results)
ylim_range <- c(min(all_bias) - 0.1, max(all_bias) + 0.1)

# Plot
plot(NA,
     xlim = range(rhos),
     ylim = ylim_range,
     xlab = expression(rho),
     ylab = expression("Bias di " * hat(beta)[1]),
     main = expression("Bias di " * hat(beta)[1] * " al variare di " * rho * " per diversi " * beta[2]))

abline(h = 0, col = "gray60", lty = 2)
abline(v = 0, col = "gray60", lty = 2)

# Punti evidenziati
rhos_hi <- c(-0.95, -0.5, -0.2, 0, 0.2, 0.5, 0.95)
# Linee per ogni beta2
for (k in seq_along(betas2)) {
  
  bk <-results[[k]]
  
  lines(rhos, bk,
        col = palette_b2[as.character(betas2[k])],
        lwd = 2.5,
        lty = lty_b2[as.character(betas2[k])])
  
  
  idx_hi  <- sapply(rhos_hi, function(r) which.min(abs(rhos - r)))
  points(rhos_hi, bk[idx_hi],
         pch = 19, cex = 1.2, col = palette_b2[as.character(betas2[k])])
  
  text(rhos[idx_hi], bk[idx_hi],          # ← stessa x e y dei punti
       labels = round(bk[idx_hi], 2),
       pos    = 3,                         # sopra il punto
       cex    = 0.75,
       col    = palette_b2[as.character(betas2[k])])
}

# Legenda 
legend("bottom",
       legend = paste0("β2 = ", betas2),
       col    = palette_b2[as.character(betas2)],
       lty    = lty_b2[as.character(betas2)],
       lwd    = 2,
       pch    = 19,
       bty    = "n",
       title  = expression(beta[2]))

str(results)
length(rhos)
# Verifica
#cov(xs)        # dovrebbe essere vicina a Sigma
#cor(xs)        # correlazione empirica anche essa vicino a Sigma
#colMeans(xs)   # deve restituire un vettore (0, 0)



# plot della distribuzione del bias valutare se valutare in abs o no
plot(rhos, bias_omitted,
     type = "l", lwd = 2.5,
     col  = col_dens_omit,
     xlab = expression(rho),
     ylab = "Bias",
     main = expression("Bias di " * hat(beta)[1] * " al variare di " * rho),
     ylim = c(min(bias_omitted) - 0.05, max(bias_omitted) + 0.05))

lines(rhos, (bias_omitted), col = col_true,      lwd = 2, lty = 2)
abline(h = 0, col = "gray50", lty = 2)
abline(v = 0, col = "gray50", lty = 2)

#  Punti di interesse 
rhos_highlight <- c(-0.95, -0.5, -0.2, 0, 0.2, 0.5, 0.95)

idx_highlight <- sapply(rhos_highlight, function(r) which.min(abs(rhos - r)))

bias_highlight <- bias_omitted[idx_highlight]

points(rhos_highlight, bias_highlight,
       pch = 19, cex = 1.5, col = col_dens_omit)

text(rhos_highlight, bias_highlight,
     labels = round(bias_highlight, 2),
     pos    = 3,
     cex    = 0.85,
     col    = col_dens_omit)

legend("topright",
       legend = c("Bias simulato (omitted)"),
       col    = c(col_dens_omit),
       lty    = c(1, 2, 3),
       lwd    = 2,
       bty    = "n")
