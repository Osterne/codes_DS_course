# ============================================================
# MCMC
# Prof. Vinícius Osterne, PhD
# ============================================================

set.seed(123)

# ------------------------------------------------------------
# BLOCO 1 — O PROBLEMA: amostragem ingênua
# Motivação: por que não simplesmente jogar pontos aleatórios?
# ------------------------------------------------------------

# Distribuição alvo: mistura bimodal (difícil de amostrar diretamente)
f_alvo <- function(x) {
  0.9 * dnorm(x, mean = -1.2, sd = sqrt(0.35 / 2)) +
    0.55 * dnorm(x, mean =  1.4, sd = sqrt(0.70 / 2))
}

# Amostragem ingênua: pontos uniformes no intervalo
n_ingenua <- 500
x_ingenua <- runif(n_ingenua, -3, 3)
y_ingenua <- runif(n_ingenua, 0, 1.6)

x_grid <- seq(-3, 3, length.out = 500)

par(mfrow = c(1, 1))
plot(x_grid, f_alvo(x_grid),
     type = "l", lwd = 2,
     main = "Amostragem ingênua: pontos espalhados sem critério",
     xlab = expression(theta), ylab = "densidade")
points(x_ingenua, y_ingenua, col = "red", pch = 16, cex = 0.5)


# ------------------------------------------------------------
# BLOCO 2 — AMOSTRAGEM POR REJEIÇÃO
# Aceita apenas pontos abaixo da curva
# Funciona em 1D, mas piora muito em alta dimensão
# ------------------------------------------------------------

M <- 1.6

aceitos_x    <- x_ingenua[y_ingenua <= f_alvo(x_ingenua)]
rejeitados_x <- x_ingenua[y_ingenua >  f_alvo(x_ingenua)]
aceitos_y    <- y_ingenua[y_ingenua <= f_alvo(x_ingenua)]
rejeitados_y <- y_ingenua[y_ingenua >  f_alvo(x_ingenua)]

round(length(aceitos_x) / n_ingenua * 100, 1) # Taxa de aceitação (%)

plot(x_grid, f_alvo(x_grid),
     type = "l", lwd = 2,
     main = "Amostragem por rejeição",
     xlab = expression(theta), ylab = "densidade")
rect(-3, 0, 3, M, border = "darkgreen", lty = 2)
points(rejeitados_x, rejeitados_y, col = "red",       pch = 16, cex = 0.5)
points(aceitos_x,    aceitos_y,    col = "darkgreen",  pch = 16, cex = 0.7)
legend("topright",
       legend = c("Rejeitado", "Aceito"),
       col    = c("red", "darkgreen"),
       pch    = 16, bty = "n")


# ------------------------------------------------------------
# BLOCO 3 — CADEIA DE MARKOV SIMPLES
# Propriedade de Markov: o futuro depende apenas do presente
# Cadeia AR(1) com distribuição estacionária Normal(0,1)
# ------------------------------------------------------------

N   <- 5000
rho <- 0.9

theta_markov    <- numeric(N)
theta_markov[1] <- 10             # ponto inicial longe do centro

for (t in 2:N) {
  epsilon         <- rnorm(1, mean = 0, sd = sqrt(1 - rho^2))
  theta_markov[t] <- rho * theta_markov[t - 1] + epsilon
}

plot(theta_markov, type = "l",
     main = "Trajetória da cadeia de Markov (AR1)",
     xlab = "Iteração", ylab = expression(theta),
     col  = "steelblue")
abline(h = 0, col = "red", lwd = 2, lty = 2)

burnin <- 200
abline(v = burnin, col = "orange", lwd = 2, lty = 2)
legend("topright",
       legend = c("Cadeia", "Média teórica", "Fim burn-in"),
       col    = c("steelblue", "red", "orange"),
       lty    = c(1, 2, 2), lwd = 2, bty = "n")


# ------------------------------------------------------------
# BLOCO 4 — BURN-IN E DISTRIBUIÇÃO ESTACIONÁRIA
# Descartamos o burn-in e usamos o restante
# ------------------------------------------------------------

theta_pos <- theta_markov[(burnin + 1):N]

par(mfrow = c(1, 2))

plot(theta_markov[1:burnin], type = "l", col = "red",
     main = "Burn-in (descartado)",
     xlab = "Iteração", ylab = expression(theta))

plot(theta_pos, type = "l", col = "darkgreen",
     main = "Pós burn-in (usado)",
     xlab = "Iteração", ylab = expression(theta))
abline(h = 0, col = "gray50", lty = 2)

par(mfrow = c(1, 1))

hist(theta_pos, breaks = 50, probability = TRUE,
     main = "Amostras pós burn-in vs Normal(0,1) teórica",
     xlab = expression(theta), col = "lightblue", border = "white")
curve(dnorm(x, 0, 1), add = TRUE, lwd = 2, col = "red")
legend("topright",
       legend = c("Amostras MCMC", "Normal(0,1) teórica"),
       col    = c("lightblue", "red"),
       lty    = c(NA, 1), pch = c(15, NA),
       lwd    = c(NA, 2), bty = "n")

round(mean(theta_pos), 4) # Média estimada   | Teórica: 0
round(var(theta_pos),  4) # Variância estimada | Teórica: 1


# ------------------------------------------------------------
# BLOCO 5 — DISTRIBUIÇÃO ESTACIONÁRIA: MÚLTIPLAS CADEIAS
# Cadeias de pontos iniciais diferentes convergem para o mesmo lugar
# ------------------------------------------------------------

N_multi <- 300
inicios <- c(-5, 0, 8)
cores   <- c("red", "blue", "darkgreen")

plot(NULL, xlim = c(1, N_multi), ylim = c(-4, 10),
     main = "Múltiplas cadeias convergindo para Normal(0,1)",
     xlab = "Iteração", ylab = expression(theta))
abline(h = 0, col = "gray70", lty = 2)

for (i in seq_along(inicios)) {
  cadeia    <- numeric(N_multi)
  cadeia[1] <- inicios[i]
  for (t in 2:N_multi) {
    prop      <- cadeia[t - 1] + rnorm(1, 0, 1)
    log_a     <- dnorm(prop, 0, 1, log = TRUE) - dnorm(cadeia[t-1], 0, 1, log = TRUE)
    cadeia[t] <- if (runif(1) < exp(log_a)) prop else cadeia[t - 1]
  }
  lines(cadeia, col = cores[i], lwd = 1.5)
}

legend("topright",
       legend = paste("Início =", inicios),
       col    = cores, lty = 1, lwd = 2, bty = "n")


# ------------------------------------------------------------
# BLOCO 6 — EQUILÍBRIO DETALHADO (verificação empírica)
# pi(x) * P(x -> y) ≈ pi(y) * P(y -> x)
# ------------------------------------------------------------

theta_seq <- theta_pos
regiao    <- ifelse(theta_seq < 0, "A", "B")

trans_AB <- sum(regiao[-length(regiao)] == "A" & regiao[-1] == "B")
trans_BA <- sum(regiao[-length(regiao)] == "B" & regiao[-1] == "A")

trans_AB                       # Transições A → B
trans_BA                       # Transições B → A
round(trans_AB / trans_BA, 3)  # Razão (deve ser ≈ 1)

