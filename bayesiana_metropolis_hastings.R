# ============================================================
# Metropolis-Hastings
# Prof. Vinícius Osterne, PhD
# ============================================================

set.seed(123)


# ------------------------------------------------------------
# BLOCO 1 — O PROBLEMA: avaliar sem amostrar
# Temos uma densidade que conseguimos avaliar, mas não amostrar
# ------------------------------------------------------------

pi_alvo <- function(theta) {
  if (theta <= 0 || theta >= 1) return(0)
  theta^7 * (1 - theta)^3
}

theta_grid <- seq(0.01, 0.99, length.out = 300)
densidade  <- sapply(theta_grid, pi_alvo)

plot(theta_grid, densidade,
     type = "l", lwd = 2, col = "steelblue",
     main = "Distribuição alvo: pi(theta) ∝ theta^7 (1-theta)^3",
     xlab = expression(theta), ylab = "densidade (não normalizada)")
curve(dbeta(x, 8, 4), add = TRUE, col = "red", lwd = 2, lty = 2)
legend("topleft",
       legend = c("Não normalizada (o que usamos)", "Beta(8,4) verdadeira"),
       col    = c("steelblue", "red"),
       lty    = c(1, 2), lwd = 2, bty = "n")

pi_alvo(0.60) # pi(theta) em theta = 0.60
pi_alvo(0.68) # pi(theta) em theta = 0.68


# ------------------------------------------------------------
# BLOCO 2 — PROPOSTA SIMÉTRICA
# q(theta' | theta) = Normal(theta, sigma²)
# ------------------------------------------------------------

sigma <- 0.1

theta_atual    <- 0.60
theta_proposto <- rnorm(1, mean = theta_atual, sd = sigma)

round(theta_proposto, 4)                              # Proposta gerada
round(dnorm(theta_proposto, theta_atual, sigma), 6)   # q(theta' | theta)
round(dnorm(theta_atual, theta_proposto, sigma), 6)   # q(theta  | theta') — igual


# ------------------------------------------------------------
# BLOCO 3 — CÁLCULO MANUAL DE ALPHA
# ------------------------------------------------------------

theta_atual    <- 0.60
theta_proposto <- 0.68

pi_atual    <- pi_alvo(theta_atual)
pi_proposto <- pi_alvo(theta_proposto)
alpha       <- min(1, pi_proposto / pi_atual)

round(pi_atual,            6) # pi(theta)
round(pi_proposto,         6) # pi(theta')
round(pi_proposto / pi_atual, 4) # Razão pi(theta') / pi(theta)
round(alpha,               4) # alpha = min(1, razão) → ACEITA

# Segundo exemplo: proposta pior
theta_atual    <- 0.68
theta_proposto <- 0.40

pi_atual    <- pi_alvo(theta_atual)
pi_proposto <- pi_alvo(theta_proposto)
alpha       <- min(1, pi_proposto / pi_atual)

round(alpha, 4) # alpha → aceita com essa probabilidade


# ------------------------------------------------------------
# BLOCO 4 — ALGORITMO METROPOLIS-HASTINGS COMPLETO
# ------------------------------------------------------------

N          <- 10000
sigma      <- 0.10
theta_mh   <- numeric(N)
theta_mh[1] <- 0.10
n_aceitos  <- 0

for (t in 2:N) {
  theta_atual    <- theta_mh[t - 1]
  theta_proposto <- rnorm(1, mean = theta_atual, sd = sigma)
  alpha <- ifelse(
    theta_proposto > 0 & theta_proposto < 1,
    min(1, pi_alvo(theta_proposto) / pi_alvo(theta_atual)),
    0
  )
  if (runif(1) < alpha) {
    theta_mh[t] <- theta_proposto
    n_aceitos   <- n_aceitos + 1
  } else {
    theta_mh[t] <- theta_atual
  }
}

round(n_aceitos / (N - 1) * 100, 1) # Taxa de aceitação (%) | Meta: 25–50%


# ------------------------------------------------------------
# BLOCO 5 — TRACE PLOT E BURN-IN
# ------------------------------------------------------------

burnin <- 1000

plot(theta_mh, type = "l", col = "steelblue",
     main = "Trace plot — trajetória completa do MH",
     xlab = "Iteração", ylab = expression(theta))
abline(v = burnin,    col = "orange", lwd = 2, lty = 2)
abline(h = 8 / (8+4), col = "red",    lwd = 1, lty = 2)
legend("bottomright",
       legend = c("Cadeia MH", "Fim burn-in", "Moda teórica (8/12)"),
       col    = c("steelblue", "orange", "red"),
       lty    = c(1, 2, 2), lwd = 2, bty = "n")

plot(theta_mh[1:200], type = "l", col = "red",
     main = "Zoom: primeiras 200 iterações (burn-in)",
     xlab = "Iteração", ylab = expression(theta))
abline(h = 8 / (8+4), col = "gray50", lty = 2)


# ------------------------------------------------------------
# BLOCO 6 — DIAGNÓSTICO PÓS BURN-IN
# ------------------------------------------------------------

theta_pos <- theta_mh[(burnin + 1):N]

plot(theta_pos, type = "l", col = "darkgreen",
     main = "Trace plot pós burn-in",
     xlab = "Iteração", ylab = expression(theta))
abline(h = mean(theta_pos), col = "gray40", lty = 2)

hist(theta_pos, breaks = 60, probability = TRUE,
     col  = "lightblue", border = "white",
     main = "MH pós burn-in vs Beta(8,4) teórica",
     xlab = expression(theta))
curve(dbeta(x, 8, 4), add = TRUE, lwd = 2, col = "red")
legend("topleft",
       legend = c("Amostras MH", "Beta(8,4) teórica"),
       col    = c("lightblue", "red"),
       pch    = c(15, NA), lty = c(NA, 1),
       lwd    = c(NA, 2), bty = "n")

round(mean(theta_pos),   4) # Média estimada    | Teórica: 0.6667
round(var(theta_pos),    5) # Variância estimada | Teórica: 0.01709
round(median(theta_pos), 4) # Mediana estimada


# ------------------------------------------------------------
# BLOCO 7 — EFEITO DO TAMANHO DO PASSO
# ------------------------------------------------------------

rodar_mh <- function(sigma, N = 5000, inicio = 0.10) {
  cadeia    <- numeric(N)
  cadeia[1] <- inicio
  aceitos   <- 0
  for (t in 2:N) {
    atual    <- cadeia[t - 1]
    proposto <- rnorm(1, atual, sigma)
    alpha    <- ifelse(
      proposto > 0 & proposto < 1,
      min(1, pi_alvo(proposto) / pi_alvo(atual)),
      0
    )
    if (runif(1) < alpha) {
      cadeia[t] <- proposto
      aceitos   <- aceitos + 1
    } else {
      cadeia[t] <- atual
    }
  }
  list(cadeia = cadeia, taxa = aceitos / (N - 1))
}

sigmas <- c(0.01, 0.10, 0.80)
cores  <- c("red", "darkgreen", "orange")
labels <- c("Passo pequeno (σ=0.01)", "Passo adequado (σ=0.10)", "Passo grande (σ=0.80)")

par(mfrow = c(3, 2))

for (i in seq_along(sigmas)) {
  res <- rodar_mh(sigmas[i])
  
  plot(res$cadeia[1:500], type = "l", col = cores[i],
       main  = paste(labels[i], "— trace plot"),
       xlab  = "Iteração", ylab = expression(theta),
       ylim  = c(0, 1))
  abline(h = 8/12, col = "gray50", lty = 2)
  
  hist(res$cadeia[1001:5000], breaks = 40, probability = TRUE,
       col    = adjustcolor(cores[i], alpha.f = 0.4),
       border = "white",
       main   = paste(labels[i], "— histograma"),
       xlab   = expression(theta), xlim = c(0, 1))
  curve(dbeta(x, 8, 4), add = TRUE, lwd = 2, col = "black")
  
  round(res$taxa * 100, 1) # Taxa de aceitação (%) | Meta: 25–50%
}

par(mfrow = c(1, 1))


# ------------------------------------------------------------
# BLOCO 8 — CONSTANTE DE NORMALIZAÇÃO CANCELA
# ------------------------------------------------------------

pi_normalizada <- function(theta) dbeta(theta, 8, 4)

rodar_mh_norm <- function(pi_fn, N = 5000) {
  cadeia    <- numeric(N)
  cadeia[1] <- 0.10
  for (t in 2:N) {
    atual    <- cadeia[t - 1]
    proposto <- rnorm(1, atual, 0.10)
    alpha    <- ifelse(
      proposto > 0 & proposto < 1,
      min(1, pi_fn(proposto) / pi_fn(atual)),
      0
    )
    cadeia[t] <- if (runif(1) < alpha) proposto else atual
  }
  cadeia[1001:N]
}

amostras_norm     <- rodar_mh_norm(pi_normalizada)
amostras_nao_norm <- rodar_mh_norm(pi_alvo)

round(mean(amostras_norm),     4) # Média    | pi normalizada
round(var(amostras_norm),      5) # Variância | pi normalizada
round(mean(amostras_nao_norm), 4) # Média    | pi não normalizada
round(var(amostras_nao_norm),  5) # Variância | pi não normalizada
round(8/12,          4)           # Média teórica   Beta(8,4)
round(8*4/(12^2*13), 5)           # Variância teórica Beta(8,4)


# ------------------------------------------------------------
# BLOCO 9 — MÚLTIPLAS CADEIAS
# ------------------------------------------------------------

inicios <- c(0.05, 0.50, 0.95)
cores   <- c("red", "blue", "darkgreen")

plot(NULL, xlim = c(1, 300), ylim = c(0, 1),
     main = "Múltiplas cadeias convergindo para Beta(8,4)",
     xlab = "Iteração", ylab = expression(theta))
abline(h = 8/12, col = "gray60", lty = 2)

for (i in seq_along(inicios)) {
  res <- rodar_mh(sigma = 0.10, N = 300, inicio = inicios[i])
  lines(res$cadeia, col = cores[i], lwd = 1.5)
}

legend("topright",
       legend = paste("Início =", inicios),
       col    = cores, lty = 1, lwd = 2, bty = "n")


# ------------------------------------------------------------
# BLOCO 10 — INFERÊNCIA COM AS AMOSTRAS
# ------------------------------------------------------------

round(mean(theta_pos),   4) # Média de theta
round(median(theta_pos), 4) # Mediana de theta
round(sd(theta_pos),     4) # Desvio padrão

ic <- quantile(theta_pos, probs = c(0.025, 0.975))
round(ic, 4) # IC 95% empírico

round(mean(theta_pos > 0.70), 4) # P(theta > 0.70)
round(mean(theta_pos > 0.80), 4) # P(theta > 0.80)
round(mean(theta_pos < 0.50), 4) # P(theta < 0.50)

round(8/12,                    4) # Média teórica Beta(8,4)
round(qbeta(0.025, 8, 4),      4) # IC 95% inferior teórico
round(qbeta(0.975, 8, 4),      4) # IC 95% superior teórico
round(1 - pbeta(0.70, 8, 4),   4) # P(theta > 0.70) teórico