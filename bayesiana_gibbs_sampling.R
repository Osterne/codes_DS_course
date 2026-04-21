# ============================================================
# Gibbs Sampling
# Prof. Vinícius Osterne, PhD
# ============================================================

set.seed(123)


# ------------------------------------------------------------
# BLOCO 1 — O PROBLEMA: distribuição conjunta vs condicionais
# ------------------------------------------------------------

mu_x <- 0
mu_y <- 0
rho  <- 0.9
sd_x <- 1
sd_y <- 1

cond_x_dado_y <- function(y) {
  media <- mu_x + rho * (sd_x / sd_y) * (y - mu_y)
  vari  <- sd_x^2 * (1 - rho^2)
  rnorm(1, mean = media, sd = sqrt(vari))
}

cond_y_dado_x <- function(x) {
  media <- mu_y + rho * (sd_y / sd_x) * (x - mu_x)
  vari  <- sd_y^2 * (1 - rho^2)
  rnorm(1, mean = media, sd = sqrt(vari))
}

rho                                                        # Correlação entre X e Y
round(mu_x + rho * (sd_x/sd_y) * (1 - mu_y), 4)          # Média condicional X | Y=1
round(mu_y + rho * (sd_y/sd_x) * (1 - mu_x), 4)          # Média condicional Y | X=1


# ------------------------------------------------------------
# BLOCO 2 — UMA ITERAÇÃO MANUAL DO GIBBS
# ------------------------------------------------------------

x_atual <- -1.8
y_atual <- -1.2

# Passo 1: fixa Y, amostra X
x_novo <- cond_x_dado_y(y_atual)
round(x_novo, 4)  # X novo | Y = -1.2

# Passo 2: fixa X novo, amostra Y
y_novo <- cond_y_dado_x(x_novo)
round(y_novo, 4)  # Y novo | X novo


# ------------------------------------------------------------
# BLOCO 3 — ALGORITMO GIBBS COMPLETO
# ------------------------------------------------------------

N      <- 10000
burnin <- 1000

X <- numeric(N)
Y <- numeric(N)

X[1] <- -3
Y[1] <- -3

for (t in 2:N) {
  X[t] <- cond_x_dado_y(Y[t - 1])
  Y[t] <- cond_y_dado_x(X[t])
}

X_pos <- X[(burnin + 1):N]
Y_pos <- Y[(burnin + 1):N]


# ------------------------------------------------------------
# BLOCO 4 — VISUALIZAÇÃO DO CAMINHO EM ZIGUE-ZAGUE
# ------------------------------------------------------------

n_vis <- 20

plot(X[1:n_vis], Y[1:n_vis],
     type = "n",
     xlim = c(-3.5, 3.5), ylim = c(-3.5, 3.5),
     main = "Caminho em zigue-zague do Gibbs (primeiras 20 iterações)",
     xlab = "X", ylab = "Y")

library(MASS)
xy_grid <- expand.grid(
  x = seq(-3.5, 3.5, length.out = 100),
  y = seq(-3.5, 3.5, length.out = 100)
)
Sigma <- matrix(c(sd_x^2, rho*sd_x*sd_y,
                  rho*sd_x*sd_y, sd_y^2), 2, 2)
z <- matrix(
  mapply(function(x, y) {
    v <- c(x - mu_x, y - mu_y)
    exp(-0.5 * t(v) %*% solve(Sigma) %*% v)
  }, xy_grid$x, xy_grid$y),
  100, 100
)
contour(seq(-3.5, 3.5, length.out = 100),
        seq(-3.5, 3.5, length.out = 100),
        z, add = TRUE, col = "gray70", lty = 2)

for (t in 1:(n_vis - 1)) {
  segments(X[t],   Y[t],   X[t+1], Y[t],   col = "red",  lwd = 1.5)
  segments(X[t+1], Y[t],   X[t+1], Y[t+1], col = "blue", lwd = 1.5)
}

points(X[1:n_vis], Y[1:n_vis], pch = 16, col = "gray40", cex = 0.8)
points(X[1],     Y[1],     pch = 16, col = "blue",   cex = 1.5)
points(X[n_vis], Y[n_vis], pch = 16, col = "green3", cex = 1.5)

legend("topright",
       legend = c("Amostra X | Y (horizontal)",
                  "Amostra Y | X (vertical)",
                  "Início", "Posição atual"),
       col    = c("red", "blue", "blue", "green3"),
       lty    = c(1, 1, NA, NA),
       pch    = c(NA, NA, 16, 16),
       lwd    = 2, bty = "n")


# ------------------------------------------------------------
# BLOCO 5 — TRACE PLOTS E BURN-IN
# ------------------------------------------------------------

par(mfrow = c(2, 1))

plot(X, type = "l", col = "steelblue",
     main = "Trace plot — X (trajetória completa)",
     xlab = "Iteração", ylab = "X")
abline(v = burnin, col = "orange", lwd = 2, lty = 2)
abline(h = mu_x,   col = "red",    lwd = 1, lty = 2)
legend("topright",
       legend = c("Cadeia X", "Fim burn-in", "Média teórica"),
       col    = c("steelblue", "orange", "red"),
       lty    = c(1, 2, 2), lwd = 2, bty = "n")

plot(Y, type = "l", col = "darkgreen",
     main = "Trace plot — Y (trajetória completa)",
     xlab = "Iteração", ylab = "Y")
abline(v = burnin, col = "orange", lwd = 2, lty = 2)
abline(h = mu_y,   col = "red",    lwd = 1, lty = 2)

par(mfrow = c(1, 1))


# ------------------------------------------------------------
# BLOCO 6 — AMOSTRAS PÓS BURN-IN
# ------------------------------------------------------------

plot(X_pos, Y_pos,
     pch = 16, cex = 0.3, col = adjustcolor("steelblue", alpha.f = 0.3),
     main = "Amostras do Gibbs pós burn-in",
     xlab = "X", ylab = "Y",
     xlim = c(-3.5, 3.5), ylim = c(-3.5, 3.5))
contour(seq(-3.5, 3.5, length.out = 100),
        seq(-3.5, 3.5, length.out = 100),
        z, add = TRUE, col = "red", lwd = 1.5)
legend("topright",
       legend = c("Amostras Gibbs", "Contornos teóricos"),
       col    = c("steelblue", "red"),
       pch    = c(16, NA), lty = c(NA, 1),
       lwd    = c(NA, 1.5), bty = "n")

round(mean(X_pos),        4) # Média X estimada    | Teórica: 0
round(mean(Y_pos),        4) # Média Y estimada    | Teórica: 0
round(var(X_pos),         4) # Var X estimada      | Teórica: 1
round(var(Y_pos),         4) # Var Y estimada      | Teórica: 1
round(cor(X_pos, Y_pos),  4) # Corr(X,Y) estimada  | Teórica: 0.9


# ------------------------------------------------------------
# BLOCO 7 — EXEMPLO DO SLIDE: MODELO NORMAL COM mu E sigma²
# ------------------------------------------------------------

set.seed(42)
n     <- 30
dados <- rnorm(n, mean = 5, sd = 2)

round(mean(dados), 4) # Média amostral
round(sd(dados),   4) # DP amostral

# Hiperparâmetros
mu0  <- 0
tau2 <- 100
a    <- 0.01
b    <- 0.01

rinvgamma <- function(n, a, b) 1 / rgamma(n, shape = a, rate = b)

N_gibbs  <- 10000
burnin_g <- 1000

mu_chain     <- numeric(N_gibbs)
sigma2_chain <- numeric(N_gibbs)

mu_chain[1]     <- 0
sigma2_chain[1] <- 1

soma_x <- sum(dados)

for (t in 2:N_gibbs) {
  sigma2_t <- sigma2_chain[t - 1]
  vari_mu  <- 1 / (n / sigma2_t + 1 / tau2)
  media_mu <- vari_mu * (soma_x / sigma2_t + mu0 / tau2)
  mu_chain[t] <- rnorm(1, mean = media_mu, sd = sqrt(vari_mu))
  
  mu_t    <- mu_chain[t]
  a_pos   <- a + n / 2
  b_pos   <- b + 0.5 * sum((dados - mu_t)^2)
  sigma2_chain[t] <- rinvgamma(1, a_pos, b_pos)
}

mu_pos     <- mu_chain[(burnin_g + 1):N_gibbs]
sigma2_pos <- sigma2_chain[(burnin_g + 1):N_gibbs]

round(mean(mu_pos),                    4) # Média de mu       | Verdadeira: 5
round(quantile(mu_pos, c(0.025,0.975)),4) # IC 95% de mu
round(mean(sigma2_pos),                4) # Média de sigma²   | Verdadeira: 4
round(quantile(sigma2_pos,c(0.025,0.975)),4) # IC 95% de sigma²

par(mfrow = c(2, 2))

plot(mu_chain, type = "l", col = "steelblue",
     main = "Trace plot: mu", xlab = "Iteração", ylab = expression(mu))
abline(v = burnin_g, col = "orange", lty = 2, lwd = 2)
abline(h = 5, col = "red", lty = 2)

plot(sigma2_chain, type = "l", col = "darkgreen",
     main = "Trace plot: sigma²", xlab = "Iteração", ylab = expression(sigma^2))
abline(v = burnin_g, col = "orange", lty = 2, lwd = 2)
abline(h = 4, col = "red", lty = 2)

hist(mu_pos, breaks = 50, probability = TRUE,
     col = "lightblue", border = "white",
     main = "Posterior de mu", xlab = expression(mu))
abline(v = 5, col = "red", lwd = 2, lty = 2)

hist(sigma2_pos, breaks = 50, probability = TRUE,
     col = "lightgreen", border = "white",
     main = "Posterior de sigma²", xlab = expression(sigma^2))
abline(v = 4, col = "red", lwd = 2, lty = 2)

par(mfrow = c(1, 1))


# ------------------------------------------------------------
# BLOCO 8 — LIMITAÇÃO: CORRELAÇÃO ALTA CAUSA MISTURA LENTA
# ------------------------------------------------------------

gibbs_bivariado <- function(rho, N = 5000, inicio = c(-3, -3)) {
  X <- numeric(N); Y <- numeric(N)
  X[1] <- inicio[1]; Y[1] <- inicio[2]
  var_cond <- 1 - rho^2
  for (t in 2:N) {
    X[t] <- rnorm(1, rho * Y[t-1], sqrt(var_cond))
    Y[t] <- rnorm(1, rho * X[t],   sqrt(var_cond))
  }
  list(X = X, Y = Y)
}

rhos   <- c(0.2, 0.95)
cores  <- c("steelblue", "red")
labels <- c("rho = 0.2 (baixa correlação)", "rho = 0.95 (alta correlação)")

par(mfrow = c(2, 2))

for (i in seq_along(rhos)) {
  res <- gibbs_bivariado(rhos[i])
  
  plot(res$X[1:200], type = "l", col = cores[i],
       main  = paste(labels[i], "— trace X"),
       xlab  = "Iteração", ylab = "X",
       ylim  = c(-3.5, 3.5))
  abline(h = 0, col = "gray50", lty = 2)
  
  plot(res$X[501:5000], res$Y[501:5000],
       pch = 16, cex = 0.3,
       col = adjustcolor(cores[i], alpha.f = 0.4),
       main = paste(labels[i], "— nuvem"),
       xlab = "X", ylab = "Y",
       xlim = c(-3.5, 3.5), ylim = c(-3.5, 3.5))
  
  round(acf(res$X[501:5000], plot = FALSE)$acf[2], 3) # Autocorr X lag 1
}

par(mfrow = c(1, 1))


# ------------------------------------------------------------
# BLOCO 9 — DIAGNÓSTICO: autocorrelação e ESS
# ------------------------------------------------------------

par(mfrow = c(1, 2))
acf(mu_pos,     main = "Autocorrelação: mu",     col = "steelblue", lwd = 2)
acf(sigma2_pos, main = "Autocorrelação: sigma²", col = "darkgreen", lwd = 2)
par(mfrow = c(1, 1))

ess_manual <- function(cadeia) {
  N    <- length(cadeia)
  acfs <- acf(cadeia, plot = FALSE, lag.max = 100)$acf[-1]
  N / (1 + 2 * sum(acfs))
}

round(ess_manual(mu_pos))                                   # ESS de mu
round(ess_manual(sigma2_pos))                               # ESS de sigma²
length(mu_pos)                                              # N pós burn-in
round(ess_manual(mu_pos)     / length(mu_pos),     3)       # Razão ESS/N — mu
round(ess_manual(sigma2_pos) / length(sigma2_pos), 3)       # Razão ESS/N — sigma²


# ------------------------------------------------------------
# BLOCO 10 — INFERÊNCIA FINAL COM AS AMOSTRAS
# ------------------------------------------------------------

round(mean(mu_pos),                        4) # Média posterior de mu
round(median(mu_pos),                      4) # Mediana posterior de mu
round(quantile(mu_pos, c(0.025, 0.975)),   4) # IC 95% credível — mu
round(mean(mu_pos > 4),                    4) # P(mu > 4)
round(mean(mu_pos > 6),                    4) # P(mu > 6)

round(mean(sigma2_pos),                    4) # Média posterior de sigma²
round(quantile(sigma2_pos, c(0.025,0.975)),4) # IC 95% credível — sigma²
round(mean(sigma2_pos > 3),                4) # P(sigma² > 3)

plot(mu_pos, sqrt(sigma2_pos),
     pch = 16, cex = 0.3,
     col = adjustcolor("steelblue", alpha.f = 0.2),
     main = "Posterior conjunta de (mu, sigma)",
     xlab = expression(mu),
     ylab = expression(sigma))
abline(v = 5, col = "red", lty = 2)
abline(h = 2, col = "red", lty = 2)
legend("topright",
       legend = c("Amostras Gibbs", "Valores verdadeiros"),
       col    = c("steelblue", "red"),
       pch    = c(16, NA), lty = c(NA, 2),
       bty    = "n")