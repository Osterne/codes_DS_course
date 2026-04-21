# ============================================================
# Exemplo: Lei dos Grandes Números com Monte Carlo
# Objetivo: mostrar que a média amostral de uma Uniforme(0,1)
# converge para o valor esperado teórico, que é 0.5
# ============================================================
set.seed(123)

# ------------------------------------------------------------
# 1. Valor teórico da esperança de U(0,1)
# ------------------------------------------------------------
valor_teorico <- 0.5

# ------------------------------------------------------------
# 2. Primeiras tentativas — amostras pequenas
# ------------------------------------------------------------
theta_10 <- runif(10, min = 0, max = 1)
round(mean(theta_10), 4) # Média amostral com N = 10

theta_50 <- runif(50, min = 0, max = 1)
round(mean(theta_50), 4) # Média amostral com N = 50

theta_100 <- runif(100, min = 0, max = 1)
round(mean(theta_100), 4) # Média amostral com N = 100

theta_500 <- runif(500, min = 0, max = 1)
round(mean(theta_500), 4) # Média amostral com N = 500

# ------------------------------------------------------------
# 3. Agora com N = 1000
# ------------------------------------------------------------
theta <- runif(1000, min = 0, max = 1)
round(mean(theta), 4) # Média amostral com N = 1000

# ------------------------------------------------------------
# 4. Convergência da média acumulada
# ------------------------------------------------------------
media_acumulada <- cumsum(theta) / (1:1000)

plot(
  media_acumulada,
  type = "l",
  lwd = 2,
  xlab = "Número de simulações (N)",
  ylab = "Média acumulada",
  main = "Lei dos Grandes Números: convergência para 0.5"
)
abline(h = valor_teorico, lty = 2, lwd = 2)

# ------------------------------------------------------------
# 5. Tabela comparativa
# ------------------------------------------------------------
Ns <- c(10, 50, 100, 500, 1000, 5000)
set.seed(123)
estimativas <- sapply(Ns, function(n) {
  mean(runif(n, min = 0, max = 1))
})

data.frame(
  N = Ns,
  Estimativa_MC = estimativas,
  Valor_Teorico = rep(0.5, length(Ns)),
  Erro_Absoluto = abs(estimativas - 0.5)
)
