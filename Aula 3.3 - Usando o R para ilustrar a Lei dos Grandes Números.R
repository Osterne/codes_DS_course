# Lei dos Grandes Números: integração de Monte Carlo
# Integral de 0 a 1 de u^2 du = 1/3

set.seed(123)

N <- 10000

# Gerar observações de uma Uniforme(0,1)
u <- runif(N)

# Calcular a estimativa a cada nova observação
estimativa <- cumsum(u^2) / seq_len(N)

# Valor verdadeiro da integral
valor_verdadeiro <- 1 / 3

# Visualizar a convergência
plot(
  seq_len(N), estimativa,
  type = "l",
  col = "steelblue",
  lwd = 1.5,
  xlab = "Número de simulações (N)",
  ylab = "Estimativa de Monte Carlo",
  main = "Lei dos Grandes Números"
)

abline(h = valor_verdadeiro, col = "red", lwd = 2, lty = 2)

legend(
  "topright",
  legend = c("Média acumulada", "Valor verdadeiro (1/3)"),
  col = c("steelblue", "red"),
  lty = c(1, 2),
  lwd = 2,
  bty = "n"
)

# Comparar as estimativas para diferentes tamanhos de amostra
tamanhos <- c(10, 100, 1000, 10000)

resultado <- data.frame(
  N = tamanhos,
  Estimativa = estimativa[tamanhos],
  Valor_verdadeiro = valor_verdadeiro
)

print(resultado)