# Teorema Central do Limite: integração de Monte Carlo
# Integral de 0 a 1 de u^2 du = 1/3

set.seed(123)

N <- 30       # Observações por simulação
B <- 10000    # Número de simulações independentes

# Repetir o experimento B vezes
estimativas <- replicate(B, {
  u <- runif(N)
  mean(u^2)
})

# Valor verdadeiro e desvio-padrão teórico do estimador
valor_verdadeiro <- 1 / 3
erro_padrao <- sqrt(4 / (45 * N))

# Histograma das estimativas
hist(
  estimativas,
  breaks = 40,
  probability = TRUE,
  col = "gray85",
  border = "white",
  main = paste("Teorema Central do Limite: N =", N),
  xlab = "Estimativa de Monte Carlo"
)

# Curva normal prevista pelo TCL
curve(
  dnorm(x, mean = valor_verdadeiro, sd = erro_padrao),
  add = TRUE,
  col = "steelblue",
  lwd = 2
)

# Valor verdadeiro da integral
abline(
  v = valor_verdadeiro,
  col = "red",
  lwd = 2,
  lty = 2
)

legend(
  "topright",
  legend = c("Normal teórica", "Valor verdadeiro (1/3)"),
  col = c("steelblue", "red"),
  lty = c(1, 2),
  lwd = 2,
  bty = "n"
)

# Comparar resultados empíricos com os valores teóricos
resultado <- data.frame(
  Medida = c("Média", "Desvio-padrão"),
  Empirico = c(mean(estimativas), sd(estimativas)),
  Teorico = c(valor_verdadeiro, erro_padrao)
)

print(resultado)