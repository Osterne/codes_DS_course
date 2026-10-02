# ============================================================
# ERRO DE MONTE CARLO
# ============================================================

library(ggplot2)
library(dplyr)

set.seed(123)


# ------------------------------------------------------------
# Integral de u^2 em [0,1]
# ------------------------------------------------------------

# I = integral_0^1 u^2 du = E(U^2) = 1/3,
# com U ~ Uniforme(0,1)

I <- 1 / 3

N <- 100

u <- runif(N)
y <- u^2

I_hat <- mean(y)

I
I_hat


# ------------------------------------------------------------
# Erro observado e erro-padrão
# ------------------------------------------------------------

erro <- I_hat - I
erro_abs <- abs(I_hat - I)

sigma_hat <- sd(y)
ep_hat <- sigma_hat / sqrt(N)

data.frame(
  estimativa = I_hat,
  erro = erro,
  erro_absoluto = erro_abs,
  erro_padrao = ep_hat
)


# ------------------------------------------------------------
# Repetições independentes do experimento
# ------------------------------------------------------------

B <- 5000
N <- 100

estimativas <- replicate(
  B,
  mean(runif(N)^2)
)

mean(estimativas)
sd(estimativas)

dados_estimativas <- data.frame(
  estimativa = estimativas
)

ggplot(dados_estimativas, aes(x = estimativa)) +
  geom_histogram(
    bins = 40,
    fill = "#762D41",
    color = "white"
  ) +
  geom_vline(
    xintercept = I,
    linetype = "dashed",
    linewidth = 1
  ) +
  labs(
    title = "Distribuição das estimativas de Monte Carlo",
    subtitle = paste0("N = ", N, " | ", B, " repetições"),
    x = "Estimativa",
    y = "Frequência"
  ) +
  theme_minimal(base_size = 14)


# ------------------------------------------------------------
# Erro-padrão teórico
# ------------------------------------------------------------

# Var(U^2) = E(U^4) - E(U^2)^2
#           = 1/5 - 1/9
#           = 4/45

var_y <- 4 / 45
sigma <- sqrt(var_y)

ep_teorico <- sigma / sqrt(N)
ep_empirico <- sd(estimativas)

data.frame(
  erro_padrao_teorico = ep_teorico,
  erro_padrao_empirico = ep_empirico
)


# ------------------------------------------------------------
# Efeito do número de simulações
# ------------------------------------------------------------

valores_N <- c(
  10, 30, 100, 300, 1000,
  3000, 10000, 30000, 100000
)

resultados <- lapply(valores_N, function(N) {
  
  u <- runif(N)
  y <- u^2
  
  data.frame(
    N = N,
    estimativa = mean(y),
    erro_absoluto = abs(mean(y) - I),
    erro_padrao = sd(y) / sqrt(N),
    ep_teorico = sigma / sqrt(N)
  )
  
}) |>
  bind_rows()

resultados


# ------------------------------------------------------------
# Convergência da estimativa
# ------------------------------------------------------------

ggplot(resultados, aes(x = N, y = estimativa)) +
  geom_line(
    linewidth = 0.9,
    color = "#762D41"
  ) +
  geom_point(
    size = 3,
    color = "#762D41"
  ) +
  geom_hline(
    yintercept = I,
    linetype = "dashed",
    linewidth = 0.9
  ) +
  scale_x_log10(
    breaks = valores_N
  ) +
  labs(
    title = "Convergência da estimativa",
    subtitle = expression(I == 1/3),
    x = "Número de simulações",
    y = expression(hat(I)[N])
  ) +
  theme_minimal(base_size = 14)


# ------------------------------------------------------------
# Taxa de convergência do erro-padrão
# ------------------------------------------------------------

dados_ep <- data.frame(
  N = valores_N,
  erro_padrao = sigma / sqrt(valores_N)
)

dados_ep

ggplot(dados_ep, aes(x = N, y = erro_padrao)) +
  geom_line(
    linewidth = 1,
    color = "#762D41"
  ) +
  geom_point(
    size = 3,
    color = "#762D41"
  ) +
  scale_x_log10(
    breaks = valores_N
  ) +
  labs(
    title = "Erro-padrão de Monte Carlo",
    subtitle = expression(EP(hat(I)[N]) == sigma / sqrt(N)),
    x = "Número de simulações",
    y = "Erro-padrão"
  ) +
  theme_minimal(base_size = 14)


# ------------------------------------------------------------
# Duplicando o número de simulações
# ------------------------------------------------------------

N <- 1000

ep_N <- sigma / sqrt(N)
ep_2N <- sigma / sqrt(2 * N)

data.frame(
  N = c(N, 2 * N),
  erro_padrao = c(ep_N, ep_2N)
)

ep_2N / ep_N

1 / sqrt(2)

100 * (1 - ep_2N / ep_N)


# ------------------------------------------------------------
# Reduzindo o erro-padrão pela metade
# ------------------------------------------------------------

ep_4N <- sigma / sqrt(4 * N)

data.frame(
  N = c(N, 4 * N),
  erro_padrao = c(ep_N, ep_4N)
)

ep_4N / ep_N


# ------------------------------------------------------------
# Custo da precisão
# ------------------------------------------------------------

fator_N <- c(1, 4, 16, 100)

custo_precisao <- data.frame(
  fator_N = fator_N,
  erro_relativo = 1 / sqrt(fator_N)
)

custo_precisao

ggplot(
  custo_precisao,
  aes(x = fator_N, y = erro_relativo)
) +
  geom_line(
    linewidth = 1,
    color = "#762D41"
  ) +
  geom_point(
    size = 3.5,
    color = "#762D41"
  ) +
  scale_x_log10(
    breaks = fator_N
  ) +
  labs(
    title = "O custo de aumentar a precisão",
    x = "Fator de aumento em N",
    y = "Erro-padrão relativo"
  ) +
  theme_minimal(base_size = 14)


# ------------------------------------------------------------
# Integral de u^2: erro-padrão para diferentes N
# ------------------------------------------------------------

tabela_ep <- data.frame(
  N = c(100, 1000, 10000, 100000)
) |>
  mutate(
    erro_padrao = 2 / sqrt(45 * N)
  )

tabela_ep


# ------------------------------------------------------------
# Escolha de N a partir de uma precisão desejada
# ------------------------------------------------------------

# EP <= epsilon
#
# sigma / sqrt(N) <= epsilon
#
# N >= sigma^2 / epsilon^2

epsilon <- 0.001

N_necessario <- ceiling(
  sigma^2 / epsilon^2
)

N_necessario

sigma / sqrt(N_necessario)


# Outras precisões

epsilon <- c(
  0.01,
  0.005,
  0.002,
  0.001
)

planejamento <- data.frame(
  epsilon = epsilon,
  N = ceiling(sigma^2 / epsilon^2)
)

planejamento


# ------------------------------------------------------------
# Planejamento quando sigma é desconhecido
# ------------------------------------------------------------

set.seed(123)

N_piloto <- 1000

u_piloto <- runif(N_piloto)
y_piloto <- u_piloto^2

sigma_piloto <- sd(y_piloto)

sigma_piloto
sigma


# Precisão desejada

epsilon <- 0.001

N_planejado <- ceiling(
  sigma_piloto^2 / epsilon^2
)

N_planejado


# ------------------------------------------------------------
# Simulação com o N planejado
# ------------------------------------------------------------

set.seed(321)

u_final <- runif(N_planejado)
y_final <- u_final^2

I_final <- mean(y_final)
ep_final <- sd(y_final) / sqrt(N_planejado)

data.frame(
  N = N_planejado,
  estimativa = I_final,
  valor_exato = I,
  erro_absoluto = abs(I_final - I),
  erro_padrao = ep_final
)


# ------------------------------------------------------------
# Estimativa acumulada
# ------------------------------------------------------------

set.seed(123)

N <- 10000

u <- runif(N)
y <- u^2

estimativa_acumulada <- cumsum(y) / seq_len(N)

convergencia <- data.frame(
  N = seq_len(N),
  estimativa = estimativa_acumulada
)

ggplot(convergencia, aes(x = N, y = estimativa)) +
  geom_line(
    linewidth = 0.7,
    color = "#762D41"
  ) +
  geom_hline(
    yintercept = I,
    linetype = "dashed",
    linewidth = 0.9
  ) +
  labs(
    title = "Convergência da estimativa de Monte Carlo",
    subtitle = expression(I == 1/3),
    x = "Número de simulações",
    y = "Estimativa acumulada"
  ) +
  theme_minimal(base_size = 14)


# ------------------------------------------------------------
# Erro-padrão ao longo da simulação
# ------------------------------------------------------------

ep_acumulado <- sapply(
  seq_len(N),
  function(n) {
    
    if (n < 2) {
      return(NA_real_)
    }
    
    sd(y[1:n]) / sqrt(n)
  }
)

precisao <- data.frame(
  N = seq_len(N),
  erro_padrao = ep_acumulado
)

ggplot(
  precisao |> filter(N >= 10),
  aes(x = N, y = erro_padrao)
) +
  geom_line(
    linewidth = 0.7,
    color = "#762D41"
  ) +
  labs(
    title = "Redução do erro-padrão",
    subtitle = expression(EP == sigma / sqrt(N)),
    x = "Número de simulações",
    y = "Erro-padrão estimado"
  ) +
  theme_minimal(base_size = 14)