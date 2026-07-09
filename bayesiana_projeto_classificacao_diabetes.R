# ============================================================
# Projeto — Estatística Bayesiana
# Aplicação 1: Classificação Bayesiana
# Diagnóstico de diabetes (Pima Indians Diabetes Dataset)
# Prof. Vinícius Osterne, PhD
# ============================================================
#
# CONTEXTO GERAL (leia antes de rodar)
# -------------------------------------
# Queremos prever se uma pessoa tem diabetes (sim/não) a partir
# de exames simples (glicose, pressão, idade, etc). Isso é um
# problema de CLASSIFICAÇÃO BINÁRIA.
#
# A abordagem clássica (não-bayesiana) daria UM valor para cada
# coeficiente da regressão logística (ex: "o efeito da glicose é
# 0.8"). A abordagem BAYESIANA, em vez disso, entrega uma
# DISTRIBUIÇÃO INTEIRA de valores plausíveis para cada
# coeficiente — ou seja, entrega também o quão INCERTOS estamos
# sobre esse efeito.
#
# Neste script usamos o pacote rstanarm, que já implementa por
# debaixo dos panos exatamente os algoritmos que vimos em sala
# (MCMC via Hamiltonian Monte Carlo, e também Inferência
# Variacional). A vantagem de usar uma função pronta é que ela
# já resolve os detalhes técnicos (calibração de passo,
# diagnóstico, etc) e o MESMO CÓDIGO funciona para qualquer
# outro conjunto de dados — basta trocar a fórmula e o data.frame.
#
# Este script tem 5 etapas:
#   1. Preparar os dados
#   2. Ajustar o modelo bayesiano com stan_glm() (HMC)
#   3. Checar se o ajuste é confiável (diagnóstico)
#   4. Interpretar a posterior (o que aprendemos sobre cada exame)
#   5. Comparar com Inferência Variacional (mais rápida) e usar a
#      posterior para tomar uma DECISÃO prática (o ponto de corte
#      ideal para diagnosticar, considerando que alguns erros são
#      mais graves que outros)
# ============================================================

# ---- 0. Pacotes ---------------------------------------------------
# mlbench   -> só para termos acesso ao dataset real de diabetes
# rstanarm  -> ajusta modelos bayesianos prontos (regressão, logística,
#              hierárquicos, etc), usando Stan por debaixo dos panos
# bayesplot -> gráficos padronizados para diagnóstico de modelos bayesianos
# dplyr     -> manipulação de dados (filtros, seleção de colunas)
# ggplot2   -> gráficos
# install.packages(c("mlbench","rstanarm","bayesplot","ggplot2","dplyr"))
library(mlbench)
library(rstanarm)
library(bayesplot)
library(dplyr)
library(ggplot2)

set.seed(42)  # fixamos a semente aleatória só para os resultados
# serem reprodutíveis (você rodar de novo e ver o mesmo número)

# ============================================================
# ETAPA 1 — DADOS E PRÉ-PROCESSAMENTO
# ============================================================
# Cada linha do dataset é uma paciente. As colunas são exames
# (glicose, pressão, IMC, etc) e a última coluna diz se ela
# desenvolveu diabetes ou não.

data("PimaIndiansDiabetes2", package = "mlbench")
df <- PimaIndiansDiabetes2
head(df,10)

# Problema comum nesse dataset: exames como "pressão = 0" não
# fazem sentido fisiológico (ninguém tem pressão zero e está vivo).
# Isso é um erro de coleta, não um valor real. A versão "2" do
# dataset já substitui esses zeros impossíveis por NA (valor
# ausente) para nós.
df <- df %>%
  select(pregnant, glucose, pressure, mass, pedigree, age, diabetes) %>%
  na.omit()   # removemos linhas com qualquer valor faltante
# (simplificação didática — em um projeto mais robusto,
#  poderíamos "imputar" esses valores em vez de descartar)

# Convertendo a variável resposta para 0/1 (1 = tem diabetes)
df$y <- ifelse(df$diabetes == "pos", 1, 0)
head(df)

# Padronizar as variáveis (subtrair a média, dividir pelo desvio-padrão)
# ajuda o algoritmo a convergir mais rápido, porque todas as covariáveis
# passam a estar na mesma escala. Não é obrigatório, mas é boa prática.
X_raw <- df %>% select(pregnant, glucose, pressure, mass, pedigree, age)
X_std <- as.data.frame(scale(X_raw))
df_model <- data.frame(y = df$y, X_std)
head(df_model)

cat("Dimensão dos dados:", nrow(df_model), "observações,",
    ncol(df_model) - 1, "covariáveis\n")

# ============================================================
# ETAPA 2 — AJUSTANDO O MODELO BAYESIANO (stan_glm)
# ============================================================
# O modelo tem três peças, exatamente como vimos em sala:
#
#   (a) LIKELIHOOD (verossimilhança): como os dados são gerados
#       dado um valor de beta.
#         y_i ~ Bernoulli(p_i)
#         logit(p_i) = beta_0 + beta_1*glicose_i + ... + beta_6*idade_i
#
#   (b) PRIOR: nossa crença sobre os coeficientes ANTES de olhar
#       para os dados.
#         beta_j ~ N(0, 10^2)
#       Isso diz: "achamos mais plausível que cada efeito seja
#       pequeno (perto de 0), mas estamos dispostos a mudar de
#       ideia se os dados indicarem o contrário" (por isso o
#       desvio-padrão de 10 é bem generoso/vago).
#
#   (c) POSTERIOR: o que queremos calcular.
#         P(beta | dados) ∝ P(dados | beta) * P(beta)
#       stan_glm() gera amostras dessa posterior automaticamente,
#       usando Hamiltonian Monte Carlo (HMC) — uma versão mais
#       sofisticada do Metropolis-Hastings que vimos em sala,
#       que usa informação de gradiente para propor passos mais
#       inteligentes (por isso é o "padrão-ouro" da área).
#
# A função abaixo funciona para QUALQUER dataset: basta trocar
# a fórmula (o que está à esquerda e à direita do "~") e o
# argumento "data". A estrutura fica sempre a mesma.

fit <- stan_glm(
  y ~ pregnant + glucose + pressure + mass + pedigree + age,
  data   = df_model,
  family = binomial(link = "logit"),   # diz que é uma regressão LOGÍSTICA
  prior            = normal(0, 10),    # prior para os coeficientes
  prior_intercept  = normal(0, 10),    # prior para o intercepto
  chains = 4,      # quantas cadeias independentes rodar (para o diagnóstico)
  iter   = 4000,   # quantas iterações por cadeia
  seed   = 42
)

# ============================================================
# ETAPA 3 — DIAGNÓSTICO DE CONVERGÊNCIA
# ============================================================
# Assim como discutimos em sala: rodar o algoritmo não garante
# nada por si só — precisamos CHECAR se as amostras geradas
# realmente representam a posterior. A boa notícia é que
# stan_glm() já calcula os diagnósticos automaticamente.

print(fit)
# O summary já traz, para cada parâmetro:
#   mean / sd            -> média e desvio-padrão posterior
#   Rhat                 -> deve estar bem próximo de 1.00
#                            (valores > 1.10 são sinal de alerta)
#   n_eff (tamanho efetivo de amostra, equivalente ao ESS)
#                            quanto maior, melhor (queremos > 400)

# TRACE PLOT: gráfico do valor de cada beta ao longo das
# iterações, para cada cadeia. Queremos ver uma "faixa horizontal
# barulhenta e estável" (parece estática de TV), sem tendência
# de subida/descida — isso indica que a cadeia já "esqueceu" o
# ponto inicial e está só oscilando ao redor da posterior.
mcmc_trace(fit, pars = names(coef(fit)))

# GRÁFICO DE DENSIDADE POSTERIOR por parâmetro (mais fácil de
# visualizar a incerteza do que só olhar para uma tabela de números)
mcmc_areas(fit, pars = names(coef(fit))[-1], prob = 0.95) +
  labs(title = "Distribuição posterior dos coeficientes",
       subtitle = "Área sombreada = intervalo de credibilidade de 95%")

cat("\nSe o Rhat de todos os parâmetros estiver <= 1.01 e o n_eff for alto,\n")
cat("podemos confiar nas amostras da posterior.\n")

# ============================================================
# ETAPA 4 — INTERPRETANDO A POSTERIOR
# ============================================================
# Extraímos as amostras da posterior como um data.frame — cada
# linha é uma "amostra plausível" do vetor de coeficientes.
posterior_samples <- as.data.frame(fit)

# intervalos de credibilidade de 95% (equivalente bayesiano do
# "intervalo de confiança", mas com interpretação direta:
# "há 95% de probabilidade de o efeito estar nesse intervalo")
print(posterior_interval(fit, prob = 0.95))

# probabilidade de que cada efeito seja POSITIVO, calculada
# diretamente a partir das amostras da posterior
p_positivo <- posterior_samples %>%
  select(-`(Intercept)`) %>%
  summarise(across(everything(), ~ mean(.x > 0)))

print(t(p_positivo))
cat("\nLeitura: se 'p_positivo' de uma variável está perto de 1 (ou de 0),\n")
cat("temos bastante certeza de que o efeito é positivo (ou negativo).\n")
cat("Se está perto de 0.5, a evidência é fraca — o efeito pode ser nulo.\n")

# ============================================================
# ETAPA 5 — INFERÊNCIA VARIACIONAL (mais rápida) E DECISÃO BAYESIANA
# ============================================================

# ---- 5a. Inferência Variacional (VI / ADVI) ------------------------
# Em vez de AMOSTRAR da posterior (como o HMC faz), a VI faz uma
# OTIMIZAÇÃO: procura a distribuição mais simples (aqui, gaussiana)
# que fica mais "próxima" da posterior real. É bem mais rápida,
# mas é uma aproximação — em datasets grandes ou quando o tempo é
# curto, é uma alternativa prática ao MCMC.
fit_vi <- stan_glm(
  y ~ pregnant + glucose + pressure + mass + pedigree + age,
  data = df_model, family = binomial(link = "logit"),
  prior = normal(0, 10), prior_intercept = normal(0, 10),
  algorithm = "meanfield", seed = 42
)

comparacao <- data.frame(
  parametro = names(coef(fit)),
  media_HMC = coef(fit),
  media_VI  = coef(fit_vi)
)
print(comparacao)
cat("\nA diferença prática é o TEMPO: VI roda em segundos, HMC em minutos.\n")
cat("As médias costumam ficar próximas; a VI tende a subestimar levemente\n")
cat("o desvio-padrão posterior (consequência da divergência KL 'reverse').\n")

# ---- 5b. Decisão bayesiana: qual ponto de corte usar? ---------------
# O modelo entrega uma PROBABILIDADE de diabetes para cada
# paciente. Na prática, o médico precisa de uma decisão binária:
# "chamar para mais exames" ou "não chamar". O corte ingênuo
# seria 0.5, mas os dois tipos de erro não são igualmente graves:
#   - Falso Negativo (FN): dizer que está saudável quando na
#     verdade tem diabetes -> MUITO grave (doença não tratada)
#   - Falso Positivo (FP): chamar a pessoa à toa -> chato, mas
#     bem menos grave
#
# A DECISÃO BAYESIANA formaliza isso: definimos custos diferentes
# para cada erro e escolhemos o ponto de corte que MINIMIZA a
# perda esperada.

custo_FN <- 5   # custo de não detectar um caso positivo (grave)
custo_FP <- 1   # custo de um alarme falso (leve)

# probabilidade prevista pelo modelo para cada paciente (média
# posterior das probabilidades previstas)
p_pred <- colMeans(posterior_epred(fit))

perda_esperada <- function(threshold, p_pred, y, custo_FN, custo_FP) {
  acao <- ifelse(p_pred >= threshold, 1, 0)   # 1 = "chamar para exame"
  FN <- sum(acao == 0 & y == 1)   # deixamos passar um caso real
  FP <- sum(acao == 1 & y == 0)   # chamamos à toa
  (FN * custo_FN + FP * custo_FP) / length(y)
}

thresholds <- seq(0.05, 0.95, by = 0.01)
perdas <- sapply(thresholds, perda_esperada, p_pred = p_pred, y = df_model$y,
                 custo_FN = custo_FN, custo_FP = custo_FP)

threshold_otimo <- thresholds[which.min(perdas)]

ggplot(data.frame(thresholds, perdas), aes(thresholds, perdas)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_vline(xintercept = threshold_otimo, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0.5, linetype = "dotted", color = "gray40") +
  labs(title = "Perda esperada por threshold de decisão",
       subtitle = paste("Threshold ótimo =", threshold_otimo,
                        "| threshold ingênuo = 0.5"),
       x = "Threshold de classificação", y = "Perda esperada") +
  theme_minimal()

cat("\nThreshold ótimo (Bayes):", threshold_otimo, "\n")
cat("O threshold ingênuo de 0.5 é subótimo quando os custos são assimétricos:\n")
cat("como um falso negativo custa", custo_FN, "vezes mais que um falso positivo,\n")
cat("a decisão ótima é ser mais 'cauteloso' e chamar mais gente para exame,\n")
cat("mesmo que isso gere alguns alarmes falsos a mais.\n")

# ============================================================
# COMO ADAPTAR ESTE SCRIPT PARA OUTRO DATASET
# ============================================================
# 1. Troque o carregamento dos dados (Etapa 1) pelo seu próprio
#    data.frame, com uma coluna resposta 0/1 e as covariáveis
#    que quiser usar.
# 2. Ajuste a fórmula em stan_glm() (Etapa 2) para os nomes das
#    suas colunas: y ~ var1 + var2 + var3 + ...
# 3. Todo o resto do script (diagnóstico, interpretação, VI,
#    decisão bayesiana) funciona sem nenhuma alteração.