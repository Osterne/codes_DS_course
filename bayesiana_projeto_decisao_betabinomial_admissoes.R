# ============================================================
# Projeto — Estatística Bayesiana
# Aplicação: Modelo Beta-Binomial + Decisão Bayesiana
# Admissões na UC Berkeley (1973) — o clássico Paradoxo de Simpson
# Prof. Vinícius Osterne, PhD
# ============================================================
#
# Objetivo: usar o modelo conjugado Beta-Binomial (o mais simples
# do curso) para comparar taxas de admissão por gênero dentro de
# cada departamento, ilustrar atualização sequencial de crenças,
# tomar uma decisão bayesiana sobre "quais departamentos investigar"
# e fechar com uma demonstração de aprendizado ativo (Thompson Sampling).
#
# Dataset: datasets::UCBAdmissions (já vem no R base, dado real)
#
# Estrutura:
#   1. Dados e o paradoxo de Simpson
#   2. Modelo conjugado Beta-Binomial por departamento x gênero
#   3. Atualização sequencial (mostrando o prior virando posterior)
#   4. Decisão bayesiana: quais departamentos "sinalizar"
#   5. Aprendizado ativo: priorização via incerteza posterior
# ============================================================

# install.packages(c("dplyr","ggplot2","tidyr"))
library(dplyr)
library(ggplot2)
library(tidyr)

set.seed(42)

# ---- 1. Dados -------------------------------------------------------
data("UCBAdmissions")

df <- as.data.frame(UCBAdmissions) %>%
  pivot_wider(names_from = Admit, values_from = Freq) %>%
  rename(admitidos = Admitted, rejeitados = Rejected) %>%
  mutate(n = admitidos + rejeitados,
         taxa_crua = admitidos / n)

print(df)

# taxa agregada (ignorando departamento) — o paradoxo aparece aqui
agregado <- df %>%
  group_by(Gender) %>%
  summarise(admitidos = sum(admitidos), n = sum(n)) %>%
  mutate(taxa = admitidos / n)
print(agregado)

cat("\nNo agregado, a taxa de admissão de mulheres parece menor.\n")
cat("Mas isso pode ser um artefato de para quais departamentos elas mais se\n")
cat("candidataram (Paradoxo de Simpson) — vamos investigar departamento a departamento.\n")

ggplot(df, aes(x = Dept, y = taxa_crua, fill = Gender)) +
  geom_col(position = "dodge") +
  labs(title = "Taxa de admissão bruta por departamento e gênero",
       y = "Taxa de admissão", x = "Departamento") +
  theme_minimal()

# ---- 2. Modelo Beta-Binomial conjugado -------------------------------
# Para cada combinação (departamento, gênero):
#   admitidos_k ~ Binomial(n_k, p_k)
#   p_k ~ Beta(alpha0, beta0)          prior fracamente informativo
#   p_k | dados ~ Beta(alpha0 + admitidos_k, beta0 + rejeitados_k)

alpha0 <- 2; beta0 <- 2   # prior fracamente informativo, centrado em 0.5

df <- df %>%
  mutate(
    alpha_post = alpha0 + admitidos,
    beta_post  = beta0 + rejeitados,
    media_post = alpha_post / (alpha_post + beta_post),
    q2.5  = qbeta(0.025, alpha_post, beta_post),
    q97.5 = qbeta(0.975, alpha_post, beta_post)
  )

print(df %>% select(Dept, Gender, n, taxa_crua, media_post, q2.5, q97.5))

ggplot(df, aes(x = Dept, y = media_post, color = Gender)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5),
                position = position_dodge(width = 0.5), width = 0.2) +
  labs(title = "Taxa de admissão posterior (Beta-Binomial) por departamento",
       subtitle = "Barras = intervalo de credibilidade de 95%",
       y = "P(admissão | dados)", x = "Departamento") +
  theme_minimal()

# probabilidade posterior de que mulheres têm taxa menor, por departamento
# (via amostragem de Monte Carlo das duas Betas — o próprio método de Monte
# Carlo do início do curso, aqui aplicado a uma pergunta concreta)
n_sim <- 20000
depts <- unique(df$Dept)
prob_mulher_menor <- sapply(depts, function(d) {
  linha_M <- df %>% filter(Dept == d, Gender == "Male")
  linha_F <- df %>% filter(Dept == d, Gender == "Female")
  amostra_M <- rbeta(n_sim, linha_M$alpha_post, linha_M$beta_post)
  amostra_F <- rbeta(n_sim, linha_F$alpha_post, linha_F$beta_post)
  mean(amostra_F < amostra_M)
})
names(prob_mulher_menor) <- depts
print(prob_mulher_menor)

cat("\nRepare: na maioria dos departamentos, P(mulher < homem) fica perto de 0.5\n")
cat("(sem diferença real) — o efeito agregado é explicado pela distribuição\n")
cat("desigual de candidaturas entre departamentos com taxas de admissão distintas.\n")

# ---- 3. Atualização sequencial (ilustração didática) ------------------
# Reconstruímos uma sequência hipotética de decisões individuais do
# Departamento A (homens) para visualizar o prior virando posterior aos poucos.

dept_A_M <- df %>% filter(Dept == "A", Gender == "Male")
sequencia <- sample(c(rep(1, dept_A_M$admitidos), rep(0, dept_A_M$rejeitados)))

checkpoints <- unique(round(seq(1, length(sequencia), length.out = 6)))
curvas <- data.frame()
for (cp in checkpoints) {
  dados_ate_aqui <- sequencia[1:cp]
  a_n <- alpha0 + sum(dados_ate_aqui)
  b_n <- beta0 + sum(1 - dados_ate_aqui)
  x_grid <- seq(0, 1, length.out = 200)
  curvas <- rbind(curvas, data.frame(
    x = x_grid, densidade = dbeta(x_grid, a_n, b_n),
    n_obs = paste0("n = ", cp)
  ))
}
curvas$n_obs <- factor(curvas$n_obs, levels = paste0("n = ", checkpoints))

ggplot(curvas, aes(x = x, y = densidade, color = n_obs)) +
  geom_line(linewidth = 1) +
  labs(title = "Atualização sequencial da crença (Departamento A, homens)",
       subtitle = "O prior Beta(2,2) se concentra à medida que mais dados chegam",
       x = "p (taxa de admissão)", y = "densidade") +
  theme_minimal()

# ---- 4. Decisão bayesiana: quais departamentos sinalizar? --------------
# Ação: sinalizar (a=1) ou não sinalizar (a=0) um departamento para revisão
# Perda: falso alarme custa pouco; deixar passar uma disparidade real custa mais
custo_alarme_falso        <- 1
custo_disparidade_ignorada <- 4
limiar_disparidade        <- 0.10  # diferença de 10 p.p. já é considerada relevante

decisao <- sapply(depts, function(d) {
  linha_M <- df %>% filter(Dept == d, Gender == "Male")
  linha_F <- df %>% filter(Dept == d, Gender == "Female")
  amostra_M <- rbeta(n_sim, linha_M$alpha_post, linha_M$beta_post)
  amostra_F <- rbeta(n_sim, linha_F$alpha_post, linha_F$beta_post)
  diferenca <- amostra_M - amostra_F
  
  p_disparidade_real  <- mean(abs(diferenca) > limiar_disparidade)
  perda_sinalizar      <- (1 - p_disparidade_real) * custo_alarme_falso
  perda_nao_sinalizar  <- p_disparidade_real * custo_disparidade_ignorada
  
  ifelse(perda_sinalizar < perda_nao_sinalizar, "SINALIZAR", "não sinalizar")
})
names(decisao) <- depts
print(decisao)

# ---- 5. Aprendizado ativo: priorização por incerteza -----------------------
# Simulação: se pudéssemos "auditar" um departamento por vez para reduzir
# incerteza, qual escolher primeiro? A lógica (próxima de Thompson Sampling)
# prioriza onde a incerteza sobre a diferença ainda é maior.

incerteza <- sapply(depts, function(d) {
  linha_M <- df %>% filter(Dept == d, Gender == "Male")
  linha_F <- df %>% filter(Dept == d, Gender == "Female")
  var_beta <- function(a, b) (a * b) / ((a + b)^2 * (a + b + 1))
  var_beta(linha_M$alpha_post, linha_M$beta_post) +
    var_beta(linha_F$alpha_post, linha_F$beta_post)
})
names(incerteza) <- depts
proxima_auditoria <- names(which.max(incerteza))

cat("\nPróximo departamento a priorizar para coleta de mais dados (maior incerteza):",
    proxima_auditoria, "\n")
cat("Essa é a lógica por trás do aprendizado ativo: gastar esforço de coleta\n")
cat("onde a posterior ainda está mais dispersa.\n")