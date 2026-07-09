# ============================================================
# Projeto — Estatística Bayesiana
# Aplicação: Regressão Hierárquica Bayesiana
# Desempenho em matemática por escola (High School & Beyond)
# Prof. Vinícius Osterne, PhD
# ============================================================
#
# CONTEXTO GERAL (leia antes de rodar)
# -------------------------------------
# Temos alunos (nível 1) agrupados dentro de escolas (nível 2).
# Alunos da mesma escola tendem a se parecer mais entre si do
# que alunos de escolas diferentes — professores, infraestrutura
# e contexto socioeconômico são compartilhados dentro da escola.
# Ignorar isso (tratando todo mundo como se fosse independente)
# é estatisticamente incorreto e pode gerar conclusões erradas.
#
# A solução é um MODELO HIERÁRQUICO (também chamado de
# multinível ou "mixed model"): cada escola ganha o seu próprio
# "efeito" (intercepto aleatório), mas esses efeitos não são
# estimados de forma totalmente independente — eles COMPARTILHAM
# informação por meio de uma distribuição comum. O resultado é
# um equilíbrio entre "confiar só nos dados daquela escola" e
# "confiar na média geral de todas as escolas" — esse equilíbrio
# se chama POOLING PARCIAL, e é a grande vantagem prática desse
# tipo de modelo.
#
# NOTA TÉCNICA IMPORTANTE:
# Usamos aqui o pacote rstanarm (a mesma família de funções do
# script 1: stan_glm). A diferença para outros pacotes como o
# "brms" é que o rstanarm já vem com os modelos PRÉ-COMPILADOS
# dentro do próprio pacote — ou seja, NÃO exige um compilador
# C++ instalado na sua máquina (Xcode/Rtools). Isso evita erros
# de instalação para quem está começando, e é por isso que
# usamos a mesma família de funções do script 1 aqui também.
# O mesmo raciocínio de MCMC (HMC) explicado em sala continua
# valendo por trás dos panos.
#
# Este script tem 5 etapas:
#   1. Preparar os dados e visualizar a estrutura hierárquica
#   2. Ajustar o modelo bayesiano hierárquico com stan_lmer()
#   3. Checar se o ajuste é confiável (diagnóstico)
#   4. Interpretar os componentes de variância (entre e dentro
#      das escolas) e o efeito de pooling parcial
#   5. Comparar a média "crua" de cada escola com a média que o
#      modelo estima (evidenciando o shrinkage)
# ============================================================

# ---- 0. Pacotes ---------------------------------------------------
# nlme      -> só para termos acesso ao dataset real de escolas
# rstanarm  -> ajusta modelos bayesianos (inclusive hierárquicos)
#              prontos, usando Stan pré-compilado por debaixo dos panos
# bayesplot -> gráficos padronizados de diagnóstico
# dplyr     -> manipulação de dados
# ggplot2   -> gráficos
# install.packages(c("nlme","rstanarm","bayesplot","dplyr","ggplot2"))
library(nlme)
library(rstanarm)
library(bayesplot)
library(dplyr)
library(ggplot2)

set.seed(42)

# ============================================================
# ETAPA 1 — DADOS E ESTRUTURA HIERÁRQUICA
# ============================================================
# Dataset real "High School and Beyond": cada linha é um aluno,
# com o seu nível socioeconômico (SES), a nota em matemática
# (MathAch) e a escola a que pertence.

data(MathAchieve)

df <- MathAchieve %>%
  transmute(school = as.factor(School), ses = SES, y = MathAch) %>%
  na.omit()

n <- nrow(df)
J <- length(unique(df$school))
cat("Alunos:", n, " | Escolas:", J, "\n")

# Visualizando a heterogeneidade entre escolas: cada caixa é uma
# escola. Se as caixas estivessem todas na mesma altura, não
# precisaríamos de um modelo hierárquico — mas elas claramente
# não estão.
ggplot(df, aes(x = school, y = y)) +
  geom_boxplot(outlier.size = 0.5) +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  labs(title = "Desempenho em matemática por escola",
       subtitle = "Cada caixa é uma escola — note a heterogeneidade entre grupos",
       x = "Escola", y = "MathAch")

# ============================================================
# ETAPA 2 — AJUSTANDO O MODELO HIERÁRQUICO (stan_lmer)
# ============================================================
# O modelo tem a seguinte estrutura, exatamente como vimos em sala:
#
#   y_ij = beta_0 + beta_1 * ses_ij + u_j + eps_ij
#
#   u_j    ~ N(0, tau^2)     efeito aleatório da escola j
#                            (o quanto aquela escola desvia da média geral)
#   eps_ij ~ N(0, sigma^2)   ruído dentro da escola (variação entre alunos)
#
#   beta_0, beta_1 ~ N(0, 10^2)     priors fracamente informativos
#   tau, sigma                      priors default do rstanarm
#                                    (weakly informative, calibrados
#                                     automaticamente pela escala dos dados)
#
# Na sintaxe do rstanarm (herdada do lme4), "(1 | school)" é
# exatamente o "u_j": um intercepto que varia aleatoriamente por
# escola. Essa notação é o padrão da indústria para especificar
# modelos multinível em R — a mesma sintaxe funciona também no
# lme4 (versão não-bayesiana) e no brms.
#
# stan_lmer() é o equivalente hierárquico do stan_glm() usado no
# script 1, para resposta CONTÍNUA (Gaussiana). Esta MESMA função
# serve para qualquer estrutura de grupos — troque "school" pelo
# nome da sua variável de agrupamento (hospital, cidade, turma, etc).

fit <- stan_lmer(
  y ~ ses + (1 | school),
  data = df,
  prior            = normal(0, 10),   # prior para o coeficiente de ses
  prior_intercept  = normal(0, 10),   # prior para o intercepto
  chains = 4, iter = 4000, seed = 42
)

# ============================================================
# ETAPA 3 — DIAGNÓSTICO DE CONVERGÊNCIA
# ============================================================
# Assim como no script 1, rodar o algoritmo não garante nada por
# si só — precisamos checar se as amostras representam bem a
# posterior. O rstanarm já calcula tudo isso automaticamente.

print(fit)
print(summary(fit))
# No summary, para cada parâmetro:
#   mean / sd       -> média e desvio-padrão posterior
#   Rhat            -> deve estar bem próximo de 1.00
#   n_eff           -> tamanho efetivo de amostra
#                      (queremos valores altos, tipicamente > 400)

# trace plots dos parâmetros fixos (intercepto e efeito de ses) e
# do desvio-padrão residual — uma checagem visual rápida
plot(fit, plotfun = "trace", pars = c("(Intercept)", "ses", "sigma"))

cat("\nSe o Rhat de todos os parâmetros estiver <= 1.01 e o n_eff for alto,\n")
cat("podemos confiar nas amostras da posterior.\n")

# ============================================================
# ETAPA 4 — INTERPRETANDO OS COMPONENTES DE VARIÂNCIA
# ============================================================
# A grande pergunta de um modelo hierárquico é: QUANTO da
# variação total no desempenho dos alunos se deve a DIFERENÇAS
# ENTRE ESCOLAS, e quanto se deve a diferenças ENTRE ALUNOS
# dentro da mesma escola?

vc <- as.data.frame(VarCorr(fit))
tau2   <- vc$vcov[vc$grp == "school"]     # variância entre escolas
sigma2 <- vc$vcov[vc$grp == "Residual"]   # variância dentro da escola

# ICC (coeficiente de correlação intraclasse): a proporção da
# variância total que é "entre escolas". Um ICC alto significa
# que a escola importa muito para explicar o desempenho do aluno.
icc <- tau2 / (tau2 + sigma2)

cat("\nVariância entre escolas (tau^2):", round(tau2, 3), "\n")
cat("Variância dentro das escolas (sigma^2):", round(sigma2, 3), "\n")
cat("ICC (proporção da variância explicada pela escola):", round(icc, 3), "\n")

# efeito de SES (nível socioeconômico) sobre o desempenho —
# extraído diretamente da posterior
posterior_samples <- as.data.frame(fit)
p_positivo_ses <- mean(posterior_samples$ses > 0)
cat("P(efeito de SES > 0 | dados):", round(p_positivo_ses, 3), "\n")
cat("(perto de 1 = temos bastante certeza de que SES aumenta o desempenho)\n")

# ============================================================
# ETAPA 5 — POOLING PARCIAL: A GRANDE VANTAGEM DO MODELO
# ============================================================
# Vamos comparar a média "crua" de cada escola (simplesmente a
# média dos alunos daquela escola) com a média que o MODELO
# estima para aquela escola (que combina a informação da escola
# com a informação de todas as outras escolas).
#
# ranef() extrai o efeito aleatório estimado (u_j) de cada escola.

efeitos_escola <- ranef(fit)$school
beta0_post <- fixef(fit)["(Intercept)"]

medias_cruas    <- tapply(df$y, df$school, mean)
tamanho_escola  <- as.numeric(table(df$school))

df_pooling <- data.frame(
  escola = rownames(efeitos_escola),
  n_alunos = tamanho_escola,
  media_crua = as.numeric(medias_cruas[rownames(efeitos_escola)]),
  media_bayes = beta0_post + efeitos_escola[["(Intercept)"]]
)

ggplot(df_pooling, aes(x = media_crua, y = media_bayes, size = n_alunos)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  geom_hline(yintercept = beta0_post, linetype = "dotted", color = "red") +
  labs(title = "Pooling parcial: shrinkage em direção à média geral",
       subtitle = "Escolas pequenas (pontos menores) encolhem mais em direção à linha vermelha",
       x = "Média crua da escola", y = "Média estimada pelo modelo (bayesiana)") +
  theme_minimal()

cat("\nObserve como escolas com poucos alunos (pontos pequenos) desviam mais\n")
cat("da reta y=x — é o efeito do 'partial pooling': quando há pouca informação\n")
cat("sobre uma escola específica, o modelo confia mais na média geral.\n")
cat("Escolas com muitos alunos (pontos grandes) ficam quase sobre a reta,\n")
cat("porque já têm evidência suficiente para 'falar por si mesmas'.\n")

# ============================================================
# COMO ADAPTAR ESTE SCRIPT PARA OUTRO DATASET
# ============================================================
# 1. Troque o carregamento dos dados (Etapa 1) pelo seu próprio
#    data.frame, com uma variável resposta contínua, uma ou mais
#    covariáveis, e uma variável de agrupamento (escola, hospital,
#    cidade, turma, indivíduo em medidas repetidas, etc).
# 2. Ajuste a fórmula em stan_lmer() (Etapa 2):
#      y ~ x1 + x2 + (1 | grupo)
#    Se quiser que o EFEITO de x1 também varie por grupo (não só
#    o intercepto), use:
#      y ~ x1 + x2 + (1 + x1 | grupo)
# 3. Se a variável resposta for BINÁRIA (0/1) em vez de contínua,
#    troque stan_lmer() por stan_glmer(..., family = binomial()) —
#    mesma sintaxe de fórmula, mesma lógica de diagnóstico.
# 4. Todo o resto do script (diagnóstico, componentes de
#    variância, ICC, pooling parcial) funciona sem alteração.