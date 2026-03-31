############################################################
# METANÁLISE APLICADA À SAÚDE – EXEMPLO COMPLETO EM R
# Autor: Vinícius Osterne (exemplo para aula de metanálise)
#
# Contexto:
#   - Ensaios clínicos randomizados que avaliam um novo medicamento
#     (Intervenção) para reduzir crises de asma em adultos.
#   - Desfecho binário: ocorrência de pelo menos 1 crise de asma
#     durante o seguimento (sim/não).
#   - Vamos simular/definir uma base com vários estudos.
#
# Objetivo:
#   - Combinar os resultados dos estudos usando metanálise
#     de desfechos binários (risco relativo).
#   - Gerar forest plot, funnel plot e testar viés de publicação.
############################################################


############################################################
# 0) PREPARAR AMBIENTE
############################################################

# Instalar os pacotes (descomente na primeira vez)
# install.packages("meta")
# install.packages("metafor")

library(meta)     # funções de metanálise (metabin, metacont etc.)
library(metafor)  # funções adicionais, meta-regressão, testes etc.


############################################################
# 1) CONSTRUIR UMA BASE DE DADOS EXEMPLO
############################################################

estudo <- c(
  "Estudo A (2010)",
  "Estudo B (2011)",
  "Estudo C (2012)",
  "Estudo D (2014)",
  "Estudo E (2015)",
  "Estudo F (2016)",
  "Estudo G (2017)",
  "Estudo H (2018)",
  "Estudo I (2019)",
  "Estudo J (2020)",
  "Estudo K (2021)",
  "Estudo L (2022)"
)

# Tamanho dos grupos (valores plausíveis)
n_trat <- c(120, 80, 60, 150, 90, 110, 75, 130, 95, 85, 100, 140)
n_ctrl <- c(118, 78, 62, 148, 88, 105, 77, 128, 97, 84, 102, 138)

# Eventos (redução de risco mais evidente nos grandes estudos)
event_trat <- c(18, 14, 12, 30, 10, 16, 11, 21, 13, 15, 18, 25)
event_ctrl <- c(32, 22, 15, 45, 18, 23, 16, 29, 20, 19, 24, 33)

# Qualidade (apenas para exemplo)
qualidade <- c(
  "alta", "moderada", "alta", "alta",
  "moderada", "alta", "baixa", "moderada",
  "alta", "moderada", "alta", "alta"
)

# Montar a base
dados_meta <- data.frame(
  estudo,
  n_trat,
  event_trat,
  n_ctrl,
  event_ctrl,
  qualidade
)

dados_meta

str(dados_meta)


############################################################
# 2) EXECUTAR A METANÁLISE (DESFECHO BINÁRIO – RISCO RELATIVO)
############################################################
# Vamos usar a função metabin() do pacote {meta}.
#   - event.e = eventos no grupo experimental (tratamento)
#   - n.e     = tamanho do grupo experimental
#   - event.c = eventos no grupo controle
#   - n.c     = tamanho do grupo controle
#   - sm      = "RR" (Risk Ratio) ou "OR" (Odds Ratio)
#   - method  = método de pooling (por ex. "MH" = Mantel-Haenszel)

meta_asma <- metabin(
  event.e = event_trat,
  n.e     = n_trat,
  event.c = event_ctrl,
  n.c     = n_ctrl,
  studlab = estudo,
  data    = dados_meta,
  sm      = "RR",      # medida de efeito: risco relativo
  method  = "MH",      # método de combinação (Mantel-Haenszel)
  random  = TRUE,      # modelo de efeitos aleatórios
  fixed   = FALSE,     # se quiser mostrar só o random
  hakn    = TRUE       # ajuste de Hartung-Knapp (ICs mais conservadores)
)

# Ver resumo numérico da metanálise
summary(meta_asma)

# Interpretação básica (para falar em aula):
# - TE.random = log(RR) combinado (na escala logarítmica)
# - exp(TE.random) = RR combinado
# - Se o IC de 95% para RR NÃO inclui 1, o efeito é estatisticamente significativo.
# - I^2 = medida de heterogeneidade (% da variação explicada por diferenças entre estudos).


############################################################
# 3) FOREST PLOT (GRÁFICO DE FLORESTA)
############################################################
# Esse gráfico resume:
#   - Resultado individual de cada estudo (RR + IC 95%)
#   - Peso de cada estudo na metanálise
#   - Efeito combinado (losango)

forest(meta_asma,
       sortvar = TE,            # opcional: ordenar pelos efeitos
       xlab    = "Risco relativo (RR)",
       leftlabs = c("Estudo", "Trat.", "Ctrl."),
       lab.e   = "Novo fármaco",
       lab.c   = "Trat. padrão",
       print.tau2 = TRUE,
       col.diamond = "blue",
       col.diamond.lines = "black")

# Em aula, você pode interpretar assim:
# - Se a maior parte dos quadradinhos estiver à esquerda da linha RR = 1,
#   o tratamento reduz o risco de crises.
# - O losango à esquerda de 1 indica efeito protetor global.


############################################################
# 4) FUNNEL PLOT E TESTE DE VIÉS DE PUBLICAÇÃO
############################################################
# Funnel plot: avalia simetricamente a distribuição dos estudos
# em torno do efeito combinado. Assimetria pode sugerir viés de publicação.

funnel(meta_asma,
       xlab = "Risco relativo (RR)",
       studlab = TRUE)

# Teste de Egger (regressão linear para assimetria de funil)
# No {meta}, usamos metabias().

metabias(meta_asma,
         method.bias = "linreg")   # método de Egger

# Interpretação:
# - p-valor pequeno (ex.: p < 0.05) sugere assimetria do funil
#   e possível viés de publicação.
# - Em aula, sempre enfatizar que é uma sugestão, não prova absoluta.


############################################################
# 5) OPCIONAL: REPETIR A ANÁLISE COM {metafor}
############################################################
# {metafor} permite um controle ainda mais fino, por exemplo
# se você quiser fazer meta-regressão ou modelos mais gerais.

# Primeiro, calcular manualmente o log(RR) e sua variância
escalc_rr <- escalc(
  measure = "RR",
  ai = event_trat,  # eventos tratamento
  bi = n_trat - event_trat,
  ci = event_ctrl,  # eventos controle
  di = n_ctrl - event_ctrl,
  data = dados_meta
)

# Ajustar um modelo de efeitos aleatórios (DerSimonian-Laird por padrão)
rma_asma <- rma(yi = yi, vi = vi, data = escalc_rr, method = "DL")

rma_asma
exp(rma_asma$b)          # RR combinado (na escala original)
exp(confint(rma_asma)$ci.lb)
exp(confint(rma_asma)$ci.ub)

# Forest plot com {metafor}
forest(rma_asma,
       slab = dados_meta$estudo,
       xlab = "Risco relativo (RR)")

# Funnel plot com {metafor}
funnel(rma_asma,
       xlab = "Risco relativo (RR)")

# Teste de Egger (regtest em {metafor})
regtest(rma_asma, model = "rma", predictor = "sei")  # método clássico


############################################################
# 6) IDEIA PARA VERSÃO CONTÍNUA (ESQUELETO)
############################################################
# Se depois você quiser mostrar um exemplo com desfecho contínuo,
# a função equivalente é metacont(). Exemplo de esqueleto:

# Exemplo direto (não executado aqui):
# meta_cont <- metacont(
#   n.e, mean.e, sd.e,
#   n.c, mean.c, sd.c,
#   data = sua_base,
#   studlab = estudo,
#   sm = "MD"   # ou "SMD" para diferença padronizada
# )
# summary(meta_cont)
# forest(meta_cont)

############################################################
# FIM DO SCRIPT
############################################################
