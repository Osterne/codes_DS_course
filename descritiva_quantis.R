############################################################
# Quantis usando o R
# Prof. Vinícius Osterne, PhD
############################################################

#-----------------------------------------------------------
# Contexto do problema
#-----------------------------------------------------------

# Cada valor representa a idade
# de um grupo de pessoas.

dados <- c(18, 20, 20, 21, 25, 30, 35)
dados


#===========================================================
# 1. QUARTIS
#===========================================================

# Quartis dividem os dados
# em quatro partes iguais.

#-----------------------------------------------------------
# Ordenando os dados
#-----------------------------------------------------------

dados_ordenados <- sort(dados)
dados_ordenados

#-----------------------------------------------------------
# Cálculo manual
#-----------------------------------------------------------

n <- length(dados_ordenados)
n

pos_q1 <- (n + 1) * 0.25
pos_q1

pos_q2 <- (n + 1) * 0.50
pos_q2

pos_q3 <- (n + 1) * 0.75
pos_q3

q1_manual <- dados_ordenados[pos_q1]
q1_manual

q2_manual <- dados_ordenados[pos_q2]
q2_manual

q3_manual <- dados_ordenados[pos_q3]
q3_manual

#-----------------------------------------------------------
# Utilizando a função do R
#-----------------------------------------------------------

quartis_r <- quantile(dados, probs = c(0.25, 0.50, 0.75))
quartis_r


#===========================================================
# 2. PERCENTIS
#===========================================================

# Percentis dividem os dados
# em 100 partes iguais.

#-----------------------------------------------------------
# Cálculo manual
#-----------------------------------------------------------

pos_p10 <- (n + 1) * 0.10
pos_p10

pos_p90 <- (n + 1) * 0.90
pos_p90

# Como as posições não são inteiras,
# utilizamos aproximação.

p10_manual <- dados_ordenados[1] +
  (pos_p10 - 1) *
  (dados_ordenados[2] - dados_ordenados[1])

p10_manual

p90_manual <- dados_ordenados[6] +
  (pos_p90 - 6) *
  (dados_ordenados[7] - dados_ordenados[6])

p90_manual

#-----------------------------------------------------------
# Utilizando a função do R
#-----------------------------------------------------------

percentis_r <- quantile(dados, probs = c(0.10, 0.90))
percentis_r


#===========================================================
# INTERPRETAÇÃO DOS QUANTIS
#===========================================================

# Quartil 1  -> 25% dos dados abaixo
# Quartil 2  -> 50% dos dados abaixo
# Quartil 3  -> 75% dos dados abaixo

# Percentil 10 -> 10% dos dados abaixo
# Percentil 90 -> 90% dos dados abaixo


#===========================================================
# RESUMO FINAL
#===========================================================

data.frame(
  
  Medida = c(
    "Q1",
    "Q2",
    "Q3",
    "P10",
    "P90"
  ),
  
  Manual = c(
    q1_manual,
    q2_manual,
    q3_manual,
    p10_manual,
    p90_manual
  ),
  
  Funcao_R = c(
    quartis_r[1],
    quartis_r[2],
    quartis_r[3],
    percentis_r[1],
    percentis_r[2]
  )
)
