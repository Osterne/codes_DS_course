############################################################
# Medidas de dispersão usando o R
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
# 1. AMPLITUDE
#===========================================================

# A amplitude representa a diferença
# entre o maior e o menor valor.

#-----------------------------------------------------------
# Cálculo manual
#-----------------------------------------------------------

maior_valor <- max(dados)
maior_valor

menor_valor <- min(dados)
menor_valor

amplitude_manual <- maior_valor - menor_valor
amplitude_manual

#-----------------------------------------------------------
# Utilizando no R
#-----------------------------------------------------------

amplitude_r <- max(dados) - min(dados)
amplitude_r


#===========================================================
# 2. VARIÂNCIA
#===========================================================

# A variância mede o grau de dispersão
# dos dados em torno da média.

#-----------------------------------------------------------
# Cálculo manual
#-----------------------------------------------------------

media <- mean(dados)
media

desvios <- dados - media
desvios

desvios_quadrado <- desvios^2
desvios_quadrado

soma_quadrados <- sum(desvios_quadrado)
soma_quadrados

n <- length(dados)
n

variancia_manual <- soma_quadrados / (n - 1)
variancia_manual

#-----------------------------------------------------------
# Utilizando a função do R
#-----------------------------------------------------------

variancia_r <- var(dados)
variancia_r


#===========================================================
# 3. DESVIO PADRÃO
#===========================================================

# O desvio padrão representa
# a dispersão média dos dados.

#-----------------------------------------------------------
# Cálculo manual
#-----------------------------------------------------------

desvio_padrao_manual <- sqrt(variancia_manual)
desvio_padrao_manual

#-----------------------------------------------------------
# Utilizando a função do R
#-----------------------------------------------------------

desvio_padrao_r <- sd(dados)
desvio_padrao_r


#===========================================================
# 4. COEFICIENTE DE VARIAÇÃO
#===========================================================

# O coeficiente de variação mede
# a dispersão relativa dos dados.

#-----------------------------------------------------------
# Cálculo manual
#-----------------------------------------------------------

coeficiente_variacao_manual <- 
  (desvio_padrao_manual / media) * 100

coeficiente_variacao_manual

#-----------------------------------------------------------
# Utilizando diretamente no R
#-----------------------------------------------------------

coeficiente_variacao_r <- 
  (sd(dados) / mean(dados)) * 100

coeficiente_variacao_r


#===========================================================
# RESUMO FINAL
#===========================================================

data.frame(
  
  Medida = c(
    "Amplitude",
    "Variância",
    "Desvio Padrão",
    "Coeficiente de Variação"
  ),
  
  Manual = c(
    amplitude_manual,
    variancia_manual,
    desvio_padrao_manual,
    coeficiente_variacao_manual
  ),
  
  Funcao_R = c(
    amplitude_r,
    variancia_r,
    desvio_padrao_r,
    coeficiente_variacao_r
  )
)
