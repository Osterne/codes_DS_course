############################################################
# Medidas de forma usando o R
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
# 1. ASSIMETRIA
#===========================================================

# A assimetria mede o grau
# de simetria da distribuição.

#-----------------------------------------------------------
# Cálculo manual
#-----------------------------------------------------------

media <- mean(dados)
media

n <- length(dados)
n

m2 <- sum((dados - media)^2) / n
m2

m3 <- sum((dados - media)^3) / n
m3

assimetria_manual <- m3 / (m2^(3/2))
assimetria_manual


# Assimetria ≈ 0  -> □■■■■■■■■■□
# Assimetria > 0  -> ■■■■■■■■■□□□□□
# Assimetria < 0  -> □□□□□■■■■■■■■■
# |Assimetria| > 1 -> cauda longa (assimetria forte)
# 0.5 < |Assimetria| < 1 -> cauda moderada
# |Assimetria| < 0.5 -> distribuição quase simétrica



#-----------------------------------------------------------
# Utilizando no R
#-----------------------------------------------------------

#install.packages("moments")
library(moments)

assimetria_r <- skewness(dados)
assimetria_r


#===========================================================
# 2. CURTOSE
#===========================================================

# A curtose mede o grau
# de achatamento da distribuição.

#-----------------------------------------------------------
# Cálculo manual
#-----------------------------------------------------------

media <- mean(dados)
media

n <- length(dados)
n

m2 <- sum((dados - media)^2) / n
m2

m4 <- sum((dados - media)^4) / n
m4

curtose_manual <- m4 / (m2^2)
curtose_manual


# Curtose ≈ 3  -> □■■■■■■■■■□  (mesocúrtica)
# Curtose > 3  -> □□■■■■■□□    (leptocúrtica)
# Curtose < 3  -> ■■■■■■■■■■   (platicúrtica)

# Curtose alta  -> pico mais alto e caudas mais pesadas
# Curtose baixa -> distribuição mais achatada

#-----------------------------------------------------------
# Utilizando no R
#-----------------------------------------------------------

curtose_r <- kurtosis(dados)
curtose_r


#===========================================================
# RESUMO FINAL
#===========================================================

data.frame(
  
  Medida = c(
    "Assimetria",
    "Curtose"
  ),
  
  Manual = c(
    assimetria_manual,
    curtose_manual
  ),
  
  Funcao_R = c(
    assimetria_r,
    curtose_r
  )
)
