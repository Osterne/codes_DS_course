############################################################
# Medidas de posição usando o R
# Prof. Vinícius Osterne, PhD
############################################################

#-----------------------------------------------------------
# Contexto do problema
#-----------------------------------------------------------

# Cada valor representa a idade
# de um grupo de pessoas.

dados <- c(20, 20, 18, 21, 25, 30, 35)
dados


#===========================================================
# 1. MODA
#===========================================================

# A moda representa a idade
# que mais aparece no grupo.

#-----------------------------------------------------------
# Cálculo manual
#-----------------------------------------------------------

frequencias <- table(dados)
frequencias

maior_frequencia <- max(frequencias)
maior_frequencia

moda_manual <- names(frequencias[frequencias == maior_frequencia])
moda_manual

#-----------------------------------------------------------
# Utilizando no R
#-----------------------------------------------------------

moda_r <- as.numeric(names(table(dados))[table(dados) == max(table(dados))])
moda_r


#===========================================================
# 2. MEDIANA
#===========================================================

# A mediana representa a idade
# central do grupo.

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

posicao_central <- (n + 1) / 2
posicao_central

mediana_manual <- dados_ordenados[posicao_central]
mediana_manual

#-----------------------------------------------------------
# Utilizando a função do R
#-----------------------------------------------------------

mediana_r <- median(dados)
mediana_r


#===========================================================
# 3. MÉDIA ARITMÉTICA
#===========================================================

# A média aritmética representa
# a idade média do grupo.

#-----------------------------------------------------------
# Cálculo manual
#-----------------------------------------------------------

soma_dados <- sum(dados)
soma_dados

quantidade <- length(dados)
quantidade

media_manual <- soma_dados / quantidade
media_manual

#-----------------------------------------------------------
# Utilizando a função do R
#-----------------------------------------------------------

media_r <- mean(dados)
media_r


#===========================================================
# 4. MÉDIA PONDERADA
#===========================================================

# Suponha agora que cada pessoa
# possui uma importância diferente
# na pesquisa.

idades <- c(18, 20, 25)
pesos <- c(1, 2, 4)

idades
pesos

#-----------------------------------------------------------
# Cálculo manual
#-----------------------------------------------------------

produto <- idades * pesos
produto

soma_produto <- sum(produto)
soma_produto

soma_pesos <- sum(pesos)
soma_pesos

media_ponderada_manual <- soma_produto / soma_pesos
media_ponderada_manual

#-----------------------------------------------------------
# Utilizando a função do R
#-----------------------------------------------------------

media_ponderada_r <- weighted.mean(idades, pesos)
media_ponderada_r


#===========================================================
# 5. MÉDIA GEOMÉTRICA
#===========================================================

# Suponha o crescimento percentual
# da idade média de uma população
# ao longo do tempo.

dados_geo <- c(1.02, 1.03, 1.05)
dados_geo

#-----------------------------------------------------------
# Cálculo manual
#-----------------------------------------------------------

produto_geo <- prod(dados_geo)
produto_geo

n_geo <- length(dados_geo)
n_geo

media_geometrica_manual <- produto_geo^(1 / n_geo)
media_geometrica_manual

#-----------------------------------------------------------
# Utilizando diretamente no R
#-----------------------------------------------------------

media_geometrica_r <- prod(dados_geo)^(1 / length(dados_geo))
media_geometrica_r


#===========================================================
# 6. MÉDIA HARMÔNICA
#===========================================================

# Suponha velocidades médias
# de deslocamento de pessoas.

dados_har <- c(40, 60, 80)
dados_har

#-----------------------------------------------------------
# Cálculo manual
#-----------------------------------------------------------

inversos <- 1 / dados_har
inversos

soma_inversos <- sum(inversos)
soma_inversos

n_har <- length(dados_har)
n_har

media_harmonica_manual <- n_har / soma_inversos
media_harmonica_manual

#-----------------------------------------------------------
# Utilizando diretamente no R
#-----------------------------------------------------------

media_harmonica_r <- length(dados_har) / sum(1 / dados_har)
media_harmonica_r


#===========================================================
# RESUMO FINAL
#===========================================================

data.frame(
  
  Medida = c(
    "Moda",
    "Mediana",
    "Média Aritmética",
    "Média Ponderada",
    "Média Geométrica",
    "Média Harmônica"
  ),
  
  Manual = c(
    as.numeric(moda_manual),
    mediana_manual,
    media_manual,
    media_ponderada_manual,
    media_geometrica_manual,
    media_harmonica_manual
  ),
  
  Funcao_R = c(
    moda_r,
    mediana_r,
    media_r,
    media_ponderada_r,
    media_geometrica_r,
    media_harmonica_r
  )
)
