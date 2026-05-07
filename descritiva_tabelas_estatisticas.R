############################################################
# Tabelas estatísticas usando o R
# Prof. Vinícius Osterne, PhD
############################################################


#===========================================================
# 1. TABELA DE FREQUÊNCIA
#===========================================================

gols <- c(
  0, 1, 1, 2, 2, 2,
  3, 3, 4, 4, 4, 5
)

gols

table(gols)

prop.table(table(gols))

prop.table(table(gols)) * 100


#===========================================================
# 2. DADOS AGRUPADOS EM CLASSES
#===========================================================

salarios <- c(
  1200, 1500, 1800, 2000, 2200,
  2500, 2700, 3000, 3200, 3500,
  4000, 4500, 5000
)

salarios

classes <- cut(
  
  salarios,
  
  breaks = c(1000, 2000, 3000, 4000, 5000),
  
  right = FALSE
)

classes

table(classes)

prop.table(table(classes))

prop.table(table(classes)) * 100


#===========================================================
# 3. TABELA DE CONTINGÊNCIA
#===========================================================

sexo <- c(
  "Masculino", "Feminino", "Feminino",
  "Masculino", "Masculino", "Feminino",
  "Feminino", "Masculino"
)

situacao <- c(
  "Aprovado", "Aprovado", "Reprovado",
  "Aprovado", "Reprovado", "Aprovado",
  "Aprovado", "Reprovado"
)

sexo
situacao

table(sexo, situacao)

prop.table(table(sexo, situacao))

prop.table(table(sexo, situacao), margin = 1)

prop.table(table(sexo, situacao), margin = 2)
