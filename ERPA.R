
dados <- read.csv("data.csv")

#Substituição da virgula pelo ponto decimal
dados$Resultado_da_Pesquisa <- as.numeric(sub(",", ".", dados$Resultado_da_Pesquisa, fixed = TRUE))
dados$Brancos_Nulos <- as.numeric(sub(",", ".", dados$Brancos_Nulos, fixed = TRUE))
dados$Indecisos <- as.numeric(sub(",", ".", dados$Indecisos, fixed = TRUE))
attach(dados)


