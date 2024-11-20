# 
#  Distribuicao para variaveis continuas

setwd("H:/_EAD/_DISCIPLINAS/ANALYTICS/FORMULARIOS/REVISAO01/Aula04_Probabilidades")
getwd()



num_amostras <- 600                          
min <- 1                                  
max <- 6

# Geracao de valores com a funcao sample
# size 1 que é uma face do dado
# replace = true para incluir o valor retirado na proxima amostragem

sample(min:max, size = 1, replace = TRUE)

# Armazenando 600 valores

# Cria um vetor vazio

lances<- vector("numeric") 

# Executa 600 lances

for (i in 1:num_amostras) {
  x <- sample(min:max, size = 1, replace = TRUE)
  lances[i] <- x 
}
View(lances)

#  Mostra a quantidade que cada lado do lado aparece

table(lances)


# Segundo formato
# A funcao unif cria uma distribuição uniforme de valores 

lances2 <- runif(num_amostras, min = 1, max = 6 + 1)     
View(lances2)

# Arredonadamento de resultados inclusive para o valor 6

lances2 <- as.integer(runif(num_amostras, min = 1, max = 6 + 1))  
View(lances2)
table(lances2)

# Histograma

hist(lances,  main = paste(num_amostras," Lances  - Jogando um dado"), breaks = seq(min-.5, max+.5, 1))  
hist(lances2, main = paste(num_amostras," Lances2 - Jogando um dado"), breaks = seq(min-.5, max+.5, 1)) 

# Qual a probabilidade de jogar um dado de 6 faces obter o valor 4? E o valor 6?
#

dunif(4, min = 1, max = 6 + 1) 
dunif(6, min = 1, max = 6 + 1) 

# Pacote dice para fazer simulacoes

install.packages("dice")

library(dice)
?getEventProb

# Funcao que dimula o jogar dados de 6 faces o valor 5?

getEventProb(nrolls = 1,
             ndicePerRoll = 1,
             nsidesPerDie = 6,
             eventList = list(5))

# Qual a probabilidade de jogar um dado de 6 faces e obter o valor 1 ou 2?
# 2/6 = 0.33333

# Para calcular densidade acumulada utiliza-se a funcao CDF ?punif
# o valor 1 sera zero
#
punif(1, min = 1, max = 6 + 1) 
punif(2, min = 1, max = 6 + 1) 
punif(3, min = 1, max = 6 + 1) 

# Validando o resultado  getEventProb()
getEventProb(nrolls = 1,
             ndicePerRoll = 1,
             nsidesPerDie = 6,
             eventList = list(1:2))


# Qual a probabilidade de jogar um dado de 6 faces 10 vezes e obter o valor 5?
# 
dbinom(x = 1, size = 10, prob = 1/6)

# Qual a probabilidade de jogar um dado de 6 faces 10 vezes e obter o valor 5 menos de 4 vezes?
# 
pbinom(4, size = 10, prob = 1/6)

