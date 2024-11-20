
# Usando uma probabilidade especifica
# Tempo de 2 a 4 semena
# Volume de vendas por semana 5 que é o valor de lambda
# 
# Qual é a probabilidade de ocorrer de 2 a 4 vendas nas proximas semanas sendo que a taxa media de 
# vendas é de 5 por semana

dpois(x = 2, lambda = 5) + dpois(x = 3, lambda = 5) + dpois(x = 4, lambda = 5)

# Usando probabilidade acumulada 
#
ppois(q = 4, lambda = 5) 
ppois(q = 4, lambda = 5, lower.tail = TRUE)
ppois(q = 4, lambda = 5, lower.tail = FALSE)
ppois(q = 4, lambda = 5, lower.tail = TRUE) - ppois(q = 1, lambda = 5, lower.tail = TRUE)

# Alterando o valçor do lower.tail
#
ppois(q = 4, lambda = 5, lower.tail = FALSE) - ppois(q = 1, lambda = 5, lower.tail = FALSE)

# Vamos plotar todas as possibilidades
x <- ppois(q = 0:10, lambda = 5,  lower.tail = TRUE)
barplot(x, names.arg = 0:10, space = 0)

# Qual a probabilidade de qualquer numero de vendas
# 
#
# Importa a Pacotes
library(ggplot2)

# Formatar valores com casas decimais

options(scipen = 999, digits = 2) 

# Definir o numero de num_eventos 

num_eventos <- 0:10

# Calcula as probabilidades para todos os num_eventos utiulizando dpois e armazendo em uma variavel

probab  <- dpois(x = num_eventos, lambda = 5)
probab 


# Calcula as probabilidades acumuladas para todos os num_eventos

probab_acumulada <- ppois(q = num_eventos, lambda = 5, lower.tail = TRUE)

# Consolidar o resultado em um dataframe para usar o ggplot
# É necessario um data frame

df <- data.frame(num_eventos, probab , probab_acumulada)
df

# Plot (cuidado com a escala do plot)

# Probabilidada nao acumulada
#
ggplot(df, aes(x = factor(num_eventos), y = probab )) +
  geom_col() +
  geom_text(aes(label = round(probab ,2), y = probab  + 0.01), position = position_dodge(0.9), size = 5, vjust = 0) +
  labs(title = "Distribuicao Poisson Para Calcular a Probabilidade de Vendas Pelo periodo", 
       x = "Evento (Numero de Vendas)", 
       y = "Probabilidade") 

# Probabilidade acumulada
#
ggplot(df, aes(x = factor(num_eventos), y = probab )) +
  geom_col() +
  geom_text(aes(label = round(probab ,2), y = probab  + 0.01), position = position_dodge(0.9), size = 5, vjust = 0) +
  labs(title = "Distribuicao Poisson Para Calcular a Probabilidade de Vendas Pelo periodo", 
       x = "Evento (Numero de Vendas)", 
       y = "Probabilidade") +
  geom_line(data = df, aes(x = num_eventos, y = probab_acumulada))



