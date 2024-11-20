#
# Distribui��o Normal - 
#
# numero de vendas por dia em um loja
#
numero_vendas = c(3, 5, 2, 3, 3, 6, 3, 10, 5, 5, 5, 7, 8, 7, 1, 5, 5, 4, 4, 7)

numero_vendas = sample(1:10, 20, replace=TRUE)

par(mar=c(1, 1, 1, 1))
rnorm(10)
?rnorm
sample(1:10, 20, replace=TRUE)
?hist
#
# Contagem de frequencia
#
hist(numero_vendas)

# Teremos 5 "barras" � um grafico que mostra a frequencia

#
# 
#
#
mean(numero_vendas)
sd(numero_vendas)

# 
#  Histograma customizado
#
hist(numero_vendas, 
     main = "Frequencia do Numero de Vendas Por Dia", 
     xlab = "Numero de Vendas Por Dia",
     ylab = "Frequencia",
     labels = TRUE)


#
# Geralmente o histograma tem a forma de sino
#
# Histograma de frequencia mostra a contagem de elementos
# Histograma de densidade as probabilidades para um range de valores
# probability
# 
hist(numero_vendas, 
     main = "Densidade do Numero de Vendas Por Dia", 
     xlab = "Numero de Vendas Por Dia",
     ylab = "Densidade de Probabilidade", 
     probability = TRUE)

# O eixo y mostra a densidade de probabilidades
# Incluir uma linha com o qual das densidades de uma variavel
#
?lines
lines(density(numero_vendas), col = "blue", lwd = 2)


# lines tra�ar todas as probalidades  possiveis da variavel dentro de histograma
#
# Como os valores foram calculados de forma aleatorio os valores da media e do desvio padrao pode ser diferente
#
?dnorm
dnorm(numero_vendas, mean = 4.9, sd = 2.17)




# PDF (Probability Density Function) � usado para especificar a probabilidade da variavel aleatoria cair dentro de um 
# determinado intervalo de valores, em vez de assumir qualquer valor. 
#
x = seq(-3, 3, 0.1)
plot(x = x, y = dnorm(x), type = "l", bty = "n")

# A curva de sino é uma curva de densidade e a área sob a curva de sino entre um conjunto de valores 
# representa a porcentagem de numeros na distribuicao entre esses valores.

# Em teoria das probabilidades e estatística, a funcao densidade de probabilidade, ou densidade de uma variável 
# aleatória contínua, é uma funcao que descreve a probabilidade relativa de uma variável aleatória tomar um valor 
# dado. A probabilidade da variável aleatória cair em uma faixa particular é dada pela integral da densidade dessa 
# variável sobre tal faixa - isto é, é dada pela área abaixo da funcao densidade mas acima do eixo horizontal 
# e entre o menor e o maior valor dessa faixa. A funcao densidade de probabilidade é não negativa sempre, e sua 
# integral sobre todo o espaço é igual a um. A funcao densidade pode ser obtida a partir da funcao distribuicao 
# acumulada a partir da operacao de derivacao (quando esta é derivável).

# Para variáveis aleatórias contínuas, as probabilidades são representadas pelas áreas sob a curva.

# O valor médio de uma distribuicao normal é a média, e a largura da curva de sino é definida pelo desvio padrão.

# Regra 68-95-99 para a Distribuicao Normal

# 68,2% dos valores estão dentro de 1 desvio padrão da média
# 95,4% dos valores estão dentro de 2 desvios padrão da média
# 99,7% dos valores estão dentro de 3 desvios padrão da média

# O numero de desvios padrão dos quais um valor se afasta da média é chamado de escore z. 
# O escore z da média é zero. Por exemplo, se a média de uma distribuicao for 7 e o desvio padrão for 2, 
# um valor de 4 terá um escore z de -1,5.

valor_media = 7
valor_desvio = 2
x = 3:11
z = (x - valor_media) / valor_desvio
data.frame(x, z)

# Podemos generalizar o exemplo anterior do dado para o caso de amostras de tamanhos variados retiradas 
# de uma distribuicao contínua que varia de 0-1. Esta simulacao mostra a distribuicao de amostras 
# dos tamanhos 1, 2, 4, ... 32 retiradas de uma distribuicao uniforme. Observe que, para cada amostra, 
# estamos descobrindo o valor médio da amostra, e não a soma que estávamos fazendo no caso dos dados.

# Parâmetros para o experimento
num_amostras <- 10000             
min <- 0                       
max <- 1
n_vezes <- 6

# Ajustando a área de plotagem
par(mar = c(1,1,1,1))
op <- par(mfrow = c(n_vezes, 1))       
i2 <- 1                

# Aumentando o tamanho de cada amostra e criando um histograma
# Comprovando um dos fundamentos do Teorema Central do Limite
for (i in 1:n_vezes)               
  {  sample = rep(0, num_amostras)      
     k = 0             
  for (j in 1:i2)        
  {
    sample <- sample + runif(num_amostras, min, max) 
    k <- k+1  }
  x <- sample/k
  saida <- c(k, mean(x), sd(x))

  hist(x, xlim = range(0,1), prob = T, main = paste( "Amostras de tamanho",  k ), col = "blue")
  i2 <- 2*i2
}    

# Outro exemplo do Teorema Central do Limite:

# Reset da área de plotagem (se necessário, feche o RStudio e abra novamente)
par(mfrow = c(1,1))

# Dataset de idades de segurados
idades <- read.csv("dados/idades.csv")
head(idades)
tail(idades)

# Histograma
hist(idades$idade, right = FALSE)

# Vamos melhorar este histograma
hist(idades$idade, 
     right = FALSE, 
     breaks = seq(0,102,2), 
     col = "blue", 
     las = 1, 
     xlab = "Idade do Segurado (anos)", 
     ylab = "Frequencia", 
     main = "")

# Vamos coletar várias amostras de dados
n <- 4
resultados <- vector()

for(i in 1:10000) {
  valores_idades <- sample(idades$idade, size = n, replace = FALSE)
  resultados[i] <- mean(valores_idades)
}

# Histograma do resultado
hist(resultados, 
     right = FALSE, 
     breaks = 50, 
     col = "firebrick", 
     las = 1, 
     xlab = "Média de Idade do Segurado", 
     ylab = "Frequency", 
     main = "")

# A influencia do desvio padrão 

# Massa de dados
x = seq(-8, 8, length = 500)

# Desvio padrão igual a 1
y1 = dnorm(x, mean = 0, sd = 1)
plot(x, y1, type="l", lwd=2, col="red")

# Desvio padrão igual a 2
y2 = dnorm(x, mean = 0, sd = 2)
lines(x, y2, type="l", lwd=2, col="blue")


# Vamos calcular probabilidades para variável aleatória.

# Suponha que as pontuações dos exames de vestibular se enquadrem em uma distribuicao normal. 
# Além disso, a nota média do teste é 72 e o desvio padrão é 9.3. 
# O exame vai de 0 a 100 pontos possíveis.

# Qual é a probabilidade de alunos conseguirem exatamente 85 pontos no exame?
# Qual é a probabilidade de alunos conseguirem menos de 70 pontos no exame?
# Qual é a probabilidade de alunos conseguirem mais de 90 pontos no exame?

# Reset da área de plotagem (se necessário, feche o RStudio e abra novamente)
par(mfrow = c(1,1))

# Gerando a massa de dados com notas aleatórias de 100 alunos

# Média e desvio
media <- 72
desvio <- 9.3

# Sequencia de valores para a massa de dados
?rnorm
notas <- rnorm(100, mean = media, sd = desvio)
min(notas)
max(notas)

# Plot
hist(notas, main = "Notas Para o Exame Vestibular", xlab = "Notas", col = "blue", breaks = 10)
h <- hist(notas, main = "Notas Para o Exame Vestibular", xlab = "Notas", col = "blue", breaks = 10)
text(h$mids, h$counts, labels = h$counts, adj = c(0.5, -0.5))

# Curva PDF
probabilidades_notas = dnorm(notas, mean = media, sd = desvio) 
plot(x = notas, y = probabilidades_notas)

# Curva CDF
probabilidades_notas_cumul = pnorm(notas, mean = media, sd = desvio) 
plot(x = notas, y = probabilidades_notas_cumul)


# Qual é a probabilidade de alunos conseguirem exatamente 85 pontos no exame?
?dnorm
dnorm(85, mean = media, sd = desvio) 
dnorm(85, mean = media, sd = desvio) * 100


# Qual é a probabilidade de alunos conseguirem menos de 70 pontos no exame?
# Aplicamos a funcao pnorm da distribuicao normal com média 72 e desvio padrão 9.3. Uma vez que 
# estamos procurando o percentual de alunos com pontuacao inferior a 70, estamos interessados na cauda 
# inferior da distribuicao normal.
?pnorm
pnorm(70, mean = media, sd = desvio, lower.tail = TRUE) 
pnorm(70, mean = media, sd = desvio, lower.tail = TRUE) * 100


# Qual é a probabilidade de alunos conseguirem mais de 90 pontos no exame?
# Aplicamos a funcao pnorm da distribuicao normal com média 72 e desvio padrão 9.3. Uma vez que 
# estamos procurando o percentual de alunos com pontuacao superior a 90, estamos interessados na cauda 
# superior da distribuicao normal.
?pnorm
pnorm(90, mean = media, sd = desvio, lower.tail = FALSE) 
pnorm(90, mean = media, sd = desvio, lower.tail = FALSE) * 100


# As pontuações de QI das crianças são normalmente distribuídas com um média de 100 e desvio padrão de 15.  
# Que proporcao de crianças deverá ter um QI entre 80 e 120? Crie um plot para demonstrar seu resultado.

# Reset da área de plotagem
par(mfrow = c(1,1))

# Variáveis
media = 100; desvio = 15
limite_inferior = 80; limite_superior = 120

# Cria uma distribuicao de dados
x <- seq(-4, 4, length = 100) * desvio + media

# Calcula a PDF
hx <- dnorm(x, media, desvio)

# Plot
plot(x, hx, type = "n", xlab = "Valores de QI", ylab = "", main = "", axes = FALSE)

# Define os valores entre os limites
i <- x >= limite_inferior & x <= limite_superior

# Adiciona uma linha com valores de x e as probabilidades
lines(x, hx)

# Cria o polygon
?polygon
polygon(c(limite_inferior, x[i], limite_superior), c(0, hx[i] ,0), col = "red")

# Calcula as probabilidades acumuladas entre os limites
area <- pnorm(limite_superior, media, desvio) - pnorm(limite_inferior, media, desvio)
area

# Prepara o título para o gráfico
resultado <- paste("P(", limite_inferior, "< QI <", limite_superior,") =", signif(area, digits = 3))
mtext(resultado, 3)
axis(1, at = seq(40, 160, 20), pos=0)


# Teste de Normalidade

# Gerando 2 datasets
# O primeiro segue uma distribuicao normal e o segundo segue uma distribuicao uniforme
df1 = rnorm(100)
df2 = runif(100)

hist(df1)
hist(df2)

# Plot das densidades
plot(density(df1))
plot(density(df2))

# Shapiro Test
# Hipótese Nula (H0): Os dados são normalmente distribuídos. 
# Se o valor-p for maior que 0.05 não rejeitamos a hipótese nula e podemos assumir a normalidade dos dados.
# Se o valor-p for menor que 0.05 rejeitamos a hipótese nula e não podemos assumir a normalidade dos dados.
?shapiro.test
shapiro.test(df1)
shapiro.test(df2)

# Teste Visual Usando Normal Q-Q Plot
?qqnorm
?qqline
qqnorm(df1);qqline(df1, col = 2)
qqnorm(df2);qqline(df2, col = 2)

# Testando a variável que criamos anteriormente para o QI
x <- rnorm(100, mean = 100, sd = 15)
qqnorm(x);qqline(x,  col = 2)
shapiro.test(x)


######----------------- Distribuicao Exponencial -----------------------######

# A distribuicao exponencial descreve o tempo de chegada de uma sequencia de eventos independentes, 
# aleatoriamente recorrentes.

# Suponha que o tempo médio de checkout de um caixa de supermercado seja de 3 minutos. 
# Encontre a probabilidade de uma compra de cliente ser concluída pelo caixa em menos de 2 minutos.

# A taxa de processamento de saída é igual a 1 dividido pelo tempo médio de conclusão do checkout. 
# Daí a taxa de processamento é 1/3 checkouts por minuto. 
# Aplicamos então a funcao pexp da distribuicao exponencial com taxa = 1/3.

# A probabilidade de terminar um checkout em menos de dois minutos pelo caixa é de 48,7%
?pexp
pexp(2, rate = 1/3) 









