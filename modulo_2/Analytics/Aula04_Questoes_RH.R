# Projeto RH - People Analytics

setwd("E:\\_EAD\\_DISCIPLINAS\\ANALYTICS\\FORMULARIOS\\REVISAO\\AULA04\\AULA04_MATERIAL_APOIO")
getwd()

# Imports
install.packages("caret")	
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("data.table")
install.packages("car")
install.packages("caTools")
install.packages("corrplot")
install.packages("rpart")
install.packages("rpart.plot")
#
library(caret)
library(ggplot2)
library(gridExtra)
library(data.table)
library(car)
library(caTools)
library(corrplot)
library(rpart)
library(rpart.plot)

# Carregando o dataset
dataset <-  read.csv("E:\\_EAD\\_DISCIPLINAS\\ANALYTICS\\FORMULARIOS\\REVISAO\\AULA04\\AULA04_MATERIAL_APOIO\\Aula04_Dataset.csv", stringsAsFactors = FALSE)
dim(dataset)
View(dataset)
str(dataset)
summary(dataset)

#
# Ao exibir as variaveis temos duas situaçoes
# Se for variavel caracter teremos apenas contagem de valores no summary
# Se for variavel inteiro  teremos um resumo estatistico
# 
# Algumas variaveis sao inteiros mas realmente serao categoricas e deverao ser transformadas
#
# Nem todas as variaveis serao utilizadas
#
# POr isso é preciso fazer analise do tipo univariada
#
# Fazer analise multivariada se se pretende observar mais de uma variavel
# estudando o relacionamento entre elas
#
# Serao estudadas as variaveis numericas e as categorica
# pois tem comportamentos diferentes
#
# Pre processamento dos dados # 
#
# Transformando variaveis para factor
#
View(dataset)
dataset$Attrition                <- as.factor(dataset$Attrition)
dataset$BusinessTravel           <- as.factor(dataset$BusinessTravel)
dataset$Department               <- as.factor(dataset$Department)
dataset$Education                <- as.factor(dataset$Education)
dataset$EducationField           <- as.factor(dataset$EducationField)
dataset$'Employee Source'        <- as.factor(dataset$'Employee Source')
dataset$EnvironmentSatisfaction  <- as.factor(dataset$EnvironmentSatisfaction)
dataset$Gender                   <- as.factor(dataset$Gender)
dataset$JobInvolvement           <- as.factor(dataset$JobInvolvement)
dataset$JobLevel                 <- as.factor(dataset$JobLevel)
dataset$JobRole                  <- as.factor(dataset$JobRole)
dataset$JobSatisfaction          <- as.factor(dataset$JobSatisfaction)
dataset$MaritalStatus            <- as.factor(dataset$MaritalStatus)
dataset$OverTime                 <- as.factor(dataset$OverTime)
dataset$PerformanceRating        <- as.factor(dataset$PerformanceRating)
dataset$RelationshipSatisfaction <- as.factor(dataset$RelationshipSatisfaction)
dataset$StockOptionLevel         <- as.factor(dataset$StockOptionLevel)
dataset$WorkLifeBalance          <- as.factor(dataset$WorkLifeBalance)
str(dataset)
#
# Transformando  para o tipo inteiro
#
View(dataset)
dataset$DistanceFromHome  <- as.integer(dataset$DistanceFromHome)
dataset$MonthlyIncome     <- as.integer(dataset$MonthlyIncome)
dataset$PercentSalaryHike <- as.integer(dataset$PercentSalaryHike)
#
# Limpando variaveis factor com 0 count
#
# Algumas variaveis factor podem ter niveis sem tipos de registro
#
# Algumas variaveis factor podem ter valores numericos como JobSatisfaction
#
# É importante se certificar dos tipos de variaveis apos as mudança
#
dados <- droplevels(dataset)
?droplevels
str(dataset)
summary(dataset)
View(dataset)

#
# Conclusao da etapa de transformação e limpeza
#

# Manipulacao e engenharia de atributos 
#
# Muitas informações ja estao no conjunto de dados o Cientista de Dados deve
# verificar a necessidade criar novos campos
#
# Criacao de uma coluna de anos de experiencia anterior
#
dataset$PriorYearsOfExperience <- dataset$TotalWorkingYears - dataset$YearsAtCompany
View(dataset)

# Criacao de uma coluna de anos trabalhados em media em empresas anteriores
# Qualquer operação de divisao pode resultar em valores nao aceitavei
# Validar o resultado e dependendo do valor corrigi-lo
# Nalinguagem R o valor inf (infinito) significa que nao foi possivel fazer a divisao

dataset$AverageStabily <- dataset$PriorYearsOfExperience / dataset$NumCompaniesWorked
View(dataset)

# Limpando os dados da coluna $AverageStabily
# 
summary(dataset$AverageStabily)
dataset$AverageStabily[!is.finite(dataset$AverageStabily)] <- 0
summary(dataset$AverageStabily)
View(dataset)
#
# Engenharia de atributos concluida
#
# A preparação dos dados evitara problemas no resultado final de analise de dados
#
#
# Extração de insights a partir da analise exploratorio
#
# Plots de analise univariada, observado variaveid individualmente
#
# A varaivel Gender é categorica
# Utiliza-se o geom_bar
#
ggplot(dataset) + geom_bar(aes(x = Gender))
#
# A variavel age    é numerica
# Utiliza-se o geom_density
#
ggplot(dataset) + geom_density(aes(x = Age))
#
ggplot(dataset) + geom_bar(aes(x = Attrition))
#
ggplot(dataset) + geom_bar(aes(x = Department))
#
ggplot(dataset) + geom_bar(aes(x = JobRole))
#
ggplot(dataset) + geom_bar(aes(x = Education)) + facet_grid(~EducationField)
#
# É importante interpretar os graficos, se a variavel faz sentido
# O valor dos dados podem estar incoerentes
# 

# Multiplot Grid - ccomparando diversas variaveis em um criterio especifico
#
# Criar o multiplot 0 foram criados varios graficos e foram colocado em uma variavel
#
#
p.TotalWorkingYears       <- ggplot(dataset) + geom_density(aes(TotalWorkingYears))
p.YearsAtCompany          <- ggplot(dataset) + geom_density(aes(YearsAtCompany))
p.YearsSinceLastPromotion <- ggplot(dataset) + geom_density(aes(YearsSinceLastPromotion))
p.YearsWithCurrManager    <- ggplot(dataset) + geom_density(aes(YearsWithCurrManager))
p.YearsInCurrentRole      <- ggplot(dataset) + geom_density(aes(YearsInCurrentRole))
p.PriorYearsOfExperience  <- ggplot(dataset) + geom_density(aes(PriorYearsOfExperience))

# Organiza as variaveis em um grid

grid.arrange(p.TotalWorkingYears, 
             p.YearsAtCompany, 
             p.YearsSinceLastPromotion, 
             p.YearsWithCurrManager, 
             p.YearsInCurrentRole, 
             p.PriorYearsOfExperience, 
             nrow = 2, 
             ncol = 3)
#
# Comparar diversar variaveis
# Eixo Y densidades
# Eixo x variaveis
#
# Os primeiros anos tem valor maior que os outros, o que aparentmente os dados sao coerentes
# 
# Se o comportamento for anormal afetara a modelagem preditiva negativamente
#
#
#
# Tempo de experiencia anterior
# 
# Exemplo de insight:
# 
# Funcionarios com menos de 3 anos de experiencia antes de entrar na IBM
# 
length(which(dataset$PriorYearsOfExperience < 3)) / length(dataset$PriorYearsOfExperience)   

# Exemplo de insight:
# 
# Funcionarios com menos de 3 anos de experiencia antes de entrar na IBM
# 
# Idade
length(which(dataset$Age < 30)) / length(dataset$Age)

# Exemplo de insight:
# Apenas 22% dos funcionarios tem menos de 30 anos, a base de funcionarios
#

# Educacao 
# 
# Percentual que tem mestrado $Education == 4
#
summary(dataset$Education)
length(which(dataset$Education == 3)) / length(dataset$Education)
length(which(dataset$Education == 4)) / length(dataset$Education)

# Correlacao Exemplo de Insight
# 
# Verificar e a relacao entre salario mais alto levar a uma maior satisfacao

# Correlacao
# Varia  de -1 a +1
# Proximo de zero nao ha correlação
# Proximo de -1 correlação negativa
# Proximo de +1 Correlação positiva - quando uma variavl aumenta a outra tambem aumenta
#
# Utlizada para numeros inteiros
#
# Para variaveis categoricas utiliza-se associação
#
# complete.obs informa que sera usado comparação com valores validos
# se o registro tiver o valor "na" sera descartado
#
#
cor(dataset$YearsAtCompany,    dataset$MonthlyIncome,           use = "complete.obs")  

