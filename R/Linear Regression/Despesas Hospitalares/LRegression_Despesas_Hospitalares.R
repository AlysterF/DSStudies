####---------------------------------------------------####
### --- --- --- --- --- --- --- --- --- --- --- --- --- ###
# Regressão Linear para previsão de despesas hospitalares #
### --- --- --- --- --- --- --- --- --- --- --- --- --- ###

# Processamento básico
######

## Ativação de pacotes
library(ggplot2)
library(tidyverse)
library(GGally)

## Leitura do arquivo
dados <- read.csv('https://raw.githubusercontent.com/AlysterF/LRegression_Despesas_Hospitalares/main/database/despesas.csv')


# Analise Exploratoria
#####

#exibição das primeiras 6 linhas
head(dados)

#renomear coluna bmi para imc
dados <- rename(imc = bmi,.data = dados)

#exibição do formato dos dados
str(dados)

#checagem de valores únicos nas colunas de caracteres
unique(dados$sexo)
unique(dados$fumante)
unique(dados$regiao)

#transformação de dados char em factor
dados$sexo <- as.factor(dados$sexo)
dados$fumante <- as.factor(dados$fumante)
dados$regiao <- as.factor(dados$regiao)

#exibicao do formato dos dados para avaliar os fatores
str(dados)

#checagem de valores nulos e missing data
anyNA(dados)

#exibicao do resumo estatístico das variaveis numéricas
summary(dados)

#histograma dos dados

hist(dados$gastos)
hist(dados$imc)
hist(dados$idade)
hist(dados$filhos)

#boxplots de idade e imc

par(mfrow = c(1,2))

boxplot(dados$idade, xlab='idade')
boxplot(dados$imc, xlab='imc')

par(mfrow = c(1,1))

#contagem de outliers do imc

imc_bplt <- boxplot(dados$imc) 
imc_out <- imc_bplt$out
length(imc_out)
length(imc_out)/nrow(dados)

#menos de 1% dos dados estão como outliers
#o valor maximo de imc também está pouco distante do terceiro quartil do limite superior.

#Boxplot dos gastos

boxplot(dados$gastos)
## Analise de correlações

### Gastos em função da idade

plot(dados$idade, dados$gastos)
abline(lm(dados$gastos ~ dados$idade))

### Mapa de pares segmentado por sexo

dadosquant_sexo=subset(dados, select = c(idade, imc, filhos, gastos, sexo))

ggpairs(data = dadosquant_sexo, ggplot2::aes(colour = sexo))

#É possível ver que as maiores correlações em função dos gastos são originadas nos fatores de idade e imc.
#Os gastos segmentados por sexo são bem equilibrados, sem grandes discrepâncias.


#-------------------------------------------#
### Mapa de pares segmentado por sim ou não da variável fumante

dadosquant_fum=subset(dados, select = c(idade, imc, filhos, gastos, fumante))

ggpairs(data = dadosquant_fum, ggplot2::aes(colour = fumante))

#Já nesse segundo plot, há um aumento de gastos consideravel quando fumante.


#-------------------------------------------#
### Mapa de pares segmentado por regiao

dadosquant_reg=subset(dados, select = c(idade, imc, filhos, gastos, regiao))

ggpairs(data = dadosquant_reg, ggplot2::aes(colour = regiao))

#Os dados parecem bem equilibrados por regiao tambem.


# Machine Learning
#####

## Separação dos dados de treino e teste
set.seed(2021)

dt <- sort(sample(nrow(dados), nrow(dados)*0.7))

treino <- dados[dt,]
teste <- dados[-dt,]

summary(treino$gastos)
summary(teste$gastos)

## Treino do modelo

modelo1 <- lm(gastos ~ ., data = treino)

summary(modelo1)


### Criacao das variaveis com os valores previstos e residuos

treino1 <- treino

treino1$gasto_pred <- predict(modelo1) 


### Erro quadratico medio na amostra de treino
mse <- mean((treino1$gastos - treino1$gasto_pred)^2)
print(paste('RMSE=', sqrt(mse)))


### grafico de resultados do modelo

par(mfrow = c(2,2))
plot(modelo1)

### Teste de Normalidade de Shapiro Wilk

# se Valor P do teste for pequeno (considerei 5%), rejeita-se a hipotese de normalidade dos residuos e,
# por consequencia, conclui-se que os erros nao sao normalmente distribuidos.
# os resultados parecem apontar para 

# Teste de hipoteses
# H0: Distribuicao Normal (p > 0.05)
# H1: Distribuicao diferente da Normal (p <= 0.05)

shapiro.test(residuals(modelo1))


#O valor do teste de shapiro diz que os erros aleatorios nao sao normalmente distribuidos.
#O modelo1 e acordo com o multiple r-squared explica cerca de 75% dos dados

####################-
## Otimizacao do modelo

#Para tentar otimizar o modelo, irei padronizar os dados de idade, imc e filhos

dados_novos <- dados

dados_novos$idade <- scale(dados$idade)
dados_novos$imc <- scale(dados$imc)
dados_novos$filhos <- scale(dados$filhos)

str(dados_novos)

summary(dados_novos)

## Separacao de dados de Treino e Teste

set.seed(500)
dt2 <- sort(sample(nrow(dados_novos), nrow(dados_novos)*0.7))

treino2 <- dados_novos[dt2,]
teste2 <- dados_novos[-dt2,]

summary(treino2$gastos)
summary(teste2$gastos)


## Novo Modelo de Regressao

modelo2 <- lm(gastos ~ ., data = treino2)
summary(modelo2)

treino_com_pred <- treino2

treino_com_pred$gasto_pred <- predict(modelo2) 


### Erro quadratico medio na amostra de treino
mse <- mean((treino_com_pred$gastos - treino_com_pred$gasto_pred)^2)
print(paste('RMSE=', sqrt(mse)))


### Grafico de resultados do modelo

par(mfrow = c(2,2))
plot(modelo2)

par(mfrow = c(1,1))



#O modelo obteve um aumento de 1% apenas no Multiple R-Squared,
#mas os residuos padronizados não obtiveram uma distribuição normal.
#O QQPlot parece indicar uma distribução assimétrica a direita, o que faz sentido,
#visto que os dados de gastos são distribuídos dessa forma.

####---
#------Amostra de teste----------------------------

# Amostra de validacao

valor_pred <- predict(modelo2,interval = "prediction", level = 0.95,
                      newdata = teste2, se.fit = T) 

valor_pred1 <-valor_pred$fit
ValidSet_pred=cbind(teste2,valor_pred1)

# Residuo na amostra de validacao
residuo_valid <- ValidSet_pred$gastos - ValidSet_pred$fit
hist(residuo_valid)
qqnorm(residuo_valid, ylab="Residuos",xlab="Quantis Teoricos",main="")
qqline(residuo_valid)


# Erro quadratico medio na amostra de validacao
mse2 <- mean((ValidSet_pred$gastos - ValidSet_pred$fit)^2)
print(paste("RMSE= ", round(sqrt(mse2),2)))


# Erro quadratico medio comparado com o do treino

sqrt(mse) - sqrt(mse2)

#O valor do teste está bem próximo do valor de treino,
#além de apresentar uma distribuição de dados extremamente similar.