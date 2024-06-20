#Instalação e carregamento dos pacotes necessários
install.packages("plotly")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("pacman")

library(plotly)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(pacman)

#Importando o dataset manualmente 
Fraud = read.csv2("Fraud.csv") #Não rodar

#Visualizando apenas o tamanho do conjunto de dados: 
dim(Fraud)

#Visualizando as 6 primeiras linhas do objeto "Fraud" criado: 
head(Fraud)

str(Fraud)

glimpse(Fraud)

#Obtendo informações resumidas dos dados
names(Fraud)

#Entendendo um pouco mais sobre o dataset
summary(Fraud)

#Sumário das variáveis presentes no objeto dados. (Tendo uma alternativa mais informativa, porém ainda mais rápida) Pesquisar sobre o que é essa informação
sum(is.na(Fraud))

#Visualizando a quantidade de resposta em 1 e 0:
table(Fraud$isFraud)

table(Fraud$isFlaggedFraud)

table(Fraud$type)

#Verificar se aparece naus de yna vez o nome original da transferência e do destinatário
table(Fraud$nameOrig)
#Pudemos perceber que o nome orginal de quem realizou a tranasferência, não foi repetido, então a partir de agora vamos verificar se repete em quem recebeu a transferência 
table(Fraud$nameDest)

#Gerando os identificadores que mais repetem na coluna do NameDest e NameOrig
top_5_valoresD <- Fraud %>% 
  group_by(nameDest) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  head(5) %>% 
  pull(nameDest)

top_5_valoresO <- Fraud %>% 
  group_by(nameOrig) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  head(5) %>% 
  pull(nameOrig)

print(top_5_valoresD)
print(top_5_valoresO)

#Calculando a contagem de cada valor 
contagemD <- table(top_5_valoresD)
contagemO <- table(top_5_valoresO)

#Criando um gráfico de barras
graficoD <- barplot(contagemD, main = "Top 5 valores mais repetidos", xlab = "Valores", ylab = "Contagem") +
text(grafico, contagem, labels = contagem, pos = 3, cex = 0.8)

#Verificar o motivo de näo estar gerando o gráfico
graficoO <- barplot(contagemO, main = "Top 5 valores mais repetidos", xlab = "Valores", ylab = "Contagem") +
text(grafico, contagem, labels = contagem, pos = 3, cex = 0.8)

#Desses usuários que tiveram a maior transaçao, qual foi a maior? 
relação_usuario_transacao <- Fraud %>% 
  group_by(nameOrig, nameDest, type) %>% 
  summarise(contagem = n())

#A contagem que contém em cada tabela de relação, significa a quantidade de vezes que aquele destinitário executou uma transação, não importando o seu tipo
relação_usuario_transacao_tipo <- relação_usuario_transacao %>% 
  group_by(nameOrig, type) %>% 
  summarise(contagem = n())

relação_usuario_transacao_tipo2 <- relação_usuario_transacao %>% 
  group_by(nameDest, type) %>% 
  summarise(contagem = n())

#Antes de começarmos, podemos verificar pelo menos quantas transaçoes foram realizadas, sendo assim: 
table(relação_usuario_transacao_tipo$contagem)
table(relação_usuario_transacao_tipo2$contagem)

#Agrupar os dados por identificador e contar as transações de cada identificador
transações_por_identificador <- relação_usuario_transacao_tipo %>% 
  group_by(nameOrig) %>% 
  summarise(total_transacoes=sum(contagem))

transações_por_identificador2 <- relação_usuario_transacao_tipo2 %>% 
    group_by(nameDest) %>% 
    summarise(total_transacoes=sum(contagem))

#Encontrar o identificador com maior número de transações 
identificador_mais_transações <- transações_por_identificador$nameOrig[which.max(transações_por_identificador$total_transacoes)]

identificador_mais_transações2 <- transações_por_identificador2$nameDest[which.max(transações_por_identificador2$total_transacoes)]

#código sobre o identificador com maior quantidade (45) ficou pronto (Verificar melhor esse código depois)
mais_transacoes <- max(transações_por_identificador$total_transacoes)

transações_por_identificador <- table(total_transacoes$nameOrig)

#Agora, é preciso compreender quais ids fizeram mais transações entre si: 
relação_id <- relação_usuario_transacao %>% 
  group_by(nameDest, nameOrig) %>% 
  summarise(total_transacoes=sum(contagem))

#Utilizar groupby com mutate para poder identificar relações com nameOrig com step e nameDest com step:
exemplo_mutate <- Fraud %>% 
  group_by(nameOrig) %>% 
  mutate(quant_Orig = n(), acumulado = row_number())

exemplo_mutate2 <- Fraud %>% 
  group_by(nameDest) %>% 
  mutate(quant_Dest = n())

#Transações caindo na conta e saindo; montar uma condição no qual entenda que eu quero apenas as transações que o saldo entrou na conta e já saiu 
cont_0_Orig <- exemplo_mutate %>% 
  group_by(oldbalanceOrg, newbalanceOrig) %>% 
  summarise(count = n())

acumulado <- exemplo_mutate %>% 
  group_by(nameDest, step) %>% 
  mutate(quant_Orig = n(), acumulado = row_number())

#REGRESSÃO LOGÍSTICA BINOMIAL
table(relação_usuario_transacao_tipo$contagem)
table(relação_usuario_transacao$contagem)

#Foi notaado que as informações em maiores quantidades nas duas colunas, "nameOrig" e "nameDest" são diferentes
#Colocando informações de quantidade em cada coluna 
#Compreender por que a informação não gera com comando pipe e sem nenhuma requisição 

#Construção do modelo 
mod <- glm(isFraud ~ type + amount + oldbalanceOrg + newbalanceOrig + oldbalanceDest + newbalanceDest, 
            family = binomial(link = 'logit'), data = Fraud)

#Ausência de outliers/pontos de alavancagem 
summary(stdres(mod))
summary(mod)

#Build the model = Logist Regression
install.packages("caTools")
library(caTools)

#Atribuir um nome ao conjunto de dados (80% para treino e 20% para teste)
sample <- sample.split(Fraud$isFraud, SplitRatio = 0.8)

#training data 
train <- subset(Fraud, sample == TRUE)
test <- subset(Fraud, sample == FALSE)

#Applying - Model 1
mod1 <- glm(isFraud ~ type + amount + oldbalanceOrg + newbalanceOrig + oldbalanceDest + newbalanceDest, 
            family = binomial(link = 'logit'), data = train)
summary(mod1)

#Desenvolved Prediction for treinament base 
predicttrain = predict(mod1, type = "response")
summary(predicttrain)
tapply(predicttrain, train$isFraud, mean)

#Confusion Matrix for threshlod of 0.2 
table(train$isFraud,predicttrain>0.2)

#Confusion Matrix for threshlod of 0.5
table(train$isFraud,predicttrain>0.5)

#The limit the base train is 0.5 
#Results: 

#Precision: 0.999402 
#Recall: 0.999765 
#F1 Score: 0.999583 
#Accuracy: 0.999171

#Now, run the base test in Confunsion Matrix 
mod2 <- glm(isFraud ~ type + amount + oldbalanceOrg + newbalanceOrig + oldbalanceDest + newbalanceDest, 
            family = binomial(link = 'logit'), data = test)
summary(mod2)

#Desenvolved Prediction for test base 
predicttest = predict(mod2,type= "response")
summary(predicttest)
tapply(predicttest, test$isFraud, mean)

#Confusion Matrix for threshlod of 0.2 
table(test$isFraud,predicttest>0.2)

#Confusion Matrix for threshlod of 0.5
table(test$isFraud,predicttest>0.5)

#Confusion Matrix for threshlod of 0.7
table(test$isFraud,predicttest>0.7)

#Verificando probabilidade 
prop.table(table(train$Survived))

#Results: 

#Precision: 0.999327
#Recall: 0.9997159
#F1 Score: 0.9995218
#Accuracy: 0.9990444