setwd("~/Documents/FCD/BusinessAnalytics")
getwd

# Business Analytics 

# O objetivo deste projeto é tentar prever o tempo de sobrevivência de um paciente um ano após receberem um 
# transplante de fígado. Usaremos dados reais disponibilizados publicamente pelo site
# https://www.srtr.org/about-the-data/the-srtr-database/

# Há normalmente  uma  grande  fila  de pacientes  esperando  em  um  determinado momento para receber um 
# transplante de fígado, pois não há outra cura para o estágio final de doença hepática. 
# Nosso estudo visa ajudar os pacientes a compreender melhor suas chances de sobrevivência após transplante.

# -------------------------------------------------------------------------------------------------
# Carregando pacotes
install.packages("dplyr")
install.packages("ggcorrplot")
install.packages("forecast") # para função "Accuracy"
install.packages("nnet")
install.packages("neuralnet") # para construção de modelo de rede neural

library(dplyr)
library(ggcorrplot)
library(forecast)
library(nnet)      # para criação das variáveis dummy para o algoritmo de rede neural
library(neuralnet) # algoritmo da rede neural

# -------------------------------------------------------------------------------------------------
# Carregando os dados
dados <- read.csv("dados/dataset.csv", header = TRUE, na.strings = c(""))
dim(dados)
View(dados)

# -------------------------------------------------------------------------------------------------
# Análise Exploratória, Limpeza, Transformação e Manipulação dos Dados
colnames(dados)[1] <- "DAYSWAIT_CHRON"
str(dados)

# Explorando variáveis númericas
hist(dados$AGE)
hist(dados$AGE_DON) # variáveis "DON" corresponde a dados do doador
hist(dados$PTIME) # variável que mostra o tempo que o paciente sobreviveu(em dias) após o transplante
hist(dados$DAYSWAIT_CHRON) # tempo(em dias) de espera para o transplante
hist(dados$FINAL_MELD_SCORE) # pontuação que define o nível de severidade da doença hepática
# Obs: Pontuação MELD ajuda a dizer com que rapidez você pode precisar de um transplante de fígado.
# Quanto maior o número, mais urgente.

# Explorando os dados das variáveis categóricas
dados$PSTATUS                <- as.factor(dados$PSTATUS) 
dados$TX_DATE                <- as.factor(dados$TX_DATE)
dados$PX_STAT                <- as.factor(dados$PX_STAT)
dados$PX_STAT_DATE           <- as.factor(dados$PX_STAT_DATE)
dados$ABO                    <- as.factor(dados$ABO)
dados$GENDER                 <- as.factor(dados$GENDER)
dados$DIAB                   <- as.factor(dados$DIAB)
dados$REGION                 <- as.factor(dados$REGION)
dados$PERM_STATE             <- as.factor(dados$PERM_STATE)
dados$TX_Year                <- as.factor(dados$TX_Year)
dados$PREV_TX                <- as.factor(dados$PREV_TX)
dados$GENDER_DON             <- as.factor(dados$GENDER_DON)
dados$HOME_STATE_DON         <- as.factor(dados$HOME_STATE_DON)
dados$DIABETES_DON           <- as.factor(dados$DIABETES_DON)
dados$HIST_HYPERTENS_DON     <- as.factor(dados$HIST_HYPERTENS_DON)
dados$HIST_IV_DRUG_OLD_DON   <- as.factor(dados$HIST_IV_DRUG_OLD_DON)
dados$ABO_DON                <- as.factor(dados$ABO_DON)
dados$HIST_CANCER_DON        <- as.factor(dados$HIST_CANCER_DON)
dados$ALCOHOL_HEAVY_DON      <- as.factor(dados$ALCOHOL_HEAVY_DON)
dados$MALIG                  <- as.factor(dados$MALIG)
dados$TX_MELD                <- as.factor(dados$TX_MELD)

# indica se o paciente tem ou não diabetes
table(dados$DIAB)

# indica se o paciente sobreviveu ou não após o transplante sendo 0-sobreviveu e 1-não sobreviveu
table(dados$PSTATUS)
# Obs: as classes estão desbalanceadas, porém não afetará o projeto por um problema de regressão.

# indica gênero do paciente sendo 0-masculino e 1-feminino
table(dados$GENDER)

# indica gênero do doador sendo 0-masculino e 1-feminino
table(dados$GENDER_DON)

# indica a coleta dos dados por ano
table(dados$TX_Year)

# indica se havia ou não tumor maligno sendo N-não U-não definido Y-sim
table(dados$MALIG)

# histórico de câncer do doador sendo N-não U-não definido Y-sim
table(dados$HIST_CANCER_DON)

# Filtro considerando os pacientes que sobreviveram ao primeiro ano de cirurgia
dados1 <- dados %>%
  filter(PTIME > 365) %>%
  mutate(PTIME = (PTIME - 365))

dim(dados1)

# Dos pacientes que sobreviveram ao primeiro ano da cirurgia,
# filtramos os que permaneceram vivos até 3 anos depois da cirurgia.
dados2 <- dados1 %>%
  filter(PTIME <= 1095)

dim(dados2)

# Divisâo dos dados em treino e teste
set.seed(1)
index <- sample(1: nrow(dados2), dim(dados2)[1]*.7)
dados_treino <- dados2[index,]
dados_teste <- dados2[-index,]

# Separação das variáveis númericas e categóricas treino
dados_treino_num <- dados_treino[,!unlist(lapply(dados_treino, is.factor))]
dim(dados_treino_num)
dados_treino_fator <- dados_treino[,unlist(lapply(dados_treino, is.factor))]
dim(dados_treino_fator)

# Separação das variáveis númericas e categóricas teste
dados_teste_num <- dados_teste[,!unlist(lapply(dados_teste, is.factor))]
dim(dados_teste_num)
dados_teste_fator <- dados_teste[,unlist(lapply(dados_teste, is.factor))]
dim(dados_teste_fator)

# Padronização (colocar dados na mesma escala)das variáveis numéricas e combinação 
# em um novo dataframe com variáveis categóricas

# Padronização treino
dados_treino_num_norm <- scale(dados_treino_num)
dados_treino_final <- cbind(dados_treino_num_norm, dados_treino_fator)
dim(dados_treino_final)

# Padronização teste
dados_teste_num_norm <- scale(dados_teste_num)
dados_teste_final <- cbind(dados_teste_num_norm, dados_teste_fator)
dim(dados_teste_final)


# Remoção dos registros dos anos 2001 e 2002 visto que possuem poucos registros
dados_treino_final <- dados_treino_final %>%
  filter(TX_Year != 2001) %>%
  filter(TX_Year != 2002)

dados_teste_final <- dados_teste_final %>%
  filter(TX_Year != 2001) %>%
  filter(TX_Year != 2002)

# -------------------------------------------------------------------------------------------------
# Modelagem Preditiva com Modelo de Regressão

# Seleção de algumas variáveis 
?lm
modelo_v1 <- lm(PTIME ~ FINAL_MELD_SCORE +
                  REGION +
                  LiverSize +
                  LiverSizeDon +
                  ALCOHOL_HEAVY_DON +
                  MALIG +
                  TX_Year,
                data = dados_treino_final)

summary(modelo_v1)

# Avaliação do modelo
# Com dados de treino
modelo_v1_pred_1 = predict(modelo_v1, newdata = dados_treino_final)
?accuracy
accuracy(modelo_v1_pred_1, dados_treino_final$PTIME)

# Com dados de teste
modelo_v1_pred_2 = predict(modelo_v1, newdata = dados_teste_final)
accuracy(modelo_v1_pred_2, dados_teste_final$PTIME)

# O modelo com dados de treino e do modelo com dados de teste possuem o RMSE parecidos.
# Se a diferença da métrica fosse muito grande poderia indicar overfitting ou algum
# outro problema.

# Histograma dos resíduos do modelo
# É esperado um formato de distribuição normal
# Distribuição do erro de validação
par(mfrow = c(1,1))
residuos <- dados_teste_final$PTIME - modelo_v1_pred_2
hist(residuos, xlab = "Resíduos", main = "Sobreviventes de 1 a 3 Anos")
# obs: Está próximo de uma distribuição normal, isso indica que o modelo está equilibrado embora
# O modelo possua uma taxa de acertos um pouco baixa.

# Desfazer a escala dos dados
variaveis_amostra <- c("PTIME",
                      "FINAL_MELD_SCORE",
                      "REGION",
                      "LiverSize",
                      "LiverSizeDon",
                      "ALCOHOL_HEAVY_DON",
                      "MALIG",
                      "TX_Year")

# Remoção de valores NA das variáveis que será usada para aplicar o unscale
dados_unscale <- na.omit(dados2[,variaveis_amostra])

# Retorna os dados unscale
dados_final_unscale <- dados_unscale[-index,] %>%
  filter(TX_Year != 2001) %>%
  filter(TX_Year != 2002)

# Histograma dos dados sem escala (formato original)
previsoes = predict(modelo_v1, newdata = dados_final_unscale)
hist(previsoes)
accuracy(previsoes, dados_final_unscale$PTIME)

# Modelagem Preditiva com Modelo de Rede Neural
# Preparação dos dados

# Padronização das variáveis numéricas e combinação em um novo dataframe 
# com as variáveis categóricas

# Separação das variáveis numéricas e categóricas
dados_num <- dados2[,!unlist(lapply(dados2, is.factor))]
dim(dados_num)
dados_fator <- dados2[,unlist(lapply(dados2, is.factor))]
dim(dados_fator)

# Padronização
dados_num_norm <- scale(dados_num)
dados_final <- cbind(dados_num_norm, dados_fator)
dim(dados_final)
View(dados_final)
dados_final2 <- na.omit(dados_final[,variaveis_amostra])

# Retorna somente as variáveis que não são do tipo fator
variaveis_numericas <- !unlist(lapply(dados_final2, is.factor))
View(variaveis_numericas)

# Retorna o nome das variáveis numéricas
variaveis_numericas_nomes <- names(dados_final2[,!unlist(lapply(dados_final2, is.factor))])
View(variaveis_numericas_nomes)

# Gera o dataframe final com variáveis dummy
?class.ind
df_final = cbind(dados_final2[,variaveis_numericas],
                 class.ind(dados_final2$REGION),
                 class.ind(dados_final2$ALCOHOL_HEAVY_DON),
                 class.ind(dados_final2$MALIG),
                 class.ind(dados_final2$TX_Year))

dim(df_final)
View(df_final)

# Nomes das variáveis
names(df_final) = c(variaveis_numericas_nomes,
                    paste("REGION", c(1:11),sep = ""),
                    paste("ALCOHOL_HEAVY_DON", c(1:3),sep = ""),
                    paste("MALIG", c(1:3), sep = ""),
                    paste("LISTYR", c(01:18), sep = ""))

dim(df_final)
View(df_final)

# Divisão em dados de treino e teste
index2 <- sample(1:nrow(df_final), dim(df_final)[1]*.70)
dados_treino2 <- df_final[index2,]
dados_teste2 <- df_final[-index2,]
print(dados_teste2[1,])

# Modelo
?neuralnet
modelo_v2 <- neuralnet::neuralnet(PTIME ~ ., 
                                  data = dados_treino2, 
                                  linear.output = TRUE,
                                  hidden = 2,
                                  stepmax = 1e7)

# Plot
plot(modelo_v2,
     col.entry.synapse = "red", 
     col.entry = "brown",
     col.hidden = "green", 
     col.hidden.synapse = "black",
     col.out = "yellow", 
     col.out.synapse = "purple",
     col.intercept = "green", 
     fontsize = 10,
     show.weights = TRUE ,
     rep = "best")

# Avaliação do modelo

# Com dados de treino
modelo_v2_pred_1 <- compute(modelo_v2, dados_treino2)
accuracy(unlist(modelo_v2_pred_1), dados_treino2$PTIME)

# Com dados de teste
modelo_v2_pred_2 <- compute(modelo_v2, dados_teste2)
accuracy(unlist(modelo_v2_pred_2), dados_teste2$PTI)

# Conclusão : O modelo de regressão linear apresentou uma taxa de erro menor e, portanto,
# deve ser usado como versão final. Conseguimos sim prever o tempo de sovrevivência dos
# pacientes 1 ano após receberem um transplante.











