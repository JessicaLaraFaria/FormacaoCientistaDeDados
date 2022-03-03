setwd("C:/Users/Dell/Documents/FCD/BusinessAnalytics/Cap03/meus")
getwd()

# O objetivo deste projeto é analisar o conjunto de dados e extrair 
# informações valiosas com foco principal em estudar o efeito de um novo 
# revestimento em uma bola de futebol na distância total percorrida.


###-----------------------Instalação dos Pacotes------------------------###

install.packages("readxl")
install.packages("e1071")
library(readxl)
library(e1071)


###-----------------------Carregando os Dados------------------------###

dados <- read_excel("dados/Bola_Futebol.xlsx")
View(dados)
dim(dados)
str(dados)


###-----------------------Análise Exploratória------------------------###

# Verificando valores missing
colSums(is.na(dados))

# Estatísticas
summary(dados)

# Criando um vetor com os nomes das estatísticas
nomes_stats <- c("Média", "Desvio Padrão", "Variância", "Tipo de Bola")

# Calculando as estatísticas para a Bola com Revestimento Atual
dados_stats_atual <- c(round(mean(dados$Atual), digits = 2),
                       round(sd(dados$Atual), digits = 2),
                       round(var(dados$Atual), digits = 2),
                       "Bola com Revestimento Atual")

# Calculando as estatísticas para a Bola com Revestimento Novo
dados_stats_novo <- c(round(mean(dados$Novo), digits = 2),
                      round(sd(dados$Novo), digits = 2),
                      round(var(dados$Novo), digits = 2),
                      "Bola com Revestimento Novo")

# Combinar os resultados para comparações
dados_stats_combined <- rbind(nomes_stats, dados_stats_atual, dados_stats_novo)
View(dados_stats_combined)

# O modelo de revestimento atual cobrem, em média, uma distância maior que as bolas do modelo novo.
# Os desvios padrão são aproximados.


# Análise Univariada

# Range
range_atual <- max(dados$Atual) - min(dados$Atual)
range_atual

range_novo <- max(dados$Novo) - min(dados$Novo)
range_novo

# Interquartil
IQR_atual <- IQR(dados$Atual)
IQR_atual

IQR_novo <- IQR(dados$Novo)
IQR_novo

# O intervalo interquartil de ambas as variáveis é equivalente aproximadamente a 12, ou seja, não
# há diferença entre os valores do primeiro ao terceiro quartil.

# Portanto, podemos dizer que não há muita diferença (significativa) nos dois modelos de revestimento da bola.


# Representação Gráfica

# Ajusta a área de plotagem
par(mfrow = c(2,2))

# Histograma
hist(dados$Atual,
     main = "Distância - Bola Com Revestimento Atual",
     xlab = "Distância Metros",
     ylab = "Número de Bolas",
     col = "Green")

hist(dados$Novo,
     main = "Distância - Bola Com Revestimento Novo",
     xlab = "Distância Metros",
     ylab = "Número de Bolas",
     col = "Blue")

# Boxplot
boxplot(dados$Atual, 
        main = "Distância - Bola Com Revestimento Atual", 
        xlab = "Distância (metros)", 
        ylab = "Número de Bolas", 
        col = "Blue", 
        horizontal = TRUE)

boxplot(dados$Novo, 
        main = "Distância - Bola Com Revestimento Novo", 
        xlab = "Distância (metros)", 
        ylab = "Número de Bolas", 
        col = "Green", 
        horizontal = TRUE)

# Assimetria
skewness(dados$Atual)
summary(dados$Atual)

skewness(dados$Novo)
summary(dados$Novo)

# A assimetria é entre -0,5 e 0,5, então a distribuição é aproximadamente simétrica.
# A medida positiva indicaria que a média dos valores dos dados é maior do que a mediana e 
# a distribuição dos dados é inclinada para a direita.

# Curtose
kurtosis(dados$Atual)
kurtosis(dados$Novo)


# Teste de Normalidade - Shapiro Test
# Hipótese Nula (H0): Os dados são normalmente distribuídos. 
# Hipótese Alternativa (H1): Os dados não são normalmente distribuídos.
shapiro.test(dados$Atual)
shapiro.test(dados$Novo)

# O valor-p > 0.05 não rejeitamos a hipótese nula e podemos assumir a normalidade dos dados
#para ambos os testes


###-----------------------Conclusão da Análise Univariada------------------------###
# Não parece haver diferença significativa em relação ao revestimento da bola


# Análise Bivariada

# Ajusta a área de plotagem
par(mfrow = c(1,1))

# Scatter Plot
plot(dados$Atual, dados$Novo)

# Correlação
cor(dados$Atual, dados$Novo)

# A correlação indica total independência das variáveis


# Teste T
# teste t de amostras independentes 
teste_hipo <- t.test(dados$Atual, dados$Novo, paired = F, conf.level = 0.95, alternative = "t") 
teste_hipo

# valor-p > 0.05, ou seja, há uma probabilidade alta de não haver diferença significativa entre 
# os tipos de revestimento das bolas de futebol.


# Determinando a Força do Teste e o Tamanho o Ideal de Amostra

# Diferença das médias
delta_mean <- mean(dados$Atual) - mean(dados$Novo)
delta_mean

# Desvio padrão da diferença entre os dados
delta_desvio <- sd(dados$Atual - dados$Novo)
delta_desvio

# Size Effect
size_effect = delta_mean/delta_desvio
size_effect

# Power Test - Força do Teste
install.packages("pwr")
library(pwr)
dim(dados)
power_teste <- pwr.t.test(n = 40, d = size_effect, sig.level = 0.05, alternative = "t")
power_teste

# Tamanho ideal da amostra
tamanho_amostra <- pwr.t.test(power = .95, d = 0.5, type = "t", alternative = "t", sig.level = .05)
tamanho_amostra


###-----------------------Conclusão da Força do Teste T------------------------###
# A força do teste é 0,144, o que é baixo.
# Tamanho da amostra: O teste deve ser realizado com um tamanho de amostra maior, pelo menos 105 registros.
















