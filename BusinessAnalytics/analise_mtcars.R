setwd("~/analise_mtcars")
getwd()

#Base de dados = "Motor Trend Car Road". Disponível no RStudio.

#Análises:
#Qual tipo de transmissão consome menos combustível(mpg), automática ou manual?
#Quão diferente é a autonomia(mpg) entre as transmissões "Automática" e "Manual"?

#mpg -milhas/galão
#cyl - número de cilindros
#disp - deslocamento
#hp - potência do automóvel
#drat - relação do eixo traseiro
#wt - peso(1000 lbs)
#qsec - 1/4 mile time
#vs - motor ( 0 = turbo, 1 = normal)
#am - transmissão ( 0 = automático, 1 = manual)
#gear - número de engrenagens
#carb - número de carburadores


#####--------------------- Pré-processamento e Transformações de Dados -----------------------------

#Carregamento dos dados:
data(mtcars)
View(mtcars)
dim(mtcars)


#####--------------------- Análise Exploratória -----------------------------

install.packages("corrplot")
install.packages("plyr")
install.packages("printr")
install.packages("GGally")
library(corrplot) #gráfico de correlação
library(plyr) #tabela de frequência
library(knitr) #formatar a saída dos dados
library(printr) #formatar a saída dos dados
library(GGally)
library(ggplot2)
library(MASS) 


#Formatação dos dados para posterior documentação
?kable
kable(head(mtcars), align = 'c')


#Tipos das variáveis e resumo estatístico
str(mtcars)
summary(mtcars)
sum(is.na(mtcars))


#Tabela de frequência
table(mtcars$cyl)
count(mtcars, 'carb')


#Gráficos
#Percentual de carros por número de cilindros
cyl_freq <- table(mtcars$cyl)
labels <- names(cyl_freq)
percent <- round(cyl_freq/sum(cyl_freq) * 100)
labels <- paste(labels, percent)
labels <- paste(labels, "%", sep = "")
length(labels)
pie(cyl_freq, labels = labels, col = rainbow(length(labels)), main = "% de Carros Por Número de Cilindros")


#Números de carros por HP
count <- table(mtcars$hp)

barplot(count,
        main = "Carros Por HP",
        xlab = "HP",
        ylab = "Número de Carros")

barplot(sort(count, decreasing = TRUE),
        main = "Carros Por Hp",
        xlab = "HP",
        ylab = "Número de Carros")

#Scatter Plot
plot(mtcars$mpg, mtcars$hp, xlab = "MPG (Autonomia)", ylab = "HP(Potência)")

#histograma
hist(mtcars$mpg,
     breaks = 10,
     xlab = "Milhas por Galão",
     main = "Histograma da Variável MPG",
     xlim = range(10:35))

#Correlação das variáveis
m_cor <- cor(mtcars)
corrplot(m_cor,method = "circle")

#Verificação de como o MPG varia de acordo com a transmissão automática e manual.
#Gráfico violino
ggplot(mtcars, aes(y = mpg,
                   x = factor(am, labels = c("automatic", "manual")),
                   fill = factor(am))) +
  geom_violin(colour = "black", size = 1)+
  xlab("Transmissão")+
  ylab("MPG")

#De acordo com o gráfico violino, o automóvel com a trasmissão manual consegue fazer mais milhas/galão.
#Vamos agora testar a hipótese de que carros com transmissão automática consomem
#mais combustível do que carros com transmissão manual. 
#Usaremos o Teste T
teste <- t.test(mpg ~ am, data = mtcars, var.equal = FALSE, paired = FALSE, conf.level = .95)
print(teste)

#O Teste T informou que as médias dos grupos são diferentes.


#Testando as suposições

#Extraindo os registros segmentados pelo tipo de transmissão
mtcars$amfactor <- factor(mtcars$am, labels = c("automatic", "manual")) 

Automatic <- mtcars[mtcars$amfactor == "automatic",]
Manual <- mtcars[mtcars$amfactor == "manual",]
kable(head(mtcars), align = 'c')

#Média e sumário
mean(Automatic$mpg)
mean(Manual$mpg)
summary(mtcars$mpg)

#Suposições para normalidade dos dados
#Teste de Normalidade - Shapiro Test
#Hipótese nula (H0): Os dados são normalmente distribuídos
#Hipótese alternativa(H1): Os dados não são normalmente distribuídos

shapiro.test(Automatic$mpg)
shapiro.test(Manual$mpg)

#Os dois testes de Normalidade resultaram em p-value > 0.05
#logo não rejeitamos a hipótese nula
#podemos assumir a normalidade dos dados nas duas amostras


#suposições para homogeneidade das variâncias nas amostras
#Teste de Homogeneidade das variâncias - Bartlett's Test(caso os dados ja assumam uma distribuição normal)
#A hipótese nula (H0) para o teste é que as variâncias são iguais para todas as amostras.
#A hipótese alternativa(H1) (a que estamos testanto) é que as variâncias  não são iguais.
bartlett.test(mpg ~ am, data = mtcars)

#como o p-value > 0.05 não rejeitamos a hipótese nula e podemos assumir que as variâncias são iguais.


#Realização o Teste T novamente, porém com alteração do parâmetro "var.equal" para "TRUE"
teste <- t.test(mpg ~ am, data = mtcars, var.equal = TRUE, paired = FALSE, conf.level = .95)
print(teste)

#H0: a verdadeira diferença das médias é igual a 0
#H1: a verdadeira diferença das médias não é igual a 0

#O p-value que mostra a probabilidade de que essa aparente diferença entre os dois
#grupos possa aparecer por acaso é muito baixo.

#Rejeitamos a H0 e portanto as médias apresentam diferenças. Conclui-se que as
#médias de consumo de combustível entre os tipos de transmissão são diferentes e não são frutos do acaso.

boxplot(Automatic$mpg, Manual$mpg)



#####--------------------- Análise de Regressão -----------------------------

#Modelo de Regressão Linear Simples

#A variável explicativa é uma variável fator, a Transmission (am).Será feito o ajustamento da variável fatorial
#como regressor e a criação de uma análise de variância como um caso especial de modelos de regressão linear.

#Criação do modelo base com a variável am com seu tipo categórico
modelo_v1_base <- lm(mpg ~ amfactor, data = mtcars)
summary(modelo_v1_base)

# A inclinação (slope) de 7.24 é a mudança na média entre transmissão manual e transmissão automática. 
# O valor-p de 0,000285 para a diferença média de MPG entre transmissão manual e automática é 
# significativo. Portanto, concluímos que, de acordo com este modelo, a transmissão manual é 
# mais eficiente em termos de combustível.


#Modelo de Regressão Linear Multipla
modelo_v2 <- lm(mpg ~ cyl+disp+hp+drat+wt+qsec+factor(vs)+factor(am)+gear+carb, data = mtcars)
summary(modelo_v2)

#Avaliação da colinearidade pelo Fator de Inflação de Variância (VIF) no modelo_v2
install.packages("carData")
library(carData)
kable(vif(modelo_v2), align = 'c')


#Utilização do critério de informação de Akaike (AIC)
step <- stepAIC(modelo_v2, direction = "both", trace = FALSE)
summary(step)

# Isso mostra que, além da transmissão, o peso do veículo (wt) e a velocidade de 
# aceleração (qsec) têm a maior relação com a explicação da variação em mpg. 
# O R^2 é de 85%, o que significa que o modelo explica 85% da variação 
# em mpg, indicando que é um modelo robusto e altamente preditivo.


#Modelo final
modelo_final <- lm(mpg ~ wt+qsec+factor(am), data = mtcars)

# Detectando colinearidade
kable (vif(modelo_final), align = 'c')

#Verificação de resíduos
modelo_final$fitted.values
qqPlot(modelo_final, main = "Plot para Análise de Resíduos")


#####--------------------- Conclusões -----------------------------

# A partir do resumo (modelo_final) podemos concluir o seguinte:
coefficients(modelo_final)
confint(modelo_final)
summary(modelo_final)

# Milhas por galão (mpg) irá aumentar em 2.93 em carros com transmissão 'Manual' 
# em comparação com carros com transmissão 'Automatic' (ajustado por wt e qsec). 
# Conclusão para a Motor Trend Magazine é: 'Transmissão manual' é melhor para mpg.

# Milhas por galão (mpg) irá diminuir em 3.9 por cada 1000 lb de aumento em peso.
# Conclusão: Milhas por galão (mpg) diminui com aumento de peso (wt) do veículo.

# Milhas por galão (mpg) irá aumentar em um fator de 1.2 se aumentarmos em 1 ponto a aceleração.
# Conclusão: Milhas por galão (mpg) aumenta com um leve aumento da velocidade de aceleração.






