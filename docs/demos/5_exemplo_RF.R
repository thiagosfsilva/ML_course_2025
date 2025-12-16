# Exemplo de ML usando o Random Forest
# Thiago S. F. Silva
# 15/12/2025

# Carrega pacotes necessários
library(ranger) # Necessario para ajustar um modelo RF
library(ggplot2) # Necessario para graficos
library(GGally) # Para fazer um pairplot

# Carregando o dataset Breast Cancer
data('BreastCancer', package='mlbench')

# Investigando o dataset
head(BreastCancer)
str(BreastCancer)

# Exploracao gráfica
ggplot(BreastCancer,aes(x = Cl.thickness,fill = Class)) + geom_bar()
ggplot(BreastCancer,aes(x = Cell.size,fill = Class)) + geom_bar()
ggplot(BreastCancer,aes(x = Cell.shape,fill = Class)) + geom_bar()
ggplot(BreastCancer,aes(x = Marg.adhesion,fill = Class)) + geom_bar()
# Cansativo plotar par a par

# Vamos usar um pair plot. Como temos 10 varáveis, separamos em dois plots:
# Plotando as feicoes 1-5
ggpairs(BreastCancer, columns = c(2:5,11))
# Plotando as feicoes 6-10
ggpairs(BreastCancer, columns = c(6:10,11))

# Sumario os dados
summary(BreastCancer)

# Passo 2 - Separando treino e teste
set.seed(46)

# Criando um vetor de numeros de 1-699 (numero de linhas da tabela)
ind <- c(1:699)

# Selecionar aleatoriamente 70% das linhas
699 * 0.7 # Quantas linhas são 70%?
ind_treino <- sample(ind,490)

# Podemos deicar o codigo acima mais generico evitando
# o uso de 'números mágicos'(numeros digitados diretamente)
set.seed(46)
ind <- c(1:nrow(BreastCancer))
# ceiling arredonda pra cima o resultado de nrow * 0.7
ind_treino <- sample(ind,ceiling(nrow(BreastCancer)*0.7))

# Separando entre treino e teste
treino_df <- BreastCancer[ind_treino,2:11]
teste_df <- BreastCancer[-ind_treino,2:11]

# Ajustando o modelo Random Forest
modelo1 <- ranger(Class ~ ., data=treino_df, seed=46)

# Qual é o nosso modelo?
modelo1

# Exemplo 1 - Otimizando o numero de arvores
tune_df <- data.frame(
    num_arvores = c(10,50,100,200,500,1000),
    erro = NA)

# Qual a 'cara' dessa tabela?
tune_df

for(n in c(1:5)){ # Para valores de n de 1 a 5, repita o codigo abaixo
    modelo_temp <- ranger(Class ~ ., data=treino_df,
                      num.trees = tune_df[n,1], # ajusta o modelo com cada n. arvores
                      , seed=46) # Fixa o sorteio aleatorio
    tune_df[n,2] <- modelo_temp$prediction.error # salva o erro do modelo na tabela
}

# Como está tabela agora que foi preenchida?
tune_df

# Qual o erro mínimo?
min_err <- min(tune_df$erro)
min_err

# Quais linhas atingiram erro mínimo?
min_lines <- which(tune_df$erro == min_err)
min_lines

# Qual valor de num_trees minimiza o erro?
tune_df[min_lines,]

## Otimizando dois hiperparametros ao mesmo tempo: num_trees e mtry
tune_df <- data.frame(
    numtree_vals = c(10,50,100,200,500,1000), # lista de valores de num_tree
    mtry_vals = rep(c(2,3,4,5,6),1, each=6), # Repete cada um dos tres valores seis vezes
    error = NA # Erros vazios pra serem preenchidos depois
)

# Agora temos 5 valores de mtry * 6 valores de numtree = 30 combinações pra testar
tune_df

# Idem acima, mas agora definindo mtry e num.trees a cada rodada
for(n in c(1:nrow(tune_df))){
    temp_mod <- ranger(Class ~ ., data = treino_df,
                       num.trees = tune_df[n,1],
                       mtry = tune_df[n,2],
                       seed = 46)
    tune_df[n,3] <- temp_mod$prediction.error
}

# Como ficaram os erros agora?
tune_df

# Qual o erro mínimo?
min_err <- min(tune_df$erro)
min_err

# Quais linhas atingiram erro mínimo?
min_lines <- which(tune_df$erro == min_err)
min_lines

# Qual valor de num_trees minimiza o erro?
tune_df[min_lines,]
# Temos três combinações de hiperparams com resultados iguais
# Tanto faz usar qualquer uma

## Ajustando o modelo final
m_final <- ranger(Class ~ ., data=treino_df,
                  num.trees = 100,
                  mtry = 3,
                  seed = 46)

m_final

# Erro final calculado com dados de teste:
m_final$prediction.error

# Part 4 - Teste do Modelo

# Gerando uma predicão das classes a partir dos dados de TESTE
pred <- predict(m_final,teste_df)
pred$predictions # A predicão mesmo fica dentro dessa coluna

# Criando uma matriz de confusão entre o observado
# (as classes da tabela de teste que já sabíamos) e
# o predito (as classes preditas atraves das features de teste)
conf_mat <- table(teste_df$Class,pred$predictions)

# Como ficou a tabela?
conf_mat

# Qual a % de acerto do modelo (acurácia global)?
acuracia <- sum(diag(conf_mat)) / sum(conf_mat)
acuracia

# Qual a % de erro?
erro <- 1 - acuracia
erro
# Ótimo resultado! Nosso erro de teste é ainda menor que o erro de treino,
# então não estamos tendo overfitting!