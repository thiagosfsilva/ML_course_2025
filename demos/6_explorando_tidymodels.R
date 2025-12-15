## Explorando o tidymodels com um exemplo
## Thiago S. F. Silva
## 15/12/2025

# Carregando pacotes
# Para esse exemplo, voiu carregar os pacotes no 'momento' do uso,
# para ficar mais claro quem é usado onde

### Passo 0 - Obtendo os dados
library(readr) # Para usar a funçao read_csv

# read_csv permite ler o csv direto do link de download
hotels <- read_csv('https://tidymodels.org/start/case-study/hotels.csv')

### Passo 1 - Inspecionando os dados

# Inspecionando os dados
str(hotels) # nomes e tipos das colunas
summary(hotels) # tem dados faltantes?

# Plots
library(ggplot2) # para a funcao ggplot

# Plot só da variável alvo
ggplot(hotels,aes(children)) + geom_bar()

### Passo 3 - organizacao dos dados e 'engenharia de feicoes"

# Para isso usamos o pacote recipes
library(recipes) # para preparar os dados
library(dplyr) # para poder usar o pipe %>%

# Criamos uma receita com diversas transformaçoes
# O pacote recipes oferece alternativas a funcoes do dplyr, lubridate etc
# com funcoes mais especificas para machine learning

# Primeiro criamos uma lista de feriados para a função step_holiday()
holiday_list <- c("AllSouls","AshWednesday","ChristmasEve",
                  "Easter","ChristmasDay","GoodFriday",
                  "NewYearsDay","PalmSunday")

# A formula define de onde vem os dados e quem sao o alvo e as feicoes
rf_recipe <- recipe(children ~ ., data = hotels) %>%
    # converte todas as colunas chr para fct
    step_string2factor(all_nominal_predictors()) %>%
    # separa a data em dia, mes e ano
    step_date(arrival_date) %>%
    # cria uma varivel binaria feriado/nao-feriado
    step_holiday(arrival_date, holidays = holiday_list) %>%
    # remove a coluna original de datas
    step_rm(arrival_date) %>%
    # cria variaveis 'dummy' para todas os fatores
    step_dummy(all_nominal_predictors())
    # remove variaveis com variancia zero
    step_zv() %>%
    # nornaliza todos os preditores numericos
    step_normalize(all_numeric_predictors())

# Para que a receita seja aplicada, ela precisa ser 'preparada'
rf_prep <- prep(rf_recipe)

# Receita "Preparada"
rf_prep

### Passo 4 - Criando a separacao de treino e teste
library(rsample) # Para gerar splits

# Cria uma separacao dos dados em 0.75/0.25,
# estratificando por children
data_split <- initial_split(hotels,
                            prop=3/4,
                            strata = 'children')

# Separa as tabelas de fato
hotel_train <- training(data_split) # Cria o dataset de treino
hotel_test <- testing(data_split) # Cria o dataset de teste

# Para aplicar a receita preparada, tenmos que 'cozinhar' o dataset
hotel_train_prep <- bake(rf_prep, new_data = hotel_train)
hotel_test_prep <- bake(rf_prep, new_data = hotel_test)

# Resultado:
head(hotel_train_prep)

# Escolhendo o algoritmo
rf_mod <- rand_forest(mtry = tune(), trees = tune()) %>%
    set_engine('ranger') %>%
    set_mode('classification')

# Criando o grid para otimização
rf_grid <- expand.grid(mtry = c(3,5,7),
                       trees = c(500,1000,5000))


# Validacao Cruzada

# Juntando tudo em um workflow
rf_workflow <- workflow() %>%
    add_model(rf_mod) %>%
    add_recipe(rf_recipe) %>%
    tune_grid(hotel_train,
              grid = rf_grid,
              control = control_grid(save_pred = TRUE),
              metrics = metric_set(roc_auc))
