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


### Passo 2 - Criando a separacao de treino e teste
library(rsample) # Para gerar splits

# Cria uma separacao dos dados em 0.75/0.25,
# estratificando por children
data_split <- initial_split(hotels,
                            prop=3/4,
                            strata = 'children')

# Separa as tabelas de fato
hotel_train <- training(data_split) # Cria o dataset de treino
hotel_test <- testing(data_split) # Cria o dataset de teste


### Passo 3 - organizacao dos dados e 'engenharia de feicoes'

# Para isso usamos o pacote recipes
library(recipes) # para preparar os dados
library(dplyr) # para poder usar o pipe %>%

# Criamos uma receita com diversas transformaçoes
# O pacote recipes oferece alternativas a funcoes do dplyr, lubridate etc
# com funcoes mais especificas para machine learning

# Primeiro criamos uma lista de feriados para a função step_holiday()
holiday_list <- c('AllSouls','AshWednesday','ChristmasEve',
                  'Easter','ChristmasDay','GoodFriday',
                  'NewYearsDay','PalmSunday')

# A formula define de onde vem os dados e quem sao o alvo e as feicoes
rf_recipe <- recipe(children ~ ., data = hotel_train) %>%
    # converte todas as colunas chr para fct
    step_string2factor(all_nominal_predictors()) %>%
    # separa a data em dia, mes e ano
    step_date(arrival_date) %>%
    # cria uma varivel binaria feriado/nao-feriado
    step_holiday(arrival_date, holidays = holiday_list) %>%
    # remove a coluna original de datas e outras desnecessarias
    step_rm(arrival_date, hotel, country) %>%
    # cria variaveis 'dummy' para todas os fatores
    step_zv() %>%
    # nornaliza todos os preditores numericos
    step_normalize(all_numeric_predictors())

### Passo 4 - Escolhendo e preparando o modelo

# Escolhendo o algoritmo: Random Forest
library(parsnip) # Contém os algoritmos de ML

rf_mod <- rand_forest(mtry = tune(), trees = tune()) %>% # quais parametros otimizar?
    set_engine('ranger', importance = 'impurity') %>% # Qual pacote usar para implementacao do RF?
    set_mode('classification') # Classificacao ou Regressao?

# Preparando um grid de valores de hiperparametros para otimizacao
# expand.grid já cria todas as combinações pra gente
rf_grid <- expand.grid(mtry = c(3,5),
                       trees = c(50,100))

# Criando 5 folds para validação cruzada
set.seed(42)
hotel_folds <- vfold_cv(hotel_train, v = 5, strata = children)

## Passo 6 - executando a otimização!
library(tune) # para a otimizacao
library(yardstick) # para a metrica de erro
library(workflows) # para ligar todos os passos

# Primeiro criamos um 'workflow' conectando os passos anteriores
rf_workflow <-
    workflow() %>%
    add_model(rf_mod) %>%
    add_recipe(rf_recipe)

# Depois 'plugamos' a otimização no workflow para gerar o resultado

rf_tune <- rf_workflow %>%
    tune_grid(resamples = hotel_folds, # os folds da validação
              grid = rf_grid, # o grid de parametros que criamos
              control = control_grid(save_pred = TRUE, # salva os resultados da otimizacao
                                     verbose = TRUE), # Indica o andamento da otimizacao
              metrics = metric_set(roc_auc)  # define a métrica
              )

# Inspecionando o objeto resultante
rf_tune

# Quais os melhores resultados?
rf_res <-  rf_tune %>% show_best(metric = 'roc_auc')
rf_res

# Selecionando o melhor de todos
rf_best <- rf_tune %>% select_best(metric = 'roc_auc')

# Plotando a ROC
rf_auc <-
    rf_tune %>%
    collect_predictions(parameters = rf_best) %>%
    roc_curve(children, .pred_children) %>%
    mutate(model = 'Random Forest')

bind_rows(rf_auc) %>%
    ggplot(aes(x = 1 - specificity, y = sensitivity, col = model)) +
    geom_path(lwd = 1.5, alpha = 0.8) +
    geom_abline(lty = 3) +
    coord_equal() +
    scale_color_viridis_d(option = 'plasma', end = .6)

### Passo 7 - ajuste do modelo final e teste

#  Definindo o modelo final
rf_final <- rf_mod %>% finalize_model(rf_best)

# Atualizando o workflow
rf_workflow_final <- rf_workflow %>%
    update_model(rf_final)

# Gerando o ajuste final
set.seed(345)
rf_final_fit <- rf_workflow_final %>% last_fit(data_split)

# Gerando varias metricas sobre o modelo final
rf_final_fit %>% collect_metrics()

### Bonus - Importancia das variaveis
library(vip)
rf_final_fit %>% extract_fit_parsnip() %>% vip(num_features = 20)
