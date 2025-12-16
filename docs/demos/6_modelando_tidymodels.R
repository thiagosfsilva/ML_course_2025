## Modelando com tidymodels
## Thiago S. F. Silva
## 16/12/2025

#### Passo 0 - Obter os dados
library(readr) # para usar a funcao read_csv()

hotels <- read_csv('https://tidymodels.org/start/case-study/hotels.csv')

#### Passo 1 - Inspecionando os dados

# Inspecionando a tabela
head(hotels)
str(hotels)
summary(hotels)

# Visualizar a variavel alvo - children
library(ggplot2) # Para criar gráficos

ggplot(hotels,aes(children)) + geom_bar()


#### Passo 2 - Preparando os dados pra modelagem
library(rsample) # Usado para separar os dados

# Criar um split (divisao) dos dados
data_split <- initial_split(hotels,
                           prop = 0.75,
                           strata = 'children')

# O que é o split?
data_split

# Aplicar o split e criar as duas tabelas
hotel_training <- training(data_split) # puxa os dados de treinamento
hotel_testing <- testing(data_split) # puxa os dados de teste

#### Passo 3 - prepara os dados para entrar no modelo (engenharia de feicoes)
library(recipes) # funcoes pra preparar os dados
library(dplyr) # para usar o pipe (%>%)

# Criando uma receita de preparo dos dados
# Algumas funcoes do recipes se sobrepoe com dplyr, lubridade, forcats, etc.

holiday_list <- c('AllSouls','AshWednesday','ChristmasEve',
                  'Easter','ChristmasDay','GoodFriday',
                  'NewYearsDay','PalmSunday')

# Definindo a nossa receita de deliciosos dados:

# Primeiro, definimos uma formula que diz de onde vem os dados
# e quem são as feicoes e o alvo
rf_recipe <- recipe(children ~ ., data=hotel_training) %>%
    # converter as colunas que sao chr para fator
    step_string2factor(all_nominal_predictors()) %>%
    # separar dia, mes e ano
    step_date(arrival_date) %>%
    # cria variavel que identifica os feriados
    step_holiday(arrival_date, holidays= holiday_list) %>%
    # remover variaveis desnecessarias
    step_rm(arrival_date, hotel, country) %>%
    # normalizar as variaveis numericas
    step_normalize(all_numeric_predictors()) %>%
    # remover variaveis com variancia zero
    step_zv(all_predictors())

# O que tem dentro da receita
rf_recipe

### Passo 4 - Preparando o modelo para ajustar e otimizar
library(parsnip) # contem os algoritmos

# Escolhendo o algoritmo: Random Forest
rf_mod <- rand_forest(mtry = tune(), trees= tune()) %>% # tipo de modelo e hiperparams para otimizar
    set_engine('ranger', importance = 'impurity') %>% # qual pacote?
    set_mode('classification') # define o modo

# O que tem dentro do modelo?
rf_mod

# Conjunto de opcoes de hiperparametros
rf_grid <- expand.grid(mtry=c(3,5),
                       trees=c(50,100))
rf_grid

# Criando os folds (blocos) da validacao cruzada
# Tambem é função do pacote rsample
hotel_folds <- vfold_cv(hotel_training, v = 5, strata = children)
hotel_folds

### Passo 6 - otimizando o modelo!
### Essa é a parte mais pesada computacionalmente
### Esteja preparado para esperar
library(workflows) # pra conectar os passos
library(yardstick) # contem as metricas de erro
library(tune) # faz a otimização do modelo

# Criando um workflow
rf_workflow <- workflow() %>%
    add_model(rf_mod) %>%
    add_recipe(rf_recipe)

# Conecta o workflow com a otimização
rf_tune <- rf_workflow %>%
    tune_grid(resamples = hotel_folds, # blocos que criamos
              grid = rf_grid, # Grid que criamos
              control = control_grid(save_pred = TRUE,
                           verbose = TRUE),
              metrics = metric_set(roc_auc), # vem do yardstick
    )

### Inspecionando os resultados da otimização

# Lista os melhores resultados
rf_res <- rf_tune %>% show_best(metric = 'roc_auc')
rf_res

# Selecionar o melhor resultado de todos
rf_best <- rf_tune %>% select_best(metric = 'roc_auc')

# Plotando a ROC

# Extraindo valores para construir a curva ROC
rf_auc <- rf_tune %>%
    collect_predictions(parameters = rf_best) %>%
    roc_curve(children, .pred_children) %>%
    mutate(model = 'RandomForest')

rf_auc %>% ggplot(aes(x=1-specificity, y=sensitivity, col=model)) +
    geom_line(lwd=1.5, alpha=0.8, color = 'blue') +
    geom_abline(lty=3) +
    coord_equal()

### Passo 7 - ajustar o modelo final e testar

# O modelo final é:
rf_final <- rf_mod %>% finalize_model(rf_best)
rf_final

# Atualizando o workflow
rf_workflow_final <- rf_workflow %>%
    update_model(rf_final)

# Gera o ajuste final
rf_final_fit <- rf_workflow_final %>%
    last_fit(data_split)

# Puxando metricas de erro para o teste
rf_final_fit %>% collect_metrics()

### Bonus - Importancia das variáveis
library(vip)
rf_final_fit %>%
    extract_fit_parsnip() %>%
    vip(num_features = 10)
