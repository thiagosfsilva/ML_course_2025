### Classificando uma imagem
### Thiago S. F. Silva
### 17/12/2025

# Pacotes necessarios:
library(terra)
library(sf)
library(mapview)
library(rstac)
library(dplyr)

### Parte 0 - Como baixar dados direto de um catalogo STAC

# Definir o endereco do catalogo
bdc <- stac("https://data.inpe.br/bdc/stac/v1/")
get_request(bdc)

area_interesse <- c(xmin = -43.272,
                    ymin = -2.510,
                    xmax = -43.148,
                    ymax = -2.440)

busca_cbers <- bdc %>%
    stac_search('CBERS4-MUX-2M-1',
                bbox = area_interesse,
                datetime = "2024-12-01/2025-01-31") %>%
    get_request()

# Quais os links?
busca_cbers %>% assets_url()

# Quais os assets?
busca_cbers %>% items_assets()

# Preview da imagem
imagem_1 <- busca_cbers %>%assets_url(asset_names = "thumbnail")

# Preview imagem 1
preview_plot(imagem_1[1])
# Preview imagem 2
preview_plot(imagem_1[2])

# NAO EXECUTE
# Ira baixar a imagem inteira
# busca_cbers %>% assets_download(output_dir = 'pastaquenaoexiste')

# Ao inves de baixar tudo, vamos pegar so um pedaco, usando o terra

# Lista as bandas do NDVI
ndvi_link <- busca_cbers %>% assets_url(asset_names = "NDVI")
ndvi_link

# Cria um objeto SpatRast conectado com o servidor
# a funcao rast() nao le os dados, só conecta
cbers_ndvi <- rast(ndvi_link[1])

# Transformamos a area de interesse em poligono sf
area_interesse_sf <- st_as_sfc(st_bbox(area_interesse, crs = 4326))

# Cortamos a imagem (ainda no servidor) usando o poligono da AdI
cbers_ndvi_crop <- crop(cbers_ndvi,area_interesse_sf)

# Salva a imagem como GeoTiff
writeRaster(cbers_ndvi_crop,'dados/CBERS_NDVI.tif')

### Parte 1 - importando os dados de imagem e amostras e convertendo para tabela

# Carrega imagem CBERS
imagem <- rast('dados/imagem_composta_CBERS.tif')
imagem
names(imagem)

# Renomear as bandas
names(imagem) <- c('B5','B6','B7','B8','NDVI','EVI')

# Carrega poligonos das amostras
amostras <- st_read('dados/amostras.gpkg')
head(amostras)

# Quais sao as classes?
unique(amostras$classe)

# Os poligonos coincidem com a imagem?
mapview(imagem) + mapview(amostras)

# Extraindo as amostras do raster
amostras_df <- extract(imagem,amostras)

head(amostras_df)

# Conectando amostras com pixels

amostras_final <- amostras %>%
    mutate(ID = row_number()) %>%
    right_join(amostras_df, by=join_by('ID')) %>%
    st_drop_geometry() %>%
    select(-area_m2)

### Parte II - Classificando a imagem

# Visualizar a variavel alvo - classe
library(ggplot2) # Para criar gráficos

# classes estao desbalanceadas
ggplot(amostras_final,aes(classe)) + geom_bar()

# visualizando combinacoes de bandas
ggplot(amostras_final,aes(x =B5, y=B8, colour = classe)) +
    geom_point()

ggplot(amostras_final,aes(x = NDVI, y=EVI, colour = classe)) +
    geom_point()

### Adicionando uma coluna de NDWI
amostras_final <- amostras_final %>%
    mutate(NDWI = (B6-B8)/(B6+B8))

ggplot(amostras_final,aes(x = NDVI, y=NDWI, colour = classe)) +
    geom_point()

#### Passo 2 - Preparando os dados pra modelagem
library(rsample) # Usado para separar os dados

# Criar um split (divisao) dos dados
data_split <- initial_split(amostras_final,
                            prop = 0.75,
                            strata = 'classe')

# O que é o split?
data_split

# Aplicar o split e criar as duas tabelas
imagem_training <- training(data_split) # puxa os dados de treinamento
imagem_testing <- testing(data_split) # puxa os dados de teste

#### Passo 3 - prepara os dados para entrar no modelo (engenharia de feicoes)
library(recipes) # funcoes pra preparar os dados
library(dplyr) # para usar o pipe (%>%)

# Criando uma receita de preparo dos dados
# Algumas funcoes do recipes se sobrepoe com dplyr, lubridade, forcats, etc.

rf_recipe <- recipe(classe ~ ., data=imagem_training) %>%
    # normalizar as variaveis numericas
    step_normalize(all_numeric_predictors()) %>%
    # remover variaveis com variancia zero
    step_zv(all_predictors()) %>%
    step_rm(ID)

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
rf_grid <- expand.grid(mtry=c(3,4,5),
                       trees=c(50,100,500))
rf_grid

# Criando os folds (blocos) da validacao cruzada
# Tambem é função do pacote rsample
imagem_folds <- vfold_cv(imagem_training, v = 5, strata = classe)
imagem_folds

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
    tune_grid(resamples = imagem_folds, # blocos que criamos
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

## Aplicando o modelo

# Criar uma data_frame que é a imagem toda

imagem_df <- as.data.frame(imagem)
head(imagem_df)

imagem_df <- imagem_df %>%
    mutate(NDWI = (B6-B8)/(B6+B8),
           ID = NA)

mod <- rf_final_fit %>% extract_workflow()

res <- predict(mod, new_data = imagem_df)
levels(res$.pred_class)
res_num <- as.numeric(res$.pred_class)

class_rf <- imagem$B5
values(class_rf) <- res_num

pal = mapviewPalette("mapviewTopoColors")
mapview(imagem) + mapview(class_rf, col.regions = pal(4))

writeRaster(class_rf,'dados/classificao_rf')

### E o SVM?

# Precisamos de um novo modelo
svm_mod <- svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
    set_engine('kernlab') %>%
    set_mode('classification')

svm_workflow <- rf_workflow %>% update_model(svm_mod)

# Novo grid

svm_grid <- expand.grid(cost = c(0.1,1,10),
                        rbf_sigma = c(0.01,0.1,1))

# Conecta o workflow com a otimização
svm_tune <- svm_workflow %>%
    tune_grid(resamples = imagem_folds, # blocos que criamos
              grid = svm_grid, # Grid que criamos
              control = control_grid(save_pred = TRUE,
                                     verbose = TRUE),
              metrics = metric_set(roc_auc), # vem do yardstick
    )

### Inspecionando os resultados da otimização

# Lista os melhores resultados
svm_res <- svm_tune %>% show_best(metric = 'roc_auc')
svm_res

# Selecionar o melhor resultado de todos
svm_best <- svm_tune %>% select_best(metric = 'roc_auc')

### Passo 7 - ajustar o modelo final e testar

# O modelo final é:
svm_final <- svm_mod %>% finalize_model(svm_best)
svm_final

# Atualizando o workflow
svm_workflow_final <- svm_workflow %>%
    update_model(svm_final)

# Gera o ajuste final
svm_final_fit <- svm_workflow_final %>%
    last_fit(data_split)

# Puxando metricas de erro para o teste
svm_final_fit %>% collect_metrics()

mod_svm <- svm_final_fit %>% extract_workflow()

res_svm <- predict(mod_svm, new_data = imagem_df)
res_svm_num <- as.numeric(res_svm$.pred_class)

class_svm <- imagem$B5
values(class_svm) <- res_svm_num

pal = mapviewPalette("mapviewTopoColors")
mapview(imagem) +
    mapview(class_rf, col.regions = pal(4), alpha.regions=1) +
    mapview(class_svm, col.regions = pal(4),alpha.regions=1)

