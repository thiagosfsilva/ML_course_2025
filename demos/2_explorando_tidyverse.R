# Explorando o tidyverse
# Thiago Silva
# 2025-12-11

# Carrega pacotes necessarios
library(tidyverse) # Estamos aprendendo funcoes desse pacote

# Usando o dataset relig_income incluso no pacote tidyr
head(relig_income)
glimpse(relig_income)

# Rearranjando colunas para o formato longo
relig_tidy <- relig_income %>%
    pivot_longer(cols = !religion,
                 names_to = 'renda',
                 values_to = 'freq',
                 names_transform = as.factor)

# Inspecionando o resultado
head(relig_tidy)
glimpse(relig_tidy)

# Usando agora o dataset billboard
head(billboard)

# Rearranjando para o formato longo
billboard_tidy <- billboard %>%
    pivot_longer(cols = starts_with('wk'),
                 names_to = 'week',
                 values_to = 'rank',
                 values_drop_na = TRUE)

# Inspecionando o resultado
head(billboard_tidy)
summary(billboard_tidy)

# Agora trabalhando com o dataset who
head(who)

# country: país
# iso2 e iso3: siglas padronizadas para cada país
# year: ano da medição
# new_/new: indica que são novos casos.
# sp/rel/ep: método de diagnóstico do caso
# m/f: gênero do paciente
# 014/1524/2535/3544/4554/65: intervalos de idade

# Começando a desmembrar
who_tidy <- who %>%
    pivot_longer(cols = starts_with('new'),
                                 names_to = 'capeta',
                                 values_to = 'freq') %>%
    mutate(new = str_sub(capeta,1,3),
           diagn = str_sub(capeta,4,6),
           gender = str_sub(capeta,8,8),
           age_range = str_sub(capeta,9,12)) %>%
    select(!c(capeta,new)) %>%
    mutate_if(is.character,as.factor) %>%
    mutate(diagn = fct_recode(diagn,
        'sp'='_sp','sn'='_sn','ep'='_ep'))


head(who_tidy)

summary(who_tidy)
unique(who_tidy$diagn)

# Parentese - entendendo o dplyr com o dataset iris
head(iris)


res <- iris %>%
    mutate(Sepal_ratio = Sepal.Length/Sepal.Width,
                       Petal_ratio = Petal.Length/Petal.Width) %>%
    select(Species,Sepal_ratio,Petal_ratio) %>%
    filter(Species != 'setosa') %>%
    group_by(Species) %>%
    summarise(Sepal_mean = mean(Sepal_ratio),
              Sepal_sd = sd(Sepal_ratio))

res
