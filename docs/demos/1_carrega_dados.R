# Lendo arquivos de dados no R
# Thiago Silva - 2025-12-10

# carrega os pacotes necessarios
library(readxl) # Para ler planilha excel
library(readr) # Para ler planilha CSV
library(dplyr) # Para usar a funcao glimpse()
library(arrow) # Para ler arquivo parquet

# Importa a planilha Excel
plot_florestal <- read_xlsx(
    path = 'dados/brutos/plot_florestal.xlsx'
    )

plot_florestal_obs <- read_xlsx(
    path = 'dados/brutos/plot_florestal.xlsx',
    sheet = 'Obs'
    )

# Conferindo a integridade dos dados
head(plot_florestal) # Primeiars linhas
tail(plot_florestal) # Ultimas linhas
summary(plot_florestal) # estatisticas descritivas
str(plot_florestal) # estrtura e tipos de variavel
glimpse(plot_florestal) # estrutura e tipos de variavel- tidyverse


# Importa arquivo CSV usando R base
turbidez <- read.csv(
    'dados/brutos/turbidez.csv'
    )

head(turbidez) # Primeiars linhas
tail(turbidez) # Ultimas linhas
summary(turbidez) # estatisticas descritivas
str(turbidez) # estrtura e tipos de variavel
glimpse(turbidez) # estrutura e tipos de variavel- tidyverse

# Importa aqruivo CSV usando tidyverse
turbidez2 <- read_csv(
    'dados/brutos/turbidez.csv'
    )

head(turbidez2) # Primeiras linhas
tail(turbidez2) # Ultimas linhas
summary(turbidez2) # estatisticas descritivas
str(turbidez2) # estrtura e tipos de variavel
glimpse(turbidez2) # estrutura e tipos de variavel- tidyverse

# Importa um arquivo parquet
voos <- read_parquet('dados/brutos/flights-1m.parquet')

write_csv(voos,'dados/processados/flights-1m.csv')

saveRDS(voos,'dados/processados/flights-1m.rds')

voos_csv <- read_csv('dados/processados/flights-1m.csv')

voos_rds<- readRDS('dados/processados/flights-1m.rds')
