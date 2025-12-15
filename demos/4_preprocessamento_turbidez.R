## Prática de gerenciamento e visualizacao
## Thiago S. F. Silva
## 12/12/2025

# Carregando pacotes necessarios
library(tidyverse)

# Carregar os dados brutos
turbidez_bruto <- read_csv('dados/brutos/turbidez.csv')

# Inspecao inicial
head(turbidez_bruto)
summary(turbidez_bruto)

# Usando o lubridate pra converter as datas para
# o tipo de dado POSIX (que representa datas)
turbidez <- turbidez_bruto %>%
    mutate(data_iso = parse_date_time(data,orders = 'Y-m-d')) %>%
    mutate(data_outro = parse_date_time(data,orders = 'd/m/Y')) %>%
    rowwise() %>%
    mutate(data_final = max(data_iso,data_outro, na.rm = TRUE)) %>%
    ungroup() %>%
    select(!c(data,data_iso,data_outro))
# Resolvemos o problema das datas

# Resolvendo problema dos valores numericos
teste_temp_c <- turbidez %>%
    select(temp_c) %>%
    mutate(temp_teste = as.numeric(temp_c))

teste_temp_c %>% filter(is.na(temp_teste))

# Quais as linhas com NA
which(is.na(teste_temp_c$temp_teste))

# Substituindo manualmente
turbidez$temp_c[9] <- NA
turbidez$temp_c[43] <- 27.4
turbidez$temp_c[47] <- 15.2
turbidez$temp_c[69] <- NA

turbidez <- turbidez %>% mutate(temp_c = as.numeric(temp_c))
# Resolvemos os valores errados de temperatura

# Resolvendo o problema dos valores de turbidez
teste_turb <- turbidez %>%
    select(turbidez_ntu) %>%
    mutate(turb_teste = as.numeric(turbidez_ntu))

teste_turb %>% filter(is.na(turb_teste))

# Quais as linhas com NA
which(is.na(teste_turb$turb_teste))

# Substituindo manualmente
turbidez$turbidez_ntu[21] <- NA
turbidez$turbidez_ntu[85] <- NA
turbidez$turbidez_ntu[108] <- 20.3

turbidez <- turbidez %>% mutate(turbidez_ntu = as.numeric(turbidez_ntu))
# Resolvemos os valores errados de turbidez

# Resolvendo o problema dos valores de ph
teste_ph <- turbidez %>%
    select(ph) %>%
    mutate(ph_teste = as.numeric(ph))

teste_ph %>% filter(is.na(ph_teste))

# Quais as linhas com NA
which(is.na(teste_ph$ph_teste))

# Substituindo manualmente
turbidez$ph[24] <- 8.1
turbidez$ph[74] <- NA
turbidez$ph[75] <- NA

turbidez <- turbidez %>% mutate(ph = as.numeric(ph))
# Resolvemos os valores errados de ph

# Usando gráficos para avaliar os dados

# Um otimo grafico inicial é o boxplot
ggplot(turbidez,aes(x = id_amostra)) + geom_boxplot()
ggplot(turbidez,aes(x = temp_c)) + geom_boxplot()
ggplot(turbidez,aes(x = ph)) + geom_boxplot()
ggplot(turbidez,aes(x = turbidez_ntu)) + geom_boxplot()

summary(turbidez)

# Temos valores de -999 que provavelmente são NA

# Quais linhas?
which(turbidez$temp_c == -999)
which(turbidez$ph == -999)
which(turbidez$turbidez_ntu == -999)

turbidez <- turbidez %>%
    mutate(temp_c = na_if(temp_c,-999)) %>%
    mutate(ph = na_if(ph,-999)) %>%
    mutate(turbidez_ntu = na_if(turbidez_ntu,-999))

summary(turbidez)

# Salvando a tabela corrigida
saveRDS(turbidez,
        'dados/processados/turbidez.rds')

# Para recuperar a tabela
# turbidez <- readRDS('dados/processados/turbidez.rds')

###### Dados do plot
library(readxl)
dados_plot_bruto <- read_xlsx('dados/brutos/plot_florestal.xlsx')

names(dados_plot_bruto)

# Renomeando colunas
dados_plot <- dados_plot_bruto %>%
    rename(especie = Espécie,
           familia = Família,
           genero = Gênero,
           ano_recrutamento = 'Ano Recrutamento') %>%
    rename_with(tolower)

names(dados_plot)
