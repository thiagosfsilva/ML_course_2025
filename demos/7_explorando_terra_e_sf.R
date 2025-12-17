#### Explorando geoprocessamento no R
#### Thiago S. F. Silva
#### 17/12/2025

# Se precisar instalar
# install.packages("terra","sf","dplyr","RStoolbox")

# Pacotes necessários
library(terra)
library(sf)
library(dplyr)
library(RStoolbox)

# Obtendo dados - exemplo ao contrario
# Salvando os datasets de exemplo do RStoolbox

# Exemplo de imagem Landsat do pacote RStoolbox
# usando pacote terra para salvar como um Geotiff
writeRaster(lsat,'data/large/exemplo_landsat.tif')

# Exemplo de arquivo vetorial
vetor <- readRDS(system.file("external/trainingPolygons_lsat.rds",
                package="RStoolbox"))

# Usando o pacote sf para salvar
st_write(vetor,'data/large/exemplo_vetor.gpkg')

# Agora podemos fingir que temos dois arquivos externos

### Operacoes com Raster

# Lendo um raster para o R
landsat <- rast('data/large/exemplo_landsat.tif')

# O que esta dentro do objeto?
landsat

# Inspecionando arquivo raster
summary(landsat)
nrow(landsat)
ncol(landsat)
nlyr(landsat)
crs(landsat)

# Visualisando o conteudo do arquivo
plot(landsat)
plotRGB(landsat, r=5, g=4, b=3, stretch='linear')

# Uma alternativa é usar o pacote mapview
mapview(landsat)

# Renomeando bandas
names(landsat) # Lista os nomes

names(landsat) <- c('Azul','Verde',
                    'Vermelho','IVP','IVM1',
                    'Termal','IVM2')
names(landsat)

# Operacoes com bandas
ndvi <- (landsat$IVP-landsat$Vermelho)/(landsat$IVP+landsat$Vermelho)
plot(ndvi)

# Operacoes com escalares
ndvi_prc <- ndvi*100
plot(ndvi_prc)

### Vetor

# Carregando um aquivo vetorial com o pacote sf
vetor <- st_read('data/large/exemplo_vetor.gpkg')

# Inspecionando arquivo vetorial
vetor
head(vetor)
summary(vetor)

# Visualisando o conteudo do arquivo no mapa
plot(vetor)
mapview(vetor)

# Combinando layers
mapview(landsat) + mapview(vetor)

# Operacoes com atributos
floresta <- vetor %>%
    filter(class == 'forest')

# Se precisar transformar em dataframe sem geometria
# use %>% st_drop_geometry() no final do pipe

mapview(landsat) + mapview(floresta)


