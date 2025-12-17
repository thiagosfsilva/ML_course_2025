library(terra)
library(rstac)
library(mapview)
library(sf)

# Definindo um catálogo STAC e testabdo a conexão
bdc_stac <- stac("https://data.inpe.br/bdc/stac/v1/")
get_request(bdc_stac)

aoi_bbox <- c(xmin = -43.272,ymin = -2.510,xmax = -43.148,ymax = -2.440)

# Convert to sf object (WGS84)
aoi_sf <- st_as_sfc(st_bbox(aoi_bbox, crs = 4326))

mapview(aoi_sf)

cbers_mux <- bdc_stac  %>%
    stac_search('CBERS4-MUX-2M-1',
                bbox = aoi_bbox,
                datetime = "2024-12-01/2025-06-30") %>%
    get_request()

cbers_links <- cbers_mux %>% assets_url()

# usando o mesmo box de antes, mas o terra espera uma ordem diferente:
# longmin, longmax, latmin,latmax

# Lendo direto da web pra memória
for(img in c(11:length(cbers_links))){
    nome <- paste(unlist(strsplit(cbers_links[img],"_"))[4:5],collapse="_")
    print(img)
    print(nome)
    start <- Sys.time()
    print(paste('started connection at', start))
    rs <- rast(cbers_links[img])
    print(paste('Connection time:', round(Sys.time()-start,2),'s'))
    print('started_processing')
    start <- Sys.time()
    print(paste('started processing at', start))
    aoi_rep <- st_transform(aoi_sf, crs = crs(rs))
    cp <- crop(rs,aoi_rep)
    print(paste('Processing time:', round(Sys.time()-start,2),'s'))
    print('Saving...')
    writeRaster(cp,paste0('data/large/',nome),overwrite=T)
    print('End')
}

r1 <- rast(c('data/large/20241101_BAND5.tif',
           'data/large/20241101_BAND6.tif',
           'data/large/20241101_BAND7.tif',
           'data/large/20241101_BAND8.tif',
           'data/large/20241101_NDVI.tif',
           'data/large/20241101_EVI.tif'))

r2<-  rast(c('data/large/20250101_BAND5.tif',
             'data/large/20250101_BAND6.tif',
             'data/large/20250101_BAND7.tif',
             'data/large/20250101_BAND8.tif',
             'data/large/20250101_NDVI.tif',
             'data/large/20250101_EVI.tif'))

r3<-  rast(c('data/large/20250301_BAND5.tif',
             'data/large/20250301_BAND6.tif',
             'data/large/20250301_BAND7.tif',
             'data/large/20250301_BAND8.tif',
             'data/large/20250301_NDVI.tif',
             'data/large/20250301_EVI.tif'))

r4<-  rast(c('data/large/20250101_BAND5.tif',
             'data/large/20250501_BAND6.tif',
             'data/large/20250501_BAND7.tif',
             'data/large/20250501_BAND8.tif',
             'data/large/20250501_NDVI.tif',
             'data/large/20250501_EVI.tif'))


library(ggspatial)
library(ggplot2)
ggplot() + layer_spatial(r2[[c(2,4,3)]])

writeRaster(r2,'data/large/imagem_composta_CBERS.tif')
