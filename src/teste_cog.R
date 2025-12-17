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
stime <- Sys.time()
img1 <- rast(cbers_links[1])
print(Sys.time()-stime)

# Reproject poly to match CRS
aoi_rep <- st_transform(aoi_sf, crs = crs(img1))
crop1 <- crop(img1,aoi_rep)
plot(crop1)

