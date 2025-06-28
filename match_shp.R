
library("data.table")
library("geocodebr")
library("sf")

polygons <- st_read("./setores_shp/SP_setores_CD2022.shp")
polygons <- polygons[polygons$NM_RGINT=="Ribeirão Preto",]
polygons <- st_transform(polygons, crs = 4326)


dt <- readRDS("./data/active_rp_ju.rds")
dt_select <- dt[precisao %in% c("cep", "localidade", "municipio")]

ceps_unicos <- unique(na.omit(dt_select$cep))

# amostra de CEPs
df_ceps <- busca_por_cep(
 cep = ceps_unicos,
 resultado_sf = FALSE,
 verboso = FALSE
 )

df_ceps <- df_ceps[!is.na(lat) & !is.na(lon)]
points_sf <- st_as_sf(df_ceps, coords = c("lon", "lat"), crs = 4326)
joined <- st_join(points_sf, polygons, left = FALSE)
joined_dt <- as.data.table(joined)
result <- joined_dt[, .(n_polygons = uniqueN(CD_SETOR)), by = cep]

View(result)