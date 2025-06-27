library(data.table)
setwd("d:/Users/anderson.chen/Documents/mapeando_cep/")
dt<-fread("active_rp_ju.csv",encoding="Latin-1")

library(geocodebr)
library(geosphere)

file <- paste0("data/estabs_sem_coords.csv")
dt <- fread(file, encoding = "Latin-1")
names(dt)

ceps_unicos <- unique(na.omit(dt$cep))

# amostra de CEPs

df_ceps <- busca_por_cep(
  cep = ceps_unicos,
  resultado_sf = FALSE,
  verboso = FALSE
)

compute_centroid <- function(lat, lon) {
  # Convert to radians
  lat_rad <- lat * pi / 180
  lon_rad <- lon * pi / 180
  
  # Convert to Cartesian
  x <- cos(lat_rad) * cos(lon_rad)
  y <- cos(lat_rad) * sin(lon_rad)
  z <- sin(lat_rad)
  
  # Mean of coordinates
  x_mean <- mean(x)
  y_mean <- mean(y)
  z_mean <- mean(z)
  
  # Back to lat/lon
  hyp <- sqrt(x_mean^2 + y_mean^2)
  lat_c <- atan2(z_mean, hyp) * 180 / pi
  lon_c <- atan2(y_mean, x_mean) * 180 / pi
  
  return(c(lat_centroid = lat_c, lon_centroid = lon_c))
}

mean_pairwise_distance <- function(lat, lon) {
  coords <- cbind(lon, lat)  # lon-lat format for geosphere
  n <- nrow(coords)
  if (n < 2) return(0)  # return 0 instead of NA
  dists <- distm(coords)
  mean(dists[upper.tri(dists)])
}

## Zipcode centroid and mean pairwise distance 

result <- df_ceps[, {
  centroid <- compute_centroid(lat, lon)
  mpd <- mean_pairwise_distance(lat, lon)
  .(lat_centroid = centroid["lat_centroid"],
    lon_centroid = centroid["lon_centroid"],
    mean_pairwise_distance = mpd)
}, by = cep]
saveRDS(result, file = "data/result.rds")

## Check if each zipcode refers to only one city 
df_ceps[, .(n_cities = uniqueN(municipio)), by = cep][n_cities > 1]
#Answer: NO, in 145 cases more than one city per zip :(

## Relate each zipcode with the most common city
counts <- df_ceps[, .N, by = .(cep, municipio, estado)]
most_common_city <- counts[order(-N), .SD[1], by = cep]

## Inner join
final <- merge(result, most_common_city , by = "cep", all = FALSE)
saveRDS(final, file = "data/final.rds")

## Create estabs_COM_coords
final[, cep := as.numeric(gsub("-", "", cep))]
estabs_com_coords <- merge(dt, final, by = "cep", all = FALSE)
fwrite(estabs_com_coords, file = paste0("data/estabs_com_coords.csv"), sep = ";", quote = FALSE, row.names=FALSE)


# Exporta apenas as colunas desejadas do objeto 'result'
fwrite(
  result[, .(cep, lat_centroid, lon_centroid, mean_pairwise_distance)],
  file = "data/ceps_centroides_distancia.csv",
  sep = ";",
  quote = FALSE,
  row.names = FALSE
)



# Supondo que seu data.table se chame dt
diferente <- final[!municipio %in% c("RIBEIRAO PRETO", "JUNDIAI")]

sem_precisão <- dt[precisao %in% c("municipio", "localidade")]

ceps_unicos <- unique(na.omit(sem_precisão$cep))

# amostra de CEPs

df_ceps_sem_prec <- busca_por_cep(
  cep = ceps_unicos,
  resultado_sf = FALSE,
  verboso = FALSE
)

result_sem_prec <- df_ceps_sem_prec[, {
  centroid <- compute_centroid(lat, lon)
  mpd <- mean_pairwise_distance(lat, lon)
  .(lat_centroid = centroid["lat_centroid"],
    lon_centroid = centroid["lon_centroid"],
    mean_pairwise_distance = mpd)
}, by = cep]

counts <- df_ceps_sem_prec[, .N, by = .(cep, municipio, estado)]
most_common_city <- counts[order(-N), .SD[1], by = cep]

## Inner join
final <- merge(result, most_common_city , by = "cep", all = FALSE)




# MAPA

# Instalar pacotes necessários (caso não tenha instalado)
# install.packages(c("data.table", "sp", "rgdal"))

# Carregar pacotes
library(data.table)
library(sp)
library(rgdal)

# Função para calcular a área de um polígono usando a fórmula de Gauss (Shoelace)
polygon_area <- function(coords) {
  x <- coords$longitude
  y <- coords$latitude
  return(0.5 * abs(sum(x * c(y[-1], y[1])) - sum(y * c(x[-1], x[1]))))
}

# Exemplo de dados de latitude e longitude (pode substituir por seus dados)
coords <- data.table(
  id = 1:5,
  latitude = c(37.7749, 34.0522, 36.1699, 40.7128, 41.8781),  # São Francisco, Los Angeles, Las Vegas, Nova York, Chicago
  longitude = c(-122.4194, -118.2437, -115.1398, -74.0060, -87.6298)
)

# Definir o sistema de coordenadas geográficas (WGS84)
coordinates(coords) <- ~longitude + latitude
proj4string(coords) <- CRS("+proj=longlat +datum=WGS84")

# Convertendo para UTM (Zona 33N - ajuste conforme necessário)
utm_coords <- spTransform(coords, CRS("+proj=utm +zone=33 +datum=WGS84"))

# Convertendo para data.table novamente para facilitar manipulação
coords_utm <- as.data.table(utm_coords)

# Calculando o Convex Hull (índices dos pontos)
hull_indices <- chull(coords_utm$longitude, coords_utm$latitude)

# Obter as coordenadas do Convex Hull
hull_coords <- coords_utm[hull_indices, ]

# Plotando o Convex Hull
plot(coords_utm$longitude, coords_utm$latitude, main = "Convex Hull", xlab = "Longitude", ylab = "Latitude")
polygon(hull_coords$longitude, hull_coords$latitude, col = rgb(0.2, 0.5, 0.2, alpha = 0.5), border = "red")

# Calcular a área do Convex Hull
area <- polygon_area(hull_coords)

# Exibir a área
cat("Área do Convex Hull (em unidades quadradas UTM):", area, "\n")

