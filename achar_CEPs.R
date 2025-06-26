source("general-functions.R") # General functions

# packages
required_packages <- c("data.table", "geocodebr", "geosphere")

check_and_install_packages(required_packages)
lapply(required_packages, library, character.only = TRUE)

file <- paste0("data/estabs_sem_coords.csv")
dt <- fread(file, encoding = "Latin-1")
names(dt)

ceps_unicos <- unique(na.omit(dt$CO_CEP))

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

