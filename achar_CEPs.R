source("general-functions.R") # General functions

# packages
required_packages <- c("data.table", "geocodebr", "geosphere", "sf", "leaflet")

check_and_install_packages(required_packages)
lapply(required_packages, library, character.only = TRUE)

file <- paste0("data/active_rp_ju.csv")
dt <- fread(file, encoding = "Latin-1", fill = TRUE)
names(dt)

dt_select <- dt[precisao=="cep"]
dt_select <- dt[precisao %in% c("cep", "localidade", "municipio")]

ceps_unicos <- unique(na.omit(dt_select$cep))

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


####################################
## Selection


dt_select <- dt[precisao=="cep"]
dt_select <- dt[precisao %in% c("cep", "localidade", "municipio")]

select_ <- dt_select[cep %in% c(14098000)]


######################################
### convexhull


df_ceps[, uniqueN(cep)]

select <- df_ceps[cep %in% c("14098-000")]
select <- df_ceps

dt_sf <- st_as_sf(select, coords = c("lon", "lat"), crs = 4326)  # WGS84

dt_sf <- st_transform(dt_sf, crs = 3857)  # Web Mercator

cep_groups <- split(dt_sf, dt_sf$cep)

# Step 4: Compute convex hull for each cep
convex_list <- lapply(cep_groups, function(group) {
  combined <- st_combine(group)                 # sfc MULTIPOINT
  hull <- st_convex_hull(combined)[[1]]         # extract sfg POLYGON
  return(hull)
})

convex_areas <- st_sf(
  cep = names(convex_list),
  geometry = st_sfc(convex_list),
  crs = 3857
)

convex_areas$area_m2 <- st_area(convex_areas)

new_geoms <- lapply(convex_areas$geometry, function(geom) {
  if (st_geometry_type(geom) %in% c("POINT", "LINESTRING")) {
    st_buffer(geom, dist = 1)
  } else {
    geom
  }
})

# Step 2: Create an sfc geometry collection with the right CRS
new_sfc <- st_sfc(new_geoms, crs = st_crs(convex_areas))

# Step 3: Replace geometry with new sfc in the sf object using st_set_geometry()
convex_areas_fixed <- st_set_geometry(convex_areas, new_sfc)

convex_areas_fixed_wgs <- st_transform(convex_areas_fixed, 4326)
dt_sf_wgs <- st_transform(dt_sf, 4326)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = convex_areas_fixed_wgs,
              fillColor = "blue", fillOpacity = 0.3,
              color = "black", weight = 1,
              label = ~paste0("CEP: ", cep, "<br>Area: ", round(area_m2, 0), " m²")) %>%
  addCircleMarkers(data = dt_sf_wgs,
                   radius = 4, color = "red",
                   stroke = FALSE, fillOpacity = 0.8,
                   label = ~paste("CEP:", cep))
