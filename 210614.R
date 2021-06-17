# 14 de junio de 2021

# Carga de bibliotecas
library(sf)
library(raster)
library(dplyr)
library(spData)

library(leaflet)
library(plotly)
library(DT)

# Carga de la capa de provincias
provincias <-
  st_read(
    "https://raw.githubusercontent.com/gf0604-procesamientodatosgeograficos/2021i-datos/main/ign/delimitacion-territorial-administrativa/cr_provincias_simp_wgs84.geojson",
    quiet = TRUE
  )

# Mapa de la capa de provincias
plot(
  provincias$geometry,
  extent = extent(-86,-82.3, 8, 11.3),
  main = "Provincias de Costa Rica",
  axes = TRUE,
  graticule = TRUE
)

# Carga de la capa de cantones
cantones <-
  st_read(
    "https://raw.githubusercontent.com/gf0604-procesamientodatosgeograficos/2021i-datos/main/ign/delimitacion-territorial-administrativa/cr_cantones_simp_wgs84.geojson",
    quiet = TRUE
  )

# Mapa de la capa de cantones
plot(
  cantones$geometry,
  extent = extent(-86,-82.3, 8, 11.3),
  main = "Cantones de Costa Rica",
  axes = TRUE,
  graticule = TRUE
)

# Carga de la capa de ASP
asp <-
  st_read(
    "https://raw.githubusercontent.com/gf0604-procesamientodatosgeograficos/2021i-datos/main/sinac/asp/asp-wgs84.geojson",
    quiet = TRUE
  )

# Mapa de la capa de ASP
plot(
  asp$geometry,
  extent = extent(-86,-82.3, 8, 11.3),
  main = "Áreas silvestres protegidas (ASP) de Costa Rica",
  axes = TRUE,
  graticule = TRUE
)

# Carga de la capa de aeródromos
aerodromos <-
  st_read(
    "https://raw.githubusercontent.com/gf0604-procesamientodatosgeograficos/2021i-datos/main/ign/aerodromos/aerodromos-wgs84.geojson",
    quiet = TRUE
  )

# Mapa de la capa de aeródromos
plot(
  aerodromos$geometry,
  extent = extent(-86,-82.3, 8, 11.3),
  main = "Aeródromos de Costa Rica",
  axes = TRUE,
  graticule = TRUE
)

# Carga de la capa de vipéridos
viperidos <-
  st_read(
    "https://raw.githubusercontent.com/gf0604-procesamientodatosgeograficos/2021i-datos/main/gbif/viperidae-cr-registros.csv",
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    ),
    quiet = TRUE
  )

# Asignación del sistema de coordenadas
st_crs(viperidos) = 4326

# Mapa de la capa de vipéridos
plot(
  viperidos$geometry,
  pch = 16,
  extent = extent(-86,-82.3, 8, 11.3),
  main = "Registros de Viperidae (tobobas) en Costa Rica",
  col = "green",
  axes = TRUE,
  graticule = TRUE
)

# Selección de la provincia de Limón (por atributos)
limon <- provincias[provincias$provincia == "Limón", ]

# Selección de los aeródromos ubicados en Limón (espacial)
aerodromos_limon <- aerodromos[limon, , op = st_within]

# Mapa de aeródromos
plot(
  limon$geometry,
  main = "Aeródromos de Limón (1)",
  axes = TRUE,
  graticule = TRUE,
  reset = FALSE
)
plot(aerodromos_limon$geometry, pch=16, add = TRUE)


# Selección de la provincia de Limón (por atributos)
limon <-
  provincias %>%
  filter(provincia == "Limón")

# Selección de los aeródromos ubicados en Limón (espacial)
aerodromos_limon <-
  aerodromos %>%
  filter(st_within(x = ., y = limon, sparse = FALSE))

# Mapa de aeródromos
plot(
  limon$geometry,
  main = "Aeródromos de Limón (2)",
  axes = TRUE,
  graticule = TRUE,
  reset = FALSE
)
plot(aerodromos_limon$geometry, pch = 16, add = TRUE)


## Ejemplo de st_contains: ASP contenidas en Limón

# Selección de las ASP contenidas en Limón
asp_limon <-
  asp %>%
  filter(st_contains(x = limon, y = ., sparse = FALSE))

# Mapa de ASP contenidas en Limón
plot(
  limon$geometry,
  main = "ASP contenidas en Limón",
  axes = TRUE,
  graticule = TRUE,
  reset = FALSE
)
plot(asp_limon$geometry, col = "green", add = TRUE)


## Ejemplo de st_intersects: ASP intersecadas con Limón

# Selección de las ASP intersecadas con Limón
asp_limon <-
  asp %>%
  filter(st_intersects(x = limon, y = ., sparse = FALSE))

# Mapa de ASP intersecadas con Limón
plot(
  limon$geometry,
  main = "ASP intersecadas con Limón",
  axes = TRUE,
  graticule = TRUE,
  reset = FALSE
)
plot(asp_limon$geometry, col = "green", add = TRUE)


## Ejemplo de st_disjoint: ASP ubicadas fuera de Limón

# Selección de las ASP ubicadas fuera de Limón
asp_fuera_limon <-
  asp %>%
  filter(st_disjoint(x = limon, y = ., sparse = FALSE))

# Mapa de ASP ubicadas fuera de Limón
plot(
  provincias$geometry,
  extent = extent(-86,-82.3, 8, 11.3),
  main = "ASP ubicadas fuera de Limón",
  axes = TRUE,
  graticule = TRUE,
  reset = FALSE
)
plot(asp_fuera_limon$geometry, col = "green", add = TRUE)



## 2. CRUCE DE DATOS
# Filtrado de los registros de serpientes de terciopelo (Bothrops asper) en el conjunto de vipéridos
terciopelos <-
  viperidos %>%
  filter(species == "Bothrops asper")

# Mapeo de la capa de terciopelos
plot(
  terciopelos$geometry,
  pch = 16,
  main = "Registros de terciopelos (Bothrops asper) en Costa Rica",
  col = "green",
  axes = TRUE,
  graticule = TRUE
)

# Cruce de datos espaciales con la tabla de cantones, para obtener el nombre del cantón
terciopelos <- 
  terciopelos %>%
  st_join(cantones["canton"])

# Despliegue de los datos cruzados
terciopelos %>%
  st_drop_geometry() %>%
  slice(1:10) %>%
  select(stateProvince, canton, locality)

# Promedio de altitud de puntos más altos para cada región de NZ
nz_altitud_promedio_x_region <-
  nz_height %>%
  aggregate(by = nz, FUN = mean)

# Mapa de altitud promedio en regiones
plot(
  nz_altitud_promedio_x_region["elevation"],
  main = "Altitud promedio de los puntos altos en cada región de NZ (1)",
  axes = TRUE,
  graticule = TRUE
)

nz_altitud_promedio_x_region <-
  nz %>%
  st_join(nz_height) %>%
  group_by(Name) %>%
  summarize(elevation = mean(elevation))  

# Mapa de altitud promedio en regiones
plot(
  nz_altitud_promedio_x_region["elevation"],
  main = "Altitud promedio de los puntos altos en cada región de NZ (2)",
  axes = TRUE,
  graticule = TRUE
)

# Transformación de la capa de provincias al CRS CRTM05 (EPSG = 5367)
provincias_crtm05 <-
  provincias %>%
  st_transform(5367)

# Centroide de la provincia de San José
centroide_sanjose <-
  provincias_crtm05 %>%
  filter(provincia == "San José") %>%
  st_centroid()

# Centroide la provincia de Alajuela
centroide_alajuela <-
  provincias_crtm05 %>%
  filter(provincia == "Alajuela") %>%
  st_centroid()

# Distancia entre los centroides de San José y Alajuela
st_distance(centroide_sanjose, centroide_alajuela)

# Mapa de los centroides
plot(
  provincias_crtm05$geometry,
  main = "Centroides de San José y Alajuela",
  axes = TRUE,
  graticule = TRUE,
  reset = FALSE
)
plot(centroide_sanjose$geometry, pch = 16, add = TRUE)
plot(centroide_alajuela$geometry, pch = 16, add = TRUE)


# Tabla de datos de registros de presencia
terciopelos %>%
  st_drop_geometry() %>%
  select(stateProvince,
         canton,
         locality,
         year
  ) %>%
  DT::datatable(
    colnames = c("Provincia", "Cantón", "Localidad", "Año"),
    rownames = FALSE,
    options = list(
      searchHighlight = TRUE
    )
  )

# Gráfico de registros de presencia por mes
terciopelos %>%
  st_drop_geometry() %>%
  group_by(mes = format(as.Date(eventDate, "%Y-%m-%d"), "%m")) %>%
  summarize(suma_registros = n()) %>%
  filter(!is.na(mes))  %>%
  plot_ly(x = ~ mes,
          y = ~ suma_registros) %>%
  layout(title = "Registros de terciopelos (Bothrops asper) por mes",
         xaxis = list(title = "Mes"),
         yaxis = list(title = "Cantidad de registros"))

# Mapa de registros de presencia
terciopelos %>%
  select(stateProvince,
         canton,
         locality,
         eventDate) %>%
  leaflet() %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Stamen Toner Lite") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Imágenes de ESRI") %>%
  addCircleMarkers(
    stroke = F,
    radius = 4,
    fillColor = 'green',
    fillOpacity = 1,
    popup = paste(
      terciopelos$stateProvince,
      terciopelos$canton,
      terciopelos$locality,
      terciopelos$eventDate,
      sep = '<br/>'
    ),
    group = "Terciopelos (Bothrops asper)"
  ) %>%
  addLayersControl(
    baseGroups = c("OpenStreetMap", "Stamen Toner Lite", "Imágenes de ESRI"),
    overlayGroups = c("Terciopelos (Bothrops asper)")
  ) %>%
  addMiniMap(
    tiles = providers$Stamen.OpenStreetMap.Mapnik,
    position = "bottomleft",
    toggleDisplay = TRUE
  )