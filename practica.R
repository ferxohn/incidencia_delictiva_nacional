# Práctica 1: Incidencia Delictiva Nacional
#
# Objetivo de la práctica: Construir mapas temáticos que muestren la incidencia
# delictiva en los estados de la República Mexicana durante los últimos años.
library(fs)
library(readxl)
library(tidyr)
library(dplyr)
library(stringr)
library(sf)
library(mapsf)
library(maptiles)

# Incidencia delictiva del fuero común
# Fuente: Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública
seg_pub <- read_excel(path("extdata", "Estatal-Delitos-2015-2021_feb2021", ext = "xlsx")) %>%
  mutate(across(Clave_Ent, as.character)) %>%
  mutate(across(Clave_Ent, ~ str_pad(.x, width = 2, side = "left", pad = "0"))) %>%
  mutate(across(where(is.numeric), as.integer))

# Tamaño de población por estado
# Fuente: INEGI Encuesta Intercensal 2015
inegi <- read_excel(path("extdata", "01_poblacion", ext = "xls"), sheet = 4, range = "A7:E3573")

# Polígonos estatales
# Fuente: CONABIO
unzip(path("extdata", "destdv250k_2gw", ext = "zip"), exdir = tempdir())

polig_est <- read_sf(path(tempdir(), "destdv250k_2gw", "destdv250k_2gw", ext = "shp")) %>%
  select(-one_of("AREA", "PERIMETER", "COV_", "COV_ID", "CAPITAL", "RASGO_GEOG")) %>%
  rename(Entidad = ENTIDAD, Clave_Ent = NUM_EDO) %>%
  group_by(Clave_Ent, Entidad) %>%
  summarise(geometry = st_combine(geometry)) %>%
  mutate(across(Entidad, str_to_title, locale = "es"))

# Filtro: Homicidios dolosos por arma de fuego
# Datos agrupados por año y por entidad
hom_dol_arma_fuego <- seg_pub %>%
  # Obtención de los homicidios dolosos por arma de fuego
  filter(`Subtipo de delito` == "Homicidio doloso", Modalidad == "Con arma de fuego") %>%
  select(-one_of("Bien jurídico afectado", "Tipo de delito", "Subtipo de delito", "Modalidad")) %>%
  pivot_longer(
    -one_of("Año", "Clave_Ent", "Entidad"),
    names_to = "Mes",
    values_to = "Total"
  ) %>%
  # Agrupación de los datos por año y por entidad
  group_by(`Año`, Clave_Ent, Entidad) %>%
  summarise(across(Total, sum)) %>%
  rename(Hom_Dol_Arma_Fuego = Total)

# Filtro: Tamaño de población por entidad
tam_pob <- inegi %>%
  filter(
    `Entidad federativa` != "Estados Unidos Mexicanos",
    Sexo == "Total",
    `Grandes grupos de edad` == "Total",
    Estimador == "Valor"
  ) %>%
  separate(`Entidad federativa`, into = c("Clave_Ent", "Entidad"), sep = 3) %>%
  mutate(across(Clave_Ent, str_trim)) %>%
  rename(Poblacion = `Población total`) %>%
  select(-one_of("Sexo", "Grandes grupos de edad", "Estimador"))

# Cálculo de la tasa de homicidios dolosos con arma de fuego por entidad por año
tasa_hom <- hom_dol_arma_fuego %>%
  full_join(tam_pob, by = "Clave_Ent") %>%
  mutate(Tasa_Hom = Hom_Dol_Arma_Fuego / Poblacion * 100000) %>%
  select(`Año`, Clave_Ent, Tasa_Hom)

# Unión con los poligonos estatales
map_tasa_hom <- polig_est %>%
  ungroup() %>%
  full_join(tasa_hom, by = "Clave_Ent") %>%
  select(`Año`, Clave_Ent, Entidad, Tasa_Hom)

### Creación de los mapas temáticos ###

# Mapa base de OpenStreetMap
mex.osm <- get_tiles(polig_est, zoom = 5, crop = TRUE)

# División de la información por años
tasa_hom_anio <- list(
  "2016" = filter(map_tasa_hom, `Año` == 2016),
  "2015" = filter(map_tasa_hom, `Año` == 2015),
  "2017" = filter(map_tasa_hom, `Año` == 2017),
  "2018" = filter(map_tasa_hom, `Año` == 2018),
  "2019" = filter(map_tasa_hom, `Año` == 2019),
  "2020" = filter(map_tasa_hom, `Año` == 2020)
)

# Cortes y paleta de colores para el Choropleth
pal <- mf_get_pal(10, palette = "Reds", rev = TRUE)
breaks <- seq(0, 100, by = 10)

# Creación del PDF con los 5 mapas temáticos
pdf("inc_delic_anio_mex.pdf", width = 7, height = 5.3)
for (info_anio in tasa_hom_anio) {
  # Creación del mapa temático
  mf_init(info_anio)
  plot_tiles(mex.osm, add = TRUE)
  mf_map(
    info_anio,
    var = "Tasa_Hom",
    type = "choro",
    breaks = breaks,
    pal = pal,
    border = "black",
    lwd = 0.2,
    leg_pos = "n",
    add = TRUE
  )
  mf_legend_c(
    pos = "left",
    val = breaks,
    pal = pal,
    title = "Tasa de\nhomicidios",
    title_cex = 0.6,
    val_cex = 0.5,
    val_rnd = 0,
    frame = TRUE,
    cex = 0.6
  )
  mf_layout(
    title = paste0("Tasa de homicidios dolosos por arma de fuego (", unique(info_anio$`Año`), ")"),
    credits = paste0("Por: Fernando Gómez Perera, usando mapsf ", packageVersion("mapsf"), "\n",
                     "Fuentes: INEGI (2016) y Secretariado Ejecutivo del SNSP (2021)\n",
                     get_credit("OpenStreetMap")),
    arrow = TRUE,
    frame = TRUE
  )
}
dev.off()
