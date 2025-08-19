# Script elaborado por: geografia_en_viz
#
# Descripción:
# Este código R se utiliza para analizar y visualizar la relación entre la distribución
# de especies y variables ambientales como la elevación y la temperatura.
#
# Su uso potencial es crear perfiles de elevación y temperatura para diferentes especies,
# lo que es fundamental en estudios de biogeografía, como determinar sus nichos ecológicos
# y su distribución en relación con el relieve.

# --------------------------------------------------------------------------------------
# LIBRERÍAS NECESARIAS
# --------------------------------------------------------------------------------------
library(sf)
library(raster)
library(ggplot2)
library(dplyr)
library(stringr)

# --------------------------------------------------------------------------------------
# ANÁLISIS DE ELEVACIÓN
# --------------------------------------------------------------------------------------

# Cargar datos de especies y DEM.
# NOTA: Asegúrate de que las rutas de los archivos sean correctas en tu entorno.
# Al subir a GitHub, es recomendable usar rutas relativas si los datos están en el mismo repositorio.
# Por ejemplo: "data/phaseolus_sp.shp"
Phaseolus <- st_read("RUTA")
DEM <- raster("RUTA")
##file.choose #en caso de no saber como buscar la ruta
# Extraer elevación del DEM para cada punto de especie
Phaseolus$elevacion <- raster::extract(DEM, Phaseolus)

# Crear una versión abreviada del nombre de la especie
Phaseolus$abreviado <- str_trunc(Phaseolus$acceptedSc, 30)

# Calcular la media y el máximo de elevación por especie abreviada
medias <- Phaseolus %>%
  group_by(abreviado) %>%
  summarise(media_elev = mean(elevacion, na.rm = TRUE),
            max_elev = max(elevacion, na.rm = TRUE)) # Para posicionar las etiquetas de texto

# Generar el gráfico de caja (boxplot) de elevación
ggplot(Phaseolus, aes(x = abreviado, y = elevacion, fill = abreviado)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.5) +
  geom_text(data = medias,
            aes(x = abreviado, y = max_elev + 70, 
                label = paste0(round(media_elev, 0), " msnm")),
            color = "black", size = 3.5, fontface = "bold") +
  labs(title = "Elevación por especie de Phaseolus",
       x = "Especie (abreviada)",
       y = "Elevación (msnm)",
       caption = "Elaborado por: geografia_en_viz") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 9),
        legend.position = "none",
        plot.margin = margin(10, 20, 10, 20)) +
  scale_fill_brewer(palette = "Set3")

# Realizar una prueba de Kruskal-Wallis para elevación
kruskal.test(elevacion ~ acceptedSc, data = Phaseolus)

# Calcular el rango de altitud por especie
rango_alt <- Phaseolus %>%
  group_by(acceptedSc) %>%
  summarise(
    min = min(elevacion, na.rm = TRUE),
    max = max(elevacion, na.rm = TRUE),
    rango = max - min
  )
print(rango_alt)
