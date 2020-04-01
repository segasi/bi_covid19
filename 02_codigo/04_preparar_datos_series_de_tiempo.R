# Este script es necesario porque México publica la actualización de sus datos a las 19 hrs. (CDMX), una hora después del corte del CSSE de Johns Hopkins

### Cargar paquetes, definir setup y tema de gráficas ----
source("02_codigo/00_paquetes_setup_tema.R") 

### Cargar funciones ----
source("02_codigo/01_funciones_limpieza_bd.R")

### Importar datos ----
datos <- 
  read_csv(str_c("04_datos_generados/actualizacion_diaria_st/serie_de_tiempo_diaria_por_pais_2020_03_", 
                 day(Sys.Date()), 
                 ".csv"))

### Actualizar datos de México ----
datos <- 
  datos %>% 
  mutate(casos_confirmados = ifelse(pais == "México", lead(casos_confirmados), casos_confirmados),
         muertes = ifelse(pais == "México", lead(muertes), muertes),
         recuperados = ifelse(pais == "México", lead(recuperados), recuperados),
         cambio_diario_casos = ifelse(pais == "México", lead(cambio_diario_casos), cambio_diario_casos),
         en_tratamiento = ifelse(pais == "México", lead(en_tratamiento), en_tratamiento),
         por_casos_conf_fallecieron = ifelse(pais == "México", lead(por_casos_conf_fallecieron), por_casos_conf_fallecieron)) %>% 
  mutate(casos_confirmados = ifelse(fecha_corte == max(fecha_corte) & pais == "México", 1215, casos_confirmados),
         muertes = ifelse(fecha_corte == max(fecha_corte) & pais == "México", 29, muertes),
         recuperados = ifelse(fecha_corte == max(fecha_corte) & pais == "México", 35, recuperados),
         total_casos = ifelse(pais == "México", 1215, total_casos), 
         cambio_diario_casos = ifelse(pais == "México", casos_confirmados - lag(casos_confirmados), cambio_diario_casos),
         en_tratamiento = ifelse(pais == "México", casos_confirmados - muertes - recuperados, en_tratamiento),
         por_casos_conf_fallecieron = ifelse(pais == "México", round(muertes/casos_confirmados*100, 1), por_casos_conf_fallecieron)) 

datos %>% 
  filter(pais == "México") %>% 
  tail()
