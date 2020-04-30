# Este script es necesario porque México publica la actualización de sus datos a las 19 hrs. (CDMX), una hora después del corte del CSSE de Johns Hopkins

### Cargar paquetes, definir setup y tema de gráficas ----
source("02_codigo/00_paquetes_setup_tema.R") 

### Cargar funciones ----
source("02_codigo/01_funciones_limpieza_bd.R")

### Importar datos ----
datos <- 
  read_csv(str_c("04_datos_generados/actualizacion_diaria_st/serie_de_tiempo_diaria_por_pais_2020_04_", 
                 day(Sys.Date()) - 1, 
                 ".csv"))

datos %>% 
  filter(pais == "México") %>% 
  tail(n = 20)

### Corregir datos de México para algunos días ----
# datos <- 
#   datos %>%
#   mutate(casos_confirmados = ifelse(pais == "México" & fecha_corte <= as_date("2020-04-22"), lead(casos_confirmados), casos_confirmados),
#          muertes = ifelse(pais == "México" & fecha_corte <= as_date("2020-04-22"), lead(muertes), muertes),
#          recuperados = ifelse(pais == "México" & fecha_corte <= as_date("2020-04-22"), lead(recuperados), recuperados),
#          casos_confirmados = ifelse(pais == "México" & fecha_corte == as_date("2020-04-22"), 10544, casos_confirmados),
#          casos_confirmados = ifelse(pais == "México" & fecha_corte == as_date("2020-04-24"), 12872, casos_confirmados),
#          muertes = ifelse(pais == "México" & fecha_corte == as_date("2020-04-22"), 970, muertes),
#          muertes = ifelse(pais == "México" & fecha_corte == as_date("2020-04-24"), 1221, muertes),
#          cambio_diario_casos = ifelse(pais == "México", casos_confirmados - lag(casos_confirmados), cambio_diario_casos),
#          en_tratamiento = ifelse(pais == "México", casos_confirmados - muertes - recuperados, en_tratamiento),
#          por_casos_conf_fallecieron = ifelse(pais == "México", round(muertes/casos_confirmados*100, 1), por_casos_conf_fallecieron))