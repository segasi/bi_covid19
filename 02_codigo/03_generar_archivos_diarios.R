### Cargar paquetes, definir setup y tema de gráficas ----	2143
source("02_codigo/00_paquetes_setup_tema.R") 	

### Cargar funciones ----	
source("02_codigo/01_funciones_limpieza_bd.R")	


### Importar datos ----	

# Datos diarios por país	
covid_x_pais <- 	
  read_csv(str_c("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/04-",  day(Sys.Date()) - 1, "-2020.csv")) %>% 	
  clean_names()	


# Serie de tiempo de casos confirmados	
casos_x_pais_serie <- 	
  # read_csv("01_datos/coronavirus/time_series/time_series_19-covid-Confirmed.csv") %>%	
  read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") %>% 	
  pivot_longer(cols = -c(`Province/State`:Long),	
               names_to = "fecha_corte", 	
               values_to = "casos_confirmados") %>% 	
  clean_names()	

# Serie de tiempo de muertos	
muertos_x_pais_serie <- 	
  # read_csv("01_datos/coronavirus/time_series/time_series_19-covid-Deaths.csv") %>% 	
  read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") %>%	
  pivot_longer(cols = -c(`Province/State`:Long),	
               names_to = "fecha_corte", 	
               values_to = "muertes") %>% 	
  clean_names()	


# Serie de tiempo de recuperados	
recuperados_x_pais_serie <- 	
  # read_csv("01_datos/coronavirus/time_series/time_series_19-covid-Recovered.csv") %>% 	
  read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv") %>%	
  pivot_longer(cols = -c(`Province/State`:Long),	
               names_to = "fecha_corte", 	
               values_to = "recuperados") %>% 	
  clean_names()	



### Unir datos de series de tiempo ----	
covid_serie <- 	
  casos_x_pais_serie %>% 	
  # Con st de muertes	
  left_join(muertos_x_pais_serie %>% 	
              select(province_state, 	
                     country_region, 	
                     fecha_corte,	
                     muertes),	
            by = c("province_state", 	
                   "country_region", 	
                   "fecha_corte")) %>% 	
  # Con st de recuperados	
  left_join(recuperados_x_pais_serie %>% 	
              select(province_state, 	
                     country_region, 	
                     fecha_corte,	
                     recuperados),	
            by = c("province_state", 	
                   "country_region", 	
                   "fecha_corte")) %>% 	
  # Cambiar tipo a fecha_corte	
  mutate(fecha_corte = mdy(fecha_corte))	



### "Limpiar" nombres de países ----	
covid_x_pais <- limpiar_nombres_paises(covid_x_pais)	
covid_serie <- limpiar_nombres_paises(covid_serie) 	



### Renombrar columnas ----	
covid_x_pais <- 	
  covid_x_pais %>% 	
  rename(provincia_estado = province_state,	
         pais = country_region,	
         ultima_actualizacion = last_update,	
         casos_confirmados = confirmed,	
         muertes = deaths,	
         recuperados = recovered)	

covid_serie <- 	
  covid_serie %>% 	
  rename(provincia_estado = province_state,	
         pais = country_region)	


### Revisar nombre de países ----	
covid_x_pais %>% 	
  count(pais) %>% 	
  print(n = Inf)

covid_serie %>% 
  count(pais) %>% 
  print(n = Inf)


### Revisar última fecha ----

max(covid_serie$fecha_corte, na.rm = T)


### Calcular totales por país ----
covid_x_pais_total <- 
  covid_x_pais %>% 
  group_by(pais) %>% 
  summarise_at(vars(casos_confirmados, muertes, recuperados), 
               list(~ sum(., na.rm = TRUE))) %>% 
  ungroup() %>% 
  arrange(-casos_confirmados)

### Calcular totales por país y fecha ----
covid_serie_pais_dia <- 
  covid_serie %>% 
  group_by(pais, fecha_corte) %>% 
  summarise_at(vars(casos_confirmados, muertes, recuperados), 
               list(~ sum(., na.rm = TRUE))) %>% 
  ungroup() %>% 
  arrange(-casos_confirmados)


  

### Reemplazar valores de fechas en las que Hopkins no actualizó los datos ----  
covid_serie_pais_dia <- 
  covid_serie_pais_dia %>% 
  # Ordenar por país y fecha de corte
  arrange(pais, fecha_corte) %>% 
  # Número de casos nuevos al día por país
  group_by(pais) %>%
  mutate(total_casos = max(casos_confirmados)) %>% 
  ungroup() %>% 
  mutate(casos_confirmados = case_when(pais == "Italia" & casos_confirmados == 12462 & fecha_corte == as_date("2020-03-12") ~ 15113, # Fuentes: https://ourworldindata.org/coronavirus y http://opendatadpc.maps.arcgis.com/apps/opsdashboard/index.html#/b0c68bce2cce478eaac82fe38d4138b1
                                       pais == "Francia" & casos_confirmados == 2289 & fecha_corte == as_date("2020-03-12") ~ 2860, 
                                       # Fuente: https://ourworldindata.org/coronavirus
                                       pais == "Suiza" & casos_confirmados == 652 & fecha_corte == as_date("2020-03-12") ~ 858, 
                                       # Fuente: https://ourworldindata.org/coronavirus
                                       pais == "Reino Unido" & casos_confirmados == 459 & fecha_corte == as_date("2020-03-12") ~ 594, 
                                       # Fuente: https://ourworldindata.org/coronavirus
                                       pais == "Holanda" & casos_confirmados == 503 & fecha_corte == as_date("2020-03-12") ~ 614, 
                                       # Fuente: https://ourworldindata.org/coronavirus
                                       pais == "Japón" & casos_confirmados == 639 & fecha_corte == as_date("2020-03-12") ~ 675, 
                                       # Fuente: https://ourworldindata.org/coronavirus
                                       pais == "España" & casos_confirmados == 2277 & fecha_corte == as_date("2020-03-12") ~ 2965,
                                       TRUE ~ casos_confirmados)) %>% 
  arrange(pais, fecha_corte) %>% 
  # Número de casos nuevos al día por país
  group_by(pais) %>%
  mutate(cambio_diario_casos = casos_confirmados - lag(casos_confirmados)) %>% 
  ungroup() %>% 
  # filter(pais == "Francia") %>% 
  # arrange(-total_casos, desc(fecha_corte))
  arrange(-total_casos, fecha_corte)


### Calcular nuevas variables ----
covid_serie_pais_dia <- 
  covid_serie_pais_dia %>% 
  mutate(en_tratamiento = casos_confirmados - muertes - recuperados, 
         por_casos_conf_fallecieron = round((muertes/casos_confirmados)*100, 2))

covid_x_pais_total <- 
  covid_x_pais_total %>% 
  mutate(en_tratamiento = casos_confirmados - muertes - recuperados, 
         por_casos_conf_fallecieron = round((muertes/casos_confirmados)*100, 2),
         actualizacion = Sys.Date()) 

### Salvar archivos ----
covid_serie_pais_dia %>% 
  arrange(pais, fecha_corte) %>% 
  write_csv(str_c("04_datos_generados/actualizacion_diaria_st/serie_de_tiempo_diaria_por_pais_", str_replace_all(str_replace_all(str_replace_all(Sys.Date() - 1, "\\:", "_"), "-", "_"), " ", "_"),".csv"))

covid_x_pais_total %>% 
  write_csv(str_c("04_datos_generados/reporte_diario_por_pais/reporte_diario_por_pais_", str_replace_all(str_replace_all(str_replace_all(Sys.Date() - 1, "\\:", "_"), "-", "_"), " ", "_"),".csv"))

covid_x_pais_total %>% 
  write.xlsx(str_c("04_datos_generados/reporte_diario_por_pais/reporte_diario_por_pais_", str_replace_all(str_replace_all(str_replace_all(Sys.Date() - 1, "\\:", "_"), "-", "_"), " ", "_"),".xlsx"))
