2143

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
  write_csv(str_c("04_datos_generados/actualizacion_diaria_st/serie_de_tiempo_diaria_por_pais_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".csv"))

covid_x_pais_total %>% 
  write_csv(str_c("04_datos_generados/reporte_diario_por_pais/reporte_diario_por_pais_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".csv"))

covid_x_pais_total %>% 
  write.xlsx(str_c("04_datos_generados/reporte_diario_por_pais/reporte_diario_por_pais_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".xlsx"))
