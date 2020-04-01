### Cargar paquetes, definir setup y tema de gráficas ----
source("02_codigo/00_paquetes_setup_tema.R") 

### Definir cortes de datos ----
source("02_codigo/02_cortes_datos.R")

### Generar folder para guardar las gráficas ----
dir_graficas <- 
  dir.create(file.path("03_graficas/01_graficas_analisis_mundial/", 
                       str_c("graficas_", str_replace_all(Sys.Date(), "-", "_"))))
ruta_graficas_global <- str_c("03_graficas/01_graficas_analisis_mundial/", 
                       str_c("graficas_", str_replace_all(Sys.Date(), "-", "_"), "/"))

### Importar datos ----

## Datos usados en gráficas 01 a 10 ----
novel <- 
  read_excel("01_datos/covid19_sars/datos_coronavirus.xlsx", 
             sheet = "novel_coronavirus") %>% 
  mutate(fecha = as_date(fecha))

sars <- 
  read_excel("01_datos/covid19_sars/datos_coronavirus.xlsx", 
             sheet = "sars") %>% 
  mutate(fecha = as_date(fecha))

novel_x_pais <- 
  read_csv(str_c("04_datos_generados/reporte_diario_por_pais/reporte_diario_por_pais_2020_03_",
                 day(Sys.Date()), 
                 ".csv")) %>% 
  clean_names() 

## Datos usados en gráficas 11 a XX ----
source("02_codigo/04_preparar_datos_series_de_tiempo.R")


### Preparar datos ----

## Para las gráficas 01 a 10 ----
novel_x_pais <- 
  novel_x_pais %>% 
  mutate(casos_confirmados = ifelse(pais == "México", 1215, casos_confirmados),
         recuperados = ifelse(pais == "México", 35, recuperados),
         muertes = ifelse(pais == "México", 29, muertes)) %>%  
  mutate(en_tratamiento = casos_confirmados - muertes - recuperados, 
         por_casos_conf_fallecieron = round((muertes/casos_confirmados)*100, 2))

novel_x_pais %>% 
  filter(pais == "México") %>% 
  tail()

# Calcular días desde que se cree inició el brote ----
novel <- 
  novel %>% 
  mutate(dias_brote = as.numeric(fecha - as_date("2019-12-08"))) %>% # Fuente: https://www.bbc.com/news/51245373
  select(fecha, dias_brote, everything())

sars <- 
  sars %>% 
  mutate(dias_brote = as.numeric(fecha - as_date("2002-11-01"))) %>% 
  select(fecha, dias_brote, everything())

# Calcular el porcentaje de casos confirmados que murieron ----
datos_novel <- 
  novel %>% 
  tail(n = 1) %>% 
  mutate(por_casos_conf_fallecieron = round(muertes/casos_totales, 4)*100)

datos_sars <- 
  sars %>% 
  tail(n = 1) %>% 
  mutate(por_casos_conf_fallecieron = round(muertes/casos_totales, 4)*100)



### Gráfica 01: Número de casos confirmados de COVID-19 y SARS ----
sars %>% 
  ggplot(aes(x = dias_brote, y = casos_totales)) +
  geom_line(color = "salmon", size = 1) +
  geom_line(data = novel, aes(x = dias_brote, y = casos_totales), color = "steelblue", size = 1) +
  ggplot2::annotate(geom = "text", x = datos_novel$dias_brote + 17, y = max(novel$casos_totales), label = "COVID-19", color = "steelblue", size = 8, hjust = 0.5) +
  ggplot2::annotate(geom = "text", x = datos_novel$dias_brote + 17, y = max(novel$casos_totales) - max(novel$casos_totales) * 0.7, label = str_c("Casos: ", comma(datos_novel$casos_totales), "\nMuertos: ", comma(datos_novel$muertes)), color = "grey50", size = 5, hjust = 0.5) +
  ggplot2::annotate(geom = "text", x = 255, y = 65000, label = "SARS", color = "salmon", size = 8, hjust = 0.5) +
  ggplot2::annotate(geom = "text", x = 255, y = 30000, label = "(2002-2003)", color = "salmon", size = 6, hjust = 0.5) +
  ggplot2::annotate(geom = "text", x = 255, y = 15000, label = str_c("Casos: ", comma(datos_sars$casos_totales), " | Muertos: ", comma(datos_sars$muertes)), color = "grey50", size = 5, hjust = 0.5) +
  scale_x_continuous(limits = c(0, 275), breaks = seq(0, 275, 25)) +
  scale_y_log10(labels = comma_format(accuracy = 1), 
                breaks = c(1, 3, 10, 30, 100, 300, 1000, 3e3, 10e3, 3e4, 10e4, 3e5, 10e5, 3e6, 10e6, 3e7, 10e7)) + 
  labs(title = "Número de casos confirmados reportados de COVID-19 y SARS desde el comienzo\nde la respectiva epidemia",
       subtitle = str_c(subtitulo_lineas, " | Distancia logarítmica en las etiquetas del eje vertical"),
       x = "\nDías transcurridos desde la fecha en que      \nse considera que inició el brote      ",
       y = "Número (log 10)\n",
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuentes: OMS y el CSSE de la Universidad de Johns Hopkins\n\nNota: En el caso del brote de SARS, la OMS inicialmente consideró que éste comenzó el 1 de febrero de 2003, pero el 28 de marzo de ese año determinó que la epidemia\nhabía iniciado el 1 de noviembre de 2002. En el caso del COVID-19, se considera que el brote inició el 8 de diciembre de 2019.") +
  tema +
  theme(plot.caption = element_text(size = 14),
        axis.title.y = element_text(angle = 90)) +
  ggsave(str_c(ruta_graficas_global, "01_casos_confirmados_covid19_sars_log10_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16.2, height = 9)

### Gráfica 02_01: Número de casos confirmados de COVID-19 ----
novel %>% 
  ggplot(aes(x = dias_brote, y = casos_totales)) +
  geom_line(aes(x = dias_brote, y = casos_totales), color = "steelblue", size = 1) +
  ggplot2::annotate(geom = "text", x = datos_novel$dias_brote + 8, y = max(novel$casos_totales), label = "COVID-19", color = "steelblue", size = 8, hjust = 0.5) +
  ggplot2::annotate(geom = "text", x = datos_novel$dias_brote + 8, y = max(novel$casos_totales) - max(novel$casos_totales) * 0.1, label = str_c("Casos: ", comma(datos_novel$casos_totales), "\nMuertos: ", comma(datos_novel$muertes)), color = "grey50", size = 5, hjust = 0.5) +
  scale_x_continuous(limits = c(0, 130), breaks = seq(0, 300, 10)) +
  scale_y_continuous(breaks = c(seq(0, 1.5e6, 100000)), labels = comma) +
  labs(title = "Número de casos confirmados reportados de COVID-19 desde el comienzo\nde la epidemia",
       subtitle = subtitulo_lineas,
       x = "\nDías transcurridos desde la fecha en que      \nse considera que inició el brote      ",
       y = "Número\n",
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuentes: OMS y el CSSE de la Universidad de Johns Hopkins\n\nNota: Se considera que el brote del COVID-19 inició el 8 de diciembre de 2019.") +
  tema +
  theme(plot.caption = element_text(size = 14)) +
  ggsave(str_c(ruta_graficas_global, "02_01_casos_confirmados_covid19_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)


### Gráfica 02_02: Número de casos confirmados de COVID-19, log 10 ----
novel %>% 
  ggplot(aes(x = dias_brote, y = casos_totales)) +
  geom_line(aes(x = dias_brote, y = casos_totales), color = "steelblue", size = 1) +
  ggplot2::annotate(geom = "text", x = datos_novel$dias_brote + 8, y = max(novel$casos_totales), label = "COVID-19", color = "steelblue", size = 8, hjust = 0.5) +
  ggplot2::annotate(geom = "text", x = datos_novel$dias_brote + 8, y = max(novel$casos_totales) - max(novel$casos_totales) * 0.7, label = str_c("Casos: ", comma(datos_novel$casos_totales), "\nMuertos: ", comma(datos_novel$muertes)), color = "grey50", size = 5, hjust = 0.5) +
  scale_x_continuous(limits = c(0, 130), breaks = seq(0, 300, 10)) +
  scale_y_log10(labels = comma_format(accuracy = 1), 
                breaks = c(1, 3, 10, 30, 100, 300, 1000, 3e3, 10e3, 3e4, 10e4, 3e5, 10e5, 3e6, 10e6, 3e7, 10e7)) + 
  labs(title = "Número de casos confirmados reportados de COVID-19 desde el comienzo\nde la epidemia",
       subtitle = str_c(subtitulo_lineas, " | Distancia logarítmica en las etiquetas del eje vertical"),
       x = "\nDías transcurridos desde la fecha en que      \nse considera que inició el brote      ",
       y = "Número (log 10)\n",
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuentes: OMS y el CSSE de la Universidad de Johns Hopkins\n\nNota: Se considera que el brote del COVID-19 inició el 8 de diciembre de 2019.") +
  tema +
  theme(plot.caption = element_text(size = 14)) +
  ggsave(str_c(ruta_graficas_global, "02_02_casos_confirmados_covid19_log10_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)


### Gráfica 03_01: Número de muertes ----
sars %>% 
  ggplot(aes(x = dias_brote, y = muertes)) +
  geom_line(color = "salmon", size = 1) +
  geom_line(data = novel, aes(x = dias_brote, y = muertes), color = "steelblue", size = 1) +
  ggplot2::annotate(geom = "text", x = datos_novel$dias_brote + 17, y = max(novel$muertes), label = "COVID-19", color = "steelblue", size = 8, hjust = 0.5) +
  ggplot2::annotate(geom = "text", x = datos_novel$dias_brote + 17, y = max(novel$muertes) - max(novel$muertes) * 0.1, label = str_c("Casos: ", comma(datos_novel$casos_totales), "\nMuertos: ", comma(datos_novel$muertes)), color = "grey50", size = 5, hjust = 0.5) +
  ggplot2::annotate(geom = "text", x = 255, y = 6000, label = "SARS", color = "salmon", size = 8, hjust = 0.5) +
  ggplot2::annotate(geom = "text", x = 255, y = 4000, label = "(2002-2003)", color = "salmon", size = 6, hjust = 0.5) +
  ggplot2::annotate(geom = "text", x = 255, y = 2050, label = str_c("Casos: ", comma(datos_sars$casos_totales), " | Muertos: ", comma(datos_sars$muertes)), color = "grey50", size = 5, hjust = 0.5) +
  scale_x_continuous(limits = c(0, 275), breaks = seq(0, 300, 25)) +
  scale_y_continuous(breaks = c(seq(0, 40000, 5000)), labels = comma) +
  labs(title = "Número de muertes provocadas por COVID-19 y SARS desde el comienzo de la\nrespectiva epidemia",
       subtitle = subtitulo_lineas,
       x = "\nDías transcurridos desde la fecha en que      \nse considera que inició el brote      ",
       y = "Número de muertes\n",
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuentes: OMS y el CSSE de la Universidad de Johns Hopkins\n\nNota: En el caso del brote de SARS, la OMS inicialmente consideró que éste comenzó el 1 de febrero de 2003, pero el 28 de marzo de ese año determinó que la epidemia\nhabía iniciado el 1 de noviembre de 2002. En el caso del COVID-19, se considera que el brote inició el 8 de diciembre de 2019.") +
  tema +
  theme(plot.caption = element_text(size = 14)) +
  ggsave(str_c(ruta_graficas_global, "03_01_muertes_novel_sars_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)

### Gráfica 03_02: Número de muertes log 10 ----
sars %>% 
  ggplot(aes(x = dias_brote, y = muertes)) +
  geom_line(color = "salmon", size = 1) +
  geom_line(data = novel, aes(x = dias_brote, y = muertes), color = "steelblue", size = 1) +
  ggplot2::annotate(geom = "text", x = datos_novel$dias_brote + 17, y = max(novel$muertes), label = "COVID-19", color = "steelblue", size = 8, hjust = 0.5) +
  ggplot2::annotate(geom = "text", x = datos_novel$dias_brote + 17, y = max(novel$muertes) - max(novel$muertes) * 0.62, label = str_c("Casos: ", comma(datos_novel$casos_totales), "\nMuertos: ", comma(datos_novel$muertes)), color = "grey50", size = 5, hjust = 0.5) +
  ggplot2::annotate(geom = "text", x = 255, y = 4300, label = "SARS", color = "salmon", size = 8, hjust = 0.5) +
  ggplot2::annotate(geom = "text", x = 255, y = 2200, label = "(2002-2003)", color = "salmon", size = 6, hjust = 0.5) +
  ggplot2::annotate(geom = "text", x = 255, y = 1250, label = str_c("Casos: ", comma(datos_sars$casos_totales), " | Muertos: ", comma(datos_sars$muertes)), color = "grey50", size = 5, hjust = 0.5) +
  scale_x_continuous(limits = c(0, 275), breaks = seq(0, 300, 25)) +
  scale_y_log10(labels = comma_format(accuracy = 1), 
                # limits = c(1, 6e5), 
                breaks = c(1, 3, 10, 30, 100, 300, 1000, 3e3, 10e3, 3e4, 10e4, 3e5, 10e5, 3e6, 10e6, 3e7, 10e7)) +
  labs(title = "Número de muertes provocadas por COVID-19 y SARS desde el comienzo de la\nrespectiva epidemia",
       subtitle = str_c(subtitulo_lineas, " | Distancia logarítmica en las etiquetas del eje vertical"),
       x = "\nDías transcurridos desde la fecha en que      \nse considera que inició el brote      ",
       y = "Número (log 10)\n",
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuentes: OMS y el CSSE de la Universidad de Johns Hopkins\n\nNota: En el caso del brote de SARS, la OMS inicialmente consideró que éste comenzó el 1 de febrero de 2003, pero el 28 de marzo de ese año determinó que la epidemia\nhabía iniciado el 1 de noviembre de 2002. En el caso del COVID-19, se considera que el brote inició el 8 de diciembre de 2019.") +
  tema +
  theme(plot.caption = element_text(size = 14)) +
  ggsave(str_c(ruta_graficas_global, "03_02_muertes_novel_sars_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)

### Gráfica 04: Porcentaje de casos confirmados que murieron  ----
sars %>% 
  ggplot(aes(x = dias_brote, y = muertes/casos_totales*100)) +
  geom_line(color = "salmon", size = 1) +
  geom_line(data = novel, aes(x = dias_brote, y = muertes/casos_totales*100), color = "steelblue", size = 1) +
  ggplot2::annotate(geom = "text", x = datos_novel$dias_brote + 17, y = max(novel$muertes/novel$casos_totales*100), label = "COVID-19", color = "steelblue", size = 8, hjust = 0.5) +
  ggplot2::annotate(geom = "text", x = datos_novel$dias_brote + 17, y = max(novel$muertes/novel$casos_totales*100) - max(novel$muertes/novel$casos_totales*100) * 0.14, label = str_c("Porcentaje: ", datos_novel$por_casos_conf_fallecieron), color = "grey50", size = 5, hjust = 0.5) +
  ggplot2::annotate(geom = "text", x = 255, y = 9, label = "SARS", color = "salmon", size = 8, hjust = 0.5) +
  ggplot2::annotate(geom = "text", x = 255, y = 8.2, label = "(2002-2003)", color = "salmon", size = 6, hjust = 0.5) +
  ggplot2::annotate(geom = "text", x = 255, y = 7.5,label = str_c("Porcentaje: ", datos_sars$por_casos_conf_fallecieron), color = "grey50", size = 5, hjust = 0.5) +
  scale_x_continuous(limits = c(0, 275), breaks = seq(0, 300, 25)) +
  scale_y_continuous(breaks = 0:10, labels = comma, limits = c(0, 10.1)) +
  labs(title = "Porcentaje de casos confirmados* de Covid-19 y SARS que murieron desde el\ncomienzo de la respectiva epidemia",
       subtitle = subtitulo_lineas,
       x = "\nDías transcurridos desde la fecha en que      \nse considera que inició el brote      ",
       y = "Porcentaje  \n",
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuentes: OMS y el CSSE de la Universidad de Johns Hopkins\n\nNotas: *La categoría \"casos confirmados\"se refiere a las personas que dieron positivo en la prueba de Covid-19. La variable graficada fue calculada dividiendo el número\nde muertes entre el número de casos confirmados, por 100. En el caso del brote de SARS, la OMS inicialmente consideró que éste comenzó el 1 de febrero de 2003, pero\nel 28 de marzo de ese año determinó que la epidemia había iniciado el 1 de noviembre de 2002. En el caso del Covid-19, se considera que el brote inició el 8 de\ndiciembre de 2019.") +
  tema +
  theme(plot.caption = element_text(size = 14)) +
  ggsave(str_c(ruta_graficas_global, "04_por_casos_conf_fallecieron_covid19_sars_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)


### Gráfica 05: Distribución de numero de casos confirmados y reportados de COVID-19 en en cada país y territorio ----
novel_x_pais %>% 
  summarise(subtotal_casos = sum(casos_confirmados))

novel_x_pais %>% 
  count(pais) %>% 
  print(n = Inf)

novel_x_pais %>% 
  ggplot(aes(area = casos_confirmados, fill = log(casos_confirmados))) +
  geom_treemap(col = "white") +
  geom_treemap_text(aes(label = pais), fontface = "bold", color = "white", alpha = 1, min.size = 0, grow = F) +
  geom_treemap_text(aes(label = paste(comma(casos_confirmados, accuracy = 1), "casos", sep = " ")), color = "white", padding.y = unit(7, "mm"),min.size = 0) +
  geom_treemap_text(aes(label = paste(comma(casos_confirmados/sum(casos_confirmados)*100, accuracy = 1), "% de los casos", sep = "")), color = "white", padding.y = unit(14, "mm"), min.size = 0, size = 14) +
  scale_fill_gradient(low = "grey95", high = "#f85441", guide = guide_colorbar(barwidth = 18, nbins = 6), labels = comma, breaks = pretty_breaks(n = 6)) +
  labs(title = "Número de casos confirmados y reportados de COVID-19 en cada\npaís y territorio",
       subtitle = subtitulo_treemaps,
       x = NULL,
       y = NULL,
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuentes: OMS y el CSSE de la Universidad de Johns Hopkins") +
  tema +
  theme(legend.position = "none", 
        plot.title = element_text(size = 29),
        plot.subtitle = element_text(size = 20)) +
  ggsave(str_c(ruta_graficas_global, "05_casos_confirmados_covid19_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 13, height = 9)


### Gráfica 06: Distribución de número de muertes provocadas por el COVID-19 en cada país y territorio ----
novel_x_pais %>% 
  ggplot(aes(area = muertes, fill = (muertes), subgroup = pais, label = pais)) +
  geom_treemap(col = "white") +
  geom_treemap_text(aes(label = pais), fontface = "bold", color = "white", alpha = 1, min.size = 0, grow = F) +
  geom_treemap_text(aes(label = paste(comma(muertes, accuracy = 1), "muerto(s)", sep = " ")), color = "white", padding.y = unit(7, "mm"),min.size = 0) +
  geom_treemap_text(aes(label = paste(comma(muertes/sum(muertes)*100, accuracy = 1), "% de las muertes", sep = "")), color = "white", padding.y = unit(14, "mm"), min.size = 0, size = 14) + 
  scale_fill_gradient(low = "grey95", high = "grey10", guide = guide_colorbar(barwidth = 18, nbins = 6), labels = comma, breaks = pretty_breaks(n = 6)) +
  labs(title = "Muertes provocadas por el COVID-19 en cada país y territorio",
       subtitle = subtitulo_treemaps,
       x = NULL,
       y = NULL,
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuentes: OMS y el CSSE de la Universidad de Johns Hopkins") +
  tema +
  theme(legend.position = "none", 
        plot.title = element_text(size = 29),
        plot.subtitle = element_text(size = 20)) +
  ggsave(str_c(ruta_graficas_global, "06_muertes_covid19_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 13, height = 9)


### Gráfica 07: Distribución del número de pacientes que se recuperaron después de haber enfermado por el COVID-19 en países y territorios ----
novel_x_pais %>% # 
  ggplot(aes(area = recuperados, fill = log(recuperados))) +
  geom_treemap(col = "white") +
  geom_treemap_text(aes(label = pais), fontface = "bold", color = "white", alpha = 1, min.size = 0, grow = F) +
  geom_treemap_text(aes(label = paste(comma(recuperados, accuracy = 1), "recuperado(s)", sep = " ")), color = "white", padding.y = unit(7, "mm"),min.size = 0) +
  geom_treemap_text(aes(label = paste(comma(recuperados/sum(recuperados)*100, accuracy = 1), "% de las personas recuperadas", sep = "")), color = "white", padding.y = unit(14, "mm"), min.size = 0, size = 14) + 
  scale_fill_gradient(low = "grey95", high = "#41ab5d", guide = guide_colorbar(barwidth = 18, nbins = 6), labels = comma, breaks = pretty_breaks(n = 6)) +
  labs(title = "Pacientes que se recuperaron después de haber enfermado por el\nCOVID-19 en cada país y territorio",
       subtitle = subtitulo_treemaps,
       x = NULL,
       y = NULL,
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuentes: OMS y el CSSE de la Universidad de Johns Hopkins") +
  tema +
  theme(legend.position = "none", 
        plot.title = element_text(size = 29),
        plot.subtitle = element_text(size = 20)) +
  ggsave(str_c(ruta_graficas_global, "07_recuperados_covid19_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 13, height = 9)


### Gráfica 08: Estatus de los casos confirmados de COVID-19 en cada país y territorio ----
novel_x_pais %>%
  mutate(pais = ifelse(str_detect(pais, "Crucero"), "Crucero", pais)) %>% 
  # filter(pais != "China") %>%  
  rename(Muertes = muertes,
         Recuperados = recuperados,
         `En tratamiento` = en_tratamiento) %>% 
  select(-c(por_casos_conf_fallecieron, actualizacion)) %>% 
  pivot_longer(cols = Muertes:`En tratamiento`, 
               names_to = "estatus", 
               values_to = "numero") %>%
  group_by(pais, estatus) %>% 
  summarise(numero = sum(numero)) %>% 
  ungroup() %>% 
  group_by(pais) %>% 
  mutate(casos_confirmados = sum(numero),
         porcentaje = round((numero/casos_confirmados)*100, 1)) %>% 
  ungroup() %>% 
  mutate(porcentaje = round((numero/casos_confirmados)*100, 1), 
         num_casos = ifelse(casos_confirmados == max(casos_confirmados), 
                            str_c(comma(casos_confirmados), " casos"),
                            comma(casos_confirmados))) %>% 
  filter(casos_confirmados >= 1000) %>%
  ggplot(aes(x = fct_reorder(pais, casos_confirmados), 
             y = porcentaje,
             fill = estatus)) +
  geom_col() +
  geom_hline(yintercept = seq(10, 90, 10), linetype = 3, color = "white", size = 0.5) +
  geom_text(aes(x = fct_reorder(pais, casos_confirmados),
                label = num_casos),
            y = 101,
            hjust = 0,
            family = "Roboto", color = "grey35", 
            size = 5) +
  scale_x_discrete(expand = c(0.01, 0.01)) + 
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(-0.5, 109),
                     breaks = seq(0, 100, 10)) +
  scale_fill_manual(values = c("grey80", "#d73027", "#1a9850"),
                    labels = c("En tratamiento", "Fallecidos", "Recuperados")) +
  coord_flip() +
  labs(title = "Estatus de los casos confirmados de COVID-19 en cada país y territorio",
       subtitle = subtitulo_treemaps,
       x = NULL,
       y = "\nPorcentaje                       ",
       fill = NULL,
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuente: CSSE de la Universidad de Johns Hopkins.\n*La gráfica solo incluye los países y territorios que han reportado 1,000 o más casos") +
  tema +
  theme(plot.title = element_text(size = 37), 
        plot.subtitle = element_text(size = 23), 
        plot.caption = element_text(size = 15), 
        panel.grid = element_blank(), 
        legend.position = c(0.165, -0.07), 
        legend.direction = "horizontal",
        legend.text = element_text(size = 18.5)) +
  ggsave(str_c(ruta_graficas_global, "08_estatus_pacientes_covid19_paises_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 19.5, height = 14)

### Gráfica 09: Burbujas del top-10 de países y territorios con más casos confirmados de COVID-19 ----
foo <- 
  novel_x_pais %>% 
  mutate(pais = ifelse(pais == "Crucero Diamond Princess", "Crucero", pais)) %>%
  # count(pais) %>% print(n = Inf)
  group_by(pais) %>% 
  summarise(casos_totales = sum(casos_confirmados)) %>% 
  ungroup() %>% 
  arrange(-casos_totales) %>% 
  mutate(porcentaje = round(casos_totales/sum(casos_totales)*100, 1), 
         pais_bis = case_when(pais == "Corea del Sur" ~ "Corea del\n  Sur", 
                              pais == "Hong Kong" ~ "Hong\nKong", 
                              TRUE ~ pais),
         pais_bis = fct_reorder(pais_bis, casos_totales), 
         ranking = rank(-casos_totales, ties.method = "first")) %>% 
  filter(ranking <= 10)  

foo %>% 
  mutate(etiqueta_paises = ifelse(pais == "China", "", pais), 
         etiqueta_casos = ifelse(pais == "China", "", str_c(comma(casos_totales, accuracy = 1), " casos")),
         etiqueta_casos_china= ifelse(pais == "China", str_c(comma(casos_totales, accuracy = 1), " casos"), ""),
         etiqueta_porcentajes = ifelse(pais == "China", "", str_c(porcentaje, "%")),
         etiqueta_porcentajes_china = ifelse(pais == "China", str_c(porcentaje, "%"), ""),
         etiqueta_paises = case_when(etiqueta_paises == "Corea del Sur" ~ "Corea del\n  Sur",
                                     etiqueta_paises == "Hong Kong" ~ "Hong\nKong",
                                     TRUE ~ etiqueta_paises)) %>% 
  ggplot(aes(x = fct_rev(pais_bis), y = 1)) +
  geom_point(aes(size = casos_totales), color = "salmon") +
  geom_text(x = 1, y = 1, label = "China", color = "white", size = 6, fontface = "bold", vjust = -1.9) +
  geom_text(aes(x = fct_rev(pais_bis), y = 0.99994, label = etiqueta_casos_china), color = "white", fontface = "bold", size = 4.5) +
  geom_text(aes(x = fct_rev(pais_bis), y = 0.9998, label = etiqueta_porcentajes_china), color = "white", fontface = "bold", size = 4.5) +
  geom_text(aes(x = fct_rev(pais_bis), y = 0.9995, label = etiqueta_paises), size = 5, color = "grey30", vjust = 1, fontface = "bold") +
  geom_text(aes(x = fct_rev(pais_bis), y = 0.99915, label = etiqueta_casos), color = "#ef3b2c", alpha = 0.7, fontface = "bold", size = 4.5) +
  geom_text(aes(x = fct_rev(pais_bis), y = 0.999, label = etiqueta_porcentajes), color = "grey50", fontface = "bold", size = 4.5) +
  scale_y_continuous(limits = c(0.999, 1.001)) +
  scale_x_discrete(expand = c(0.05, 0.05)) +
  scale_size(range = c(1, 50)) +
  labs(title = "Los 10 países y territorios con más casos de COVID-19 confirmados y reportados",
       subtitle = subtitulo_treemaps,
       x = NULL,
       y = NULL,
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuentes: OMS y el CSSE de la Universidad de Johns Hopkins\n\n") +
  labs(x = NULL,
       y = NULL) +
  tema +
  theme(legend.position = "none",
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        panel.grid = element_blank(),
        plot.caption = element_text(size = 10)) +
  ggsave(str_c(ruta_graficas_global, "09_top_10_mas_casos_confirmados", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 6)


### Gráfica 10: Porcentaje de casos confirmados que murieron, por país y territorio ----
novel_x_pais %>% 
  mutate(pais = ifelse(pais == "Crucero Diamond Princess", "Crucero", pais),
         pais = str_replace(pais, "República", "Rep.")) %>%
  mutate(etiqueta_grandes = ifelse(por_casos_conf_fallecieron > 10.09, str_c("Tasa: ", por_casos_conf_fallecieron, "% ", " (", comma(muertes), " muertos | ",  comma(casos_confirmados), " casos)"), ""),
         etiqueta_chicos = ifelse(por_casos_conf_fallecieron <= 10.09, str_c(por_casos_conf_fallecieron, "%", " (", comma(muertes), " | ", comma(casos_confirmados), ")"), ""),
         color_pais = ifelse(pais == "México", "a", "b")) %>%
  filter(casos_confirmados >= 500,
         muertes > 0) %>% 
  ggplot(aes(x = fct_reorder(pais, por_casos_conf_fallecieron), 
             y = por_casos_conf_fallecieron, 
             fill = color_pais)) +
  geom_col() +
  scale_y_continuous(limits = c(0, 12.5),
                     expand = c(0, 0)) +
  scale_fill_manual(values = c("salmon", "black")) +
  coord_flip() +
  geom_text(aes(label = etiqueta_grandes), color = "white", fontface = "bold", family = "Roboto", hjust = 1.05, size = 5) +
  geom_text(aes(label = etiqueta_chicos), color = "grey30", fontface = "bold", family = "Roboto", hjust = -0.05, size = 5) +
  labs(title = "Porcentaje de casos confirmados de Covid-19\nque murieron, por país y territorio",
       subtitle = subtitulo_treemaps,
       x = NULL,
       y = NULL,
       fill = NULL,
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40\nFuentes: OMS y el CSSE de la Universidad de Johns Hopkins\n\nNotas: *La categoría \"casos confirmados\"se refiere a las personas que dieron positivo en la prueba de Covid-19.\nLa variable graficada fue calculada dividiendo el número de muertes entre el número de casos confirmados, por 100.\nLa gráfica solo incluye los países y territorios que han reportado 500 o más casos y al menos una muerte.") +
  tema +
  theme(plot.title = element_text(size = 36), 
        plot.subtitle = element_text(size = 26), 
        panel.grid = element_blank(), 
        axis.text.x = element_blank(),
        legend.position = "none") +
  ggsave(str_c(ruta_graficas_global, "10_porcentaje_casos_confirmados_que_murieron", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 13.2, height = 20)

### Gráfica 11: Número de nuevos casos de Covid-19 confirmados diariamente ----
datos %>% 
  group_by(fecha_corte) %>% 
  summarise(total_casos_nuevos_diarios = sum(cambio_diario_casos, na.rm = T)) %>% 
  ungroup() %>% 
  # print(n = Inf)
  ggplot(aes(x = fecha_corte, y = total_casos_nuevos_diarios)) +
  geom_col(fill = "steelblue") +
  ggplot2::annotate(geom = "text", 
                    label = "Fecha en la que China cambió la\nforma de diagnosticar sus casos", 
                    x = as_date("2020-02-16"), 
                    y = 14000,
                    hjust = 0, 
                    size = 5, 
                    color = "grey40") +
  ggplot2::annotate(geom = "segment", 
                    x = as_date("2020-02-15"), 
                    xend = as_date("2020-02-13"), 
                    y = 14000,
                    yend = 14000) +
  scale_x_date(breaks = c(seq(from = as_date("2020-01-22"), 
                              to = max(datos$fecha_corte), 
                              by = 7),
                          max(datos$fecha_corte)), 
               date_labels = "%b-%d") +
  scale_y_continuous(labels = comma, 
                     # limits = c(0, 45000),
                     breaks = seq(0, 80000, 5000), 
                     expand = c(0, 0)) +
  labs(title = "Número de nuevos casos de Covid-19 confirmados diariamente",
       subtitle = subtitulo_treemaps,
       x = "",
       y = "Número\n",
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuente: CSSE de la Universidad Johns Hopkins") +
  tema +
  theme(plot.title = element_text(size = 36),
        axis.text.x = element_text(face = "bold", family = "Didact Gothic Regular", angle = 90, hjust = 1, vjust = 0.5)) +
  ggsave(str_c(ruta_graficas_global, "11_casos_nuevos_diarios_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)


### Gráfica 12: Número diario de muertes de pacientes enfermos de Covid-19 ----
datos %>% 
  arrange(pais, fecha_corte) %>% 
  group_by(pais) %>%
  mutate(cambio_diario_muertes = muertes - lag(muertes)) %>% 
  ungroup() %>% 
  group_by(fecha_corte) %>% 
  summarise(total_muertes_nuevas_diarios = sum(cambio_diario_muertes, na.rm = T)) %>% 
  ungroup() %>% 
  ggplot(aes(x = fecha_corte, y = total_muertes_nuevas_diarios)) +
  geom_col(fill = "black") +
  scale_x_date(breaks = c(seq(from = as_date("2020-01-22"), 
                              to = max(datos$fecha_corte), 
                              by = 7),
                          max(datos$fecha_corte)), 
               date_labels = "%b-%d") +
  scale_y_continuous(labels = comma, 
                     expand = c(0, 0),
                     breaks = seq(0, 5000, 250)) +
  labs(title = "Número de casos confirmados de Covid-19 que fallecieron diariamente",
       subtitle = subtitulo_treemaps,
       x = "",
       y = "Número\n",
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuente: CSSE de la Universidad Johns Hopkins") +
  tema +
  theme(plot.title = element_text(size = 32),
        axis.text.x = element_text(face = "bold", family = "Didact Gothic Regular", angle = 90, hjust = 1, vjust = 0.5)) +
  ggsave(str_c(ruta_graficas_global, "12_ruta_graficas_global, muertes_nuevas_diarios_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)


### Gráfica 13: Número diario de pacientes que se recuperaron de Covid-19 ----
datos %>% 
  arrange(pais, fecha_corte) %>% 
  group_by(pais) %>%
  mutate(cambio_diario_recuperados = recuperados - lag(recuperados)) %>% 
  ungroup() %>% 
  group_by(fecha_corte) %>% 
  summarise(total_recuperados_nuevos_diarios = sum(cambio_diario_recuperados, na.rm = T)) %>% 
  ungroup() %>% 
  ggplot(aes(x = fecha_corte, y = total_recuperados_nuevos_diarios)) +
  geom_col(fill = "#41ab5d") +
  scale_x_date(breaks = c(seq(from = as_date("2020-01-22"), 
                              to = max(datos$fecha_corte), 
                              by = 7),
                          max(datos$fecha_corte)), 
               date_labels = "%b-%d") +
  scale_y_continuous(labels = comma, 
                     breaks = seq(0, 20000, 2000),
                     expand = c(0, 0)) +
  labs(title = "Número de casos confirmados de Covid-19 que se recuperaron diariamente",
       subtitle = subtitulo_treemaps,
       x = "",
       y = "Número\n",
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuente: CSSE de la Universidad Johns Hopkins") +
  tema +
  theme(plot.title = element_text(size = 30),
        axis.text.x = element_text(face = "bold", family = "Didact Gothic Regular", angle = 90, hjust = 1, vjust = 0.5)) +
  ggsave(str_c(ruta_graficas_global, "13_recuperados_nuevos_diarios_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)


### Gráfica 14_01: Evolución del número de pacientes enfermos de Covid-19 en tratamiento en los 15 países con más casos confirmados ----
datos %>% 
  mutate(pais = ifelse(pais == "Crucero Diamond Princess", "Crucero", pais)) %>%
  group_by(pais) %>% 
  mutate(total_casos = max(casos_confirmados)) %>% 
  ungroup() %>% 
  mutate(pais = fct_rev(fct_reorder(pais, total_casos))) %>% 
  group_by(fecha_corte) %>%
  mutate(ranking_total_casos = rank(-total_casos, 
                                    ties.method = "first")) %>% 
  ungroup() %>% 
  mutate(ranking_total_casos = ifelse(fecha_corte == max(fecha_corte), ranking_total_casos, NA)) %>% 
  arrange(pais, desc(fecha_corte)) %>% 
  group_by(pais) %>% 
  mutate(ranking_total_casos = na.locf(ranking_total_casos, fromLast = T)) %>% 
  ungroup() %>% 
  filter(ranking_total_casos < 16) %>%
  # Alargar duración de los últimos siete días
  arrange(fecha_corte) %>% 
  mutate(show_time = case_when(fecha_corte >= Sys.Date() - 7 ~ 5,
                               TRUE           ~ 1),
         reveal_time = cumsum(show_time)) %>% 
  # Calcular % de pacientes que están en tratamiento respecto al total de casos confirmados
  mutate(porcentaje_en_tratamiento = round(en_tratamiento/casos_confirmados*100, 1)) %>% 
  ggplot(aes(x = fecha_corte, y = en_tratamiento/1000, group = pais)) +
  geom_line(color = "#F5772F", size = 1) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b-%d") +
  scale_y_continuous(label = comma) +
  facet_wrap(~ pais, ncol = 5) +
  labs(title = "Evolución del número de pacientes enfermos de Covid-19 en tratamiento en los\n15 países con más casos confirmados",
       subtitle = str_c(subtitulo_treemaps, " | Misma escala en el eje vertical"),
       x = "\n",
       y = "Miles de casos\n",
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuente: CSSE de la Universidad de Johns Hopkins\nNota: la categoría \"en tratamiento\" se refiere a los pacientes que dieron positivo en la prueba y que no han terminado su recuperación o fallecieron.") +
  tema +
  theme(axis.text.x = element_text(size = 13, face = "bold", family = "Didact Gothic Regular", angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 13), 
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        # panel.spacing.x = unit(1.5, "lines"),
        strip.background = element_rect(fill = "grey70", color = "grey70")) +
  ggsave(str_c(ruta_graficas_global, "14_01_numero_pacientes_en_tratamiento_top_15_casos_misma_escala_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 10)

### Gráfica 14_02: Evolución del número de pacientes enfermos de Covid-19 en tratamiento en los 15 países con más casos confirmados ----
datos %>% 
  mutate(pais = ifelse(pais == "Crucero Diamond Princess", "Crucero", pais)) %>%
  group_by(pais) %>% 
  mutate(total_casos = max(casos_confirmados)) %>% 
  ungroup() %>% 
  mutate(pais = fct_rev(fct_reorder(pais, total_casos))) %>% 
  group_by(fecha_corte) %>%
  mutate(ranking_total_casos = rank(-total_casos, 
                                    ties.method = "first")) %>% 
  ungroup() %>% 
  mutate(ranking_total_casos = ifelse(fecha_corte == max(fecha_corte), ranking_total_casos, NA)) %>% 
  arrange(pais, desc(fecha_corte)) %>% 
  group_by(pais) %>% 
  mutate(ranking_total_casos = na.locf(ranking_total_casos, fromLast = T)) %>% 
  ungroup() %>% 
  filter(ranking_total_casos < 16) %>%
  # Alargar duración de los últimos siete días
  arrange(fecha_corte) %>% 
  mutate(show_time = case_when(fecha_corte >= Sys.Date() - 7 ~ 5,
                               TRUE           ~ 1),
         reveal_time = cumsum(show_time)) %>% 
  # Calcular % de pacientes que están en tratamiento respecto al total de casos confirmados
  mutate(porcentaje_en_tratamiento = round(en_tratamiento/casos_confirmados*100, 1)) %>% 
  ggplot(aes(x = fecha_corte, y = en_tratamiento/1000, group = pais)) +
  geom_line(color = "#F5772F", size = 1) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b-%d") +
  scale_y_continuous(label = comma) +
  facet_wrap(~ pais, ncol = 5, scales = "free_y") +
  labs(title = "Evolución del número de pacientes enfermos de Covid-19 en tratamiento en los\n15 países con más casos confirmados",
       subtitle = str_c(subtitulo_treemaps, " | Escala libre en el eje vertical"),
       x = "\n",
       y = "Miles de casos\n",
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuente: CSSE de la Universidad de Johns Hopkins\nNota: la categoría \"en tratamiento\" se refiere a los pacientes que dieron positivo en la prueba y que no han terminado su recuperación o fallecieron.") +
  tema +
  theme(axis.text.x = element_text(size = 13, face = "bold", family = "Didact Gothic Regular", angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 13), 
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        # panel.spacing.x = unit(1.5, "lines"),
        strip.background = element_rect(fill = "grey70", color = "grey70")) +
  ggsave(str_c(ruta_graficas_global, "14_02_numero_pacientes_en_tratamiento_top_15_casos_escala_libre_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 10)



### Gráfica 15: Evolución del número de casos confirmados de Covid-19 en los 15 países y territorios con más casos además de China ----
datos  %>% 
  mutate(pais = ifelse(pais == "Crucero Diamond Princess", "Crucero", pais)) %>%
  group_by(pais) %>% 
  mutate(total_casos = max(casos_confirmados)) %>% 
  ungroup() %>% 
  mutate(pais = fct_rev(fct_reorder(pais, total_casos))) %>% 
  group_by(fecha_corte) %>%
  mutate(ranking_total_casos = rank(-total_casos, 
                                    ties.method = "first")) %>% 
  ungroup() %>% 
  mutate(ranking_total_casos = ifelse(fecha_corte == max(fecha_corte), ranking_total_casos, NA)) %>% 
  arrange(pais, desc(fecha_corte)) %>% 
  group_by(pais) %>% 
  mutate(ranking_total_casos = na.locf(ranking_total_casos, fromLast = T)) %>% 
  ungroup() %>% 
  filter(ranking_total_casos < 16) %>%
  # Alargar duración de los últimos siete días
  arrange(fecha_corte) %>% 
  mutate(show_time = case_when(fecha_corte >= Sys.Date() - 7 ~ 5,
                               TRUE           ~ 1),
         reveal_time = cumsum(show_time)) %>% 
  ggplot(aes(x = fecha_corte, y = casos_confirmados, group = pais)) +
  geom_line(color = "#F5772F", size = 1) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b-%d") +
  scale_y_continuous(breaks = seq(0, 250000, 25000),
                     label = comma) +
  facet_wrap(~ pais, ncol = 5) +
  labs(title = "Evolución del número de casos confirmados de Covid-19 en los\n15 países con más casos",
       subtitle = subtitulo_treemaps,
       x = "\n",
       y = "Número de casos\n",
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40\nFuentes: OMS y el CSSE de la Universidad de Johns Hopkins / Nota: la categoría \"casos confirmados\" incluye\ncasos que presuntamente dieron positivo en la prueba.") +
  tema +
  theme(plot.title = element_text(size = 33),
        axis.text.x = element_text(size = 13, face = "bold", family = "Didact Gothic Regular", angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 11),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        panel.spacing.x = unit(1.5, "lines"),
        strip.background = element_rect(fill = "grey70", color = "grey70")) +
  ggsave(str_c(ruta_graficas_global, "15_evolucion_casos_confirmados_top_15_casos_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 10)


### Gráfica 16: Evolución del número de muertes provocadas por el Covid-19 en los 15 países y territorios con más casos además de China ----
datos %>% 
  mutate(pais = ifelse(pais == "Crucero Diamond Princess", "Crucero", pais)) %>%
  group_by(pais) %>% 
  mutate(total_muertes = max(muertes)) %>% 
  ungroup() %>% 
  mutate(pais = fct_rev(fct_reorder(pais, total_muertes))) %>% 
  group_by(fecha_corte) %>%
  mutate(ranking_total_muertes = rank(-total_muertes, 
                                      ties.method = "first")) %>% 
  ungroup() %>% 
  mutate(ranking_total_muertes = ifelse(fecha_corte == max(fecha_corte), ranking_total_muertes, NA)) %>% 
  arrange(pais, desc(fecha_corte)) %>% 
  group_by(pais) %>% 
  mutate(ranking_total_muertes = na.locf(ranking_total_muertes, fromLast = T)) %>% 
  ungroup() %>% 
  filter(ranking_total_muertes < 16) %>%
  # Alargar duración de los últimos siete días
  arrange(fecha_corte) %>% 
  mutate(show_time = case_when(fecha_corte >= Sys.Date() - 7 ~ 5,
                               TRUE           ~ 1),
         reveal_time = cumsum(show_time)) %>% 
  ggplot(aes(x = fecha_corte, y = muertes, group = pais)) +
  geom_line(size = 1, color = "steelblue") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b-%d") +
  scale_y_continuous(breaks = seq(0, 20000, 2500),
                     label = comma) +
  facet_wrap(~ pais, ncol = 5) +
  labs(title = "Evolución del número de muertes provocadas por el Covid-19 en los\n15 países con más muertes",
       subtitle = subtitulo_treemaps,
       x = "\n",
       y = "Número de muertes\n",
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40\nFuentes: OMS y el CSSE de la Universidad de Johns Hopkins") +
  tema +
  theme(plot.title = element_text(size = 33),
        axis.text.x = element_text(size = 13, face = "bold", family = "Didact Gothic Regular", angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 11),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        panel.spacing.x = unit(1.5, "lines"),
        strip.background = element_rect(fill = "grey70", color = "grey70")) +
  ggsave(str_c(ruta_graficas_global, "16_evolucion_muertes_top_15_casos_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 10)


### Gráfica 17_01: Evolución del número acumulado de casos confirmados de Covid-19 desde el primer caso confirmado ----

faa <- 
  datos %>% 
  group_by(pais) %>% 
  # Identificar el día en que se registró un caso en cada país 
  mutate(primer_caso = ifelse(casos_confirmados > 0 & fecha_corte == as_date("2020-01-22") | casos_confirmados > 0 & lag(casos_confirmados) == 0, 1, NA),
         # Construir variable de dummy_dias_primer_caso. Ésta tendrá un valor de uno para todos los días del 
         dummy_dias_primer_caso = primer_caso) %>% 
  fill(dummy_dias_primer_caso, .direction = "down") %>%
  mutate(dias_primer_caso = cumsum(replace_na(dummy_dias_primer_caso, 0))) %>% 
  ungroup() %>% 
  # Corregir días transcurridos desde el primer caso en China porque el primero se detectó y reportó a la OMS el 31 de diciembre de 2018
  mutate(dias_primer_caso = ifelse(pais == "China", dias_primer_caso + 22, dias_primer_caso)) %>% 
  filter(dias_primer_caso != 0) %>% 
  mutate(color_linea = ifelse(pais == "México", "México", "Otros países"),
         etiquetas_paises_casos = ifelse(fecha_corte == max(fecha_corte) & casos_confirmados > 12000 | fecha_corte == max(fecha_corte) & pais == "México", pais, ""),
         
         etiquetas_paises_muertes = ifelse(fecha_corte == max(fecha_corte) & muertes > 300 | fecha_corte == max(fecha_corte) & pais == "México", pais, ""),
         etiquetas_paises_muertes_log = ifelse(fecha_corte == max(fecha_corte) & muertes > 100 | fecha_corte == max(fecha_corte) & pais == "México" | fecha_corte == max(fecha_corte) & dias_primer_caso >= 65, pais, ""),
         pais = fct_relevel(pais, "México", after = Inf)) %>% 
  group_by(pais) %>% 
  mutate(puntito_final_casos = ifelse(fecha_corte == max(fecha_corte) & casos_confirmados > 12000 | fecha_corte == max(fecha_corte) & pais == "México", casos_confirmados, NA),
         puntito_final_muertes = ifelse(fecha_corte == max(fecha_corte) & muertes > 300 | fecha_corte == max(fecha_corte) & pais == "México", muertes, NA),
         puntito_final_muertes_log = ifelse(fecha_corte == max(fecha_corte) & muertes > 100 | fecha_corte == max(fecha_corte) & pais == "México" | fecha_corte == max(fecha_corte) & dias_primer_caso >= 65, muertes, NA)) %>% 
  ungroup()

faa %>% 
  ggplot(aes(dias_primer_caso, casos_confirmados, group = pais, color = color_linea, alpha = color_linea)) +
  geom_line(size = 1) +
  geom_point(aes(x = dias_primer_caso, 
                 y = puntito_final_casos),
             size = 2) +
  geom_text_repel(aes(label = etiquetas_paises_casos),
                  force = 0.1,
                  color = "grey30",
                  fontface = "bold", 
                  # direction = "y",
                  size = 5) +
  scale_x_continuous(breaks = c(1, seq(5, 120, 5)), limits = c(0, max(faa$dias_primer_caso) + max(faa$dias_primer_caso)*0.05)) +
  scale_y_continuous(limits = c(0, max(faa$casos_confirmados) + max(faa$casos_confirmados)*0.1),
                     breaks = seq(0, 300000, 20000),
                     labels = comma) +
  scale_color_manual(values = c("#1E6847", "grey80")) +
  scale_alpha_manual(values = c(1, 0.7)) +
  labs(title = "Evolución del número acumulado de casos confirmados de Covid-19 en cada\npaís o territorio desde el primer caso confirmado",
       subtitle = subtitulo_treemaps,
       x = "\nDías desde el primer caso confirmado        ",
       y = "Casos acumulado   \n",
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuentes: CSSE de la Universidad de Johns Hopkins y Secretaría de Salud de México.") +
  tema +
  theme(legend.position = "none") +
  ggsave(str_c(ruta_graficas_global, "17_01_evolucion_casos_desde_primer_caso_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 15.5, height = 9)


### Gráfica 17_02: Evolución del número acumulado de casos confirmados de Covid-19 desde el primer caso confirmado ----

faa <- 
  datos %>% 
  group_by(pais) %>% 
  # Identificar el día en que se registró un caso en cada país 
  mutate(primer_caso = ifelse(casos_confirmados > 0 & fecha_corte == as_date("2020-01-22") | casos_confirmados > 0 & lag(casos_confirmados) == 0, 1, NA),
         # Construir variable de dummy_dias_primer_caso. Ésta tendrá un valor de uno para todos los días del 
         dummy_dias_primer_caso = primer_caso) %>% 
  fill(dummy_dias_primer_caso, .direction = "down") %>%
  mutate(dias_primer_caso = cumsum(replace_na(dummy_dias_primer_caso, 0))) %>% 
  ungroup() %>% 
  # Corregir días transcurridos desde el primer caso en China porque el primero se detectó y reportó a la OMS el 31 de diciembre de 2018
  mutate(dias_primer_caso = ifelse(pais == "China", dias_primer_caso + 22, dias_primer_caso)) %>% 
  filter(dias_primer_caso != 0) %>% 
  mutate(color_linea = ifelse(pais == "México", "México", "Otros países"),
         etiquetas_paises_casos = ifelse(fecha_corte == max(fecha_corte) & casos_confirmados > 7000 | fecha_corte == max(fecha_corte) & pais == "México" | fecha_corte == max(fecha_corte) & dias_primer_caso > 65, pais, ""),
         etiquetas_paises_muertes = ifelse(fecha_corte == max(fecha_corte) & muertes > 300 | fecha_corte == max(fecha_corte) & pais == "México", pais, ""),
         pais = fct_relevel(pais, "México", after = Inf)) %>% 
  group_by(pais) %>% 
  mutate(puntito_final_casos = ifelse(fecha_corte == max(fecha_corte) & casos_confirmados > 7000 | fecha_corte == max(fecha_corte) & pais == "México" | fecha_corte == max(fecha_corte) & dias_primer_caso > 65, casos_confirmados, NA),
         puntito_final_muertes = ifelse(fecha_corte == max(fecha_corte) & muertes > 300 | fecha_corte == max(fecha_corte) & pais == "México", muertes, NA)) %>% 
  ungroup()

set.seed(18)
faa %>% 
  ggplot(aes(dias_primer_caso, casos_confirmados, group = pais, color = color_linea, alpha = color_linea)) +
  geom_line(size = 1) +
  geom_point(aes(x = dias_primer_caso, 
                 y = puntito_final_casos),
             size = 2) +
  geom_text_repel(aes(label = etiquetas_paises_casos), 
            # vjust = -0.8, 
            color = "grey30",
            fontface = "bold", 
            size = 5) +
  scale_x_continuous(breaks = c(1, seq(5, 120, 5)), limits = c(0, max(faa$dias_primer_caso) + max(faa$dias_primer_caso)*0.05)) +
  scale_y_log10(labels = comma_format(accuracy = 1), 
                # limits = c(1, 6e5),
                breaks = c(1, 3, 10, 30, 100, 300, 1000, 3e3, 10e3, 3e4, 10e4, 3e5, 10e5, 3e6, 10e6, 3e7, 10e7)) + 
  scale_color_manual(values = c("#1E6847", "grey80")) +
  scale_alpha_manual(values = c(1, 0.7)) +
  labs(title = "Evolución del número acumulado de casos confirmados de Covid-19 en cada\npaís o territorio desde el primer caso confirmado",
       subtitle = str_c(subtitulo_treemaps, " | Distancia logarítmica en las etiquetas del eje vertical"),
       x = "\nDías desde el primer caso confirmado        ",
       y = "Casos acumulados (log 10)\n",
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuentes: CSSE de la Universidad de Johns Hopkins y Secretaría de Salud de México.") +
  tema +
  theme(legend.position = "none") +
  ggsave(str_c(ruta_graficas_global, "17_02_evolucion_casos_desde_primer_caso_log10_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 15.5, height = 9)

### Gráfica 18_01: Evolución del número acumulado de muertes desde el primer caso confirmado ----

faa %>% 
  ggplot(aes(x = dias_primer_caso, y = muertes, group = pais, color = color_linea, alpha = color_linea)) +
  geom_line(size = 1) +
  geom_point(aes(x = dias_primer_caso, 
                 y = puntito_final_muertes),
             size = 2) +
  geom_text_repel(aes(label = etiquetas_paises_muertes), 
            # vjust = -0.8, 
            color = "grey30", 
            fontface = "bold", 
            size = 5) +
  scale_x_continuous(breaks = c(1, seq(5, 120, 5)), limits = c(0, max(faa$dias_primer_caso) + max(faa$dias_primer_caso)*0.05)) +
  scale_y_continuous(limits = c(0, max(faa$muertes) + max(faa$muertes)*0.1),
                     breaks = seq(0, 20000, 1000),
                     labels = comma) +
  scale_color_manual(values = c("#1E6847", "grey80")) +
  scale_alpha_manual(values = c(1, 0.7)) +
  labs(title = "Evolución del número acumulado de muertes de pacientes con Covid-19 en cada\npaís o territorio desde el primer caso confirmado",
       subtitle = subtitulo_treemaps,
       x = "\nDías desde el primer caso confirmado        ",
       y = "Muertes acumuladas   \n",
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuentes: CSSE de la Universidad de Johns Hopkins y Secretaría de Salud de México.") +
  tema +
  theme(legend.position = "none") +
  ggsave(str_c(ruta_graficas_global, "18_01_evolucion_casos_desde_primer_caso_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)



### Gráfica 18_02: Evolución del número acumulado de muertes desde el primer caso confirmado, log base 10 ----

faa %>% 
  ggplot(aes(x = dias_primer_caso, y = muertes, group = pais, color = color_linea, alpha = color_linea)) +
  geom_line(size = 1) +
  geom_point(aes(x = dias_primer_caso, 
                 y = puntito_final_muertes_log),
             size = 2) +
  geom_text_repel(aes(label = etiquetas_paises_muertes_log), 
            # vjust = -0.8, 
            color = "grey30", 
            fontface = "bold", 
            size = 5) +
  scale_x_continuous(breaks = c(1, seq(5, 120, 5)), limits = c(0, max(faa$dias_primer_caso) + max(faa$dias_primer_caso)*0.05)) +
  scale_y_log10(labels = comma_format(accuracy = 1), 
                # limits = c(1, 6e5),
                breaks = c(1, 3, 10, 30, 100, 300, 1000, 3e3, 10e3, 3e4, 10e4, 3e5, 10e5, 3e6, 10e6, 3e7, 10e7)) + 
  scale_color_manual(values = c("#1E6847", "grey80")) +
  scale_alpha_manual(values = c(1, 0.7)) +
  labs(title = "Evolución del número acumulado de muertes de pacientes con Covid-19 en cada\npaís o territorio desde el primer caso confirmado",
       subtitle = str_c(subtitulo_treemaps, " | Distancia logarítmica en las etiquetas del eje vertical"),
       x = "\nDías desde el primer caso confirmado  ",
       y = "Muertes acumuladas (log 10)   \n",
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuentes: CSSE de la Universidad de Johns Hopkins y Secretaría de Salud de México.") +
  tema +
  theme(legend.position = "none") +
  ggsave(str_c(ruta_graficas_global, "18_02_evolucion_casos_desde_primer_caso_log10_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)





### Gráfica 19_01: Relación entre el número acumulado de casos confirmados y el número de muertes por Covid-19 en cada país o territorio -----
novel_x_pais %>% 
  mutate(pais = fct_relevel(pais, "México", after = Inf),
         color_mexico = ifelse(pais == "México", "México", "Otro"),
         etiquetas_paises_muertes_log = ifelse(pais == "México" | casos_confirmados > 15000, as.character(pais), ""),
         tamaño_paises_muertes_log = ifelse(pais == "México", 2, 1)) %>% 
  ggplot(aes(x = casos_confirmados, 
             y = muertes,
             color = color_mexico, 
             alpha = color_mexico,
             size = tamaño_paises_muertes_log)) +
  geom_point() +
  geom_text(aes(label = etiquetas_paises_muertes_log),
            vjust = -0.8,
            color = "grey30",
            fontface = "bold",
            alpha = 0.9,
            size = 5) +
  # geom_text_repel(aes(label = etiquetas_paises_muertes_log), 
  #                 nudge_y = 1.5,
  #                 direction = "y",
  #                 vjust = 1,
  #                 color = "grey30",
  #                 force = 0.1,
  #                 fontface = "bold",
  #                 segment.color = "grey80", 
  #                 alpha = 0.9,
  #                 size = 5) +
  scale_x_continuous(labels = comma_format(accuracy = 1),
                     breaks = c(12500, seq(0, 150e3, 25e3))) +
  scale_y_continuous(labels = comma_format(accuracy = 1),
                     breaks = c(seq(0, 2000, 500), seq(0, 16000, 2000))) +
  scale_color_manual(values = c("#1E6847", "grey70")) +
  scale_alpha_manual(values = c(1, 0.7)) +
  scale_size(range = c(2, 6)) +
  labs(title = "Relación entre el número acumulado de casos confirmados y el número de muertes\npor Covid-19 en cada país o territorio",
       subtitle = subtitulo_treemaps,
       x = "\nCasos acumulados        ",
       y = "Muertes\n",
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuentes: CSSE de la Universidad de Johns Hopkins y Secretaría de Salud de México.") +
  tema +
  theme(legend.position = "none")  +
  ggsave(str_c(ruta_graficas_global, "19_01_relacion_casos_acumulados_muertes_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16.5, height = 9) 

### Gráfica 19_02: Relación entre el número acumulado de casos confirmados y el número de muertes por Covid-19 en cada país o territorio, version log10 -----
novel_x_pais %>% 
  mutate(color_mexico = ifelse(pais == "México", "México", "Otro"),
         etiquetas_paises_muertes_log = ifelse(muertes >= 100 | pais == "México" | casos_confirmados > 3100, pais, ""),
         tamaño_paises_muertes_log = ifelse(pais == "México", 2, 1),
         pais = fct_relevel(pais, "México", after = Inf),
  ) %>% 
  ggplot(aes(x = casos_confirmados, 
             y = muertes,
             color = color_mexico, 
             size = log(tamaño_paises_muertes_log))) +
  geom_point(alpha = 0.7) +
  geom_text_repel(aes(label = etiquetas_paises_muertes_log), 
                  vjust = -0.7,
                  color = "grey30",
                  force = 0.2,
                  fontface = "bold",
                  segment.color = "grey80", 
                  # direction = "x",
                  size = 5) +
  scale_x_log10(labels = comma_format(accuracy = 1),
                breaks = c(1, 3, 10, 30, 100, 300, 1000, 3e3, 10e3, 3e4, 10e4, 3e5, 10e5, 3e6, 10e6, 3e7, 10e7)) +
  scale_y_log10(labels = comma_format(accuracy = 1),
                breaks = c(1, 3, 10, 30, 100, 300, 1000, 3e3, 10e3, 3e4, 10e4, 3e5, 10e5, 3e6, 10e6, 3e7, 10e7)) +
  scale_color_manual(values = c("#1E6847", "grey70")) +
  scale_size(range = c(2, 5)) +
  labs(title = "Relación entre el número acumulado de casos confirmados y el número de muertes\npor Covid-19 en cada país o territorio",
       subtitle = str_c(subtitulo_treemaps, " | Distancia logarítmica en las etiquetas de ambos ejes"),
       x = "\nCasos acumulados (log 10)        ",
       y = "Muertes (log 10)\n",
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuentes: CSSE de la Universidad de Johns Hopkins y Secretaría de Salud de México.") +
  tema +
  theme(legend.position = "none")  +
  ggsave(str_c(ruta_graficas_global, "19_02_relacion_casos_acumulados_muertes_log10_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9) 

