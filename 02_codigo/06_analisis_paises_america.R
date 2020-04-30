### Limpiar ambiente ----
remove(list = ls())

### Cargar paquetes, definir setup y tema de gráficas ----
source("02_codigo/00_paquetes_setup_tema.R") 

### Cargar funciones ----
source("02_codigo/01_funciones_limpieza_bd.R")

### Definir cortes de datos ----
subtitulo <- str_c("Cifras a las 19:00 hrs. del ", 
                   day(Sys.Date()) - 1, 
                   " de abril de 2020 (CDMX)")


### Generar folder para guardar las gráficas ----
dir_graficas <- 
  dir.create(file.path("03_graficas/02_graficas_analisis_america/", 
                       str_c("graficas_", str_replace_all(Sys.Date() - 1, "-", "_"))))
ruta_graficas_america <- str_c("03_graficas/02_graficas_analisis_america/", 
                       str_c("graficas_", str_replace_all(Sys.Date() - 1, "-", "_"), "/"))

### Importar datos ----

# Automático
datos_diarios <- 
  read_csv(str_c("04_datos_generados/reporte_diario_por_pais/reporte_diario_por_pais_2020_04_", 
                 day(Sys.Date()) - 1,
                 ".csv"))

# Serie de tiempo
source("02_codigo/04_preparar_datos_series_de_tiempo.R")

max(datos$fecha_corte)

### Generar bases solo con países de América ----

bd_america <- 
  datos_diarios %>% 
  mutate(continente = case_when(pais %in% c("Antigua y Barbuda", "Argentina", "Aruba", "Bolivia", "Brasil", "Canadá", "Chile", "Colombia", "Costa Rica", "Cuba", "Curacao", "Ecuador", "EEUU", "El Salvador",  "Guatemala", "Guayana Francesa", "Guyana", "Haití", "Honduras", "Islas Caimán", "Isla de San Martín", "Jamaica",  "Martinica", "México", "Nicaragua", "Panamá", "Paraguay", "Perú", "Rep. Dominicana", "San Bartolomé", "San Vincente y las Granadinas", "Santa Lucía", "Surinam", "Trinidad y Tobago", "Uruguay", "Venezuela") ~ "América")) %>% 
  filter(continente == "América", 
         # Eliminar países con menos de 1M de personas
         !pais %in% c("Guayana Francesa", "Guyana", "Santa Lucía", "Surinam", "San Vincente y las Granadinas", "Martinica", "Curacao", "Aruba", "Islas Caimán", "Antigua y Barbuda"))

bd_america_st <- 
  datos %>% # Este tibble es generado en el script generar_gifs_y_videos.R
  mutate(continente = case_when(pais %in% c("Antigua y Barbuda", "Argentina", "Aruba", "Bolivia", "Brasil", "Canadá", "Chile", "Colombia", "Costa Rica", "Cuba", "Curacao", "Ecuador", "EEUU", "El Salvador",  "Guatemala", "Guayana Francesa", "Guyana", "Haití", "Honduras", "Islas Caimán", "Isla de San Martín", "Jamaica",  "Martinica", "México", "Nicaragua", "Panamá", "Paraguay", "Perú", "Rep. Dominicana", "San Bartolomé", "San Vincente y las Granadinas", "Santa Lucía", "Surinam", "Trinidad y Tobago", "Uruguay", "Venezuela") ~ "América")) %>% 
  filter(continente == "América", 
         # Eliminar países con menos de 1M de personas
         !pais %in% c("Guayana Francesa", "Guyana", "Santa Lucía", "Surinam", "San Vincente y las Granadinas", "Martinica", "Curacao", "Aruba", "Islas Caimán", "Antigua y Barbuda")) %>% 
  select(pais:recuperados, total_casos, cambio_diario_casos)

bd_america_st %>% 
  filter(pais == "México") %>% 
  tail()

### Ajustar datos de México con datos actualizados a las 19 hrs. ----
# bd_america <- 
#   bd_america %>% 
#   mutate(casos_confirmados = ifelse(pais == "México", 9501, casos_confirmados),
#          muertes = ifelse(pais == "México", 857, muertes),
#          recuperados = ifelse(pais == "México", 3263, recuperados)) %>%  
#   mutate(en_tratamiento = casos_confirmados - muertes - recuperados, 
#          por_casos_conf_fallecieron = round((muertes/casos_confirmados)*100, 2))

  

### Gráfica 01: Número de casos confirmados de COVID-19 en países de América -----
bd_america %>%   
  mutate(color_barras = ifelse(pais == "México", "México", "Los demás"),
         etiquetas_gdes = ifelse(casos_confirmados > 55000, comma(casos_confirmados, accuracy = 1), ""),
         etiquetas_peques = ifelse(casos_confirmados <= 55000, comma(casos_confirmados, accuracy = 1), "")) %>% 
  ggplot(aes(x = fct_reorder(pais, casos_confirmados), 
             y = casos_confirmados,
             fill = color_barras)) +
  geom_col() +
  geom_text(aes(label = etiquetas_gdes), color = "white", size = 4, hjust = 1.2, fontface = "bold") +
  geom_text(aes(label = etiquetas_peques), color = "grey20", size = 4, hjust = -0.2, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0), 
                     # limits = c(-1, 1050), 
                     breaks = seq(0, 2000, 500),
                     labels = comma_format(accuracy = 1)) +
  scale_fill_manual(values = c("grey70", "salmon")) +
  labs(title = "Número de casos confirmados de COVID-19 en países de América*",
       subtitle = subtitulo,
       x = NULL,
       y = NULL,
       fill = NULL,
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuentes: CSSE de la Universidad de Johns Hopkins y Secretaría de Salud de México.\nNota: *Solo se incluyen países con una población de 1 millón o más de personas.") +
  tema +
  theme(plot.title = element_text(size = 33), 
        panel.grid.major = element_blank(), 
        axis.text.x = element_blank(),
        legend.position = "none") +
  ggsave(str_c(ruta_graficas_america, "01_numero_casos_paises_america_latina_", str_replace_all(str_replace_all(str_replace_all(Sys.Date() - 1, "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16.5, height = 9)
  
### Gráfica 02: Distribución de numero de casos confirmados y reportados de COVID-19 en en cada país y territorio ----
bd_america %>% 
  ggplot(aes(area = casos_confirmados, fill = log(casos_confirmados))) +
  geom_treemap(col = "white") +
  geom_treemap_text(aes(label = pais), fontface = "bold", color = "white", alpha = 1, min.size = 0, grow = F) +
  geom_treemap_text(aes(label = paste(comma(casos_confirmados, accuracy = 1), "casos", sep = " ")), color = "white", padding.y = unit(7, "mm"),min.size = 0) +
  geom_treemap_text(aes(label = paste(comma(casos_confirmados/sum(casos_confirmados)*100, accuracy = 1), "% del total", sep = "")), color = "white", padding.y = unit(14, "mm"), min.size = 0, size = 14) +
  scale_fill_gradient(low = "grey95", high = "steelblue", guide = guide_colorbar(barwidth = 18, nbins = 6), labels = comma, breaks = pretty_breaks(n = 6)) +
  labs(title = "Número de casos confirmados y reportados de COVID-19 en países\nde América*",
       subtitle = subtitulo,
       x = NULL,
       y = NULL,
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuentes: CSSE de la Universidad de Johns Hopkins y Secretaría de Salud de México.\nNota: *Solo se incluyen países con una población de 1 millón o más de personas.") +
  tema +
  theme(legend.position = "none", 
        plot.title = element_text(size = 29),
        plot.subtitle = element_text(size = 20)) +
  ggsave(str_c(ruta_graficas_america, "02_distribucion_casos_confirmados_covid19_america_", str_replace_all(str_replace_all(str_replace_all(Sys.Date() - 1, "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 14, height = 9)


### Gráfica 03: Número de muertes por COVID-19 en países de América -----
bd_america %>% 
  mutate(color_barras = ifelse(pais == "México", "México", "Los demás"),
         etiquetas_gdes = ifelse(muertes > 1000, comma(muertes, accuracy = 1), ""),
         etiquetas_peques = ifelse(muertes <= 1000, comma(muertes, accuracy = 1), "")) %>% 
  ggplot(aes(x = fct_reorder(pais, muertes), 
             y = muertes,
             fill = color_barras)) +
  geom_col() +
  geom_text(aes(label = etiquetas_gdes), color = "white", size = 4, hjust = 1.2, fontface = "bold") +
  geom_text(aes(label = etiquetas_peques), color = "grey20", size = 4, hjust = -0.3, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0), 
                     # limits = c(-1, 1050), 
                     breaks = seq(0, 1000, 100),
                     labels = comma_format(accuracy = 1)) +
  scale_fill_manual(values = c("black", "salmon")) +
  labs(title = "Casos confirmados de Covid-19 que fallecieron en países de América*",
       subtitle = subtitulo,
       x = NULL,
       y = NULL,
       fill = NULL,
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuentes: CSSE de la Universidad de Johns Hopkins y Secretaría de Salud de México.\nNota: *Solo se incluyen países con una población de 1 millón o más de personas.") +
  tema +
  theme(plot.title = element_text(size = 32), 
        panel.grid.major = element_blank(), 
        axis.text.x = element_blank(),
        legend.position = "none") +
  ggsave(str_c(ruta_graficas_america, "03_numero_muertes_paises_america_latina_", str_replace_all(str_replace_all(str_replace_all(Sys.Date() - 1, "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16.5, height = 9)

### Gráfica 04: Distribución de numero de muertes de COVID-19 en en cada país y territorio ----
bd_america %>% 
  ggplot(aes(area = muertes, fill = log(muertes))) +
  geom_treemap(col = "white") +
  geom_treemap_text(aes(label = pais), fontface = "bold", color = "white", alpha = 1, min.size = 0, grow = F) +
  geom_treemap_text(aes(label = paste(comma(muertes, accuracy = 1), "muertes", sep = " ")), color = "white", padding.y = unit(7, "mm"),min.size = 0) +
  geom_treemap_text(aes(label = paste(comma(muertes/sum(muertes)*100, accuracy = 1), "% del total", sep = "")), color = "white", padding.y = unit(14, "mm"), min.size = 0, size = 14) +
  scale_fill_gradient(low = "grey95", high = "black", guide = guide_colorbar(barwidth = 18, nbins = 6), labels = comma, breaks = pretty_breaks(n = 6)) +
  labs(title = "Casos confirmados de Covid-19 que fallecieron en países de América*",
       subtitle = subtitulo,
       x = NULL,
       y = NULL,
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuentes: CSSE de la Universidad de Johns Hopkins y Secretaría de Salud de México.\nNota: *Solo se incluyen países con una población de 1 millón o más de personas.") +
  tema +
  theme(legend.position = "none", 
        plot.title = element_text(size = 29),
        plot.subtitle = element_text(size = 20)) +
  ggsave(str_c(ruta_graficas_america, "04_distribucion_muertes_covid19_america_", str_replace_all(str_replace_all(str_replace_all(Sys.Date() - 1, "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 14, height = 9)

### Gráfica 05: Número de pacientes que se recuperaron por COVID-19 en países de América -----
bd_america %>% 
  mutate(color_barras = ifelse(pais == "México", "México", "Los demás"),
         etiquetas_gdes = ifelse(recuperados > 400, comma(recuperados, accuracy = 1), ""),
         etiquetas_peques = ifelse(recuperados <= 400, comma(recuperados, accuracy = 1), "")) %>% 
  ggplot(aes(x = fct_reorder(pais, recuperados), 
             y = recuperados,
             fill = color_barras)) +
  geom_col() +
  geom_text(aes(label = etiquetas_gdes), color = "white", size = 4, hjust = 1.2, fontface = "bold") +
  geom_text(aes(label = etiquetas_peques), color = "grey20", size = 4, hjust = -0.3, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0), 
                     # limits = c(-1, 1050), 
                     breaks = seq(0, 1000, 100),
                     labels = comma_format(accuracy = 1)) +
  scale_fill_manual(values = c("#41ab5d", "salmon")) +
  labs(title = "Casos confirmados de COVID-19 que se recuperaron en países de América*",
       subtitle = subtitulo,
       x = NULL,
       y = NULL,
       fill = NULL,
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuentes: CSSE de la Universidad de Johns Hopkins y Secretaría de Salud de México.\nNota: *Solo se incluyen países con una población de 1 millón o más de personas.") +
  tema +
  theme(plot.title = element_text(size = 30), 
        panel.grid.major = element_blank(), 
        axis.text.x = element_blank(),
        legend.position = "none") +
  ggsave(str_c(ruta_graficas_america, "05_numero_recuperados_paises_america_latina_", str_replace_all(str_replace_all(str_replace_all(Sys.Date() - 1, "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16.5, height = 9)

### Gráfica 06: Distribución de numero de recuperados de COVID-19 en en cada país y territorio ----
bd_america %>% 
  ggplot(aes(area = recuperados, fill = log(recuperados))) +
  geom_treemap(col = "white") +
  geom_treemap_text(aes(label = pais), fontface = "bold", color = "white", alpha = 1, min.size = 0, grow = F) +
  geom_treemap_text(aes(label = paste(comma(recuperados, accuracy = 1), "recuperados", sep = " ")), color = "white", padding.y = unit(7, "mm"),min.size = 0) +
  geom_treemap_text(aes(label = paste(comma(recuperados/sum(recuperados)*100, accuracy = 1), "% del total", sep = "")), color = "white", padding.y = unit(14, "mm"), min.size = 0, size = 14) +
  scale_fill_gradient(low = "grey95", high = "#41ab5d", guide = guide_colorbar(barwidth = 18, nbins = 6), labels = comma, breaks = pretty_breaks(n = 6)) +
  labs(title = "Pacientes que se recuperaron después de haber enfermado de COVID-19\nen países de América*",
       subtitle = subtitulo,
       x = NULL,
       y = NULL,
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuentes: CSSE de la Universidad de Johns Hopkins y Secretaría de Salud de México.\nNota: *Solo se incluyen países con una población de 1 millón o más de personas.") +
  tema +
  theme(legend.position = "none", 
        plot.title = element_text(size = 29),
        plot.subtitle = element_text(size = 20)) +
  ggsave(str_c(ruta_graficas_america, "06_distribucion_recuperados_covid19_america_", str_replace_all(str_replace_all(str_replace_all(Sys.Date() - 1, "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 14, height = 9)


### Gráfica 07: Estatus de pacientes que enfermaron por el COVID-19 en países de América ----
bd_america %>%
    mutate(en_tratamiento = casos_confirmados - muertes - recuperados) %>%
    pivot_longer(cols = casos_confirmados:en_tratamiento, 
               names_to = "estatus", 
               values_to = "numero") %>% 
  filter(estatus != "casos_confirmados") %>% 
  group_by(pais) %>% 
  mutate(num_casos = sum(numero)) %>% 
  ungroup() %>% 
  mutate(porcentaje = round(numero/num_casos*100, 1),
         num_casos_texto = ifelse(numero == max(numero), str_c(comma(num_casos), " casos"), comma(num_casos)))  %>% 
  ggplot(aes(x = fct_reorder(pais, num_casos), 
             y = porcentaje,
             fill = estatus)) +
  geom_col() +
  geom_hline(yintercept = seq(10, 90, 10), linetype = 3, color = "white", size = 0.5) +
  geom_text(aes(x = fct_reorder(pais, num_casos),
                label = num_casos_texto),
            y = 101,
            hjust = 0,
            family = "Roboto", color = "salmon", 
            size = 5) +
  scale_x_discrete(expand = c(0.01, 0.01)) + 
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(-0.5, 111),
                     breaks = seq(0, 100, 10)) +
  scale_fill_manual(values = c("grey80", "#d73027", "#1a9850"),
                    labels = c("En tratamiento", "Fallecidos", "Recuperados")) +
  coord_flip() +
  labs(title = "Estatus de pacientes que enfermaron por el COVID-19 en países\nde América*",
       subtitle = subtitulo,
       x = NULL,
       y = "\nPorcentaje                       ",
       fill = NULL,
       caption = "\nElaborado por @segasi / Fuentes: CSSE de la Universidad de Johns Hopkins y Secretaría de Salud de México.\nNota: *Solo se incluyen países con una población de 1 millón o más de personas.") +
  tema +
  theme(plot.title = element_text(size = 33), 
        plot.subtitle = element_text(size = 22), 
        panel.grid = element_blank(), 
        legend.position = c(0.185, -0.07), 
        legend.direction = "horizontal",
        legend.text = element_text(size = 18.5)) +
  ggsave(str_c(ruta_graficas_america, "07_estatus_pacientes_paises_america_latina_", str_replace_all(str_replace_all(str_replace_all(Sys.Date() - 1, "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 17, height = 14)



### Gráfica 08: Evolución del número de casos confirmados de Covid-19 en países de América, escala libre ----
bd_america_st %>% 
  ggplot(aes(x = fecha_corte, y = casos_confirmados)) +
  geom_line(color = "dodgerblue", size = 1) +
  scale_y_continuous(labels = comma) +
  facet_wrap(~ pais, ncol = 6) +
  labs(title = "Evolución del número de casos confirmados de Covid-19 en países de América*",
       subtitle = subtitulo,
       x = "\n",
       y = "Número de casos\n",
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuentes: CSSE de la Universidad de Johns Hopkins y Secretaría de Salud de México.\nNota: *Solo se incluyen países con una población de 1 millón o más de personas.") +
  tema +
  theme(axis.text.x = element_text(size = 13, face = "bold", family = "Didact Gothic Regular", angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 13), 
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        # panel.spacing.x = unit(1.5, "lines"),
        strip.background = element_rect(fill = "grey70", color = "grey70")) +
  ggsave(str_c(ruta_graficas_america, "08_evolucion_casos_paises_america_misma_libre_", str_replace_all(str_replace_all(str_replace_all(Sys.Date() - 1, "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16.5, height = 9)


### Gráfica 09: Evolución del número de casos confirmados de Covid-19 en países de América, escala libre ----

bd_america_st %>% 
  mutate(pais = case_when(pais == "República Dominicana" ~ "Rep. Dominicana", 
                          TRUE ~ pais)) %>% 
  ggplot(aes(x = fecha_corte, y = casos_confirmados)) +
  geom_line(color = "dodgerblue", size = 1) +
  scale_y_continuous(labels = comma) +
  facet_wrap(~ pais, scales = "free_y", ncol = 6) +
  labs(title = "Evolución del número de casos confirmados de Covid-19 en países de América*",
       subtitle = str_c(subtitulo, " | Escala libre en el eje vertical"),
       x = "\n",
       y = "Número de casos\n",
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuentes: CSSE de la Universidad de Johns Hopkins y Secretaría de Salud de México.\nNota: *Solo se incluyen países con una población de 1 millón o más de personas.") +
  tema +
  theme(axis.text.x = element_text(size = 13, face = "bold", family = "Didact Gothic Regular", angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 13), 
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        # panel.spacing.x = unit(1.5, "lines"),
        strip.background = element_rect(fill = "grey70", color = "grey70")) +
  ggsave(str_c(ruta_graficas_america, "09_evolucion_casos_paises_america_escala_libre_", str_replace_all(str_replace_all(str_replace_all(Sys.Date() - 1, "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16.5, height = 9)




### Gráfica 10_01: Evolución del número acumulado de casos desde el primer caso confirmado en países de América Latina ---- 
foo <- 
  bd_america_st %>% 
  mutate(pais = ifelse(str_detect(pais, "Dominicana"), "Rep. Dom.", pais)) %>% 
  group_by(pais) %>% 
  mutate(primer_caso = ifelse(casos_confirmados > 0 & fecha_corte == as_date("2020-01-22") | casos_confirmados > 0 & lag(casos_confirmados) == 0 & pais != "EEUU", 1, NA),
         dummy_dias_primer_caso = primer_caso) %>% 
  fill(dummy_dias_primer_caso, .direction = "down") %>% 
  mutate(dias_primer_caso = cumsum(replace_na(dummy_dias_primer_caso, 0)),
         dias_primer_caso = dias_primer_caso - 1) %>% 
  ungroup() %>% 
  filter(dias_primer_caso >= 0, 
         !pais %in% c("Canadá", "EEUU")) %>% 
  mutate(color_linea = ifelse(pais == "México", "México", "Otros países"),
         etiquetas_paises = ifelse(fecha_corte == max(fecha_corte) & casos_confirmados > 350, pais, ""),
         etiquetas_paises_log = ifelse(fecha_corte == max(fecha_corte) & casos_confirmados > 1350 | fecha_corte == max(fecha_corte) & dias_primer_caso >= 15, pais, ""),
         pais = fct_relevel(pais, "México", after = Inf)) %>% 
  group_by(pais) %>% 
  mutate(puntito_final = ifelse(fecha_corte == max(fecha_corte) & casos_confirmados > 350, casos_confirmados, NA),
         puntito_final_log = ifelse(fecha_corte == max(fecha_corte) & casos_confirmados > 350 | fecha_corte == max(fecha_corte) & dias_primer_caso >= 15, casos_confirmados, NA)) %>% 
  ungroup()

set.seed(12)
foo %>% 
  ggplot(aes(x = dias_primer_caso, 
             y = casos_confirmados, 
             group = pais, 
             color = color_linea, 
             alpha = color_linea)) +
  geom_line(size = 1) +
  geom_point(aes(x = dias_primer_caso, 
                 y = puntito_final),
             size = 2) +
  geom_text_repel(aes(label = etiquetas_paises),
                  vjust = -0.7,
                  color = "grey30",
                  force = 0.2,
                  fontface = "bold",
                  direction = "x",
                  size = 5) +
  scale_x_continuous(breaks = c(0, seq(5, 100, 5)), limits = c(0, max(foo$dias_primer_caso) + max(foo$dias_primer_caso)*0.05)) +
  scale_y_continuous(limits = c(0, max(foo$casos_confirmados) + max(foo$casos_confirmados)*0.05),
                     label = comma, 
                     breaks = seq(0, 100000, 5000)) +
  scale_color_manual(values = c("#1E6847", "grey80")) +
  scale_alpha_manual(values = c(1, 0.7)) +
  labs(title = "Evolución del número acumulado de casos confirmados desde el primer caso\nconfirmado en países de América Latina*",
       subtitle = subtitulo,
       x = "\nDías desde el primer caso confirmado  ",
       y = "Número de casos  \n",
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuentes: CSSE de la Universidad de Johns Hopkins y Secretaría de Salud de México.\nNota: *Solo se incluyen países con una población de 1 millón o más de personas.") +
  tema +
  theme(legend.position = "none")  +
  ggsave(str_c(ruta_graficas_america, "10_01_evolucion_casos_paises_america_latina_desde_primer_caso_", str_replace_all(str_replace_all(str_replace_all(Sys.Date() - 1, "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)


### Gráfica 10_02: Evolución del número acumulado de casos desde el primer caso confirmado en países de América Latina, log 10 ---- 

foo %>% 
  ggplot(aes(x = dias_primer_caso, 
             y = casos_confirmados, 
             group = pais, 
             color = color_linea, 
             alpha = color_linea)) +
  geom_line(size = 1) +
  geom_point(aes(x = dias_primer_caso, 
                 y = puntito_final_log),
             size = 2) +
  geom_text_repel(aes(label = etiquetas_paises_log),
                  # vjust = -0.7,
                  color = "grey30",
                  # force = 0.5,
                  fontface = "bold",
                  # direction = "x",
                  size = 5) +
  scale_x_continuous(breaks = c(0, seq(5, 100, 5)), limits = c(0, max(foo$dias_primer_caso) + max(foo$dias_primer_caso)*0.05)) +
  scale_y_log10(labels = comma_format(accuracy = 1), 
                # limits = c(1, 6e5),
                breaks = c(1, 3, 10, 30, 100, 300, 1000, 3e3, 10e3, 3e4, 10e4, 3e5, 10e5, 3e6, 10e6, 3e7, 10e7)) + 
  scale_color_manual(values = c("#1E6847", "grey80")) +
  scale_alpha_manual(values = c(1, 0.7)) +
  labs(title = "Evolución del número acumulado de casos confirmados desde el primer caso\nconfirmado en países de América Latina*",
       subtitle = str_c(subtitulo, " | Distancia logarítmica en las etiquetas del eje vertical"),
       x = "\nDías desde el primer caso confirmado  ",
       y = "Número de casos (log 10)    \n",
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuentes: CSSE de la Universidad de Johns Hopkins y Secretaría de Salud de México.\nNota: *Solo se incluyen países con una población de 1 millón o más de personas.") +
  tema +
  theme(legend.position = "none")  +
  ggsave(str_c(ruta_graficas_america, "10_02_evolucion_casos_paises_america_latina_desde_primer_caso_log_10_", str_replace_all(str_replace_all(str_replace_all(Sys.Date() - 1, "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)



### Gráfica 11_01: Evolución del número acumulado de muertes desde el primer caso confirmado en países de América Latina ---- 
foo <- 
  bd_america_st %>% 
  mutate(pais = ifelse(str_detect(pais, "Dominicana"), "Rep. Dom.", pais)) %>% 
  group_by(pais) %>% 
  mutate(primer_caso = ifelse(casos_confirmados > 0 & fecha_corte == as_date("2020-01-22") | casos_confirmados > 0 & lag(casos_confirmados) == 0 & pais != "EEUU", 1, NA),
         dummy_dias_primer_caso = primer_caso) %>% 
  fill(dummy_dias_primer_caso, .direction = "down") %>% 
  mutate(dias_primer_caso = cumsum(replace_na(dummy_dias_primer_caso, 0)),
         dias_primer_caso = dias_primer_caso - 1) %>% 
  ungroup() %>% 
  filter(dias_primer_caso >= 0, 
         !pais %in% c("Canadá", "EEUU")) %>% 
  mutate(color_linea = ifelse(pais == "México", "México", "Otros países"),
         etiquetas_paises = ifelse(fecha_corte == max(fecha_corte) & muertes > 20, pais, ""),
         etiquetas_paises_log = ifelse(fecha_corte == max(fecha_corte) & muertes > 20 | fecha_corte == max(fecha_corte) & dias_primer_caso >= 20, pais, ""),
         pais = fct_relevel(pais, "México", after = Inf)) %>% 
  group_by(pais) %>% 
  mutate(puntito_final = ifelse(fecha_corte == max(fecha_corte) & muertes > 20, muertes, NA),
         puntito_final_log = ifelse(fecha_corte == max(fecha_corte) & muertes > 20 | fecha_corte == max(fecha_corte) & dias_primer_caso >= 20, muertes, NA)) %>% 
  ungroup()

set.seed(12)
foo %>% 
  ggplot(aes(x = dias_primer_caso, 
             y = muertes, 
             group = pais, 
             color = color_linea, 
             alpha = color_linea)) +
  geom_line(size = 1) +
  geom_point(aes(x = dias_primer_caso, 
                 y = puntito_final),
             size = 2) +
  geom_text_repel(aes(label = etiquetas_paises),
                  vjust = -0.7,
                  color = "grey30",
                  force = 0.2,
                  fontface = "bold",
                  direction = "x",
                  size = 5) +
  scale_x_continuous(breaks = c(0, seq(5, 100, 5)), limits = c(0, max(foo$dias_primer_caso) + max(foo$dias_primer_caso)*0.02)) +
  scale_y_continuous(limits = c(0, max(foo$muertes) + max(foo$muertes)*0.02),
                     label = comma, 
                     breaks = seq(0, 10000, 500)) +
  scale_color_manual(values = c("#1E6847", "grey80")) +
  scale_alpha_manual(values = c(1, 0.7)) +
  labs(title = "Evolución del número acumulado de muertes desde el primer caso confirmado\nen países de América Latina*",
       subtitle = subtitulo,
       x = "\nDías desde el primer caso confirmado  ",
       y = "Número de muertes  \n",
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuentes: CSSE de la Universidad de Johns Hopkins y Secretaría de Salud de México.\nNota: *Solo se incluyen países con una población de 1 millón o más de personas.") +
  tema +
  theme(legend.position = "none")  +
  ggsave(str_c(ruta_graficas_america, "11_01_evolucion_muertes_paises_america_latina_desde_primer_caso_", str_replace_all(str_replace_all(str_replace_all(Sys.Date() - 1, "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)


### Gráfica 11_02: Evolución del número acumulado de muertes desde el primer caso confirmado en países de América Latina, log 10 ---- 

foo %>% 
  ggplot(aes(x = dias_primer_caso, 
             y = muertes, 
             group = pais, 
             color = color_linea, 
             alpha = color_linea)) +
  geom_line(size = 1) +
  geom_point(aes(x = dias_primer_caso, 
                 y = puntito_final_log),
             size = 2) +
  geom_text_repel(aes(label = etiquetas_paises_log),
                  vjust = -0.7,
                  color = "grey30",
                  force = 0.2,
                  fontface = "bold",
                  direction = "x",
                  size = 5) +
  scale_x_continuous(breaks = c(0, seq(5, 100, 5)), limits = c(0, max(foo$dias_primer_caso) + max(foo$dias_primer_caso)*0.05)) +
  scale_y_log10(labels = comma_format(accuracy = 1), 
                # limits = c(1, 6e5),
                breaks = c(1, 3, 10, 30, 100, 300, 1000, 3e3, 10e3, 3e4, 10e4, 3e5, 10e5, 3e6, 10e6, 3e7, 10e7)) + 
  scale_color_manual(values = c("#1E6847", "grey80")) +
  scale_alpha_manual(values = c(1, 0.7)) +
  labs(title = "Evolución del número acumulado de muertes desde el primer caso confirmado\nen países de América Latina*",
       subtitle = str_c(subtitulo, " | Distancia logarítmica en las etiquetas del eje vertical"),
       x = "\nDías desde el primer caso confirmado  ",
       y = "Número de muertes (log 10)    \n",
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuentes: CSSE de la Universidad de Johns Hopkins y Secretaría de Salud de México.\nNota: *Solo se incluyen países con una población de 1 millón o más de personas.") +
  tema +
  theme(legend.position = "none")  +
  ggsave(str_c(ruta_graficas_america, "11_02_evolucion_muertes_paises_america_latina_desde_primer_caso_log_11_", str_replace_all(str_replace_all(str_replace_all(Sys.Date() - 1, "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)



### Gráfica 12_01: Número de nuevos casos de Covid-19 confirmados diariamente en los 20 países con más casos en América confirmados ----
bd_america_st %>% 
  group_by(fecha_corte) %>%
  mutate(ranking_total_casos = rank(-total_casos, 
                                    ties.method = "first")) %>% 
  ungroup() %>% 
  mutate(ranking_total_casos = ifelse(fecha_corte == max(fecha_corte), ranking_total_casos, NA),
         pais = fct_rev(fct_reorder(pais, total_casos))) %>% 
  group_by(pais) %>% 
  mutate(ranking_total_casos = na.locf(ranking_total_casos, fromLast = T),
         promedio_movil_cinco_dias = rollmean(cambio_diario_casos, k = 5, align = 'right', fill = NA)) %>% 
  ungroup() %>%
  filter(ranking_total_casos <= 20) %>% 
  ggplot(aes(x = fecha_corte, y = cambio_diario_casos)) +
  geom_col(color = "steelblue") +
  geom_line(aes(y = promedio_movil_cinco_dias), color = "salmon", size = 1) +
  scale_y_continuous(labels = comma) +
  facet_wrap(~ pais) +
  labs(title = "Número de nuevos casos de Covid-19 confirmados diariamente\nen los 20 países de América con más casos",
       subtitle = subtitulo,
       x = "",
       y = "Número\n",
       caption = "</span><br>Elaborado por @segasi para el Buró de Investigación de ADN40 / Fuente: CSSE de la Universidad Johns Hopkins<br><br>Nota: La línea **<span style='color:#fa8072;'>roja</span>** muestra el promedio móvil de cinco días del número de casos diarios confirmados.</span>") +
  tema +
  theme(plot.title = element_text(size = 35),
        plot.subtitle = element_text(size = 22),
        plot.caption = element_markdown(size = 18),
        axis.text.x = element_text(size = 13, face = "bold", family = "Didact Gothic Regular", angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 22),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        panel.spacing.x = unit(1.5, "lines"),
        strip.text = element_text(size = 18),
        strip.background = element_rect(fill = "grey70", color = "grey70")) +
  ggsave(str_c(ruta_graficas_america, "12_01_numero_diario_casos_top_20_casos_misma_escala_", str_replace_all(str_replace_all(str_replace_all(Sys.Date() - 1, "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 12)


### Gráfica 12_02: Número de nuevos casos de Covid-19 confirmados diariamente en los 20 países con más casos en América confirmados, escala libre ----
bd_america_st %>% 
  group_by(fecha_corte) %>%
  mutate(ranking_total_casos = rank(-total_casos, 
                                    ties.method = "first")) %>% 
  ungroup() %>% 
  mutate(ranking_total_casos = ifelse(fecha_corte == max(fecha_corte), ranking_total_casos, NA),
         pais = fct_rev(fct_reorder(pais, total_casos))) %>% 
  group_by(pais) %>% 
  mutate(ranking_total_casos = na.locf(ranking_total_casos, fromLast = T),
         promedio_movil_cinco_dias = rollmean(cambio_diario_casos, 5, align = 'right', fill = NA)) %>% 
  ungroup() %>%
  filter(ranking_total_casos <= 20) %>% 
  ggplot(aes(x = fecha_corte, y = cambio_diario_casos)) +
  geom_col(color = "steelblue") +
  geom_line(aes(y = promedio_movil_cinco_dias), color = "salmon", size = 1) +
  scale_y_continuous(labels = comma) +
  facet_wrap(~ pais, scales = "free_y") +
  labs(title = "Número de casos de Covid-19 confirmados diariamente en los\n20 países de América con más casos",
       subtitle = str_c(subtitulo, " | Escala libre en el eje vertical"),
       x = "",
       y = "Número\n",
       caption = "</span><br>Elaborado por @segasi para el Buró de Investigación de ADN40 / Fuente: CSSE de la Universidad Johns Hopkins<br><br>Nota: La línea **<span style='color:#fa8072;'>roja</span>** muestra el promedio móvil de cinco días del número de casos diarios confirmados.</span>") +
  tema +
  theme(plot.title = element_text(size = 35),
        plot.subtitle = element_text(size = 22),
        plot.caption = element_markdown(size = 18),
        axis.text.x = element_text(size = 13, face = "bold", family = "Didact Gothic Regular", angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 22),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        panel.spacing.x = unit(1.5, "lines"),
        strip.text = element_text(size = 18),
        strip.background = element_rect(fill = "grey70", color = "grey70")) +
  ggsave(str_c(ruta_graficas_america, "12_02_numero_diario_casos_top_20_casos_escala_libre_", str_replace_all(str_replace_all(str_replace_all(Sys.Date() - 1, "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 12)


### Gráfica 13_01: Número de casos confirmados de Covid-19 que fallecieron diariamente en los 20 países con más muertes por esta enfermedad ----
bd_america_st %>% 
  arrange(pais, fecha_corte) %>% 
  group_by(pais) %>%
  mutate(cambio_diario_muertes = muertes - lag(muertes)) %>% 
  ungroup() %>% 
  group_by(pais) %>% 
  mutate(total_muertes = max(muertes)) %>% 
  ungroup() %>% 
  group_by(fecha_corte) %>%
  mutate(ranking_total_muertes = rank(-total_muertes, 
                                      ties.method = "first")) %>% 
  ungroup() %>% 
  mutate(ranking_total_muertes = ifelse(fecha_corte == max(fecha_corte), ranking_total_muertes, NA),
         pais = fct_rev(fct_reorder(pais, total_muertes))) %>% 
  group_by(pais) %>% 
  mutate(ranking_total_muertes = na.locf(ranking_total_muertes, fromLast = T),
         promedio_movil_cinco_dias = rollmean(cambio_diario_muertes, k = 5, align = 'right', fill = NA)) %>% 
  ungroup() %>%
  filter(ranking_total_muertes <= 20) %>% 
  ggplot(aes(x = fecha_corte, y = cambio_diario_muertes)) +
  geom_col(color = "grey10") +
  geom_line(aes(y = promedio_movil_cinco_dias), color = "salmon", size = 1) +
  scale_y_continuous(labels = comma) +
  facet_wrap(~ pais) +
  labs(title = "Número de casos confirmados de Covid-19 que fallecieron\ndiariamente en los 20 países de América con más muertes",
       subtitle = subtitulo,
       x = "",
       y = "Número\n",
       caption = "</span><br>Elaborado por @segasi para el Buró de Investigación de ADN40 / Fuente: CSSE de la Universidad Johns Hopkins<br><br>Nota: La línea **<span style='color:#fa8072;'>roja</span>** muestra el promedio móvil de cinco días del número de muertes diarias.</span>") +
  tema +
  theme(plot.title = element_text(size = 35),
        plot.subtitle = element_text(size = 22),
        plot.caption = element_markdown(size = 18),
        axis.text.x = element_text(size = 13, face = "bold", family = "Didact Gothic Regular", angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 22),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        panel.spacing.x = unit(1.5, "lines"),
        strip.text = element_text(size = 18),
        strip.background = element_rect(fill = "grey70", color = "grey70")) +
  ggsave(str_c(ruta_graficas_america, "13_01_numero_diario_muertes_top_20_casos_misma_escala_", str_replace_all(str_replace_all(str_replace_all(Sys.Date() - 1, "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 12)


### Gráfica 13_02: Número de casos confirmados de Covid-19 que fallecieron diariamente en los 20 países con más muertes por esta enfermedad, escala libre ----
bd_america_st %>% 
  arrange(pais, fecha_corte) %>% 
  group_by(pais) %>%
  mutate(cambio_diario_muertes = muertes - lag(muertes)) %>% 
  ungroup() %>% 
  group_by(pais) %>% 
  mutate(total_muertes = max(muertes)) %>% 
  ungroup() %>% 
  group_by(fecha_corte) %>%
  mutate(ranking_total_muertes = rank(-total_muertes, 
                                      ties.method = "first")) %>% 
  ungroup() %>% 
  mutate(ranking_total_muertes = ifelse(fecha_corte == max(fecha_corte), ranking_total_muertes, NA),
         pais = fct_rev(fct_reorder(pais, total_muertes))) %>% 
  group_by(pais) %>% 
  mutate(ranking_total_muertes = na.locf(ranking_total_muertes, fromLast = T),
         promedio_movil_cinco_dias = rollmean(cambio_diario_muertes, k = 5, align = 'right', fill = NA)) %>% 
  ungroup() %>%
  filter(ranking_total_muertes <= 20) %>% 
  ggplot(aes(x = fecha_corte, y = cambio_diario_muertes)) +
  geom_col(color = "grey10") +
  geom_line(aes(y = promedio_movil_cinco_dias), color = "salmon", size = 1) +
  scale_y_continuous(labels = comma) +
  facet_wrap(~ pais, scales = "free_y") +
  labs(title = "Número de casos confirmados de Covid-19 que fallecieron\ndiariamente en los 20 países de América con más muertes",
       subtitle = str_c(subtitulo, " | Escala libre en el eje vertical"),
       x = "",
       y = "Número\n",
       caption = "</span><br>Elaborado por @segasi para el Buró de Investigación de ADN40 / Fuente: CSSE de la Universidad Johns Hopkins<br><br>Nota: La línea **<span style='color:#fa8072;'>roja</span>** muestra el promedio móvil de cinco días del número de muertes diarias.</span>") +
  tema +
  theme(plot.title = element_text(size = 35),
        plot.subtitle = element_text(size = 22),
        plot.caption = element_markdown(size = 18),
        axis.text.x = element_text(size = 13, face = "bold", family = "Didact Gothic Regular", angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 22),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        panel.spacing.x = unit(1.5, "lines"),
        strip.text = element_text(size = 18),
        strip.background = element_rect(fill = "grey70", color = "grey70")) +
  ggsave(str_c(ruta_graficas_america, "13_02_numero_diario_muertes_top_20_casos_escala_libre_", str_replace_all(str_replace_all(str_replace_all(Sys.Date() - 1, "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 12)



### Gráfica 14_01: Número de casos confirmados de Covid-19 que se recuperaron diariamente en los 20 países con más casos en América ----
bd_america_st %>% 
  filter(!str_detect(pais, "Crucero")) %>% 
  arrange(pais, fecha_corte) %>% 
  group_by(pais) %>%
  mutate(cambio_diario_recuperados = recuperados - lag(recuperados)) %>% 
  ungroup() %>% 
  group_by(pais) %>% 
  mutate(total_casos = max(recuperados)) %>% 
  ungroup() %>% 
  group_by(fecha_corte) %>%
  mutate(ranking_total_casos = rank(-total_casos, 
                                    ties.method = "first")) %>% 
  ungroup() %>% 
  mutate(ranking_total_casos = ifelse(fecha_corte == max(fecha_corte), ranking_total_casos, NA),
         pais = fct_rev(fct_reorder(pais, total_casos))) %>% 
  group_by(pais) %>% 
  mutate(ranking_total_casos = na.locf(ranking_total_casos, fromLast = T),
         promedio_movil_cinco_dias = rollmean(cambio_diario_recuperados, k = 5, align = 'right', fill = NA)) %>% 
  ungroup() %>%
  filter(ranking_total_casos <= 20) %>% 
  ggplot(aes(x = fecha_corte, y = cambio_diario_recuperados)) +
  geom_col(color = "#41ab5d") +
  geom_line(aes(y = promedio_movil_cinco_dias), color = "salmon", size = 1) +
  scale_y_continuous(labels = comma) +
  facet_wrap(~ pais) +
  labs(title = "Número de casos confirmados de Covid-19 que se recuperaron\ndiariamente en los 20 países de América con más casos",
       subtitle = subtitulo,
       x = "",
       y = "Número\n",
       caption = "</span><br>Elaborado por @segasi para el Buró de Investigación de ADN40 / Fuente: CSSE de la Universidad Johns Hopkins<br><br>Nota: La línea **<span style='color:#fa8072;'>roja</span>** muestra el promedio móvil de cinco días del número de casos recuperados diariamente.</span>") +
  tema +
  theme(plot.title = element_text(size = 35),
        plot.subtitle = element_text(size = 22),
        plot.caption = element_markdown(size = 18),
        axis.text.x = element_text(size = 13, face = "bold", family = "Didact Gothic Regular", angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 22),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        panel.spacing.x = unit(1.5, "lines"),
        strip.text = element_text(size = 18),
        strip.background = element_rect(fill = "grey70", color = "grey70")) +
  ggsave(str_c(ruta_graficas_america, "14_01_numero_diario_recuperados_top_20_casos_misma_escala_", str_replace_all(str_replace_all(str_replace_all(Sys.Date() - 1, "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 12)


### Gráfica 14_02: Número de casos confirmados de Covid-19 que se recuperaron diariamente en los 20 países con más casos en América, escala libre ----
bd_america_st %>% 
  filter(!str_detect(pais, "Crucero")) %>% 
  arrange(pais, fecha_corte) %>% 
  group_by(pais) %>%
  mutate(cambio_diario_recuperados = recuperados - lag(recuperados)) %>% 
  ungroup() %>% 
  group_by(pais) %>% 
  mutate(total_casos = max(recuperados)) %>% 
  ungroup() %>% 
  group_by(fecha_corte) %>%
  mutate(ranking_total_casos = rank(-total_casos, 
                                    ties.method = "first")) %>% 
  ungroup() %>% 
  mutate(ranking_total_casos = ifelse(fecha_corte == max(fecha_corte), ranking_total_casos, NA),
         pais = fct_rev(fct_reorder(pais, total_casos))) %>% 
  group_by(pais) %>% 
  mutate(ranking_total_casos = na.locf(ranking_total_casos, fromLast = T),
         promedio_movil_cinco_dias = rollmean(cambio_diario_recuperados, k = 5, align = 'right', fill = NA)) %>% 
  ungroup() %>%
  filter(ranking_total_casos <= 20) %>% 
  ggplot(aes(x = fecha_corte, y = cambio_diario_recuperados)) +
  geom_col(color = "#41ab5d") +
  geom_line(aes(y = promedio_movil_cinco_dias), color = "salmon", size = 1) +
  scale_y_continuous(labels = comma) +
  facet_wrap(~ pais, scales = "free_y") +
  labs(title = "Número de casos confirmados de Covid-19 que se recuperaron\ndiariamente en los 20 países de América con más casos",
       subtitle = str_c(subtitulo, " | Escala libre en el eje vertical"),
       x = "",
       y = "Número\n",
       caption = "</span><br>Elaborado por @segasi para el Buró de Investigación de ADN40 / Fuente: CSSE de la Universidad Johns Hopkins<br><br>Nota: La línea **<span style='color:#fa8072;'>roja</span>** muestra el promedio móvil de cinco días del número de casos recuperados diariamente.</span>") +
  tema +
  theme(plot.title = element_text(size = 35),
        plot.subtitle = element_text(size = 22),
        plot.caption = element_markdown(size = 18),
        axis.text.x = element_text(size = 13, face = "bold", family = "Didact Gothic Regular", angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 22),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        panel.spacing.x = unit(1.5, "lines"),
        strip.text = element_text(size = 18),
        strip.background = element_rect(fill = "grey70", color = "grey70")) +
  ggsave(str_c(ruta_graficas_america, "14_02_numero_diario_recuperados_top_20_casos_escala_libre_", str_replace_all(str_replace_all(str_replace_all(Sys.Date() - 1, "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 12)


### Gráfica 15: Porcentaje de casos confirmados que murieron, por país y territorio ----
foo <- 
  bd_america %>% 
  mutate(etiqueta_grandes = ifelse(por_casos_conf_fallecieron > 14, str_c("Tasa: ", por_casos_conf_fallecieron, "% ", " (", comma(muertes, accuracy = 1), " muertos | ",  comma(casos_confirmados, accuracy = 1), " casos)"), ""),
         etiqueta_chicos = ifelse(por_casos_conf_fallecieron <= 14, str_c(por_casos_conf_fallecieron, "%", " (", comma(muertes, accuracy = 1), " | ", comma(casos_confirmados, accuracy = 1), ")"), ""),
         color_pais = ifelse(pais == "México", "a", "b")) %>%
  filter(muertes > 0) 

foo %>% 
  ggplot(aes(x = fct_reorder(pais, por_casos_conf_fallecieron), 
             y = por_casos_conf_fallecieron, 
             fill = color_pais)) +
  geom_col() +
  scale_y_continuous(limits = c(0, max(foo$por_casos_conf_fallecieron) + 1),
                     expand = c(0, 0)) +
  scale_fill_manual(values = c("salmon", "black")) +
  coord_flip() +
  geom_text(aes(label = etiqueta_grandes), color = "white", fontface = "bold", family = "Roboto", hjust = 1.05, size = 5) +
  geom_text(aes(label = etiqueta_chicos), color = "grey30", fontface = "bold", family = "Roboto", hjust = -0.05, size = 5) +
  labs(title = "Porcentaje de casos confirmados de Covid-19\nque murieron en países de América*",
       subtitle = subtitulo,
       x = NULL,
       y = NULL,
       fill = NULL,
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40\nFuentes: OMS y el CSSE de la Universidad de Johns Hopkins\n\nNotas: *Solo se incluyen países de América con una población de 1 millón o más de personas. La categoría \"casos \nconfirmados\"se refiere a las personas que dieron positivo en la prueba de Covid-19. La variable graficada fue calculada\ndividiendo el número de muertes entre el número de casos confirmados, por 100.") +
  tema +
  theme(plot.title = element_text(size = 36), 
        plot.subtitle = element_text(size = 26), 
        panel.grid = element_blank(), 
        axis.text.x = element_blank(),
        legend.position = "none") +
  ggsave(str_c(ruta_graficas_america, "15_porcentaje_casos_confirmados_que_murieron_en_america_", str_replace_all(str_replace_all(str_replace_all(Sys.Date() - 1, "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 13.2, height = 15) 
