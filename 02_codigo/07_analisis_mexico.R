### Limpiar ambiente ----
remove(list = ls())

# devtools::install_github("pablorm296/covidMex")
### Cargar paquetes, definir setup y tema de gráficas ----
source("02_codigo/00_paquetes_setup_tema.R") 

### Definir cortes de datos ----
subtitulo_mx <-  str_c("Cifras a las 19:00 hrs. del ", 
                       day(Sys.Date()),
                       " de abril de 2020 (CDMX)")

subtitulo_mx

### Generar folder para guardar las gráficas ----
dir_graficas <- 
  dir.create(file.path("03_graficas/03_graficas_analisis_mexico/", 
                       str_c("graficas_", str_replace_all(Sys.Date(), "-", "_"))))
ruta_graficas_mx <- str_c("03_graficas/03_graficas_analisis_mexico/", 
                          str_c("graficas_", str_replace_all(Sys.Date(), "-", "_"), "/"))

ruta_graficas_mx

### Importar serie de tiempo de datos de casos y muertes por COVID ----
muertes_mx_st <- 
  read_csv("https://raw.githubusercontent.com/mariorz/covid19-mx-time-series/master/data/covid19_deaths_mx.csv") %>% 
  pivot_longer(-Estado, names_to = "fecha_corte", values_to = "muertes") %>% 
  mutate(fecha_corte = dmy(fecha_corte))

casos_mx_st <- 
  read_csv("https://raw.githubusercontent.com/mariorz/covid19-mx-time-series/master/data/covid19_confirmed_mx.csv") %>% 
  pivot_longer(-Estado, names_to = "fecha_corte", 
               values_to = "casos") %>% 
  mutate(fecha_corte = dmy(fecha_corte))


max(muertes_mx_st$fecha_corte)
max(casos_mx_st$fecha_corte)

### Importar datos del último corte curados por Serendipia ----
fecha_hoy <- Sys.Date()

datos_dia <- 
  getData(where = "Mexico", 
          type = "confirmed", 
          date = str_c(day(fecha_hoy), "/", month(fecha_hoy), "/", year(fecha_hoy)),
          source = "Serendipia", neat = F) %>% 
  clean_names() %>% 
  mutate(fecha_corte = fecha_hoy) %>% 
  filter(!str_detect(n_caso, "Fuente|Recuper"))

datos_dia %>% 
  tail()


### Generar nuevo tibble con series de tiempo de casos y muertes ----
mx_st <- 
  casos_mx_st%>% 
  left_join(muertes_mx_st, by = c("Estado", "fecha_corte"))

max(mx_st$fecha_corte)


### Generar tibble con datos NACIONALES diarios ----
mx_diario_nal <- 
  mx_st %>% 
  group_by(fecha_corte) %>% 
  summarise(casos_acumulados = sum(casos),
            muertes_acumulados = sum(muertes)) %>% 
  ungroup() %>%  
  # Corregir dos datos al comienzo de la serie
  mutate(casos_acumulados = ifelse(fecha_corte == as_date("2020-02-27"), 1, casos_acumulados),
         casos_acumulados = ifelse(fecha_corte == as_date("2020-02-28"), 2, casos_acumulados))

### Generar tibble con datos ESTATALES del último corte ----
mx_ultimo_corte_edo <- 
  mx_st %>% 
  filter(fecha_corte == max(fecha_corte))

### Gráfica 01: Número acumulado de casos confirmados de Covid-19 confirmados en México ----
foo <- 
  mx_diario_nal %>% 
  mutate(puntito_final = ifelse(fecha_corte == max(fecha_corte), casos_acumulados, NA),
         texto_puntito_final = ifelse(!is.na(puntito_final), str_c(comma(puntito_final), " casos"), "")) 

foo %>%
  ggplot(aes(x = fecha_corte)) +
  geom_line(aes(y = casos_acumulados),
            color = "#1E6847", size = 2, alpha = 0.9) +
  geom_point(aes(y = puntito_final),
             color = "#1E6847", size = 4, alpha = 1) +
  geom_text(aes(y = puntito_final, label = texto_puntito_final), 
            size = 6, 
            fontface = "bold",
            color = "grey30",
            hjust = 0.5,
            vjust = -1) +
  scale_x_date(breaks = seq(from = as_date("2020-02-27"), 
                            to = max(foo$fecha_corte), 
                            by = 1), 
               date_labels = "%b-%d", 
               limits = c(as_date("2020-02-27"), max(foo$fecha_corte))) +
  scale_y_continuous(breaks = seq(0, 5000, 500),
                     limits = c(-10, max(foo$casos_acumulados) + max(foo$casos_acumulados)*0.1),
                     expand = c(0, 0),
                     labels = comma) +
  labs(title = "Número acumulado de casos confirmados de Covid-19 en México",
       subtitle = subtitulo_mx,
       x = "",
       y = "Número\n",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud, curados por @mariorz.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.ticks.y = element_blank()) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0)) +
  ggsave(str_c(ruta_graficas_mx, "01_evolucion_casos_acumulados_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16.2, height = 9)


### Gráfica 02: Número de nuevos casos de Covid-19 confirmados diariamente en México ----
foo <- 
  mx_diario_nal %>% 
  filter(fecha_corte > as_date("2020-02-26")) %>% 
  mutate(num_casos_diarios = casos_acumulados - lag(casos_acumulados),
         num_casos_diarios = ifelse(is.na(num_casos_diarios) & fecha_corte == as_date("2020-02-27"), 1, num_casos_diarios),
         promedio_movil_cinco_dias = rollmean(num_casos_diarios, k = 5, align = 'right', fill = NA)) 

foo %>% 
  tail()

foo %>% 
  ggplot(aes(x = fecha_corte, y = num_casos_diarios)) +
  geom_col(fill = "#1E6847", alpha = 0.9) +
  geom_line(aes(y = promedio_movil_cinco_dias), color = "salmon", size = 2) +
  scale_x_date(date_breaks = "1 day", date_labels = "%b-%d", expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 500, 50), 
                     expand = c(0, 0),
                     limits = c(0, max(foo$num_casos_diarios) + max(foo$num_casos_diarios)*0.1)) +
  labs(title = "Número de casos nuevos de Covid-19 confirmados diariamente en México",
       subtitle = subtitulo_mx,
       x = "",
       y = "Número\n",
       caption = "</span><br>Elaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud, curados por @mariorz.<br><br>Nota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de<br>caso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".<br><br>La línea **<span style='color:#fa8072;'>roja</span>** muestra el promedio móvil de cinco días del número de casos confirmados.</span>") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22),
        plot.caption = element_markdown(size = 16),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.ticks.y = element_blank()) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0)) +
  ggsave(str_c(ruta_graficas_mx, "02_evolucion_casos_confirmados_diariamente_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16.2, height = 9)



### Gráfica 03: Número de casos de Covid-19 confirmados en cada entidad ----
mx_ultimo_corte_edo %>% 
  ggplot(aes(x = casos, y = fct_reorder(Estado, casos))) +
  geom_col(fill = "#1E6847", alpha = 0.9) +
  scale_x_continuous(breaks = seq(0, 2000, 100), 
                     limits = c(0, max(mx_ultimo_corte_edo$casos) + max(mx_ultimo_corte_edo$casos)*0.05),
                     expand = c(0, 0),
                     labels = comma_format(accuracy = 1)) +
  labs(title = "Número de casos de Covid-19 confirmados en cada entidad",
       subtitle = subtitulo_mx,
       x = "\nNúmero     ",
       y = "",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud, curados por @mariorz.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22)) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0)) +
  ggsave(str_c(ruta_graficas_mx, "03_numero_casos_por_entidad_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16.2, height = 12)


### Gráfica 04: Treemap del número de casos de Covid-19 confirmados en cada entidad ----
mx_ultimo_corte_edo %>%
  ggplot(aes(area = casos, fill = log(casos))) +
  geom_treemap(col = "white") +
  geom_treemap_text(aes(label = Estado), fontface = "bold", color = "white", alpha = 1, min.size = 0, grow = F) +
  geom_treemap_text(aes(label = paste(comma(casos, accuracy = 1), "casos", sep = " ")), color = "white", padding.y = unit(7, "mm"),min.size = 0) +
  geom_treemap_text(aes(label = paste(comma(casos/sum(casos)*100, accuracy = 1), "% de los casos", sep = "")), color = "white", padding.y = unit(14, "mm"), min.size = 0, size = 14) +
  scale_fill_gradient(low = "grey95", high = "#1E6847", guide = guide_colorbar(barwidth = 18, nbins = 6), labels = comma, breaks = pretty_breaks(n = 6)) +
  labs(title = "Casos confirmados de Covid-19 en cada entidad",
       subtitle = subtitulo_mx,
       x = NULL,
       y = NULL,
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud, curados por @mariorz.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de caso sospechoso\ny que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22),
        legend.position = "none") +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0)) +
  ggsave(str_c(ruta_graficas_mx, "04_numero_casos_por_entidad_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 12)



### Gráfica 05: Heatmap del número acumulado de casos confirmados de Covid-19 en cada entidad de México ----
mx_st %>% 
  filter(fecha_corte > as_date("2020-02-27")) %>% 
  ggplot(aes(x = fecha_corte, 
             y = fct_rev(Estado),
             fill = log(casos + 1))) +
  geom_tile(color = "grey60") +
  scale_x_date(date_breaks = "1 day", date_labels = "%b-%d", expand = c(0, 0)) +
  scale_fill_gradient(low = "#ffffff", 
                      high = "#1E6847", 
                      breaks = 0:7,
                      labels = c(str_c("0", " (mín.)"), "", "", "", "", "", "", str_c(comma(max(mx_st$casos)), " (máx.)"))) +
  labs(title = "Número acumulado de casos confirmados de Covid-19 en cada entidad de México",
       subtitle = subtitulo_mx,
       x = "",
       y = NULL,
       fill = "Número acumulado (log)  ",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40.\nFuente: datos de la Secretaría de Salud, curados por @mariorz.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(plot.title = element_text(size = 28),
        legend.position = c(0.86, -0.15), 
        legend.direction = "horizontal",
        legend.key.width = unit(1.3, "cm"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks.y = element_blank()) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0))  +
  ggsave(str_c(ruta_graficas_mx, "05_evolucion_casos_confirmados_por_edo_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 17, height = 13)



### Gráfica 06_01: Evolución del número acumulado de casos confirmados desde el primer caso confirmado en las entidades de México ----
foo <- 
  mx_st %>% 
  filter(fecha_corte > as_date("2020-02-26")) %>%
  # Corregir dos datos al comienzo de la serie
  mutate(casos = ifelse(fecha_corte == as_date("2020-02-27") & Estado == "Ciudad de México", 1, casos),
         casos = ifelse(fecha_corte == as_date("2020-02-28") & Estado == "Ciudad de México", 1, casos)) %>% 
  mutate(Estado = case_when(Estado == "Ciudad de México" ~ "CDMX",
                            Estado == "Baja California" ~ "BC",
                            Estado == "Baja California Sur" ~ "BCS",
                            Estado == "Nuevo León" ~ "NL",
                            Estado == "San Luis Potosí" ~ "SLP",
                            TRUE ~ Estado)) %>%
  group_by(Estado) %>%
  mutate(primer_caso = ifelse(casos > 0 & fecha_corte == as_date("2020-02-27") | casos > 0 & lag(casos) == 0 & Estado != "CDMX", 1, NA),
         dummy_dias_primer_caso = primer_caso) %>%
  fill(dummy_dias_primer_caso, .direction = "down") %>% 
  mutate(dias_primer_caso = cumsum(replace_na(dummy_dias_primer_caso, 0)) - 1) %>% 
  ungroup() %>% 
  mutate(puntito_final = ifelse(fecha_corte == max(fecha_corte), casos, NA), 
         etiquetas_entidad = ifelse(fecha_corte == max(fecha_corte) & casos >= 100 | fecha_corte == max(fecha_corte) & dias_primer_caso >= 30, Estado, ""),
         etiquetas_entidad_log = ifelse(fecha_corte == max(fecha_corte), Estado, "")) %>% 
  filter(dias_primer_caso > -1) 


foo %>% 
  ggplot(aes(x = dias_primer_caso, 
             y = casos, 
             group = Estado)) +
  geom_line(size = 1, 
            color = "#1E6847", 
            alpha = 0.6) +
  geom_point(aes(x = dias_primer_caso, 
                 y = puntito_final),
             size = 2, 
             color = "#1E6847",
             alpha = 0.8) +
  geom_text_repel(aes(label = etiquetas_entidad), 
                  # vjust = -0.7,
                  color = "grey30",
                  segment.color = "grey70",
                  # bg.colour = 'white',
                  fontface = "bold",
                  size = 5) +
  scale_x_continuous(breaks = c(seq(0, 100, 5), max(foo$dias_primer_caso)), limits = c(0, max(foo$dias_primer_caso) + max(foo$dias_primer_caso)*0.01)) +
  scale_y_continuous(limits = c(0, max(foo$casos) + max(foo$casos)*0.05),
                     label = comma, 
                     breaks = seq(0, 2000, 100)) +
  labs(title = "Evolución del número acumulado de casos confirmados desde el primer caso\nconfirmado en las entidades de México*",
       subtitle = subtitulo_mx,
       x = "\nDías desde el primer caso confirmado  ",
       y = "Número de casos  \n",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud, curados por @mariorz.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(legend.position = "none")  +
  ggsave(str_c(ruta_graficas_mx, "06_01_evolucion_casos_entidades_mexico_desde_primer_caso_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)


### Gráfica 06_02: Evolución del número acumulado de casos confirmados desde el primer caso confirmado en las entidades de México, log 10 ----
set.seed(1)
foo %>% 
  ggplot(aes(x = dias_primer_caso, 
             y = casos, 
             group = Estado)) + 
  geom_line(size = 1, 
            color = "#1E6847", 
            alpha = 0.4) +
  geom_point(aes(x = dias_primer_caso, 
                 y = puntito_final),
             size = 2, 
             color = "#1E6847",
             alpha = 0.5) +
  geom_text_repel(aes(label = etiquetas_entidad_log), 
                  check_overlap = F,
                  force = 3,
                  # vjust = -0.7,
                  color = "grey30",
                  # bg.colour = 'white',
                  fontface = "bold",
                  size = 5) +
  scale_x_continuous(breaks = c(seq(0, 100, 5), max(foo$dias_primer_caso)), limits = c(0, max(foo$dias_primer_caso) + max(foo$dias_primer_caso)*0.01)) +
  scale_y_log10(breaks = c(1, 3, 10, 30, 100, 300, 1000, 3e3, 10e3, 3e4, 10e4, 3e5, 10e5, 3e6, 10e6, 3e7, 10e7),
                labels = comma_format(accuracy = 1)) +
  labs(title = "Evolución del número acumulado de casos confirmados desde el primer caso\nconfirmado en las entidades de México*",
       subtitle = str_c(subtitulo_mx, " | Distancia logarítmica en las etiquetas del eje vertical"),
       x = "\nDías desde el primer caso confirmado  ",
       y = "Número de casos (log 10)\n",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud, curados por @mariorz.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(legend.position = "none")  +
  ggsave(str_c(ruta_graficas_mx, "06_02_evolucion_casos_entidades_mexico_desde_primer_caso_log10_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 12)





### Gráfica 07: Número de casos confirmados de Covid-19, por género y edad ----
foo <- 
  datos_dia %>% 
  mutate(rango_edad = case_when(edad <= 20 ~ "20 años o menos",
                                edad > 20 & edad <= 30 ~ "21-30",
                                edad > 30 & edad <= 40 ~ "31-40",
                                edad > 40 & edad <= 50 ~ "41-50",
                                edad > 50 & edad <= 60 ~ "51-60",
                                edad > 60 & edad <= 70 ~ "61-70",
                                edad > 70 & edad <= 80 ~ "71-80",
                                edad > 80 ~ "Más de 80 años"),
         genero = ifelse(sexo == "FEMENINO", "Mujeres", "Hombres"),
         genero = fct_relevel(genero, "Mujeres", "Hombres")) %>% 
  count(genero, rango_edad)

## Verificar
foo %>% 
  mutate(total = sum(n))

foo %>% 
  ggplot(aes(x = str_wrap(rango_edad, width = 8), y = n, fill = n)) +
  geom_col(fill = "#1E6847", alpha = 0.9) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, max(foo$n) + max(foo$n)*0.05),
                     breaks = seq(0, 900, 50)) +
  facet_wrap(~ genero) +
  labs(x = NULL, 
       y = "Número    \n") +
  labs(title = "Casos confirmados de Covid-19, por género y rango de edad",
       subtitle = subtitulo_mx,
       x = NULL,
       y = "Número\n   ",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud obtenidos a través del paquete {covidMex}\ncon cifras curadas por @SerendipiaData.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22),
        axis.text.x = element_text(size = 15),
        legend.position = "none",
        strip.text = element_text(size = 18)) +
  ggsave(str_c(ruta_graficas_mx, "07_numero_casos_por_genero_edad", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)


### Gráfica 08: Número acumulado de muertes por Covid-19 en México ----
foo <- 
  mx_diario_nal %>% 
  filter(fecha_corte > as_date("2020-03-18")) %>% 
  mutate(puntito_final = ifelse(fecha_corte == max(fecha_corte), muertes_acumulados, NA),
         texto_puntito_final = ifelse(!is.na(puntito_final), str_c(comma(puntito_final), " casos"), "")) 

foo %>%
  ggplot(aes(x = fecha_corte)) +
  geom_line(aes(y = muertes_acumulados),
            color = "grey10", size = 2, alpha = 0.9) +
  geom_point(aes(y = puntito_final),
             color = "grey10", size = 4, alpha = 1) +
  geom_text(aes(y = puntito_final, label = texto_puntito_final), 
            size = 6, 
            fontface = "bold",
            color = "grey30",
            hjust = 0.5,
            vjust = -1) +
  scale_x_date(breaks = seq(from = as_date("2020-03-19"), 
                            to = max(foo$fecha_corte), 
                            by = 1), 
               date_labels = "%b-%d", 
               limits = c(as_date("2020-03-19"), max(foo$fecha_corte))) +
  scale_y_continuous(breaks = seq(0, 500, 25),
                     limits = c(-10, max(foo$muertes_acumulados) + max(foo$muertes_acumulados)*0.1),
                     expand = c(0, 0),
                     labels = comma) +
  labs(title = "Número acumulado de muertes por Covid-19 en México",
       subtitle = subtitulo_mx,
       x = "",
       y = "Número\n",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud, curados por @mariorz.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.ticks.y = element_blank()) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0)) +
  ggsave(str_c(ruta_graficas_mx, "08_evolucion_muertes_acumuladas_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16.2, height = 9)




### Gráfica 09: Número de casos confirmados de Covid-19 que fallecieron diariamente en México ----
foo <- 
  mx_diario_nal %>% 
  filter(fecha_corte > as_date("2020-03-18")) %>%
  mutate(num_muertes_diarias = muertes_acumulados - lag(muertes_acumulados),
         promedio_movil_cinco_dias = rollmean(num_muertes_diarias, k = 5, align = 'right', fill = NA)) 

foo %>% 
  tail()

foo %>%
  ggplot(aes(x = fecha_corte, y = num_muertes_diarias)) +
  geom_col(fill = "grey10", alpha = 0.9) +
  geom_line(aes(y = promedio_movil_cinco_dias), color = "salmon", size = 2) +
  scale_x_date(date_breaks = "1 day", date_labels = "%b-%d", expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 200, 5), 
                     limits = c(0, (max(foo$num_muertes_diarias) + max(foo$num_muertes_diarias)*0.1)),
                     expand = c(0, 0)) +
  labs(title = "Número de casos confirmados de Covid-19 que fallecieron diariamente en México",
       subtitle = subtitulo_mx,
       x = "",
       y = "Número\n",
       caption = "</span><br>Elaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud, curados por @mariorz.<br><br>Nota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de<br>caso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".<br><br>La línea **<span style='color:#fa8072;'>roja</span>** muestra el promedio móvil de cinco días del número de muertes diarias.</span>") +
  tema +
  theme(plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 22),
        plot.caption = element_markdown(size = 16),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.ticks.y = element_blank()) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0)) +
  ggsave(str_c(ruta_graficas_mx, "09_evolucion_muertes_diariamente_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16.5, height = 9)



### Gráfica 10: Número acumulado de casos confirmados de Covid-19 que fallecieron en México, por entidad ----
mx_st %>% 
  filter(fecha_corte == max(fecha_corte)) %>% 
  ggplot(aes(area = muertes, fill = log(muertes))) +
  geom_treemap(col = "white") +
  geom_treemap_text(aes(label = Estado), fontface = "bold", color = "white", alpha = 1, min.size = 0, grow = F) +
  geom_treemap_text(aes(label = paste(comma(muertes, accuracy = 1), "muertes", sep = " ")), color = "white", padding.y = unit(7, "mm"),min.size = 0) +
  geom_treemap_text(aes(label = paste(comma(muertes/sum(muertes)*100, accuracy = 1), "% del total", sep = "")), color = "white", padding.y = unit(14, "mm"), min.size = 0, size = 14) +
  scale_fill_gradient(low = "grey95", high = "black", guide = guide_colorbar(barwidth = 18, nbins = 6), labels = comma, breaks = pretty_breaks(n = 6)) +
  labs(title = "Número acumulado de casos confirmados de Covid-19 que fallecieron en\nMéxico, por entidad",
       subtitle = subtitulo_mx,
       x = NULL,
       y = NULL,
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud, curados por @mariorz.") +
  tema +
  theme(legend.position = "none", 
        plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 20)) +
  ggsave(str_c(ruta_graficas_mx, "10_distribucion_muertes_covid19_mexico_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)


### Gráfica 11: Cambio porcentual diario del número de casos confirmados y muertes reportadas en México desde el 24 de marzo de 2020 ----
mx_diario_nal %>% 
  left_join(mx_muertes_x_dia , by = "fecha_corte") %>% 
  mutate(muertes = ifelse(is.na(muertes), 0, muertes),
         Casos = round((casos_acumulados - lag(casos_acumulados))/lag(casos_acumulados)*100, 1),
         Muertes = round((muertes - lag(muertes))/lag(muertes)*100, 1)) %>% 
  # tail()
  pivot_longer(Casos:Muertes,
               names_to = "tipo",
               values_to = "cambio") %>%
  filter(fecha_corte >= as_date("2020-03-28")) %>%
  # print(n = Inf)
  ggplot(aes(x = fecha_corte,
             y = cambio,
             color = tipo)) +
  geom_line(size = 2) +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%b-%d") +
  scale_y_continuous(limits = c(0, 50),
                     breaks = seq(0, 50, 10)) +
  scale_color_manual(values = c("#1E6847", "grey50"), 
                     labels = c("Casos confirmados", "Muertes")) +
  labs(title = "Cambio porcentual diario del número de casos confirmados y muertes provocadas\npor Covid-19 en México a partir del 27 de marzo",
       subtitle = subtitulo_mx,
       x = "\n",
       y = "Cambio porcentual\n",
       color = NULL,
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud, curados por @mariorz.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".\nEl 27 de marzo fue el primer día con más de 10 muertes acumuladas por Covid-19 en México") +
  tema +
  theme(legend.position = c(0.838, 0.87),
        legend.text = element_text(size = 20),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ggsave(str_c(ruta_graficas_mx, "11_cambio_porcentual_diario_casos_y_muertes_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)



