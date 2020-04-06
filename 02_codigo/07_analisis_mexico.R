### Limpiar ambiente ----
remove(list = ls())

# devtools::install_github("pablorm296/covidMex")
### Cargar paquetes, definir setup y tema de gráficas ----
source("02_codigo/00_paquetes_setup_tema.R") 

### Definir cortes de datos ----
subtitulo_mx <-  str_c("Cifras a las 19:00 hrs. del ", 
                       day(Sys.Date()),
                       " de abril de 2020 (CDMX)")

### Generar folder para guardar las gráficas ----
dir_graficas <- 
  dir.create(file.path("03_graficas/03_graficas_analisis_mexico/", 
                       str_c("graficas_", str_replace_all(Sys.Date(), "-", "_"))))
ruta_graficas_mx <- str_c("03_graficas/03_graficas_analisis_mexico/", 
                          str_c("graficas_", str_replace_all(Sys.Date(), "-", "_"), "/"))

### Importar datos del 16 de marzo curados por Katia Guzmán ----
mx_guzmart <- 
  read_excel("01_datos/covid19_sars/guzmart/covid_mex_20200316.xlsx")


### Renombrar y transformar variables ----
mx_guzmart <- 
  mx_guzmart %>% 
  rename(n_caso = num_caso, 
         estado = ent,
         fecha_de_inicio_de_sintomas = fecha_inicio,
         identificacion_de_covid_19_por_rt_pcr_en_tiempo_real = identificado,
         fecha_del_llegada_a_mexico = fecha_llegada_mexico) %>% 
  mutate(fecha_de_inicio_de_sintomas = as_date(fecha_de_inicio_de_sintomas),
         fecha_corte = as_date(fecha_corte)) 


### Importar datos de Covid19 curados por Serendipia ----

# Generar tibble vacío ----

# A este le pegaré los datos de los diferentes cortes en los siguientes pasos

mx_serendipia <- 
  tibble(n_caso = as.character(NA),
         estado = NA,
         sexo = NA,
         edad = NA,
         fecha_de_inicio_de_sintomas = NA,
         identificacion_de_covid_19_por_rt_pcr_en_tiempo_real = NA,
         procedencia = NA, 
         fecha_del_llegada_a_mexico = NA,
         fecha_corte = NA)

# Obtener cortes de marzo ----

# Definir fechas de las cuales obtener cortes

# A pesar de que el primer corte publicado por la S. Salud es del 16 de marzo, comienzo a usar los datos de Serendipia el 17 de marzo para utilizar el archivo de Katia Guzmán del 16 de marzo. Ésto, porque el archivo de Katia incluye la fecha en que se publicaron los datos de los 82 casos registrados entre el 27 de febrero y el 16 de marzo.

dia_marzo <-  17:31

# Loop para obtener datos
for (i in seq_along(dia_marzo)) {
  datos_dia <- 
    getData(where = "Mexico", 
            type = "confirmed", 
            # date = "17/03/2020",
            date = str_c(dia_marzo[i], "/03/2020"),
            source = "Serendipia", neat = F) %>% 
    clean_names() %>% 
    mutate(fecha_corte = make_date("2020", "03", as.character(dia_marzo[i])),
           n_caso = as.character(n_caso)) %>% 
    filter(!str_detect(n_caso, "Fuente|Recuper"))
  
  mx_serendipia <- 
    mx_serendipia %>% 
    bind_rows(datos_dia) %>% 
    mutate(n_caso = as.character(n_caso))
}

# Obtener cortes de abril ----

# Definir fechas de las cuales obtener cortes
dia_abril <-  1:day(Sys.Date() - 1)


# Loop para obtener datos
for (i in seq_along(dia_abril)) {
  datos_dia <- 
    getData(where = "Mexico", 
            type = "confirmed", 
            # date = "17/03/2020",
            date = str_c("0", dia_abril[i], "/04/2020"),
            source = "Serendipia", neat = F) %>% 
    clean_names() %>% 
    mutate(fecha_corte = make_date("2020", "04", as.character(dia_abril[i])),
           n_caso = as.character(n_caso)) %>% 
    filter(!str_detect(n_caso, "Fuente|Recuper"))
  
  mx_serendipia <- 
    mx_serendipia %>% 
    bind_rows(datos_dia) %>% 
    mutate(n_caso = as.character(n_caso))
}

### Eliminar observaciones con missing values ----
mx_serendipia <- 
  mx_serendipia %>% 
  filter(!is.na(n_caso)) 

mx_serendipia <- 
  mx_serendipia %>% 
  mutate(n_caso = as.numeric(n_caso))

### Verificar número de renglones del último corte ----
mx_serendipia %>% 
  filter(fecha_corte == max(fecha_corte)) %>% 
  nrow()

### Limpiar y cambiar tipo de algunas variables en mx_guzman y mx_serendipia----
mx_guzmart <-
  mx_guzmart %>% 
  # Poner nombres de estados en mayúsculas y minúsculas
  mutate(estado = str_to_title(estado),
         estado = str_replace(estado, " De ", " de "),
         estado = ifelse(str_detect(estado, "Quer"), "Querétaro", estado),
         estado = str_replace(estado, "\\r", ""),
         estado = str_replace(estado, "\\n", " "),
         fecha_corte = as_date(fecha_corte)) 

mx_serendipia <- 
  mx_serendipia %>% 
  mutate(estado = str_to_title(estado),
         estado = str_replace(estado, " De ", " de "),
         estado = ifelse(str_detect(estado, "Quer"), "Querétaro", estado),
         estado = str_replace(estado, "\\r", ""),
         estado = str_replace(estado, "\\n", " "),
         fecha_corte = as_date(fecha_corte))



### Importar datos de muertes por COVID ----
mx_muertes_x_dia <- 
  read_excel("01_datos/ssa/datos_muertos.xlsx", 
             sheet = "por_dia") %>% 
  mutate(fecha_corte = as_date(fecha_corte))

### Importar datos poblacionales de CONAPO ----
source("02_codigo/08_importar_preparar_datos_conapo.R")



### Generar tibble con datos NACIONALES diarios ----
foo_guzmart <- 
  mx_guzmart %>% 
  group_by(fecha_corte) %>% 
  summarise(casos_diarios = n()) %>% 
  ungroup() %>% 
  mutate(casos_acumulados = cumsum(casos_diarios)) %>% 
  select(-casos_diarios)

foo_serendipia <- 
  mx_serendipia %>% 
  group_by(fecha_corte) %>% 
  summarise(casos_acumulados = n()) %>% 
  ungroup()  

mx_diario_nal <- 
  rbind(foo_guzmart, foo_serendipia)


### Generar tibble con datos ESTATALES diarios ----
foo_guzmart_edo <- 
  mx_guzmart %>% 
  group_by(estado, fecha_corte) %>% 
  summarise(casos_diarios = n()) %>% 
  ungroup() %>% 
  group_by(estado) %>% 
  mutate(casos_acumulados = cumsum(casos_diarios)) %>% 
  select(-casos_diarios) %>% 
  ungroup()

foo_serendipia_edo <- 
  mx_serendipia %>% 
  group_by(estado, fecha_corte) %>% 
  summarise(casos_acumulados = n()) %>% 
  ungroup()  

# Unir y reordenar
mx_diario_edo <- 
  rbind(foo_guzmart_edo , foo_serendipia_edo) %>% 
  arrange(estado, fecha_corte)

# Completar valores faltantes
mx_diario_edo <- 
  mx_diario_edo %>% 
  # Agregar siete renglones para que cuando use complete() tenga datos en todas las fechas
  add_row(estado = "Aguascalientes", fecha_corte = as_date("2020-03-02"), casos_acumulados = 0) %>%
  add_row(estado = "Aguascalientes", fecha_corte = as_date("2020-03-03"), casos_acumulados = 0) %>%
  add_row(estado = "Aguascalientes", fecha_corte = as_date("2020-03-04"), casos_acumulados = 0) %>%
  add_row(estado = "Aguascalientes", fecha_corte = as_date("2020-03-05"), casos_acumulados = 0) %>%
  add_row(estado = "Aguascalientes", fecha_corte = as_date("2020-03-08"), casos_acumulados = 0) %>%
  add_row(estado = "Aguascalientes", fecha_corte = as_date("2020-03-09"), casos_acumulados = 0) %>%
  add_row(estado = "Aguascalientes", fecha_corte = as_date("2020-03-10"), casos_acumulados = 0) %>%
  arrange(estado, fecha_corte) %>%   
  complete(estado, fecha_corte) %>%
  group_by(estado) %>%
  mutate(casos_acumulados = na.locf(casos_acumulados, fromLast = F, na.rm = FALSE), 
         casos_acumulados = replace_na(casos_acumulados, replace = 0)) %>%
  ungroup()

### Generar tibble con datos ESTATALES del último corte ----
mx_ultimo_corte_edo <- 
  mx_serendipia %>% 
  filter(fecha_corte == max(fecha_corte)) %>% 
  group_by(estado) %>% 
  summarise(casos_acumulados = n()) %>% 
  ungroup()


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
  scale_y_continuous(breaks = seq(0, 3000, 200),
                     limits = c(-10, max(foo$casos_acumulados) + max(foo$casos_acumulados)*0.1),
                     expand = c(0, 0),
                     labels = comma) +
  labs(title = "Número acumulado de casos confirmados de Covid-19 en México",
       subtitle = subtitulo_mx,
       x = "",
       y = "Número\n",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud obtenidos a través del paquete {covidMex}\ncon cifras curadas por @guzmart_ y  @SerendipiaData.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
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
mx_diario_nal %>% 
  mutate(num_casos_diarios = casos_acumulados - lag(casos_acumulados),
         num_casos_diarios = ifelse(is.na(num_casos_diarios) & fecha_corte == as_date("2020-02-27"), 1, num_casos_diarios)) %>% 
  ggplot(aes(x = fecha_corte, y = num_casos_diarios)) +
  geom_col(fill = "#1E6847", alpha = 0.9) +
  scale_x_date(date_breaks = "1 day", date_labels = "%b-%d", expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 400, 20), expand = c(0, 0)) +
  labs(title = "Número de casos nuevos de Covid-19 confirmados diariamente en México",
       subtitle = subtitulo_mx,
       x = "",
       y = "Número\n",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud obtenidos a través del paquete {covidMex}\ncon cifras curadas por @guzmart_ y  @SerendipiaData.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.ticks.y = element_blank()) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0)) +
  ggsave(str_c(ruta_graficas_mx, "02_evolucion_casos_confirmados_diariamente_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16.2, height = 9)



### Gráfica 03: Número de casos de Covid-19 confirmados en cada entidad ----
mx_ultimo_corte_edo %>% 
  ggplot(aes(x = casos_acumulados, y = fct_reorder(estado, casos_acumulados))) +
  geom_col(fill = "#1E6847", alpha = 0.9) +
  scale_x_continuous(breaks = seq(0, 1000, 25), 
                     limits = c(0, max(mx_ultimo_corte_edo$casos_acumulados) + max(mx_ultimo_corte_edo$casos_acumulados)*0.05),
                     expand = c(0, 0)) +
  labs(title = "Número de casos de Covid-19 confirmados en cada entidad",
       subtitle = subtitulo_mx,
       x = "\nNúmero     ",
       y = "",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud obtenidos a través del paquete {covidMex}\ncon cifras curadas por @guzmart_ y  @SerendipiaData.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22)) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0)) +
  ggsave(str_c(ruta_graficas_mx, "03_numero_casos_por_entidad_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16.2, height = 12)


### Gráfica 04: Treemap del número de casos de Covid-19 confirmados en cada entidad ----
mx_ultimo_corte_edo %>%
  ggplot(aes(area = casos_acumulados, fill = log(casos_acumulados))) +
  geom_treemap(col = "white") +
  geom_treemap_text(aes(label = estado), fontface = "bold", color = "white", alpha = 1, min.size = 0, grow = F) +
  geom_treemap_text(aes(label = paste(comma(casos_acumulados, accuracy = 1), "casos", sep = " ")), color = "white", padding.y = unit(7, "mm"),min.size = 0) +
  geom_treemap_text(aes(label = paste(comma(casos_acumulados/sum(casos_acumulados)*100, accuracy = 1), "% de los casos", sep = "")), color = "white", padding.y = unit(14, "mm"), min.size = 0, size = 14) +
  scale_fill_gradient(low = "grey95", high = "#1E6847", guide = guide_colorbar(barwidth = 18, nbins = 6), labels = comma, breaks = pretty_breaks(n = 6)) +
  labs(title = "Casos confirmados de Covid-19 en cada entidad",
       subtitle = subtitulo_mx,
       x = NULL,
       y = NULL,
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud obtenidos a través del paquete {covidMex} con cifras curadas por\n@guzmart_.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de caso sospechoso\ny que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22),
        legend.position = "none") +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0)) +
  ggsave(str_c(ruta_graficas_mx, "04_numero_casos_por_entidad_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 12)




### Gráfica 05: Heatmap del número acumulado de casos confirmados de Covid-19 en cada entidad de México ----
mx_diario_edo %>% 
  ggplot(aes(x = fecha_corte, 
             y = fct_rev(estado),
             fill = log(casos_acumulados + 1))) +
  geom_tile(color = "grey60") +
  scale_x_date(date_breaks = "1 day", date_labels = "%b-%d", expand = c(0, 0)) +
  scale_fill_gradient(low = "#ffffff", 
                      high = "#1E6847", 
                      breaks = 0:4,
                      labels = c(str_c("0", " (mín.)"), "", "", "", str_c(max(mx_diario_edo$casos_acumulados), " (máx.)"))
  ) +
  labs(title = "Número acumulado de casos confirmados de Covid-19 en cada entidad de México",
       subtitle = subtitulo_mx,
       x = "",
       y = NULL,
       fill = "Número acumulado (log)  ",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud\nobtenidos a través del paquete {covidMex} con cifras curadas por @guzmart_ y  @SerendipiaData.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(plot.title = element_text(size = 28),
        legend.position = c(0.88, -0.15), 
        legend.direction = "horizontal",
        legend.key.width = unit(1.3, "cm"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks.y = element_blank()) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0))  +
  ggsave(str_c(ruta_graficas_mx, "05_evolucion_casos_confirmados_por_edo_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 17, height = 13)


### Gráfica 06_01: Evolución del número acumulado de casos confirmados desde el primer caso confirmado en las entidades de México ----
foo <- 
  mx_diario_edo %>% 
  mutate(estado = case_when(estado == "Ciudad de México" ~ "CDMX",
                            estado == "Baja California" ~ "BC",
                            estado == "Baja California Sur" ~ "BCS",
                            estado == "Nuevo León" ~ "NL",
                            estado == "San Luis Potosí" ~ "SLP",
                            TRUE ~ estado)) %>%
  group_by(estado) %>%
  mutate(primer_caso = ifelse(casos_acumulados > 0 & fecha_corte == as_date("2020-02-27") | casos_acumulados > 0 & lag(casos_acumulados) == 0 & estado != "Ciudad de México", 1, NA),
         dummy_dias_primer_caso = primer_caso) %>%
  fill(dummy_dias_primer_caso, .direction = "down") %>% 
  mutate(dias_primer_caso = cumsum(replace_na(dummy_dias_primer_caso, 0)) - 1) %>% 
  ungroup() %>% 
  mutate(puntito_final = ifelse(fecha_corte == max(fecha_corte), casos_acumulados, NA), 
         etiquetas_entidad = ifelse(fecha_corte == max(fecha_corte) & casos_acumulados >= 70 | fecha_corte == max(fecha_corte) & dias_primer_caso > 22, estado, ""),
         etiquetas_entidad_log = ifelse(fecha_corte == max(fecha_corte), estado, "")) %>% 
  filter(dias_primer_caso > -1)


foo %>% 
  ggplot(aes(x = dias_primer_caso, 
             y = casos_acumulados, 
             group = estado)) +
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
  scale_x_continuous(breaks = c(seq(0, 100, 5), max(foo$dias_primer_caso)), limits = c(0, max(foo$dias_primer_caso) + max(foo$dias_primer_caso)*0.05)) +
  scale_y_continuous(limits = c(0, max(foo$casos_acumulados) + max(foo$casos_acumulados)*0.1),
                     label = comma, 
                     breaks = seq(0, 600, 50)) +
  labs(title = "Evolución del número acumulado de casos confirmados desde el primer caso\nconfirmado en las entidades de México*",
       subtitle = subtitulo_mx,
       x = "\nDías desde el primer caso confirmado  ",
       y = "Número de casos  \n",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud obtenidos a través del paquete {covidMex}\ncon cifras curadas por @guzmart_ y  @SerendipiaData.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(legend.position = "none")  +
  ggsave(str_c(ruta_graficas_mx, "06_01_evolucion_casos_paises_america_latina_desde_primer_caso_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)


### Gráfica 06_02: Evolución del número acumulado de casos confirmados desde el primer caso confirmado en las entidades de México, log 10 ----
set.seed(1)
foo %>% 
  ggplot(aes(x = dias_primer_caso, 
             y = casos_acumulados, 
             group = estado)) + 
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
  scale_y_log10(breaks = c(1, 3, 10, 30, 100, 300, 1000, 3e3, 10e3, 3e4, 10e4, 3e5, 10e5, 3e6, 10e6, 3e7, 10e7)) +
  labs(title = "Evolución del número acumulado de casos confirmados desde el primer caso\nconfirmado en las entidades de México*",
       subtitle = str_c(subtitulo_mx_confirmados, " | Distancia logarítmica en las etiquetas del eje vertical"),
       x = "\nDías desde el primer caso confirmado  ",
       y = "Número de casos (log 10)\n",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud obtenidos a través del paquete {covidMex}\ncon cifras curadas por @guzmart_ y  @SerendipiaData.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(legend.position = "none")  +
  ggsave(str_c(ruta_graficas_mx, "06_02_evolucion_casos_paises_america_latina_desde_primer_caso_log10_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)


### Gráfica 07: Número de casos confirmados de Covid-19, por género y edad ----
foo <- 
  mx_serendipia %>% 
  filter(fecha_corte == max(fecha_corte)) %>% 
  mutate(rango_edad = case_when(edad <= 20 ~ "20 años o menos",
                                edad > 20 & edad <= 30 ~ "21-30",
                                edad > 30 & edad <= 40 ~ "31-40",
                                edad > 40 & edad <= 50 ~ "41-50",
                                edad > 50 & edad <= 60 ~ "51-60",
                                edad > 60 & edad <= 70 ~ "61-70",
                                edad > 70 & edad <= 80 ~ "71-80",
                                edad > 80 ~ "Más de 80 años",
  ),
  genero = ifelse(sexo == "F", "Mujeres", "Hombres"),
  genero = fct_relevel(genero, "Mujeres", "Hombres"))%>% 
  count(genero, rango_edad) 

## Verificar
foo %>% 
  mutate(total = sum(n))

foo %>% 
  ggplot(aes(x = str_wrap(rango_edad, width = 8), y = n, fill = n)) +
  geom_col(fill = "#1E6847", alpha = 0.9) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, max(foo$n) + max(foo$n)*0.1),
                     breaks = seq(0, 400, 20)) +
  facet_wrap(~ genero) +
  labs(x = NULL, 
       y = "Número    \n") +
  labs(title = "Casos confirmados de Covid-19, por género y rango de edad",
       subtitle = subtitulo_mx,
       x = NULL,
       y = "Número\n   ",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud obtenidos a través del paquete {covidMex}\ncon cifras curadas por @guzmart_ y  @SerendipiaData.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22),
        axis.text.x = element_text(size = 15),
        legend.position = "none",
        strip.text = element_text(size = 18)) +
  ggsave(str_c(ruta_graficas_mx, "07_numero_casos_por_genero_edad", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)


### Gráfica 08: Porcentaje de casos de Covid-19 confirmados diariamente cuyo contagio ocurrió\nen México o el extranjero ----
mx_serendipia %>% 
  # count(procedencia)
  filter(procedencia != "En investigación") %>% 
  mutate(procedencia_dummy = ifelse(procedencia != "Contacto", "Contagio en el extranjero", "Contagio en México")) %>% 
  group_by(fecha_corte) %>% 
  count(procedencia_dummy) %>% 
  ungroup() %>% 
  complete(fecha_corte, procedencia_dummy) %>% 
  mutate(n = ifelse(is.na(n), 0, n)) %>% 
  group_by(fecha_corte) %>% 
  mutate(porcentaje = round(n/sum(n)*100, 1)) %>% 
  ungroup() %>% 
  # tail()
  ggplot(aes(x = fecha_corte, y = porcentaje, fill = procedencia_dummy)) +
  geom_area(alpha = 0.9) +
  geom_hline(yintercept = seq(10, 90, 10), linetype = 2, color = "white") +
  scale_x_date(breaks = seq(from = as_date("2020-02-27"), 
                            to = max(mx_serendipia$fecha_corte), 
                            by = 1), 
               date_labels = "%b-%d", 
               limits = c(as_date("2020-02-27"), max(mx_serendipia$fecha_corte) + 0.5),
               expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 100, 10), 
                     limits = c(-1, 101),
                     expand = c(0.01, 0)) +
  scale_fill_manual(values = c("grey80", "#1E6847")) +
  labs(title = "Porcentaje de casos de Covid-19 confirmados diariamente cuyo contagio ocurrió\nen México o el extranjero",
       subtitle = subtitulo_mx,
       x = "",
       y = "Porcentaje\n",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40\nFuente: datos de la Secretaría de Salud obtenidos a través del paquete {covidMex}\ncon cifras curadas por @guzmart_ y  @SerendipiaData.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".",
       fill = "") +
  tema +
  theme(plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 22),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.ticks.y = element_blank(),
        legend.position = c(0.77, -0.25), 
        legend.direction = "horizontal",
        legend.text = element_text(size = 20)) +
  ggsave(str_c(ruta_graficas_mx, "08_evolucion_porcentaje_contagios_domesticos_foraneos_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 10)

### Gráfica 09: Número acumulado de muertes por Covid-19 en México ----
foo <- 
  mx_muertes_x_dia %>% 
  mutate(puntito_final = ifelse(fecha_corte == max(fecha_corte), muertes, NA),
         texto_puntito_final = ifelse(!is.na(puntito_final), str_c(comma(puntito_final), " muertes"), "")) 

foo %>%
  ggplot(aes(x = fecha_corte)) +
  geom_line(aes(y = muertes),
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
  scale_y_continuous(breaks = seq(0, 300, 10),
                     limits = c(-10, max(foo$muertes) + max(foo$muertes)*0.1),
                     expand = c(0, 0),
                     labels = comma) +
  labs(title = "Número acumulado de muertes por Covid-19 en México",
       subtitle = subtitulo_mx,
       x = "",
       y = "Número\n",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos del CSSE de Johns Hopkins y la Secretaría de Salud obtenidos a través del\npaquete {covidMex} con cifras curadas por @guzmart_ y  @SerendipiaData.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.ticks.y = element_blank()) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0)) +
  ggsave(str_c(ruta_graficas_mx, "09_evolucion_muertes_acumuladas_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16.2, height = 9)





### Gráfica 10: Número de casos confirmados de Covid-19 que fallecieron diariamente en México ----
foo <- 
  mx_muertes_x_dia %>% 
  mutate(num_muertes_diarias = muertes - lag(muertes),
         num_muertes_diarias = ifelse(is.na(num_muertes_diarias) & fecha_corte == as_date("2020-03-19"), 2, num_muertes_diarias)) 

foo %>% 
  ggplot(aes(x = fecha_corte, y = num_muertes_diarias)) +
  geom_col(fill = "grey10", alpha = 0.9) +
  scale_x_date(date_breaks = "1 day", date_labels = "%b-%d", expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 200, 5), 
                     limits = c(0, (max(foo$num_muertes_diarias) + max(foo$num_muertes_diarias)*0.1)),
                     expand = c(0, 0)) +
  labs(title = "Número de casos confirmados de Covid-19 que fallecieron diariamente en México",
       subtitle = subtitulo_mx,
       x = "",
       y = "Número\n",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud obtenidos a través del paquete {covidMex}\ncon cifras curadas por @guzmart_ y  @SerendipiaData.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 22),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.ticks.y = element_blank()) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0)) +
  ggsave(str_c(ruta_graficas_mx, "10_evolucion_muertes_diariamente_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16.5, height = 9)




### Limpiar ambiente ----
remove(list = ls())

# devtools::install_github("pablorm296/covidMex")
### Cargar paquetes, definir setup y tema de gráficas ----
source("02_codigo/00_paquetes_setup_tema.R") 

### Definir cortes de datos ----
subtitulo_mx_confirmados <-  str_c("Cifras a las 19:00 hrs. del ", 
                       day(Sys.Date()),
                       " de abril de 2020 (CDMX)")

### Generar folder para guardar las gráficas ----
dir_graficas <- 
  dir.create(file.path("03_graficas/03_graficas_analisis_mexico/", 
                       str_c("graficas_", str_replace_all(Sys.Date(), "-", "_"))))
ruta_graficas_mx <- str_c("03_graficas/03_graficas_analisis_mexico/", 
                       str_c("graficas_", str_replace_all(Sys.Date(), "-", "_"), "/"))


### Importar datos de Covid19 ----
mx_confirmados <- covidConfirmedMx()

### Limpiar y cambiar tipo de algunas variables ----
mx_confirmados <-
  mx_confirmados%>% 
  # Transformar tipo de fecha_inicio
  mutate(fecha_inicio = as.Date(fecha_inicio, origin = "1899-12-30"),
         # Poner nombres de estados en mayúsculas y minúsculas
         ent = str_to_title(ent),
         ent = str_replace(ent, " De ", " de "),
         ent = ifelse(str_detect(ent, "Quer"), "Querétaro", ent),
         ent = str_replace(ent, "\\r", ""),
         ent = str_replace(ent, "\\n", " "),
         fecha_corte = as_date(fecha_corte)) 

#### Verificar fecha de la última observación ----
max(mx_confirmados$fecha_corte)

### Eliminar casos que estaban en reportes de días previos pero no en el de hoy ----
mx_confirmados <- 
  mx_confirmados %>% 
  filter(inconsistencia_omision != 1)

### Importar datos poblacionales de CONAPO ----
source("02_codigo/08_importar_preparar_datos_conapo.R")

### Generar tibble con casos acumulados por estado ----
mx_confirmados_x_edo <- 
  mx_confirmados %>% 
  group_by(ent) %>% 
  summarise(num_casos = n()) %>% 
  ungroup()


### Unir datos de Covid19 con datos poblacionales ----
mx_confirmados_x_edo <- 
  mx_confirmados_x_edo %>% 
  left_join(bd_pob_edo %>% select(entidad, pob_tot, clave_ent),
            by = c("ent" = "entidad")) %>% 
  mutate(tasa_casos_100k = num_casos/pob_tot*100000) %>% 
  select(clave_ent, ent, pob_tot, num_casos, tasa_casos_100k)


### Gráfica 01: Número acumulado de casos confirmados de Covid-19 confirmados en México ----
foo <- 
  mx_confirmados %>% 
  group_by(fecha_corte) %>% 
  summarise(num_casos_diarios = n()) %>% 
  ungroup() %>% 
  mutate(num_acumulado = cumsum(num_casos_diarios),
         puntito_final = ifelse(fecha_corte == max(fecha_corte), num_acumulado, NA),
         texto_puntito_final = ifelse(!is.na(puntito_final), str_c(comma(puntito_final), " casos"), "")) 

foo %>%
  ggplot(aes(x = fecha_corte)) +
  geom_line(aes(y = num_acumulado),
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
                            to = max(mx_confirmados$fecha_corte), 
                            by = 1), 
               date_labels = "%b-%d", 
               limits = c(as_date("2020-02-27"), max(mx_confirmados$fecha_corte))) +
  scale_y_continuous(breaks = seq(0, 1500, 200),
                     limits = c(-1, max(foo$num_acumulado) + max(foo$num_acumulado)*0.1),
                     expand = c(0, 0),
                     labels = comma) +
  labs(title = "Número acumulado de casos confirmados de Covid-19 en México",
       subtitle = subtitulo_mx_confirmados,
       x = "",
       y = "Número\n",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud obtenidos a través del paquete {covidMex}\ncon cifras curadas por @guzmart_.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
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
mx_confirmados %>% 
  group_by(fecha_corte) %>% 
  summarise(num_casos_diarios = n()) %>% 
  ungroup() %>% 
  # tail()
  ggplot(aes(x = fecha_corte, y = num_casos_diarios)) +
  geom_col(fill = "#1E6847", alpha = 0.9) +
  scale_x_date(date_breaks = "1 day", date_labels = "%b-%d", expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 200, 10), expand = c(0, 0)) +
  labs(title = "Número de casos nuevos de Covid-19 confirmados diariamente en México",
       subtitle = subtitulo_mx_confirmados,
       x = "",
       y = "Número\n",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud obtenidos a través del paquete {covidMex}\ncon cifras curadas por @guzmart_.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.ticks.y = element_blank()) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0)) +
  ggsave(str_c(ruta_graficas_mx, "02_evolucion_casos_confirmados_diariamente_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16.2, height = 9)


### Gráfica 03: Número de casos de Covid-19 confirmados en cada entidad ----
mx_confirmados_x_edo %>% 
  ggplot(aes(x = num_casos, y = fct_reorder(ent, num_casos))) +
  geom_col(fill = "#1E6847", alpha = 0.9) +
  scale_x_continuous(breaks = seq(0, 400, 25), 
                     limits = c(0, max(mx_confirmados_x_edo$num_casos) + max(mx_confirmados_x_edo$num_casos)*0.05),
                     expand = c(0, 0)) +
  labs(title = "Número de casos de Covid-19 confirmados en cada entidad",
       subtitle = subtitulo_mx_confirmados,
       x = "\nNúmero     ",
       y = "",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud obtenidos a través del paquete {covidMex}\ncon cifras curadas por @guzmart_.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22)) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0)) +
  ggsave(str_c(ruta_graficas_mx, "03_numero_casos_por_entidad_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16.2, height = 12)

### Gráfica 04: Treemap del número de casos de Covid-19 confirmados en cada entidad ----
mx_confirmados_x_edo %>%
  ggplot(aes(area = num_casos, fill = log(num_casos))) +
  geom_treemap(col = "white") +
  geom_treemap_text(aes(label = ent), fontface = "bold", color = "white", alpha = 1, min.size = 0, grow = F) +
  geom_treemap_text(aes(label = paste(comma(num_casos, accuracy = 1), "casos", sep = " ")), color = "white", padding.y = unit(7, "mm"),min.size = 0) +
  geom_treemap_text(aes(label = paste(comma(num_casos/sum(num_casos)*100, accuracy = 1), "% de los casos", sep = "")), color = "white", padding.y = unit(14, "mm"), min.size = 0, size = 14) +
  scale_fill_gradient(low = "grey95", high = "#1E6847", guide = guide_colorbar(barwidth = 18, nbins = 6), labels = comma, breaks = pretty_breaks(n = 6)) +
  labs(title = "Casos confirmados de Covid-19 en cada entidad",
       subtitle = subtitulo_mx_confirmados,
       x = NULL,
       y = NULL,
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud obtenidos a través del paquete {covidMex} con cifras curadas por\n@guzmart_.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de caso sospechoso\ny que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22),
        legend.position = "none") +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0)) +
  ggsave(str_c(ruta_graficas_mx, "04_numero_casos_por_entidad_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 12)

### Gráfica 05: Heatmap del número acumulado de casos confirmados de Covid-19 en cada entidad de México ----
foo <- 
  mx_confirmados %>% 
  arrange(ent, fecha_corte) %>% 
  group_by(ent) %>% 
  mutate(dummy = 1) %>% 
  ungroup() %>%
  add_row(ent = "Aguascalientes", fecha_corte = as_date("2020-03-02"), dummy = 0) %>%
  add_row(ent = "Aguascalientes", fecha_corte = as_date("2020-03-03"), dummy = 0) %>%
  add_row(ent = "Aguascalientes", fecha_corte = as_date("2020-03-04"), dummy = 0) %>%
  add_row(ent = "Aguascalientes", fecha_corte = as_date("2020-03-05"), dummy = 0) %>%
  add_row(ent = "Aguascalientes", fecha_corte = as_date("2020-03-08"), dummy = 0) %>%
  add_row(ent = "Aguascalientes", fecha_corte = as_date("2020-03-09"), dummy = 0) %>%
  add_row(ent = "Aguascalientes", fecha_corte = as_date("2020-03-10"), dummy = 0) %>%
  arrange(ent, fecha_corte) %>% 
  group_by(ent) %>% 
  mutate(num_acumulado_casos_x_edo = cumsum(dummy)) %>% 
  ungroup() %>% 
  select(ent, fecha_corte, dummy, num_acumulado_casos_x_edo) %>% 
  complete(ent, fecha_corte)  %>% 
  group_by(ent) %>%
  mutate(num_acumulado_casos_x_edo = ifelse(is.na(num_acumulado_casos_x_edo) & lead(num_acumulado_casos_x_edo) == 1, 0, num_acumulado_casos_x_edo)) %>% 
  mutate(num_acumulado_casos_x_edo = na.locf(num_acumulado_casos_x_edo, fromLast = F, na.rm = FALSE),
         num_acumulado_casos_x_edo = na.locf(num_acumulado_casos_x_edo, fromLast = T, na.rm = FALSE)) %>%
  ungroup() 


foo %>% 
  ggplot(aes(x = fecha_corte, 
             y = fct_rev(ent),
             fill = log(num_acumulado_casos_x_edo + 1))) +
  geom_tile(color = "grey60") +
  scale_x_date(date_breaks = "1 day", date_labels = "%b-%d", expand = c(0, 0)) +
  scale_fill_gradient(low = "#ffffff", 
                      high = "#1E6847", 
                      breaks = 0:4,
                      labels = c(str_c("0", " (mín.)"), "", "", "", str_c(max(foo$num_acumulado_casos_x_edo), " (máx.)"))
                      ) +
  labs(title = "Número acumulado de casos confirmados de Covid-19 en cada entidad de México",
       subtitle = subtitulo_mx_confirmados,
       x = "",
       y = NULL,
       fill = "Número acumulado (log)  ",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud\nobtenidos a través del paquete {covidMex} con cifras curadas por @guzmart_.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(plot.title = element_text(size = 28),
        legend.position = c(0.9, -0.15), 
        legend.direction = "horizontal",
        legend.key.width = unit(1, "cm"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks.y = element_blank()) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5))  +
  ggsave(str_c(ruta_graficas_mx, "05_evolucion_casos_confirmados_por_edo_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 17, height = 13)


### Gráfica 06_01: Evolución del número acumulado de casos confirmados desde el primer caso confirmado en las entidades de México ----
foo <- 
  mx_confirmados %>% 
  mutate(ent = case_when(ent == "Ciudad de México" ~ "CDMX",
                         ent == "Baja California" ~ "BC",
                         ent == "Baja California Sur" ~ "BCS",
                         ent == "Nuevo León" ~ "NL",
                         ent == "San Luis Potosí" ~ "SLP",
                         TRUE ~ ent)) %>%
  arrange(ent, fecha_corte) %>% 
  group_by(ent, fecha_corte) %>% 
  summarise(casos_confirmados = n()) %>% 
  ungroup() %>% 
  rbind(tibble(ent = rep(x = "Ciudad de México"),
               fecha_corte = c(as_date("2020-03-02"), as_date("2020-03-03"), as_date("2020-03-04"), as_date("2020-03-05"), as_date("2020-03-08"), as_date("2020-03-09"), as_date("2020-03-10")),
               casos_confirmados = NA)) %>%
  complete(ent, fecha_corte) %>%
  group_by(ent) %>%
  mutate(casos_confirmados = replace_na(casos_confirmados, replace = 0),
         casos_confirmados_acumulados = cumsum(casos_confirmados),
         primer_caso = ifelse(casos_confirmados > 0 & fecha_corte == as_date("2020-02-27") | casos_confirmados > 0 & lag(casos_confirmados) == 0 & ent != "Ciudad de México", 1, NA),
         dummy_dias_primer_caso = primer_caso) %>% 
  fill(dummy_dias_primer_caso, .direction = "down") %>% 
  mutate(dias_primer_caso = cumsum(replace_na(dummy_dias_primer_caso, 0)) - 1) %>% 
  ungroup() %>% 
  mutate(puntito_final = ifelse(fecha_corte == max(fecha_corte), casos_confirmados_acumulados, NA), 
         etiquetas_entidad = ifelse(fecha_corte == max(fecha_corte) & casos_confirmados_acumulados >= 25 | fecha_corte == max(fecha_corte) & dias_primer_caso > 15 | fecha_corte == max(fecha_corte) & dias_primer_caso < 5 & casos_confirmados_acumulados > 9 , ent, ""),
         etiquetas_entidad_log = ifelse(fecha_corte == max(fecha_corte), ent, "")) %>% 
  filter(dias_primer_caso > -1)


foo %>% 
  ggplot(aes(x = dias_primer_caso, 
             y = casos_confirmados_acumulados, 
             group = ent)) +
  geom_line(size = 1, 
            color = "#1E6847", 
            alpha = 0.6) +
  geom_point(aes(x = dias_primer_caso, 
                 y = puntito_final),
             size = 2, 
             color = "#1E6847",
             alpha = 0.8) +
  geom_text_repel(aes(label = etiquetas_entidad), 
                  check_overlap = T,
                  # vjust = -0.7,
                  color = "grey30",
                  segment.color = "grey70",
                  # bg.colour = 'white',
                  fontface = "bold",
                  size = 5) +
  scale_x_continuous(breaks = c(seq(0, 100, 5), max(foo$dias_primer_caso)), limits = c(0, max(foo$dias_primer_caso) + max(foo$dias_primer_caso)*0.05)) +
  scale_y_continuous(limits = c(0, max(foo$casos_confirmados_acumulados) + max(foo$casos_confirmados_acumulados)*0.1),
                     label = comma, 
                     breaks = seq(0, 400, 25)) +
  labs(title = "Evolución del número acumulado de casos confirmados desde el primer caso\nconfirmado en las entidades de México*",
       subtitle = subtitulo_mx_confirmados,
       x = "\nDías desde el primer caso confirmado  ",
       y = "Número de casos  \n",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud obtenidos a través del paquete {covidMex}\ncon cifras curadas por @guzmart_.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(legend.position = "none")  +
  ggsave(str_c(ruta_graficas_mx, "06_01_evolucion_casos_paises_america_latina_desde_primer_caso_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)


### Gráfica 06_02: Evolución del número acumulado de casos confirmados desde el primer caso confirmado en las entidades de México, log 10 ----

set.seed(1)
foo %>% 
  ggplot(aes(x = dias_primer_caso, 
             y = casos_confirmados_acumulados, 
             group = ent)) +
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
  scale_x_continuous(breaks = c(seq(0, 100, 5), max(foo$dias_primer_caso)), limits = c(0, max(foo$dias_primer_caso) + max(foo$dias_primer_caso)*0.05)) +
  scale_y_log10(breaks = c(1, 3, 10, 30, 100, 300, 1000, 3e3, 10e3, 3e4, 10e4, 3e5, 10e5, 3e6, 10e6, 3e7, 10e7)) +
  labs(title = "Evolución del número acumulado de casos confirmados desde el primer caso\nconfirmado en las entidades de México*",
       subtitle = str_c(subtitulo_mx_confirmados, " | Distancia logarítmica en las etiquetas del eje vertical"),
       x = "\nDías desde el primer caso confirmado  ",
       y = "Número de casos (log 10)\n",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud obtenidos a través del paquete {covidMex}\ncon cifras curadas por @guzmart_.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(legend.position = "none")  +
  ggsave(str_c(ruta_graficas_mx, "06_02_evolucion_casos_paises_america_latina_desde_primer_caso_log10_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)

### Gráfica 07: Número de casos confirmados de Covid-19, por género y edad ----
foo <- 
  mx_confirmados %>% 
  mutate(rango_edad = case_when(edad <= 20 ~ "20 años o menos",
                                edad > 20 & edad <= 30 ~ "21-30",
                                edad > 30 & edad <= 40 ~ "31-40",
                                edad > 40 & edad <= 50 ~ "41-50",
                                edad > 50 & edad <= 60 ~ "51-60",
                                edad > 60 & edad <= 70 ~ "61-70",
                                edad > 70 & edad <= 80 ~ "71-80",
                                edad > 80 ~ "Más de 80 años",
  ),
  genero = ifelse(sexo == "F", "Mujeres", "Hombres"),
  genero = fct_relevel(genero, "Mujeres", "Hombres"))%>% 
  count(genero, rango_edad) 

foo %>% 
  ggplot(aes(x = str_wrap(rango_edad, width = 8), y = n, fill = n)) +
  geom_col(fill = "#1E6847", alpha = 0.9) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, max(foo$n) + max(foo$n)*0.1),
                     breaks = seq(0, 200, 20)) +
  facet_wrap(~ genero) +
  labs(x = NULL, 
       y = "Número    \n") +
  labs(title = "Casos confirmados de Covid-19, por género y rango de edad",
       subtitle = subtitulo_mx_confirmados,
       x = NULL,
       y = "Número\n   ",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud obtenidos a través del paquete {covidMex}\ncon cifras curadas por @guzmart_.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22),
        axis.text.x = element_text(size = 15),
        legend.position = "none",
        strip.text = element_text(size = 18)) +
  ggsave(str_c(ruta_graficas_mx, "07_numero_casos_por_genero_edad", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)


### Gráfica 08: Porcentaje de casos de Covid-19 confirmados diariamente cuyo contagio ocurrió\nen México o el extranjero ----
mx_confirmados %>% 
  # count(procedencia)
  filter(procedencia != "En investigación") %>% 
  mutate(procedencia_dummy = ifelse(procedencia != "Contacto", "Contagio en el extranjero", "Contagio en México")) %>% 
  group_by(fecha_corte) %>% 
  count(procedencia_dummy) %>% 
  ungroup() %>% 
  complete(fecha_corte, procedencia_dummy) %>% 
  mutate(n = ifelse(is.na(n), 0, n)) %>% 
  group_by(fecha_corte) %>% 
  mutate(porcentaje = round(n/sum(n)*100, 1)) %>% 
  ungroup() %>% 
  ggplot(aes(x = fecha_corte, y = porcentaje, fill = procedencia_dummy)) +
  geom_area(alpha = 0.9) +
  geom_hline(yintercept = seq(10, 90, 10), linetype = 2, color = "white") +
  scale_x_date(breaks = seq(from = as_date("2020-02-27"), 
                            to = max(mx_confirmados$fecha_corte), 
                            by = 1), 
               date_labels = "%b-%d", 
               limits = c(as_date("2020-02-27"), max(mx_confirmados$fecha_corte) + 0.5),
               expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 100, 10), 
                     limits = c(-1, 101),
                     expand = c(0.01, 0)) +
  scale_fill_manual(values = c("grey80", "#1E6847")) +
  labs(title = "Porcentaje de casos de Covid-19 confirmados diariamente cuyo contagio ocurrió\nen México o el extranjero",
       subtitle = subtitulo_mx_confirmados,
       x = "",
       y = "Porcentaje\n",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40\nFuente: datos de la Secretaría de Salud obtenidos a través del paquete {covidMex}\ncon cifras curadas por @guzmart_.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".",
       fill = "") +
  tema +
  theme(plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 22),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.ticks.y = element_blank(),
        legend.position = c(0.77, -0.25), 
        legend.direction = "horizontal",
        legend.text = element_text(size = 20)) +
  ggsave(str_c(ruta_graficas_mx, "08_evolucion_porcentaje_contagios_domesticos_foraneos_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 10)


### Gráfica 09: Casos confirmados de Covid-19 en cada entidad por cada 100 mil habitantes ----
mx_confirmados_x_edo %>% 
  ggplot(aes(x = fct_reorder(ent, tasa_casos_100k), 
             y = tasa_casos_100k)) +
  geom_col(fill = "#1E6847", alpha = 0.9) +
  scale_y_continuous(breaks = seq(0, 4, 0.25),
                     limits = c(0, max(mx_confirmados_x_edo$tasa_casos_100k) + max(mx_confirmados_x_edo$tasa_casos_100k)*0.1),
                     expand = c(0, 0)) +
  coord_flip() +
  labs(title = "Casos confirmados de Covid-19 en cada entidad por cada\n100 mil habitantes",
       subtitle = subtitulo_mx_confirmados,
       x = NULL,
       y = "\nTasa  ",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud obtenidos a través del paquete {covidMex}\ncon cifras curadas por @guzmart_ y proyecciones poblacionales de CONAPO.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(plot.title = element_text(size = 38),
        plot.subtitle = element_text(size = 25)) +
  ggsave(str_c(ruta_graficas_mx, "09_tasa_casos_confirmados_por_entidad_100k_habitantes", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16.2, height = 14)
