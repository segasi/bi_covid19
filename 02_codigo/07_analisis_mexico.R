### Cargar paquetes, definir setup y tema de gráficas ----
source("02_codigo/00_paquetes_setup_tema.R") 

### Definir cortes de datos ----
subtitulo_mx_confirmados <-  str_c("Cifras a las 19:00 hrs. del ", 
                       day(Sys.Date()),
                       " de marzo de 2020 (CDmx_confirmados)")

### Generar folder para guardar las gráficas ----
dir_graficas <- 
  dir.create(file.path("03_graficas/03_graficas_analisis_mexico/", 
                       str_c("graficas_", str_replace_all(Sys.Date(), "-", "_"))))
ruta_graficas_x <- str_c("03_graficas/03_graficas_analisis_mexico/", 
                       str_c("graficas_", str_replace_all(Sys.Date(), "-", "_"), "/"))


### Obtener datos ----
mx_confirmados <- getData(type = "confirmed")

# Verificar fecha de la última observación
max(mx_confirmados$fecha_corte)
  
### Gráfica 01: Número acumulado de casos de Covid-19 confirmados en México ----
foo <- 
  mx_confirmados %>% 
  group_by(fecha_corte) %>% 
  summarise(num_casos_diarios = n()) %>% 
  ungroup() %>% 
  mutate(num_acumulado = cumsum(num_casos_diarios),
         puntito_final = ifelse(fecha_corte == max(fecha_corte), num_acumulado, NA),
         texto_puntito_final = ifelse(!is.na(puntito_final), str_c(puntito_final, " casos"), "")) 

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
  scale_y_continuous(breaks = seq(0, 400, 50),
                     limits = c(-1, max(foo$num_acumulado) + max(foo$num_acumulado)*0.1),
                     expand = c(0, 0)) +
  labs(title = "Número acumulado de casos de Covid-19 en México",
       subtitle = subtitulo_mx_confirmados,
       x = "",
       y = "Número\n",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud obtenidos a través del paquete {covidMex} con cifras\ncuradas por @guzmart_.") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.ticks.y = element_blank()) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0)) +
  ggsave(str_c(ruta_graficas_x, "01_evolucion_casos_acumulados_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16.2, height = 9)


### Gráfica 02: Número de nuevos casos de Covid-19 confirmados diariamente en México ----
mx_confirmados %>% 
  group_by(fecha_corte) %>% 
  summarise(num_casos_diarios = n()) %>% 
  ungroup() %>% 
  # tail()
  ggplot(aes(x = fecha_corte, y = num_casos_diarios)) +
  geom_col(fill = "#1E6847", alpha = 0.9) +
  scale_x_date(date_breaks = "1 day", date_labels = "%b-%d", expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 100, 10), expand = c(0, 0)) +
  labs(title = "Número de casos nuevos de Covid-19 confirmados diariamente en México",
       subtitle = subtitulo_mx_confirmados,
       x = "",
       y = "Número\n",
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud obtenidos a través del paquete {covidMex} con cifras\ncuradas por @guzmart_.") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.ticks.y = element_blank()) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0)) +
  ggsave(str_c(ruta_graficas_x, "02_evolucion_casos_confirmados_diariamente_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16.2, height = 9)



### Gráfica 03: Número de casos de Covid-19 confirmados en cada entidad ----
foo <- 
  mx_confirmados %>% 
  group_by(ent) %>% 
  summarise(num_casos = n()) %>% 
  ungroup() 

foo %>% 
  ggplot(aes(x = num_casos, y = fct_reorder(ent, num_casos))) +
  geom_col(fill = "#1E6847", alpha = 0.9) +
  scale_x_continuous(breaks = seq(0, 80, 5), 
                     limits = c(0, max(foo$num_casos) + max(foo$num_casos)*0.1),
                     expand = c(0, 0)) +
  labs(title = "Número de casos de Covid-19 confirmados en cada entidad",
       subtitle = subtitulo_mx_confirmados,
       x = "\nNúmero     ",
       y = "",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud obtenidos a través del paquete {covidMex}\ncon cifras curadas por @guzmart_.") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22)) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0)) +
  ggsave(str_c(ruta_graficas_x, "03_numero_casos_por_entidad_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16.2, height = 12)

### Gráfica 04: Treemap del número de casos de Covid-19 confirmados en cada entidad ----
mx_confirmados %>% 
  group_by(ent) %>% 
  summarise(casos_confirmados = n()) %>% 
  ungroup() %>%
  ggplot(aes(area = casos_confirmados, fill = (casos_confirmados))) +
  geom_treemap(col = "white") +
  geom_treemap_text(aes(label = ent), fontface = "bold", color = "white", alpha = 1, min.size = 0, grow = F) +
  geom_treemap_text(aes(label = paste(comma(casos_confirmados, accuracy = 1), "casos", sep = " ")), color = "white", padding.y = unit(7, "mm"),min.size = 0) +
  geom_treemap_text(aes(label = paste(comma(casos_confirmados/sum(casos_confirmados)*100, accuracy = 1), "% de los casos", sep = "")), color = "white", padding.y = unit(14, "mm"), min.size = 0, size = 14) +
  scale_fill_gradient(low = "grey95", high = "#1E6847", guide = guide_colorbar(barwidth = 18, nbins = 6), labels = comma, breaks = pretty_breaks(n = 6)) +
  labs(title = "Casos confirmados de Covid-19 en cada entidad",
       subtitle = subtitulo_mx_confirmados,
       x = NULL,
       y = NULL,
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud obtenidos a través del paquete {covidMex} con cifras\ncuradas por @guzmart_.") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22),
        legend.position = "none") +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0)) +
  ggsave(str_c(ruta_graficas_x, "04_numero_casos_por_entidad_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 12)

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
  scale_fill_gradient(low = "#ffffff", high = "#1E6847", n.breaks = 5, labels = c(str_c("0", " (mín.)"), "", "", "", str_c(max(foo$num_acumulado_casos_x_edo), " (máx.)"))) +
  labs(title = "Número acumulado de casos confirmados de Covid-19 en cada entidad de México",
       subtitle = subtitulo_mx_confirmados,
       x = "",
       y = NULL,
       fill = "Número acumulado (log)",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40\nFuente: datos de la Secretaría de Salud obtenidos a través del paquete {covidMex} con cifras curadas por @guzmart_.\n\n") +
  tema +
  theme(legend.position = c(0.9, -0.2), 
        legend.direction = "horizontal",
        legend.key.width = unit(1, "cm"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks.y = element_blank()) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0))  +
  ggsave(str_c(ruta_graficas_x, "05_evolucion_casos_confirmados_por_edo_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 17, height = 11)


### Gráfica 06: Número de casos confirmados de Covid-19, por género y edad ----
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
                     breaks = seq(0, 100, 5)) +
  facet_wrap(~ genero) +
  labs(x = NULL, 
       y = "Número    \n") +
  labs(title = "Casos confirmados de Covid-19, por género y rango de edad",
       subtitle = subtitulo_mx_confirmados,
       x = NULL,
       y = "Número\n   ",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud obtenidos a través del paquete {covidMex} con cifras\ncuradas por @guzmart_.") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22),
        axis.text.x = element_text(size = 15),
        legend.position = "none",
        strip.text = element_text(size = 18)) +
  ggsave(str_c(ruta_graficas_x, "06_numero_casos_por_genero_edad", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)


### Gráfica 07: Porcentaje de casos de Covid-19 confirmados diariamente cuyo contagio ocurrió\nen México o el extranjero ----
mx_confirmados %>% 
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
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40\nFuente: datos de la Secretaría de Salud obtenidos a través del\npaquete {covidMex} con cifras curadas por @guzmart_.",
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
  ggsave(str_c(ruta_graficas_x, "07_evolucion_porcentaje_contagios_domesticos_foraneos_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 10)



