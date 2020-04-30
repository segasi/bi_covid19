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

### Importar y procesar proyecciones poblacionales de CONAPO ----
source("02_codigo/08_importar_preparar_datos_conapo.R")

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

### Generar nuevo tibble con series de tiempo de casos y muertes ----
mx_st <- 
  casos_mx_st%>% 
  left_join(muertes_mx_st, by = c("Estado", "fecha_corte")) %>% 
  mutate(Estado = ifelse(Estado == "Queretaro", "Querétaro", Estado))

max(mx_st$fecha_corte)

mx_st %>% 
  tail()

### Unir datos de proyecciones poblacionales de CONAPO a mx_st ----
mx_st <- 
  mx_st %>% 
  left_join(bd_pob_edo %>% select(Estado = entidad, pob_tot), by = "Estado") 

### Calcular tasa de casos confirmados y muertes por cada 100k habitantes ----
mx_st <- 
  mx_st %>% 
  mutate(tasa_casos_100k = round(casos/pob_tot*1e5, 1),
         tasa_muertes_100k = round(muertes/pob_tot*1e5, 1)) 

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

### Importar y procesar datos abiertos ----

# Datos diarios
mx_datos <- 
  read_csv("01_datos/ssa/bd/200429COVID19MEXICO.csv") %>% 
  clean_names()

# Catálogo de muncipios
cve_mpo <- 
  read_excel("01_datos/ssa/bd/Catalogos_0412.xlsx", 
             sheet = "Catálogo MUNICIPIOS") %>% 
  clean_names() %>% 
  mutate(cve_mpo = str_c(clave_entidad, clave_municipio))


## Convertir valores numéricos en texto ----
mx_datos <- 
  mx_datos %>% 
  mutate(origen = case_when(origen == 1 ~ "USMER",
                            origen == 2 ~ "Fuera de USMER",
                            origen == 3 ~ "No especificado"),
         sector = case_when(sector == 1 ~ "Cruz Roja",
                            sector == 2 ~ "DIF",
                            sector == 3 ~ "Estatal",
                            sector == 4 ~ "IMSS",
                            sector == 5 ~ "IMSS-Bienestar",
                            sector == 6 ~ "ISSSTE",
                            sector == 7 ~ "Municipal",
                            sector == 8 ~ "PEMEX",
                            sector == 9 ~ "Privada",
                            sector == 10 ~ "SEDENA",
                            sector == 11 ~ "SEMAR",
                            sector == 12 ~ "SSA",
                            sector == 13 ~ "Universitario",
                            sector == 99 ~ "No especificado"),
         sexo = case_when(sexo == 1 ~ "Mujer",
                          sexo == 2 ~"Hombre",
                          sexo == 99 ~ "No especificado"),
         tipo_paciente = case_when(tipo_paciente == 1 ~ "Ambulatorio",
                                   tipo_paciente == 2 ~ "Hospitalizado",
                                   tipo_paciente == 99 ~ "No especificado"),
         nacionalidad = case_when(nacionalidad == 1 ~ "Mexicana",
                                  nacionalidad == 2 ~ "Extranjera",
                                  nacionalidad == 99 ~ "No especificada"),
         resultado = case_when(resultado == 1 ~ "Positivo SARS-CoV-2",
                               resultado == 2 ~ "No positivo SARS-CoV-2",
                               resultado == 3 ~ "Resultado pendiente"),
         entidad_nac = case_when(entidad_nac == "01" ~ "AGUASCALIENTES",
                                 entidad_nac == "02" ~ "BAJA CALIFORNIA",
                                 entidad_nac == "03" ~ "BAJA CALIFORNIA SUR",
                                 entidad_nac == "04" ~ "CAMPECHE",
                                 entidad_nac == "05" ~ "COAHUILA",
                                 entidad_nac == "06" ~ "COLIMA",
                                 entidad_nac == "07" ~ "CHIAPAS",
                                 entidad_nac == "08" ~ "CHIHUAHUA",
                                 entidad_nac == "09" ~ "CIUDAD DE MÉXICO",
                                 entidad_nac == "10" ~ "DURANGO",
                                 entidad_nac == "11" ~ "GUANAJUATO",
                                 entidad_nac == "12" ~ "GUERRERO",
                                 entidad_nac == "13" ~ "HIDALGO",
                                 entidad_nac == "14" ~ "JALISCO",
                                 entidad_nac == "15" ~ "MÉXICO",
                                 entidad_nac == "16" ~ "MICHOACÁN",
                                 entidad_nac == "17" ~ "MORELOS",
                                 entidad_nac == "18" ~ "NAYARIT",
                                 entidad_nac == "19" ~ "NUEVO LEÓN",
                                 entidad_nac == "20" ~ "OAXACA",
                                 entidad_nac == "21" ~ "PUEBLA",
                                 entidad_nac == "22" ~ "QUERÉTARO",
                                 entidad_nac == "23" ~ "QUINTANA ROO",
                                 entidad_nac == "24" ~ "SAN LUIS POTOSÍ",
                                 entidad_nac == "25" ~ "SINALOA",
                                 entidad_nac == "26" ~ "SONORA",
                                 entidad_nac == "27" ~ "TABASCO",
                                 entidad_nac == "28" ~ "TAMAULIPAS",
                                 entidad_nac == "29" ~ "TLAXCALA",
                                 entidad_nac == "30" ~ "VERACRUZ",
                                 entidad_nac == "31" ~ "YUCATÁN",
                                 entidad_nac == "32" ~ "ZACATECAS",
                                 entidad_nac == "36" ~ "ESTADOS UNIDOS MEXICANOS",
                                 entidad_nac == "97" ~ "NO APLICA",
                                 entidad_nac == "98" ~ "SE IGNORA",
                                 entidad_nac == "99" ~ "NO ESPECIFICADO"),
         entidad_nac = str_to_title(entidad_nac),
         entidad_residencia = case_when(entidad_res == "01" ~ "AGUASCALIENTES",
                                        entidad_res == "02" ~ "BAJA CALIFORNIA",
                                        entidad_res == "03" ~ "BAJA CALIFORNIA SUR",
                                        entidad_res == "04" ~ "CAMPECHE",
                                        entidad_res == "05" ~ "COAHUILA",
                                        entidad_res == "06" ~ "COLIMA",
                                        entidad_res == "07" ~ "CHIAPAS",
                                        entidad_res == "08" ~ "CHIHUAHUA",
                                        entidad_res == "09" ~ "CIUDAD DE MÉXICO",
                                        entidad_res == "10" ~ "DURANGO",
                                        entidad_res == "11" ~ "GUANAJUATO",
                                        entidad_res == "12" ~ "GUERRERO",
                                        entidad_res == "13" ~ "HIDALGO",
                                        entidad_res == "14" ~ "JALISCO",
                                        entidad_res == "15" ~ "MÉXICO",
                                        entidad_res == "16" ~ "MICHOACÁN",
                                        entidad_res == "17" ~ "MORELOS",
                                        entidad_res == "18" ~ "NAYARIT",
                                        entidad_res == "19" ~ "NUEVO LEÓN",
                                        entidad_res == "20" ~ "OAXACA",
                                        entidad_res == "21" ~ "PUEBLA",
                                        entidad_res == "22" ~ "QUERÉTARO",
                                        entidad_res == "23" ~ "QUINTANA ROO",
                                        entidad_res == "24" ~ "SAN LUIS POTOSÍ",
                                        entidad_res == "25" ~ "SINALOA",
                                        entidad_res == "26" ~ "SONORA",
                                        entidad_res == "27" ~ "TABASCO",
                                        entidad_res == "28" ~ "TAMAULIPAS",
                                        entidad_res == "29" ~ "TLAXCALA",
                                        entidad_res == "30" ~ "VERACRUZ",
                                        entidad_res == "31" ~ "YUCATÁN",
                                        entidad_res == "32" ~ "ZACATECAS",
                                        entidad_res == "36" ~ "ESTADOS UNIDOS MEXICANOS",
                                        entidad_res == "97" ~ "NO APLICA",
                                        entidad_res == "98" ~ "SE IGNORA",
                                        entidad_res == "99" ~ "NO ESPECIFICADO"),
         entidad_residencia = str_to_title(entidad_residencia),
         entidad_residencia = str_replace(entidad_residencia, " De ", " de "),
         entidad_uni_med = case_when(entidad_um == "01" ~ "AGUASCALIENTES",
                                     entidad_um == "02" ~ "BAJA CALIFORNIA",
                                     entidad_um == "03" ~ "BAJA CALIFORNIA SUR",
                                     entidad_um == "04" ~ "CAMPECHE",
                                     entidad_um == "05" ~ "COAHUILA",
                                     entidad_um == "06" ~ "COLIMA",
                                     entidad_um == "07" ~ "CHIAPAS",
                                     entidad_um == "08" ~ "CHIHUAHUA",
                                     entidad_um == "09" ~ "CIUDAD DE MÉXICO",
                                     entidad_um == "10" ~ "DURANGO",
                                     entidad_um == "11" ~ "GUANAJUATO",
                                     entidad_um == "12" ~ "GUERRERO",
                                     entidad_um == "13" ~ "HIDALGO",
                                     entidad_um == "14" ~ "JALISCO",
                                     entidad_um == "15" ~ "MÉXICO",
                                     entidad_um == "16" ~ "MICHOACÁN",
                                     entidad_um == "17" ~ "MORELOS",
                                     entidad_um == "18" ~ "NAYARIT",
                                     entidad_um == "19" ~ "NUEVO LEÓN",
                                     entidad_um == "20" ~ "OAXACA",
                                     entidad_um == "21" ~ "PUEBLA",
                                     entidad_um == "22" ~ "QUERÉTARO",
                                     entidad_um == "23" ~ "QUINTANA ROO",
                                     entidad_um == "24" ~ "SAN LUIS POTOSÍ",
                                     entidad_um == "25" ~ "SINALOA",
                                     entidad_um == "26" ~ "SONORA",
                                     entidad_um == "27" ~ "TABASCO",
                                     entidad_um == "28" ~ "TAMAULIPAS",
                                     entidad_um == "29" ~ "TLAXCALA",
                                     entidad_um == "30" ~ "VERACRUZ",
                                     entidad_um == "31" ~ "YUCATÁN",
                                     entidad_um == "32" ~ "ZACATECAS",
                                     entidad_um == "36" ~ "ESTADOS UNIDOS MEXICANOS",
                                     entidad_um == "97" ~ "NO APLICA",
                                     entidad_um == "98" ~ "SE IGNORA",
                                     entidad_um == "99" ~ "NO ESPECIFICADO"),
         entidad_uni_med = str_to_title(entidad_uni_med),
         entidad_uni_med = str_replace(entidad_uni_med, " De ", " de "),
         intubado = case_when(intubado == 1 ~ "Sí",
                              intubado == 2 ~ "No",
                              intubado == 97 ~ "No aplica",
                              intubado == 98 ~ "Se ignora",
                              intubado == 99 ~ "No especificado"),
         neumonia = case_when(neumonia == 1 ~ "Sí",
                              neumonia == 2 ~ "No",
                              neumonia == 97 ~ "No aplica",
                              neumonia == 98 ~ "Se ignora",
                              neumonia == 99 ~ "No especificado"),
         embarazo = case_when(embarazo == 1 ~ "Sí",
                              embarazo == 2 ~ "No",
                              embarazo == 97 ~ "No aplica",
                              embarazo == 98 ~ "Se ignora",
                              embarazo == 99 ~ "No especificado"),
         habla_lengua_indig = case_when(habla_lengua_indig == 1 ~ "Sí",
                                        habla_lengua_indig == 2 ~ "No",
                                        habla_lengua_indig== 97 ~ "No aplica", 
                                        habla_lengua_indig== 98 ~ "Se ignora",
                                        habla_lengua_indig == 99 ~ "No especificado"),
         diabetes = case_when(diabetes == 1 ~ "Sí",
                              diabetes == 2 ~ "No",
                              diabetes == 97 ~ "No aplica",
                              diabetes == 98 ~ "Se ignora",
                              diabetes == 99 ~ "No especificado"),
         epoc = case_when(epoc == 1 ~ "Sí",
                          epoc == 2 ~ "No",
                          epoc == 97 ~ "No aplica",
                          epoc == 98 ~ "Se ignora",
                          epoc == 99 ~ "No especificado"),
         asma = case_when(asma == 1 ~ "Sí",
                          asma == 2 ~ "No",
                          asma == 97 ~ "No aplica",
                          asma == 98 ~ "Se ignora",
                          asma == 99 ~ "No especificado"),
         inmusupr = case_when(inmusupr == 1 ~ "Sí",
                              inmusupr == 2 ~ "No",
                              inmusupr == 97 ~ "No aplica",
                              inmusupr == 98 ~ "Se ignora",
                              inmusupr == 99 ~ "No especificado"),
         hipertension = case_when(hipertension == 1 ~ "Sí",
                                  hipertension == 2 ~ "No",
                                  hipertension == 97 ~ "No aplica",
                                  hipertension == 98 ~ "Se ignora",
                                  hipertension == 99 ~ "No especificado"),
         otra_com = case_when(otra_com == 1 ~ "Sí",
                              otra_com == 2 ~ "No",
                              otra_com == 97 ~ "No aplica",
                              otra_com == 98 ~ "Se ignora",
                              otra_com == 99 ~ "No especificado"),
         cardiovascular = case_when(cardiovascular == 1 ~ "Sí",
                                    cardiovascular == 2 ~ "No",
                                    cardiovascular == 97 ~ "No aplica",
                                    cardiovascular == 98 ~ "Se ignora",
                                    cardiovascular == 99 ~ "No especificado"),
         obesidad = case_when(obesidad == 1 ~ "Sí",
                              obesidad == 2 ~ "No",
                              obesidad == 97 ~ "No aplica",
                              obesidad == 98 ~ "Se ignora",
                              obesidad == 99 ~ "No especificado"),
         renal_cronica = case_when(renal_cronica == 1 ~ "Sí",
                                   renal_cronica == 2 ~ "No",
                                   renal_cronica == 97 ~ "No aplica",
                                   renal_cronica == 98 ~ "Se ignora",
                                   renal_cronica == 99 ~ "No especificado"),
         tabaquismo = case_when(tabaquismo == 1 ~ "Sí",
                                tabaquismo == 2 ~ "No",
                                tabaquismo == 97 ~ "No aplica",
                                tabaquismo == 98 ~ "Se ignora",
                                tabaquismo == 99 ~ "No especificado"),
         otro_caso = case_when(otro_caso == 1 ~ "Sí",
                               otro_caso == 2 ~ "No",
                               otro_caso == 97 ~ "No aplica",
                               otro_caso == 98 ~ "Se ignora",
                               otro_caso == 99 ~ "No especificado"),
         uci = case_when(uci == 1 ~ "Sí",
                         uci == 2 ~ "No",
                         uci == 97 ~ "No aplica",
                         uci == 98 ~ "Se ignora",
                         uci == 99 ~ "No especificado"),
         cve_mpo = str_c(entidad_res, municipio_res))


## Unir catálogo de mnunicipios y genera variable "municipio" ----
mx_datos <- 
  mx_datos %>%
  left_join(cve_mpo %>% select(municipio, cve_mpo), 
            by = "cve_mpo") %>%
  mutate(municipio = str_to_title(municipio),
         municipio = str_replace(municipio, " De ", " de "),
         municipio = str_replace(municipio, " Del ", " del "),
         municipio = str_replace(municipio, " Los ", " los "),
         municipio = str_replace(municipio, " La ", " la ")) 


## Crear tibble con datos a nivel estatal ----
mx_datos_edo <- 
  mx_datos %>% 
  # count(tipo_paciente)
  # glimpse()
  group_by(entidad_um, entidad_uni_med) %>% 
  summarise(num_pruebas = n(),
            num_positivos = sum(resultado == "Positivo SARS-CoV-2"),
            num_negativos = sum(resultado == "No positivo SARS-CoV-2"),
            num_sospechosos = sum(resultado == "Resultado pendiente"),
            num_ambulatorio = sum(tipo_paciente == "Ambulatorio" & resultado == "Positivo SARS-CoV-2"),
            num_hospitalizado = sum(tipo_paciente == "Hospitalizado" & resultado == "Positivo SARS-CoV-2"),
            num_uci = sum(uci == "Sí" & resultado == "Positivo SARS-CoV-2"),
            num_intubado = sum(intubado == "Sí" & resultado == "Positivo SARS-CoV-2"),
            num_neumonia = sum(neumonia == "Sí" & resultado == "Positivo SARS-CoV-2"),
            
            num_muertos = sum(!is.na(fecha_def) & resultado == "Positivo SARS-CoV-2")) %>% 
  ungroup() 

## Crear tibble con datos a nivel municipal ----
mx_datos_mpo <- 
  mx_datos %>% 
  # count(tipo_paciente)
  # glimpse()
  group_by(cve_mpo, municipio, entidad_um, entidad_uni_med) %>% 
  summarise(num_pruebas = n(),
            num_positivos = sum(resultado == "Positivo SARS-CoV-2"),
            num_negativos = sum(resultado == "No positivo SARS-CoV-2"),
            num_sospechosos = sum(resultado == "Resultado pendiente"),
            num_ambulatorio = sum(tipo_paciente == "Ambulatorio" & resultado == "Positivo SARS-CoV-2"),
            num_hospitalizado = sum(tipo_paciente == "Hospitalizado" & resultado == "Positivo SARS-CoV-2"),
            num_uci = sum(uci == "Sí" & resultado == "Positivo SARS-CoV-2"),
            num_intubado = sum(intubado == "Sí" & resultado == "Positivo SARS-CoV-2"),
            num_neumonia = sum(neumonia == "Sí" & resultado == "Positivo SARS-CoV-2"),
            
            num_muertos = sum(!is.na(fecha_def) & resultado == "Positivo SARS-CoV-2")) %>% 
  ungroup() 

## Unir proyecciones poblacionales de CONAPO a datos estatales ----
mx_datos_edo <- 
  mx_datos_edo %>%
  left_join(bd_pob_edo %>% select(clave_ent,  pob_tot), by = c("entidad_um" = "clave_ent")) 


## Calcular tasas para datos estatales ----
mx_datos_edo <- 
  mx_datos_edo %>% 
  mutate(tasa_positivos = num_positivos/pob_tot*1e5,
         tasa_pruebas = num_pruebas/pob_tot*1e5)

## Unir proyecciones poblacionales de CONAPO a datos municipales ----
mx_datos_mpo <- 
  mx_datos_mpo %>% 
  left_join(bd_pob_mpo %>% select(clave,  pob_tot), by = c("cve_mpo" = "clave")) 



### Importar datos del último corte curados por Serendipia ----
# fecha_hoy <- Sys.Date()
# 
# datos_dia <- 
#   getData(where = "Mexico", 
#           type = "confirmed", 
#           date = str_c(day(fecha_hoy), "/", month(fecha_hoy), "/", year(fecha_hoy)),
#           source = "Serendipia", neat = F) %>% 
#   clean_names() %>% 
#   mutate(fecha_corte = fecha_hoy) %>% 
#   filter(!str_detect(n_caso, "Fuente|Recuper"))
# 
# datos_dia %>% 
#   tail()

### Gráfica 01_01: Número acumulado de casos confirmados de Covid-19 confirmados en México ----
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
  scale_x_date(breaks = c(seq(from = as_date("2020-02-27"), 
                            to = max(foo$fecha_corte), 
                            by = 7),
                          max(foo$fecha_corte)), 
               date_labels = "%b-%d", 
               limits = c(as_date("2020-02-27"), max(foo$fecha_corte) + 1)) +
  scale_y_continuous(breaks = seq(0, 20000, 2000),
                     limits = c(-15, max(foo$casos_acumulados) + max(foo$casos_acumulados)*0.1),
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
  ggsave(str_c(ruta_graficas_mx, "01_01_evolucion_casos_acumulados_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16.2, height = 9)


### Gráfica 01_02: Número de nuevos casos de Covid-19 confirmados diariamente en México ----
foo <- 
  mx_diario_nal %>% 
  filter(fecha_corte > as_date("2020-02-26")) %>% 
  mutate(num_casos_diarios = casos_acumulados - lag(casos_acumulados),
         num_casos_diarios = ifelse(is.na(num_casos_diarios) & fecha_corte == as_date("2020-02-27"), 1, num_casos_diarios),
         promedio_movil_cinco_dias = rollmean(num_casos_diarios, k = 5, align = 'right', fill = NA)) 

foo %>% 
  tail(n = 10)

foo %>% 
  ggplot(aes(x = fecha_corte, y = num_casos_diarios)) +
  geom_col(fill = "#1E6847", alpha = 0.9) +
  geom_line(aes(y = promedio_movil_cinco_dias), color = "salmon", size = 2) +
  scale_x_date(date_breaks = "1 day", date_labels = "%b-%d", expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 2000, 200), 
                     expand = c(0, 0),
                     limits = c(0, max(foo$num_casos_diarios) + max(foo$num_casos_diarios)*0.1),
                     labels = comma) +
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
  ggsave(str_c(ruta_graficas_mx, "01_02_evolucion_casos_confirmados_diariamente_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16.5, height = 9)



### Gráfica 01_03: Tasa de casos confirmados de Covid-19 en cada entidad por cada 100 mil habitantes----
mx_ultimo_corte_edo %>% 
  ggplot(aes(x = tasa_casos_100k, y = fct_reorder(Estado, tasa_casos_100k))) +
  geom_col(fill = "#1E6847", alpha = 0.9) +
  scale_x_continuous(breaks = seq(0, 50, 5), 
                     limits = c(0, max(mx_ultimo_corte_edo$tasa_casos_100k) + max(mx_ultimo_corte_edo$tasa_casos_100k)*0.05),
                     expand = c(0, 0),
                     labels = comma_format(accuracy = 1)) +
  labs(title = "Tasa estatal de casos confirmados de Covid-19,  por cada\n100 mil habitantes",
       subtitle = subtitulo_mx,
       x = "\nTasa       ",
       y = "",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuentes: datos de la Secretaría de Salud, curados por @mariorz y proyecciones\npoblacionales de CONAPO.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(plot.title = element_text(size = 34),
        plot.subtitle = element_text(size = 22)) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0)) +
  ggsave(str_c(ruta_graficas_mx, "01_03_numero_casos_por_entidad_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16.2, height = 12)


### Gráfica 01_04: Treemap del número de casos de Covid-19 confirmados en cada entidad ----
mx_ultimo_corte_edo %>%
  ggplot(aes(area = casos, fill = log(casos))) +
  geom_treemap(col = "white") +
  geom_treemap_text(aes(label = Estado), fontface = "bold", color = "white", alpha = 1, min.size = 0, grow = F) +
  geom_treemap_text(aes(label = paste(comma(casos, accuracy = 1), "casos", sep = " ")), color = "white", padding.y = unit(7, "mm"),min.size = 0) +
  geom_treemap_text(aes(label = paste(comma(casos/sum(casos)*100, accuracy = 1), "% de los casos", sep = "")), color = "white", padding.y = unit(14, "mm"), min.size = 0, size = 14) +
  geom_treemap_text(aes(label = paste(tasa_casos_100k, " por c/100 mil habitantes", sep = "")), color = "white", padding.y = unit(20, "mm"), min.size = 0, size = 14) +
  scale_fill_gradient(low = "grey95", high = "#1E6847", guide = guide_colorbar(barwidth = 18, nbins = 6), labels = comma, breaks = pretty_breaks(n = 6)) +
  labs(title = "Casos confirmados de Covid-19 en cada entidad",
       subtitle = subtitulo_mx,
       x = NULL,
       y = NULL,
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud, curados por @mariorz.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de caso sospechoso\ny que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(plot.title = element_text(size = 35),
        plot.subtitle = element_text(size = 26),
        legend.position = "none") +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0)) +
  ggsave(str_c(ruta_graficas_mx, "01_04_numero_casos_por_entidad_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 12)



### Gráfica 01_05_a: Heatmap del número acumulado de casos confirmados de Covid-19 en cada entidad de México ----
mx_st %>% 
  filter(fecha_corte > as_date("2020-02-27")) %>% 
  ggplot(aes(x = fecha_corte, 
             y = fct_rev(Estado),
             fill = log(casos + 1))) +
  geom_tile(color = "grey60") +
  scale_x_date(breaks = c(seq(from = as_date("2020-02-27"), 
                              to = max(mx_st$fecha_corte), 
                              by = 7),
                          max(mx_st$fecha_corte)), 
               date_labels = "%b-%d", 
               expand = c(0, 0)) +
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
        plot.subtitle = element_text(size = 24),
        legend.position = c(0.86, -0.15), 
        legend.direction = "horizontal",
        legend.key.width = unit(1.3, "cm"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks.y = element_blank()) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0))  +
  ggsave(str_c(ruta_graficas_mx, "01_05_a_evolucion_casos_confirmados_por_edo_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 17, height = 13)


### Gráfica 01_05_b: Heatmap de la evolución de la tasa de casos confirmados de Covid-19 en cada\nentidad por cada 100 mil habitantes ----
mx_st %>% 
  filter(fecha_corte > as_date("2020-02-27")) %>% 
  ggplot(aes(x = fecha_corte, 
             y = fct_rev(Estado),
             fill = log(tasa_casos_100k + 1))) +
  geom_tile(color = "grey60") +
  scale_x_date(breaks = c(seq(from = as_date("2020-02-27"), 
                              to = max(mx_st$fecha_corte), 
                              by = 7),
                          max(mx_st$fecha_corte)), 
               date_labels = "%b-%d", 
               expand = c(0, 0)) +
  scale_fill_gradient(low = "#ffffff", 
                      high = "#1E6847", 
                      breaks = 0:3,
                      labels = c(str_c("0", " (mín.)"), "", "", str_c(comma(max(mx_st$tasa_casos_100k)), " (máx.)"))) +
  labs(title = "Evolución de la tasa estatal de casos confirmados de Covid-19,\npor cada 100 mil habitantes",
       subtitle = subtitulo_mx,
       x = "",
       y = NULL,
       fill = "Tasa por c/100k habitantes (log)     ",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40.\nFuentes: datos de la Secretaría de Salud, curados por @mariorz y proyecciones poblacionales de CONAPO.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(plot.title = element_text(size = 35),
        plot.subtitle = element_text(size = 28),
        legend.position = c(0.86, -0.16), 
        legend.direction = "horizontal",
        legend.key.width = unit(1.7, "cm"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks.y = element_blank()) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))  +
  ggsave(str_c(ruta_graficas_mx, "01_05_b_evolucion_tasa_casos_confirmados_por_edo_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 17, height = 13)


### Gráfica 01_06_a: Evolución del número acumulado de casos confirmados desde el primer caso confirmado en las entidades de México ----
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
                     breaks = seq(0, 8000, 500)) +
  labs(title = "Evolución del número acumulado de casos confirmados desde el primer caso\nconfirmado en las entidades de México*",
       subtitle = subtitulo_mx,
       x = "\nDías desde el primer caso confirmado  ",
       y = "Número de casos  \n",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud, curados por @mariorz.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(legend.position = "none")  +
  ggsave(str_c(ruta_graficas_mx, "01_06_a_evolucion_casos_entidades_mexico_desde_primer_caso_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)


### Gráfica 01_06_b: Evolución del número acumulado de casos confirmados desde el primer caso confirmado en las entidades de México, log 10 ----
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
  ggsave(str_c(ruta_graficas_mx, "01_06_b_evolucion_casos_entidades_mexico_desde_primer_caso_log10_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 12)


### Gráfica 01_07_a: Evolución de la tasa de casos confirmados de Covid-19 desde el primer caso\nconfirmado en las entidades de México, por cada 100 mil habitantes ----
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
  mutate(puntito_final = ifelse(fecha_corte == max(fecha_corte), tasa_casos_100k, NA), 
         etiquetas_entidad = ifelse(fecha_corte == max(fecha_corte) & casos >= 100 | fecha_corte == max(fecha_corte) & dias_primer_caso >= 30, Estado, ""),
         etiquetas_entidad_log = ifelse(fecha_corte == max(fecha_corte), Estado, "")) %>% 
  filter(dias_primer_caso > -1) 


foo %>% 
  ggplot(aes(x = dias_primer_caso, 
             y = tasa_casos_100k, 
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
  scale_y_continuous(limits = c(0, max(foo$tasa_casos_100k) + max(foo$tasa_casos_100k)*0.05),
                     label = comma_format(accuracy = 1), 
                     breaks = seq(0, 100, 5)) +
  labs(title = "Evolución de la tasa estatal de casos confirmados acumulados de Covid-19,\npor cada 100 mil habitantes",
       subtitle = subtitulo_mx,
       x = "\nDías desde el primer caso confirmado  ",
       y = "Tasa  \n",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 \nFuentes: datos de la Secretaría de Salud, curados por @mariorz y proyecciones poblacionales de CONAPO.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(legend.position = "none")  +
  ggsave(str_c(ruta_graficas_mx, "01_07_a_evolucion_tasa_casos_entidades_mexico_desde_primer_caso_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)


### Gráfica 01_07_b: Evolución de la tasa de casos confirmados de Covid-19 desde el primer caso\nconfirmado en las entidades de México, por cada 100 mil habitantes, log 10 ----
set.seed(1)
foo %>% 
  ggplot(aes(x = dias_primer_caso, 
             y = tasa_casos_100k, 
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
  labs(title = "Evolución de la tasa estatal de casos confirmados acumulados de Covid-19,\npor cada 100 mil habitantes",
       subtitle = str_c(subtitulo_mx, " | Distancia logarítmica en las etiquetas del eje vertical"),
       x = "\nDías desde el primer caso confirmado  ",
       y = "Tasa (log 10)\n",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 \nFuentes: datos de la Secretaría de Salud, curados por @mariorz y proyecciones poblacionales de CONAPO.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(legend.position = "none")  +
  ggsave(str_c(ruta_graficas_mx, "01_07_b_evolucion_casos_entidades_mexico_desde_primer_caso_log10_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 12)

### Gráfica 01_08: Número de casos confirmados de Covid-19, por género y edad ----
foo <- 
  mx_datos %>%
  mutate(rango_edad = case_when(edad <= 10 ~ "0-10",
                                edad > 10 & edad <= 20 ~ "11-20",
                                edad > 20 & edad <= 30 ~ "21-30",
                                edad > 30 & edad <= 40 ~ "31-40",
                                edad > 40 & edad <= 50 ~ "41-50", 
                                edad > 50 & edad <= 60 ~ "51-60",
                                edad > 60 & edad <= 70 ~ "61-70",
                                edad > 70 & edad <= 80 ~ "71-80",
                                edad > 80 ~ "+80"),
         rango_edad = fct_relevel(rango_edad, "0-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "+80"), 
         genero = ifelse(sexo == "Mujer", "Mujeres", "Hombres"),
         genero = fct_relevel(genero, "Mujeres", "Hombres"))  %>% 
  group_by(genero, rango_edad) %>%
  summarise(num_positivos = sum(resultado == "Positivo SARS-CoV-2"), 
            num_muertos = sum(!is.na(fecha_def) & resultado == "Positivo SARS-CoV-2")) %>% 
  ungroup() %>% 
  mutate(por_positivios_fallecieron = num_muertos/num_positivos*100,
         por_positivios_fallecieron = ifelse(is.na(por_positivios_fallecieron), 0, por_positivios_fallecieron)) 

## Verificar
foo %>% 
  mutate(total = sum(num_positivos))

foo %>% 
  ggplot(aes(x = rango_edad, y = num_positivos)) +
  geom_col(fill = "#1E6847", alpha = 0.9) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, max(foo$num_positivos) + max(foo$num_positivos)*0.05),
                     breaks = seq(0, 4000, 200),
                     labels = comma) +
  facet_wrap(~ genero) +
  labs(x = NULL, 
       y = "Número    \n") +
  labs(title = "Casos confirmados de Covid-19, por género y rango de edad",
       subtitle = subtitulo_mx,
       x = NULL,
       y = "Número\n   ",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: Secretaría de Salud.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        axis.text.x = element_text(size = 15),
        legend.position = "none",
        strip.text = element_text(size = 18)) +
  ggsave(str_c(ruta_graficas_mx, "01_08_numero_casos_por_genero_edad", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)

### Gráfica 01_09_a: Número de casos confirmados de Covid-19, por entidad y rango de edad ----
mx_datos %>%
  mutate(rango_edad = case_when(edad <= 10 ~ "0-10",
                                edad > 10 & edad <= 20 ~ "11-20",
                                edad > 20 & edad <= 30 ~ "21-30",
                                edad > 30 & edad <= 40 ~ "31-40",
                                edad > 40 & edad <= 50 ~ "41-50",
                                edad > 50 & edad <= 60 ~ "51-60",
                                edad > 60 & edad <= 70 ~ "61-70",
                                edad > 70 & edad <= 80 ~ "71-80",
                                edad > 80 ~ "+80"),
         rango_edad = fct_relevel(rango_edad, "0-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "+80"))  %>% 
  group_by(entidad_uni_med, rango_edad) %>%
  summarise(num_positivos = sum(resultado == "Positivo SARS-CoV-2"), 
            num_muertos = sum(!is.na(fecha_def) & resultado == "Positivo SARS-CoV-2")) %>% 
  ungroup() %>% 
  group_by(entidad_uni_med) %>% 
  mutate(total_casos_positivos = sum(num_positivos),
         edo_breve = case_when(entidad_uni_med == "Ciudad de México" ~ "CDMX",
                               str_detect(entidad_uni_med, "Sur") ~ "BCS",
                               TRUE ~ entidad_uni_med), 
         etiqueta_estado = str_c(edo_breve, " (", comma(total_casos_positivos, accuracy = 1), ")")) %>% 
  ungroup() %>% 
  ggplot(aes(x = rango_edad, y = num_positivos)) +
  geom_col(fill = "#1E6847") +
  scale_y_continuous(labels = comma) +
  labs(title = "Casos confirmados de Covid-19 por entidad y rango de edad",
       subtitle = str_c(subtitulo_mx, " | El número entre paréntesis indica los casos confirmados en cada entidad"),
       x = "",
       y = NULL,
       fill = "Porcentaje",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40. Fuente: Secretaría de Salud.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de caso sospechoso y que\ncuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  facet_wrap(~ etiqueta_estado, ncol = 8) +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 20),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 13)) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0))  +
  ggsave(str_c(ruta_graficas_mx, "01_09_a_numero_casos_por_entidad_rango_edad_barras_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 17, height = 11)


### Gráfica 01_09_a: Número de casos confirmados de Covid-19, por entidad y rango de edad, escala libre eje vertical ----
mx_datos %>%
  mutate(rango_edad = case_when(edad <= 10 ~ "0-10",
                                edad > 10 & edad <= 20 ~ "11-20",
                                edad > 20 & edad <= 30 ~ "21-30",
                                edad > 30 & edad <= 40 ~ "31-40",
                                edad > 40 & edad <= 50 ~ "41-50",
                                edad > 50 & edad <= 60 ~ "51-60",
                                edad > 60 & edad <= 70 ~ "61-70",
                                edad > 70 & edad <= 80 ~ "71-80",
                                edad > 80 ~ "+80"),
         rango_edad = fct_relevel(rango_edad, "0-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "+80"))  %>% 
  group_by(entidad_uni_med, rango_edad) %>%
  summarise(num_positivos = sum(resultado == "Positivo SARS-CoV-2"), 
            num_muertos = sum(!is.na(fecha_def) & resultado == "Positivo SARS-CoV-2")) %>% 
  ungroup() %>% 
  group_by(entidad_uni_med) %>% 
  mutate(total_casos_positivos = sum(num_positivos),
         edo_breve = case_when(entidad_uni_med == "Ciudad de México" ~ "CDMX",
                               str_detect(entidad_uni_med, "Sur") ~ "BCS",
                               TRUE ~ entidad_uni_med), 
         etiqueta_estado = str_c(edo_breve, " (", comma(total_casos_positivos, accuracy = 1), ")")) %>% 
  ungroup() %>% 
  ggplot(aes(x = rango_edad, y = num_positivos)) +
  geom_col(fill = "#1E6847") +
  scale_y_continuous(labels = comma) +
  labs(title = "Casos confirmados de Covid-19 por entidad y rango de edad",
       subtitle = str_c(subtitulo_mx, " | El número entre paréntesis indica los casos confirmados en cada entidad"),
       x = "",
       y = NULL,
       fill = "Porcentaje",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40. Fuente: Secretaría de Salud.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de caso sospechoso y que\ncuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  facet_wrap(~ etiqueta_estado, ncol = 8, scale = "free_y") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 20),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 13),
        axis.text.y = element_text(size = 13)) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0))  +
  ggsave(str_c(ruta_graficas_mx, "01_09_b_numero_casos_por_entidad_rango_edad_barras_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 20, height = 11)

### Gráfica 02_01: Número acumulado de muertes por Covid-19 en México ----
foo <- 
  mx_diario_nal %>% 
  filter(fecha_corte > as_date("2020-03-18")) %>% 
  mutate(puntito_final = ifelse(fecha_corte == max(fecha_corte), muertes_acumulados, NA),
         texto_puntito_final = ifelse(!is.na(puntito_final), str_c(comma(puntito_final), " muertes"), "")) 

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
            hjust = 1,
            vjust = -1) +
  scale_x_date(breaks = c(seq(from = as_date("2020-03-19"), 
                            to = max(foo$fecha_corte), 
                            by = 7),
                          max(foo$fecha_corte)), 
               date_labels = "%b-%d", 
               limits = c(as_date("2020-03-19"), max(foo$fecha_corte))) +
  scale_y_continuous(breaks = seq(0, 2000, 250),
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
  ggsave(str_c(ruta_graficas_mx, "02_01_evolucion_muertes_acumuladas_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16.2, height = 9)




### Gráfica 02_02: Número de casos confirmados de Covid-19 que fallecieron diariamente en México ----
foo <- 
  mx_diario_nal %>% 
  filter(fecha_corte > as_date("2020-03-18")) %>%
  mutate(num_muertes_diarias = muertes_acumulados - lag(muertes_acumulados),
         promedio_movil_cinco_dias = rollmean(num_muertes_diarias, k = 5, align = 'right', fill = NA)) 

foo %>% 
  tail(n = 10)

foo %>%
  ggplot(aes(x = fecha_corte, y = num_muertes_diarias)) +
  geom_col(fill = "grey10", alpha = 0.9) +
  geom_line(aes(y = promedio_movil_cinco_dias), color = "salmon", size = 2) +
  scale_x_date(date_breaks = "1 day", date_labels = "%b-%d", expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 200, 10), 
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
  ggsave(str_c(ruta_graficas_mx, "02_02_evolucion_muertes_diariamente_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16.5, height = 9)



 
### Gráfica 02_03: Tasa estatal de muertes de pacientes a quienes se les confirmó Covid-19 por cada 100 mil habitantes ----
mx_st %>% 
  filter(fecha_corte == max(fecha_corte)) %>% 
  ggplot(aes(x = tasa_muertes_100k, y = fct_reorder(Estado, tasa_muertes_100k))) +
  geom_col(fill = "grey10") +
  scale_x_continuous(breaks = seq(0, 10, 0.5),
                     limits = c(0, max(mx_st$tasa_muertes_100k) + max(mx_st$tasa_muertes_100k) * 0.025),
                     expand = c(0, 0)) +
  labs(title = "Tasa estatal de muertes de pacientes a quienes se les confirmó\nCovid-19, por cada 100 mil habitantes",
       subtitle = subtitulo_mx,
       x = "\nTasa    ",
       y = NULL,
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud, curados por @mariorz y proyecciones\npoblacionales de CONAPO.") +
  tema +
  theme(legend.position = "none", 
        plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 20),
        panel.grid.major.y = element_blank()) +
  ggsave(str_c(ruta_graficas_mx, "02_03_tasa_estatal_muertes_covid19_mexico_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 11)

### Gráfica 02_04: Número acumulado de casos confirmados de Covid-19 que fallecieron en México, por entidad ----
mx_st %>%
  filter(fecha_corte == max(fecha_corte)) %>% 
  ggplot(aes(area = muertes, fill = log(muertes))) +
  geom_treemap(col = "white") +
  geom_treemap_text(aes(label = Estado), fontface = "bold", color = "white", alpha = 1, min.size = 0, grow = F) +
  geom_treemap_text(aes(label = paste(comma(muertes, accuracy = 1), "muertes", sep = " ")), color = "white", padding.y = unit(7, "mm"),min.size = 0) +
  geom_treemap_text(aes(label = paste(comma(muertes/sum(muertes)*100, accuracy = 1), "% del total", sep = "")), color = "white", padding.y = unit(14, "mm"), min.size = 0, size = 14) +
  geom_treemap_text(aes(label = paste(tasa_muertes_100k, " muertes por c/100 mil habitantes", sep = "")), color = "white", padding.y = unit(20, "mm"), min.size = 0, size = 14) +
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
  ggsave(str_c(ruta_graficas_mx, "02_04_distribucion_muertes_covid19_mexico_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)
 
### Gráfica 02_05: Heatmap de la evolución de la tasa de muertes de casos confirmados de Covid-19 en cada entidad, por cada 100 mil habitantes ----
mx_st %>% 
  filter(fecha_corte > as_date("2020-03-18")) %>% 
  ggplot(aes(x = fecha_corte, 
             y = fct_rev(Estado),
             fill = log(tasa_muertes_100k + 1))) +
  geom_tile(color = "grey60") +
  scale_x_date(breaks = c(seq(from = as_date("2020-02-27"), 
                              to = max(mx_st$fecha_corte), 
                              by = 7),
                          max(mx_st$fecha_corte)), 
               date_labels = "%b-%d", 
               expand = c(0, 0)) +
  scale_fill_gradient(low = "#ffffff", 
                      high = "black", 
                      breaks = seq(0, 1.5, 0.5),
                      labels = c(str_c("0", " (mín.)"), "", "", str_c(comma(max(mx_st$tasa_muertes_100k)), " (máx.)"))) +
  labs(title = "Evolución de la tasa estatal de muertes de pacientes a quienes se\nles confirmó Covid-19, por cada 100 mil habitantes",
       subtitle = subtitulo_mx,
       x = "",
       y = NULL,
       fill = "Tasa por c/100k habitantes (log)     ",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40.\nFuentes: datos de la Secretaría de Salud, curados por @mariorz y proyecciones poblacionales de CONAPO.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(plot.title = element_text(size = 35),
        plot.subtitle = element_text(size = 28),
        legend.position = c(0.86, -0.16), 
        legend.direction = "horizontal",
        legend.key.width = unit(1.7, "cm"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks.y = element_blank()) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))  +
  ggsave(str_c(ruta_graficas_mx, "02_05_evolucion_tasa_muertes_casos_confirmados_por_edo_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 17, height = 13)

 
 
### Gráfica 02_06_a: Evolución del número acumulado de muertes de casos confirmados que desde la primera muerte en las entidades de México ----
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
  mutate(primera_muerte = ifelse(muertes > 0 & fecha_corte == as_date("2020-03-19") | muertes > 0 & lag(muertes) == 0 & Estado != "CDMX", 1, NA),
         dummy_dias_primera_muerte = primera_muerte) %>%
  fill(dummy_dias_primera_muerte, .direction = "down") %>% 
  mutate(dias_primera_muerte = cumsum(replace_na(dummy_dias_primera_muerte, 0)) - 1) %>% 
  ungroup() %>% 
  mutate(puntito_final = ifelse(fecha_corte == max(fecha_corte), muertes, NA), 
         etiquetas_entidad = ifelse(fecha_corte == max(fecha_corte) & muertes >= 25 | fecha_corte == max(fecha_corte) & dias_primera_muerte >= 24, Estado, ""),
         etiquetas_entidad_log = ifelse(fecha_corte == max(fecha_corte), Estado, "")) %>% 
  filter(dias_primera_muerte > -1)


foo %>% 
  ggplot(aes(x = dias_primera_muerte, 
             y = muertes, 
             group = Estado)) +
  geom_line(size = 1, 
            color = "grey10", 
            alpha = 0.6) +
  geom_point(aes(x = dias_primera_muerte, 
                 y = puntito_final),
             size = 2, 
             color = "grey10",
             alpha = 0.8) +
  geom_text_repel(aes(label = etiquetas_entidad), 
                  # vjust = -0.7,
                  color = "grey30",
                  segment.color = "grey70",
                  # bg.colour = 'white',
                  fontface = "bold",
                  size = 5) +
  scale_x_continuous(breaks = c(seq(0, 100, 5), max(foo$dias_primera_muerte)), limits = c(0, max(foo$dias_primera_muerte) + max(foo$dias_primera_muerte)*0.01)) +
  scale_y_continuous(limits = c(0, max(foo$muertes) + max(foo$muertes)*0.05),
                     label = comma, 
                     breaks = seq(0, 500, 50)) +
  labs(title = "Evolución del número acumulado de muertes de pacientes a quienes se les confirmó\nCovid-19 en cada entidad",
       subtitle = subtitulo_mx,
       x = "\nDías desde la primera muerte  ",
       y = "Número de muertes  \n",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud, curados por @mariorz.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(legend.position = "none")  +
  ggsave(str_c(ruta_graficas_mx, "02_06_a_evolucion_muertes_de_casos_confirmadosentidades_mexico_desde_primera_muerte_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)


### Gráfica 02_06_b: Evolución del número acumulado de casos confirmados desde el primer caso confirmado en las entidades de México, log 10 ----
set.seed(1)
foo %>% 
  ggplot(aes(x = dias_primera_muerte, 
             y = muertes, 
             group = Estado)) + 
  geom_line(size = 1, 
            color = "grey10", 
            alpha = 0.4) +
  geom_point(aes(x = dias_primera_muerte, 
                 y = puntito_final),
             size = 2, 
             color = "grey10",
             alpha = 0.5) +
  geom_text_repel(aes(label = etiquetas_entidad_log), 
                  check_overlap = F,
                  force = 3,
                  # vjust = -0.7,
                  color = "grey30",
                  # bg.colour = 'white',
                  fontface = "bold",
                  size = 5) +
  scale_x_continuous(breaks = c(seq(0, 100, 5), max(foo$dias_primera_muerte)), limits = c(0, max(foo$dias_primera_muerte) + max(foo$dias_primera_muerte)*0.01)) +
  scale_y_log10(breaks = c(1, 3, 10, 30, 100, 300, 1000, 3e3, 10e3, 3e4, 10e4, 3e5, 10e5, 3e6, 10e6, 3e7, 10e7),
                labels = comma_format(accuracy = 1)) +
  labs(title = "Evolución del número acumulado de muertes de pacientes a quienes se les confirmó\nCovid-19 en cada entidad",
       subtitle = str_c(subtitulo_mx, " | Distancia logarítmica en las etiquetas del eje vertical"),
       x = "\nDías desde el primer caso confirmado  ",
       y = "Número de muertes (log 10)\n",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud, curados por @mariorz.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(legend.position = "none")  +
  ggsave(str_c(ruta_graficas_mx, "02_06_b_evolucion_muertes_de_casos_confirmadosentidades_mexico_desde_primera_muerte_log10_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 12)



### Gráfica 02_07_a: Evolución de la tasa estatal de muertes de pacientes a quienes se les confirmó Covid-19 por cada 100 mil habitantes ----
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
  mutate(primera_muerte = ifelse(muertes > 0 & fecha_corte == as_date("2020-03-19") | muertes > 0 & lag(muertes) == 0 & Estado != "CDMX", 1, NA),
         dummy_dias_primera_muerte = primera_muerte) %>%
  fill(dummy_dias_primera_muerte, .direction = "down") %>% 
  mutate(dias_primera_muerte = cumsum(replace_na(dummy_dias_primera_muerte, 0)) - 1) %>% 
  ungroup() %>% 
  mutate(puntito_final = ifelse(fecha_corte == max(fecha_corte), tasa_muertes_100k, NA), 
         etiquetas_entidad = ifelse(fecha_corte == max(fecha_corte) & tasa_muertes_100k >= 0.7 | fecha_corte == max(fecha_corte) & dias_primera_muerte >= 24, Estado, ""),
         etiquetas_entidad_log = ifelse(fecha_corte == max(fecha_corte), Estado, "")) %>% 
  filter(dias_primera_muerte > -1)


foo %>% 
  ggplot(aes(x = dias_primera_muerte, 
             y = tasa_muertes_100k, 
             group = Estado)) +
  geom_line(size = 1, 
            color = "grey10", 
            alpha = 0.6) +
  geom_point(aes(x = dias_primera_muerte, 
                 y = puntito_final),
             size = 2, 
             color = "grey10",
             alpha = 0.8) +
  geom_text_repel(aes(label = etiquetas_entidad), 
                  vjust = -0.7,
                  color = "grey30",
                  segment.color = "grey70",
                  # seed = 18,  
                  # force = 0.1,
                  # bg.colour = 'white',
                  fontface = "bold",
                  size = 5) +
  scale_x_continuous(breaks = c(seq(0, 100, 5), max(foo$dias_primera_muerte)), limits = c(0, max(foo$dias_primera_muerte) + max(foo$dias_primera_muerte)*0.01)) +
  scale_y_continuous(limits = c(0, max(foo$tasa_muertes_100k) + max(foo$tasa_muertes_100k)*0.05),
                     label = comma, 
                     breaks = seq(0, 100, 1)) +
  labs(title = "Evolución de la tasa estatal de muertes de pacientes a quienes se les confirmó\nCovid-19, por cada 100 mil habitantes",
       subtitle = subtitulo_mx,
       x = "\nDías desde la primera muerte  ",
       y = "Tasa  \n",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente:: datos de la Secretaría de Salud, curados por @mariorz y proyecciones poblacionales\nde CONAPO.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(legend.position = "none")  +
  ggsave(str_c(ruta_graficas_mx, "02_07_a_evolucion_tasa_muertes_de_casos_confirmadosentidades_mexico_desde_primera_muerte_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)


### Gráfica 02_07_b: Evolución de la tasa estatal de muertes de pacientes a quienes se les confirmó Covid-19 por cada 100 mil habitantes, log 10 ----
set.seed(1)
foo %>% 
  ggplot(aes(x = dias_primera_muerte, 
             y = tasa_muertes_100k, 
             group = Estado)) + 
  geom_line(size = 1, 
            color = "grey10", 
            alpha = 0.4) +
  geom_point(aes(x = dias_primera_muerte, 
                 y = puntito_final),
             size = 2, 
             color = "grey10",
             alpha = 0.5) +
  geom_text_repel(aes(label = etiquetas_entidad_log), 
                  check_overlap = F,
                  force = 3,
                  # vjust = -0.7,
                  color = "grey30",
                  # bg.colour = 'white',
                  fontface = "bold",
                  size = 5) +
  scale_x_continuous(breaks = c(seq(0, 100, 5), max(foo$dias_primera_muerte)), limits = c(0, max(foo$dias_primera_muerte) + max(foo$dias_primera_muerte)*0.01)) +
  scale_y_log10(breaks = c(0.3, 1, 3, 10, 30, 100, 300, 1000, 3e3, 10e3, 3e4, 10e4, 3e5, 10e5, 3e6, 10e6, 3e7, 10e7)) +
  labs(title = "Evolución de la tasa estatal de muertes de pacientes a quienes se les confirmó\nCovid-19, por cada 100 mil habitantes",
       subtitle = str_c(subtitulo_mx, " | Distancia logarítmica en las etiquetas del eje vertical"),
       x = "\nDías desde el primer caso confirmado  ",
       y = "Número de muertes (log 10)\n",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente:: datos de la Secretaría de Salud, curados por @mariorz y proyecciones poblacionales\nde CONAPO.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(legend.position = "none")  +
  ggsave(str_c(ruta_graficas_mx, "02_07_b_evolucion_tasa_muertes_de_casos_confirmadosentidades_mexico_desde_primera_muerte_log10_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 12)


### Gráfica 02_08: Número de casos confirmados de Covid-19 que fallecieron, por género y rango de edad ----
mx_datos %>%
  mutate(rango_edad = case_when(edad <= 10 ~ "10 años o menos",
                                edad > 10 & edad <= 20 ~ "11-20",
                                edad > 20 & edad <= 30 ~ "21-30",
                                edad > 30 & edad <= 40 ~ "31-40",
                                edad > 40 & edad <= 50 ~ "41-50",
                                edad > 50 & edad <= 60 ~ "51-60",
                                edad > 60 & edad <= 70 ~ "61-70",
                                edad > 70 & edad <= 80 ~ "71-80",
                                edad > 80 ~ "Más de 80 años"),
         genero = ifelse(sexo == "Mujer", "Mujeres", "Hombres"),
         genero = fct_relevel(genero, "Mujeres", "Hombres"))  %>% 
  group_by(genero, rango_edad) %>% 
  summarise(num_muertos = sum(!is.na(fecha_def) & resultado == "Positivo SARS-CoV-2")) %>% 
  ungroup() %>% 
  ggplot(aes(x = str_wrap(rango_edad, width = 8), y = num_muertos)) +
  geom_col(fill = "black") +
  scale_y_continuous(breaks = seq(0, 250, 25)) +
  facet_wrap(~ genero) +
  labs(title = "Número de casos confirmados de Covid-19 que fallecieron, por género\ny rango de edad",
       subtitle = subtitulo_mx,
       x = NULL,
       y = "Número\n   ",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: Secretaría de Salud.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de\ncaso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        axis.text.x = element_text(size = 15),
        legend.position = "none",
        strip.text = element_text(size = 18)) +
  ggsave(str_c(ruta_graficas_mx, "02_08_numero_casos_que_murieron_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)


### Gráfica 02_09: Porcentaje de casos confirmados de Covid-19 que murieron, por rango de edad ----  
mx_datos %>%
  mutate(rango_edad = case_when(edad <= 10 ~ "10 años o menos",
                                edad > 10 & edad <= 20 ~ "11-20",
                                edad > 20 & edad <= 30 ~ "21-30",
                                edad > 30 & edad <= 40 ~ "31-40",
                                edad > 40 & edad <= 50 ~ "41-50",
                                edad > 50 & edad <= 60 ~ "51-60",
                                edad > 60 & edad <= 70 ~ "61-70",
                                edad > 70 & edad <= 80 ~ "71-80",
                                edad > 80 ~ "Más de 80 años"))  %>%
  group_by(rango_edad) %>% 
  summarise(num_positivos = sum(resultado == "Positivo SARS-CoV-2"), 
            num_muertos = sum(!is.na(fecha_def) & resultado == "Positivo SARS-CoV-2")) %>% 
  ungroup() %>% 
  mutate(por_positivios_fallecieron = round(num_muertos/num_positivos*100, 1),
         por_positivios_fallecieron = ifelse(is.na(por_positivios_fallecieron), 0, por_positivios_fallecieron), 
         etiqueta_grandes = ifelse(por_positivios_fallecieron > 5, str_c(por_positivios_fallecieron, "%"), ""),
         etiqueta_pequeños = ifelse(por_positivios_fallecieron <= 5, str_c(por_positivios_fallecieron, "%"), "")) %>% 
  ggplot(aes(x = rango_edad, y = por_positivios_fallecieron)) +
  geom_col(fill = "grey10") +
  geom_text(aes(label = etiqueta_grandes), vjust = 1.5, color = "white", size = 6, fontface = "bold") +
  geom_text(aes(label = etiqueta_pequeños), vjust = -1, color = "grey40", size = 6, fontface = "bold") +
  labs(title = "Porcentaje de casos confirmados de Covid-19 que murieron,\npor rango de edad",
       subtitle = subtitulo_mx,
       x = "",
       y = NULL,
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40. Fuente: Secretaría de Salud.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de caso\nsospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(plot.title = element_text(size = 36),
        plot.subtitle = element_text(size = 25),
        axis.text.y = element_blank(), 
        panel.grid.major = element_blank()) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0))  +
  ggsave(str_c(ruta_graficas_mx, "02_09_porcentaje_casos_que_murieron_por_rango_edad_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)


### Gráfica 02_10: Porcentaje de casos confirmados de Covid-19 que murieron, por entidad\ny rango de edad ----
mx_datos %>%
  mutate(rango_edad = case_when(edad <= 10 ~ "10 años o menos",
                                edad > 10 & edad <= 20 ~ "11-20",
                                edad > 20 & edad <= 30 ~ "21-30",
                                edad > 30 & edad <= 40 ~ "31-40",
                                edad > 40 & edad <= 50 ~ "41-50",
                                edad > 50 & edad <= 60 ~ "51-60",
                                edad > 60 & edad <= 70 ~ "61-70",
                                edad > 70 & edad <= 80 ~ "71-80",
                                edad > 80 ~ "Más de 80 años"))  %>% 
  group_by(entidad_uni_med, rango_edad) %>%
  summarise(num_positivos = sum(resultado == "Positivo SARS-CoV-2"), 
            num_muertos = sum(!is.na(fecha_def) & resultado == "Positivo SARS-CoV-2")) %>% 
  ungroup() %>% 
  mutate(por_positivios_fallecieron = num_muertos/num_positivos*100,
         por_positivios_fallecieron = ifelse(is.na(por_positivios_fallecieron), 0, por_positivios_fallecieron)) %>% 
  ggplot(aes(x = rango_edad, y = fct_rev(entidad_uni_med), fill = (por_positivios_fallecieron))) +
  geom_tile(color = "grey80") +
  scale_fill_gradient(low = "#ffffff", 
                      high = "grey10") +
  labs(title = "Porcentaje de casos confirmados de Covid-19 que murieron, por entidad\ny rango de edad",
       subtitle = subtitulo_mx,
       x = "",
       y = NULL,
       fill = "Porcentaje",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40. Fuente: Secretaría de Salud.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona\nque cumpla con la definición operacional de caso sospechoso y que cuente con diagnóstico confirmado\npor la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 25),
        legend.position = c(0.86, -0.14), 
        legend.direction = "horizontal",
        legend.key.width = unit(1.3, "cm"),
        axis.ticks.y = element_blank()) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0))  +
  ggsave(str_c(ruta_graficas_mx, "02_10_porcentaje_casos_muriero_por_entidad_rango_edad_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 17, height = 13)


### Gráfica 02_11: Porcentaje de casos confirmados de Covid-19 que murieron, por entidad y rango de edad ----
mx_datos %>%
  mutate(rango_edad = case_when(edad <= 10 ~ "0-10",
                                edad > 10 & edad <= 20 ~ "11-20",
                                edad > 20 & edad <= 30 ~ "21-30",
                                edad > 30 & edad <= 40 ~ "31-40",
                                edad > 40 & edad <= 50 ~ "41-50", 
                                edad > 50 & edad <= 60 ~ "51-60",
                                edad > 60 & edad <= 70 ~ "61-70",
                                edad > 70 & edad <= 80 ~ "71-80",
                                edad > 80 ~ "+80"),
         rango_edad = fct_relevel(rango_edad, "0-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "+80"))  %>% 
  group_by(entidad_uni_med, rango_edad) %>%
  summarise(num_positivos = sum(resultado == "Positivo SARS-CoV-2"), 
            num_muertos = sum(!is.na(fecha_def) & resultado == "Positivo SARS-CoV-2")) %>% 
  ungroup() %>% 
  mutate(por_positivios_fallecieron = num_muertos/num_positivos*100,
         por_positivios_fallecieron = ifelse(is.na(por_positivios_fallecieron), 0, por_positivios_fallecieron)) %>% 
  group_by(entidad_uni_med) %>% 
  mutate(total_muertos_edo = sum(num_muertos),
         edo_breve = case_when(entidad_uni_med == "Ciudad de México" ~ "CDMX",
                               str_detect(entidad_uni_med, "Sur") ~ "BCS",
                               TRUE ~ entidad_uni_med), 
         etiqueta_estado = str_c(edo_breve, " (", total_muertos_edo, ")")) %>%  
  ungroup() %>% 
  ggplot(aes(x = rango_edad, y = por_positivios_fallecieron)) +
  geom_col(fill = "grey10") +
  labs(title = "Porcentaje de casos confirmados de Covid-19 que murieron, por entidad\ny rango de edad",
       subtitle = str_c(subtitulo_mx, " | El número entre paréntesis indica el total de muertes en cada entidad"),
       x = "",
       y = NULL,
       fill = "Porcentaje",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40. Fuente: Secretaría de Salud.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de caso sospechoso y que\ncuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  facet_wrap(~ etiqueta_estado, ncol = 8) +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 13)) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0))  +
  ggsave(str_c(ruta_graficas_mx, "02_11_porcentaje_casos_muriero_por_entidad_rango_edad_barras_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 17, height = 11)

### Gráfica 02_12_a: Número de casos confirmados de Covid-19 que murieron, por entidad y rango de edad ----
mx_datos %>%
  mutate(rango_edad = case_when(edad <= 10 ~ "0-10",
                                edad > 10 & edad <= 20 ~ "11-20",
                                edad > 20 & edad <= 30 ~ "21-30",
                                edad > 30 & edad <= 40 ~ "31-40",
                                edad > 40 & edad <= 50 ~ "41-50",
                                edad > 50 & edad <= 60 ~ "51-60",
                                edad > 60 & edad <= 70 ~ "61-70",
                                edad > 70 & edad <= 80 ~ "71-80",
                                edad > 80 ~ "+80"),
         rango_edad = fct_relevel(rango_edad, "0-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "+80"))  %>% 
  group_by(entidad_uni_med, rango_edad) %>%
  summarise(num_positivos = sum(resultado == "Positivo SARS-CoV-2"), 
            num_muertos = sum(!is.na(fecha_def) & resultado == "Positivo SARS-CoV-2")) %>% 
  ungroup() %>% 
  group_by(entidad_uni_med) %>% 
  mutate(total_muertos_edo = sum(num_muertos),
         edo_breve = case_when(entidad_uni_med == "Ciudad de México" ~ "CDMX",
                               str_detect(entidad_uni_med, "Sur") ~ "BCS",
                               TRUE ~ entidad_uni_med), 
         etiqueta_estado = str_c(edo_breve, " (", total_muertos_edo, ")")) %>%  
  ungroup() %>% 
  ggplot(aes(x = rango_edad, y = num_muertos)) +
  geom_col(fill = "grey10") +
  facet_wrap(~ etiqueta_estado, ncol = 8) +
  labs(title = "Número de personas a las que se les confirmó Covid-19 que murieron, por\nentidad y rango de edad",
       subtitle = str_c(subtitulo_mx, " | El número entre paréntesis indica el total de muertes en cada entidad"),
       x = "",
       y = NULL,
       fill = "Porcentaje",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40. Fuente: Secretaría de Salud.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de caso sospechoso y que\ncuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(plot.title = element_text(size = 33),
        plot.subtitle = element_text(size = 22),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 13)) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0))  +
  ggsave(str_c(ruta_graficas_mx, "02_12_a_numero_casos_muriero_por_entidad_rango_edad_barras_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 17, height = 11)


### Gráfica 02_12_b: Número de casos confirmados de Covid-19 que murieron, por entidad y rango de edad ----
mx_datos %>%
  mutate(rango_edad = case_when(edad <= 10 ~ "0-10",
                                edad > 10 & edad <= 20 ~ "11-20",
                                edad > 20 & edad <= 30 ~ "21-30",
                                edad > 30 & edad <= 40 ~ "31-40",
                                edad > 40 & edad <= 50 ~ "41-50",
                                edad > 50 & edad <= 60 ~ "51-60",
                                edad > 60 & edad <= 70 ~ "61-70",
                                edad > 70 & edad <= 80 ~ "71-80",
                                edad > 80 ~ "+80"),
         rango_edad = fct_relevel(rango_edad, "0-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "+80"))  %>% 
  group_by(entidad_uni_med, rango_edad) %>%
  summarise(num_positivos = sum(resultado == "Positivo SARS-CoV-2"), 
            num_muertos = sum(!is.na(fecha_def) & resultado == "Positivo SARS-CoV-2")) %>% 
  ungroup() %>% 
  group_by(entidad_uni_med) %>% 
  mutate(total_muertos_edo = sum(num_muertos),
         edo_breve = case_when(entidad_uni_med == "Ciudad de México" ~ "CDMX",
                               str_detect(entidad_uni_med, "Sur") ~ "BCS",
                               TRUE ~ entidad_uni_med), 
         etiqueta_estado = str_c(edo_breve, " (", total_muertos_edo, ")")) %>%  
  ungroup() %>% 
  ggplot(aes(x = rango_edad, y = num_muertos)) +
  geom_col(fill = "grey10") +
  facet_wrap(~ etiqueta_estado, ncol = 8, scale = "free_y") +
  scale_y_continuous(label = comma_format(accuracy = 1)) +
  labs(title = "Número de personas a las que se les confirmó Covid-19 que murieron, por entidad\ny rango de edad",
       subtitle = str_c(subtitulo_mx, " | El número entre paréntesis indica el total de muertes en cada entidad"),
       x = "",
       y = NULL,
       fill = "Porcentaje",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40. Fuente: Secretaría de Salud.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de caso sospechoso y que cuente con diagnóstico confirmado\npor la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(plot.title = element_text(size = 35),
        plot.subtitle = element_text(size = 22),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 13)) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0))  +
  ggsave(str_c(ruta_graficas_mx, "02_12_b_numero_casos_muriero_por_entidad_rango_edad_barras_escala_libre_y_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 20, height = 11)


### Gráfica 02_13: Porcentaje de casos confirmados de Covid-19 que murieron, por entidad ----
mx_st %>% 
  filter(fecha_corte == max(fecha_corte)) %>% 
  mutate(por_casos_conf_fallecieron = round(muertes/casos*100, 1)) %>% 
  # arrange(-por_casos_conf_fallecieron)
  mutate(etiqueta_grandes = ifelse(por_casos_conf_fallecieron > 24.5, str_c("Tasa: ", por_casos_conf_fallecieron, "% ", " (", comma(muertes, accuracy = 1), " muertos | ",  comma(casos, accuracy = 1), " casos)"), ""),
         etiqueta_chicos = ifelse(por_casos_conf_fallecieron <= 24.5, str_c(por_casos_conf_fallecieron, "%", " (", comma(muertes, accuracy = 1), " | ", comma(casos, accuracy = 1), ")"), "")) %>%
  ggplot(aes(x = fct_reorder(Estado, por_casos_conf_fallecieron), 
             y = por_casos_conf_fallecieron)) +
  geom_col(fill = "black") +
  scale_y_continuous(limits = c(0, 30),
                     expand = c(0, 0)) +
  coord_flip() +
  geom_text(aes(label = etiqueta_grandes), color = "white", fontface = "bold", family = "Roboto", hjust = 1.05, size = 5) +
  geom_text(aes(label = etiqueta_chicos), color = "grey30", fontface = "bold", family = "Roboto", hjust = -0.05, size = 5) +
  labs(title = "Porcentaje de pacientes a quienes se les\nconfirmó Covid-19 que murieron, por entidad",
       subtitle = subtitulo_mx,
       x = NULL,
       y = NULL,
       fill = NULL,
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40\nFuente: datos de la Secretaría de Salud, curados por @mariorz\n\nNotas: *La categoría \"casos confirmados\"se refiere a las personas que dieron positivo en la prueba de Covid-19.\nLa variable graficada fue calculada dividiendo el número de muertes entre el número de casos confirmados, por 100.") +
  tema +
  theme(plot.title = element_text(size = 36), 
        plot.subtitle = element_text(size = 26), 
        # plot.caption = element_text(size = 18), 
        panel.grid = element_blank(), 
        axis.text.x = element_blank(),
        legend.position = "none") +
  ggsave(str_c(ruta_graficas_mx, "02_13_porcentaje_casos_que_murieron_por_entidad_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 13.2, height = 18) 

### Gráfica 02_14: Evolución del porcentaje de casos confirmados de Covid-19 que murieron, por entidad ----
mx_st %>% 
  filter(fecha_corte >= as_date("2020-03-18")) %>% 
  mutate(por_positivios_fallecieron = muertes/casos*100) %>% 
  group_by(Estado) %>% 
  mutate(total_muertos = max(muertes),
         edo_breve = case_when(Estado == "Ciudad de México" ~ "CDMX",
                               str_detect(Estado, "Sur") ~ "BCS",
                               TRUE ~ Estado), 
         etiqueta_estado = str_c(edo_breve, " (", comma(total_muertos, accuracy = 1), ")")) %>% 
  ungroup() %>% 
  ggplot(aes(x = fecha_corte, y = por_positivios_fallecieron)) +
  geom_line(size = 1) +
  facet_wrap(~ Estado, ncol = 8) +
  labs(title = "Evolución del porcentaje de casos confirmados de Covid-19 que murieron, \npor entidad",
       subtitle = str_c(subtitulo_mx, " | El número entre paréntesis indica el total de muertes en cada entidad"),
       x = "",
       y = "%",
       fill = "Porcentaje",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40. Fuente: datos de la Secretaría de Salud, curados por @mariorz.\n\nNota: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición operacional de caso sospechoso y que cuente con diagnóstico confirmado\npor la Red Nacional de Laboratorios de Salud Pública reconocidos por el InDRE\".") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 20),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1, size = 13)) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0))  +
  ggsave(str_c(ruta_graficas_mx, "02_14_evolucion_porcentaje_casos_que_murieron_por_entidad_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 17, height = 12)

### Gráfica 02_15: Cambio porcentual diario del número de casos confirmados y muertes reportadas en México desde el 24 de marzo de 2020 ----
mx_diario_nal %>% 
  mutate(muertes = ifelse(is.na(muertes_acumulados), 0, muertes_acumulados),
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
  ggsave(str_c(ruta_graficas_mx, "02_15_cambio_porcentual_diario_casos_y_muertes_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)


### Gráfica 03_01: Pruebas de Covid-19 realizadas en cada entidad de México por cada 100 mil habitantes ----
mx_datos_edo %>% 
  ggplot(aes(x = num_pruebas/1000, y = fct_reorder(entidad_uni_med, num_pruebas))) +
  geom_col(fill = "#1E6847", alpha = 0.9) +
  scale_x_continuous(breaks = seq(0, 20, 1), 
                     expand = c(0, 0),
                     limits = c(0, max(mx_datos_edo$num_pruebas)/1000 +  max(mx_datos_edo$num_pruebas)/1000 * 0.05)) +
  labs(title = "Número de pruebas de Covid-19 realizadas en cada entidad de México",
       subtitle = subtitulo_mx,
       x = "\nMiles     ",
       y = NULL,
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22),
        plot.caption = element_markdown(size = 16)) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0)) +
  ggsave(str_c(ruta_graficas_mx, "03_01_numero_pruebas_por_estado_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16.2, height = 11)


### Gráfica 03_02: Pruebas de Covid-19 realizadas en cada entidad de México por cada 100 mil habitantes ----

mx_datos_edo %>% 
  # select(entidad_uni_med, num_pruebas, pob_tot, tasa_pruebas) %>% 
  # arrange(-tasa_pruebas)
  ggplot(aes(x = tasa_pruebas, y = fct_reorder(entidad_uni_med, tasa_pruebas))) +
  geom_col(fill = "#1E6847", alpha = 0.9) +
  scale_x_continuous(breaks = seq(0, 200, 10),
                     expand = c(0, 0)) +
  labs(title = "Pruebas de Covid-19 realizadas en cada entidad de México por cada\n100 mil habitantes",
       subtitle = subtitulo_mx,
       x = "\nTasa  ",
       y = NULL,
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22),
        plot.caption = element_markdown(size = 16)) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0)) +
  ggsave(str_c(ruta_graficas_mx, "03_02_tasa_pruebas_por_estado_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16.2, height = 11)

### Gráfica 03_03: Porcentaje de pruebas de Covid-19 realizadas en cada entidad cuyo resultado fue positivo ----

# Generar tibble para gráfica
foo <- 
  mx_datos_edo %>% 
  mutate(por_positivas = round(num_positivos/num_pruebas*100, 1)) 

# Verificación
foo %>% 
  select(entidad_uni_med, num_pruebas, num_positivos, por_positivas) %>% 
  mutate(total_positivas = sum(num_positivos)) %>% 
  arrange(-por_positivas) %>% 
  print(n = Inf)

mx_datos %>% 
  filter(resultado == "Positivo SARS-CoV-2") %>% 
  count(entidad_uni_med, sort = T)  %>% 
  print(n = Inf)

# Gráfica
foo %>% 
  ggplot(aes(x = por_positivas, 
             y = fct_reorder(entidad_uni_med, 
                             por_positivas))) +
  geom_col(fill = "#1E6847", alpha = 0.9) +
  scale_x_continuous(breaks = seq(0, 50, 5),
                     expand = c(0, 0),
                     limits = c(-0.25, max(foo$por_positivas) + max(foo$por_positivas) * 0.05)) +
  labs(title = "Porcentaje de pruebas de Covid-19 realizadas en cada entidad cuyo\nresultado fue positivo ", 
       subtitle = subtitulo_mx,
       x = "\nPorcentaje",
       y = NULL,
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40 / Fuente: datos de la Secretaría de Salud") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22)) +
  ggsave(str_c(ruta_graficas_mx, "03_03_porcentaje_pruebas_positivas_por_estado_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16.2, height = 11)


### Gráfica 03_04: Relación entre la tasa de pruebas por cada 100 mil habitantes y el porcentaje de pruebas positivas en cada entidad ----
mx_datos_edo %>% 
  mutate(por_positivas = round(num_positivos/num_pruebas*100, 1)) %>% 
  ggplot(aes(x = tasa_pruebas, 
             y = por_positivas)) +
  geom_point(color = "#1E6847", size = 4) +
  geom_text_repel(aes(label = entidad_uni_med),
                  size = 5, 
                  # point.padding = 2,
                  # lineheight = 8,
                  # force_pull = 10, 
                  # point.size = 10,
                  # force = 1.5,
                  color = "grey40") +
  scale_x_continuous(breaks = seq(0, 200, 20)) +
  scale_y_continuous(breaks = seq(0, 40, 10)) +
  labs(title = "Relación entre la tasa de pruebas por cada 100 mil habitantes y el porcentaje\nde pruebas positivas en cada entidad", 
       subtitle = subtitulo_mx,
       x = "\nPruebas por cada 100 mil habitantes",
       y = "Porcentaje de pruebas positivas\n",
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuentes: Los datos de Covid-19 provienen de la Secretaría de Salud y las proyecciones\npoblacionales a 2020 de Conapo.") +
  tema +
  ggsave(str_c(ruta_graficas_mx, "03_04_relacion_tasa_pruebas_porcentaje_positivos_por_entidad_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 11)  


### Gráfica 03_05: Relación entre la tasa de pruebas y la tasa de casos confirmados en cada entidad, ambas por cada 100 mil habitantes ----
mx_datos_edo %>% 
  ggplot(aes(x = tasa_pruebas, 
             y = tasa_positivos)) +
  geom_point(color = "#1E6847", size = 4) +
  geom_text_repel(aes(label = entidad_uni_med),
                  seed = 18,
                  size = 5, 
                  force = 1.5,
                  color = "grey40") +
  scale_x_continuous(breaks = seq(0, 200, 10)) +
  scale_y_continuous(breaks = seq(0, 200, 5)) +
  labs(title = "Relación entre la tasa de pruebas y la tasa de casos confirmados en cada entidad,\nambas por cada 100 mil habitantes", 
       subtitle = subtitulo_mx,
       x = "\nPruebas por cada 100 mil habitantes",
       y = "Casos confirmados por cada 100 mil habitantes\n",
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuentes: Los datos de Covid-19 provienen de la Secretaría de Salud y las proyecciones\npoblacionales a 2020 de Conapo.") +
  tema +
  ggsave(str_c(ruta_graficas_mx, "03_05_relacion_tasa_pruebas_tasa_casos_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 11)


### Gráfica 03_06: Pruebas de Covid-19 realizadas en cada institución ----
mx_datos %>% 
  group_by(sector) %>% 
  summarise(num_pruebas = n()) %>% 
  ungroup() %>% 
  ggplot(aes(area = num_pruebas, fill = log(num_pruebas))) +
  geom_treemap(col = "white") +
  geom_treemap_text(aes(label = sector), fontface = "bold", color = "white", alpha = 1, min.size = 0, grow = F) +
  geom_treemap_text(aes(label = paste(comma(num_pruebas, accuracy = 1), "pruebas", sep = " ")), color = "white", padding.y = unit(7, "mm"),min.size = 0) +
  geom_treemap_text(aes(label = paste(comma(num_pruebas/sum(num_pruebas)*100, accuracy = 1), "% de las pruebas", sep = "")), color = "white", padding.y = unit(14, "mm"), min.size = 0, size = 14) +
  scale_fill_gradient(low = "grey95", high = "#1E6847", guide = guide_colorbar(barwidth = 18, nbins = 6), labels = comma, breaks = pretty_breaks(n = 6)) +
  labs(title = "Pruebas de Covid-19 realizadas en cada institucióndel sector Salud",
       subtitle = subtitulo_mx,
       x = NULL,
       y = NULL,
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuentes: Secretaría de Salud.") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22),
        legend.position = "none") +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0)) +
  ggsave(str_c(ruta_graficas_mx, "03_06_numero_pruebas_por_sector_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 12)


### Gráfica 03_07: Pruebas de Covid-19 realizadas en cada institución del sector Salud cuyo resultado fue positivo ----
mx_datos %>% 
  filter(resultado == "Positivo SARS-CoV-2") %>% 
  group_by(sector) %>% 
  summarise(num_positivos = n()) %>% 
  ungroup() %>% 
  ggplot(aes(area = num_positivos, fill = log(num_positivos))) +
  geom_treemap(col = "white") +
  geom_treemap_text(aes(label = sector), fontface = "bold", color = "white", alpha = 1, min.size = 0, grow = F) +
  geom_treemap_text(aes(label = paste(comma(num_positivos, accuracy = 1), "pruebas", sep = " ")), color = "white", padding.y = unit(7, "mm"),min.size = 0) +
  geom_treemap_text(aes(label = paste(comma(num_positivos/sum(num_positivos)*100, accuracy = 1), "% de las pruebas", sep = "")), color = "white", padding.y = unit(14, "mm"), min.size = 0, size = 14) +
  scale_fill_gradient(low = "grey95", high = "#1E6847", guide = guide_colorbar(barwidth = 18, nbins = 6), labels = comma, breaks = pretty_breaks(n = 6)) +
  labs(title = "Pruebas de Covid-19 realizadas en cada institución del sector Salud cuyo\nresultado fue positivo",
       subtitle = subtitulo_mx,
       x = NULL,
       y = NULL,
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuentes: Secretaría de Salud.") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22),
        legend.position = "none") +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0)) +
  ggsave(str_c(ruta_graficas_mx, "03_07_numero_de_pruebas_positivas_por_sector_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 12)

### Gráfica 03_08: Porcentaje de pruebas de Covid-19 realizadas en cada institución del sector Salud cuyo resultado fue positivo ----
foo <- 
  mx_datos %>% 
  group_by(sector, resultado) %>% 
  summarise(numero = n()) %>% 
  ungroup() %>% 
  group_by(sector) %>% 
  mutate(total_pruebas = sum(numero),
         porcentaje = round(numero/total_pruebas*100, 1)) %>% 
  ungroup() %>% 
  filter(total_pruebas >= 100) %>%
  filter(resultado == "Positivo SARS-CoV-2") %>% 
  mutate(texto_mayor = ifelse(porcentaje > 36, str_c("(", porcentaje, "% pruebas positivas | ", as.character(comma(total_pruebas, accuracy = 1)), " pruebas", ")"), ""),
         texto_menor = ifelse(porcentaje < 36, str_c("(", porcentaje, "% | ", as.character(comma(total_pruebas, accuracy = 1)), ")"), "")) 

foo %>% 
  ggplot(aes(x = porcentaje, y = fct_reorder(sector, porcentaje))) +
  geom_col(fill = "#1E6847") +
  geom_text(aes(label = texto_mayor), fontface = "bold", color = "white", hjust = 1.05, size = 6) +
  geom_text(aes(label = texto_menor), fontface = "bold", hjust = -0.05, size = 6, color = "grey30") +
  scale_x_continuous(expand = c(0, 0), 
                     limits = c(0, max(foo$porcentaje) + max(foo$porcentaje)* 0.1)) +
  labs(title = "Porcentaje de pruebas de Covid-19 realizadas en cada institución del\nsector Salud cuyo resultado fue positivo",
       subtitle = subtitulo_mx,
       x = NULL,
       y = NULL,
       caption = "\nElaborado por @segasi para el Buró de Investigación de ADN40 / Fuentes: Secretaría de Salud.\nNota: La gráfica solo incluye datos de las instituciones que han realizado 100 o más pruebas.") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22),
        panel.grid = element_blank(),
        axis.text.x = element_blank()) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0)) +
  ggsave(str_c(ruta_graficas_mx, "03_08_porcentaje_de_pruebas_positivas_por_sector_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 12)

### Gráfica 03_09: Número de pruebas de Covid-19 realizadas en cada institución del sector Salud en cada entidad ----
foo <- 
  mx_datos %>% 
  group_by(sector, entidad_uni_med) %>% 
  summarise(numero = n()) %>% 
  ungroup() %>% 
  complete(sector, entidad_uni_med) %>% 
  mutate(numero = ifelse(is.na(numero), 0, numero)) %>% 
  group_by(sector) %>% 
  mutate(total_pruebas = sum(numero)) %>% 
  ungroup() %>% 
  filter(total_pruebas >= 100) 

foo %>%
  ggplot(aes(x = fct_rev(fct_reorder(sector, total_pruebas)), 
             y = fct_rev(entidad_uni_med), 
             fill = log(numero + 1))) +
  geom_tile(color = "grey80") +
  scale_fill_gradient(low = "#ffffff", 
                      high = "#1E6847", 
                      breaks = 0:7,
                      labels = c(str_c("0", " (mín.)"), "", "", "", "", "", "", str_c(comma(max(foo$numero)), " (máx.)"))) +
  tema +
  theme(panel.grid = element_blank()) +
  labs(title = "Número de pruebas de Covid-19 realizadas en cada institución del sector\nSalud en cada entidad",
       subtitle = subtitulo_mx,
       x = "\n",
       y = NULL,
       fill = "Número de pruebas (log)",
       caption = "Elaborado por @segasi para el Buró de Investigación de ADN40 / Fuentes: Secretaría de Salud.\nNota: La gráfica solo incluye datos de las instituciones que han realizado 100 o más pruebas.\n") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22),
        legend.position = c(0.86, -0.1), 
        legend.direction = "horizontal",
        legend.key.width = unit(1.3, "cm"),
        panel.grid = element_blank()) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0)) +
  ggsave(str_c(ruta_graficas_mx, "03_09_numero_de_pruebas_por_sector_y_estado_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 17, height = 13)


### Gráfica 03_10: Porcentaje de pruebas de Covid-19 realizadas a mujeres y hombres en cada entidad ----

mx_datos %>% 
  count(entidad_uni_med, sexo) %>% 
  group_by(entidad_uni_med) %>% 
  mutate(pruebas_totales = sum(n)) %>% 
  ungroup() %>% 
  mutate(porcentaje = n/pruebas_totales*100, 
         ranking = ifelse(sexo == "Mujer", rank(-porcentaje, ties.method = "first"), NA)) %>% 
  fill(ranking, .direction = "up") %>% 
  ggplot(aes(x = porcentaje, y = fct_reorder(entidad_uni_med, ranking), 
             fill = sexo)) +
  geom_col() +
  geom_vline(xintercept = seq(10, 90, 10), linetype = 3, color = "white") +
  scale_x_continuous(breaks = seq(0, 100, 10), 
                     expand = c(0, 0),
                     limits = c(-1, 101)) +
  scale_fill_manual(values = c("grey70", "#1E6847")) +
  labs(title = "Porcentaje de pruebas de Covid-19 realizadas a mujeres y hombres\nen cada entidad",
       subtitle = subtitulo_mx,
       x = "",
       y = NULL,
       fill = "",
       caption = "Elaborado por @segasi para el Buró de Investigación de ADN40 / Fuentes: Secretaría de Salud.\n") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22),
        legend.position = c(0.85, -0.1), 
        legend.direction = "horizontal",
        legend.key.width = unit(1.3, "cm"),
        panel.grid = element_blank()) +
  ggsave(str_c(ruta_graficas_mx, "03_10_porcentaje_pruebas_por_estado_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 10)

### Gráfica 03_11: Porcentaje de pruebas de Covid-19 realizadas a mujeres y hombres en cada entidad ----

mx_datos %>% 
  group_by(entidad_uni_med, sexo) %>% 
  summarise(num_positivos = sum(resultado == "Positivo SARS-CoV-2")) %>% 
  ungroup() %>% 
  group_by(entidad_uni_med) %>% 
  mutate(por_positivo_por_genero = round(num_positivos/sum(num_positivos)*100, 1)) %>% 
  ungroup() %>% 
  mutate(ranking = ifelse(sexo == "Mujer", rank(-por_positivo_por_genero, ties.method = "first"), NA)) %>% 
  fill(ranking, .direction = "up") %>%   
  ggplot(aes(x = por_positivo_por_genero, y = fct_reorder(entidad_uni_med, ranking), 
             fill = sexo)) +
  geom_col() +
  geom_vline(xintercept = seq(10, 90, 10), linetype = 3, color = "white") +
  scale_x_continuous(breaks = seq(0, 100, 10), 
                     expand = c(0, 0),
                     limits = c(-1, 101)) +
  scale_fill_manual(values = c("grey70", "#1E6847")) +
  labs(title = "Proporción de pruebas de Covid-19 con resultado positivo por género\nen cada entidad",
       subtitle = subtitulo_mx,
       x = "",
       y = NULL,
       fill = "",
       caption = "Elaborado por @segasi para el Buró de Investigación de ADN40 / Fuentes: Secretaría de Salud.\n") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22),
        legend.position = c(0.85, -0.1), 
        legend.direction = "horizontal",
        legend.key.width = unit(1.3, "cm"),
        panel.grid = element_blank()) +
  ggsave(str_c(ruta_graficas_mx, "03_11_porcentaje_pruebas_positivas_por_estado_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 10)



### Generar tibbles que usaré en las siguientes gráficas ----

# mx_datos_evolucion ----
mx_datos_evolucion <- 
  mx_datos %>% 
  filter(resultado == "Positivo SARS-CoV-2") %>% 
  rowid_to_column("id") %>% 
  mutate(rango_edad = case_when(edad <= 10 ~ "0-10",
                                edad > 10 & edad <= 20 ~ "11-20",
                                edad > 20 & edad <= 30 ~ "21-30",
                                edad > 30 & edad <= 40 ~ "31-40",
                                edad > 40 & edad <= 50 ~ "41-50", 
                                edad > 50 & edad <= 60 ~ "51-60",
                                edad > 60 & edad <= 70 ~ "61-70",
                                edad > 70 & edad <= 80 ~ "71-80",
                                edad > 80 ~ "+80"),
         rango_edad = fct_relevel(rango_edad, "0-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "+80"), 
         genero = ifelse(sexo == "Mujer", "Mujeres", "Hombres"),
         genero = fct_relevel(genero, "Mujeres", "Hombres"),
         fallecio = ifelse(is.na(fecha_def), "No", "Sí")) %>% 
  select(id, rango_edad, genero, resultado, tipo_paciente, fallecio, fecha_def, uci, intubado, neumonia, embarazo,
         diabetes:hipertension, cardiovascular:tabaquismo) 

# num_pacientes_comorbilidad ----
num_pacientes_sin_comorbilidad <- 
  mx_datos_evolucion %>% 
  transmute(sin_comorbilidad = ifelse(diabetes == "No" & epoc == "No" & asma == "No" & inmusupr == "No" & cardiovascular == "No" & obesidad == "No" & renal_cronica == "No" & tabaquismo == "No" & hipertension == "No", "sin_comorbilidad", "con_comorbilidad")) %>% 
  rename(comorbilidad = sin_comorbilidad) %>% 
  group_by(comorbilidad) %>% 
  summarise(total = sum(comorbilidad == "sin_comorbilidad")) %>% 
  ungroup() %>% 
  filter(comorbilidad == "sin_comorbilidad")

num_pacientes_comorbilidad <- 
  mx_datos_evolucion %>% 
  select(tipo_paciente, uci:neumonia, diabetes:tabaquismo) %>% 
  pivot_longer(diabetes:tabaquismo,
               names_to = "comorbilidad",
               values_to = "valor") %>% 
  group_by(comorbilidad) %>% 
  summarise(total = sum(valor == "Sí")) %>% 
  ungroup()

num_pacientes_comorbilidad <- 
  num_pacientes_comorbilidad %>% 
  bind_rows(num_pacientes_sin_comorbilidad)


# num_pacientes_comorbilidad_x_edad ----
num_pacientes_sin_comorbilidad_x_edad <- 
  mx_datos_evolucion %>% 
  mutate(sin_comorbilidad = ifelse(diabetes == "No" & epoc == "No" & asma == "No" & inmusupr == "No" & cardiovascular == "No" & obesidad == "No" & renal_cronica == "No" & tabaquismo == "No" & hipertension == "No", "sin_comorbilidad", "con_comorbilidad")) %>% 
  rename(comorbilidad = sin_comorbilidad) %>% 
  group_by(rango_edad, comorbilidad) %>% 
  summarise(total = sum(comorbilidad == "sin_comorbilidad")) %>% 
  ungroup() %>% 
  filter(comorbilidad == "sin_comorbilidad") 

num_pacientes_comorbilidad_x_edad <- 
  mx_datos_evolucion %>% 
  select(rango_edad, tipo_paciente, uci:neumonia, diabetes:tabaquismo) %>% 
  pivot_longer(diabetes:tabaquismo,
               names_to = "comorbilidad",
               values_to = "valor") %>% 
  group_by(rango_edad, comorbilidad) %>% 
  summarise(total = sum(valor == "Sí")) %>% 
  ungroup()

num_pacientes_comorbilidad_x_edad <- 
  num_pacientes_comorbilidad_x_edad %>% 
  bind_rows(num_pacientes_sin_comorbilidad_x_edad)

### Gráfica 04_01: Porcentaje de casos confirmados con Covid-19 hospitalizados, de acuerdo con el tipo de comorbilidad ----

mx_datos_evolucion %>% 
  select(tipo_paciente, uci:neumonia, diabetes:tabaquismo) %>% 
  mutate(sin_comorbilidad = ifelse(diabetes == "No" & epoc == "No" & asma == "No" & inmusupr == "No" & cardiovascular == "No" & obesidad == "No" & renal_cronica == "No" & tabaquismo == "No" & hipertension == "No", "Sí", "No")) %>% 
  pivot_longer(diabetes:sin_comorbilidad,
               names_to = "comorbilidad",
               values_to = "valor") %>% 
  group_by(tipo_paciente, comorbilidad) %>% 
  summarise(subtotal = sum(valor == "Sí")) %>% 
  ungroup() %>% 
  left_join(num_pacientes_comorbilidad, by = "comorbilidad") %>% 
  mutate(porcentaje = round(subtotal/total*100, 1),
         comorbilidad = case_when(comorbilidad == "asma" ~ "Asma",
                                  comorbilidad == "cardiovascular" ~ "Enfermedades\ncardiovasculares",
                                  comorbilidad == "diabetes" ~ "Diabetes",
                                  comorbilidad == "epoc" ~ "EPOC",
                                  comorbilidad == "hipertension" ~ "Hipertensión",
                                  comorbilidad == "inmusupr" ~ "Inmunosupresión",
                                  comorbilidad == "obesidad" ~ "Obesidad",
                                  comorbilidad == "renal_cronica" ~ "Insuficiencia\nrenal crónica",
                                  comorbilidad == "tabaquismo" ~ "Tabaquismo",
                                  comorbilidad == "sin_comorbilidad" ~ "Sin comorbilidad"),
         comorbilidad_bis = str_c(comorbilidad, "\n (", comma(total), ")")) %>% 
  filter(tipo_paciente == "Hospitalizado") %>% 
  mutate(color_barras = ifelse(comorbilidad == "Sin comorbilidad", "Sí", "No")) %>% 
  ggplot(aes(x = fct_reorder(comorbilidad_bis, porcentaje), y = porcentaje)) +
  geom_col(aes(fill = color_barras)) +
  geom_text(aes(label = str_c(porcentaje, "%")), vjust = 1.5, size = 6, color = "white", fontface = "bold") +
  scale_y_continuous(breaks = seq(0, 100, 10),
                     expand = c(0, 0)) +
  scale_fill_manual(values = c("grey70", "#1E6847")) +
  labs(title = "Porcentaje de casos confirmados con Covid-19 hospitalizados, de acuerdo\ncon el tipo de comorbilidad",
       subtitle = subtitulo_mx,
       x = "",
       y = NULL,
       fill = "",
       caption = "Elaborado por @segasi para el Buró de Investigación de ADN40 / Fuente: Secretaría de Salud.\nNotas: Un paciente puede tener más de una comorbilidad y cada una se registra por separado. El número entre paréntesis indica el total de pacientes con esa condición.") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22),
        legend.position = "none", 
        axis.text.y = element_blank(),
        panel.grid.major = element_blank()) +
  ggsave(str_c(ruta_graficas_mx, "04_01_comorbilidades_hospitalizados_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)



### Gráfica 04_02: Porcentaje de casos confirmados con Covid-19 y neumonia, de acuerdo con el tipo de comorbilidad ----
mx_datos_evolucion %>% 
  select(tipo_paciente, uci:neumonia, diabetes:tabaquismo) %>% 
  mutate(sin_comorbilidad = ifelse(diabetes == "No" & epoc == "No" & asma == "No" & inmusupr == "No" & cardiovascular == "No" & obesidad == "No" & renal_cronica == "No" & tabaquismo == "No" & hipertension == "No", "Sí", "No")) %>% 
  pivot_longer(diabetes:sin_comorbilidad,
               names_to = "comorbilidad",
               values_to = "valor") %>% 
  group_by(neumonia, comorbilidad) %>% 
  summarise(subtotal = sum(valor == "Sí")) %>% 
  ungroup() %>% 
  left_join(num_pacientes_comorbilidad, by = "comorbilidad") %>% 
  mutate(porcentaje = round(subtotal/total*100, 1),
         comorbilidad = case_when(comorbilidad == "asma" ~ "Asma",
                                  comorbilidad == "cardiovascular" ~ "Enfermedades\ncardiovasculares",
                                  comorbilidad == "diabetes" ~ "Diabetes",
                                  comorbilidad == "epoc" ~ "EPOC",
                                  comorbilidad == "hipertension" ~ "Hipertensión",
                                  comorbilidad == "inmusupr" ~ "Inmunosupresión",
                                  comorbilidad == "obesidad" ~ "Obesidad",
                                  comorbilidad == "renal_cronica" ~ "Insuficiencia\nrenal crónica",
                                  comorbilidad == "tabaquismo" ~ "Tabaquismo",
                                  comorbilidad == "sin_comorbilidad" ~ "Sin comorbilidad"),
         comorbilidad_bis = str_c(comorbilidad, "\n (", comma(total), ")"))%>% 
  filter(neumonia == "Sí") %>% 
  mutate(color_barras = ifelse(comorbilidad == "Sin comorbilidad", "Sí", "No")) %>% 
  ggplot(aes(x = fct_reorder(comorbilidad_bis, porcentaje), y = porcentaje)) +
  geom_col(aes(fill = color_barras)) +
  geom_text(aes(label = str_c(porcentaje, "%")), vjust = 1.5, size = 6, color = "white", fontface = "bold") +
  scale_y_continuous(breaks = seq(0, 100, 10),
                     expand = c(0, 0)) +
  scale_fill_manual(values = c("grey70", "#1E6847")) +
  labs(title = "Porcentaje de casos confirmados con Covid-19 y neumonia, de acuerdo con el\ntipo de comorbilidad",
       subtitle = subtitulo_mx,
       x = "",
       y = NULL,
       fill = "",
       caption = "Elaborado por @segasi para el Buró de Investigación de ADN40 / Fuente: Secretaría de Salud.\nNotas: Un paciente puede tener más de una comorbilidad y cada una se registra por separado. El número entre paréntesis indica el total de pacientes con esa condición.") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22),
        legend.position = "none", 
        axis.text.y = element_blank(),
        panel.grid.major = element_blank()) +
  ggsave(str_c(ruta_graficas_mx, "04_02_comorbilidades_neumonia_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)




### Gráfica 04_03: Porcentaje de casos confirmados con Covid-19 que requirieron ser intubados, de acuerdo con el tipo de comorbilidad ----
mx_datos_evolucion %>% 
  select(tipo_paciente, uci:neumonia, diabetes:tabaquismo) %>% 
  mutate(sin_comorbilidad = ifelse(diabetes == "No" & epoc == "No" & asma == "No" & inmusupr == "No" & cardiovascular == "No" & obesidad == "No" & renal_cronica == "No" & tabaquismo == "No" & hipertension == "No", "Sí", "No")) %>% 
  pivot_longer(diabetes:sin_comorbilidad,
               names_to = "comorbilidad",
               values_to = "valor") %>% 
  group_by(intubado, comorbilidad) %>% 
  summarise(subtotal = sum(valor == "Sí")) %>% 
  ungroup() %>% 
  left_join(num_pacientes_comorbilidad, by = "comorbilidad") %>% 
  mutate(porcentaje = round(subtotal/total*100, 1),
         comorbilidad = case_when(comorbilidad == "asma" ~ "Asma",
                                  comorbilidad == "cardiovascular" ~ "Enfermedades\ncardiovasculares",
                                  comorbilidad == "diabetes" ~ "Diabetes",
                                  comorbilidad == "epoc" ~ "EPOC",
                                  comorbilidad == "hipertension" ~ "Hipertensión",
                                  comorbilidad == "inmusupr" ~ "Inmunosupresión",
                                  comorbilidad == "obesidad" ~ "Obesidad",
                                  comorbilidad == "renal_cronica" ~ "Insuficiencia\nrenal crónica",
                                  comorbilidad == "tabaquismo" ~ "Tabaquismo",
                                  comorbilidad == "sin_comorbilidad" ~ "Sin comorbilidad"),
         comorbilidad_bis = str_c(comorbilidad, "\n (", comma(total), ")"))%>% 
  filter(intubado == "Sí") %>% 
  mutate(color_barras = ifelse(comorbilidad == "Sin comorbilidad", "Sí", "No")) %>% 
  ggplot(aes(x = fct_reorder(comorbilidad_bis, porcentaje), y = porcentaje)) +
  geom_col(aes(fill = color_barras)) +
  geom_text(aes(label = str_c(porcentaje, "%")), vjust = 1.5, size = 6, color = "white", fontface = "bold") +
  scale_y_continuous(breaks = seq(0, 100, 10),
                     expand = c(0, 0)) +
  scale_fill_manual(values = c("grey70", "#1E6847")) +
  labs(title = "Porcentaje de casos confirmados con Covid-19 que requirieron ser intubados,\nde acuerdo con el tipo de comorbilidad",
       subtitle = subtitulo_mx,
       x = "",
       y = NULL,
       fill = "",
       caption = "Elaborado por @segasi para el Buró de Investigación de ADN40 / Fuente: Secretaría de Salud.\nNotas: Un paciente puede tener más de una comorbilidad y cada una se registra por separado. El número entre paréntesis indica el total de pacientes con esa condición.") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22),
        legend.position = "none", 
        axis.text.y = element_blank(),
        panel.grid.major = element_blank()) +
  ggsave(str_c(ruta_graficas_mx, "04_03_comorbilidades_intubado_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)



### Gráfica 04_04: Porcentaje de casos confirmados con Covid-19 internados en una Unidad de Cuidados Intensivos, de acuerdo con el tipo de comorbilidad ----
mx_datos_evolucion %>% 
  select(tipo_paciente, uci:neumonia, diabetes:tabaquismo) %>% 
  mutate(sin_comorbilidad = ifelse(diabetes == "No" & epoc == "No" & asma == "No" & inmusupr == "No" & cardiovascular == "No" & obesidad == "No" & renal_cronica == "No" & tabaquismo == "No" & hipertension == "No", "Sí", "No")) %>% 
  pivot_longer(diabetes:sin_comorbilidad,
               names_to = "comorbilidad",
               values_to = "valor") %>% 
  group_by(uci, comorbilidad) %>% 
  summarise(subtotal = sum(valor == "Sí")) %>% 
  ungroup() %>% 
  left_join(num_pacientes_comorbilidad, by = "comorbilidad") %>% 
  mutate(porcentaje = round(subtotal/total*100, 1),
         comorbilidad = case_when(comorbilidad == "asma" ~ "Asma",
                                  comorbilidad == "cardiovascular" ~ "Enfermedades\ncardiovasculares",
                                  comorbilidad == "diabetes" ~ "Diabetes",
                                  comorbilidad == "epoc" ~ "EPOC",
                                  comorbilidad == "hipertension" ~ "Hipertensión",
                                  comorbilidad == "inmusupr" ~ "Inmunosupresión",
                                  comorbilidad == "obesidad" ~ "Obesidad",
                                  comorbilidad == "renal_cronica" ~ "Insuficiencia\nrenal crónica",
                                  comorbilidad == "tabaquismo" ~ "Tabaquismo",
                                  comorbilidad == "sin_comorbilidad" ~ "Sin comorbilidad"),
         comorbilidad_bis = str_c(comorbilidad, "\n (", comma(total), ")"))%>% 
  filter(uci == "Sí") %>% 
  mutate(color_barras = ifelse(comorbilidad == "Sin comorbilidad", "Sí", "No")) %>% 
  ggplot(aes(x = fct_reorder(comorbilidad_bis, porcentaje), y = porcentaje)) +
  geom_col(aes(fill = color_barras)) +
  geom_text(aes(label = str_c(porcentaje, "%")), vjust = 1.5, size = 6, color = "white", fontface = "bold") +
  scale_y_continuous(breaks = seq(0, 100, 10),
                     expand = c(0, 0)) +
  scale_fill_manual(values = c("grey70", "#1E6847")) +
  labs(title = "Porcentaje de casos confirmados con Covid-19 internados en una Unidad de\nCuidados Intensivos, de acuerdo con el tipo de comorbilidad",
       subtitle = subtitulo_mx,
       x = "",
       y = NULL,
       fill = "",
       caption = "Elaborado por @segasi para el Buró de Investigación de ADN40 / Fuente: Secretaría de Salud.\nNotas: Un paciente puede tener más de una comorbilidad y cada una se registra por separado. El número entre paréntesis indica el total de pacientes con esa condición.") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22),
        legend.position = "none", 
        axis.text.y = element_blank(),
        panel.grid.major = element_blank()) +
  ggsave(str_c(ruta_graficas_mx, "04_04_comorbilidades_uci_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)



### Gráfica 04_05: Porcentaje de casos confirmados con Covid-19 y una comorbiliad fallecieron ----
mx_datos_evolucion %>% 
  select(fallecio, tipo_paciente, uci:neumonia, diabetes:tabaquismo) %>% 
  mutate(sin_comorbilidad = ifelse(diabetes == "No" & epoc == "No" & asma == "No" & inmusupr == "No" & cardiovascular == "No" & obesidad == "No" & renal_cronica == "No" & tabaquismo == "No" & hipertension == "No", "Sí", "No")) %>% 
  pivot_longer(diabetes:sin_comorbilidad,
               names_to = "comorbilidad",
               values_to = "valor") %>% 
  group_by(fallecio, comorbilidad) %>% 
  summarise(subtotal = sum(valor == "Sí")) %>% 
  ungroup() %>% 
  left_join(num_pacientes_comorbilidad, by = "comorbilidad") %>% 
  mutate(porcentaje = round(subtotal/total*100, 1),
         comorbilidad = case_when(comorbilidad == "asma" ~ "Asma",
                                  comorbilidad == "cardiovascular" ~ "Enfermedades\ncardiovasculares",
                                  comorbilidad == "diabetes" ~ "Diabetes",
                                  comorbilidad == "epoc" ~ "EPOC",
                                  comorbilidad == "hipertension" ~ "Hipertensión",
                                  comorbilidad == "inmusupr" ~ "Inmunosupresión",
                                  comorbilidad == "obesidad" ~ "Obesidad",
                                  comorbilidad == "renal_cronica" ~ "Insuficiencia\nrenal crónica",
                                  comorbilidad == "tabaquismo" ~ "Tabaquismo",
                                  comorbilidad == "sin_comorbilidad" ~ "Sin comorbilidad"),
         comorbilidad_bis = str_c(comorbilidad, "\n (", comma(total), ")"))%>% 
  filter(fallecio == "Sí") %>% 
  mutate(color_barras = ifelse(comorbilidad == "Sin comorbilidad", "Sí", "No")) %>% 
  ggplot(aes(x = fct_reorder(comorbilidad_bis, porcentaje), y = porcentaje)) +
  geom_col(aes(fill = color_barras)) +
  geom_text(aes(label = str_c(porcentaje, "%")), vjust = 1.5, size = 6, color = "white", fontface = "bold") +
  scale_y_continuous(breaks = seq(0, 100, 10),
                     expand = c(0, 0)) +
  scale_fill_manual(values = c("grey70", "#1E6847")) +
  labs(title = "Porcentaje de casos confirmados con Covid-19 que fallecieron, de acuerdo\ncon el tipo de comorbilidad",
       subtitle = subtitulo_mx,
       x = "",
       y = NULL,
       fill = "",
       caption = "Elaborado por @segasi para el Buró de Investigación de ADN40 / Fuente: Secretaría de Salud.\nNotas: Un paciente puede tener más de una comorbilidad y cada una se registra por separado. El número entre paréntesis indica el total de pacientes con esa condición.") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 22),
        legend.position = "none", 
        axis.text.y = element_blank(),
        panel.grid.major = element_blank()) +
  ggsave(str_c(ruta_graficas_mx, "04_05_comorbilidades_fallecieron_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 16, height = 9)




### Gráfica 04_06: Porcentaje de casos confirmados con Covid-19 hospitalizados, por comorbilidad y rango de edad ----
mx_datos_evolucion %>% 
  select(rango_edad, fallecio, tipo_paciente, uci:neumonia, diabetes:tabaquismo) %>% 
  mutate(sin_comorbilidad = ifelse(diabetes == "No" & epoc == "No" & asma == "No" & inmusupr == "No" & cardiovascular == "No" & obesidad == "No" & renal_cronica == "No" & tabaquismo == "No" & hipertension == "No", "Sí", "No")) %>% 
  pivot_longer(diabetes:sin_comorbilidad,
               names_to = "comorbilidad",
               values_to = "valor") %>% 
  group_by(rango_edad, tipo_paciente, comorbilidad) %>% 
  summarise(subtotal = sum(valor == "Sí")) %>% 
  ungroup() %>% 
  complete(rango_edad, tipo_paciente, comorbilidad) %>% 
  mutate(subtotal = ifelse(is.na(subtotal), 0, subtotal)) %>%
  left_join(num_pacientes_comorbilidad_x_edad, by = c("rango_edad", "comorbilidad")) %>% 
  mutate(porcentaje = round(subtotal/total*100, 1),
         porcentaje = ifelse(is.na(porcentaje), 0, porcentaje),
         comorbilidad = case_when(comorbilidad == "asma" ~ "Asma",
                                  comorbilidad == "cardiovascular" ~ "Enfermedades\ncardiovasculares",
                                  comorbilidad == "diabetes" ~ "Diabetes",
                                  comorbilidad == "epoc" ~ "EPOC",
                                  comorbilidad == "hipertension" ~ "Hipertensión",
                                  comorbilidad == "inmusupr" ~ "Inmunosupresión",
                                  comorbilidad == "obesidad" ~ "Obesidad",
                                  comorbilidad == "renal_cronica" ~ "Insuficiencia\nrenal crónica",
                                  comorbilidad == "tabaquismo" ~ "Tabaquismo",
                                  comorbilidad == "sin_comorbilidad" ~ "Sin comorbilidad"),
         comorbilidad_bis = str_c(comorbilidad, "\n (", comma(total), ")")) %>%
  filter(tipo_paciente == "Hospitalizado") %>% 
  mutate(rango_edad = ifelse(rango_edad == "0-10", "0 a 10 años", as.character(rango_edad)),
         rango_edad = ifelse(rango_edad == "+80", "Más de 80 años", rango_edad),
         comorbilidad = fct_relevel(comorbilidad, "Sin comorbilidad", after = Inf)) %>% 
  ggplot(aes(x = rango_edad, y = fct_rev(comorbilidad), fill = porcentaje)) +
  geom_tile(color = "white") +
  geom_text(aes(label = str_c(porcentaje, "%"),
                color = ifelse(porcentaje > 10, "white", "blue")), fontface = "bold", size = 6, vjust = -0.4) +
  geom_text(aes(label = ifelse(comorbilidad == "Asma" & rango_edad == "0 a 10 años" | comorbilidad == "Sin comorbilidad" & rango_edad == "Más de 80 años" , str_c("\n(", comma(subtotal), ifelse(total == 1, " hospita. | ", " hospita. | "), comma(total), ifelse(total == 1, " caso)", " casos)")), str_c("\n(", comma(subtotal), " | ", comma(total), ")")),
                color = ifelse(porcentaje > 10, "white", "blue")), fontface = "bold", size = 3.5, vjust = 0.6) +
  scale_fill_gradient(low = "grey95", 
                      high = "grey10") +
  scale_color_manual(values = c("grey30", "white"), guide = "none") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Porcentaje de casos confirmados con Covid-19 hospitalizados, por comorbilidad\ny rango de edad",
       subtitle = subtitulo_mx,
       x = "",
       y = NULL,
       fill = "Porcentaje",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40. Fuente: Secretaría de Salud.\n\nNotas: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición\noperacional de caso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública\nreconocidos por el InDRE\". Un paciente puede tener más de una comorbilidad y cada una se registra por separado.") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 25),
        panel.grid.major = element_blank(),
        legend.position = c(0.86, -0.14), 
        legend.direction = "horizontal",
        legend.key.width = unit(1.3, "cm"),
        axis.ticks.y = element_blank()) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0)) +
  ggsave(str_c(ruta_graficas_mx, "04_06_comorbilidades_rango_de_edad_hospitalizados_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 18.5, height = 12)

### Gráfica 04_07: Porcentaje de casos confirmados con Covid-19 y neumonía, por comorbilidad y rango de edad ----
mx_datos_evolucion %>% 
  select(rango_edad, fallecio, tipo_paciente, uci:neumonia, diabetes:tabaquismo) %>% 
  mutate(sin_comorbilidad = ifelse(diabetes == "No" & epoc == "No" & asma == "No" & inmusupr == "No" & cardiovascular == "No" & obesidad == "No" & renal_cronica == "No" & tabaquismo == "No" & hipertension == "No", "Sí", "No")) %>% 
  pivot_longer(diabetes:sin_comorbilidad,
               names_to = "comorbilidad",
               values_to = "valor") %>% 
  group_by(rango_edad, neumonia, comorbilidad) %>% 
  summarise(subtotal = sum(valor == "Sí")) %>% 
  ungroup() %>% 
  complete(rango_edad, neumonia, comorbilidad) %>% 
  mutate(subtotal = ifelse(is.na(subtotal), 0, subtotal)) %>%
  left_join(num_pacientes_comorbilidad_x_edad, by = c("rango_edad", "comorbilidad")) %>% 
  mutate(porcentaje = round(subtotal/total*100, 1),
         porcentaje = ifelse(is.na(porcentaje), 0, porcentaje),
         comorbilidad = case_when(comorbilidad == "asma" ~ "Asma",
                                  comorbilidad == "cardiovascular" ~ "Enfermedades\ncardiovasculares",
                                  comorbilidad == "diabetes" ~ "Diabetes",
                                  comorbilidad == "epoc" ~ "EPOC",
                                  comorbilidad == "hipertension" ~ "Hipertensión",
                                  comorbilidad == "inmusupr" ~ "Inmunosupresión",
                                  comorbilidad == "obesidad" ~ "Obesidad",
                                  comorbilidad == "renal_cronica" ~ "Insuficiencia\nrenal crónica",
                                  comorbilidad == "tabaquismo" ~ "Tabaquismo",
                                  comorbilidad == "sin_comorbilidad" ~ "Sin comorbilidad"),
         comorbilidad_bis = str_c(comorbilidad, "\n (", comma(total), ")")) %>%
  filter(neumonia == "Sí") %>% 
  mutate(rango_edad = ifelse(rango_edad == "0-10", "0 a 10 años", as.character(rango_edad)),
         rango_edad = ifelse(rango_edad == "+80", "Más de 80 años", rango_edad),
         comorbilidad = fct_relevel(comorbilidad, "Sin comorbilidad", after = Inf)) %>% 
  ggplot(aes(x = rango_edad, y = fct_rev(comorbilidad), fill = porcentaje)) +
  geom_tile(color = "white") +
  geom_text(aes(label = str_c(porcentaje, "%"),
                color = ifelse(porcentaje > 10, "white", "blue")), fontface = "bold", size = 6, vjust = -0.4) +
  geom_text(aes(label = ifelse(comorbilidad == "Asma" & rango_edad == "0 a 10 años" | comorbilidad == "Sin comorbilidad" & rango_edad == "Más de 80 años" , str_c("\n(", comma(subtotal), ifelse(total == 1, " neumonía | ", " neumonía | "), comma(total), ifelse(total == 1, " caso)", " casos)")), str_c("\n(", comma(subtotal), " | ", comma(total), ")")),
                color = ifelse(porcentaje > 10, "white", "blue")), fontface = "bold", size = 3.5, vjust = 0.6) +
  scale_fill_gradient(low = "grey95", 
                      high = "grey10") +
  scale_color_manual(values = c("grey30", "white"), guide = "none") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Porcentaje de casos confirmados con Covid-19 y neumonía, por comorbilidad\ny rango de edad",
       subtitle = subtitulo_mx,
       x = "",
       y = NULL,
       fill = "Porcentaje",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40. Fuente: Secretaría de Salud.\n\nNotas: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición\noperacional de caso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública\nreconocidos por el InDRE\". Un paciente puede tener más de una comorbilidad y cada una se registra por separado.") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 25),
        panel.grid.major = element_blank(),
        legend.position = c(0.86, -0.14), 
        legend.direction = "horizontal",
        legend.key.width = unit(1.3, "cm"),
        axis.ticks.y = element_blank()) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0)) +
  ggsave(str_c(ruta_graficas_mx, "04_07_comorbilidades_rango_de_edad_neumonia_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 18, height = 12)

### Gráfica 04_08: Porcentaje de casos confirmados con Covid-19 que requirieron ser intubados, por comorbilidad y rango de edad ----
mx_datos_evolucion %>% 
  select(rango_edad, fallecio, tipo_paciente, uci:neumonia, diabetes:tabaquismo) %>% 
  mutate(sin_comorbilidad = ifelse(diabetes == "No" & epoc == "No" & asma == "No" & inmusupr == "No" & cardiovascular == "No" & obesidad == "No" & renal_cronica == "No" & tabaquismo == "No" & hipertension == "No", "Sí", "No")) %>% 
  pivot_longer(diabetes:sin_comorbilidad,
               names_to = "comorbilidad",
               values_to = "valor") %>% 
  group_by(rango_edad, intubado, comorbilidad) %>% 
  summarise(subtotal = sum(valor == "Sí")) %>% 
  ungroup() %>% 
  complete(rango_edad, intubado, comorbilidad) %>% 
  mutate(subtotal = ifelse(is.na(subtotal), 0, subtotal)) %>%
  left_join(num_pacientes_comorbilidad_x_edad, by = c("rango_edad", "comorbilidad")) %>% 
  mutate(porcentaje = round(subtotal/total*100, 1),
         porcentaje = ifelse(is.na(porcentaje), 0, porcentaje),
         comorbilidad = case_when(comorbilidad == "asma" ~ "Asma",
                                  comorbilidad == "cardiovascular" ~ "Enfermedades\ncardiovasculares",
                                  comorbilidad == "diabetes" ~ "Diabetes",
                                  comorbilidad == "epoc" ~ "EPOC",
                                  comorbilidad == "hipertension" ~ "Hipertensión",
                                  comorbilidad == "inmusupr" ~ "Inmunosupresión",
                                  comorbilidad == "obesidad" ~ "Obesidad",
                                  comorbilidad == "renal_cronica" ~ "Insuficiencia\nrenal crónica",
                                  comorbilidad == "tabaquismo" ~ "Tabaquismo",
                                  comorbilidad == "sin_comorbilidad" ~ "Sin comorbilidad"),
         comorbilidad_bis = str_c(comorbilidad, "\n (", comma(total), ")")) %>%
  filter(intubado == "Sí") %>% 
  mutate(rango_edad = ifelse(rango_edad == "0-10", "0 a 10 años", as.character(rango_edad)),
         rango_edad = ifelse(rango_edad == "+80", "Más de 80 años", rango_edad),
         comorbilidad = fct_relevel(comorbilidad, "Sin comorbilidad", after = Inf)) %>% 
  ggplot(aes(x = rango_edad, y = fct_rev(comorbilidad), fill = porcentaje)) +
  geom_tile(color = "white") +
  geom_text(aes(label = str_c(porcentaje, "%"),
                color = ifelse(porcentaje > 10, "white", "blue")), fontface = "bold", size = 6, vjust = -0.4) +
  geom_text(aes(label = ifelse(comorbilidad == "Asma" & rango_edad == "0 a 10 años" | comorbilidad == "Sin comorbilidad" & rango_edad == "Más de 80 años" , str_c("\n(", comma(subtotal), ifelse(total == 1, " intubados | ", " intubados | "), comma(total), ifelse(total == 1, " caso)", " casos)")), str_c("\n(", comma(subtotal), " | ", comma(total), ")")),
                color = ifelse(porcentaje > 10, "white", "blue")), fontface = "bold", size = 3.5, vjust = 0.6) +
  scale_fill_gradient(low = "grey95", 
                      high = "grey10") +
  scale_color_manual(values = c("grey30", "white"), guide = "none") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Porcentaje de casos confirmados con Covid-19 que requirieron ser intubados,\npor comorbilidad y rango de edad",
       subtitle = subtitulo_mx,
       x = "",
       y = NULL,
       fill = "Porcentaje",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40. Fuente: Secretaría de Salud.\n\nNotas: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición\noperacional de caso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública\nreconocidos por el InDRE\". Un paciente puede tener más de una comorbilidad y cada una se registra por separado.") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 25),
        panel.grid.major = element_blank(),
        legend.position = c(0.86, -0.14), 
        legend.direction = "horizontal",
        legend.key.width = unit(1.3, "cm"),
        axis.ticks.y = element_blank()) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0)) +
  ggsave(str_c(ruta_graficas_mx, "04_08_comorbilidades_rango_de_edad_intubados_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 18, height = 12)


### Gráfica 04_09: Porcentaje de casos confirmados con Covid-19 internados en una Unidad de Cuidados Intensivos, por comorbilidad y rango de edad ----
mx_datos_evolucion %>% 
  select(rango_edad, fallecio, tipo_paciente, uci:neumonia, diabetes:tabaquismo) %>% 
  mutate(sin_comorbilidad = ifelse(diabetes == "No" & epoc == "No" & asma == "No" & inmusupr == "No" & cardiovascular == "No" & obesidad == "No" & renal_cronica == "No" & tabaquismo == "No" & hipertension == "No", "Sí", "No")) %>% 
  pivot_longer(diabetes:sin_comorbilidad,
               names_to = "comorbilidad",
               values_to = "valor") %>% 
  group_by(rango_edad, uci, comorbilidad) %>% 
  summarise(subtotal = sum(valor == "Sí")) %>% 
  ungroup() %>% 
  complete(rango_edad, uci, comorbilidad) %>% 
  mutate(subtotal = ifelse(is.na(subtotal), 0, subtotal)) %>%
  left_join(num_pacientes_comorbilidad_x_edad, by = c("rango_edad", "comorbilidad")) %>% 
  mutate(porcentaje = round(subtotal/total*100, 1),
         porcentaje = ifelse(is.na(porcentaje), 0, porcentaje),
         comorbilidad = case_when(comorbilidad == "asma" ~ "Asma",
                                  comorbilidad == "cardiovascular" ~ "Enfermedades\ncardiovasculares",
                                  comorbilidad == "diabetes" ~ "Diabetes",
                                  comorbilidad == "epoc" ~ "EPOC",
                                  comorbilidad == "hipertension" ~ "Hipertensión",
                                  comorbilidad == "inmusupr" ~ "Inmunosupresión",
                                  comorbilidad == "obesidad" ~ "Obesidad",
                                  comorbilidad == "renal_cronica" ~ "Insuficiencia\nrenal crónica",
                                  comorbilidad == "tabaquismo" ~ "Tabaquismo",
                                  comorbilidad == "sin_comorbilidad" ~ "Sin comorbilidad"),
         comorbilidad_bis = str_c(comorbilidad, "\n (", comma(total), ")")) %>%
  filter(uci == "Sí") %>% 
  mutate(rango_edad = ifelse(rango_edad == "0-10", "0 a 10 años", as.character(rango_edad)),
         rango_edad = ifelse(rango_edad == "+80", "Más de 80 años", rango_edad),
         comorbilidad = fct_relevel(comorbilidad, "Sin comorbilidad", after = Inf)) %>% 
  ggplot(aes(x = rango_edad, y = fct_rev(comorbilidad), fill = porcentaje)) +
  geom_tile(color = "white") +
  geom_text(aes(label = str_c(porcentaje, "%"),
                color = ifelse(porcentaje > 10, "white", "blue")), fontface = "bold", size = 6, vjust = -0.4) +
  geom_text(aes(label = ifelse(comorbilidad == "Asma" & rango_edad == "0 a 10 años" | comorbilidad == "Sin comorbilidad" & rango_edad == "Más de 80 años" , str_c("\n(", comma(subtotal), ifelse(total == 1, " internado | ", " internados | "), comma(total), ifelse(total == 1, " caso)", " casos)")), str_c("\n(", comma(subtotal), " | ", comma(total), ")")),
                color = ifelse(porcentaje > 10, "white", "blue")), fontface = "bold", size = 3.5, vjust = 0.6) +
  scale_fill_gradient(low = "grey95", 
                      high = "grey10") +
  scale_color_manual(values = c("grey30", "white"), guide = "none") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Porcentaje de casos confirmados con Covid-19 internados en una Unidad de\nCuidados Intensivos, por comorbilidad y rango de edad",
       subtitle = subtitulo_mx,
       x = "",
       y = NULL,
       fill = "Porcentaje",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40. Fuente: Secretaría de Salud.\n\nNotas: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición\noperacional de caso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública\nreconocidos por el InDRE\". Un paciente puede tener más de una comorbilidad y cada una se registra por separado.") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 25),
        panel.grid.major = element_blank(),
        legend.position = c(0.86, -0.14), 
        legend.direction = "horizontal",
        legend.key.width = unit(1.3, "cm"),
        axis.ticks.y = element_blank()) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0)) +
  ggsave(str_c(ruta_graficas_mx, "04_09_comorbilidades_rango_de_edad_ucis_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 18, height = 12)


### Gráfica 04_10: Porcentaje de casos confirmados de Covid-19 que murieron, por comorbilidad y rango de edad ----
mx_datos_evolucion %>% 
  select(rango_edad, fallecio, tipo_paciente, uci:neumonia, diabetes:tabaquismo) %>% 
  mutate(sin_comorbilidad = ifelse(diabetes == "No" & epoc == "No" & asma == "No" & inmusupr == "No" & cardiovascular == "No" & obesidad == "No" & renal_cronica == "No" & tabaquismo == "No" & hipertension == "No", "Sí", "No")) %>% 
  pivot_longer(diabetes:sin_comorbilidad,
               names_to = "comorbilidad",
               values_to = "valor") %>% 
  group_by(rango_edad, fallecio, comorbilidad) %>% 
  summarise(subtotal = sum(valor == "Sí")) %>% 
  ungroup() %>% 
  complete(rango_edad, fallecio, comorbilidad) %>% 
  mutate(subtotal = ifelse(is.na(subtotal), 0, subtotal)) %>%
  left_join(num_pacientes_comorbilidad_x_edad, by = c("rango_edad", "comorbilidad")) %>% 
  mutate(porcentaje = round(subtotal/total*100, 1),
         porcentaje = ifelse(is.na(porcentaje), 0, porcentaje),
         comorbilidad = case_when(comorbilidad == "asma" ~ "Asma",
                                  comorbilidad == "cardiovascular" ~ "Enfermedades\ncardiovasculares",
                                  comorbilidad == "diabetes" ~ "Diabetes",
                                  comorbilidad == "epoc" ~ "EPOC",
                                  comorbilidad == "hipertension" ~ "Hipertensión",
                                  comorbilidad == "inmusupr" ~ "Inmunosupresión",
                                  comorbilidad == "obesidad" ~ "Obesidad",
                                  comorbilidad == "renal_cronica" ~ "Insuficiencia\nrenal crónica",
                                  comorbilidad == "tabaquismo" ~ "Tabaquismo",
                                  comorbilidad == "sin_comorbilidad" ~ "Sin comorbilidad"),
         comorbilidad_bis = str_c(comorbilidad, "\n (", comma(total), ")")) %>%
  filter(fallecio == "Sí") %>% 
  mutate(rango_edad = ifelse(rango_edad == "0-10", "0 a 10 años", as.character(rango_edad)),
         rango_edad = ifelse(rango_edad == "+80", "Más de 80 años", rango_edad),
         comorbilidad = fct_relevel(comorbilidad, "Sin comorbilidad", after = Inf)) %>% 
  ggplot(aes(x = rango_edad, y = fct_rev(comorbilidad), fill = porcentaje)) +
  geom_tile(color = "white") +
  geom_text(aes(label = str_c(porcentaje, "%"),
                color = ifelse(porcentaje > 10, "white", "blue")), fontface = "bold", size = 6, vjust = -0.4) +
  geom_text(aes(label = ifelse(comorbilidad == "Asma" & rango_edad == "0 a 10 años" | comorbilidad == "Sin comorbilidad" & rango_edad == "Más de 80 años" , str_c("\n(", comma(subtotal), ifelse(total == 1, " muerto | ", " muertos | "), comma(total), ifelse(total == 1, " caso)", " casos)")), str_c("\n(", comma(subtotal), " | ", comma(total), ")")),
                color = ifelse(porcentaje > 10, "white", "blue")), fontface = "bold", size = 3.5, vjust = 0.6) +
  scale_fill_gradient(low = "grey95", 
                      high = "grey10") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_color_manual(values = c("grey30", "white"), guide = "none") +
  labs(title = "Porcentaje de casos confirmados de Covid-19 que murieron, por comorbilidad\ny rango de edad",
       subtitle = subtitulo_mx,
       x = "",
       y = NULL,
       fill = "Porcentaje",
       caption = "\nElaborado por @segasi  para el Buró de Investigación de ADN40. Fuente: Secretaría de Salud.\n\nNotas: De acuerdo con la Secretaría de Salud, se entiende por \"casos confirmado\" el de aquella \"Persona que cumpla con la definición\noperacional de caso sospechoso y que cuente con diagnóstico confirmado por la Red Nacional de Laboratorios de Salud Pública\nreconocidos por el InDRE\". Un paciente puede tener más de una comorbilidad y cada una se registra por separado.") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 25),
        legend.position = c(0.86, -0.14), 
        legend.direction = "horizontal",
        legend.key.width = unit(1.3, "cm"),
        axis.ticks.y = element_blank()) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0))  +
  ggsave(str_c(ruta_graficas_mx, "04_10_comorbilidades_rango_de_edad_fallecieron_", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".png"), dpi = 200, width = 18, height = 12)
