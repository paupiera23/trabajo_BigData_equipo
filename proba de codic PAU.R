
#Habría que hacer un install.packages(" ") con cada uno si no los tuvieramos ya en nuestra biblioteca.
library(tidyverse)
library(klippy)  #- remotes::install_github("rlesur/klippy")
library(knitr)
library(readxl)
library(reactable)
library(ggplot2)
library(stringi)
library(ggThemeAssist)
library(plotly)
library(patchwork)
library(ggrepel)
library(sf)
library(crosstalk)
library(gganimate)
library(hrbrthemes)
library(sf)
library(RColorBrewer)
library(rio)
library(gganimate)
library(plotly)

#df_paro <- rio::import("./datos/tasa_paro.xls")
#df_natalidad <- rio::import("./datos/tasa_natalidad.xls")
#df_actividad <- rio::import("./datos/tasa_actividad.xls")
#df_vida <- rio::import("./datos/esperanza_vida.csv")
df_eman <- rio::import("./datos/edad_emancipacion.xlsx")
#df_actividad_edad <- rio::import("./datos/actividad_edad.xls")
#df_min_max <- rio::import("./datos/pens_min_max.xlsx")
#df_tramo <- rio::import("./datos/pensiones_tramo.xls")
#df_pens_med <- rio::import("./datos/pension_media.xls")
#df_w_medio <- rio::import("./datos/salario_medio.xlsx")
#df_smi <- rio::import("./datos/smi.xlsx")
df_paro_sector <- rio::import("./datos/3972bsc.csv")
str(df_paro_sector)


#- Cargamos el df del paro que anteriormente hemos descargado del INE

df_paro <- rio::import("./datos/tasa_paro.csv")


df_paro <- df_paro %>%
  select(Paro, Periodos)

#Ahora cargamos los datos del df de la tasa de actividad nacional para poder compararlos con la tasa de paro nacional.

df_actividad <- rio::import("./datos/tasa_actividad.csv")

df_actividad <- df_actividad %>%
  select(Periodos, Tasa_Actividad)


#- Vamos a juntar el df de la tasa de actividad y la tasa de paro para poder hacer un gráfico donde se representen ambas tasas.

df_union <- inner_join(df_actividad, df_paro)


p4 <- ggplot(df_union) +
  geom_bar(aes(Periodos, Tasa_Actividad), stat = "identity", fill = "grey") +
  geom_line(aes(Periodos, Paro), size = 1) +
  geom_point(aes(Periodos, Paro), size = 2) +
  labs(title = "Year: {frame_time}",
       subtitle = "Datos para el periodo 2002 - 2020",
       caption = "Datos provenientes del INEBase",
       y = "Tasa de Paro y Actividad") + theme(plot.subtitle = element_text(size = 10),
    plot.title = element_text(size = 15),
    plot.background = element_rect(fill = "azure4")) +labs(title = "Comparación Tasa de Paro y de Actividad")


ggplotly(p4)

#Este gráfico ya lo tenemos arreglado para su posterior analisis


p3 <- ggplot(df_actividad, aes(Periodos, Tasa_Actividad)) +
  geom_line() +
  geom_point()+
  transition_reveal(Periodos)

p1 <- ggplot(df_paro, aes(Periodos, Paro)) +
  geom_line() + geom_point()

ggplotly(p1)




#Juntar gráfico del paro con la tasa de actividad.

df_natalidad_1 <- df_natalidad %>%
  filter(Provincias == "Total Nacional")

df_natalidad_1_l <- df_natalidad_1 %>%
  pivot_longer(cols = 2:47, names_to = "Periodos", values_to = "Tasa_Natalidad")

rio::export(df_natalidad_1_l, "./datos/tasa_natalidad.csv")


#Vamos a comparar la evolución de la tasa de natalidad y del indice de envejecimiento para tener conocimiento de la evolución que han tenido estas variables demográficas.

df_natalidad <- rio::import("./datos/tasa_natalidad.csv")

df_indice <- rio::import("./datos/indice_envejecimiento.csv")

df_indice <- df_indice %>%
  filter(Periodo != 2021) %>%
  rename(Periodos = "Periodo")

df_indice$Total <- as.numeric(df_indice$Total)

df_union_2 <- inner_join(df_natalidad, df_indice)

df_union_2 <- df_union_2 %>%
  select(Periodos, Tasa_Natalidad, Total)


str(df_union_2)

p2 <- ggplot(df_union_2) +
  geom_line(aes(Periodos, Tasa_Natalidad)) + 
  geom_point(aes(Periodos, Tasa_Natalidad)) +
  geom_line(aes(Periodos, Total))+
  geom_point(aes(Periodos, Total))+
  scale_y_continuous(name = "Indice de envejecimiento", 
                     sec.axis = sec_axis(~./5, name = "Tasa Natalidad", 
                                         labels = function(b) { paste0(round(b * 4, 0), "%")})) + theme(plot.subtitle = element_text(colour = "gray9"),
    axis.title = element_text(size = 10),
    panel.background = element_rect(colour = "black",
        size = 2, linetype = "solid"), plot.background = element_rect(fill = "gray50",
        colour = "black", size = 2, linetype = "solid")) +labs(title = "Comparación Tasa de atalidad e Índice de Envejecimiento",
    subtitle = "Datos extraidos del INEBase")


ggplotly(p2)  





#- Vamos a juntar el df de la tasa de actividad y la tasa de paro para poder hacer un gráfico donde se representen ambas tasas.

df_union <- inner_join(df_actividad, df_paro)

p4 <- ggplot(df_union) +
  geom_line(aes(Periodos, Tasa_Actividad)) +
  geom_line(aes(Periodos, Paro)) +
  scale_y_continuous(name = "Tasa de Paro", 
                     sec.axis = sec_axis(~./5, name = "Tasa Actividad", 
                                         labels = function(b) { paste0(round(b * 100, 0), "%")}))




#este grafic esta be


df_total <- df_total %>%
  select(Periodo, Total)



df_piramide_g <- df_piramide %>%
  filter(`Año` >= 2002)

df_piramide_w <- df_piramide_g %>%
  pivot_wider(names_from = Sexo, values_from = Total)

df_piramide_w <- df_piramide_w %>%
  group_by(`Año`) %>%
  mutate(total = Hombres + Mujeres)



df_piramide_w <- df_piramide_w[248:380, ]

df_piramide_w <- df_piramide_w %>%
  select(`Año`, total) %>%
  rename(Periodo = `Año`) %>%
  group_by(Periodo)

df_piramide_w <- df_piramide_w %>%
  arrange(Periodo)
  

df_piramide_w <- df_piramide_w %>%
  pivot_wider(names_from = Periodo, values_from = total)
  
str(df_piramide_w)
  

df_piramide_w = colSums(select(df_piramide_w, contains(2020)))


df_total <- rio::import("./datos/poblacion_total.csv")


  

p5 <- ggplot() +
  geom_bar(df_piramide_w, aes(Periodos, total), stat = "identity", fill = "grey")

#Ya tenemos el total de gente mayor de 65 años

ggplot() + 
  geom_bar(mapping = aes(x = dt$when, y = dt$numinter), stat = "identity", fill = "grey") +
  geom_line(mapping = aes(x = dt$when, y = dt$prod*5), size = 2, color = "blue") + 
  scale_x_date(name = "Day", labels = NULL) +
  scale_y_continuous(name = "Interruptions/day", 
                     sec.axis = sec_axis(~./5, name = "Productivity % of best", 
                                         labels = function(b) { paste0(round(b * 100, 0), "%")})) + 
  theme(
    axis.title.y = element_text(color = "grey"),
    axis.title.y.right = element_text(color = "blue"))



#Vamos a cargar los datos referentes a la edad de emancipacion de los jovenes en España

df_eman <- rio::import("./datos/edad_emancipacion.csv")

df_eman_1 <- df_eman %>%
  filter(Periodo == 2018)

df_eman_2 <- df_eman %>%
  filter(Periodo == 2008)

library(ggplot2)

p4 <- ggplot(df_eman_1) +
 aes(x = Tramo_Edad, fill = Tramo_Edad, weight = Porcentaje) +
 geom_bar(position = "dodge") +
 scale_fill_viridis_d(option = "viridis", direction = 1) +
 scale_y_continuous(trans = "sqrt") +
 theme_gray()

p5 <- ggplot(df_eman_2) +
  aes(x = Tramo_Edad, fill = Tramo_Edad, weight = Porcentaje) +
  geom_bar(position = "dodge") +
  scale_fill_viridis_d(option = "viridis", direction = 1) +
  scale_y_continuous(trans = "sqrt") +
  theme_gray()


p6 <- p4 + p5 

#Este gráfico no termina de estar bien del todo per MESSIRVE

#- Vamos a arreglar los datos de la pension minima, máxima y media que encontramos en los siguientes df.


df_min_max<- rio::import("./datos/pens_min_max.csv")

df_pens_med <- rio::import("./datos/pension_media.csv")

janitor::clean_names(df_min_max)
names(df_min_max)

df_min_max <- df_min_max %>%
  rename(pension_max = "PensiÃ³n mÃ¡xima") %>%
  rename(pension_min = "PensiÃ³n mÃ­nima") %>%
  rename(periodo = "SERIE")

df_smi <- rio::import("./datos/smi.csv")

janitor::clean_names(df_smi)
names(df_smi)


df_smi <- df_smi %>%
  rename(periodo = "AÃ±o")

#DATOS ARREGLATS!!!



df_vida <- rio::import("./datos/esperanza_vida.csv")

df_vida <- df_vida %>%
  select("geo", "TIME_PERIOD", "OBS_VALUE") %>%
  rename(pais = "geo", periodo = "TIME_PERIOD", esperanza_vida = "OBS_VALUE")

#nos quedamos con los datos de españa

df_vida <- df_vida %>%
  filter(pais == "ES")


#DATOS ARREGLAATS

#Importamos los datos sobre el salario medio anual de españa

df_w_med <- rio::import("./datos/salario_medio.csv")

str(df_w_med)

#Nos damos cuenta que el salario medio no esta en formato numerico por lo tanto vamos a pasarlo a formato numérico

df_w_med$w_med_anual <- as.numeric(df_w_med$w_med_anual)

str(df_w_med)

#DATOS ARREGLADOSSSS!!!!!


df_tramo_l <- df_tramo %>%
  pivot_longer(cols = 2:118, names_to = "tramo", values_to = "pensionistas")

rio::export(df_tramo_l, "./datos/pensiones_tramo.csv")

df_tramo <- rio::import("./datos/pensiones_tramo.csv")

#ARREGLO DATOS ACT EDAD

df_actividad_edad <- rio::import("./datos/actividad_edad.xls")

df_actividad_edad_1 <- df_actividad_edad %>% pivot_longer(cols = 2:16, names_to = "Periodos", values_to = "Actividad_edad")

rio::export(df_actividad_edad_1, "./datos/actividad_edad.csv")

#FINS ASI ARREGLO DATOS

df_actividad_ed <- rio::import("./datos/actividad_edad.csv")

str(df_actividad_ed)

#ARREGLO DATOS FONDO DE RESERVA PENSIONES

df_fondo_reserva <- rio::import("./datos/fondo_reserva.xlsx")

df_fondo_reserva_l <- df_fondo_reserva %>%
  pivot_longer(cols = 2:21, names_to = "periodo", values_to = "millones_euros")

rio::export(df_fondo_reserva_l, "./datos/fondo_reserva.csv")

#FINS ASI ARREGLO DE DATOS FONDO DE RESERVA PENSIONES

df_fondo <- rio::import("./datos/fondo_reserva.csv")

df_fondo$millones_euros <- as.numeric(df_fondo$millones_euros)

str(df_fondo)

#ARREGLAT DEL TOT

df_piramide <- rio::import("./datos/piramide_pob.csv")

str(df_piramide)

df_piramide$Total <- as.numeric(df_piramide$Total)

df_piramide_20 <- df_piramide %>%
  filter(`Año` == 2020) %>%
  arrange(`Año`)


ggplot(df_piramide, aes(x = `Edad (grupos quinquenales)`,
                y = `Total`,
                fill = Sexo)) +
  geom_col(data = subset(df_piramide, Sexo == "Hombres") %>% 
             mutate(`Total` = - `Total`),
           width = 0.5, fill = "blue") +
  geom_col(data = subset(df_piramide, Sexo == "Mujeres"),
           width = 0.5, fill = "pink") + 
  coord_flip()


df_proyeccion <- rio::import("./datos/proyeccion_2070.csv")

str(df_proyeccion)


df_proyeccion$total_red <- as.numeric(df_proyeccion$total_red)

df_proyeccion_50 <- df_proyeccion %>%
  filter(Periodo == 2050)

ggplot(df_proyeccion_50, aes(x = `Edad`,
                        y = `total_red`,
                        fill = Sexo)) +
  geom_col(data = subset(df_proyeccion_50, Sexo == "Hombres") %>% 
             mutate(`total_red` = - `total_red`),
           width = 0.5, fill = "blue") +
  geom_col(data = subset(df_proyeccion_70, Sexo == "Mujeres"),
           width = 0.5, fill = "pink") + 
  coord_flip()




#janitor::clean_names(df)  esta funcion arregla los nombres de las variables, luego podemos hacer un rename para arreglarlo.


#- pasar a fecha coses q estan per trimestre: mutate(fecha = lubridate::yq(nom de la variable que te la fecha))













df_act <- rio::import("./datos/5360bsc.csv")
str(df_act)

df_act_1 <- df_act %>%
  filter(Edad == "Total")
library(ggplot2)

#-- codic per a poder insertar un video de youtube en el html

library(vembedr)
embed_url("https://www.youtube.com/watch?v=Iaj8yEpuvgg")



library(tidyverse)
library(klippy)  
library(knitr)
library(tibble)
library(ggthemes)
library(ggplot2)
library(dplyr)
library(patchwork)
library(ggrepel)
library(sf)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(pjpv2020.01)
library(plotly)
library(gganimate)
library (gifski)
library(gt)
library(kableExtra)
library(DT)
library(rio)
library(leaflet) 
library(leafem)
library(formattable)
library(lubridate)
library(echarts4r)