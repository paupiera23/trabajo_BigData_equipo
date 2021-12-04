
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
library(gifski)

df_paro <- rio::import("./datos/tasa_paro.xls")
df_natalidad <- rio::import("./datos/tasa_natalidad.xls")
df_actividad <- rio::import("./datos/tasa_actividad.xls")
df_vida <- rio::import("./datos/esperanza_vida.xlsx")
df_eman <- rio::import("./datos/edad_emancipacion.xlsx")
df_actividad_edad <- rio::import("./datos/actividad_edad.xls")
df_min_max <- rio::import("./datos/pens_min_max.xlsx")
df_tramo <- rio::import("./datos/pensiones_tramo.xls")
df_pens_med <- rio::import("./datos/pension_media.xls")
df_w_medio <- rio::import("./datos/salario_medio.xlsx")
df_smi <- rio::import("./datos/smi.xlsx")
df_alternativas <- rio::import("./datos/otras_alternativas.xlsx")

df_paro_l_csv <- rio::import("./datos/tasa_paro.csv")

rio::export(df_paro_l, "./datos/tasa_paro.csv")


str(df_paro_l_csv)

df_paro_l <- df_paro %>% pivot_longer(cols = 2:20, names_to = "Periodos", values_to = "Paro")


library(plotly)

p1 <- ggplot(df_paro_l_csv, aes(Periodos, Paro)) +
  geom_line() + geom_point()

ggplotly(p1)



df_natalidad_1 <- df_natalidad %>%
  filter(Provincias == "Total Nacional")

df_natalidad_1_l <- df_natalidad_1 %>%
  pivot_longer(cols = 2:47, names_to = "Periodos", values_to = "Tasa_Natalidad")

rio::export(df_natalidad_1_l, "./datos/tasa_natalidad.csv")

df_natalidad_csv <- rio::import("./datos/tasa_natalidad.csv")

p1 <- ggplot(df_natalidad_csv, aes(Periodos, Tasa_Natalidad)) +
  geom_line() + 
  geom_point()

ggplotly(p1)  

df_actividad_l <- df_actividad %>%
  pivot_longer(cols = 2:20, names_to = "Periodos", values_to = "Tasa_Actividad")

rio::export(df_actividad_l, "./datos/tasa_actividad.csv")

df_actividad_csv <- rio::import("./datos/tasa_actividad.csv")

p3 <- ggplot(df_actividad_csv, aes(Periodos, Tasa_Actividad)) +
  geom_line() +
  geom_point()

p3

ggplotly(p3)


df_eman_l <- df_eman %>%
  pivot_longer(cols = 2:6, names_to = "Tramo_Edad", values_to = "Porcentaje")

rio::export(df_eman_l, "./datos/edad_emancipacion.csv")

df_eman_csv <- rio::import("./datos/edad_emancipacion.csv")

df_eman_csv_1 <- df_eman_csv %>%
  group_by(Tramo_Edad)

p4 <- ggplot(df_eman_csv, aes(Periodo, color = Tramo_Edad)) +
  geom_bar()

p4


rio::export(df_min_max, "./datos/pens_min_max.csv")

df_min_max_csv <- rio::import("./datos/pens_min_max.csv")




rio::export(df_alternativas, "./datos/df_alternativas.csv")
df_alternativas <- rio::import("./datos/df_alternativas.csv")



#Vamos a arreglarlo un poco, seleccionamos solo uno de cada tipo para quitar duplicados y subcuentas de activos.
df_alternativas <- janitor::clean_names(df_alternativas) 
names(df_alternativas)
 
df_alternativas <- df_alternativas %>%
  rename(year = fecha) %>%
  rename(total = todos_los_instrumentos) %>%
  rename(oro = oro_monetario_y_deg_2) %>%
  rename(Ef_y_De = efectivo_y_depositos_2) %>%
  rename(deuda = valores_representativos_de_deuda_2) %>%
  rename(loan = prestamos_2) %>%
  rename(capital = participaciones_en_el_capital_y_en_fi_2) %>%
  rename(pensiones_priv = sistemas_de_seguros_pensiones_y_garantias_estandarizadas_2) %>%
  rename(otros = otros_activos_pasivos_2)

df_alternativas <- df_alternativas %>%
  select(year, total, oro, Ef_y_De, deuda, loan, capital, pensiones_priv, otros)

df_alternativas[, c(1:9)] <- sapply(df_alternativas[, c(1:9)], as.numeric)
#https://www.bde.es/webbde/es/estadis/infoest/temas/sb_cfesp.html



df_grafico_2 <- df_grafico %>% pivot_longer(cols = 2:8, names_to = "Activos", values_to = "Miles_de_euros")

df_grafico_2 <- df_grafico_2 %>% group_by(FECHA, Activos) 
df_grafico_2
g1 <- ggplot(df_grafico_2, aes(x = FECHA, y = Miles_de_euros, color = Activos)) + geom_line()
g1


