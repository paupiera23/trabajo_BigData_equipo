
df_actividad_edad <- rio::import("./datos/actividad_edad.xls")

df_actividad_edad_1 <- df_actividad_edad %>% pivot_longer(cols = 2:16, names_to = "Periodos", values_to = "Actividad_edad")

rio::export(df_actividad_edad_1, "./datos/actividad_edad.csv")

df_actividad_edad_csv <- rio::import("./datos/actividad_edad.csv")

str(df_actividad_edad_csv)

library(plotly)

p1 <- ggplot(df_actividad_edad_csv, aes(Periodos, Actividad_edad, color = Grupo)) +
  geom_line() + geom_point()

ggplotly(p1)

#tenir en conter que he canviat el excel de actividad edad
