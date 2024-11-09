library(tidyverse)
library(plotly)
library(viridis)

glimpse(DF3)

ggplotly(
  DF3 |> 
    # contar los casos
    count(`¿Cuál es la marca que más compra?`) |> 
    # ordenar la tabla
    arrange(desc(n)) |> 
    # calcular los %
    mutate(Porcentaje = round(n/sum(n), digits = 2)) |> 
    
    ggplot(aes(
      x = `¿Cuál es la marca que más compra?`,
      y = Porcentaje,
      fill = `¿Cuál es la marca que más compra?`,
      label = n
    )) +
    geom_col() +
    # coord_flip() +
    theme_minimal() +
    theme(legend.position = "none")
)



# Prueba
DF3 |> 
  pivot_longer(cols = starts_with("Prueba"),
               names_to = "Variable",
               values_to = "Prueba") |> 
  select(Prueba) |> 
  na.omit() |> 
  count(Prueba) |> 
  mutate(Proporción = n/nrow(DF3),
         Porcentaje = scales::percent(Proporción)) |>
  ggplot(aes(x = reorder(Prueba, -Proporción),
             y = Proporción,
             fill = Prueba,
             label = Porcentaje)) +
  geom_col() +
  geom_label(fill = "white") +
  labs(title = "Prueba",
       subtitle = "¿Cuáles de las siguientes marcas ha probado alguna vez?",
       caption = "Johnnie Walker es la marca más probada (75%)") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank())



# Conocimiento

DF3 |> 
  pivot_longer(cols = starts_with("Conocimiento"),
               names_to = "Variable",
               values_to = "Conocimiento") |> 
  select(Conocimiento) |> 
  na.omit() |> 
  count(Conocimiento) |> 
  mutate(Proporción = n/nrow(DF3),
         Porcentaje = scales::percent(Proporción)) |>
  ggplot(aes(x = reorder(Conocimiento, -Proporción),
             y = Proporción,
             fill = Conocimiento,
             label = Porcentaje)) +
  geom_col() +
  geom_label(fill = "white") +
  labs(title = "Conocimiento",
       subtitle = "¿Cuáles de las siguientes marcas conoce?") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank())



# Atributos

DF3 |> 
  pivot_longer(cols = starts_with("Atributo"),
               names_to = "Variable",
               values_to = "Atributo") |> 
  select(Atributo) |> 
  na.omit() |> 
  count(Atributo) |> 
  mutate(Proporción = n/nrow(DF3),
         Porcentaje = scales::percent(Proporción)) |>
  ggplot(aes(x = reorder(Atributo, -Proporción, decreasing = T),
             y = Proporción,
             label = Porcentaje)) +
  geom_col(fill = "#90e0ef") +
  geom_label(fill = "white") +
  coord_flip() +
  labs(title = "Atributo",
       subtitle = "¿Cuáles de los siguientes atributos valora más?") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank())


# Posicionamiento
DF4 <- as_tibble(DF3)

DF4 <- DF4 |> 
  select(`Precio razonable`:`Variedad de maduración (años)`) |> 
  pivot_longer(cols = everything(),
               names_to = "atributo",
               values_to = "marca")

table(DF4)

prop.table(table(DF4))
prop.table(table(DF4), margin = 1)
prop.table(table(DF4), margin = 2)


## Análisis de correspondencias
library(FactoMineR)
FactoMineR::CA(table(DF4))











