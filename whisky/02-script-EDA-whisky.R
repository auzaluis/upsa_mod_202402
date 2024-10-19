
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


















