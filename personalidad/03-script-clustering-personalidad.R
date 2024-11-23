
library(NbClust)
library(FactoMineR)

# Iteración

clustering <- NbClust(
  data = df10 |> select(all_of(dimensiones)),
  distance = 'euclidean',
  method = 'ward.D2',
  index = 'dunn'
)

clustering

clustering2 <- NbClust(
  data = df10 |> select(all_of(dimensiones)),
  distance = 'euclidean',
  method = 'ward.D2',
  index = 'dunn',
  min.nc = 5,
  max.nc = 5
)

clustering2



# Tamaño de los segmentos
table(clustering2$Best.partition)
prop.table(table(clustering2$Best.partition))



# Agregar la columna con el cluster al data frame
clustering2$Best.partition

df11 <- df10 |>
  mutate(segmento = clustering2$Best.partition)


# Analizando lo segmentos

## Cruce de variables

df11 |> 
  group_by(segmento) |> 
  summarise(
    social       = mean(social),
    consumismo   = mean(consumismo),
    tradicion    = mean(tradicion),
    intraversion = mean(intraversion),
    innovacion   = mean(innovacion)
  )


df12 <- df11 |> 
  mutate_at(.vars = dimensiones,
            .funs = rescale)


df13 <- df12 |> 
  group_by(segmento) |> 
  summarise(
    social       = mean(social),
    consumismo   = mean(consumismo),
    tradicion    = mean(tradicion),
    intraversion = mean(intraversion),
    innovacion   = mean(innovacion)
  ) |> 
  column_to_rownames("segmento")


## Análisis de correspondencias
FactoMineR::CA(df13)












