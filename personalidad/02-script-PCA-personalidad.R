
library(ggcorrplot)
library(FactoMineR)
library(plotly)

frases3 <- as.vector(frases2)

# Matriz de correlaciones ----
r <- cor(df6 |> select(all_of(frases3)),
         method = "spearman")

# Gráfica

ggplotly(
  
  ggcorrplot(corr = r,
             type = "upper",
             colors = c("red", "white", "blue"),
             show.legend = F,
             tl.cex = 6) +
    
  theme(axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank())

  )



# PCA: Principal Component Analysis ----

## Dimensión: extraversion ----

### Definiendo vector con la variables
social <- frases3[c(20,23)]

### Crear la dimensión
PCA_social <- FactoMineR::PCA(
  df6 |> select(all_of(social)),
  ncp = 1
)

### Eigenvalues
PCA_social$eig


### Correlación entre las vars y la dimensión
PCA_social$var$cor


### Valores de la dimensión
PCA_social$ind$coord


### Comparación dim vs vars
tibble(
  Dim = PCA_social$ind$coord,
  df6 |> select(all_of(social))
) |> View()



## Dimensión: consumismo ----

### Definiendo vector con la variables
consumismo <- frases3[c(2,10,16)]

### Crear la dimensión
PCA_consumismo <- FactoMineR::PCA(
  df6 |> select(all_of(consumismo)),
  ncp = 1
)

### Eigenvalues
PCA_consumismo$eig


### Correlación entre las vars y la dimensión
PCA_consumismo$var$cor

### Comparación dim vs vars
tibble(
  Dim = PCA_consumismo$ind$coord,
  df6 |> select(all_of(consumismo))
) |> View()



## Dimensión: tradición ----

### Definiendo vector con la variables
tradicion <- frases3[c(9,12,14)]

### Crear la dimensión
PCA_tradicion <- FactoMineR::PCA(
  df6 |> select(all_of(tradicion)),
  ncp = 1
)

### Eigenvalues
PCA_tradicion$eig


### Correlación entre las vars y la dimensión
PCA_tradicion$var$cor



## Dimensión: intraversión ----

### Definiendo vector con la variables
intraversion <- frases3[c(21,22)]

### Crear la dimensión
PCA_intraversion <- FactoMineR::PCA(
  df6 |> select(all_of(intraversion)),
  ncp = 1
)

### Eigenvalues
PCA_intraversion$eig


### Correlación entre las vars y la dimensión
PCA_intraversion$var$cor



## Dimensión: innovación ----

### Definiendo vector con la variables
innovacion <- frases3[c(7,8,19)]

### Crear la dimensión
PCA_innovacion <- FactoMineR::PCA(
  df6 |> select(all_of(innovacion)),
  ncp = 1
)

### Eigenvalues
PCA_innovacion$eig


### Correlación entre las vars y la dimensión
PCA_innovacion$var$cor



# Data frame con las dimensiones

df10 <- df6 |> 
  mutate(
    social       = PCA_social$ind$coord *-1,
    consumismo   = PCA_consumismo$ind$coord,
    tradicion    = PCA_tradicion$ind$coord *-1,
    intraversion = PCA_intraversion$ind$coord *-1,
    innovacion   = PCA_innovacion$ind$coord *-1,
  )

dimensiones <- c(
  "social",
  "consumismo",
  "tradicion",
  "intraversion",
  "innovacion"
)

df10 |> select(all_of(dimensiones))










