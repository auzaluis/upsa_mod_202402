
library(ggcorrplot)
library(FactoMineR)
library(plotly)

frases3 <- as.vector(frases2)

# Matriz de correlaciones
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
