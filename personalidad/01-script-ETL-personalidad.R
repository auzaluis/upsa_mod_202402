
# Tema 01: Carga de datos ----

## Carga de local ----
df <- read.csv(file = "personalidad/Personalidad y uso de apps.csv",
               check.names = FALSE)

colnames(df)

## Carga en línea ----
install.packages("gsheet")
library(gsheet)

url_google <- "https://docs.google.com/spreadsheets/d/1IQ_RxxTSmBKHTExlxboIRNlMov_F6RyqdcOPrflCv_w/edit?usp=sharing"

df <- read.csv(text = gsheet2text(url = url_google),
               check.names = FALSE)

## Estructura del data frame ----
class(df)
class(df$Sexo)
class(df$`Escribe tu edad exacta`)
nrow(df)
ncol(df)

# Tema 02: Transformación de datos ----

## Valores perdidos ----
# Los valores perdidos (NA) pueden ser tratados de 2 maneras:
# 1. Imputar (reemplazo)
# 2. Omitir / Eliminar

df$`Escribe tu edad exacta`
is.na(df$`Escribe tu edad exacta`) # Prueba lógica
summary(is.na(df$`Escribe tu edad exacta`))

df$`Escribe tu edad exacta` |> 
  is.na() |> 
  summary()


### Imputación | Reemplazo por el promedio ----
mean(df$`Escribe tu edad exacta`, na.rm = TRUE)

ifelse(test = is.na(df$`Escribe tu edad exacta`),
       yes = mean(df$`Escribe tu edad exacta`, na.rm = TRUE),
       no = df$`Escribe tu edad exacta`)




