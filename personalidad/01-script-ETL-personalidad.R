
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


install.packages("tidyverse")
library(tidyverse)

df2 <- df |> 
  mutate(edad2 = ifelse(test = is.na(`Escribe tu edad exacta`),
                        yes = mean(`Escribe tu edad exacta`, na.rm = T),
                        no = `Escribe tu edad exacta`)) |> 
  relocate(edad2, .after = `Escribe tu edad exacta`)


### Eliminar | Toda la fila ----
df2 <- df2 |> na.omit()



### Estandarización de variables ----

#### Normalización ----
scale(df2$`Escribe tu edad exacta`)
mean(df2$`Escribe tu edad exacta`)

data.frame(
  original = df2$`Escribe tu edad exacta`,
  normalizada = scale(df2$`Escribe tu edad exacta`)
)


df3 <- df2 |> 
  mutate(edadZ = scale(`Escribe tu edad exacta`)) |> 
  relocate(edadZ, .after = edad2)


#### Rango ----
library(scales)

df3 <- df3 |> 
  mutate(edadR = rescale(`Escribe tu edad exacta`)) |> 
  relocate(edadR, .after = edadZ)



## Agrupaciones ----

### Rangos numéricos ----
df4 <- df3 |> 
  mutate(edadGR = cut(`Escribe tu edad exacta`,
                      breaks = c(-Inf, 18, 21, Inf),
                      labels = c("18 o menos", "19 a 21", "Más de 21"))) |> 
  relocate(edadGR, .after = edadR)



### Categorías ----

unique(df4$`Según tu forma de ser ¿Cuál de las siguientes frases te describe mejor: [No discrimino y trato a todos por igual]`)
unique(df4[,9])



ifelse(
  test = df4[,9] == "Un poco verdadero" |
    df4[,9] == "Totalmente verdadero",
  yes = 1,
  no = 0
)



# Bucles

## Paso 1: Crear un vector con los nombres de las columnas
frases <- df4 |> select(starts_with("Según")) |> colnames()


## Paso: Ejecutar el bucle

df5 <- df4

for (frase in frases) {
  
  df5[,frase] <- ifelse(
    test = df4[,frase] == "Un poco verdadero" |
      df4[,frase] == "Totalmente verdadero",
    yes = 1,
    no = 0
  )
  
}



# Tema 03: Manipulación de datos ----

# Convirtiendo el df en un tibble
df5 <- as_tibble(df5)

## Selección de columas ----
df5 |> select(Sexo)
df5 |> select(Sexo, `Escribe tu edad exacta`)
df5 |> select(-`Marca temporal`)
df5 |> select(starts_with('edad'))
df5 |> select(ends_with('00:00'))
df5 |> select(contains('edad'))



## Filtrado de filas ----
df5 |>
  select(Sexo) |>
  filter(Sexo == 'Mujer')

df5 |> 
  select(Sexo) |> 
  filter(Sexo != "Mujer")

df5 |> 
  select(Sexo, `Escribe tu edad exacta`) |> 
  filter(`Escribe tu edad exacta` > 21)

df5 |> 
  select(Sexo, `Escribe tu edad exacta`) |> 
  filter(`Escribe tu edad exacta` <= 21)

df5 |> 
  select(Sexo, `Escribe tu edad exacta`) |> 
  filter(between(`Escribe tu edad exacta`, 18, 21))

df5 |> 
  select(Sexo, `Escribe tu edad exacta`) |> 
  filter(`Escribe tu edad exacta` %in% 18:21)

df5 |> 
  select(Sexo, `Escribe tu edad exacta`) |> 
  filter(`Escribe tu edad exacta` >= 18 &
           `Escribe tu edad exacta` <= 21)

df5 |> 
  select(Sexo, `Escribe tu edad exacta`) |> 
  filter(`Escribe tu edad exacta` >= 18,
         `Escribe tu edad exacta` <= 21)

df5 |> 
  select(Sexo, `Escribe tu edad exacta`) |> 
  filter(`Escribe tu edad exacta` >= 18,
         `Escribe tu edad exacta` <= 21,
         Sexo == "Mujer")



## Nombre de  columnas----

df6 <- df5

### APPS

#### Paso 1: Crear un vector con los nuevos nombres
apps <- c("TikTok", "Instagram", "Facebook", "YouTube")

#### Paso 2: Reemplazo
colnames(df6)[34:37] <- apps



### Frases

#### Paso 1: Crear un vector con los nuevos nombres
frases2 <- c(
  "No discrimino y trato a todos por igual",
  "Me gusta comprar marcas que representen mi status",
  "Me preocupa mucho el medio ambiente",
  "Estudio mucho, doy mi mayor esfuerzo",
  "Busco el éxito sin importar lo que deba sacrificar",                           
  "Trato de vestir sencillo para no presumir",
  "Creo que la vida se trata de tomar riesgos",
  "Busco hacer cosas emocionantes para no aburrirme",
  "En mi casa, la familia es muy importante, por eso paso mucho tiempo con ellos",
  "Tener dinero es clave para ser respetado",
  "Me cuesta mucho lidiar con gente que opina estupideces",
  "La tradición y religión ayudan a distinguir lo bueno de lo malo",
  "El dinero va y viene, trato de no cuidarlo mucho, más bien lo disfruto",
  "Hay que respetar a los adultos, aunque algunos no sean muy respetuosos",
  "Me gusta comer sano y estar en forma",
  "Vestirse bien es clave para el éxito",
  "Soy sincero con las personas, aunque la verdad les duela",
  "Me preocupo y ayudo a los necesitados",
  "Invierto mucho tiempo y dinero para aprender cosas nuevas",
  "Me gusta conocer gente y hacer nuevos amigos",
  "Soy una persona casera, prefiero estar en casa haciendo lo mío",
  "Prefiero hacer las cosas solo, me estresa trabajar en grupo",
  "Me gusta tener muchos amigos y pasar tiempo con ellos",
  "Me cuesta concentrarme porque me aburro rápido de las cosas"
  )


#### Paso 2: Reemplazo
colnames(df6)[9:32] <- frases2



## Pivot ----

### Pivot Longer

df7 <- df6 |> 
  pivot_longer(
    cols = all_of(apps),
    names_to = "app",
    values_to = "time"
  )


df8 <- df7 |> 
  pivot_wider(names_from = app,
              values_from = time)






