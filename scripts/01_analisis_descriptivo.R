library(readxl)
library(dplyr)
library(ggplot2)

datos <- read_excel(
  "data/DATA.xlsx",
  sheet = "Case Data"
)

dim(datos)
head(datos)
names(datos)
str(datos)
summary(datos)

names(datos)

colSums(is.na(datos))

str(datos)

head(datos, 10)


names(datos) <- c(
  "ID",
  "Edad",
  "Churn",
  "CHI",
  "Cambio_CHI",
  "Soporte",
  "Cambio_Soporte",
  "Prioridad",
  "Cambio_Prioridad",
  "Logins",
  "Blogs",
  "Vistas",
  "Dias_Sin_Login"
)

names(datos)
dim(datos)
summary(datos)

total_clientes <- nrow(datos)
total_abandono <- sum(datos$Churn == 1)
total_permanecen <- sum(datos$Churn == 0)
tasa_abandono <- mean(datos$Churn) * 100
total_clientes
total_abandono
total_permanecen
tasa_abandono
table(datos$Churn)
prop.table(table(datos$Churn)) * 100

tabla_churn <- datos %>%
  group_by(Churn) %>%
  summarise(
    Clientes = n(),
    .groups = "drop"
  )

print(tabla_churn)

ggplot(tabla_churn,
       aes(x = factor(Churn), y = Clientes)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Distribucion del abandono de clientes",
    x = "Churn (0 = Permanece, 1 = Abandona)",
    y = "Numero de clientes"
  ) +
  theme_minimal()

