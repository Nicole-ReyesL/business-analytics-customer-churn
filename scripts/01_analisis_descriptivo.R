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

resumen_edad <- datos %>%
  group_by(Churn) %>%
  summarise(
    Clientes = n(),
    Promedio = mean(Edad),
    Mediana = median(Edad),
    Minimo = min(Edad),
    Maximo = max(Edad)
  )
print(resumen_edad)

ggplot(datos, aes(x = factor(Churn), y = Edad)) +
  geom_boxplot(fill = "steelblue") +
  labs(
    title = "Antiguedad de clientes segun abandono",
    x = "Churn (0 = Permanece, 1 = Abandona)",
    y = "Antiguedad del cliente (meses)"
  ) +
  theme_minimal()

resumen_comportamiento <- datos %>%
  group_by(Churn) %>%
  summarise(
    Clientes = n(),
    CHI_Promedio = mean(CHI),
    Soporte_Promedio = mean(Soporte),
    Dias_Sin_Login_Promedio = mean(Dias_Sin_Login)
  )

print(resumen_comportamiento)

ggplot(datos, aes(x = factor(Churn), y = CHI)) +
  geom_boxplot(fill = "steelblue") +
  labs(
    title = "Indicador CHI segun abandono",
    x = "Churn (0 = Permanece, 1 = Abandona)",
    y = "CHI"
  ) +
  theme_minimal()

ggplot(datos, aes(x = factor(Churn), y = Soporte)) +
  geom_boxplot(fill = "steelblue") +
  labs(
    title = "Casos de soporte segun abandono",
    x = "Churn (0 = Permanece, 1 = Abandona)",
    y = "Casos de soporte"
  ) +
  theme_minimal()

ggplot(datos, aes(x = factor(Churn), y = Dias_Sin_Login)) +
  geom_boxplot(fill = "steelblue") +
  labs(
    title = "Dias sin iniciar sesion segun abandono",
    x = "Churn (0 = Permanece, 1 = Abandona)",
    y = "Dias sin iniciar sesion"
  ) +
  theme_minimal()

summary(datos$Dias_Sin_Login)

sum(datos$Dias_Sin_Login < 0)

resumen_comportamiento %>%
  select(Churn, Dias_Sin_Login_Promedio)

datos %>%
  group_by(Churn) %>%
  summarise(
    Total_Clientes = n(),
    Dias_Negativos = sum(Dias_Sin_Login < 0),
    Porcentaje_Negativos = mean(Dias_Sin_Login < 0) * 100,
    .groups = "drop"
  )

datos %>%
  group_by(Churn) %>%
  summarise(
    Minimo = min(Dias_Sin_Login),
    Mediana = median(Dias_Sin_Login),
    Maximo = max(Dias_Sin_Login),
    .groups = "drop"
  )

# --------------------------------------------------
# OBSERVACIONES DEL ANALISIS DESCRIPTIVO
# --------------------------------------------------

# La base contiene 6347 registros de clientes.
# Se identificaron 1510 valores negativos en
# la variable Dias_Sin_Login.
#
# La hoja Title Page del archivo original no
# proporciona una explicacion de estos valores.
#
# Por esta razon, los resultados asociados con
# esta variable deben interpretarse con precaucion.
# No se modificaron los valores originales.