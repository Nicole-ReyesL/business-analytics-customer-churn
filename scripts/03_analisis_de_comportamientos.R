
# ==========================================
# 1. VERIFICACION DE VARIABLES
# ==========================================

install.packages(c("readxl", "dplyr", "ggplot2"))
library(readxl)
library(dplyr)
library(ggplot2)
datos <- read_excel(
  "DATA.xlsx",
  sheet = "Case Data"
)
summary(datos)
list.files(recursive = TRUE, pattern = "\\.xlsx$", ignore.case = TRUE)

# 2. VERIFICACION DE ESTRUCTURA DE LOS DATOS

dim(datos)

head(datos)

str(datos)

names(datos)


# 3. ANALISIS DE ABANDONO DE CLIENTES

# Frecuencia de abandono
table(datos$`Churn (1 = Yes, 0 = No)`)

# Porcentaje de abandono
prop.table(
  table(datos$`Churn (1 = Yes, 0 = No)`)
) * 100

# Grafico de abandono
ggplot(datos, aes(
  x = factor(`Churn (1 = Yes, 0 = No)`)
)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Distribucion del abandono de clientes",
    x = "Churn (0 = No, 1 = Si)",
    y = "Cantidad de clientes"
  ) +
  theme_minimal()


# 4. ANALISIS DE COMPORTAMIENTO DE CLIENTES

# Edad de los clientes
summary(datos$`Customer Age (in months)`)

# Casos de soporte
summary(datos$`Support Cases Month 0`)

# Inicios de sesion
summary(datos$`Logins 0-1`)

# Visitas a la plataforma
summary(datos$`Views 0-1`)

# Articulos del blog
summary(datos$`Blog Articles 0-1`)

# Dias desde el ultimo inicio de sesion
summary(datos$`Days Since Last Login 0-1`)


# 5. RELACION ENTRE COMPORTAMIENTO Y CHURN

# Promedio de variables segun abandono

datos %>%
  group_by(`Churn (1 = Yes, 0 = No)`) %>%
  summarise(
    Edad_promedio = mean(`Customer Age (in months)`, na.rm = TRUE),
    Casos_soporte = mean(`Support Cases Month 0`, na.rm = TRUE),
    Logins = mean(`Logins 0-1`, na.rm = TRUE),
    Visitas = mean(`Views 0-1`, na.rm = TRUE),
    Dias_sin_login = mean(`Days Since Last Login 0-1`, na.rm = TRUE)
  )


# 6. COMPARACION DE INICIOS DE SESION

datos %>%
  group_by(`Churn (1 = Yes, 0 = No)`) %>%
  summarise(
    Logins_promedio = mean(`Logins 0-1`, na.rm = TRUE)
  ) %>%
  ggplot(aes(
    x = factor(`Churn (1 = Yes, 0 = No)`),
    y = Logins_promedio
  )) +
  geom_col(fill = "purple") +
  labs(
    title = "Promedio de inicios de sesion segun abandono",
    x = "Churn (0 = No, 1 = Si)",
    y = "Promedio de inicios de sesion"
  ) +
  theme_minimal()


# 7. ANALISIS DE VISITAS A LA PLATAFORMA

datos %>%
  group_by(`Churn (1 = Yes, 0 = No)`) %>%
  summarise(
    Visitas_promedio = mean(`Views 0-1`, na.rm = TRUE)
  ) %>%
  ggplot(aes(
    x = factor(`Churn (1 = Yes, 0 = No)`),
    y = Visitas_promedio
  )) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Visitas a la plataforma segun abandono",
    x = "Churn (0 = No, 1 = Si)",
    y = "Promedio de visitas"
  ) +
  theme_minimal()


# VERIFICACION DE VISITAS

summary(datos$`Views 0-1`)

# Valores por grupo de abandono
datos %>%
  group_by(`Churn (1 = Yes, 0 = No)`) %>%
  summarise(
    Minimo = min(`Views 0-1`, na.rm = TRUE),
    Maximo = max(`Views 0-1`, na.rm = TRUE),
    Promedio = mean(`Views 0-1`, na.rm = TRUE)
  )


# REVISION DE VALORES EXTREMOS

sum(datos$`Views 0-1` < 0, na.rm = TRUE)

sum(datos$`Views 0-1` > 100, na.rm = TRUE)

head(sort(datos$`Views 0-1`), 10)

head(sort(datos$`Views 0-1`, decreasing = TRUE), 10)


# IDENTIFICAR REGISTROS CON VALORES EXTREMOS

datos %>%
  filter(`Views 0-1` < 0 | `Views 0-1` > 100) %>%
  select(
    ID,
    `Churn (1 = Yes, 0 = No)`,
    `Views 0-1`
  ) %>%
  head(20)


# 8. ANALISIS DE DISTRIBUCION DE VISITAS

summary(datos$`Views 0-1`)

# Distribucion por abandono
datos %>%
  group_by(`Churn (1 = Yes, 0 = No)`) %>%
  summarise(
    Total_clientes = n(),
    Visitas_negativas = sum(`Views 0-1` < 0, na.rm = TRUE),
    Visitas_positivas = sum(`Views 0-1` >= 0, na.rm = TRUE)
  )


# 9. REVISION DE VARIABLES RELACIONADAS

summary(datos$`Views 0-1`)
summary(datos$`Logins 0-1`)
summary(datos$`Blog Articles 0-1`)

# Correlacion entre actividad
cor(
  datos[, c("Views 0-1", "Logins 0-1",
            "Blog Articles 0-1")],
  use = "complete.obs"
)


# 10. CORRELACION ENTRE ACTIVIDAD Y ABANDONO

cor(
  datos[, c("Churn (1 = Yes, 0 = No)",
            "Views 0-1",
            "Logins 0-1",
            "Blog Articles 0-1")],
  use = "complete.obs"
)

# 10. CORRELACION ENTRE ACTIVIDAD Y ABANDONO

cor(
  datos[, c("Churn (1 = Yes, 0 = No)",
            "Views 0-1",
            "Logins 0-1",
            "Blog Articles 0-1")],
  use = "complete.obs"
)


# ==================================================
# 11. MODELO PREDICTIVO DE ABANDONO DE CLIENTES
# ==================================================

# Preparar los datos

set.seed(123)

datos_modelo <- datos[, c(
  "Churn (1 = Yes, 0 = No)",
  "Customer Age (in months)",
  "CHI Score Month 0",
  "CHI Score 0-1",
  "Support Cases Month 0",
  "Support Cases 0-1",
  "SP Month 0",
  "SP 0-1",
  "Logins 0-1",
  "Blog Articles 0-1",
  "Views 0-1",
  "Days Since Last Login 0-1"
)]

# Eliminar registros incompletos
datos_modelo <- na.omit(datos_modelo)

# Dividir datos: 80% entrenamiento y 20% prueba

indices <- sample(
  seq_len(nrow(datos_modelo)),
  size = floor(0.8 * nrow(datos_modelo))
)

entrenamiento <- datos_modelo[indices, ]
prueba <- datos_modelo[-indices, ]

# Construir modelo de regresion logistica

modelo_churn <- glm(
  `Churn (1 = Yes, 0 = No)` ~ .,
  data = entrenamiento,
  family = binomial
)

# Mostrar resultados del modelo

summary(modelo_churn)


# 11. EVALUACION DEL MODELO

# Predecir probabilidades
predicciones <- predict(
  modelo_churn,
  newdata = prueba,
  type = "response"
)

# Clasificar clientes
clases_predichas <- ifelse(
  predicciones >= 0.5, 1, 0
)

# Matriz de confusion
matriz <- table(
  Real = prueba$`Churn (1 = Yes, 0 = No)`,
  Predicho = clases_predichas
)

print(matriz)

# Precision general
precision <- sum(diag(matriz)) / sum(matriz)

print(precision)


# 12. REVISION DE PROBABILIDADES

summary(predicciones)

# Distribucion de probabilidades
hist(
  predicciones,
  main = "Probabilidades de abandono",
  xlab = "Probabilidad de Churn",
  col = "steelblue"
)


# 13. OPTIMIZACION DEL UMBRAL

real <- as.numeric(as.character(
  prueba$`Churn (1 = Yes, 0 = No)`
))

umbrales <- c(0.02, 0.03, 0.04, 0.05, 0.07, 0.10)

resultados <- data.frame()

for (u in umbrales) {
  
  pred <- ifelse(predicciones >= u, 1, 0)
  
  TP <- sum(pred == 1 & real == 1)
  TN <- sum(pred == 0 & real == 0)
  FP <- sum(pred == 1 & real == 0)
  FN <- sum(pred == 0 & real == 1)
  
  sensibilidad <- TP / (TP + FN)
  precision <- ifelse(TP + FP == 0, 0, TP / (TP + FP))
  
  resultados <- rbind(
    resultados,
    data.frame(
      Umbral = u,
      Sensibilidad = sensibilidad,
      Precision = precision,
      Accuracy = (TP + TN) / length(real)
    )
  )
}

print(resultados)


# 14. CALCULO DEL F1-SCORE

resultados$F1 <- with(
  resultados,
  2 * (Precision * Sensibilidad) /
    (Precision + Sensibilidad)
)

print(resultados)

# Identificar el mejor umbral segun F1
mejor <- resultados[
  which.max(resultados$F1),
]

print(mejor)



# 15. GRAFICO DEL F1-SCORE

ggplot(resultados, aes(x = Umbral, y = F1)) +
  geom_line(color = "purple", linewidth = 1) +
  geom_point(color = "steelblue", size = 3) +
  labs(
    title = "F1-score segun el umbral",
    x = "Umbral de clasificacion",
    y = "F1-score"
  ) +
  theme_minimal()

# 16. INTERPRETACION DE RESULTADOS

cat("====================================\n")
cat("RESULTADOS DEL MODELO CUSTOMER CHURN\n")
cat("====================================\n")

cat("Mejor umbral:", mejor$Umbral, "\n")
cat("Sensibilidad:", mejor$Sensibilidad, "\n")
cat("Precision:", mejor$Precision, "\n")
cat("Accuracy:", mejor$Accuracy, "\n")
cat("F1-score:", mejor$F1, "\n")


# 17. CONCLUSIONES DEL ANALISIS

cat("\n====================================\n")
cat("CONCLUSIONES DEL ANALISIS CUSTOMER CHURN\n")
cat("====================================\n")

cat("1. El abandono de clientes representa un reto para la empresa.\n")

cat("2. El modelo de regresion logistica obtuvo un Accuracy de 88.98%.\n")

cat("3. La sensibilidad fue de 25.45%, indicando que el modelo identifica una parte limitada de los clientes que abandonan.\n")

cat("4. El F1-score fue de 16.67%, por lo que el modelo necesita mejoras.\n")

cat("5. Se recomienda fortalecer las estrategias de retencion de clientes.\n")


# 18. RECOMENDACIONES ESTRATEGICAS

cat("\n====================================\n")
cat("RECOMENDACIONES PARA REDUCIR EL CHURN\n")
cat("====================================\n")

cat("1. Identificar clientes con alta probabilidad de abandono.\n")

cat("2. Implementar programas de fidelizacion.\n")

cat("3. Mejorar la atencion y soporte al cliente.\n")

cat("4. Incentivar el uso frecuente de la plataforma.\n")

cat("5. Monitorear periodicamente el comportamiento de los clientes.\n")

cat("6. Mejorar el modelo predictivo para aumentar la deteccion de abandono.\n")
