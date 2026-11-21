
# ==========================================
# PROYECTO FINAL - BUSINESS ANALYTICS
# INTEGRANTE 2
# PREPARACION DE DATOS PARA MODELOS
# ==========================================

# Cargar libreria
library(dplyr)

# ==========================================
# ETAPA 11: PREPARACION DE VARIABLES
# ==========================================

# Importar la base original
Shoppers <- read.csv("datos/online_shoppers_intention.csv")

# Crear copia de la base original
Shoppers_modelo <- Shoppers

# Convertir la variable dependiente a numerica
# 0 = Sin compra
# 1 = Con compra

Shoppers_modelo <- Shoppers_modelo %>%
  mutate(
    Compra = as.numeric(Revenue)
  )

# Convertir variables categoricas a factores

Shoppers_modelo <- Shoppers_modelo %>%
  mutate(
    Month = as.factor(Month),
    VisitorType = as.factor(VisitorType),
    Weekend = as.factor(Weekend),
    OperatingSystems = as.factor(OperatingSystems),
    Browser = as.factor(Browser),
    Region = as.factor(Region),
    TrafficType = as.factor(TrafficType)
  )

# ==========================================
# ETAPA 12: VERIFICACION DE LOS DATOS
# ==========================================

# Revisar estructura de la nueva base
str(Shoppers_modelo)

# Verificar variable dependiente
table(Shoppers_modelo$Compra)

# Revisar resumen de las variables
summary(Shoppers_modelo)

# ==========================================
# ETAPA 13: EXPORTACION DE LA BASE PREPARADA
# ==========================================

# Guardar la base preparada
write.csv(
  Shoppers_modelo,
  "datos/online_shoppers_modelo.csv",
  row.names = FALSE
)

# Final de la preparacion de datos

