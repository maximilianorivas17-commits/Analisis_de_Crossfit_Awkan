# =========================================================
# SCRIPT 05: MODELAMIENTO, CONVERGENCIA Y SEGMENTACIÓN
# =========================================================

load("data/df_supuestos_final.RData")

library(tidyverse)
library(randomForest)
library(FactoMineR)
library(factoextra)
library(patchwork)

# =========================================================
# PREPARACIÓN DE DATOS (Sin filtros de Mahalanobis)
# =========================================================

colnames(df_model) <- make.names(colnames(df_model))
df_numeric <- df_model %>% select(starts_with("WOD"))

# =========================================================
# CONVERGENCIA DEL MODELO (Búsqueda de ntree óptimo)
# =========================================================

set.seed(124)
#Random Forest con 200 arboles
rf_conv <- randomForest(Posicion ~ ., data = df_model, ntree = 200)

# Gráfico de convergencia para visualizar convergencia del el modelo
plot(rf_conv, main = "Estudio de Convergencia: Error OOB vs Número de Árboles")

# =========================================================
# RANDOM FOREST ROBUSTO (Promedio de 150 Simulaciones)
# =========================================================
n_iter <- 150
lista_imp <- list()

for(i in 1:n_iter) {
  set.seed(i * 10)
  rf_tmp <- randomForest(Posicion ~ ., data = df_model, ntree = 1000, importance = TRUE)
  
  imp_mat <- as.data.frame(importance(rf_tmp))
  lista_imp[[i]] <- data.frame(
    WOD = rownames(imp_mat),
    IncMSE = imp_mat[, 1], 
    Purity = imp_mat[, 2]
  )
}

# Métricas
importancia_estable <- bind_rows(lista_imp) %>%
  group_by(WOD) %>%
  summarise(Media_IncMSE = mean(IncMSE), SD_IncMSE = sd(IncMSE),
            Media_Purity = mean(Purity), SD_Purity = sd(Purity)) %>%
  arrange(desc(Media_IncMSE))

# Visualización de importancia consolidada
p1 <- ggplot(importancia_estable, aes(x = reorder(WOD, Media_IncMSE), y = Media_IncMSE)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = Media_IncMSE - SD_IncMSE, ymax = Media_IncMSE + SD_IncMSE), width = 0.2) +
  coord_flip() + theme_minimal() + labs(title = "Importancia Estable: %IncMSE")

p2 <- ggplot(importancia_estable, aes(x = reorder(WOD, Media_Purity), y = Media_Purity)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  geom_errorbar(aes(ymin = Media_Purity - SD_Purity, ymax = Media_Purity + SD_Purity), width = 0.2) +
  coord_flip() + theme_minimal() + labs(title = "Importancia Estable: NodePurity")

p1 / p2
importancia_estable

# =========================================================
# 4. PCA PROFUNDO: AUTOVALORES Y VARIANZA
# =========================================================

res_pca <- PCA(df_numeric, scale.unit = TRUE, graph = FALSE)

# Tabla de Autovalores y Varianza Explicada
tabla_pca <- as.data.frame(get_eig(res_pca))
print(tabla_pca)

# Tabla de varianza de Dim vs Wods
contribuciones_wods <- as.data.frame(res_pca$var$contrib)
print(round(contribuciones_wods, 2))

# Biplot de Capacidades
fviz_pca_biplot(res_pca, repel = TRUE, col.var = "red", col.ind = "steelblue", 
                title = "Biplot PCA: Estructura Completa (Incluye Atípicos)")

# =========================================================
# CLUSTERING (K-means usando solo Dim 1 y Dim 2)
# =========================================================

# Usamos las coordenadas de las primeras 2 dimensiones del PCA 
data_cluster <- res_pca$ind$coord[, 1:2]

#Metodo del codo
p_elbow <- fviz_nbclust(data_cluster, kmeans, method = "wss") +
  labs(title = "Método del Codo", subtitle = "Busca el 'quiebre' en la curva (Inflexión)")
p_elbow

set.seed(123)
res_km <- kmeans(data_cluster, centers = 6, nstart = 25)

# Visualización de Clusters en 2D
fviz_cluster(res_km, data = data_cluster, stand = FALSE, ellipse.type = "convex",
             main = "Segmentación Basada en Principales Componentes (2D)")

# =========================================================
# 6. CARACTERIZACIÓN TÉCNICA DE LOS GRUPOS
# =========================================================

df_final <- df_model %>% 
  mutate(Cluster = as.factor(res_km$cluster))

#Calcular metricas de cada cluster
resumen_clusters <- df_final %>%
  group_by(Cluster) %>%
  summarise(
    Atletas = n(),
    Pos_Promedio = round(mean(Posicion), 1),
    # Identificamos a los líderes y rezagados de cada grupo
    Mejor_Pos    = min(Posicion),
    Peor_Pos     = max(Posicion),
    # Promedios de rendimiento físico
    across(starts_with("WOD"), ~round(mean(.x, na.rm = TRUE), 1), .names = "Avg_{.col}")
  ) %>%
  arrange(Pos_Promedio)

# Mostramos la tabla 
print(resumen_clusters)

# ==========================================
# EXPORTACIÓN PARA EL SIGUIENTE SCRIPT
# ==========================================
save(res_pca, 
     res_km, 
     df_final, 
     resumen_clusters, 
     tabla_pca, 
     importancia_estable, 
     file = "data/df_analisis_final.RData")

cat("\n>>> Script 05 finalizado. Todos los datos han sido blindados en 'df_analisis_final.RData'.")
