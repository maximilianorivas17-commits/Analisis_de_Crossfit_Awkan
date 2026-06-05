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
# PREPARACIÓN DE DATOS
# =========================================================

colnames(df_model) <- make.names(colnames(df_model))

# 1. Invertir WOD 5 y crear variable objetivo categórica (Top 12)
df_model <- df_model %>%
  mutate(WOD.5 = WOD.5 * -1) %>%
  mutate(Status = as.factor(ifelse(Posicion <= 12, "Finalista", "No_Finalista")))

# 2. Base para el PCA (mantenemos solo numéricas)
df_numeric <- df_model %>% select(starts_with("WOD"))

# 3. Base exclusiva para Random Forest (sacamos Posicion numérica para evitar la tautología)
df_rf <- df_model %>% select(Status, starts_with("WOD"))

# =========================================================
# 1. DIAGNÓSTICO DEL MODELO (Transparencia y Matriz de Confusión)
# =========================================================
set.seed(124)

# Entrenamos el modelo con balanceo estricto (12 vs 12)
rf_conv <- randomForest(Status ~ ., data = df_rf, 
                        ntree = 500, 
                        importance = TRUE,
                        strata = df_rf$Status,
                        sampsize = c(Finalista = 12, No_Finalista = 12))

print("--- Diagnóstico del Modelo: Matriz de Confusión (OOB) ---")
print(rf_conv$confusion)

# Gráfico de convergencia
plot(rf_conv, main = "Convergencia: Error OOB vs N° de Árboles")
legend("topright", colnames(rf_conv$err.rate), col=1:3, fill=1:3)

# =========================================================
# 2. RANDOM FOREST ROBUSTO (Promedio de 150 Simulaciones)
# =========================================================
n_iter <- 150
lista_imp <- list()

for(i in 1:n_iter) {
  set.seed(i * 10)
  rf_tmp <- randomForest(Status ~ ., data = df_rf, 
                         ntree = 500, 
                         importance = TRUE,
                         strata = df_rf$Status,
                         sampsize = c(Finalista = 12, No_Finalista = 12))
  
  imp_mat <- as.data.frame(importance(rf_tmp))
  
  lista_imp[[i]] <- data.frame(
    Simulacion = i,
    WOD = rownames(imp_mat),
    MeanDecreaseAccuracy = imp_mat[, "MeanDecreaseAccuracy"], 
    MeanDecreaseGini = imp_mat[, "MeanDecreaseGini"]
  )
}

df_rf_crudo_long <- bind_rows(lista_imp) %>%
  arrange(Simulacion, WOD)

importancia_estable <- df_rf_crudo_long %>%
  group_by(WOD) %>%
  summarise(
    Media_Acc = mean(MeanDecreaseAccuracy), SD_Acc = sd(MeanDecreaseAccuracy),
    Media_Gini = mean(MeanDecreaseGini), SD_Gini = sd(MeanDecreaseGini)
  ) %>%
  arrange(desc(Media_Acc))

# --- GRÁFICOS ACTUALIZADOS ---
p1 <- ggplot(importancia_estable, aes(x = reorder(WOD, Media_Acc), y = Media_Acc)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.85) +
  geom_errorbar(aes(ymin = Media_Acc - SD_Acc, ymax = Media_Acc + SD_Acc), width = 0.2, color = "gray30") +
  coord_flip() + theme_minimal(base_size = 10) + 
  labs(title = "Poder Discriminatorio (Accuracy)", subtitle = "¿Qué prueba define el corte a la Final?", x = "", y = "Pérdida de Precisión (%)")

p2 <- ggplot(importancia_estable, aes(x = reorder(WOD, Media_Gini), y = Media_Gini)) +
  geom_bar(stat = "identity", fill = "darkorange", alpha = 0.85) +
  geom_errorbar(aes(ymin = Media_Gini - SD_Gini, ymax = Media_Gini + SD_Gini), width = 0.2, color = "gray30") +
  coord_flip() + theme_minimal(base_size = 10) + 
  labs(title = "Pureza de Nodos (Gini)", subtitle = "Estructuración de niveles", x = "", y = "Caída del Índice Gini")

p1 / p2

# ==============================================================================
# 3. ANÁLISIS DE SIGNIFICANCIA ESTADÍSTICA (Tests Emparejados)
# ==============================================================================
test_significancia <- pairwise.t.test(
  x = df_rf_crudo_long$MeanDecreaseAccuracy, # Cambiamos IncMSE por Accuracy
  g = df_rf_crudo_long$WOD,         
  paired = TRUE,                     
  p.adjust.method = "holm"           
)

p_matriz <- as.data.frame(test_significancia$p.value) %>%
  rownames_to_column("WOD_A") %>%
  pivot_longer(-WOD_A, names_to = "WOD_B", values_to = "p_val") %>%
  filter(!is.na(p_val)) %>%
  mutate(Significativo = ifelse(p_val < 0.05, "Sí (p < 0.05)", "No significativo"))

p_calor <- ggplot(p_matriz, aes(x = WOD_A, y = WOD_B, fill = Significativo)) +
  geom_tile(color = "white", lwd = 0.5) +
  geom_text(aes(label = format(p_val, scientific = TRUE, digits = 2)), size = 3, fontface = "bold") +
  scale_fill_manual(values = c("Sí (p < 0.05)" = "#E6B0AA", "No significativo" = "#EAFAF1")) +
  theme_minimal(base_size = 10) +
  labs(title = "Matriz de Significancia (Permutation Accuracy)", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")

print(p_calor)

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
res_km <- kmeans(data_cluster, centers = 3, nstart = 25)

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
