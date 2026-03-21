# =========================================================
# SCRIPT 04: EVALUACIÓN DE SUPUESTOS ESTADÍSTICOS
# =========================================================

load("data/df_eda_processed.RData")

library(tidyverse)
library(nortest)    # Tests de normalidad
library(car)        # VIF y gráficos diagnósticos
library(psych)      # Bartlett
library(lmtest)     # Tests de linealidad
library(patchwork)  # Organizar gráficos

# Preparamos el dataset de trabajo (X e Y)
df_model <- df_audit %>% 
  select(Posicion, starts_with("WOD")) %>% 
  drop_na()

# Nombres de los WODs para los bucles
nombres_wods <- df_model %>% select(starts_with("WOD")) %>% names()

# =========================================================
# TEST DE NORMALIDAD UNIVARIADA (Shapiro-Wilk)
# =========================================================

normality_results <- df_model %>%
  select(starts_with("WOD")) %>%
  summarise(across(everything(), ~shapiro.test(.x)$p.value))

print("--- P-Values de Test de Normalidad (Shapiro-Wilk) ---")
print(normality_results)

# =========================================================
#  EVALUACIÓN DE LINEALIDAD INDIVIDUAL (X vs Y)
# =========================================================

test_linealidad_ind <- function(wod_name, data) {
  formula_simple <- as.formula(paste("Posicion ~ `", wod_name, "`", sep = ""))
  modelo_simple <- lm(formula_simple, data = data)
  
  p_ramsey <- resettest(modelo_simple, power = 2, type = "fitted")$p.value
  p_rainbow <- raintest(modelo_simple)$p.value
  
  tibble(
    WOD = wod_name,
    P_Ramsey = p_ramsey,
    P_Rainbow = p_rainbow,
    Decision = ifelse(p_ramsey < 0.05 | p_rainbow < 0.05, "NO LINEAL", "Lineal")
  )
}

resultados_linealidad <- map_df(nombres_wods, ~test_linealidad_ind(.x, df_model))

print("--- Diagnóstico de Linealidad Individual (WOD vs Posición) ---")
print(resultados_linealidad)

# ==========================================================
# DETERMINANCIA ROBUSTA (Correlación de Spearman)
# ==========================================================

matriz_cor <- cor(df_model, 
                  use = "pairwise.complete.obs", 
                  method = "spearman")

orden_importancia <- order(abs(matriz_cor["Posicion", ]), decreasing = TRUE)
matriz_ordenada <- matriz_cor[orden_importancia, orden_importancia]

corrplot(matriz_ordenada, 
         method = "number", 
         type = "upper", 
         diag = TRUE, 
         tl.col = "black", 
         tl.srt = 45, 
         mar = c(0, 0, 2, 0),
         title = "Jerarquía de Determinancia (Spearman)")

# =========================================================
#  MULTICOLINEALIDAD (VIF) - Modelo Conjunto
# =========================================================

modelo_conjunto <- lm(Posicion ~ ., data = df_model)
vif_values <- vif(modelo_conjunto)

print("--- Factor de Inflación de la Varianza (VIF) ---")
print(vif_values)

# =========================================================
# DISTANCIA DE MAHALANOBIS (Outliers Multivariados)
# =========================================================

m_dist <- mahalanobis(df_numeric, colMeans(df_numeric), cov(df_numeric))
df_numeric$mahal <- m_dist

umbral <- qchisq(p = 0.95, df = ncol(df_numeric) - 1)

ggplot(df_numeric, aes(x = 1:nrow(df_numeric), y = mahal)) +
  geom_point(aes(color = mahal > umbral)) +
  geom_hline(yintercept = umbral, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("steelblue", "red")) +
  theme_minimal() +
  labs(title = "Detección de Outliers Multivariados (Mahalanobis)",
       subtitle = "Puntos rojos: Atletas con perfiles de rendimiento anómalos",
       x = "ID de Atleta", y = "Distancia de Mahalanobis")

# =========================================================
# TEST DE BARTLETT (Esfericidad - Requisito PCA)
# =========================================================

bartlett_test <- cortest.bartlett(cor(df_numeric), n = nrow(df_numeric))

print("--- Test de Esfericidad de Bartlett (Requisito para PCA) ---")
print(paste("P-Value:", bartlett_test$p.value))

# ==========================================
# EXPORTACIÓN PARA EL SIGUIENTE SCRIPT
# ==========================================
save(df_model, df_numeric, matriz_cor, file = "data/df_supuestos_final.RData")
cat("\n>>> Script 04 finalizado. Listos para el PCA.")


