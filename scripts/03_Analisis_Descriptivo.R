# ==========================================
# SCRIPT 03: ANÁLISIS EXPLORATORIO (EDA)
# ==========================================

load("data/df_roki_audit.RData")

library(tidyverse)
library(patchwork)  # Para organizar múltiples gráficos
library(corrplot)   # Para la matriz de correlación visual
library(moments)    # Para calcular asimetría y curtosis
library(scales) # Para el formato de tiempo

df_posiciones <- df_audit %>%
  select(
    Competidor, 
    Puntos,
    Posicion,, 
    contains("Pos")
  )

df_audit <- df_audit %>%
  select(
    Competidor, 
    Puntos,
    Posicion,, 
    contains("_Adj"),
  )%>%
  rename_with(~str_remove(., "_Res_Adj"), contains("_Adj"))


# =============================================
# ESTADÍSTICOS DESCRIPTIVOS DE INGENIERÍA
# =============================================

# Calculamos Media, Mediana y SD
stats_desc <- df_audit %>%
  select(starts_with("WOD")) %>%
  summarise(across(everything(), list(
    Media = ~mean(.x, na.rm = TRUE),
    Mediana = ~median(.x, na.rm = TRUE),
    SD = ~sd(.x, na.rm = TRUE)
  ))) %>%
  pivot_longer(everything(), names_to = c("WOD", ".value"), names_sep = "_")

print("--- Resumen Estadístico por WOD ---")
print(stats_desc)

# =============================
# GRAFICOS DE DENSIDADES 
# =============================

# --- DENSIDAD WOD 1 ---
densidad1 <- ggplot(df_audit %>% filter(!is.na(`WOD 1`)), aes(x = `WOD 1`)) +
  geom_density(fill = "#5DADE2", alpha = 0.6, color = "white") +
  scale_x_continuous(breaks = seq(0, 1500, by = 60)) +
  theme_minimal() +
  labs(title = "Distribución: WOD 1", x = "Segundos (Marcas cada 60s)", y = "Densidad")
print(densidad1)

# --- DENSIDAD WOD 2 ---
densidad2 <- ggplot(df_audit %>% filter(!is.na(`WOD 2`)), aes(x = `WOD 2`)) +
  geom_density(fill = "#48C9B0", alpha = 0.6, color = "white") +
  scale_x_continuous(breaks = seq(0, 6000, by = 120)) +
  theme_minimal() +
  labs(title = "Distribución: WOD 2", x = "Segundos", y = "Densidad")
print(densidad2)

# --- DENSIDAD WOD 3 ---
densidad3 <- ggplot(df_audit %>% filter(!is.na(`WOD 3`)), aes(x = `WOD 3`)) +
  geom_density(fill = "#F4D03F", alpha = 0.6, color = "white") +
  scale_x_continuous(breaks = seq(0, 1500, by = 60)) +
  theme_minimal() +
  labs(title = "Distribución: WOD 3", x = "Segundos", y = "Densidad")
print(densidad3)

# --- DENSIDAD WOD 4 ---
densidad4 <- ggplot(df_audit %>% filter(!is.na(`WOD 4`)), aes(x = `WOD 4`)) +
  geom_density(fill = "#EB984E", alpha = 0.6, color = "white") +
  scale_x_continuous(breaks = seq(0, 1500, by = 60)) +
  theme_minimal() +
  labs(title = "Distribución: WOD 4", 
       subtitle = "Penalización de 10s aplicada en CAP",
       x = "Segundos", y = "Densidad")
print(densidad4)

# --- DENSIDAD WOD 5 ---
densidad5 <- ggplot(df_audit %>% filter(!is.na(`WOD 5`)), aes(x = `WOD 5`)) +
  geom_density(fill = "#AF7AC5", alpha = 0.6, color = "white") +
  scale_x_continuous(breaks = seq(0, 2000, by = 5)) +
  theme_minimal() +
  labs(title = "Distribución: WOD 5", x = "Segundos / Reps", y = "Densidad")
print(densidad5)

# ========================================================
# ANÁLISIS DE CAUNTILES Y DATOS ATÍPICOS MEDIANTE BOXPLOTS
# ========================================================

# Función auxiliar
get_metadata_wod <- function(df, var_name) {
  # Filtramos NAs para el cálculo
  df_clean <- df %>% filter(!is.na(!!sym(var_name)))
  
  # Estadísticos básicos
  res_stats <- df_clean %>%
    summarise(
      q1  = quantile(!!sym(var_name), 0.25),
      med = median(!!sym(var_name)),
      q3  = quantile(!!sym(var_name), 0.75),
      mu  = mean(!!sym(var_name))
    )
  
  # Identificación de outliers
  iqr_val <- res_stats$q3 - res_stats$q1
  res_outliers <- df_clean %>%
    filter(!!sym(var_name) < (res_stats$q1 - 1.5 * iqr_val) | 
             !!sym(var_name) > (res_stats$q3 + 1.5 * iqr_val))
  
  return(list(stats = res_stats, outliers = res_outliers))
}

# ---BOXPLOT WOD 1 ---
meta_w1 <- get_metadata_wod(df_audit, "WOD 1")
boxplot1 <- ggplot(df_audit %>% filter(!is.na(`WOD 1`)), aes(y = `WOD 1`, x = "")) +
  geom_boxplot(fill = "steelblue", alpha = 0.4, outlier.color = "red") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "darkred") +
  geom_text(data = meta_w1$outliers, aes(x = 1, y = `WOD 1`, label = round(`WOD 1`, 1)), hjust = -0.5, color = "red", fontface = "bold") +
  geom_text(data = meta_w1$stats, aes(x = 1.2, y = q1, label = paste("Q1:", round(q1, 1))), size = 3) +
  geom_text(data = meta_w1$stats, aes(x = 1.2, y = med, label = paste("Med:", round(med, 1))), fontface = "bold") +
  geom_text(data = meta_w1$stats, aes(x = 1.2, y = q3, label = paste("Q3:", round(q3, 1))), size = 3) +
  theme_minimal() + labs(title = "Cuantiles: WOD 1", y = "Segundos")
print(boxplot1)

# --- BOXPLOT WOD 2 ---
meta_w2 <- get_metadata_wod(df_audit, "WOD 2")
boxplot2 <- ggplot(df_audit %>% filter(!is.na(`WOD 2`)), aes(y = `WOD 2`, x = "")) +
  geom_boxplot(fill = "steelblue", alpha = 0.4, outlier.color = "red") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "darkred") +
  geom_text(data = meta_w2$outliers, aes(x = 1, y = `WOD 2`, label = round(`WOD 2`, 1)), hjust = -0.5, color = "red", fontface = "bold") +
  geom_text(data = meta_w2$stats, aes(x = 1.2, y = q1, label = paste("Q1:", round(q1, 1))), size = 3) +
  geom_text(data = meta_w2$stats, aes(x = 1.2, y = med, label = paste("Med:", round(med, 1))), fontface = "bold") +
  geom_text(data = meta_w2$stats, aes(x = 1.2, y = q3, label = paste("Q3:", round(q3, 1))), size = 3) +
  theme_minimal() + labs(title = "Cuantiles: WOD 2", y = "Segundos")
print(boxplot2)

# --- BOXPLOT WOD 3:  ---
meta_w3 <- get_metadata_wod(df_audit, "WOD 3")
boxplot3 <- ggplot(df_audit %>% filter(!is.na(`WOD 3`)), aes(y = `WOD 3`, x = "")) +
  geom_boxplot(fill = "steelblue", alpha = 0.4, outlier.color = "red") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "darkred") +
  geom_text(data = meta_w3$outliers, aes(x = 1, y = `WOD 3`, label = round(`WOD 3`, 1)), hjust = -0.5, color = "red", fontface = "bold") +
  geom_text(data = meta_w3$stats, aes(x = 1.2, y = q1, label = paste("Q1:", round(q1, 1))), size = 3) +
  geom_text(data = meta_w3$stats, aes(x = 1.2, y = med, label = paste("Med:", round(med, 1))), fontface = "bold") +
  geom_text(data = meta_w3$stats, aes(x = 1.2, y = q3, label = paste("Q3:", round(q3, 1))), size = 3) +
  theme_minimal() + labs(title = "Cuantiles: WOD 3", y = "Segundos")
print(boxplot3)

# --- BOXPLOT WOD 4 ---
meta_w4 <- get_metadata_wod(df_audit, "WOD 4")
boxplot4 <- ggplot(df_audit %>% filter(!is.na(`WOD 4`)), aes(y = `WOD 4`, x = "")) +
  geom_boxplot(fill = "steelblue", alpha = 0.4, outlier.color = "red") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "darkred") +
  geom_text(data = meta_w4$outliers, aes(x = 1, y = `WOD 4`, label = round(`WOD 4`, 1)), hjust = -0.5, color = "red", fontface = "bold") +
  geom_text(data = meta_w4$stats, aes(x = 1.2, y = q1, label = paste("Q1:", round(q1, 1))), size = 3) +
  geom_text(data = meta_w4$stats, aes(x = 1.2, y = med, label = paste("Med:", round(med, 1))), fontface = "bold") +
  geom_text(data = meta_w4$stats, aes(x = 1.2, y = q3, label = paste("Q3:", round(q3, 1))), size = 3) +
  theme_minimal() + labs(title = "Cuantiles: WOD 4", y = "Segundos")
print(boxplot4)

# --- BOXPLOT WOD 5 ---
meta_w5 <- get_metadata_wod(df_audit, "WOD 5")
boxplot5 <- ggplot(df_audit %>% filter(!is.na(`WOD 5`)), aes(y = `WOD 5`, x = "")) +
  geom_boxplot(fill = "steelblue", alpha = 0.4, outlier.color = "red") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "darkred") +
  geom_text(data = meta_w5$outliers, aes(x = 1, y = `WOD 5`, label = round(`WOD 5`, 1)), hjust = -0.5, color = "red", fontface = "bold") +
  geom_text(data = meta_w5$stats, aes(x = 1.2, y = q1, label = paste("Q1:", round(q1, 1))), size = 3) +
  geom_text(data = meta_w5$stats, aes(x = 1.2, y = med, label = paste("Med:", round(med, 1))), fontface = "bold") +
  geom_text(data = meta_w5$stats, aes(x = 1.2, y = q3, label = paste("Q3:", round(q3, 1))), size = 3) +
  theme_minimal() + labs(title = "Cuantiles: WOD 5", y = "Repeticiones")
print(boxplot5)

# ==============================================
# GRAFICOS DE DISPERSIÓN (WOD VS POSICION FINAL)
# ==============================================

# --- DISPERSIÓN WOD 1 ---
dispersion1 <- ggplot(df_audit, aes(x = `WOD 1`, y = Posicion)) +
  geom_point(color = "steelblue", alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE) + 
  geom_smooth(method = "loess", color = "darkblue", se = TRUE, alpha = 0.2) +
  scale_y_reverse() + theme_minimal() +
  labs(title = "Relación: WOD 1", subtitle = "Rojo: Lineal | Azul: Tendencia Real")
print(dispersion1)

# --- DISPERSIÓN WOD 2 ---
dispersion2 <-ggplot(df_audit, aes(x = `WOD 2`, y = Posicion)) +
  geom_point(color = "steelblue", alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE) + 
  geom_smooth(method = "loess", color = "darkblue", se = TRUE, alpha = 0.2) +
  scale_y_reverse() + theme_minimal() +
  labs(title = "Relación: WOD 2", subtitle = "Rojo: Lineal | Azul: Tendencia Real")
print(dispersion2)

# --- DISPERSIÓN WOD 3 ---
dispersion3 <-ggplot(df_audit, aes(x = `WOD 3`, y = Posicion)) +
  geom_point(color = "steelblue", alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE) + 
  geom_smooth(method = "loess", color = "darkblue", se = TRUE, alpha = 0.2) +
  scale_y_reverse() + theme_minimal() +
  labs(title = "Relación: WOD 3", subtitle = "Rojo: Lineal | Azul: Tendencia Real")
print(dispersion3)

# --- DISPERSIÓN WOD 4 ---
dispersion4 <- ggplot(df_audit, aes(x = `WOD 4`, y = Posicion)) +
  geom_point(color = "steelblue", alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE) + 
  geom_smooth(method = "loess", color = "darkblue", se = TRUE, alpha = 0.2) +
  scale_y_reverse() + theme_minimal() +
  labs(title = "Relación: WOD 4", subtitle = "Rojo: Lineal | Azul: Tendencia Real")
print(dispersion4)

# --- DISPERSIÓN WOD 5 ---
dispersion5 <-ggplot(df_audit, aes(x = `WOD 5`, y = Posicion)) +
  geom_point(color = "steelblue", alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE) + 
  geom_smooth(method = "loess", color = "darkblue", se = TRUE, alpha = 0.2) +
  scale_y_reverse() + theme_minimal() +
  labs( title = "Relación: WOD 5", subtitle = "Rojo: Lineal | Azul: Tendencia Real")
print(dispersion5)

# ==========================================
# TABLA DE SUPERVIVENCIA (TIME CAP)
# ==========================================

resumen_cap <- df_audit %>%
  summarise(
    # WOD 1 (TC 480s)
    W1_Cap_N = sum(`WOD 1` >= 480, na.rm = TRUE),
    W1_Cap_Pct = (W1_Cap_N / sum(!is.na(`WOD 1`))) * 100,
    
    # WOD 2 (Umbral 30 min = 1800s)
    W2_Lento_N = sum(`WOD 2` >= 1800, na.rm = TRUE),
    W2_Lento_Pct = (W2_Lento_N / sum(!is.na(`WOD 2`))) * 100,
    
    # WOD 3 (TC 540s)
    W3_Cap_N = sum(`WOD 3` >= 540, na.rm = TRUE),
    W3_Cap_Pct = (W3_Cap_N / sum(!is.na(`WOD 3`))) * 100,
    
    # WOD 4 (TC 480s)
    W4_Cap_N = sum(`WOD 4` >= 480, na.rm = TRUE),
    W4_Cap_Pct = (W4_Cap_N / sum(!is.na(`WOD 4`))) * 100,
    
    # WOD 5 (Bajo Rendimiento < 30 reps)
    W5_Bajo_N = sum(`WOD 5` < 30, na.rm = TRUE),
    W5_Bajo_Pct = (W5_Bajo_N / sum(!is.na(`WOD 5`))) * 100
  ) %>%
  pivot_longer(everything(), names_to = c("WOD", ".value"), names_sep = "_(Cap|Lento|Bajo)_")

print("--- Análisis de Atletas que alcanzaron el límite (Cap/Bajo Rendimiento) ---")
print(resumen_cap)


# =========================
# ANÁLISIS DE CONSISTENCIA
# =========================

# Selección de columnas
df_consistencia <- df_posiciones %>%
  select(Competidor, Posicion, ends_with("Pos")) %>% 
  rowwise() %>%
  mutate(
    # Promedio de sus posiciones en los WODs
    Promedio_Ranks = mean(c_across(ends_with("Pos")), na.rm = TRUE),
    # Desviación Estándar de sus posiciones (Métrica de Consistencia)
    SD_Ranking = sd(c_across(ends_with("Pos")), na.rm = TRUE),
    # Coeficiente de Variación de sus posiciones
    CV_Ranking = (SD_Ranking / Promedio_Ranks) * 100
  ) %>%
  ungroup() %>%
  arrange(SD_Ranking) # El más consistente (menor SD) arriba

print("--- Perfil de Consistencia: Atletas más Regulares ---")
print(head(df_consistencia %>% select(Competidor, Posicion, Promedio_Ranks, SD_Ranking), 5))

print("--- Perfil de Consistencia: Atletas menos Regulares ---")
print(tail(df_consistencia %>% select(Competidor, Posicion, Promedio_Ranks, SD_Ranking), 5))


# ==========================================
# ANÁLISIS DE DIFICULTAD RELATIVA (PODER DISCRIMINATORIO)
# ==========================================

# Cálculo de métricas de dispersión y normalización
dificultad_wods <- df_audit %>%
  select(starts_with("WOD")) %>%
  pivot_longer(everything(), names_to = "WOD", values_to = "Valor") %>%
  filter(!is.na(Valor)) %>%
  group_by(WOD) %>%
  summarise(
    Media = mean(Valor),
    SD = sd(Valor),
    CV = (SD / Media) * 100,
    Min_Z = (min(Valor) - Media) / SD,
    Max_Z = (max(Valor) - Media) / SD,
    Rango_Z = abs(Max_Z - Min_Z) 
  ) %>%
  arrange(desc(CV)) 

# Visualización Comparativa del CV
ggplot(dificultad_wods, aes(x = reorder(WOD, CV), y = CV, fill = CV)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal() +
  labs(
    title = "Dificultad Relativa por WOD (Poder Separador)",
    subtitle = "A mayor CV, más diferencia generó la prueba entre atletas",
    x = "Prueba (WOD)",
    y = "Coeficiente de Variación (%)"
  )

print("--- Métricas de Dificultad Relativa ---")
print(dificultad_wods)

# ==========================================
# EXPORTACIÓN PARA EL SIGUIENTE SCRIPT
# ==========================================

save(df_audit, df_consistencia, dificultad_wods, file = "data/df_eda_processed.RData")

cat("\n>>> Script 03 finalizado. Datos guardados en 'data/df_eda_processed.RData'")
