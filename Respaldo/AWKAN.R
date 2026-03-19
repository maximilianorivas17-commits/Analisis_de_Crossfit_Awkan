# ==========================================
# 1. CARGA DE LIBRERÍAS
# ==========================================
# Cargamos todas las herramientas necesarias para el flujo completo
library(readxl)
library(tidyverse)
library(lubridate)
library(Hmisc)
library(patchwork)
library(moments)  
library(gridExtra)
library(lmtest)
library(corrplot) 
library(lm.beta)
library(car)      
library(olsrr)
library(FactoMineR)
library(factoextra)
library(quantreg) 

# ==========================================
# 2. IMPORTACIÓN Y FILTRADO PREVIO (SOLO HOMBRES)
# ==========================================
ruta <- file.choose() 
base_raw <- read_excel(ruta, sheet = "Base de Datos(para R)")
# Filtramos por categoría "Roki" y sexo "Masculino"
df_roki <- base_raw %>%
  mutate(across(where(is.character), str_trim)) %>%
  filter(str_detect(categoria, fixed("roki", ignore_case = TRUE))) %>%
  filter(str_detect(sexo, fixed("masculino", ignore_case = TRUE))) 

cat("Muestra final de hombres para el análisis:", nrow(df_roki), "\n")

# ==========================================
# 3. CONVERSIÓN DE TIEMPOS A SEGUNDOS
# ==========================================
params <- list(
  W1 = list(tc = 480), W3 = list(tc = 540), 
  W4 = list(tc = 480), W6 = list(tc = 420)
)

df_roki_modificada <- df_roki %>%
  # Extracción de repeticiones en caso de CAP
  mutate(
    Reps_H_W1 = as.numeric(str_extract(ifelse(str_detect(`WOD 1_Res`, "CAP|cap"), `WOD 1_Res`, NA), "\\d+")),
    Reps_H_W3 = as.numeric(str_extract(ifelse(str_detect(`WOD 3_Res`, "CAP|cap"), `WOD 3_Res`, NA), "\\d+")),
    Reps_H_W4 = as.numeric(str_extract(ifelse(str_detect(`WOD 4_Res`, "CAP|cap"), `WOD 4_Res`, NA), "\\d+")),
    Reps_H_W6 = as.numeric(str_extract(ifelse(str_detect(`WOD 6 FINAL_Res`, "CAP|cap"), `WOD 6 FINAL_Res`, NA), "\\d+"))
  ) %>%
  # Conversión de formatos MM:SS a segundos puros
  mutate(across(c(`WOD 1_Res`, `WOD 2_Res`, `WOD 3_Res`, `WOD 4_Res`, `WOD 6 FINAL_Res`), 
                ~sapply(., function(x) {
                  x <- as.character(x)
                  if (is.na(x) || x == "" || str_detect(x, "CAP|cap")) return(NA)
                  if (str_detect(x, ":")) return(as.numeric(ms(x)))
                  return(suppressWarnings(as.numeric(x)))
                }))) %>%
  mutate(`WOD 5_Res` = as.numeric(`WOD 5_Res`), Posicion = as.numeric(Posicion))

# ==========================================
# 4. TRATAMIENTO PARA FINALISTAS (WOD 6)
# ==========================================
df_roki_modificada <- df_roki_modificada %>%
  mutate(`WOD 6 FINAL_Pos` = ifelse(`WOD 6 FINAL_Pos` >= 13, NA, `WOD 6 FINAL_Pos`),
         `WOD 6 FINAL_Res` = ifelse(is.na(`WOD 6 FINAL_Pos`), NA, `WOD 6 FINAL_Res`))

# ==========================================
# 5. LÓGICA DE PENALIZACIÓN Y AJUSTES
# ==========================================
calc_tramos <- function(reps_faltantes, cortes, tasas) {
  reps_faltantes <- ifelse(is.na(reps_faltantes), 0, reps_faltantes)
  p1 <- pmin(reps_faltantes, cortes[1]) * tasas[1]
  p2 <- pmax(0, pmin(reps_faltantes, cortes[2]) - cortes[1]) * tasas[2]
  p3 <- pmax(0, reps_faltantes - cortes[2]) * tasas[3]
  return(p1 + p2 + p3)
}

ajustar_tiempos_experto <- function(df, col_res, col_reps, tc, tipo_wod) {
  df %>% mutate(!!paste0(col_res, "_Adj") := case_when(
    !is.na(!!sym(col_res)) & !!sym(col_res) < tc ~ !!sym(col_res),
    is.na(!!sym(col_res)) & !is.na(!!sym(col_reps)) ~ {
      reps <- !!sym(col_reps)
      penalidad <- case_when(
        tipo_wod == "WOD1" ~ calc_tramos(reps, c(18, 39), c(3, 2, 1)),
        tipo_wod == "WOD3" ~ reps * 5,
        tipo_wod == "WOD4" ~ calc_tramos(reps, c(12, 14), c(2, 10, 2)),
        TRUE ~ reps * 1.5
      )
      tc + penalidad
    },
    TRUE ~ !!sym(col_res)
  ))
}

df_roki_final <- df_roki_modificada %>%
  ajustar_tiempos_experto("WOD 1_Res", "Reps_H_W1", params$W1$tc, "WOD1") %>%
  ajustar_tiempos_experto("WOD 3_Res", "Reps_H_W3", params$W3$tc, "WOD3") %>%
  ajustar_tiempos_experto("WOD 4_Res", "Reps_H_W4", params$W4$tc, "WOD4") %>%
  mutate(`WOD 2_Res_Adj` = `WOD 2_Res`, `WOD 5_Res_Adj` = `WOD 5_Res`)

# ==========================================
# 6. ANÁLISIS EXPLORATORIO VISUAL
# ==========================================
df_long <- df_roki_final %>%
  select(ends_with("_Adj"), -contains("WOD 6")) %>%
  pivot_longer(everything(), names_to = "WOD", values_to = "Valor")

# Histogramas para ver distribuciones reales
ggplot(df_long, aes(x = Valor, fill = WOD)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~WOD, scales = "free") +
  theme_minimal() +
  labs(title = "Distribución de Rendimiento Crudo")

# ==========================================
# 7. PRUEBA DE NORMALIDAD (Justificación técnica)
# ==========================================
cat("\n--- TESTS DE NORMALIDAD (SHAPIRO-WILK) ---\n")
df_roki_final %>%
  select(ends_with("_Adj"), -contains("WOD 6")) %>%
  summarise(across(everything(), ~shapiro.test(.x)$p.value)) %>%
  pivot_longer(everything(), names_to = "WOD", values_to = "p_value")

# ==========================================
# 8. DIAGNÓSTICO DE SUPUESTOS (DATOS CRUDOS)
# ==========================================
# Preparamos el dataset para el modelado
df_audit <- df_roki_final %>%
  select(Posicion, ends_with("_Adj"), -contains("WOD 6")) %>%
  rename_with(~str_remove(., "_Res_Adj"), ends_with("_Adj")) %>%
  drop_na()

cat("\n--- AUDITORÍA DE LINEALIDAD Y HOMOCEDASTICIDAD ---\n")
lapply(names(df_audit)[-1], function(x) {
  mod <- lm(as.formula(paste("Posicion ~ `", x, "`", sep="")), data = df_audit)
  data.frame(WOD = x, BP_Homoced_p = bptest(mod)$p.value, RESET_Lin_p = resettest(mod)$p.value)
}) %>% do.call(rbind, .)

# ==========================================
# 9. MATRIZ DE CORRELACIÓN DE SPEARMAN (NO PARAMÉTRICA)
# ==========================================
# Spearman usa RANGOS, eliminando el problema de la escala y normalidad
matriz_spearman <- rcorr(as.matrix(df_audit), type = "spearman")
corrplot(matriz_spearman$r, method = "color", type = "upper", addCoef.col = "black",
         p.mat = matriz_spearman$P, sig.level = 0.05, insig = "blank",
         title = "Determinancia de Rangos (Spearman)")

# ==========================================
# 10. MODELO DE MEDIANA (REGRESIÓN DE CUANTILES)
# ==========================================
df_modelo <- df_audit %>% rename(Pos_Final = Posicion)

# Modelo principal: No requiere supuestos de normalidad
modelo_mediana <- rq(Pos_Final ~ ., data = df_modelo, tau = 0.5)
summary(modelo_mediana, se = "boot")


# Cálculo del Pseudo-R2 (Goodness of Fit para mediana)
modelo_nulo <- rq(Pos_Final ~ 1, data = df_modelo, tau = 0.5)
pseudo_r2 <- 1 - (modelo_mediana$rho / modelo_nulo$rho)

# Cálculo de Precisión (MAE)
errores <- abs(df_modelo$Pos_Final - predict(modelo_mediana))
cat("\nPseudo-R2:", round(pseudo_r2, 4), "| Error Medio (MAE):", round(mean(errores), 2), "puestos")

# ==========================================
# 11. IDENTIFICACIÓN DE INFLUYENTES (COOK'S DISTANCE)
# ==========================================
# Detectamos quiénes se alejan del patrón común
modelo_aux <- lm(Pos_Final ~ ., data = df_modelo)
cooks_dist <- cooks.distance(modelo_aux)
umbr_cook <- 4/nrow(df_modelo)

influyentes <- which(cooks_dist > umbr_cook)
cat("\nAtletas influyentes detectados:", length(influyentes), "\n")
print(df_roki$Competidor[influyentes])
print(df_roki$Posicion[influyentes])


# ==========================================
# 12. ANÁLISIS DE IMPORTANCIA: MODELO ESTANDARIZADO
# ==========================================
# Estandarizamos las variables (Z-scores) para que las unidades (segundos vs reps) 
# y los diferentes Time Caps sean comparables entre sí.

# 1. Creamos una versión del dataset con las X estandarizadas
df_std <- df_modelo %>%
  mutate(across(starts_with("W"), ~as.numeric(scale(.))))

# 2. Ajustamos el modelo de mediana sobre datos estandarizados
modelo_std <- rq(Pos_Final ~ ., data = df_std, tau = 0.5)

# 3. Extraemos los coeficientes para comparar magnitud de impacto
importancia_wods <- summary(modelo_std, se = "boot")$coefficients %>%
  as.data.frame() %>%
  slice(-1) %>% # Quitamos el intercepto
  mutate(WOD = rownames(.),
         Impacto_Absoluto = abs(Value)) %>%
  arrange(desc(Impacto_Absoluto))

cat("\n--- RANKING DE IMPORTANCIA DE LOS WODS (MODELO ESTANDARIZADO) ---\n")
print(importancia_wods %>% select(WOD, Coef_Estandarizado = Value, Impacto_Absoluto))

# 4. Visualización de la Determinancia
ggplot(importancia_wods, aes(x = reorder(WOD, Impacto_Absoluto), y = Impacto_Absoluto, fill = WOD)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Determinancia del Éxito: ¿Qué WOD pesó más?",
       subtitle = "Magnitud del efecto en el ranking (Coeficientes Estandarizados)",
       x = "Capacidad Física Evaluada",
       y = "Impacto en la Posición Final") +
  theme(legend.position = "none")# ==========================================
# 13. PCA (ESTRUCTURA FÍSICA)
# ==========================================
df_pca <- df_modelo %>% select(-Pos_Final)
res_pca <- PCA(df_pca, scale.unit = TRUE, graph = FALSE)

fviz_pca_var(res_pca, col.var = "contrib", repel = TRUE,
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "Círculo de Correlaciones (Atleta Roki)")

# ==========================================
# 14. CLUSTERING JERÁRQUICO (HCPC)
# ==========================================
res_hcpc <- HCPC(res_pca, nb.clust = -1, graph = FALSE)

fviz_cluster(res_hcpc, repel = TRUE, show.clust.cent = TRUE, palette = "jco",
             main = "Segmentación de Perfiles Físicos")

# ==========================================
# 15. ANÁLISIS DE PERFILES (DATOS ROBUSTOS)
# ==========================================
df_perfiles <- res_hcpc$data.clust %>% mutate(Posicion = df_modelo$Pos_Final)

resumen_perfiles <- df_perfiles %>%
  group_by(clust) %>%
  summarise(N = n(), Mediana_Pos = median(Posicion), 
            Mejor = min(Posicion), Consistencia_IQR = IQR(Posicion)) %>%
  arrange(Mediana_Pos)

print(resumen_perfiles)




























































# ==========================================
# 2. IMPORTACIÓN Y FILTRADO (ESCALADO - HOMBRES)
# ==========================================
# ==========================================
# 2. IMPORTACIÓN Y FILTRADO EXACTO (ESCALADO)
# ==========================================

df_escalado <- base_raw %>%
  # Limpiamos espacios al inicio/final y convertimos a minúsculas para no fallar
  mutate(categoria_clean = str_trim(tolower(categoria)),
         sexo_clean = str_trim(tolower(sexo))) %>%
  
  # FILTRO EXACTO: Usamos == en lugar de str_detect
  # Aquí especificas el nombre EXACTO que aparece en tu Excel
  filter(categoria_clean == "scaled", 
         sexo_clean == "masculino") %>%
  
  # Eliminamos las columnas auxiliares de limpieza para mantener la base original
  select(-categoria_clean, -sexo_clean)

# Verificación de integridad
cat("Muestra final (Filtro Exacto):", nrow(df_escalado), "atletas.\n")


# ==========================================
# 3. CONVERSIÓN DE TIEMPOS CON CRITERIO DE EXPERTOS
# ==========================================

# Función auxiliar para extraer repeticiones faltantes del texto (ej: "CAP 15" -> 15)
extraer_reps <- function(x) {
  reps <- as.numeric(str_extract(x, "\\d+"))
  return(ifelse(is.na(reps), 0, reps))
}

df_modificada <- df_escalado %>%
  mutate(across(c(`WOD 1_Res`, `WOD 2_Res`, `WOD 3_Res`, `WOD 4_Res`), 
                ~as.character(.), .names = "{.col}_str")) %>%
  mutate(
    # --- WOD 1 (Time Cap: 480s) ---
    W1 = sapply(`WOD 1_Res_str`, function(x) {
      if (!str_detect(x, "CAP|cap")) return(as.numeric(ms(x)))
      f = extraer_reps(x)
      penalizacion = case_when(
        f <= 18 ~ f * 3,
        f > 18 & f <= 28 ~ f * 2.5,
        f > 28 ~ f * 1.5,
        TRUE ~ 0
      )
      return(480 + penalizacion)
    }),
    
    # --- WOD 2 (Sin Time Cap) ---
    W2 = sapply(`WOD 2_Res_str`, function(x) {
      if (str_detect(x, ":")) return(as.numeric(ms(x)))
      return(as.numeric(x))
    }),
    
    # --- WOD 3 (Time Cap: 540s / +4 seg por rep) ---
    W3 = sapply(`WOD 3_Res_str`, function(x) {
      if (!str_detect(x, "CAP|cap")) return(as.numeric(ms(x)))
      f = extraer_reps(x)
      return(540 + (f * 4))
    }),
    
    # --- WOD 4 (Time Cap: 480s) ---
    W4 = sapply(`WOD 4_Res_str`, function(x) {
      if (!str_detect(x, "CAP|cap")) return(as.numeric(ms(x)))
      f = extraer_reps(x)
      penalizacion = case_when(
        f <= 12 ~ f * 2.5,
        f > 12 & f <= 14 ~ f * 10, # Tramo de alta dificultad
        f > 14 ~ f * 3,
        TRUE ~ 0
      )
      return(480 + penalizacion)
    }),
    
    # --- WOD 5 (Max Reps - Se mantiene igual) ---
    W5 = as.numeric(`WOD 5_Res`),
    Pos_Final = as.numeric(Posicion)
  ) %>%
  # Limpieza de columnas temporales
  select(-ends_with("_str"))

# Verificación de que no hayamos creado NAs accidentales
cat("Atletas procesados con penalización:", nrow(df_modificada), "\n")

# ==========================================
# 4. PREPARACIÓN FINAL Y TRATAMIENTO DE NAs
# ==========================================
# Seleccionamos variables y eliminamos atletas con datos incompletos
df_modelo <- df_modificada %>%
  select(Pos_Final, W1, W2, W3, W4, W5) %>%
  drop_na() # Aquí resolvemos el problema de las filas desiguales (N=52 vs 53)

# ==========================================
# 5. DIAGNÓSTICO DE SUPUESTOS
# ==========================================
cat("\n--- AUDITORÍA DE SUPUESTOS (DATOS CRUDOS) ---\n")
lapply(names(df_modelo)[-1], function(x) {
  mod <- lm(as.formula(paste("Pos_Final ~ `", x, "`", sep="")), data = df_modelo)
  data.frame(WOD = x, 
             Normalidad_p = shapiro.test(residuals(mod))$p.value,
             BP_Homoced_p = bptest(mod)$p.value, 
             RESET_Lin_p = resettest(mod)$p.value)
}) %>% do.call(rbind, .)

# ==========================================
# 6. CORRELACIÓN DE SPEARMAN Y MODELO DE MEDIANA
# ==========================================
# Matriz de Spearman (Rangos)
matriz_spearman <- rcorr(as.matrix(df_modelo), type = "spearman")
corrplot(matriz_spearman$r, method = "color", type = "upper", addCoef.col = "black", 
         title = "Determinancia Escalado (Spearman)", mar = c(0,0,1,0))

# Modelo de Mediana (No Paramétrico)
modelo_mediana <- rq(Pos_Final ~ W1 + W2 + W3 + W4 + W5, data = df_modelo, tau = 0.5)
summary(modelo_mediana, se = "boot")

# ==========================================
# 7. IMPORTANCIA ESTANDARIZADA (Z-SCORES)
# ==========================================
df_std <- df_modelo %>% mutate(across(starts_with("W"), ~as.numeric(scale(.))))
modelo_std <- rq(Pos_Final ~ ., data = df_std, tau = 0.5)

cat("\n--- IMPORTANCIA RELATIVA DE CADA WOD ---\n")
summary(modelo_std, se = "boot")$coefficients

# ==========================================
# 8. DISTANCIA DE COOK (INFLUYENTES)
# ==========================================
cooks_dist <- cooks.distance(lm(Pos_Final ~ ., data = df_modelo))
umbr_cook <- 4/nrow(df_modelo)
cat("\nAtletas Influyentes:", df_escalado$Competidor[which(cooks_dist > umbr_cook)], "\n")
cat("\nAtletas Influyentes:", df_escalado$Posicion[which(cooks_dist > umbr_cook)], "\n")

# ==========================================
# 9. PCA Y CLUSTERING (CON ETIQUETAS FISIOLÓGICAS)
# ==========================================
res_pca <- PCA(df_modelo %>% select(-Pos_Final), scale.unit = TRUE, graph = FALSE)

# Biplot con tus categorías: Fitness/Fatiga (X) y Acidez/Aeróbico (Y)
fviz_pca_var(res_pca, col.var = "contrib", repel = TRUE,
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "Mapa Metabólico: Escalado Varones")

# Clustering Jerárquico
res_hcpc <- HCPC(res_pca, nb.clust = -1, graph = FALSE)
fviz_cluster(res_hcpc, repel = TRUE, main = "Perfiles Físicos Escalado")

# Resumen de Perfiles
resumen_perfiles <- res_hcpc$data.clust %>% 
  mutate(Posicion = df_modelo$Pos_Final) %>%
  group_by(clust) %>%
  summarise(N = n(), Mediana_Pos = median(Posicion), IQR_Consistencia = IQR(Posicion))

print(resumen_perfiles)

