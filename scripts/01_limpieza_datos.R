# ==========================================
# SCRIPT 01: LIMPIEZA Y PREPARACIÓN DE DATOS
# ==========================================

library(readxl)    # Para leer el Excel
library(tidyverse) # Para manipulación de datos 
library(lubridate) # Para convertir formatos MM:SS a segundos

# ==========================================
# CARGA DE DATOS Y FILTRO DE CATEGORIA
# ==========================================

#Cargar Base de datos
ruta <- file.choose() 
base <- read_excel(ruta, sheet = "Base de Datos(para R)")

# Filtramos por categoría "Roki" y sexo "Masculino"
df_roki <- base %>%
  mutate(across(where(is.character), str_trim)) %>%
  filter(str_detect(categoria, fixed("roki", ignore_case = TRUE))) %>%
  filter(str_detect(sexo, fixed("masculino", ignore_case = TRUE))) 

cat("Muestra final de hombres para el análisis:", nrow(df_roki), "\n")

# =========================================================
# CONVERSIÓN DE TIEMPO A SEGUNDOS y DE CAP A REPS FALTANTES
# =========================================================

#Time Cap en segundos de cada wod (wod 2 sin tc y wod 5 max reps)
params <- list(
  W1 = list(tc = 480), 
  W3 = list(tc = 540), 
  W4 = list(tc = 480), 
  W6 = list(tc = 420)
)

# Extracción de repeticiones en caso de CAP
df_roki <- df_roki %>%
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

# =========================================================
#TRATAMIENTO DEL WOD 6 (FINAL PARA MEJORES 12 ATLETAS)
# =========================================================

#Marcar NA para aquellos que no hicieron el wod final
df_roki <- df_roki %>%
  mutate(`WOD 6 FINAL_Pos` = ifelse(`WOD 6 FINAL_Pos` >= 13, NA, `WOD 6 FINAL_Pos`),
         `WOD 6 FINAL_Res` = ifelse(is.na(`WOD 6 FINAL_Pos`), NA, `WOD 6 FINAL_Res`))

# ==========================================
# EXPORTACIÓN PARA EL SIGUIENTE SCRIPT
# ==========================================

archivo_limpio <- "data/df_roki_limpio.RData"
save(df_roki, params, file = archivo_limpio)

cat("\n>>> ¡Limpieza completada! Datos guardados en:", archivo_limpio, "\n")
