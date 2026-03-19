# ======================================================
# SCRIPT 02: LÓGICA DE PENALIZACIÓN Y AJUSTES DE TIEMPOS
# ======================================================
load("data/df_roki_limpio.RData")

library(tidyverse)

# =============================================
# FUNCION DE TIEMPO AGREGADO POR REPS FALTANTES
# =============================================

#funcion por tramos (reps que le faltaron)
calc_tramos <- function(reps_faltantes, cortes, tasas) {
  reps_faltantes <- ifelse(is.na(reps_faltantes), 0, reps_faltantes)
  p1 <- pmin(reps_faltantes, cortes[1]) * tasas[1]
  p2 <- pmax(0, pmin(reps_faltantes, cortes[2]) - cortes[1]) * tasas[2]
  p3 <- pmax(0, reps_faltantes - cortes[2]) * tasas[3]
  return(p1 + p2 + p3)
}

#penalizaciones segun wod
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

#base de datos definitiva con tiempos calculados segun el criterio propuesto
df_roki_final <- df_roki %>%
  ajustar_tiempos_experto("WOD 1_Res", "Reps_H_W1", params$W1$tc, "WOD1") %>%
  ajustar_tiempos_experto("WOD 3_Res", "Reps_H_W3", params$W3$tc, "WOD3") %>%
  ajustar_tiempos_experto("WOD 4_Res", "Reps_H_W4", params$W4$tc, "WOD4") %>%
  mutate(`WOD 2_Res_Adj` = `WOD 2_Res`, `WOD 5_Res_Adj` = `WOD 5_Res`)

# ==========================================
# EXPORTACIÓN PARA EL SIGUIENTE SCRIPT
# ==========================================

#columnas relevantes para el análisis
df_audit <- df_roki_final %>%
  select(
    Competidor, 
    Puntos,
    Posicion,, 
    contains("_Adj") # Trae todos los WODs ya ajustados
  ) %>%
  rename_with(~str_remove(., "_Res_Adj"), contains("_Adj"))

save(df_audit, file = "data/df_roki_audit.RData")

cat("\n>>> ¡Script 02 Exitoso! Base de auditoría guardada con", nrow(df_audit), "atletas.")

