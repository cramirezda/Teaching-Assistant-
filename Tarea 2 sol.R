# Tarea 2
# Script creado por: Carlos Dávila
# Revisado y editado por: Arturo Aguilar

#Cargar librerias requeridas
rm(list = ls())

install <- function(packages){
  new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new.packages))
    install.packages(new.packages, dependencies = TRUE)
  sapply(packages, require, character.only = TRUE)
}
required.packages <- c('dplyr', 'tidyr', 'ggplot2', 'fixest',
                       'MASS', 'haven','jtools')

install(required.packages)

#Abrir base de datos
df <- read_dta("inpresdata.dta") %>%
  mutate(
    # --- Preparación y Renombres ---
    birthplace = p509pro * 100 + p509kab,
    birthyr    = p504thn + 1900 # R no usa 'capture rename' para esto
  ) %>%
  rename(
    classwk = p608,
    wt      = weight
  ) %>%
  mutate(
    # --- Generación de Variables (Equivalente a 'gen') ---
    
    # gen byte age74 = 1974 - birthyr
    age74 = 1974 - birthyr,
    
    # gen byte age = year - birthyr
    # Asumimos que la encuesta es de 1995 (Year=1995) si no hay variable 'year'
    age = 1995 - birthyr, 
    
    # gen byte old = inrange(age74, 12, 17)
    old = ifelse(age74 >= 12 & age74 <= 17, 1, 0),
    
    # gen byte young = inrange(age74, 2, 6)
    young = ifelse(age74 >= 2 & age74 <= 6, 1, 0),
    
    # gen byte reallyold = inrange(age74, 18, 24)
    reallyold = ifelse(age74 >= 18 & age74 <= 24, 1, 0)
  )

df_we <- df %>%
  filter(!is.na(lhwage) & lhwage >0)
  


# Calcular y mostrar tabla de resumen
data.frame(
  Estadistica = c("Muestra total", "Personas con salario > 0"),
  Valor = c(
    nrow(df),
    sum(df$wage > 1, na.rm = TRUE)
  )
)

# ====/// 1: Grafica y reg inicial \\\=====

ggplot(df, aes(x = yeduc, y = lhwage)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()

ggsave("Grafica_inicial.png",  width = 5.54, height = 4.95)

reg1 <- lm(lhwage~yeduc,data=df)
summ(reg1,robust = "HC1",digits=4)


# ====/// 2: Replicación Tabla 3 \\\=====

# 1. Calcular los promedios agrupados (con filtro inicial)
resumen <- df %>%
  filter(!is.na(lhwage) & !is.infinite(lhwage) & lhwage>0) %>% # Filtra observaciones con salario positivo/válido
  filter(treat1b == 1 | treat2b == 1) %>%
  mutate(
    cohorte = ifelse(treat2b == 1, "Aged 2 to 6", "Aged 12 to 17"),
    intensidad = ifelse(recp == 1, "High", "Low")
  ) %>%
  group_by(cohorte, intensidad) %>%
  summarise(
    yeduc = mean(yeduc, na.rm = TRUE),
    lhwage = mean(lhwage, na.rm = TRUE), 
    .groups = 'drop'
  )


# 2. Función para construir la tabla (sin cambios)
construir_tabla <- function(datos, variable) {
  tabla <- datos %>%
    dplyr::select(cohorte, intensidad, valor = all_of(variable)) %>%
    pivot_wider(names_from = intensidad, values_from = valor) %>%
    arrange(desc(cohorte)) %>% 
    mutate(Difference = High - Low)
  
  diff_row <- tibble(
    cohorte = "Difference",
    High = tabla$High[1] - tabla$High[2],
    Low  = tabla$Low[1] - tabla$Low[2],
    Difference = (tabla$High[1] - tabla$High[2]) - (tabla$Low[1] - tabla$Low[2])
  )
  bind_rows(tabla, diff_row)
}

# 3. Visualizar
print(as.data.frame(construir_tabla(resumen, "yeduc")))
print(as.data.frame(construir_tabla(resumen, "lhwage")))



# ==============================================================================
#Table 3: DID ESPECIFICACIONES WLS
# ==============================================================================
library(fixest)
library(dplyr) # Necesario para el pipe %>%

# 1. Preparación de Data Frames
# df_wages: Contiene las cohortes old/young y está filtrado por salario
df_wages <- df %>% filter(old==1 | young==1, lhwage >0)

# df_reallyold_wages: Contiene las cohortes old/reallyold y está filtrado por salario
df_reallyold_wages <- df %>% filter(old==1 | reallyold==1, lhwage >0)


# 2. Especificaciones feols (OLS Ponderado)
# Nota: La sintaxis A*B es idéntica a A + B + A:B (young##recp)

# MODELO 1: Educación (Experiment)
modelo_1 <- feols(yeduc ~ young * recp, 
                  data = df_wages, 
                  weights = ~wt) # Usamos cluster para replicar SEs robustos

# MODELO 2: Salario (Experiment)
modelo_2 <- feols(lhwage ~ young * recp, 
                  data = df_wages, 
                  weights = ~wt)

# MODELO 3: Educación (Placebo)
modelo_3 <- feols(yeduc ~ old * recp, 
                  data = df_reallyold_wages, 
                  weights = ~wt)

# MODELO 4: Salario (Placebo)
modelo_4 <- feols(lhwage ~ old * recp, 
                  data = df_reallyold_wages, 
                  weights = ~wt)

# Resumen de resultados
etable(modelo_1, modelo_2, modelo_3, modelo_4,se='iid')

# ==============================================================================
# FUNCIÓN AUXILIAR MODIFICADA: Calcula Coeficientes Y Errores Estándar
# ==============================================================================
get_table_values_se <- function(model, cohort_dummy_name) {
  
  # 1. Extraer coeficientes y matriz Varianza-Covarianza
  coefs <- coef(model)
  V <- vcov(model)
  
  # Identificar índices de los coeficientes para construir vectores
  idx_b0     <- match("(Intercept)", names(coefs))
  idx_recp   <- match("recp", names(coefs))
  idx_cohort <- match(cohort_dummy_name, names(coefs))
  
  # Buscar interacción (puede ser young:recp o recp:young)
  int_name <- grep(paste0(":", "recp"), names(coefs), value=TRUE)
  if(length(int_name)==0) int_name <- grep(paste0("recp", ":"), names(coefs), value=TRUE)
  idx_did <- match(int_name, names(coefs))
  
  # --- Función interna para calcular SE de combinación lineal ---
  calc_se <- function(indices) {
    # indices: vector numérico de posiciones de coeficientes a sumar
    # w: vector de pesos (1 en las posiciones indicadas, 0 en las demás)
    w <- rep(0, length(coefs))
    w[indices] <- 1
    # SE = raiz(w' V w)
    sqrt(t(w) %*% V %*% w)
  }
  
  # --- 2. Calcular Medias y SEs ---
  
  # Grupo Old (Cohort=0)
  # Low (recp=0): Intercepto
  mean_ctl_low <- coefs[idx_b0]
  se_ctl_low   <- calc_se(c(idx_b0))
  
  # High (recp=1): Intercepto + recp
  mean_ctl_high <- coefs[idx_b0] + coefs[idx_recp]
  se_ctl_high   <- calc_se(c(idx_b0, idx_recp))
  
  # Grupo Young/Treated (Cohort=1)
  # Low (recp=0): Intercepto + Cohort
  mean_tr_low  <- coefs[idx_b0] + coefs[idx_cohort]
  se_tr_low    <- calc_se(c(idx_b0, idx_cohort))
  
  # High (recp=1): Intercepto + Cohort + recp + Interaction
  mean_tr_high <- coefs[idx_b0] + coefs[idx_cohort] + coefs[idx_recp] + coefs[idx_did]
  se_tr_high   <- calc_se(c(idx_b0, idx_cohort, idx_recp, idx_did))
  
  # DID (Coeficiente puro)
  did_val <- coefs[idx_did]
  did_se  <- sqrt(V[idx_did, idx_did]) # SE directo de la tabla
  
  # Retornar lista con todo
  return(list(
    means = c(mean_tr_high, mean_tr_low, mean_ctl_high, mean_ctl_low, did_val),
    ses   = c(se_tr_high, se_tr_low, se_ctl_high, se_ctl_low, did_se)
  ))
}

# ==============================================================================
# FUNCIÓN DE IMPRESIÓN (Formato con Paréntesis)
# ==============================================================================
print_panel <- function(model, cohort_name, panel_title) {
  res <- get_table_values_se(model, cohort_name)
  vals <- res$means
  ses  <- res$ses
  
  # Diferencias Horizontales (High - Low)
  # SE aproximado asintótico para diferencias de medias correlacionadas es complejo sin covarianza completa.
  # Para simplificar visualización Rápida, usamos SE del DID para la diferencia de diferencias
  # y SE combinados simples para filas/columnas, o simplemente mostramos coeficientes.
  # Aquí calculamos solo los valores.
  
  diff_row1 <- vals[1] - vals[2]
  diff_row2 <- vals[3] - vals[4]
  diff_col1 <- vals[1] - vals[3]
  diff_col2 <- vals[2] - vals[4]
  
  cat(paste0("\n=== ", panel_title, " ===\n"))
  cat(sprintf("%-20s %-15s %-15s %-15s\n", "", "High (1)", "Low (2)", "Diff (3)"))
  
  # Fila 1 (Tratado)
  cat(sprintf("%-20s %6.2f          %6.2f          %6.2f\n", "Treated Cohort", vals[1], vals[2], diff_row1))
  cat(sprintf("%-20s (%5.3f)         (%5.3f)\n", "", ses[1], ses[2]))
  
  # Fila 2 (Control)
  cat(sprintf("%-20s %6.2f          %6.2f          %6.2f\n", "Control Cohort", vals[3], vals[4], diff_row2))
  cat(sprintf("%-20s (%5.3f)         (%5.3f)\n", "", ses[3], ses[4]))
  
  # Fila 3 (Difference)
  cat(sprintf("%-20s %6.2f          %6.2f          %6.2f (DID)\n", "Difference", diff_col1, diff_col2, vals[5]))
  cat(sprintf("%-20s                                (%5.3f)\n", "", ses[5]))
}

# ==============================================================================
# EJECUCIÓN
# ==============================================================================

# PANEL A
print_panel(modelo_1, "young", "PANEL A: YEARS OF EDUCATION")
print_panel(modelo_2, "young", "PANEL A: LOG WAGES")

# PANEL B
print_panel(modelo_3, "old", "PANEL B: YEARS OF EDUCATION (PLACEBO)")
print_panel(modelo_4, "old", "PANEL B: LOG WAGES (PLACEBO)")




# 1. Extraer los valores de la Tabla 3 para el Panel A (Educación)
# Modelo 1: Educación, Cohorte joven (young)
res_educ <- get_table_values_se(modelo_1, "young")
vals <- res_educ$means

# 2. Construir el Data Frame para la Gráfica
# La gráfica DID requiere dos puntos en el tiempo (Viejo y Joven)
data_plot <- data.frame(
  Cohorte = factor(c("Viejo", "Joven", "Viejo", "Joven"), levels = c("Viejo", "Joven")),
  
  # Años de Educación Observados (Y-axis)
  Obs_High = c(vals[4], vals[2], NA, NA),   # Observado: Ctl_Low, Tr_Low
  Obs_Low = c(vals[3], vals[1], NA, NA),    # Observado: Ctl_High, Tr_High
  
  # Datos Reales
  Efecto = c(vals[3], vals[1], vals[4], vals[2]), # High_Old, High_Young, Low_Old, Low_Young
  
  # Grupo: 1=Alta (Tratamiento), 0=Baja (Control)
  Grupo = factor(c("Alta (Tratamiento)", "Alta (Tratamiento)", 
                   "Baja (Control)", "Baja (Control)"), 
                 levels = c("Baja (Control)", "Alta (Tratamiento)")),
  
  # Contrafactual (Solo para el grupo de tratamiento)
  Contrafactual = c(vals[3], vals[3] + (vals[2] - vals[4]),  # Old_High, Old_High + Trend_Low
                    NA, NA)
)

# Creamos las tres líneas necesarias
df_control <- data.frame(
  Cohorte = factor(c("Viejo", "Joven"), levels = c("Viejo", "Joven")),
  Efecto = c(vals[4], vals[2]), # Low_Old, Low_Young
  Tipo = "1. Tendencia de Control (Baja)"
)

df_tratado <- data.frame(
  Cohorte = factor(c("Viejo", "Joven"), levels = c("Viejo", "Joven")),
  Efecto = c(vals[3], vals[1]), # High_Old, High_Young
  Tipo = "2. Tendencia Observada (Alta)"
)

df_contrafactual <- data.frame(
  Cohorte = factor(c("Viejo", "Joven"), levels = c("Viejo", "Joven")),
  Efecto = c(vals[3], vals[3] + (vals[2] - vals[4])), # High_Old, Contrafactual
  Tipo = "3. Tendencia Contrafactual"
)

# Unir los data frames para la visualización
data_final <- bind_rows(df_control, df_tratado, df_contrafactual)
data_final$Tipo <- factor(data_final$Tipo, levels = c("1. Tendencia de Control (Baja)", 
                                                      "3. Tendencia Contrafactual", 
                                                      "2. Tendencia Observada (Alta)"))

# 3. Graficar en ggplot2
ggplot(data_final, aes(x = Cohorte, y = Efecto, group = Tipo, color = Tipo)) +
  
  # Líneas de Tendencia
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  
  # Conexión del Contrafactual al Tratamiento Observado (efecto causal)
  geom_segment(aes(x = "Joven", xend = "Joven", 
                   y = data_final$Efecto[data_final$Tipo == "3. Tendencia Contrafactual" & data_final$Cohorte == "Joven"],
                   yend = data_final$Efecto[data_final$Tipo == "2. Tendencia Observada (Alta)" & data_final$Cohorte == "Joven"]),
               color = "black", 
               linetype = "dashed",
               arrow = arrow(length = unit(0.2, "cm"))) +
  
  # Etiquetas y Ejes
  labs(title = "Estrategia DID: Contrafactual y Efecto Causal del Programa INPRES",
       subtitle = "Años de Educación (Panel A, Tabla 3)",
       x = "Cohorte",
       y = "Años de Educación Promedio",
       color = "Trayectoria") +
  
  # Tema y Estilo
  theme_minimal() +
  scale_color_manual(values = c("1. Tendencia de Control (Baja)" = "blue", 
                                "2. Tendencia Observada (Alta)" = "red", 
                                "3. Tendencia Contrafactual" = "red4")) +
  
  # Leyenda y Eje Y
  theme(legend.position = "bottom") +
  expand_limits(y = min(data_final$Efecto) - 0.1)

# ==============================================================================
#-- TABLA 4---
# ==============================================================================

library(fixest)

# --- 1. EDUCACIÓN (Muestra Completa) ---

# Stata: xtreg yeduc 1.young#c.nin birthyr##c.ch71 if old | young, fe
m1 <- feols(yeduc ~ young:nin + i(birthyr, ch71) | birthpl + birthyr,
            data = df,
            subset = ~ old == 1 | young == 1)
            #cluster = ~birthpl)

# Stata: xtreg yeduc 1.young#c.nin birthyr##c.(ch71 en71) if old | young, fe
m2 <- feols(yeduc ~ young:nin + i(birthyr, ch71) + i(birthyr, en71) | birthpl + birthyr,
            data = df,
            subset = ~ old == 1 | young == 1)
            #cluster = ~birthpl)

# Stata: xtreg yeduc 1.young#c.nin birthyr##c.(ch71 en71 wsppc) if old | young, fe
m3 <- feols(yeduc ~ young:nin + i(birthyr, ch71) + i(birthyr, en71) + i(birthyr, wsppc) | birthpl + birthyr,
            data = df,
            subset = ~ old == 1 | young == 1)
            #cluster = ~birthpl)


# --- 2. EDUCACIÓN (Solo muestra 'part' / wage earners) ---

# Stata: xtreg yeduc ... if (old | young) & part, fe
# Nota: Asumo que la variable 'part' es 1 si trabaja, o puedes usar !is.na(lhwage)

m4 <- feols(yeduc ~ young:nin + i(birthyr, ch71) | birthpl + birthyr,
            data = df_wages,
            subset = ~ (old == 1 | young == 1))
          #  cluster = ~birthpl)

m5 <- feols(yeduc ~ young:nin + i(birthyr, ch71) + i(birthyr, en71) | birthpl + birthyr,
            data = df_wages,
            subset = ~ (old == 1 | young == 1))
           # cluster = ~birthpl)

m6 <- feols(yeduc ~ young:nin + i(birthyr, ch71) + i(birthyr, en71) + i(birthyr, wsppc) | birthpl + birthyr,
            data = df_wages,
            subset = ~ (old == 1 | young == 1))
          #  cluster = ~birthpl)


# --- 3. SALARIOS (Log Hourly Wage) ---

# Stata: xtreg lhwage ... if old | young, fe
# Nota: Las regresiones de salario implícitamente filtran a quienes tienen salario.

m7 <- feols(lhwage ~ young:nin + i(birthyr, ch71) | birthpl + birthyr,
            data = df,
            subset = ~ old == 1 | young == 1)
            #cluster = ~birthpl)
summary(m7)
m8 <- feols(lhwage ~ young:nin + i(birthyr, ch71) + i(birthyr, en71) | birthpl + birthyr,
            data = df,
            subset = ~ old == 1 | young == 1)
            #cluster = ~birthpl)

m9 <- feols(lhwage ~ young:nin + i(birthyr, ch71) + i(birthyr, en71) + i(birthyr, wsppc) | birthpl + birthyr,
            data = df,
            subset = ~ old == 1 | young == 1)
            #cluster = ~birthpl)

# --- VISUALIZAR TABLA ---
etable(m1, m2, m3, m4, m5, m6, m7, m8, m9, 
       keep = "young", # Mostrar solo el coeficiente de interés
       headers = c("Educ (All)", "Educ (All)", "Educ (All)", 
                   "Educ (Part)", "Educ (Part)", "Educ (Part)", 
                   "Wage", "Wage", "Wage"),
       se="iid",
       tex= TRUE)


# ==============================================================================
# FIGURA 1: EFFECT OF PROGRAM INTENSITY BY AGE (EVENT STUDY)
# ==============================================================================

# 1. Preparar datos para la Figura
# Duflo usa el rango de edad 2 a 24.
df_fig1 <- df %>% 
  filter(age74 >= 2 & age74 <= 24)

# 2. Estimación del Modelo (Event Study)
# Sintaxis fixest: i(factor_var, continuous_var, ref = X)
# - factor_var: age74 (Eje X)
# - continuous_var: nin (Intensidad)
# - ref = 12: Establecemos la edad 24 como base (coeficiente 0), igual que el paper.
# - FEs: birthpl (absorbido) + birthyr (dummies)

est_figura1 <- feols(yeduc ~ i(age74, nin, ref = 24)+i(birthyr, ch71)| birthpl + birthyr,
                     data = df_fig1,
                     weights = ~wt,        # Usamos pesos analíticos como en Stata
                     cluster = ~birthpl)   # Errores agrupados

# 3. Visualización Rápida (Estilo base de fixest)
iplot(est_figura1, 
      main = "Effect of INPRES program on Education by Age",
      xlab = "Age in 1974",
      ylab = "Coefficient Estimate")

# 4. Visualización Exacta (Estilo Paper con ggplot2)
# El paper invierte el eje X (Viejos a la izquierda, Jóvenes a la derecha)
# y usa líneas punteadas para los intervalos.

library(ggplot2)
library(broom) # Para extraer coeficientes limpiamente

# Extraemos los coeficientes y los intervalos de confianza
datos_grafica <- broom::tidy(est_figura1, conf.int = TRUE, conf.level = 0.95) %>%
  # Limpiamos el nombre del término para tener solo el número de la edad
  mutate(age = as.numeric(gsub("age74::", "", gsub(":nin", "", term))))
# Graficamos
ggplot(datos_grafica, aes(x = age, y = estimate)) +
  # Intervalo de confianza (Líneas punteadas)
  geom_line(aes(y = conf.low), linetype = "dashed", color = "black", size = 0.5) +
  geom_line(aes(y = conf.high), linetype = "dashed", color = "black", size = 0.5) +
  # Línea de coeficientes (Sólida con diamantes)
  geom_line(color = "black", size = 0.8) +
  geom_point(shape = 18, size = 3, color = "black") + # Shape 18 es diamante
  # Línea horizontal en 0
  geom_hline(yintercept = 0, linetype = "solid", size = 0.3) +
  # Invertir eje X (De 24 a 2)
  scale_x_reverse(breaks = seq(2, 23, 2)) +
  scale_y_continuous(limits = c(-0.3,0.5)) +
  # Etiquetas
  labs(title = "Figure 2: Coefficients of the Interactions",
       subtitle = "Y= Education",
       x = "Age in 1974",
       y = "Coefficient estimate") +
  # Tema limpio (Blanco y negro)
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

ggsave("Figure2_Duflo.png",  width = 6.54, height = 4.2)

