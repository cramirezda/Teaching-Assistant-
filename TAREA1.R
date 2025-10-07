rm(list=ls())

paquetes <- c('woolridge','dplyr','ggplot2','fixest','modelsummary',
              'knitr','tidyr','e1071','ineq','lmtest','sandwich',
              'jtools','stargazer')
sapply(paquetes, function(p) if (!require(p, character.only = TRUE)) install.packages(p, dependencies = TRUE))

library(dplyr); library(ggplot2); library(knitr); library(tidyr)
library(wooldridge); library(fixest);library(modelsummary);library(lmtest)       # coeftest
library(sandwich);library(jtools);library(stargazer) 

data(wage1)
df <- wage1
# 
# Cargar la librería necesaria
library(dplyr)

df <- df %>%
  mutate(
    west     = if ("west"     %in% names(.)) as.integer(west)     else 0L,
    south    = if ("south"    %in% names(.)) as.integer(south)    else 0L,
    northcen = if ("northcen" %in% names(.)) as.integer(northcen) else 0L,
    region = case_when(
      west == 1     ~ "West",
      south == 1    ~ "South",
      northcen == 1 ~ "Northcen",
      TRUE          ~ "East"
    ),
    region = factor(region, levels = c("East", "Northcen", "South", "West"))
  )

# -------- 1 Resumen: media, mediana, max, min, sd --------
tabla <- df %>% 
  summarise(
    media_wage = mean(wage, na.rm = TRUE),
    mediana_wage = median(wage, na.rm = TRUE),
    max_wage = max(wage, na.rm = TRUE),
    min_wage = min(wage, na.rm = TRUE),
    sd_wage = sd(wage, na.rm = TRUE),
    
    media_educ = mean(educ, na.rm = TRUE),
    mediana_educ = median(educ, na.rm = TRUE),
    max_educ = max(educ, na.rm = TRUE),
    min_educ = min(educ, na.rm = TRUE),
    sd_educ = sd(educ, na.rm = TRUE),
    
    media_exper = mean(exper, na.rm = TRUE),
    mediana_exper = median(exper, na.rm = TRUE),
    max_exper = max(exper, na.rm = TRUE),
    min_exper = min(exper, na.rm = TRUE),
    sd_exper = sd(exper, na.rm = TRUE),
    
    media_married = mean(married, na.rm = TRUE),
    mediana_married = median(married, na.rm = TRUE),
    max_married = max(married, na.rm = TRUE),
    min_married = min(married, na.rm = TRUE),
    sd_married = sd(married, na.rm = TRUE),
    
    media_nonw = mean(nonwhite, na.rm = TRUE),
    mediana_nonw = median(nonwhite, na.rm = TRUE),
    max_nonw = max(nonwhite, na.rm = TRUE),
    min_nonw = min(nonwhite, na.rm = TRUE),
    sd_nonw = sd(nonwhite, na.rm = TRUE),
    
    media_ndep = mean(numdep, na.rm = TRUE),
    mediana_ndep = median(numdep, na.rm = TRUE),
    max_ndep = max(numdep, na.rm = TRUE),
    min_ndep = min(numdep, na.rm = TRUE),
    sd_ndep = sd(numdep, na.rm = TRUE)
    
  ) %>% 
  pivot_longer(cols = everything(),
               names_to = c("estadistico", "variable"),
               names_sep = "_") %>% 
  pivot_wider(names_from = variable, values_from = value)

# 1 añadir error estándar de la media

n_wage  <- sum(!is.na(df$wage))
n_educ  <- sum(!is.na(df$educ))
n_exper <- sum(!is.na(df$exper))
n_married <- sum(!is.na(df$married))
n_nonw <- sum(!is.na(df$nonwhite))
n_ndep<- sum(!is.na(df$numdep))

se_row <- tibble::tibble(
  estadistico = "se_media",
  wage  = sd(df$wage,  na.rm = TRUE) / sqrt(n_wage),
  educ  = sd(df$educ,  na.rm = TRUE) / sqrt(n_educ),
  exper = sd(df$exper, na.rm = TRUE) / sqrt(n_exper),
  married= sd(df$married, na.rm = TRUE) / sqrt(n_married),
  nonw = sd(df$nonwhite, na.rm = TRUE) / sqrt(n_nonw),
  ndep = sd(df$numdep, na.rm = TRUE) / sqrt(n_ndep),
  
)

tabla <- bind_rows(tabla, se_row) %>%
  mutate(estadistico = factor(estadistico, levels = c("media","mediana","max","min","sd","se_media"))) %>%
  arrange(estadistico)

kable(tabla, digits = 2, caption = "Resumen estadístico")

# Analisis de muestra representativa
# 
# # --- Población ---
# df_pop <- df %>% select(wage) %>% filter(is.finite(wage))
# n_pop  <- nrow(df_pop)
# mu_pop <- mean(df_pop$wage)
# sd_pop <- sd(df_pop$wage)
# 
# # --- "Muestra" sesgada ---
# samp <- df_pop %>% arrange(wage) %>% slice_head(n = 350)
# 
# # --- Gráfica: distribuciones población vs muestra ---
# plot_df <- bind_rows(
#   df_pop %>% mutate(origen = "Población"),
#   samp   %>% mutate(origen = "Muestra")
# )
# 
# ggplot(plot_df, aes(wage, fill = origen)) +
#   geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = 0.5, position = "identity") +
#   geom_density(alpha = 0.2) +
#   geom_vline(xintercept = mu_pop, linetype = "dashed") +
#   geom_vline(xintercept = mean(samp$wage), linetype = "dotted") +
#   labs(title = "Distribución de salarios: población vs muestra ordenada",
#        x = "wage (por hora)", y = "Densidad") +
#   theme_minimal(base_size = 12)
# 
# # --- Media y error estándar de la muestra ---
# n_s   <- nrow(samp)
# xbar  <- mean(samp$wage)
# sebar <- sd(samp$wage) / sqrt(n_s)
# 
# # --- Prueba de igualdad de medias ---
# # Z-test usando sd poblacional (válido si tratamos df_pop como población conocida)
# Z   <- (xbar - mu_pop) / (sd_pop / sqrt(n_s))
# p_Z <- 2 * pnorm(-abs(Z))
# 
# # t-test estándar contra mu_pop
# t_out <- t.test(samp$wage, mu = mu_pop)
# 
# cat(sprintf(
#   "Población:  n=%d,  media=%.3f, sd=%.3f\nMuestra:    n=%d,  media=%.3f, SE(media)=%.3f\nZ=%.3f, p(Z)=%.3g\n t-test p=%.3g",
#   n_pop, mu_pop, sd_pop, n_s, xbar, sebar, Z, p_Z, t_out$p.value
# ))


# 1 Distribución de salarios y lwage

ggplot(df, aes(x = wage)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "white") +
  geom_density() +
  labs(x = "Valor", y = "Densidad", title = "Histograma + densidad")

ggplot(df, aes(x = lwage)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "white") +
  geom_density() +
  labs(x = "Valor", y = "Densidad", title = "Histograma + densidad")

# distribuciones por region
ggplot(df, aes(lwage)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "white") +
  geom_density() +
  facet_wrap(~ region, ncol = 2, scales = "free_y") +
  labs(title = "Distribución de log-salario por región (wage1)",
       x = "lwage = log(wage)", y = "Densidad") +
  theme_minimal(base_size = 12)

ggplot(df, aes(wage)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "white") +
  geom_density() +
  facet_wrap(~ region, ncol = 2, scales = "free_y") +
  labs(title = "Distribución de salarios por región (faceteado)",
       x = "wage (por hora)", y = "Densidad") +
  theme_minimal(base_size = 12)

# PRUEBA DE HIPÓTESIS DIFERENCIA DE MEDIAS
S <- df %>% filter(region=="South") %>% pull(wage)
W <- df %>% filter(region=="West")  %>% pull(wage)

# --- Paramétrico (Welch) "a mano"
nS <- length(S); nW <- length(W)
mS <- mean(S);   mW <- mean(W)
vS <- var(S);    vW <- var(W)
diff_hat <- mS - mW
se_diff  <- sqrt(vS/nS + vW/nW)
t_stat   <- diff_hat / se_diff
df_welch <- (vS/nS + vW/nW)^2 / ((vS^2/((nS^2)*(nS-1))) + (vW^2/((nW^2)*(nW-1))))
p_manual <- 2*pt(-abs(t_stat), df=df_welch)

# --- comandos automáticos
tt_welch  <- t.test(S, W, var.equal = FALSE)  # Welch
tt_pooled <- t.test(S, W, var.equal = TRUE)   # varianzas iguales

out <- data.frame(
  diff_hat = diff_hat,
  se_diff  = se_diff,
  t_stat   = t_stat,
  df_welch = df_welch,
  p_manual = p_manual,
  p_ttest_welch  = tt_welch$p.value,
  p_ttest_pooled = tt_pooled$p.value
)
print(out, digits = 4)


set.seed(123)
B   <- 500
nS  <- length(S); nW <- length(W)
pool <- c(S, W)

t_boot <- replicate(B, {
  xb <- sample(pool, size = nS + nW, replace = TRUE)
  Sb <- xb[seq_len(nS)]
  Wb <- xb[(nS+1):(nS+nW)]
  mSb <- mean(Sb); mWb <- mean(Wb)
  vSb <- var(Sb);  vWb <- var(Wb)
  se_b <- sqrt(vSb/length(Sb) + vWb/length(Wb))   # Welch en cada réplica
  (mSb - mWb)/se_b                                # estadístico propuesto
})

# p-valor bootstrap bilateral
p_boot <- mean(abs(t_boot) >= abs(t_stat))

# Comparación con paramétrico
c(t_obs = t_stat, p_manual = p_manual, p_bootstrap = p_boot)

# Gráfica de la distribución bootstrap del estadístico
ggplot(data.frame(t_boot = t_boot), aes(t_boot)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "white") +
  geom_density() +
  geom_vline(xintercept = t_stat, linetype = "dashed") +
  labs(title = "Bootstrap (pooled) del estadístico t de Welch",
       x = "t_boot", y = "Densidad")



#---- MAESTRÍA -----


# Submuestras por dummies originales: west y northcen
W   <- df %>% filter(west == 1)     %>% pull(wage)
NC  <- df %>% filter(northcen == 1) %>% pull(wage)
nW  <- length(W); nNC <- length(NC)
pool <- c(W, NC)

# --- K-S observado (dos muestras) ---
ks_obs <- suppressWarnings(ks.test(W, NC))
D_obs  <- as.numeric(ks_obs$statistic)   # sup|F_W - F_NC|
p_asym <- ks_obs$p.value                 # referencia asintótica

# --- Bootstrap bajo H0 (pooled) con B=500, tamaño nW+nNC ---
set.seed(123)
B <- 500
D_boot <- replicate(B, {
  xb <- sample(pool, size = nW + nNC, replace = TRUE)
  Wb <- xb[seq_len(nW)]
  NCb <- xb[(nW+1):(nW+nNC)]
  suppressWarnings(as.numeric(ks.test(Wb, NCb)$statistic))
})

# p-valor bootstrap (bilateral en K-S es unilateral por construcción)
p_boot <- mean(D_boot >= D_obs)

# --- Resultados ---
data.frame(
  D_obs = D_obs,
  p_ks_asintotico = p_asym,
  p_ks_bootstrap  = p_boot,
  n_west = nW, n_northcen = nNC
)

# --- Gráfica distribución bootstrap del estadístico K-S ---
ggplot(data.frame(D_boot = D_boot), aes(D_boot)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "white") +
  geom_density() +
  geom_vline(xintercept = D_obs, linetype = "dashed") +
  labs(title = "Bootstrap del estadístico K-S: West vs North Central",
       x = "D_boot", y = "Densidad")


# 1  gráfica proporción de individuos por región
# df2 <- df %>%
#   mutate(suma = northcen + south + west,
#          region = case_when(
#            suma == 0 ~ "East",
#            northcen == 1 & suma == 1 ~ "North Central",
#            south    == 1 & suma == 1 ~ "South",
#            west     == 1 & suma == 1 ~ "West",
#            TRUE ~ NA_character_
#          ))
# 
# prop_reg <- df2 %>%
#   filter(!is.na(region)) %>%
#   count(region, name = "n") %>%
#   mutate(prop = n / sum(n),
#          etiqueta = sprintf("%.1f%%", 100*prop))
# 
# ggplot(prop_reg, aes(x = "", y = prop, fill = region)) +
#   geom_col(width = 1) + coord_polar(theta = "y") +
#   geom_text(aes(label = etiqueta), position = position_stack(vjust = 0.5), color = "white") +
#   labs(x = NULL, y = NULL, fill = "Región", title = "Proporción por región") +
#   theme_void()

# 
# # --------  1 Calcular el coeficiente de asimetría --------
# 
# #lwage
# x <- na.omit(df$lwage)
# n <- length(x)
# 
# skew_G1 <- function(v){
#   v <- v[is.finite(v)]
#   n <- length(v); mu <- mean(v); s <- sd(v)
#   (n / ((n - 1) * (n - 2))) * sum((v - mu)^3) / (s^3)
# 
# }
# 
# theta_hat <- skew_G1(x)                  # G1 estimador muestral
# g1_pkg    <- e1071::skewness(x, type = 1)
# G1_pkg    <- e1071::skewness(x, type = 2) 
# 
# #wage
# x <- na.omit(df$wage)
# n <- length(x)
# 
# skew_G1 <- function(v){
#   v <- v[is.finite(v)]
#   n <- length(v); mu <- mean(v); s <- sd(v)
#   (n / ((n - 1) * (n - 2))) * sum((v - mu)^3) / (s^3)
#   
# }
# 
# theta_hat <- skew_G1(x)                  # G1 estimador muestral
# g1_pkg    <- e1071::skewness(x, type = 1)
# G1_pkg    <- e1071::skewness(x, type = 2) 
# 
# # -------- 1. II) Medidas adicionales sobre desigualdad --------
# 
# p90 <- quantile(df$lwage, 0.9, na.rm = TRUE)
# p10 <- quantile(df$lwage, 0.1, na.rm = TRUE)
# ratio_p90_p10 <- p90 / p10
# 
# 
# 
# # ---------- PRUEBA DE HIPÓTESIS COEFICIENTE DE ASIMETRÍA -------
# 
# # -------- 1 f) Cálculo del error estándar con Bootstrap por submuestreo --------
# set.seed(123)
# m <- 400; B <- 2000
# skew_boot <- replicate(B, {
#   idx <- sample.int(n, m, replace = FALSE)
#   skew_G1(x[idx])
# })
# # -------- Test H0: asimetría = 0 (BOOTSTRAP) --------
# se_boot <- sd(skew_boot) * sqrt(m / n)  # SE empírico m-out-of-n
# z_boot  <- theta_hat / se_boot
# p_boot  <- 2 * pnorm(abs(z_boot), lower.tail = FALSE)
# ci_boot <- quantile(skew_boot, c(.025, .975))
# 
# # -------- Test H0: asimetría = 0 (paramétrico) --------
# se_par <-   sqrt(6/n) #sqrt(6 * (n - 2) / ((n + 1) * (n + 3)))
# z_par  <- theta_hat / se_par
# p_par  <- 2 * pnorm(abs(z_par), lower.tail = FALSE)
# 
# # -------- Resultados del test --------
# resultados_test <- data.frame(
#   metodo = c("Paramétrico", "Bootstrap m-out-of-n"),
#   theta  = c(theta_hat, theta_hat),
#   SE     = c(se_par, se_boot),
#   z      = c(z_par, z_boot),
#   pvalor = c(p_par, p_boot)
# )
# print(resultados_test)
# print(ci_boot)
# 
# alpha <- 0.05
# crit  <- qnorm(1 - alpha/2)
# 
# rej_par      <- abs(z_par)  > crit      # o p_par  < alpha
# rej_boot     <- abs(z_boot) > crit      # o p_boot < alpha
# rej_boot_ci  <- !(0 >= ci_boot[1] && 0 <= ci_boot[2])
# 
# c(rej_par=rej_par, rej_boot=rej_boot, rej_boot_ci=rej_boot_ci)
# 
# 
# # -------- Gráfica distribución bootstrap --------
# skew_full <- theta_hat
# ggplot(data.frame(skew = skew_boot), aes(x = skew)) +
#   geom_histogram(aes(y = after_stat(density)), bins = 40, color = "white") +
#   geom_density() +
#   geom_vline(xintercept = skew_full, linetype = "dashed") +
#   labs(x = "Asimetría (G1)", y = "Densidad", title = "Bootstrap por submuestreo de la asimetría")
# 
# data(wage1)
# x <- wage1$wage
# x <- x[is.finite(x)]
# n <- length(x)
# 
# 
# # --- Estimador plug-in ---
# q10 <- as.numeric(quantile(x, 0.10, type = 7, na.rm = TRUE))
# q90 <- as.numeric(quantile(x, 0.90, type = 7, na.rm = TRUE))
# theta_hat <- q90 / q10
# phi_hat   <- log(theta_hat)                 # φ = log θ
# 
# # =========================
# # A) SE "paramétrico" (delta method para cuantiles)
# # Var(q_p) ≈ p(1-p) / [ n * f(q_p)^2 ], con f() densidad en el cuantil
# dens_fun <- with(density(x, n = 2048), approxfun(x, y, rule = 2))
# f10 <- dens_fun(q10); f90 <- dens_fun(q90)
# 
# var_q10 <- 0.10*0.90 / (n * f10^2)
# var_q90 <- 0.90*0.10 / (n * f90^2)
# # Var(log q_p) ≈ Var(q_p)/q_p^2  ;  Var(φ)=suma porque φ=log q90 - log q10
# se_phi_par <- sqrt(var_q90 / q90^2 + var_q10 / q10^2)
# 
# z_par  <- phi_hat / se_phi_par           # H0: φ=0  (↔ θ=1)
# p_par  <- 2 * pnorm(-abs(z_par))
# ci_phi_par <- c(phi_hat - qnorm(.975)*se_phi_par, phi_hat + qnorm(.975)*se_phi_par)
# ci_theta_par <- exp(ci_phi_par)
# 
# # =========================
# # B) Bootstrap m-out-of-n sobre φ (estable y evita colas)
# set.seed(123)
# B <- 2000; m <- floor(0.8*n)             # submuestreo estable
# phi_boot <- replicate(B, {
#   xb <- sample(x, m, replace = FALSE)
#   q10b <- as.numeric(quantile(xb, 0.10, type = 7))
#   q90b <- as.numeric(quantile(xb, 0.90, type = 7))
#   log(q90b/q10b)
# })
# # SE m-out-of-n reescalado a n
# se_phi_boot <- sd(phi_boot) * sqrt(m/n)
# z_boot  <- phi_hat / se_phi_boot
# p_boot  <- 2 * pnorm(-abs(z_boot))
# ci_phi_boot   <- quantile(phi_boot, c(.025, .975))
# ci_theta_boot <- exp(ci_phi_boot)
# 
# # =========================
# # Resultados
# out <- data.frame(
#   metodo  = c("Paramétrico-Delta", "Bootstrap m-out-of-n"),
#   theta   = c(theta_hat, theta_hat),
#   se_phi  = c(se_phi_par, se_phi_boot),
#   z       = c(z_par, z_boot),
#   pvalor  = c(p_par, p_boot),
#   ci_theta_low  = c(ci_theta_par[1], ci_theta_boot[1]),
#   ci_theta_high = c(ci_theta_par[2], ci_theta_boot[2])
# )
# print(out, digits = 4)
# 
# # Decisión al 5%
# alpha <- 0.05
# c(
#   rej_par  = (p_par  < alpha),
#   rej_boot = (p_boot < alpha)
# )


###################################################################################

#----- 2 a) scatterplot salario, educación, ternure

ggplot(df, aes(x = educ, y = wage)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Años de educación", y = "Salario por hora", title = "wage vs educ con línea OLS")

ggplot(df, aes(x = educ, y = lwage)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Años de con el empleador actual", y = "Salario por hora", title = "lwage vs educ con línea OLS")

df <- df %>%
  filter(is.finite(wage), is.finite(educ)) %>%
  mutate(nonwhite_f = factor(nonwhite, levels = c(0,1), labels = c("White","Nonwhite")))

# Scatter + rectas de regresión por grupo (color = nonwhite)

lwage ~ educ*nonwhite

ggplot(df, aes(x = educ, y = wage, color = nonwhite_f)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(x = "Años de educación", y = "Salario por hora",
       color = "Grupo", title = "Salario vs Educación por grupo racial (wage1)") +
  theme_minimal(base_size = 12)

# Opcional: misma figura en log-salario para reducir cola
ggplot(df, aes(x = educ, y = log(wage), color = nonwhite_f)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(x = "Años de educación", y = "log(salario)",
       color = "Grupo", title = "log(Salario) vs Educación por grupo racial (wage1)") +
  theme_minimal(base_size = 12)


ggplot(df, aes(x = exper, y = lwage)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Años de con el empleador actual", y = "Salario por hora", title = "lwage vs exper con línea OLS")


#---- MAESTRÍA ----

# hacer intervalos de educación y añadir el cuadrado para que salgan lineas concavas

 
ggplot(df, aes(exper, lwage)) +
  geom_point(alpha = 0.25, size = 1) +
  geom_smooth(method = "loess", se = TRUE, span = 0.9, linewidth = 1) +
  labs(title = "lwage vs exper con LOESS", x = "Experiencia (años)", y = "log(salario)") +
  theme_minimal(base_size = 12)

mincer <- lm(lwage ~ exper + expersq, data = df)
ggplot(df, aes(exper, lwage)) +
  geom_point(alpha = 0.2, size = 1) +
  geom_smooth(method = "loess", se = FALSE, span = 0.9, linewidth = 1) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, linetype = 2) +
  labs(title = "LOESS vs cuadrático de Mincer",
       x = "Experiencia (años)", y = "log(salario)",
       caption = "Línea continua: LOESS | Línea discontinua: cuadrático") +
  theme_minimal(base_size = 12)


#--------- 2 b)- e) Regresiones ---------------
# arcsinh
df2 <- wage1 %>%
  mutate(
    lwage   = log(wage),
    expersq = exper^2,
    ltenure = log1p(pmax(tenure, 0)),   # ≡ log(tenure + 1); evita perder tenure==0
    # Construir factor de región con "East" como base (cuando no está en las dummies):
    west     = if ("west"     %in% names(.)) as.integer(west)     else 0L,
    south    = if ("south"    %in% names(.)) as.integer(south)    else 0L,
    northcen = if ("northcen" %in% names(.)) as.integer(northcen) else 0L,
    region = case_when(
      west == 1     ~ "West",
      south == 1    ~ "South",
      northcen == 1 ~ "Northcen",
      TRUE          ~ "East"
    ),
    region = factor(region, levels = c("East","Northcen","South","West"))
  ) %>%
  # mantén observaciones válidas en variables usadas
  filter(is.finite(wage), is.finite(educ), is.finite(exper), is.finite(expersq),
         is.finite(lwage), is.finite(ltenure))

# Modelos con VCOV robusto a heterocedasticidad
m1 <- feols(wage  ~ educ + exper + expersq, data = df2, vcov = "hetero")
m2 <- feols(wage  ~ educ + exper + expersq + female + nonwhite + married + ltenure + region,
            data = df2, vcov = "hetero")

m3 <- feols(lwage ~ educ + exper + expersq, data = df2, vcov = "hetero")
m4 <- feols(lwage ~ educ + exper + expersq + female + nonwhite + married,
            data = df2, vcov = "hetero")
m5 <- feols(lwage ~ educ + exper + expersq + female + nonwhite + married + ltenure + region,
            data = df2, vcov = "hetero")

summary(m1); summary(m2); summary(m3); summary(m4); summary(m5)

contrasts(df2$region) <- contr.sum(nlevels(df2$region))

m_dev <- lm(lwage ~ educ + exper + expersq + female + nonwhite + married + ltenure + region, data=df2,
            contrasts = list(region = contr.sum))
summary(m_dev)



# 2 II) Tabla
msummary(
  list(M1=m1, M2=m2, M3=m3, M4=m4, M5=m5),
  estimate = "{estimate}{stars}",
  stars = c('***' = 0.03, '**' = 0.07, '*' = 0.13),
  gof_omit = "IC|Log|Within|FE|F",
  notes = "Note: *** p<0.03; ** p<0.07; * p<0.13"
)

msummary(
  list(M1=m1, M2=m2, M3=m3, M4=m4, M5=m5),
  estimate = "{estimate}{stars}",
  stars = c('***' = 0.03, '**' = 0.07, '*' = 0.13),
  gof_omit = "IC|Log|Within|FE|F",
  notes = "Note: *** p<0.03; ** p<0.07; * p<0.13",
  fmt = 3,
  output = "latex"
)




# IC 

# Asume que ya tienes m2 estimado como:
# m2 <- feols(wage ~ educ + exper + expersq + female + nonwhite + married + I(log(tenure)) + region,
#             data = df, vcov = "hetero")

# 1) Experiencia potencial
exper_pot <- 45 - 16 - 6  # = 23

# 2) Nueva observación (asegura niveles de 'region' compatibles)
new <- data.frame(
  educ     = 16,
  exper    = exper_pot,
  expersq  = exper_pot^2,
  female   = 1,
  nonwhite = 0,
  married  = 1,
  ltenure   = log(5),
  region   = factor("South", levels = levels(df2$region))
)

# 3) Predicción puntual e IC del 95% con VCOV hetero
pred <- predict(m2, newdata = new, se.fit = TRUE,
                interval = "confidence", level = 0.95, vcov = "hetero")
pred


######################################################################################

m1 <- lm(wage ~ educ + exper + expersq, data = df2)
m2 <- lm(wage ~ educ + exper + expersq + female + nonwhite + married + ltenure + region, data = df2)
m3 <- lm(lwage ~ educ + exper + expersq, data = df2)
m4 <- lm(lwage ~  educ + exper + expersq + female + nonwhite + married, data = df2)
m5 <- lm(lwage ~ educ + exper + expersq + female + nonwhite + married + ltenure + region,
         data = df2)

# === Errores estándar robustos HC1 (vectores para stargazer) ===
rse1 <- sqrt(diag(vcovHC(m1, type = "HC1")))
rse2 <- sqrt(diag(vcovHC(m2, type = "HC1")))
rse3 <- sqrt(diag(vcovHC(m3, type = "HC1")))
rse4 <- sqrt(diag(vcovHC(m4, type = "HC1")))
rse5 <- sqrt(diag(vcovHC(m5, type = "HC1")))

# === Ejemplo tipo: summ(reg3.e, robust="HC1", digits=4) ===
summ(m1, robust = "HC1", digits = 4)
summ(m2, robust = "HC1", digits = 4)
summ(m3, robust = "HC1", digits = 4)
summ(m4, robust = "HC1", digits = 4)
summ(m5, robust = "HC1", digits = 4)

stargazer(m1, m2, m3, m4, m5, type = "latex",
          se = list(rse1, rse2, rse3, rse4, rse5),
          title = "Wage1: LM con errores robustos HC1",
          label = "tab:reg_all_hc1",
          dep.var.labels = c("log(salario)"),
          star.cutoffs = c(0.13, 0.07, 0.03),
          float.env = "table",
          align = TRUE)
