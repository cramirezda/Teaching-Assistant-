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

kable(tabla, digits = 2, caption = "Resumen estadístico")

# 1 Distribución de salarios y lwage

theme_clean <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.background = element_rect(fill = "white", color = NA)
  )

# 1. Distribución del salario (wage)
ggplot(df, aes(x = wage)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30, fill = "#0072B2", color = "white", alpha = 0.8) +
  geom_density(color = "black", linewidth = 1) +
  labs(
    title = "Distribución del salario (wage)",
    x = "Salario por hora",
    y = "Densidad"
  ) +
  theme_clean

# 2. Distribución del logaritmo del salario (log(wage))
ggplot(df, aes(x = lwage)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30, fill = "#0072B2", color = "white", alpha = 0.8) +
  geom_density(color = "black", linewidth = 1) +
  labs(
    title = "Distribución del logaritmo del salario (log(wage))",
    x = "Logaritmo del salario por hora",
    y = "Densidad"
  ) +
  theme_clean


# distribuciones por region
ggplot(df, aes(lwage)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,fill = "#0072B2", color = "white") +
  geom_density() +
  facet_wrap(~ region, ncol = 2, scales = "free_y") +
  labs(title = "Distribución de log-salario por región (wage1)",
       x = "lwage = log(wage)", y = "Densidad") +
  theme_clean

ggplot(df, aes(wage)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,fill = "#0072B2", color = "white") +
  geom_density() +
  facet_wrap(~ region, ncol = 2, scales = "free_y") +
  labs(title = "Distribución de salarios por región",
       x = "wage (por hora)", y = "Densidad") +
  theme_clean

# PRUEBA DE HIPÓTESIS DIFERENCIA DE MEDIAS

# --- Datos
S <- df %>% filter(region == "South") %>% pull(wage)
W <- df %>% filter(region == "West")  %>% pull(wage)

# --- Resúmenes
nS <- length(S); nW <- length(W)
mS <- mean(S);   mW <- mean(W)
vS <- var(S);    vW <- var(W)
diff_hat <- mS - mW

# --- Error estándar (no agrupado)
se_diff <- sqrt(vS/nS + vW/nW)

# --- 1) t-test de Welch
tt_welch <- t.test(S, W, var.equal = FALSE, alternative = "two.sided", conf.level = 0.95)

# --- 2) Z-test asintótico (Wald)
Z_stat <- diff_hat / se_diff
p_z    <- 2 * (1 - pnorm(abs(Z_stat)))
ci_z   <- diff_hat + c(-1, 1) * qnorm(0.975) * se_diff

# --- Salida comparativa
out <- data.frame(
  metodo        = c("t_Welch", "Z_asintotico"),
  diff_hat      = c(diff_hat, diff_hat),
  se_diff       = c(se_diff,  se_diff),
  estadistico   = c(unname(tt_welch$statistic), Z_stat),
  gl_o_ref      = c(unname(tt_welch$parameter), NA),  # df para t; NA para Z
  p_value       = c(tt_welch$p.value, p_z),
  ci_low_95     = c(tt_welch$conf.int[1], ci_z[1]),
  ci_high_95    = c(tt_welch$conf.int[2], ci_z[2])
)

print(out, digits = 5)

t_obs <- (mS - mW) / sqrt(vS/nS + vW/nW)

# Bootstrap pooled
B <- 500
pool <- c(S, W)

t_boot <- replicate(B, {
  xb <- sample(pool, size = nS + nW, replace = TRUE)
  Sb <- xb[seq_len(nS)]
  Wb <- xb[(nS+1):(nS+nW)]
  mSb <- mean(Sb); mWb <- mean(Wb)
  vSb <- var(Sb);  vWb <- var(Wb)
  se_b <- sqrt(vSb/length(Sb) + vWb/length(Wb))   # Welch en cada réplica
  (mSb - mWb) / se_b
})

# p-valor bootstrap bilateral
p_boot <- mean(abs(t_boot) >= abs(t_obs))
c(t_obs = t_obs, p_boot = p_boot)

# Gráfica de la distribución bootstrap del estadístico
ggplot(data.frame(t_boot = t_boot), aes(x = t_boot)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "white") +
  geom_density(linewidth = 1) +
  geom_vline(xintercept = t_obs, linetype = "dashed") +
  labs(title = "Bootstrap (pooled) del estadístico de Welch",
       subtitle = paste0("B = ", B, ", p_boot = ", signif(p_boot, 3)),
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
  geom_histogram(aes(y = after_stat(density)), bins = 30,fill='blue', color = "white") +
  geom_density() +
  geom_vline(xintercept = D_obs, linetype = "dashed") +
  labs(title = "Bootstrap del estadístico K-S: West vs North Central",
       x = "D_boot", y = "Densidad")+
  theme_clean

###################################################################################

#----- 2 a) scatterplot salario, educación, ternure

ggplot(df, aes(x = educ, y = wage)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Años de educación", y = "Salario por hora", title = "wage vs educ con línea OLS")+
  theme_clean

ggplot(df, aes(x = educ, y = lwage)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Años de con el empleador actual", y = "Salario por hora", title = "lwage vs educ con línea OLS")+
  theme_clean

df <- df %>%
  filter(is.finite(wage), is.finite(educ)) %>%
  mutate(nonwhite_f = factor(nonwhite, levels = c(0,1), labels = c("White","Nonwhite")))

# Scatter + rectas de regresión por grupo (color = nonwhite)

#lwage ~ educ*nonwhite

#Opción wage
ggplot(df, aes(x = educ, y = wage, color = factor(nonwhite))) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(x = "Años de educación", y = "Salario por hora",
       color = "Grupo", title = "Salario vs Educación por grupo racial (wage1)") +
  theme_minimal(base_size = 12)

# Opción lwage
ggplot(df, aes(x = educ, y = lwage, color = factor(nonwhite))) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(x = "Años de educación", y = "log(salario)",
       color = "Grupo", title = "lwage vs educ por nonwhite") +
  theme_clean

#------ Exper y lwage ----
ggplot(df, aes(x = exper, y = lwage)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Años de experiencia", y = "log(wage)", title = "lwage vs exper")+
  theme_clean

#---- MAESTRÍA ----

#Opción 1
ggplot(df, aes(exper, lwage)) +
  geom_point(alpha = 0.25, size = 1) +
  geom_smooth(method = "loess", se = TRUE, span = 0.9, linewidth = 1) +
  labs(title = "lwage vs exper con LOESS", x = "Experiencia (años)", y = "log(salario)") +
  theme_minimal(base_size = 12)

#Opción 2
df$metodo <- "LOESS"

ggplot(df, aes(x = exper, y = lwage)) +
  geom_point(alpha = 0.2, size = 1, color = "gray50") +
  geom_smooth(aes(color = "LOESS"), method = "loess", se = FALSE, span = 0.9, linewidth = 1) +
  stat_smooth(aes(color = "LM"),
              method = "lm", formula = y ~ x + I(x^2),
              se = FALSE, linetype = 2, linewidth = 1) +
  labs(title = "Relación entre log(salario) y experiencia",
       x = "Experiencia (años)",
       y = "log(salario)",
       color = "Método") +
  scale_color_manual(values = c("LOESS" = "blue", "LM" = "red")) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

#--------- 2 b)- e) Regresiones ---------------
# arcsinh
df2 <- wage1 %>%
  mutate(
    lwage   = lwage,
    expersq = expersq,
    ltenure = asinh(tenure),   # ≡ log(tenure + 1); evita perder tenure==0
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


# 2 II) Tabla
msummary(
  list(M1=m1, M2=m2, M3=m3, M4=m4, M5=m5),
  estimate = "{estimate}{stars}",
  stars = c('***' = 0.03, '**' = 0.07, '*' = 0.13),
  gof_omit = "IC|Log|Within|FE|F",
  notes = "Note: *** p<0.03; ** p<0.07; * p<0.13"
)

#--- consistencia con lm y stargazer
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

# ejercicio intervalo de confianza

# Construir nuevo caso
p <- data.frame(
  educ = 16,
  exper = 23,
  expersq = 23^2,
  female = 1,
  nonwhite = 0,
  married = 1,
  ltenure = asinh(5),
  region = factor("South", levels = c("East","Northcen","South","West"))
)

# Intervalo de confianza 95% para el salario esperado
pred <- predict(m2, newdata = p, interval = "confidence", level = 0.95)
pred

####################################################################################
# m_ind_1 <- feols(lwage ~ educ*construc + educ*ndurman + educ*trade +educ*services+educ*profserv + female +married+nonwhite, data=df2, vcov='hetero')
# summary(m_ind)
# m_ind_2 <- feols(lwage ~ educ*profocc + educ*clerocc + educ*trade +educ*servocc+educ*trcommpu + female +married+nonwhite, data=df2, vcov='hetero')
# summary(m_ind)


df3 <- df2 %>%
  mutate(
    industry = case_when(
      construc == 1 ~ "construc",
      ndurman  == 1 ~ "ndurman",
      trade    == 1 ~ "trade",
      services == 1 ~ "services",
      TRUE          ~ "other"
    ),
    industry = factor(industry, levels = c("other","construc","ndurman","trade","services"))
  )

m_ind_A <- feols(lwage ~ educ*industry + female + married + nonwhite, data=df3, vcov="hetero")
summary(m_ind_A)

# 1) Extrae coeficientes y var-cov
b <- coef(m_ind_A)
V <- vcov(m_ind_A)
z <- qnorm(0.95)  # IC 90%

inds <- levels(df3$industry)  # c("other","construc","ndurman","trade","services")

mk_lincomb <- function(ind){
  cvec <- rep(0, length(b)); names(cvec) <- names(b)
  # pendiente en la industria 'ind' = beta_educ + beta_(educ:industry=ind)
  stopifnot("educ" %in% names(b))
  cvec["educ"] <- 1
  int_name <- paste0("educ:industry", ind)
  if (int_name %in% names(b)) cvec[int_name] <- 1 
  est <- sum(cvec * b)
  se  <- sqrt(as.numeric(t(cvec) %*% V %*% cvec))
  tibble(industry = ind, est = est, lwr = est - z*se, upr = est + z*se)
}

slopes <- bind_rows(lapply(inds, mk_lincomb)) %>%
  mutate(industry = factor(industry, levels = inds))

# 2) OPCION 1 Gráfica de barras con IC 90%
ggplot(slopes, aes(x = industry, y = est)) +
  geom_col(fill = "#0072B2", alpha = 0.8, width = 0.65) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2, linewidth = 0.7) +
  labs(title = "Retorno de la educación por industria (IC 90%)",
       x = "Industria",
       y = "Pendiente de educ sobre ln(wage)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())


# OPCION 2 

# Grid para predicción: educ en rango observado, controles en medias
educ_seq <- seq(min(df3$educ, na.rm=TRUE), max(df3$educ, na.rm=TRUE), length.out = 50)
ctrl_means_A <- df3 %>%
  summarise(
    female = mean(female, na.rm=TRUE),
    married= mean(married, na.rm=TRUE),
    nonwhite=mean(nonwhite, na.rm=TRUE)
  )

newdat_A <- tidyr::crossing(
  industry = levels(df3$industry),
  educ = educ_seq
) %>%
  bind_cols(ctrl_means_A[rep(1, nrow(.)), ])

# Predicciones y banda de confianza 90%
pred_A <- predict(m_ind_A, newdata=newdat_A, se.fit=TRUE, interval="confidence", level=0.90)
pred_A <- cbind(newdat_A, as.data.frame(pred_A))

pred_raw <- predict(m_ind_A, newdata = newdat_A, se.fit = TRUE)  # fixest
z <- qnorm(0.95)

pred_A <- newdat_A %>%
  mutate(
    fit = as.numeric(pred_raw$fit),
    se  = as.numeric(pred_raw$se.fit),
    lwr = fit - z * se,
    upr = fit + z * se
  )

# Gráfica
pA <- ggplot(df3, aes(x = educ, y = lwage, color = industry)) +
  geom_point(alpha = 0.25, size = 1) +
  geom_ribbon(data = pred_A,
              aes(x = educ, ymin = lwr, ymax = upr, fill = industry),
              inherit.aes = FALSE, alpha = 0.20) +
  geom_line(data = pred_A,
            aes(x = educ, y = fit),
            inherit.aes = FALSE, linewidth = 1) +
  facet_wrap(~ industry, ncol = 3, scales = "fixed") +
  labs(title = "Rendimiento de la educación por industria (IC 90%)",
       x = "Años de educación", y = "log(salario)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())
print(pA)


# PRUEBA DE HIPOTESIS CON WALD

wald(m_ind_A, keep = "educ:")

##################################################################################

df4 <- df2 %>%
  mutate(
    educ_cat = case_when(
      !is.finite(educ)           ~ NA_character_,
      educ <= 4                  ~ "none",
      educ >= 5  & educ < 8      ~ "elementary",
      educ >= 8  & educ < 12     ~ "middle",
      educ >= 12 & educ < 16     ~ "high_school",
      educ >= 16                 ~ "higher"
    ),
    educ_cat = factor(
      educ_cat,
      levels = c("none","elementary","middle","high_school","higher"),
      ordered = TRUE
    )
  )

dums <- as.data.frame(model.matrix(~ educ_cat - 1, data = df4))
names(dums) <- sub("^educ_cat", "", names(dums))
df4 <- bind_cols(df4, dums)

m5_cat <- feols(
  lwage ~ i(educ_cat, ref = "none") + exper + expersq +
    female + nonwhite + married + ltenure + region,
  data = df4, vcov = "hetero"
)
summary(m5_cat)

msummary(
  list(
    "M5: Años de educación" = m5,
    "M5_cat: Nivel educativo (dummies)" = m5_cat
  ),
  estimate = "{estimate}{stars}",
  stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
  gof_omit = "IC|Log|Within|FE|F",
  title = "Comparación: años de educación vs. niveles educativos",
  notes = "Note: *** p<0.01; ** p<0.05; * p<0.1",
  output = 'latex'
)

# Extraer coeficientes con confint
b1 <- broom::tidy(m5, conf.int=TRUE, conf.level=0.90) %>% filter(term=="educ")
b2 <- broom::tidy(m5_cat, conf.int=TRUE, conf.level=0.90) %>%
  filter(grepl("educ_cat", term))

comp <- bind_rows(
  b1 %>% mutate(model="Educ continua", level="educ"),
  b2 %>% mutate(model="Educ categórica",
                level=gsub("i\\(educ_cat, ref = \".*?\"\\)","",term))
)

ggplot(comp, aes(x=level, y=estimate, fill=model)) +
  geom_col(position="dodge") +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high),
                position=position_dodge(0.9), width=0.2) +
  labs(title="Comparación de retornos a la educación",
       x="Nivel educativo", y="Coeficiente estimado (log-salario)") +
  theme_minimal()


