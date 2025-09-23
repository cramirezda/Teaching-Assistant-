rm(list=ls())

paquetes <- c('woolridge','dplyr','ggplot2','fixest','modelsummary','knitr','tidyr','e1071','ineq')
sapply(paquetes, function(p) if (!require(p, character.only = TRUE)) install.packages(p, dependencies = TRUE))

library(dplyr); library(ggplot2); library(knitr); library(tidyr)
library(wooldridge); library(fixest);library(modelsummary)



data(wage1)
df <- wage1

# -------- 1 a) Resumen: media, mediana, max, min, sd --------
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
    sd_exper = sd(exper, na.rm = TRUE)
  ) %>% 
  pivot_longer(cols = everything(),
               names_to = c("estadistico", "variable"),
               names_sep = "_") %>% 
  pivot_wider(names_from = variable, values_from = value)

# 1 b) añadir error estándar de la media

n_wage  <- sum(!is.na(df$wage))
n_educ  <- sum(!is.na(df$educ))
n_exper <- sum(!is.na(df$exper))

se_row <- tibble::tibble(
  estadistico = "se_media",
  wage  = sd(df$wage,  na.rm = TRUE) / sqrt(n_wage),
  educ  = sd(df$educ,  na.rm = TRUE) / sqrt(n_educ),
  exper = sd(df$exper, na.rm = TRUE) / sqrt(n_exper)
)

tabla <- bind_rows(tabla, se_row) %>%
  mutate(estadistico = factor(estadistico, levels = c("media","mediana","max","min","sd","se_media"))) %>%
  arrange(estadistico)

kable(tabla, digits = 2, caption = "Resumen estadístico de wage, educ y exper")

# 1 c) Distribución de salarios y lwage
ggplot(df, aes(x = wage)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "white") +
  geom_density() +
  labs(x = "Valor", y = "Densidad", title = "Histograma + densidad")

ggplot(df, aes(x = lwage)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "white") +
  geom_density() +
  labs(x = "Valor", y = "Densidad", title = "Histograma + densidad")

# 1 d) gráfica proporción de individuos por región
df2 <- df %>%
  mutate(suma = northcen + south + west,
         region = case_when(
           suma == 0 ~ "East",
           northcen == 1 & suma == 1 ~ "North Central",
           south    == 1 & suma == 1 ~ "South",
           west     == 1 & suma == 1 ~ "West",
           TRUE ~ NA_character_
         ))

prop_reg <- df2 %>%
  filter(!is.na(region)) %>%
  count(region, name = "n") %>%
  mutate(prop = n / sum(n),
         etiqueta = sprintf("%.1f%%", 100*prop))

ggplot(prop_reg, aes(x = "", y = prop, fill = region)) +
  geom_col(width = 1) + coord_polar(theta = "y") +
  geom_text(aes(label = etiqueta), position = position_stack(vjust = 0.5), color = "white") +
  labs(x = NULL, y = NULL, fill = "Región", title = "Proporción por región") +
  theme_void()


# --------  1 e) Calcular el coeficiente de asimetría --------

#lwage
x <- na.omit(df$lwage)
n <- length(x)

skew_G1 <- function(v){
  v <- v[is.finite(v)]
  n <- length(v); mu <- mean(v); s <- sd(v)
  (n / ((n - 1) * (n - 2))) * sum((v - mu)^3) / (s^3)

}

theta_hat <- skew_G1(x)                  # G1 estimador muestral
g1_pkg    <- e1071::skewness(x, type = 1)
G1_pkg    <- e1071::skewness(x, type = 2) 

#wage
x <- na.omit(df$wage)
n <- length(x)

skew_G1 <- function(v){
  v <- v[is.finite(v)]
  n <- length(v); mu <- mean(v); s <- sd(v)
  (n / ((n - 1) * (n - 2))) * sum((v - mu)^3) / (s^3)
  
}

theta_hat <- skew_G1(x)                  # G1 estimador muestral
g1_pkg    <- e1071::skewness(x, type = 1)
G1_pkg    <- e1071::skewness(x, type = 2) 

# -------- 1. II) Medidas adicionales sobre desigualdad --------

p90 <- quantile(df$lwage, 0.9, na.rm = TRUE)
p10 <- quantile(df$lwage, 0.1, na.rm = TRUE)
ratio_p90_p10 <- p90 / p10

gini_lwage <- ineq::ineq(df$lwage, type = "Gini")
mad_lwage  <- mad(df$lwage, constant = 1, na.rm = TRUE)


# ---------- PRUEBA DE HIPÓTESIS COEFICIENTE DE ASIMETRÍA -------

# -------- 1 f) Cálculo del error estándar con Bootstrap por submuestreo --------
set.seed(123)
m <- 400; B <- 2000
skew_boot <- replicate(B, {
  idx <- sample.int(n, m, replace = FALSE)
  skew_G1(x[idx])
})
# -------- Test H0: asimetría = 0 (BOOTSTRAP) --------
se_boot <- sd(skew_boot) * sqrt(m / n)  # SE empírico m-out-of-n
z_boot  <- theta_hat / se_boot
p_boot  <- 2 * pnorm(abs(z_boot), lower.tail = FALSE)
ci_boot <- quantile(skew_boot, c(.025, .975))

# -------- Test H0: asimetría = 0 (paramétrico) --------
se_par <-   sqrt(6/n) #sqrt(6 * (n - 2) / ((n + 1) * (n + 3)))
z_par  <- theta_hat / se_par
p_par  <- 2 * pnorm(abs(z_par), lower.tail = FALSE)

# -------- Resultados del test --------
resultados_test <- data.frame(
  metodo = c("Paramétrico", "Bootstrap m-out-of-n"),
  theta  = c(theta_hat, theta_hat),
  SE     = c(se_par, se_boot),
  z      = c(z_par, z_boot),
  pvalor = c(p_par, p_boot)
)
print(resultados_test)
print(ci_boot)

alpha <- 0.05
crit  <- qnorm(1 - alpha/2)

rej_par      <- abs(z_par)  > crit      # o p_par  < alpha
rej_boot     <- abs(z_boot) > crit      # o p_boot < alpha
rej_boot_ci  <- !(0 >= ci_boot[1] && 0 <= ci_boot[2])

c(rej_par=rej_par, rej_boot=rej_boot, rej_boot_ci=rej_boot_ci)


# -------- Gráfica distribución bootstrap --------
skew_full <- theta_hat
ggplot(data.frame(skew = skew_boot), aes(x = skew)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40, color = "white") +
  geom_density() +
  geom_vline(xintercept = skew_full, linetype = "dashed") +
  labs(x = "Asimetría (G1)", y = "Densidad", title = "Bootstrap por submuestreo de la asimetría")


###################################################################################

#----- 2 a) scatterplot salario, educación, ternure

ggplot(df, aes(x = educ, y = wage)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Años de educación", y = "Salario por hora", title = "wage vs educ con línea OLS")

ggplot(df, aes(x = tenure, y = wage)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Años de con el empleador actual", y = "Salario por hora", title = "wage vs educ con línea OLS")

#--------- 2 b)- e) Regresiones ---------------

# Modelos (usa I(exper^2))
m1 <- feols(lwage ~ educ + exper, data = df, vcov = "hetero")
summary(m1)
m2 <- feols(lwage ~ educ + exper + I(exper^2), data = df, vcov = "hetero")
summary(m2)
m3 <- feols(lwage ~ exper + educ:female, data = df, vcov = "hetero")
summary(m3)
m4 <- feols(lwage ~ exper*female + educ*female, data = df, vcov = "hetero")
summary(m4)
m5 <- feols(lwage ~ educ + exper + I(exper^2) + tenure + female + nonwhite + married,
            data = df, vcov = "hetero")
summary(m5)

# 2 II) Tabla
msummary(
  list(M1=m1, M2=m2, M3=m3, M4=m4, M5=m5),
  estimate = "{estimate}{stars} ({std.error})",
  stars = TRUE,
  gof_omit = "IC|Log|Within|FE|F"
)

