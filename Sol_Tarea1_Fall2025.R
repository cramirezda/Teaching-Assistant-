# Tarea 1
# Script creado por: Carlos Dávila
# Revisado y editado por: Arturo Aguilar
# Editado con el apoyo de ChatGPT para simplificar
# parte de los códigos

rm(list=ls())

#Funcion para cargar/instalar los paquetes que se utilizaran

install <- function(packages){
  new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new.packages))
    install.packages(new.packages, dependencies = TRUE)
  sapply(packages, require, character.only = TRUE)
}
required.packages <- c('dplyr','ggplot2','fixest','modelsummary','readr',
                       'knitr','tidyr','e1071','ineq','lmtest','sandwich',
                       'jtools','stargazer','tibble','jtools','car')

install(required.packages)

dataT1 <- read_csv("mincer.csv")

# ====/// 1a: Estadistica descriptiva \\\=====
data_desc <- dataT1 %>% select("wage","educ","exper","married","nonwhite","numdep")

# Compute summary statistics including median
summary_1a <- data_desc %>%
  summarise(across(everything(),
                   list(
                     N = ~sum(!is.na(.)),
                     Mean = ~mean(., na.rm = TRUE),
                     Median = ~median(., na.rm = TRUE),
                     SD = ~sd(., na.rm = TRUE),
                     Min = ~min(., na.rm = TRUE),
                     Max = ~max(., na.rm = TRUE)
                     )))

# Reshape to have variables as rows, stats as columns
stats_df <- summary_1a %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column("Variable") %>%
  separate(Variable, into = c("Variable", "Statistic"), sep = "_") %>%
  pivot_wider(names_from = Statistic, values_from = V1) %>%
  mutate(across(-Variable, as.numeric))  # ensure numeric for rounding

# Order and round
stats_df <- stats_df %>%
  select(Variable, N, Mean, Median, SD, Min, Max) %>%
  mutate(across(-Variable, ~round(., 2)))

# Convert to matrix for stargazer
stargazer(stats_df, summary = FALSE, digits = 2, type = "latex",
          title = "Descriptive Statistics",
          out = "Table_1a.tex")


# ====/// 1b: Distribuciones \\\=====

# Esta linea es solo para mejorar el formato, no es necesaria
theme_clean <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Distribución del salario (wage)
ggplot(dataT1, aes(x = wage)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30, fill = "#0072B2", color = "white", alpha = 0.8) +
  geom_density(color = "black", linewidth = 1) +
  labs(
    title = "Distribución del salario (wage)",
    x = "Salario por hora",
    y = "Densidad"
  ) +
  theme_clean

ggsave("Histograma_1b1.png",  width = 5.54, height = 4.95)

# Distribución del logaritmo del salario (log(wage))
ggplot(dataT1, aes(x = log(wage))) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30, fill = "#0072B2", color = "white", alpha = 0.8) +
  geom_density(color = "black", linewidth = 1) +
  labs(
    title = "Distribución del logaritmo del salario (log(wage))",
    x = "Logaritmo del salario por hora",
    y = "Densidad"
  ) +
  theme_clean

ggsave("Histograma_1b2.png",  width = 5.54, height = 4.95)


# ====/// 1c: Distribuciones por region \\\=====

# Creo una variable de factor por region
dataT1 <- dataT1 %>% 
  mutate(region = factor(1+1*northcen+2*south+3*west, labels = c("east","northcen","south","west")))

ggplot(dataT1, aes(x=wage)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,fill = "#0072B2", color = "white") +
  geom_density(color = "black", linewidth = 1) +
  facet_wrap(~ region, ncol = 2, scales = "free_y") +
  labs(x = "wage (por hora)", y = "Densidad") +
  theme_clean

ggsave("Histograma_1c.png",  width = 5.54, height = 4.95)


# ====/// 1d: Diferencia de medias \\\=====

datat1_region <- dataT1 %>% filter(south==1 | west==1) %>%
  group_by(region) %>%
  summarise(Estimate = mean(wage, na.rm = TRUE),
            Var = var(wage, na.rm = TRUE),
            n = n())

stargazer(datat1_region, digits=2 ,summary = FALSE, 
          title = "Salario por Region",
          out = "tabla_region.tex")

t_stat <- dataT1 %>% filter(south==1 | west==1) %>%
  summarise(Neyman= mean(wage[west==1], na.rm = T)-mean(wage[south==1], na.rm = T),
            SD = sqrt(var(wage[west==1], na.rm = T)/sum(west == 1)+var(wage[south==1], na.rm = T)/sum(south== 1)),
            p_value = 2*pnorm(abs(Neyman / SD), 0, 1, lower.tail = F))
(t <- t_stat$Neyman/t_stat$SD)

# ====/// 1e: Bootstrap, Licenciatura \\\=====

# Bootstrap pooled
B <- 500
data_boot <- dataT1 %>% filter(south==1 | west==1)
n_boot <- length(data_boot$wage)

estat_boot <- c()
muestra <- c()

for (n in 1:B) {
  muestra <- sample_n(data_boot,size = n_boot, replace = TRUE)
  dif_mean <- mean(muestra$wage[muestra$west==1], na.rm = T)-
              mean(muestra$wage[muestra$south==1], na.rm = T)
  estat_boot <- c(estat_boot,dif_mean)
  muestra <- c()
}

estat_boot <- as.data.frame(estat_boot)

# Estadisticos
(mean_boot <- mean(estat_boot$estat_boot))
sd_boot <- sqrt(var(estat_boot$estat_boot))
(p_boot = 2*pnorm(abs(mean_boot / sd_boot), 0, 1, lower.tail = F))

# Histograma
ggplot(estat_boot, aes(x = estat_boot)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "white") +
  geom_density(linewidth = 1) +
  labs(subtitle = paste0("B = ", B, ", p_boot = ", p_boot),
       x = "Diferencia de medias", y = "Densidad")

ggsave("Histograma_1e.png",  width = 5.54, height = 4.95)


# ====/// 1e: Bootstrap, Maestria \\\=====

# Empezamos por calcular el KS observado con la
# muestra original

data_west <- dataT1 %>% filter(west==1) 
data_north <- dataT1 %>% filter(northcen==1) 
(ks_obs <- ks.test(data_west$wage, data_north$wage)$statistic)

# Simulaciones donde west y northcen se asignan al azar
# por lo tanto no habría diferencia de distribuciones

# Pool both samples (to simulate under H0)
combined <- dataT1 %>% filter(west==1 | northcen==1)
n_w <- sum(combined$west)
n_nc <- sum(combined$northcen)
ks_boot <- c()

# Bootstrap loop
for (n in 1:B) {
  muestra <- sample_n(combined,size = n_w+n_nc, replace = TRUE)
  x_boot <- muestra[1:n_w,]
  y_boot <- muestra[(n_w + 1):(n_w + n_nc),]
  ks_boot <- c(ks_boot,ks.test(x_boot$wage, y_boot$wage)$statistic)
  muestra <- c()
}


# Empirical mean and variance of K statistic
mn_emp <- mean(ks_boot)
var_emp <- var(ks_boot)

# Empirical p-value: fraction of bootstrap statistics >= observed
p_boot <- mean(ks_boot >= ks_obs)

cat("Observed KS statistic:", ks_obs, "\n")
cat("Empirical variance:", var_emp, "\n")
cat("Bootstrap p-value:", p_boot, "\n")


# --- Gráfica distribución bootstrap del estadístico K-S ---
ggplot(data.frame(ks_boot), aes(x=ks_boot)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,fill='blue', color = "white") +
  geom_vline(xintercept = ks_obs, linetype = "dashed") +
  labs(title = "Bootstrap del estadístico K-S: West vs North Central",
       x = "KS_boot", y = "Densidad")+
  theme_clean

ggsave("Histograma_1e_Maest.png",  width = 5.54, height = 4.95)


###################################################################################

# ====/// 2a,b: scatterplot salario, educación \\\=====


ggplot(dataT1, aes(x = educ, y = wage)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Años de educación", y = "Salario por hora", title = "wage vs educ con línea OLS")+
  theme_clean

ggsave("Scatter_2a.png",  width = 5.54, height = 4.95)

ggplot(dataT1, aes(x = educ, y = log(wage))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Años de con el empleador actual", y = "Salario por hora", title = "lwage vs educ con línea OLS")+
  theme_clean

ggsave("Scatter_2b.png",  width = 5.54, height = 4.95)

# ====/// 2c: scatterplot con interaccion \\\=====

dataT1 <- dataT1 %>%
    mutate(nonwhite_f = factor(nonwhite, levels = c(0,1), 
                               labels = c("White","Nonwhite")),
           nonw_educ = nonwhite*educ)

reg_int <- lm(log(wage)~educ+nonwhite+nonw_educ,data=dataT1)
summ(reg_int,robust = "HC1",digits=4)

ggplot(dataT1, aes(x = educ, y = log(wage), color = nonwhite_f)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(x = "Años de educación", y = "log(salario)",
       color = "Grupo", title = "lwage vs educ por nonwhite") +
  theme_clean

ggsave("Scatter_2c.png",  width = 5.54, height = 4.95)

# ====/// 2d: Relacion log(wage) y exper \\\=====

dataT1_avg <- dataT1 %>% 
  mutate(exp_r=round(exper/5,digits=0)*5) %>%
  group_by(exp_r) %>%
  summarize(log_w=mean(log(wage)))
  
ggplot() +
  geom_point(aes(x=exp_r,y=log_w),data=dataT1_avg) +
  geom_smooth(aes(x=exper,y=log(wage)),data=dataT1,method = "lm") +
  theme_clean

ggsave("Scatter_2d_Lic_a.png",  width = 5.54, height = 4.95)

ggplot() +
  geom_point(aes(x=exp_r,y=log_w),data=dataT1_avg) +
  geom_smooth(aes(x=exper,y=log(wage)),data=dataT1,method = "lm",formula=y~poly(x,2)) + 
  theme_clean

ggsave("Scatter_2d_Lic_b.png",  width = 5.54, height = 4.95)

ggplot() +
  geom_point(aes(x=exp_r,y=log_w),data=dataT1_avg) +
  geom_smooth(aes(x=exper,y=log(wage)),data=dataT1,method = "loess",span=0.75) + 
  theme_clean

ggsave("Scatter_2d_Maest.png",  width = 5.54, height = 4.95)

# ====/// 2e: Regresiones \\\=====

dataT1 <- dataT1 %>%
  mutate(exper2 = exper^2)

m1 <- lm(wage ~ educ + exper + exper2, data = dataT1)
m2 <- lm(wage ~ educ + exper + exper2 + female + nonwhite + married + asinh(tenure) + factor(region), data = dataT1)
m3 <- lm(lwage ~ educ + exper + exper2, data = dataT1)
m4 <- lm(lwage ~  educ + exper + exper2 + female + nonwhite + married, data = dataT1)
m5 <- lm(lwage ~ educ + exper + exper2 + female + nonwhite + married + asinh(tenure) + factor(region), data = dataT1)

# === Errores estándar robustos HC1 (vectores para stargazer) ===
rse1 <- sqrt(diag(vcovHC(m1, type = "HC1")))
rse2 <- sqrt(diag(vcovHC(m2, type = "HC1")))
rse3 <- sqrt(diag(vcovHC(m3, type = "HC1")))
rse4 <- sqrt(diag(vcovHC(m4, type = "HC1")))
rse5 <- sqrt(diag(vcovHC(m5, type = "HC1")))

stargazer(m1, m2, m3, m4, m5, type = "latex",
          se = list(rse1, rse2, rse3, rse4, rse5),
          title = "Wage1: LM con errores robustos HC1",
          label = "tab:reg_2e",
          dep.var.labels = c("salario","log(salario)"),
          star.cutoffs = c(0.13, 0.07, 0.03),
          float.env = "table",
          align = TRUE,
          out = "table_ols_2e.tex")

# ====/// 2g: Prediccion \\\=====

# Construir caso hipot\'etico
lg <- c(1,16,23,23^2,1,0,1,asinh(5),0,1,0)
lg_test <- linearHypothesis(m2,lg,rhs=0,white.adjust="hc1")
(theta <- t(lg)%*%m2$coefficients)
se_theta <- theta/sqrt(lg_test$F[2])
(ic_theta <- c(theta-1.96*se_theta,theta+1.96*se_theta))

# ====/// 2i: Prediccion \\\=====
dataT1 <- dataT1 %>%
  mutate(educ_lus = educ/5)

m1b <- lm(wage ~ educ_lus + exper + exper2, data = dataT1)
summary(m1b)


# ====/// 2j: Heterogeneidad de industria \\\=====

dataT1_ind <- dataT1 %>%
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

m_5j <- feols(log(wage) ~ educ*industry + female + married + nonwhite + factor(region), data=dataT1_ind, vcov="hetero")
summary(m_5j)

# Extrae coeficientes y var-cov
b <- coef(m_5j)
V <- vcov(m_5j)
z <- qnorm(0.95)  # IC 90%

inds <- levels(dataT1_ind$industry)  # c("other","construc","ndurman","trade","services")

# Funcion para generar los intervalos
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

#  Gráfica de barras con IC 90%
ggplot(slopes, aes(x = industry, y = est)) +
  geom_col(fill = "#0072B2", alpha = 0.8, width = 0.65) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2, linewidth = 0.7) +
  labs(title = "Retorno de la educación por industria (IC 90%)",
       x = "Industria",
       y = "Pendiente de educ sobre ln(wage)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

ggsave("Barplot_2j.png",  width = 5.54, height = 4.95)


# Crear la L para la prueba de hipotesis
mat_j <- c(0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,
           0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,
           0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,
           0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)
L_j <- matrix(mat_j,nrow=4,ncol = 16, byrow = T)
linearHypothesis(m_5j,L_j,white.adjust="hc1")


# ====/// 2k: Educacion por niveles \\\=====

dataT1 <- dataT1 %>%
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

dums <- as.data.frame(
  model.matrix(~ educ_cat - 1, data = dataT1))
names(dums) <- sub("^educ_cat", "", names(dums))
df4 <- bind_cols(df4, dums)

m5_cat <- lm(
  log(wage) ~ i(educ_cat, ref = "none") + exper + exper2 +
    female + nonwhite + married + asinh(tenure) + factor(region),
  data = dataT1)
rse5cat <- sqrt(diag(vcovHC(m5_cat, type = "HC1")))

stargazer(m5, m5_cat, type = "latex",
          se = list(rse5,rse5cat),
          title = "Educacion continua vs niveles",
          label = "tab:reg_2k",
          dep.var.labels = c("log(salario)"),
          star.cutoffs = c(0.13, 0.07, 0.03),
          float.env = "table",
          align = TRUE,
          out = "table_ols_2k.tex")

# Primero hacemos dos estimaciones solo con educ o dummies
# (a) Variable continua
m_years <- lm(log(wage) ~ educ, data = dataT1)

# (b) Dummies
m_deg <- lm(log(wage) ~ i(educ_cat, ref = "none"), data = dataT1)

#-----------------------------
# 2. Prediction grid
#-----------------------------

# grid of years for plotting the fitted curves
grid <- data.frame(
  educ = seq(min(dataT1$educ, na.rm = TRUE),
                     max(dataT1$educ, na.rm = TRUE),
                     length.out = 200)
)

# Map years of schooling to a degree category (you choose the cutoffs!)
# Example cutoffs: <= 9 years = secondary, 10–12 = highschool, >= 13 = highed
grid <- grid %>%
  mutate(
    educ_cat = cut(
      educ,
      breaks = c(-Inf, 4, 8, 12, 16, Inf),
      labels = c("none", "elementary","middle", "high_school","higher"),
      right = FALSE
    )
  )

# Predicted log(wage) from each model
grid$pred_years <- predict(m_years, newdata = grid)
grid$pred_deg   <- predict(m_deg,   newdata = grid)

# Put in long format for ggplot
plot_dat <- grid %>%
  select(educ, pred_years, pred_deg) %>%
  pivot_longer(
    cols      = starts_with("pred_"),
    names_to  = "model",
    values_to = "pred_logwage"
  )


#-----------------------------
# 3. Plot with ggplot
#-----------------------------

ggplot() +
  # raw data
  geom_point(
    data = dataT1,
    aes(x = educ, y = log(wage)),
    alpha = 0.3
  ) +
  # two fitted curves
  geom_line(
    data = plot_dat,
    aes(x = educ, y = pred_logwage, color = model),
    linewidth = 1.1
  ) +
  labs(
    x = "Years of schooling",
    y = "log(wage)",
    color = "Specification",
    title = "log(wage) vs years of schooling",
    subtitle = "Linear in years vs. schooling aggregated by degree (dummies)"
  ) +
  scale_color_manual(
    values = c("pred_years" = "blue", "pred_deg" = "green"),
    labels = c("pred_years" = "Linear model", "pred_deg" = "Degree dummies")
  ) + theme_minimal()

ggsave("Graph_2k.png",  width = 5.54, height = 4.95)
