# Tarea 2
# Script creado por: Arturo Aguilar

#Cargar librerias requeridas
rm(list = ls())

install <- function(packages){
  new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new.packages))
    install.packages(new.packages, dependencies = TRUE)
  sapply(packages, require, character.only = TRUE)
}
required.packages <- c('dplyr', 'tidyr', 'ggplot2', 'fixest',
                       'MASS', 'haven','jtools', 'forcats',
                       'nnet','stargazer')

install(required.packages)

#Abrir base de datos
data_t2 <- read.csv("movie_metadata_training.csv")

# ====/// 1_1: Graficas por clasificacion \\\=====

data_t2 <- data_t2 %>%
  mutate(rating = case_when(
    content_rating %in% c("G", "TV-G", "TV-Y", "TV-Y7", "Approved", "Passed") ~ "Family",
    content_rating %in% c("PG", "TV-PG", "GP", "M") ~ "Mild",
    content_rating %in% c("PG-13", "TV-14") ~ "Teen",
    content_rating %in% c("R", "NC-17", "X", "TV-MA") ~ "Adult",
    TRUE ~ NA_character_   # si hay valores no contemplados
  ))

data_t2 %>%
  filter(!is.na(rating)) %>%
  ggplot(aes(x = fct_infreq(rating))) +
  geom_bar() +
  coord_flip() +
  labs(
    title = "Distribution of Rating Group",
    x = "Rating Group",
    y = "Count"
  ) +
  theme_minimal()

ggsave("Fig_1_1.png",  width = 5.54, height = 4.95)

# Country classification
europe_countries <- c(
  "Belgium", "Bulgaria", "Czech Republic", "Denmark", "Finland",
  "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland",
  "Italy", "Netherlands", "Norway", "Poland", "Romania", "Russia",
  "Spain", "Sweden", "Switzerland", "UK", "West Germany", "Soviet Union"
)

asia_countries <- c(
  "China", "Hong Kong", "India", "Indonesia", "Iran", "Japan",
  "Cambodia", "Pakistan", "Philippines", "South Korea", "Taiwan",
  "Thailand", "United Arab Emirates"
)

data_t2 <- data_t2 %>%
  mutate(country_group = case_when(
    country == "USA" ~ "US",
    country == "Canada" ~ "Canada",
    country %in% europe_countries ~ "Europe",
    country %in% asia_countries ~ "Asia",
    is.na(country) ~ NA_character_,
    TRUE ~ "Other"
  ))

data_t2$rating_ord <- factor(data_t2$rating,
                          levels = c("Family",
                                     "Mild",
                                     "Teen",
                                     "Adult"),
                          ordered = TRUE)


data_t2 %>%
  filter(!is.na(rating_ord)) %>%
  group_by(rating_ord) %>%
  summarise(
    duration = mean(duration, na.rm = TRUE),
    budget   = mean(budget, na.rm = TRUE)/1000000,
    FB_likes = mean(movie_facebook_likes, na.rm = TRUE)/1000
  ) %>%
  pivot_longer(cols = c(duration, budget, FB_likes),
               names_to = "variable",
               values_to = "mean_value") %>%
  ggplot(aes(x = rating_ord, y = mean_value)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ variable, scales = "free_x") +
  labs(title = "Conditional Means by Rating Group",
       x = "Rating Group", y = "Mean") +
  theme_minimal()

ggsave("Fig_1_2a.png",  width = 5.54, height = 4.95)

data_t2 %>%
  filter(!is.na(rating_ord), !is.na(country_group)) %>%
  ggplot(aes(x = rating_ord, fill = country_group)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Country Group Distribution by Rating Group",
    x = "Rating Group",
    y = "Percent within Rating Group",
    fill = "Country Group"
  ) +
  theme_minimal()

ggsave("Fig_1_2b.png",  width = 5.54, height = 4.95)


# ====/// 1_2: Estimaciones OProbit y MNL \\\=====
data_t2 <- data_t2 %>%
  mutate(
    D_US = ifelse(country == "USA", 1, 0),
    D_UK = ifelse(country == "UK",  1, 0),
    Z_FB_likes = as.numeric(scale(movie_facebook_likes)),
    rating_mnl = relevel(
      factor(rating,
             levels = c("Mild", "Family", "Teen", "Adult")),
      ref = "Mild")
    )

# Ordered probit
oprobit <- polr(rating_ord ~ duration + log(budget) + Z_FB_likes + 
                  D_US + D_UK,
                data = data_t2, method = "probit", Hess = TRUE, 
                na.action = na.omit)

# Multinomial logit
multinom_logit <- multinom(rating_mnl ~ duration + log(budget) + Z_FB_likes + 
                             D_US + D_UK,
                           data = data_t2, trace = FALSE, 
                           na.action = na.omit)

# Agregar las constantes del ordered probit
ct <- coef(summary(oprobit))

# extract cutpoints rows
cuts_tbl <- ct[grep("\\|", rownames(ct)), , drop = FALSE]

cut_lines_se <- lapply(1:nrow(cuts_tbl), function(i) {
  est <- cuts_tbl[i, "Value"]
  se  <- cuts_tbl[i, "Std. Error"]
  
  c( rownames(cuts_tbl)[i],
     sprintf("%.3f (%.3f)", est, se),
     "", "", "" )
})

header_line <- list(c("Cutpoints (ordered probit)", "", "", "", ""))
add_lines_all <- c(header_line, cut_lines_se)

# Hacer la tabla
stargazer(oprobit, multinom_logit,
  type = "latex",
  column.labels = c("(1) Ordered Probit",
                    "(2) Family",
                    "(3) Teen",
                    "(4) Adult"),
  column.separate = c(1, 1, 1, 1),
  dep.var.labels.include = FALSE,
  covariate.labels = c("Duration", "log(Budget)", 
                       "FB_likes (z)", "US dummy", 
                       "UK dummy"),
  keep.stat = c("n", "ll", "aic"),
  add.lines = add_lines_all,
  out="Tabla_MV.tex")

# ====/// 1_2: Confusion matrix \\\=====

op_probs <- predict(oprobit, newdata = data_t2, type = "probs")
op_pred <- colnames(op_probs)[max.col(op_probs)]
op_pred <- factor(op_pred, levels = levels(data_t2$rating_ord), ordered = TRUE)
data_t2$op_pred <- op_pred

mnl_pred <- predict(multinom_logit, newdata = data_t2, type = "class")
mnl_pred <- factor(mnl_pred, levels = levels(data_t2$rating_mnl))
data_t2$ml_pred <- mnl_pred

# Confusion matrix ordered probit
cm_op <- table(Observed = data_t2$rating_ord, Predicted = data_t2$op_pred)
knitr::kable(cm_op, format = "latex", booktabs = TRUE,
             caption = "Confusion Matrix: Ordered Probit")
cm_op

# Confusion matrix multinomial logit
cm_ml <- table(Observed = data_t2$rating_mnl, Predicted = data_t2$ml_pred)
knitr::kable(cm_ml, format = "latex", booktabs = TRUE,
             caption = "Confusion Matrix: Multinomial Logit")
cm_ml

# Calculate the EPP for the elasticity
mnl_probs <- predict(multinom_logit, newdata = data_t2, type = "probs")

data_t2 <- cbind(data_t2, as.data.frame(mnl_probs))
names(data_t2)[(ncol(data_t2)-ncol(mnl_probs)+1):ncol(data_t2)] <- paste0("pr_", colnames(mnl_probs))

data_t2 <- data_t2 %>% mutate(elast_adult = coef(multinom_logit)["Adult", "log(budget)"]-
                                                        coef(multinom_logit)["Adult", "log(budget)"]*pr_Adult-
                                                        coef(multinom_logit)["Family", "log(budget)"]*pr_Family-
                                                        coef(multinom_logit)["Teen", "log(budget)"]*pr_Teen)
summary(data_t2$elast_adult)

# Calculate the EPP for the change in duration
data_t2 <- data_t2 %>% mutate(xib = oprobit$coefficients['duration']*duration +
                                oprobit$coefficients['log(budget)']*log(budget) +
                                oprobit$coefficients['Z_FB_likes']*Z_FB_likes +
                                oprobit$coefficients['D_US']*D_US +
                                oprobit$coefficients['D_UK']*D_UK,
                              epp = dnorm(oprobit$zeta['Mild|Teen']-xib) -
                                dnorm(oprobit$zeta['Teen|Adult']-xib))
mean(data_t2$epp,na.rm=TRUE)*oprobit$coefficients['duration']*15


# ====/// 3: Logit \\\=====

data_t2 <- data_t2 %>%
  mutate(
    y_family = ifelse(rating == "Family", 1, 0),
    y_mild   = ifelse(rating == "Mild", 1, 0),
    y_teen   = ifelse(rating == "Teen", 1, 0),
    y_adult  = ifelse(rating == "Adult", 1, 0)
  )

m_family <- glm(y_family ~ duration + log(budget) + Z_FB_likes + 
                  D_US + D_UK,
                data = data_t2, family = binomial(link = "logit"))

m_mild   <- glm(y_mild ~ duration + log(budget) + Z_FB_likes + 
                  D_US + D_UK,
                data = data_t2, family = binomial(link = "logit"))

m_teen   <- glm(y_teen ~ duration + log(budget) + Z_FB_likes + 
                  D_US + D_UK,
                data = data_t2, family = binomial(link = "logit"))

m_adult  <- glm(y_adult ~ duration + log(budget) + Z_FB_likes + 
                  D_US + D_UK,
                data = data_t2, family = binomial(link = "logit"))

stargazer(m_family, m_mild, m_teen, m_adult,
  type = "latex",   
  column.labels = c("Family", "Mild", "Teen", "Adult"),
  dep.var.labels.include = FALSE,
  covariate.labels = c("Duration", "log(Budget)", 
                       "FB_likes (z)", "US dummy", 
                       "UK dummy"),
  keep.stat = c("n", "aic"),
  title = "Binary Logit Models by Rating Group",
  out = "Logit_Table.tex")

data_t2 <- data_t2 %>%
  mutate(
    pr_family_logit = predict(m_family, newdata = data_t2, type = "response"),
    pr_mild_logit   = predict(m_mild,   newdata = data_t2, type = "response"),
    pr_teen_logit   = predict(m_teen,   newdata = data_t2, type = "response"),
    pr_adult_logit  = predict(m_adult,  newdata = data_t2, type = "response")
  )

#EPX
pr_teen_bar <- mean(data_t2$pr_teen_logit,na.rm=TRUE)
(pr_teen_bar*(1-pr_teen_bar)*m_teen$coefficients['log(budget)'])

#Prediccion
XiB_logit <- m_teen$coefficients['duration']*100 +
              m_teen$coefficients['log(budget)']*log(mean(data_t2$budget,na.rm=TRUE)) +
              m_teen$coefficients['Z_FB_likes']*2 +
              m_teen$coefficients['D_UK'] + 
              m_teen$coefficients['(Intercept)']

(pr_teen_4 <- exp(XiB_logit) / (1+exp(XiB_logit)))

# Matrix with the four probability columns
P_logit <- as.matrix(data_t2[, c("pr_family_logit", "pr_mild_logit", "pr_teen_logit", "pr_adult_logit")])

# Index of max probability for each row
imax <- max.col(P_logit)

# Map index to category labels (must match the probability column order)
labels_pred <- c("Family", "Mild", "Teen", "Adult")

data_t2$logit_pred <- factor(labels_pred[imax],
                         levels = labels_pred)

# Confusion matrix logit
cm_logit <- table(Observed = data_t2$rating_mnl, Predicted = data_t2$logit_pred)
knitr::kable(cm_logit, format = "latex", booktabs = TRUE,
             caption = "Confusion Matrix: Simple Logit")
cm_logit

