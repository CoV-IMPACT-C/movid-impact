# Code 2: Análisis Informe Barreras movid_i-IMPACT ------------------------------------------------------
# 1. Load packages -----------------------------------------------------
pacman::p_load(tidyverse, summarytools, sjPlot)

# 2. Load data  -------------------------------------------
data <- readRDS(file = "output/data/movid_o.RDS")

# 3. Recoding
data <- data %>% 
  mutate(disp_protesta = case_when(
    f5_6 == 4 ~ 1,
    f5_6 == 2 ~ 0,
    f5_6 == 3 ~ 0,
    f5_6 == 5 ~ 1,
    f5_6 == 1 ~ 0),
    protesta = case_when(
      f4_1 == 1 ~ 1,
      f4_1 == 2 ~ 0),
    comunitaria_dar = case_when(
      f4_2 == 1 ~ 1,
      f4_2 == 2 ~ 0),
    comunitaria_recibir = case_when(
      f4_3 == 1 ~ 1,
      f4_3 == 2 ~ 0),
    riesgo = case_when(
      f6 == 1 ~ 1,
      f6 == 2 ~ 2,
      f6 == 3 ~ 3,
      f6 == 4 ~ 4,
      f6 == 5 ~ 5),
    cae = case_when(
      g1 == 1 ~ "Ocupado",
      g1 == 2 & g5 == 1 ~ "Desocupado",
      g1 == 2 & g5 == 2 ~ "Inactivo"),
    cambio_ingreso = case_when(
      g49 == 3 ~ "Aumentado",
      g49 == 1 ~ "Bajado",
      g49 == 2 ~ "Igual"),
    desigualdad = case_when(
      f3_1 == 4 ~ 4,
      f3_1 == 2 ~ 2,
      f3_1 == 3 ~ 3,
      f3_1 == 5 ~ 5,
      f3_1 == 1 ~ 1),
    gobierno_bienestar = case_when(
      f3_2 == 4 ~ 4,
      f3_2 == 2 ~ 2,
      f3_2 == 3 ~ 3,
      f3_2 == 5 ~ 5,
      f3_2 == 1 ~ 1),
    obediencia = case_when(
      f3_3 == 4 ~ 4,
      f3_3 == 2 ~ 2,
      f3_3 == 3 ~ 3,
      f3_3 == 5 ~ 5,
      f3_3 == 1 ~ 1),
    )


# 4. Descriptivos

prop.table(table(data$protesta))
prop.table(table(data$comunitaria_dar))
prop.table(table(data$comunitaria_recibir))
prop.table(table(data$disp_protesta))

# 5. Modelos para predecir participación

lm1 <- glm(protesta ~ sexo + edad  + cae + cambio_ingreso, data=data, family="binomial")
lm2 <- glm(protesta ~ sexo + edad  + cae + cambio_ingreso + riesgo + desigualdad + gobierno_bienestar + obediencia, data=data, family="binomial")

lm3 <- glm(comunitaria_dar ~ sexo + edad  + cae + cambio_ingreso, data=data, family="binomial")
lm4 <- glm(comunitaria_dar ~ sexo + edad  + cae + cambio_ingreso + riesgo + desigualdad + gobierno_bienestar  + obediencia, data=data, family="binomial")

lm5 <- glm(disp_protesta ~ sexo + edad  + cae + cambio_ingreso, data=data, family="binomial")
lm6 <- glm(disp_protesta ~ sexo + edad  + cae + cambio_ingreso + riesgo + desigualdad + gobierno_bienestar  + obediencia, data=data, family="binomial")

tab_model(lm1, lm2, lm3, lm4, lm5, lm6)
tab_model(lm2, lm4, lm6)

names(data)

table(data$region)
