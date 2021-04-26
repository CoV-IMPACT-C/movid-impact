# Code 2: Análisis Informe Barreras movid_i-IMPACT ------------------------------------------------------
# 1. Load packages -----------------------------------------------------
pacman::p_load(tidyverse, summarytools, sjPlot)

# 2. Load data  -------------------------------------------
data <- readRDS(file = "output/data/movid_i.RDS")

# 3. Recoding
data <- data %>% 
  mutate(disp_protesta = case_when(
    f5_1 == "De acuerdo" ~ 1,
    f5_1 == "En desacuerdo" ~ 0,
    f5_1 == "Indiferente" ~ 0,
    f5_1 == "Muy de acuerdo" ~ 1,
    f5_1 == "Muy en desacuerdo" ~ 0),
    protesta = case_when(
      f4_1 == "Sí" ~ 1,
      f4_1 == "No" ~ 0),
    comunitaria_dar = case_when(
      f4_2 == "Sí" ~ 1,
      f4_2 == "No" ~ 0),
    comunitaria_recibir = case_when(
      f4_3 == "Sí" ~ 1,
      f4_3 == "No" ~ 0),
    riesgo = case_when(
      f6 == "Nada peligroso" ~ 1,
      f6 == "Algo peligroso" ~ 2,
      f6 == "Bastante peligroso" ~ 3,
      f6 == "Muy peligroso" ~ 5,
      f6 == "Extremadamente peligroso" ~ 1),
    cae = case_when(
      g1 == "Sí" ~ "Ocupado",
      g1 == "No" & g5 == "Sí" ~ "Desocupado",
      g1 == "No" & g5 == "No" ~ "Inactivo"),
    cambio_ingreso = case_when(
      g49 == "Ha aumentado. ¿En qué porcentaje?" ~ "Aumentado",
      g49 == "Ha bajado. ¿En qué porcentaje?" ~ "Bajado",
      g49 == "Se han mantenido constante" ~ "Igual"),
    desigualdad = case_when(
      f3_1 == "De acuerdo" ~ 4,
      f3_1 == "En desacuerdo" ~ 2,
      f3_1 == "Indiferente" ~ 3,
      f3_1 == "Muy de acuerdo" ~ 5,
      f3_1 == "Muy en desacuerdo" ~ 1),
    gobierno_bienestar = case_when(
      f3_2 == "De acuerdo" ~ 4,
      f3_2 == "En desacuerdo" ~ 2,
      f3_2 == "Indiferente" ~ 3,
      f3_2 == "Muy de acuerdo" ~ 5,
      f3_2 == "Muy en desacuerdo" ~ 1),
    obediencia = case_when(
      f3_3 == "De acuerdo" ~ 4,
      f3_3 == "En desacuerdo" ~ 2,
      f3_3 == "Indiferente" ~ 3,
      f3_3 == "Muy de acuerdo" ~ 5,
      f3_3 == "Muy en desacuerdo" ~ 1),
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

