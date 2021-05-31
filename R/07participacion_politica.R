# Code 2: Análisis Informe Barreras movid_i-IMPACT ------------------------------------------------------
# 1. Load packages -----------------------------------------------------
pacman::p_load(tidyverse, summarytools, sjPlot)

# 2. Load data  -------------------------------------------
data <- readRDS(file = "output/data/movid_i.RDS")

# 3. Recoding
data <- data %>% 
  mutate(disp_protesta = case_when(
    f5_protesta %in% c("Muy de acuerdo", "De acuerdo") ~ 1,
    f5_protesta %in% c("Indiferente", "En desacuerdo", "Muy en desacuerdo") ~ 0),
    protesta = case_when(
      f4_protesta == "Si" ~ 1,
      f4_protesta == "No" ~ 0),
    comunitaria_dar = case_when(
      f4_ollas == "Si" ~ 1,
      f4_ollas == "No" ~ 0),
    riesgo = as.numeric(f6),
    cambio_ingreso = g49,
    ingreso = ingh_p_log,
    desigualdad = as.numeric(f3_desigualdad), 
    gobierno_bienestar = as.numeric(f3_bienestar),
    obediencia = as.numeric(f3_obedecer),
    covid_pos = d6,
    salud_autop = e7,
    dist_social = as.numeric(f7_distance),
    no_reunion = as.numeric(f7_social),
    educ = case_when(
      educ_4cat == 1 ~ "Básica o menos",
      educ_4cat == 2 ~ "Media",
      educ_4cat == 3 ~ "Técnica",
      educ_4cat == 4 ~ "Profesional"),
    educ = as.factor(educ)
    )
    
    
    
# 4. Descriptivos

prop.table(table(data$protesta))
prop.table(table(data$comunitaria_dar))
prop.table(table(data$disp_protesta))

# 5. Modelos para predecir participación

lm1 <- glm(protesta ~ sexo + edad  + educ + cae + cambio_ingreso + ingreso + cronicos + covid_pos + salud_autop + dist_social + no_reunion, data=data, family="binomial")
lm2 <- glm(protesta ~ sexo + edad  + educ + cae + cambio_ingreso + ingreso + cronicos + covid_pos + salud_autop + dist_social + no_reunion + riesgo + desigualdad + gobierno_bienestar + obediencia, data=data, family="binomial")

lm3 <- glm(comunitaria_dar ~ sexo + edad  + educ + cae + cambio_ingreso + ingreso + cronicos + covid_pos + salud_autop + dist_social + no_reunion, data=data, family="binomial")
lm4 <- glm(comunitaria_dar ~ sexo + edad  + educ + cae + cambio_ingreso + ingreso + cronicos + covid_pos + salud_autop + dist_social + no_reunion + riesgo + desigualdad + gobierno_bienestar  + obediencia, data=data, family="binomial")

lm5 <- glm(disp_protesta ~ sexo + edad  + educ + cae + cambio_ingreso + ingreso + cronicos + covid_pos + salud_autop + dist_social + no_reunion, data=data, family="binomial")
lm6 <- glm(disp_protesta ~ sexo + edad  + educ + cae + cambio_ingreso + ingreso + cronicos + covid_pos + salud_autop + dist_social + no_reunion + riesgo + desigualdad + gobierno_bienestar  + obediencia, data=data, family="binomial")

tab_model(lm1, lm2, lm3, lm4, lm5, lm6)
tab_model(lm2, lm4, lm6)



  
  
  
