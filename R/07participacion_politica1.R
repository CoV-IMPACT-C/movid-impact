# Process data for political participation in MOVID-i ------------------------------------------------------
# 1. Load packages -----------------------------------------------------
pacman::p_load(dplyr, summarytools, sjPlot)

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
    cambing = g49,
    lning = ingh_p_log,
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

# Save processed data for political participation
saveRDS(data, file = "output/data/movidi_partpol.rds")
