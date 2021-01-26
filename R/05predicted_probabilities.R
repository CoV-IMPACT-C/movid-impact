# Code 2: Análisis Informe Barreras movid_i-IMPACT ------------------------------------------------------
# 1. Packages -----------------------------------------------------
pacman::p_load(tidyverse, summarytools, sjPlot, modelr, purrr, ggeffects,
               scales)

# 2. Import data  -------------------------------------------
## movid_i-19
movid_i_proc <- readRDS(file = "output/data/movid_i_proc.RDS")
data <- movid_i_proc

## Homologar nombres de variables
data <- data %>% 
  mutate(
    cuidado_toque = ifelse(f4_toque == "Si", 0,
                           ifelse(f4_toque == "No", 1, f4_toque)),
    cuidado_manos   = f7_wash, # Siempre 1; else 0
    cuidado_dist    = f7_distance,
    cuidado_social  = f7_social,
    cuidado_masc    = f7_mask,
    cuidado_masc2   = f7_mask2,
    barrera_protect = f5_protect, # escala continua
    barrera_fisc    = f5_legal, # 1 = En desacuerdo o Muy en desacuerdo; else 0
    barrera_riesgo  = f6, # escala continua
    barrera_normas  = f8) # invertir orden

## Recodificar para modelos
data <- data %>% 
  mutate(across(c(starts_with("cuidado"), -cuidado_toque), 
                ~case_when(
                  . == "Siempre" ~ 1,
                  is.na(.) ~ NA_real_,
                  TRUE ~ 0)),
         across(c(barrera_protect, barrera_riesgo), ~as.numeric(.)),
         barrera_normas = (as.numeric(barrera_normas)*-1)+7,
         barrera_fisc = case_when(
           barrera_fisc %in% c("Muy en desacuerdo", "En desacuerdo") ~ 1,
           is.na(barrera_fisc) ~ NA_real_,
           TRUE ~ 0),
         barrera_fisc = factor(barrera_fisc), ref = "",
         cae = relevel(factor(cae), ref = "Inactivo"),
         educ_3cat = factor(educ_3cat, 
                            labels = c("Secondary", "Technical", "University")),
         trabaja = relevel(g1, ref = "No")
         )

# 3. Models data  -------------------------------------------
# Modelos para predecir cuidados
predictores <- c("sexo + edad + trabaja + educ_3cat + cronicos + 
                 barrera_protect + barrera_fisc + barrera_riesgo + 
                 barrera_normas")

m_toque  <- glm(as.formula(paste0("cuidado_toque ~", predictores)),
                data=data, family="binomial")
m_manos  <- glm(as.formula(paste0("cuidado_manos ~", predictores)), 
                data=data, family="binomial")
m_dist   <- glm(as.formula(paste0("cuidado_dist ~", predictores)),
                data=data, family="binomial")
m_social <- glm(as.formula(paste0("cuidado_social ~", predictores)),
                data=data, family="binomial")
m_masc   <- glm(as.formula(paste0("cuidado_masc ~", predictores)),
                data=data, family="binomial")
m_masc2  <- glm(as.formula(paste0("cuidado_masc2 ~", predictores)),
                data=data, family="binomial")

tab_model(m_toque, m_manos, m_dist, m_social, m_masc, m_masc2)

# 4. Predicted probabilites  -------------------------------------------
### Numeric variables at their median
### Factor at their mode
### Variable `trabaja` is at "Sí" which is not the mode (code issue)
### Default intervals are 95% confidence intervals
models <- list(m_toque, m_manos, m_dist, m_social, m_masc, m_masc2)
dv <- c("Toque de queda", "Lavado de manos", "Distanciamiento", 
        "Social", "Mascarilla 1", "Mascarilla 2")

pp_prt <- map_df(models, 
                 ~ggpredict(., terms ="barrera_protect [minmax]",
                            typical = list(numeric = "median", 
                                           factor = "mode"))) %>%
  as_tibble() %>% 
  mutate(dv = rep(dv, each = 2),
         x  = fct_relevel(ifelse(x == 1, "Bajo", "Alto"), "Bajo"))

pp_fsc <- map_df(models, 
                 ~ggpredict(., terms ="barrera_fisc",
                            typical = list(numeric = "median", 
                                           factor = "mode"))) %>%
  as_tibble() %>% 
  mutate(dv = rep(dv, each = 2),
         x  = fct_relevel(ifelse(x == 0, "Bajo", "Alto"), "Bajo"))

pp_rsg <- map_df(models, 
                 ~ggpredict(., terms ="barrera_riesgo [minmax]",
                            typical = list(numeric = "median", 
                                           factor = "mode"))) %>%
  as_tibble() %>% 
  mutate(dv = rep(dv, each = 2),
         x  = fct_relevel(ifelse(x == 1, "Bajo", "Alto"), "Bajo"))

pp_nrm <- map_df(models, 
                 ~ggpredict(., terms ="barrera_normas [minmax]",
                            typical = list(numeric = "median",
                                           factor = "mode"))) %>%
  as_tibble() %>% 
  mutate(dv = rep(dv, each = 2),
         x  = fct_relevel(ifelse(x == 1, "Bajo", "Alto"), "Bajo"))

pp_trbj <- map_df(models, 
                  ~ggpredict(., terms ="trabaja",
                             typical = list(numeric = "median", 
                                            factor = "mode"))) %>%
  as_tibble() %>% 
  mutate(dv = rep(dv, each = 2))

# 5. Plots  -------------------------------------------
plt_pp_prt <- pp_prt %>% 
  ggplot(aes(y = predicted, x = x, ymin = conf.low, ymax = conf.high)) +
  geom_bar(aes(fill = x), stat = "identity", position = "dodge") +
  geom_errorbar(stat = "identity", position = "dodge", 
                width = .33, color = "#8D8680") +
  scale_fill_manual(values = c("#E69F00", "#D55E00")) +
  facet_wrap(~dv) +
  theme_classic(base_size = 22) +
  scale_y_continuous(labels = percent_format(suffix = "")) +
  labs(x = "Barrera protect", y = "Probabilidad predicha (%)") +
  theme(legend.position = "none")

## Save plot
ggsave(plt_pp_prt, filename = "output/figures/pred_prob_protect.png",
       dpi = 500, width = 10, height = 6)
