# Code 3: Graficos Informe Barreras movid_i-IMPACT ------------------------------------------------------
# 1. Load packages -----------------------------------------------------
pacman::p_load(tidyverse, srvyr, survey)

# 1.1 Set ups -------------------------------------------------------------
library(ggplot2); theme_set(theme_classic(base_size = 12) + 
                              theme(legend.position = "bottom",
                                    legend.box = "horizontal",
                                    text = element_text(size = 12, face = "bold"), strip.text.x = element_text(face = "bold")))
library(knitr);   options(knitr.kable.NA = 'No sabe/No responde')

library(survey);  options(survey.lonely.psu = "certainty")

# 2. Load data  -------------------------------------------
## movid_i-19
movid_i_proc <- readRDS(file = "output/data/movid_i_proc.RDS")
data <- movid_i_proc

# 3. Recodificaciones para informe ----------------------------------------
# Cumplimiento toque de queda
data <- data %>% mutate_at(vars(starts_with("f7_")),
                     funs(car::recode(.,'"Siempre" = "Si";
                                      c("Casi siempre","Frecuentemente","A veces","Casi nunca")="No"', as.factor = T,
                          levels = c('No', 'Si')))) %>% # El orden es donde 0 = No, 1 = Si
  mutate_at(vars(f4_toque, starts_with("f7_"), starts_with("f3_")),
                           funs(as_factor(.)))  %>%  # Como un factor para no perder etiquetas
  mutate_at(vars(starts_with("f3_"), starts_with("f5_"), f8), funs(forcats::fct_rev(.)))  # Revertir  los factores de las barreras donde 1 es
          

## Sintesis
### 1. Cuidados (f7_*): 1 = No, 2 = Si
### 2. Barreras legitimidad (f3_*): 1 = Muy en desacuerdo a 5 = Muy de acuerdo (revertidas)
### 3. Barreras fatiga (f5_*): 1 = Muy en desacuerdo a 5 = Muy de acuerdo (revertidas)
### 4. Barreras percepcion riesgo (f6): 1 = Nada peligroso a 5 = Extremadamente peligroso
### 5. Barreras (f8): 1 = Nada a 5 = Completamente (revertidas)



# 4. Modelos para informe -------------------------------------------------
# Modelos para predecir cuidados

# lm1 <- glm(f4_toque ~ sexo + edad + cronicos + barrera_protect + barrera_fisc + barrera_riesgo + barrera_normas, data=data, family="binomial")
# lm2 <- glm(f7_wash ~ sexo + edad + cronicos + barrera_protect + barrera_fisc + barrera_riesgo + barrera_normas, data=data, family="binomial")
# lm3 <- glm(f7_dist ~ sexo + edad + cronicos + barrera_protect + barrera_fisc + barrera_riesgo + barrera_normas, data=data, family="binomial")
# lm4 <- glm(f7_social ~ sexo + edad + cronicos + barrera_protect + barrera_fisc + barrera_riesgo + barrera_normas, data=data, family="binomial")
# lm5 <- glm(f7_mask ~ sexo + edad + cronicos + barrera_protect + barrera_fisc + barrera_riesgo + barrera_normas, data=data, family="binomial")
# lm6 <- glm(f7_mask2 ~ sexo + edad + cronicos + barrera_protect + barrera_fisc + barrera_riesgo + barrera_normas, data=data, family="binomial")
# 
# tab_model(lm1, lm2, lm3, lm4, lm5, lm6)


# 5. Informe Barreras y medidas cuidado  -------------------------------------------

# Lamina 1: ¿Qué es MOVID-19? -------------------------------------------

# Lamina 2: ¿Quienes somos MOVID-19? -------------------------------------------

# Lamina 3: MOVID-Impact: Metodología, dimensiones -------------------------------------------


# Lamina 4: ¿Cuánto nos cuidamos del COVID-19 en Chile? -------------------
# Figura 1 ---------------------------------------------------------------
## Indicacion:Descriptivo de toque de queda y los 5 cuidados (quizás usar un grid de 3 x 2) 
## Variable:f4_toque, f7_wash", "f7_distance", "f7_social", "f7_mask", "f7_mask2"
## Detalle: Cada gráfico con todas las categorías de respuesta y un NS/NR

### Version sin factores
label <- c(
  f4_toque = "Respeto toque de queda",
  f7_distance = "Distancia física de 2 metros",
  f7_mask = "Uso mascarilla fuera del hogar",
  f7_mask2 = "Uso mascarilla en lugares cerrados",
  f7_social = "Evitar reuniones con más de 10 personas en un espacio cerrado",
  f7_wash = "Lavarse las manos durante 20 segundos")

data %>%
  pivot_longer(cols = c(starts_with("f7_"), f4_toque),
                      names_to = "variable",
                      values_to = "valor") %>% 
  group_by(variable, valor) %>% 
  summarise(n = n()) %>% 
  mutate (prop = round(n/ sum(n),4)*100) %>%
  ggplot(aes(x = valor, y = prop, fill = valor)) +
  geom_bar(stat = "identity", width = 0.9) +
  facet_wrap(.~ variable, ncol = 2, labeller = labeller(variable = label)) +
  geom_label(aes(label = paste0(prop, "%")),
            position = position_stack(vjust = .5),
            color="white", size= 4, fontface = "bold",
            show.legend = FALSE) + 
  labs(x = "", y = "%", title = "") + scale_fill_jama(name = "",
                                    na.value = "grey50")


# Version con factores ----------------------------------------------------
data %>%
  pivot_longer(cols = c(starts_with("f7_"), f4_toque),
                        names_to = "variable",
                        values_to = "valor") %>%
  mutate( valor = as.character(valor),
    valor = if_else(is.na(valor), "NS/NR", valor),
    valor = as_factor(valor)) %>% 
  srvyr::as_survey_design(ids = 1, weights = factor_expansion) %>%
  group_by(variable, valor)  %>% 
  summarise(prop = survey_mean(vartype = "ci", na.rm = T)) %>% 
  mutate_at(vars(starts_with("prop")), funs(round(.,4)*100)) %>%
  ggplot(aes(x = valor, y = prop, fill = valor)) +
  geom_bar(stat = "identity", width = 0.9) +
  geom_errorbar(aes(x = valor, ymin = prop_low, ymax= prop_upp), width = 0.2,
                size = 1.1, color = "black") +
  facet_wrap(.~ variable, ncol = 2, labeller = labeller(variable = label)) +
  geom_label(aes(label = paste0(prop, "%")),
             position = position_stack(vjust = .5),
             color="white", size= 4, fontface = "bold",
             show.legend = FALSE) + 
  labs(x = "", y = "%", title = "")  + scale_fill_jama(name = "", 
                                    na.value = "grey50")


### Guardar
ggsave(plot = last_plot(), filename = "output/figures/figure1.png",
       device = "png",dpi = "retina", units = "cm",
       width = 25,height = 15)


# Lámina 5: ¿Por qué no nos cuidamos del COVID-19 en Chile? ---------------
##Porque no creemos que el COVID-19 sea un peligro real
# Figura 2 ----------------------------------------------------------------
## Indicacion: descriptivo de f6 con todas las categorías de respuesta y un NS/NR
data %>%
  pivot_longer(cols = starts_with("f6"),
               names_to = "variable",
               values_to = "valor") %>%
  mutate( valor = as.character(valor),
          valor = if_else(is.na(valor), "NS/NR", valor),
          valor = factor(valor, levels = c("Nada peligroso", "Algo peligroso", "Bastante peligroso","Muy peligroso", "Extremadamente peligroso", "NS/NR"))) %>% 
  srvyr::as_survey_design(ids = 1, weights = factor_expansion) %>%
  group_by(variable, valor)  %>% 
  summarise(prop = survey_mean(vartype = "ci", na.rm = T)) %>% 
  mutate_at(vars(starts_with("prop")), funs(round(.,4)*100)) %>%
  ggplot(aes(x = valor, y = prop, fill = valor)) +
  geom_bar(stat = "identity", width = 0.9) +
  geom_errorbar(aes(x = valor, ymin = prop_low, ymax= prop_upp), width = 0.2,
                size = 1.1, color = "black") +
  facet_wrap(.~ variable, ncol = 2, labeller = labeller(variable = c(f6 = "¿Qué tan peligroso cree que es el coronavirus para usted y sus cercanos?"))) +
  geom_label(aes(label = paste0(prop, "%")),
             position = position_stack(vjust = .5),
             color="white", size= 4, fontface = "bold",
             show.legend = FALSE) + 
  labs(x = "", y = "%", title = "")  + scale_fill_jama(name = "", 
                                    na.value = "grey50")


### Guardar
ggsave(plot = last_plot(), filename = "output/figures/figure2.png",
       device = "png",dpi = "retina", units = "cm",
       width = 25,height = 15)



# Lamina 6: ¿Por qué no nos cuidamos del COVID-19 en Chile? ---------------
##Porque no creemos que el COVID-19 sea un peligro real

# Figura 3 ----------------------------------------------------------------
## Indicacion: Probabilidades predichas de cumplir "siempre" con cada medida para peligro = 1 y peligro = 5 - 
## dejar variables de cuidado donde hay diferencias significativas



# Lamina 7: ¿Por qué no nos cuidamos del COVID-19 en Chile?  --------------
## Porque nos sentimos invencibles
# Figura 4 ----------------------------------------------------------------
## Indicacion: Descriptivo de f5_1 con todas las categorías de respuesta y un NS/NR
data %>%
  pivot_longer(cols = starts_with("f5_protect"),
               names_to = "variable",
               values_to = "valor") %>%
  mutate( valor = as.character(valor),
          valor = if_else(is.na(valor), "NS/NR", valor),
          valor = factor(valor, levels = c("Muy en desacuerdo", "En desacuerdo", "Indiferente","De acuerdo", "Muy de acuerdo", "NS/NR"))) %>% 
  srvyr::as_survey_design(ids = 1, weights = factor_expansion) %>%
  group_by(variable, valor)  %>% 
  summarise(prop = survey_mean(vartype = "ci", na.rm = T)) %>% 
  mutate_at(vars(starts_with("prop")), funs(round(.,4)*100)) %>%
  ggplot(aes(x = valor, y = prop, fill = valor)) +
  geom_bar(stat = "identity", width = 0.9) +
  geom_errorbar(aes(x = valor, ymin = prop_low, ymax= prop_upp), width = 0.2,
                size = 1.1, color = "black") +
  facet_wrap(.~ variable, ncol = 2, labeller = labeller(variable = c(f5_protect = "Puedo protegerme completamente del coronavirus si tomo las medidas de protección adecuadas"))) +
  geom_label(aes(label = paste0(prop, "%")),
             position = position_stack(vjust = .5),
             color="white", size= 4, fontface = "bold",
             show.legend = FALSE) + 
  labs(x = "", y = "%", title = "")  + scale_fill_jama(name = "", 
                                                       na.value = "grey50") +
  guides(fill = F)


### Guardar
ggsave(plot = last_plot(), filename = "output/figures/figure4.png",
       device = "png",dpi = "retina", units = "cm",
       width = 25,height = 15)


# Lamina 8: ¿Por qué no nos cuidamos del COVID-19 en Chile? ---------------
## Porque nos sentimos invencibles
# Figura 5 ----------------------------------------------------------------
## Indicacion: Probabilidades predichas de cumplir "siempre" con cada medida para f5_1 = 1 y f5_1 = 5 - 
## dejar variables de cuidado donde hay diferencias significativas


# Lamina 9: ¿Por qué no nos cuidamos del COVID-19 en Chile? ----------------------------------------------------------------
## Porque no pasará nada si no seguimos las reglas
# Figura 6 ----------------------------------------------------------------
## Indicacion: Descriptivo de f5_5 con todas las categorías de respuesta y un NS/NR
data %>%
  pivot_longer(cols = starts_with("f5_legal"),
               names_to = "variable",
               values_to = "valor") %>%
  mutate( valor = as.character(valor),
          valor = if_else(is.na(valor), "NS/NR", valor),
          valor = factor(valor, levels = c("Muy en desacuerdo", "En desacuerdo", "Indiferente","De acuerdo", "Muy de acuerdo", "NS/NR"))) %>% 
  srvyr::as_survey_design(ids = 1, weights = factor_expansion) %>%
  group_by(variable, valor)  %>% 
  summarise(prop = survey_mean(vartype = "ci", na.rm = T)) %>% 
  mutate_at(vars(starts_with("prop")), funs(round(.,4)*100)) %>%
  ggplot(aes(x = valor, y = prop, fill = valor)) +
  geom_bar(stat = "identity", width = 0.9) +
  geom_errorbar(aes(x = valor, ymin = prop_low, ymax= prop_upp), width = 0.2,
                size = 1.1, color = "black") +
  facet_wrap(.~ variable, ncol = 2, labeller = labeller(variable = c(f5_legal = "En Chile, si una persona sale sin permiso durante una cuarentena es muy poco probable que sea controlado y multado de protección adecuadas"))) +
  geom_label(aes(label = paste0(prop, "%")),
             position = position_stack(vjust = .5),
             color="white", size= 4, fontface = "bold",
             show.legend = FALSE) + 
  labs(x = "", y = "%", title = "")  + scale_fill_jama(name = "", 
                                                       na.value = "grey50") + 
  guides(fill = F)


### Guardar
ggsave(plot = last_plot(), filename = "output/figures/figure6.png",
       device = "png",dpi = "retina", units = "cm",
       width = 25,height = 15)


# Lamina 10: ¿Por qué no nos cuidamos del COVID-19 en Chile? --------------
## Porque no pasará nada si no seguimos las reglas
# Figura 7 ----------------------------------------------------------------
## Indicacion: probabilidades predichas de cumplir "siempre" con cada medida para f5_5 = 1 y f5_5 = 5 - 
## dejar variables de cuidado donde hay diferencias significativas


# Lamina 11: ¿Por qué no nos cuidamos del COVID-19 en Chile? --------------
##  Porque los demás tampoco lo hacen
# Figura 8 ----------------------------------------------------------------
## Indicacion: descriptivo de f8 con todas las categorías de respuesta y un NS/NR
data %>%
  pivot_longer(cols = starts_with("f8"),
               names_to = "variable",
               values_to = "valor") %>%
  mutate( valor = as.character(valor),
          valor = if_else(is.na(valor), "NS/NR", valor),
          valor = factor(valor, levels = c("Nada", "Poco", "Algo","Bastante", "En gran medida", "Completamente","NS/NR"))) %>% 
  srvyr::as_survey_design(ids = 1, weights = factor_expansion) %>%
  group_by(variable, valor)  %>% 
  summarise(prop = survey_mean(vartype = "ci", na.rm = T)) %>% 
  mutate_at(vars(starts_with("prop")), funs(round(.,4)*100)) %>%
  ggplot(aes(x = valor, y = prop, fill = valor)) +
  geom_bar(stat = "identity", width = 0.9) +
  geom_errorbar(aes(x = valor, ymin = prop_low, ymax= prop_upp), width = 0.2,
                size = 1.1, color = "black") +
  facet_wrap(.~ variable, ncol = 2, labeller = labeller(variable = c(f8 = "Grado en que mis cercanos y yo cumplimos las medidas de cuidado"))) +
  geom_label(aes(label = paste0(prop, "%")),
             position = position_stack(vjust = .5),
             color="white", size= 4, fontface = "bold",
             show.legend = FALSE) + 
  labs(x = "", y = "%", title = "")  + scale_fill_jama(name = "", 
                                                       na.value = "grey50") + 
  guides(fill = F)

### Guardar
ggsave(plot = last_plot(), filename = "output/figures/figure8.png",
       device = "png",dpi = "retina", units = "cm",
       width = 25,height = 15)


# Lamina 12: ¿Por qué no nos cuidamos del COVID-19 en Chile? --------------
##  Porque los demás tampoco lo hacen
# Figura 9 ----------------------------------------------------------------
## Indicacion: Probabilidades predichas de cumplir "siempre" con cada medida para f8 = 1 y f8 = 6 - 
## dejar variables de cuidado donde hay diferencias significativas



# Lamina 13 : ¿Cómo promovemos el respeto a las medidas de cuidado COVID-19? ---------------------------------------------------------------
## Comunicación de riesgos
## Fiscalización real y comunicada a la ciudadanía
## Comunicar normas sociales (ejemplos de quienes cumplen, no de quienes no cumplen)
## ¿Propuestas más concretas?


# Lamina 14: Cierre -------------------------------------------------------








