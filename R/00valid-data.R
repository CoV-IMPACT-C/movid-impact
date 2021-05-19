# Code 0: Validate movid_i-IMPACT ------------------------------------------------------
# 1. Load packages -----------------------------------------------------
pacman::p_load(tidyverse, arsenal)

# 2. Load data  -------------------------------------------
## movid_i-19
movid_o_antigua <- haven::read_dta("input/data/210123_base_movid_version02.dta")
movid_o_csv <- read.csv("input/data/20210125_base_movid_version_codificada.csv",
                        na.strings=c("","NA", ""),
                        header = TRUE, fileEncoding = "UTF-8")
movid_o <- haven::read_dta("input/data/20210125_base_movid_version_codificada.dta")

# Compare
movid_o_cambios <- anti_join(movid_o, movid_o_antigua)
#59 obs distintas
movid_o_cambios <- summary(comparedf(movid_o,movid_o_antigua))
movid_o_cambios$diffs.table


# Likerts -----------------------------------------------------------------
movid <- movid_o %>% select(id_encuesta, starts_with("f3"),starts_with("f5"), starts_with("f7")) %>% 
  mutate_at(vars(starts_with("f")), funs(as_factor(.))) %>% 
  mutate_at(vars(starts_with("f")), funs(as.character(.))) 

movid_csv <- movid_o_csv %>% select(id_encuesta,starts_with("f3"),starts_with("f5"), starts_with("f7")) %>% 
  mutate_at(vars(starts_with("f")), funs(as_factor(.))) %>% 
  mutate_at(vars(starts_with("f")), funs(as.character(.)))
  
movid_o_cambios <- summary(comparedf(movid,movid_csv))
movid_o_cambios$diffs.table
