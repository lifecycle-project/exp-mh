################################################################################
## Project: Exposome menta health
## Script purpose: Tables
## Date: 4th August 2022
## Author: Tim Cadman
## Email: tica@sund.ku.dk
################################################################################

################################################################################
# Summarise exclusions and transformations 
################################################################################

## ---- Included ---------------------------------------------------------------
included.tab <- all_vars %>%
  dplyr::filter(!variable %in% c(trans_sum, exclude.vars))

## ---- Transformed ------------------------------------------------------------
trans_sum %>% print(n = Inf)

## ---- Excluded ---------------------------------------------------------------
excluded.vars <- all_vars %>%
  dplyr::filter(variable %in% exclude.vars)

################################################################################
# Summarise included variables for methods  
################################################################################

## ---- Built environment ------------------------------------------------------
built.vars <- included.tab %>%
  dplyr::filter(family == "built_env")

write_csv(built.vars, here("tables", "built_inc.csv"))

## ---- Natural spaces ---------------------------------------------------------
natural.vars <- included.tab  %>%
  dplyr::filter(family == "natural")

write_csv(natural.vars, here("tables", "nat_inc.csv"))

## ---- Pollution --------------------------------------------------------------
pollution.vars <- included.tab %>%
  dplyr::filter(family == "pollution")

write_csv(pollution.vars, here("tables", "pol_inc.csv"))

## ---- Noise ------------------------------------------------------------------
noise.vars <- included.tab %>% 
  dplyr::filter(family == "noise")

write_csv(noise.vars, here("tables", "noise_inc.csv"))

################################################################################
# Instruments used for mental health assessment  
################################################################################
adhd_coh_tmp <- adhd_coh[!adhd_coh %in% c("alspac", "chop", "dnbc")]

mh_instr.stats <- dh.getStats(
  df = "adhd_sub", 
  vars = "adhd_instr_", 
  conns = conns[adhd_coh])

mh_instr.stats$categorical %>%
  dplyr::filter(!is.na(category) & value > 0 & cohort != "combined") %>%
  print(n = Inf)

################################################################################
# Fixed effect models  
################################################################################

## ---- Function ---------------------------------------------------------------
exwasTab <- function(fit){
  
coefs <- fit %>%
  pmap(function(exposure, cohort, fit, ...){
    
     tmp <- dh.lmTab(
      model = fit, 
      type = "glm_slma", 
      coh_names = cohort, 
      direction = "wide", 
      ci_format = "separate") %>%
       dplyr::filter(variable %in% exposure & cohort != "combined")
    }) %>%
  set_names(fit$exposure) %>%
  bind_rows 

out <- coefs %>%
  dplyr::rename(analysis_name = variable) %>%
  left_join(., all_vars, by = "analysis_name") %>%
  dplyr::select(full_name, family, cohort, est, pvalue) %>%
  dplyr::filter(pvalue < 0.05) %>%
  mutate(pvalue = round(pvalue, 2)) %>%
  pivot_wider(
    names_from = cohort, 
    values_from = c(est, pvalue), 
    names_glue = "{cohort}_{.value}") %>%
  arrange(family, full_name) %>%
  dplyr::select(family, full_name, order(colnames(.))) 

}

## ---- Language ---------------------------------------------------------------
lan.exTab <- exwasTab(lan.fit %>% dplyr::filter(converged == TRUE))
write_csv(lan.exTab, file = here("tables", "lan_exwas.csv"))

## ---- Non-verbal intelligence ------------------------------------------------
nvi.exTab <- exwasTab(nvi.fit %>% dplyr::filter(converged == TRUE))
write_csv(nvi.exTab, file = here("tables", "nvi_exwas.csv"))

## ---- Working memory ---------------------------------------------------------
wm.exTab <- exwasTab(wm.fit %>% dplyr::filter(converged == TRUE))
write_csv(wm.exTab, file = here("tables", "wm_exwas.csv"))

## ---- Fine motor -------------------------------------------------------------
fm.exTab <- exwasTab(fm.fit %>% dplyr::filter(converged == TRUE))
write_csv(fm.exTab, file = here("tables", "fm_exwas.csv"))

## ---- Gross motor ------------------------------------------------------------
gm.exTab <- exwasTab(gm.fit %>% dplyr::filter(converged == TRUE))
write_csv(gm.exTab, file = here("tables", "gm_exwas.csv"))










################################################################################
# Language: NINFEA  
################################################################################
ds.colnames("lan_sub")

ninfea_lan <- dh.getStats(
  df = "lan_sub",
  vars = c("lan_raw_.0_17", "lan_age_.0_17"))

ninfea_lan.tab <- ninfea_lan$continuous %>%
  dplyr::filter(cohort == "ninfea") %>%
  dplyr::select(variable, mean, std.dev, valid_n, cohort_n, missing_n, 
                missing_perc)

write_csv(ninfea_lan.tab, file = here("tables", "ninfea_lan.csv"))

################################################################################
# Covariates: NINFEA  
################################################################################
ds.colnames("covariates")

ninfea_covs <- dh.getStats(
  df = "covariates", 
  vars = c("agebirth_m_y", "sex", "parity_bin", "birth_month_f", "edu_m_.0_1",       
           "areases_tert_.0_1"))

ninfea_cat_covs.tab <- ninfea_covs$categorical %>% 
  dplyr::filter(cohort == "ninfea") %>%
  dplyr::select(variable, category, value, valid_n, cohort_n, perc_total)

ninfea_cont_covs.tab <- ninfea_covs$continuous %>%
  dplyr::filter(cohort == "ninfea") %>%
  dplyr::select(variable, mean, std.dev, valid_n, cohort_n, missing_n, 
                missing_perc)

write_csv(ninfea_cat_covs.tab, file = here("tables", "ninfea_cat_covs.csv"))
write_csv(ninfea_cont_covs.tab, file = here("tables", "ninfea_cont_covs.csv"))



ds.colnames("lan_sub")


lan_hist <- ds.histogram("lan_sub$lan_raw_.0_17")

dh.boxCox(
  df = "lan_sub", 
  var = "lan_raw_.0_17", 
  lamda = seq(-2, 2, 0.1),
  type = "combine",
  transform = TRUE,
  new_obj = "lan_trans", 
  checks = FALSE)

ds.histogram("lan_trans")


