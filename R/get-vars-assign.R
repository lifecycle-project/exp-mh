################################################################################
## Project: Exposome and Mental Health    
## Script purpose: Assign variables and check 
## Date: 13th April 2022
## Author: Tim Cadman
## Email: tica@sund.ku.dk
################################################################################

################################################################################
# 1. Define variables
################################################################################

## ---- Variable details -------------------------------------------------------
nonrep.vars <- c(
  "areases_tert_preg", "areases_quint_preg", "parity_m", "sex", 
  "birth_month", "birth_year", "eusilc_income_quintiles", "agebirth_m_y", 
  "ethn1_m", "ethn2_m", "ethn3_m", "coh_country", "preg_smk", "preg_alc", 
  "preg_cig", "preg_alc_unit", "breastfed_any", "breastfed_ever", "no2_preg", 
  "pm25_preg", "lden_preg", "ndvi100_preg", "ndvi300_preg", "ndvi500_preg", 
  "green_dist_preg", "blue_dist_preg", "cohort_id", "ppd", "preg_dia", 
  "preg_ht", "ga_bj", "ga_us", "prepreg_dep", "prepreg_anx", "prepreg_psych", 
  "preg_psych", "child_id", "child_no", "preg_no", "mother_id", "outcome", 
  "con_anomalies", "bdens100_preg", "bdens300_preg", "fdensity300_preg", 
  "frichness300_preg", "landuseshan300_preg", "walkability_mean_preg", 
  "agrgr_preg", "natgr_preg", "urbgr_preg", "urb_area_id", "lden_c_preg", 
  "greenyn300_preg", "blueyn300_preg", "popdens_preg", "pm10_preg")

yearrep.vars <- c(
  "child_id", "edu_m_", "areases_tert_", "areases_quint_", "fam_splitup", 
  "no2_", "pm25_", "lden_", "ndvi300_", "green_dist_", "blue_dist_", 
  "age_years", "cohab_", "bdens100_", "bdens300_", "urbgr_", "natgr_", 
  "agrgr_", "walkability_mean_", "landuseshan300_", "frichness300_", 
  "fdensity300_", "lden_c_", "greenyn300_", "blueyn300_", "popdens_", "pm10_")

################################################################################
# 2. Define tables
################################################################################
cohorts_tables <- bind_rows(
  tibble(
    cohort = "alspac",
    table = c(
      "alspac/2_1_core_1_4/non_rep",
      "alspac/2_1_core_1_4/yearly_rep")),
  tibble(
    cohort = "bib",
    table = c(
      "sp455/2_2_core_1_4/non_rep",
      "sp455/2_2_core_1_4/yearly_rep")),
  tibble(
    cohort = "dnbc",
    table = c(
      "lc_dnbc_core_2_2.2_2_core_non_rep_tcadman_2021-lc08",
      "lc_dnbc_core_2_2.2_2_core_yearly_rep_tcadman_2021-lc08")),
  tibble(
    cohort = "eden_nan",
    table = c(
      "project22-eden/2_1_core_1_0/non_rep", 
      "project22-eden/2_1_core_1_0/yearly_rep")),
  tibble(
    cohort = "eden_poit",
    table = c(
      "project22-eden/2_1_core_1_0/non_rep", 
      "project22-eden/2_1_core_1_0/yearly_rep")),
  tibble(
    cohort = "inma_gip",
    table = c(
      "lc_isglobal_core_2_1.2_1_core_1_1_non_rep_210118_1", 
      "lc_isglobal_core_2_1.2_1_core_1_1_yearly_rep_210118_1")),
  tibble(
    cohort = "inma_sab",
    table = c(
      "lc_isglobal_core_2_1.2_1_core_1_1_non_rep_210118_1", 
      "lc_isglobal_core_2_1.2_1_core_1_1_yearly_rep_210118_1")),
  tibble(
    cohort = "genr",
    table = c(
      "lc_genr_core_2_2.2_1_core_non_rep_TC _ECCNLC202053", 
      "lc_genr_core_2_2.2_1_core_yearly_rep_TC_ECCNLC202053")),
  tibble(
    cohort = "moba",
    table = c(
      "lc_moba_core_2_1.2_1_core_2021_7_non_rep_urban_environment_postnatal_depression", 
      "lc_moba_core_2_1.2_1_core_2021_7_yearly_rep_urban_environment_postnatal_depression")),
  tibble(
    cohort = "ninfea",
    table = c(
      "lc_ninfea_core_2_1.p12_tcadman", 
      "lc_ninfea_core_2_1.p12_tcadman_yearly_rep")),
  tibble(
    cohort = "rhea",
    table = c(
      "lc_rhea_core_2_1.tcadman_nr", 
      "lc_rhea_core_2_1.tcadman_y"))) %>%
  mutate(type = rep(c("nonrep", "yearrep"), nrow(.)/2))

################################################################################
# 3. Assign variables
################################################################################
cohorts_tables %>%
  dplyr::filter(cohort %in% c("eden_nan", "eden_poit")) %>%
  pwalk(function(cohort, table, type){
    
    datashield.assign(
      conns = conns[cohort], 
      symbol = type, 
      value = table, 
      variables = eval(parse(text = paste0(type, ".vars"))))
  })

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "exp-mh")
conns <- datashield.login(logindata, restore = "exp-mh")

################################################################################
# 4. Check data
################################################################################
available <- list(
  nonrep = dh.classDiscrepancy(
    df = "nonrep",
    vars = nonrep.vars), 
  yearrep = dh.classDiscrepancy(
    df = "yearrep",
    vars = yearrep.vars
  ))

save.image()

################################################################################
# 5. Fill missing variables  
################################################################################
ds.dataFrameFill("nonrep", "nonrep")
ds.dataFrameFill("yearrep", "yearrep")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "exp-mh")
conns <- datashield.login(logindata, restore = "exp-mh")

################################################################################
# 6. Check filled data
################################################################################
filled <- list(
  nonrep = dh.classDiscrepancy(
    df = "nonrep", 
    vars = nonrep.vars), 
  yearrep = dh.classDiscrepancy(
    df = "yearrep", 
    vars = yearrep.vars))

filled$nonrep %>% dplyr::filter(discrepancy == "yes")
filled$yearrep %>% dplyr::filter(discrepancy == "yes")

save.image()

################################################################################
# 7. Create cohort dummy variables  
################################################################################
ds.dataFrameFill("env_pnd", "env_pnd")
ds.asFactor("env_pnd$cohort_id", "cohort_id")
dh.dropCols(df = "env_pnd", vars = "cohort_id", type = "remove")
ds.dataFrame(x = c("env_pnd", "cohort_id"), newobj = "env_pnd")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_16a")

## ---- Get cohort codes -------------------------------------------------------
coh_codes <- dh.getStats(
  df = "env_pnd",
  vars = "cohort_id", 
  conns = conns)

coh_codes.tab <- coh_codes$categorical %>% 
  dplyr::filter(value != 0 & !cohort %in% c(sub_coh, "combined")) %>%
  mutate(ref_var = "cohort_id") %>%
  dplyr::select(category, cohort, ref_var)

## ---- Get urban ID codes -----------------------------------------------------
urb_codes <- dh.getStats(
  df = "env_pnd",
  vars = "urb_area_id", 
  conns = conns)

urb_codes.tab <- urb_codes$categorical %>% 
  dplyr::filter(value != 0 & cohort %in% sub_coh) %>%
  mutate(ref_var = "urb_area_id") %>%
  dplyr::select(category, cohort, ref_var)

ref_codes <- bind_rows(coh_codes.tab, urb_codes.tab) %>%
  mutate(
    dummy = paste0(cohort, "_d"), 
    value = as.character(category)) %>%
  dplyr::select(dummy, value, ref_var)

## ---- Make dummy variable ----------------------------------------------------
ref_codes %>%
  pmap(function(variable, dummy, value, ref_var){
    ds.Boole(
      V1 = paste0("env_pnd$", ref_var), 
      V2 = value,
      Boolean.operator = "==",
      numeric.output = TRUE, 
      na.assign = 0, 
      newobj = dummy)
  })

## ---- Add the cohort dummy variables -----------------------------------------
ds.dataFrame(
  x = c('env_pnd', ref_codes$dummy), 
  newobj = 'env_pnd'
)

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "exp-mh")
conns <- datashield.login(logindata, restore = "exp-mh")

################################################################################
# 8. Create sub-cohorts for inma and eden  
################################################################################
sub_coh <- c("inma_gip", "inma_sab", "eden_nan", "eden_poit")

tibble(
  cohort = sub_coh,
  value = c("1102", "1103", "1801", "1802")) %>%
  pmap(function(cohort, value){
    
    ds.dataFrameSubset(
      df.name = "env_pnd", 
      V1.name = "env_pnd$urb_area_id",
      V2.name = value,
      Boolean.operator = "==",
      newobj = "env_pnd", 
      datasources = conns[cohort])
    
  })

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "exp-mh")
conns <- datashield.login(logindata, restore = "exp-mh")




