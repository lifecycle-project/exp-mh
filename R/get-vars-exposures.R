################################################################################
## Project: Exposome and Mental Health    
## Script purpose: Prepare exposures 
## Date: 13th April 2022
## Author: Tim Cadman
## Email: tica@sund.ku.dk
################################################################################

################################################################################
# 1. Remove lden for cohorts it shouldn't exist for  
################################################################################
wrong_noise <- c("alspac", "bib", "inma_gip", "inma_sab")

dh.dropCols(
  df = "yearrep", 
  vars = "lden_", 
  type = "remove", 
  conns = conns[wrong_noise]
)

length_ref <- tibble(
  cohort = wrong_noise,
  length = ds.dim(
    x = "yearrep",
    type = "split", 
    datasources = conns[wrong_noise]) %>%
    map(~.[[1]]) %>%
    unlist %>%
    as.integer)

length_ref %>%
  pmap(function(cohort, length){
    ds.rep(
      x1 = NA, 
      times = length,
      source.times = "c",
      newobj = "lden_", 
      datasources = conns[cohort])
  })

ds.dataFrame(
  x = c("yearrep", "lden_"),
  newobj = "yearrep", 
  datasources = conns[wrong_noise]
)

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "exp-mh")
conns <- datashield.login(logindata, restore = "exp-mh")


################################################################################
# 2. Collapse top two levels of lden  
################################################################################
ds.recodeLevels(
  x = "nonrep$lden_c_preg", 
  newCategories = c(1, 2, 3, 4, 5, 5), 
  newobj = "lden_preg_f"
)

ds.dataFrame(
  x = c("nonrep", "lden_preg_f"), 
  newobj = "nonrep"
)

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "exp-mh")
conns <- datashield.login(logindata, restore = "exp-mh")

################################################################################
# 3. Create IQR versions of variables  
################################################################################
ds.dataFrameFill("analysis_df", "analysis_df")

iqr.vars <- bind_rows(exp_preg.ref, exp_year_1.ref) %>%
  dplyr::filter(type == "cont") %>%
  pull(variable)

dh.makeIQR(
  df = "analysis_df", 
  vars = iqr.vars, 
  type = "split")

dh.makeIQR(
  df = "analysis_df", 
  vars = iqr.vars, 
  type = "combine")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "exp-mh")
conns <- datashield.login(logindata, restore = "exp-mh")

################################################################################
# 4. Create quartiles of continuous variables
################################################################################

quart.ref <- tibble()

## ---- NO2 --------------------------------------------------------------------
dh.quartileSplit(
  df = "analysis_df",
  var = "no2_preg_iqr_c",
  type = "split",
  band_action = "ge_l",
  var_suffix = "_q_c_")

## ---- PM2.5 ------------------------------------------------------------------
dh.quartileSplit(
  df = "analysis_df",
  var = "pm25_preg_iqr_c",
  type = "split",
  band_action = "ge_l",
  var_suffix = "_q_c_")

## ---- PM10 -------------------------------------------------------------------
dh.quartileSplit(
  df = "analysis_df",
  var = "pm10_preg_iqr_c",
  type = "split",
  band_action = "ge_l",
  var_suffix = "_q_c_")

## ---- NDVI -------------------------------------------------------------------
dh.quartileSplit(
  df = "analysis_df",
  var = "ndvi300_preg_iqr_c",
  type = "split",
  band_action = "ge_l",
  var_suffix = "_q_c_")

## ---- Facility richness ------------------------------------------------------
dh.quartileSplit(
  df = "analysis_df",
  var = "frichness300_preg_iqr_c",
  type = "split",
  band_action = "ge_l",
  var_suffix = "_q_c_")

## ---- Walkability ------------------------------------------------------------
dh.quartileSplit(
  df = "analysis_df",
  var = "walkability_mean_preg_iqr_c",
  type = "split",
  band_action = "ge_l",
  var_suffix = "_q_c_")

## ---- Population density -----------------------------------------------------
dh.quartileSplit(
  df = "analysis_df",
  var = "popdens_preg_iqr_c",
  type = "split",
  band_action = "ge_l",
  var_suffix = "_q_c_")

################################################################################
# 5. Fill missing variables
################################################################################
ds.dataFrameFill("analysis_df", "analysis_df")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "exp-mh")
conns <- datashield.login(logindata, restore = "exp-mh")  