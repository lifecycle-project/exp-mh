################################################################################
## Project: Exposome and Mental Health    
## Script purpose: Prepare exposures 
## Date: 13th April 2022
## Author: Tim Cadman
## Email: tica@sund.ku.dk
################################################################################

withr::with_libpaths(
  new = "~/R/userlib",
  devtools::install_github("lifecycle-project/ds-helper", ref = "dev"))

library(dsExposomeClient)
conns <- datashield.login(logindata, restore = "exp-mh")

ds.colnames("non_rep_sub")

ds.summary("non_rep_sub$ndvi300_preg")

################################################################################
# 6. Transform variables  
################################################################################

## ---- Facility density -------------------------------------------------------
ds.assign("non_rep_sub$fdensity300_preg*0.4", newobj = "fdens_trans")

## ---- Facility richness ------------------------------------------------------
ds.assign("non_rep_sub$frichness300_preg*0.6", newobj = "frich_trans")

## ---- High-density residential -----------------------------------------------
ds.assign("non_rep_sub$hdres_preg*1.4", newobj = "hdres_trans")

## ---- Industrial -------------------------------------------------------------
ds.assign("non_rep_sub$indtr_preg*0.4", newobj = "indtr_trans")

## ---- Low-density residential ------------------------------------------------
ds.assign("non_rep_sub$ldres_preg*0.4", newobj = "ldres_trans")

## ---- Bus lines 500 ----------------------------------------------------------
ds.assign("non_rep_sub$bus_lines_500_preg*0.6", newobj = "bl_500_trans")

## ---- agrgr preg -------------------------------------------------------------
ds.assign("non_rep_sub$agrgr_preg*0.2", newobj = "agrgr_trans")

## ---- Bus lines 300 ----------------------------------------------------------
ds.assign("non_rep_sub$bus_lines_300_preg*0.6", newobj = "bl_300_trans")

## ---- Foodenvdens300_preg -------------------------------------------------------------------
ds.assign("non_rep_sub$foodenvdens300_preg*0.4", newobj = "food_env_trans")

## ---- trafnear_preg -------------------------------------------------------------------
ds.assign("log(non_rep_sub$trafnear_preg)", newobj = "traf_near_trans")

## ---- urbgr_preg -------------------------------------------------------------------
ds.assign("non_rep_sub$urbgr_preg*0.4", newobj = "urbgr_trans")

## ---- Create summary of transformations --------------------------------------
trans_sum <- tibble(
  original = c("fdensity300_preg", "frichness300_preg", "hdres_preg", 
               "indtr_preg", "ldres_preg", "bus_lines_500_preg", "agrgr_preg", 
               "bus_lines_300_preg", "foodenvdens300_preg", "trafnear_preg",
               "urbgr_preg"),
  transformed = c("fdens_trans", "frich_trans", "hdres_trans", "indtr_trans",
                  "ldres_trans", "bl_500_trans", "agrgr_trans", "bl_300_trans",
                  "food_env_trans", "traf_near_trans", "urbgr_trans"))

save.image()
datashield.workspace_save(conns, "exp-mh")










## ---- Facility density -------------------------------------------------------
ds.assign("non_rep_sub$fdensity300_preg*0.4", newobj = "fdens_trans")

fdens_pre <- ds.histogram("non_rep_sub$fdensity300_preg")

fdens_trans <- dh.boxCox(
  df = "non_rep_sub", 
  var = "fdensity300_preg",
  lamda = seq(-2, 2, 0.2),
  type = "combine",
  transform = TRUE,
  new_obj = "fdens_trans", 
  checks = FALSE)

fdens_post <- ds.histogram("fdens_trans")
datashield.workspace_save(conns, "exp-mh")

## ---- Facility richness ------------------------------------------------------
frich_pre <- ds.histogram("non_rep_sub$frichness300_preg")

frich_trans <- dh.boxCox(
  df = "non_rep_sub", 
  var = "frichness300_preg", 
  lamda = seq(-2, 2, 0.2),
  type = "combine",
  transform = TRUE,
  new_obj = "frich_trans", 
  checks = FALSE)

frich_post <- ds.histogram("frich_trans")
datashield.workspace_save(conns, "exp-mh")

## ---- High-density residential -----------------------------------------------
hdres_pre <- ds.histogram("non_rep_sub$hdres_preg")

hdres_trans <- dh.boxCox(
  df = "non_rep_sub", 
  var = "hdres_preg", 
  lamda = seq(-2, 2, 0.2),
  type = "combine",
  transform = TRUE,
  new_obj = "hdres_trans", 
  checks = FALSE)

hdres_post <- ds.histogram("hdres_trans")
datashield.workspace_save(conns, "exp-mh")
## ---- Industrial -------------------------------------------------------------
indtr_pre <- ds.histogram(
  "non_rep_sub$indtr_preg", 
  datasources = conns[names(conns) != "inma_gip"])

ds.summary("non_rep_sub$indtr_preg")

indtr_trans <- dh.boxCox(
  df = "non_rep_sub", 
  var = "indtr_preg", 
  lamda = seq(-2, 2, 0.2),
  type = "combine",
  transform = TRUE,
  new_obj = "indtr_trans", 
  checks = FALSE, 
  conns = conns[names(conns) != "inma_gip"])

ds.assign(
  toAssign = "non_rep_sub$indtr_preg", 
  newobj = "indtr_trans", 
  datasources = conns["inma_gip"])

indtr_post <- ds.histogram(
  "indtr_trans", 
  datasources = conns[names(conns) != "inma_gip"])

datashield.workspace_save(conns, "exp-mh")
## ---- Low-density residential ------------------------------------------------
ldres_pre <- ds.histogram(
  "non_rep_sub$ldres_preg", 
  datasources = conns["ninfea"])

ldres_trans <- dh.boxCox(
  df = "non_rep_sub", 
  var = "ldres_preg", 
  lamda = seq(-2, 2, 0.2),
  type = "combine",
  transform = TRUE,
  new_obj = "ldres_trans", 
  checks = FALSE, 
  conns = conns["ninfea"])

ldres_post <- ds.histogram(
  "ldres_trans", 
  datasources = conns["ninfea"])

ds.assign(
  toAssign = "non_rep_sub$ldres_preg", 
  newobj = "ldres_trans", 
  datasources = conns[names(conns) %in% c("inma_gip", "inma_val", "inma_sab")])

datashield.workspace_save(conns, "exp-mh")
## ---- Transport --------------------------------------------------------------
#ds.summary("non_rep_sub$trans_preg")

#trans_pre <- ds.histogram("non_rep_sub$trans_preg")

#trans_trans <- dh.boxCox(
#  df = "non_rep_sub", 
#  var = "trans_preg", 
#  lamda = seq(-2, 2, 0.1),
#  type = "combine",
#  transform = TRUE,
#  new_obj = "trans_trans", 
#  checks = FALSE)

#trans_post <- ds.histogram("trans_trans")

## ---- Bus lines 500 ----------------------------------------------------------
bus_lines_500_pre <- ds.histogram("non_rep_sub$bus_lines_500_preg")

bus_lines_500_trans <- dh.boxCox(
  df = "non_rep_sub", 
  var = "bus_lines_500_preg", 
  lamda = seq(-2, 2, 0.2),
  type = "combine",
  transform = TRUE,
  new_obj = "bl_500_trans", 
  checks = FALSE)

bus_lines_500_post <- ds.histogram("bl_500_trans")
datashield.workspace_save(conns, "exp-mh")
## ---- agrgr preg -------------------------------------------------------------
agrgr_pre <- ds.histogram("non_rep_sub$agrgr_preg")

agrgr_trans <- dh.boxCox(
  df = "non_rep_sub", 
  var = "agrgr_preg", 
  lamda = seq(-2, 2, 0.2),
  type = "combine",
  transform = TRUE,
  new_obj = "agrgr_trans", 
  checks = FALSE)

agrgr_post <- ds.histogram("agrgr_trans")
datashield.workspace_save(conns, "exp-mh")
## ---- Bus lines 300 ----------------------------------------------------------
bl_300_pre <- ds.histogram("non_rep_sub$bus_lines_300_preg")

bl_300_trans <- dh.boxCox(
  df = "non_rep_sub", 
  var = "bus_lines_300_preg", 
  lamda = seq(-2, 2, 0.2),
  type = "combine",
  transform = TRUE,
  new_obj = "bl_300_trans", 
  checks = FALSE)

bl_300_pre <- ds.histogram("bl_300_trans")
datashield.workspace_save(conns, "exp-mh")

## ---- Foodenvdens300_preg -------------------------------------------------------------------
food_env_pre <- ds.histogram("non_rep_sub$foodenvdens300_preg")

food_env_trans <- dh.boxCox(
  df = "non_rep_sub", 
  var = "foodenvdens300_preg", 
  lamda = seq(-2, 2, 0.2),
  type = "combine",
  transform = TRUE,
  new_obj = "food_env_trans", 
  checks = FALSE)

food_env_post <- ds.histogram("food_env_trans")
datashield.workspace_save(conns, "exp-mh")

## ---- trafnear_preg -------------------------------------------------------------------
traf_near_pre <- ds.histogram("non_rep_sub$trafnear_preg")

traf_near_trans <- dh.boxCox(
  df = "non_rep_sub", 
  var = "trafnear_preg", 
  lamda = seq(-2, 2, 0.2),
  type = "combine",
  transform = TRUE,
  new_obj = "traf_near_trans", 
  checks = FALSE)

traf_near_post <- ds.histogram("traf_near_trans")
datashield.workspace_save(conns, "exp-mh")

## ---- urbgr_preg -------------------------------------------------------------------
urbgr_pre <- ds.histogram("non_rep_sub$urbgr_preg")

urbgr_trans <- dh.boxCox(
  df = "non_rep_sub", 
  var = "urbgr_preg", 
  lamda = seq(-2, 2, 0.2),
  type = "combine",
  transform = TRUE,
  new_obj = "urbgr_trans", 
  checks = FALSE)

urbgr_pre <- ds.histogram("urbgr_trans")
datashield.workspace_save(conns, "exp-mh")

## ---- Create summary of transformations --------------------------------------
trans_sum <- tibble(
  original = c("fdensity300_preg", "frichness300_preg", "hdres_preg", 
               "indtr_preg", "ldres_preg", "bus_lines_500_preg", "agrgr_preg", 
               "bus_lines_300_preg", "foodenvdens300_preg", "trafnear_preg",
               "urbgr_preg"),
  transformed = c("fdens_trans", "frich_trans", "hdres_trans", "indtr_trans",
                  "ldres_trans", "bl_500_trans", "agrgr_trans", "bl_300_trans",
                  "food_env_trans", "traf_near_trans", "urbgr_trans"))

save.image()

datashield.workspace_save(conns, "exp-mh")

