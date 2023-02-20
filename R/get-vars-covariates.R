################################################################################
## Project: Exposome and Mental Health    
## Script purpose: Prepare covariates
## Date: 13th April 2022
## Author: Tim Cadman
## Email: tica@sund.ku.dk
################################################################################

conns <- datashield.login(logindata, restore = "exp-mh")
################################################################################
# 1. Recode parity as binary
################################################################################
ds.asNumeric("non_rep_sub$parity", "parity")

ds.Boole(
  V1 = "parity",
  V2 = 1,
  Boolean.operator = ">",
  newobj = "parity_bin")

ds.asFactor("parity_bin", "parity_bin")

datashield.workspace_save(conns, "exp-mh")

################################################################################
# 2. Recode birth month
################################################################################
ds.assign("birth_month", "birth_month_orig")
dh.dropCols(
  df = "baseline_df", 
  vars = "birth_month", 
  type = "remove")

ds.dataFrame(c("baseline_df", "birth_month_orig"), "baseline_df"))

ds.asNumeric("non_rep_sub$birth_month", "birth_month")

season_ref <- tibble(
  old_val = seq(1, 12),
  new_val = c(
    rep("spring", 3),
    rep("summer", 3),
    rep("autumn", 3), 
    rep("winter", 3)))

season_ref %>% 
  pmap(function(old_val, new_val){
    
    ds.recodeValues(
      var.name = "birth_month",
      values2replace.vector = old_val,
      new.values.vector = new_val,
      newobj = "birth_month")
  })

ds.asFactor("birth_month", "birth_month_f")

datashield.workspace_save(conns, "exp-mh")
################################################################################
# 3. Recode birth year
################################################################################
ds.asNumeric("non_rep_sub$birth_year", "birth_year_c")

ds.summary("birth_year_c")
ds.summary("non_rep_sub$birth_year")
ds.colnames("non_rep_sub")

year_ref <- tibble(
  old_val = c(
    1990, 1991, 1992, 1993, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 
    2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017),
  new_val = c(
    "90_95", "90_95", "90_95", "90_95", "96_00", "96_00", "96_00", "96_00", 
    "96_00", "01_05", "01_05", "01_05", "01_05", "01_05", "05_11", "05_11", 
    "05_11", "05_11", "05_11", "05_11", "05_11", "05_11", "05_11", "05_11", 
    "05_11", "05_11"))

year_ref %>% 
  pmap(function(old_val, new_val){
    
    ds.recodeValues(
      var.name = "birth_year_c",
      values2replace.vector = old_val,
      new.values.vector = new_val,
      newobj = "birth_year_c", 
      datasources = conns[names(conns)])
    
  })

ds.asFactor("birth_year_c", "birth_year_f")

datashield.workspace_save(conns, "exp-mh")
################################################################################
# 4. Recode maternal age at birth
################################################################################
ds.asNumeric("non_rep_sub$agebirth_m_y", "mat_age")

mat_age_ref <- tibble(
  old_val = c(
    seq(15, 20, 1),
    seq(21, 30, 1), 
    seq(31, 40, 1), 
    c(seq(41, 50, 1), 55)),
  new_val = c(
    rep("15_20", 6), 
    rep("21_25", 10), 
    rep("31_40", 10), 
    rep("41_50", 11)
    )
)

mat_age_ref %>% 
  pmap(function(old_val, new_val){
    
    ds.recodeValues(
      var.name = "mat_age",
      values2replace.vector = old_val,
      new.values.vector = new_val,
      newobj = "mat_age")
    
  })

ds.asFactor("mat_age", "mat_age_f")

datashield.workspace_save(conns, "exp-mh")
################################################################################
# 5. Create preterm birth variable
################################################################################
ds.assign(
  toAssign = "non_rep_sub$ga_bj", 
  newobj = "ga_all",
  datasources = conns[!conns == "moba"]) 

ds.assign(
  toAssign = "non_rep_sub$ga_us", 
  newobj = "ga_all",
  datasources = conns["moba"])

ds.Boole(
  V1 = "ga_all",
  V2 = 37*7,
  Boolean.operator = ">",
  newobj = "ga_bin")

ds.asFactor("ga_bin", "preterm")

datashield.workspace_save(conns, "exp-mh")
################################################################################
# 6. Create exposure for maternal education at birth  
################################################################################
dh.makeStrata(
  df = "year_rep_sub", 
  var_to_subset = "edu_m_",
  id_var = "child_id",
  age_var = "age_years",
  bands = c(0, 1), 
  band_action = "ge_le", 
  mult_action = "earliest",
  new_obj = "mat_ed")

dh.dropCols(
  df = "mat_ed", 
  vars = c("child_id", "edu_m_.0_1"),
  type = "keep", 
  checks = F)

ds.summary("mat_ed$edu_m_.0_1")



datashield.workspace_save(conns, "exp-mh")
################################################################################
# 7. Create combined area deprivation variables  
################################################################################

## ---- All cohorts except MoBa and Rhea ---------------------------------------
ds.asNumeric("non_rep_sub$areases_tert_preg", "area_ses_tert")

ds.recodeValues(
  var.name = "area_ses_tert",
  values2replace.vector = c(1, 2, 3),
  new.values.vector = c(1, 2, 2),
  newobj = "area_ses_tert", 
  datasources = conns[!names(conns) %in% c("rhea", "moba")])
   
ds.asFactor("area_ses_tert", "area_ses_tert_f",  
            datasources = conns[!names(conns) %in% c("moba", "rhea")]) 
   
## ---- MoBa -------------------------------------------------------------------
dh.makeStrata(
  df = "year_rep_sub", 
  var_to_subset = "area_ses_tert_",
  id_var = "child_id",
  age_var = "age_years",
  bands = c(0, 1), 
  band_action = "ge_le", 
  mult_action = "earliest",
  new_obj = "area_dep", 
  conns = conns["moba"])

dh.dropCols(
  df = "area_dep", 
  vars = c("child_id", "edu_m_.0_1"),
  type = "keep", 
  checks = F)

################################################################################
# Recode ethnicity  
################################################################################
eth_coh <- names(conns)[!names(conns) %in% c("dnbc", "moba", "ninfea", "rhea")]

ds.asNumeric("non_rep_sub$ethn3_m", "eth_bin")

ds.recodeValues(
  var.name = "eth_bin",
  values2replace.vector = seq(1, 3, 1),
  new.values.vector = c(1, 2, 2),
  newobj = "eth_bin")

ds.asFactor("eth_bin", "eth_bin")

datashield.workspace_save(conns, "exp-mh")

ds.summary("non_rep_sub$ethn3_m")
ds.class("non_rep_sub$ethn3_m")

ds.colnames("non_rep_sub")
