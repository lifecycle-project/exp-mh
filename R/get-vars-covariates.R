################################################################################
## Project: Exposome and Mental Health    
## Script purpose: Prepare covariates
## Date: 13th April 2022
## Author: Tim Cadman
## Email: tica@sund.ku.dk
################################################################################


################################################################################
# 6. Recode parity as binary
################################################################################
ds.asNumeric("nonrep$parity", "parity")

ds.Boole(
  V1 = "parity",
  V2 = 1,
  Boolean.operator = ">",
  newobj = "parity_bin"
)

ds.asFactor("parity_bin", "parity_bin")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_5")
conns <- datashield.login(logindata, restore = "env_pnd_5")

################################################################################
# 7. Recode birth month
################################################################################
ds.asNumeric("nonrep$birth_month", "birth_month")

season_ref <- tibble(
  old_val = seq(1, 12),
  new_val = c(
    rep("spring", 3),
    rep("summer", 3),
    rep("autumn", 3), 
    rep("winter", 3))
)

season_ref %>% 
  pmap(function(old_val, new_val){
    
    ds.recodeValues(
      var.name = "birth_month",
      values2replace.vector = old_val,
      new.values.vector = new_val,
      newobj = "birth_month")
  })

ds.asFactor("birth_month", "birth_month_f")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_6")
conns <- datashield.login(logindata, restore = "env_pnd_6")

################################################################################
# 8. Recode birth year
################################################################################
ds.asNumeric("nonrep$birth_year", "birth_year_c")

ds.table("birth_year_c")

year_ref <- tibble(
  old_val = c(
    1990, 1991, 1992, 1993, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 
    2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017),
  new_val = c(
    "90_95", "90_95", "90_95", "90_95", "96_00", "96_00", "96_00", "96_00", 
    "96_00", "01_05", "01_05", "01_05", "01_05", "01_05", "05_11", "05_11", 
    "05_11", "05_11", "05_11", "05_11", "05_11", "05_11", "05_11", "05_11", 
    "05_11", "05_11")
)

year_ref %>% 
  pmap(function(old_val, new_val){
    
    ds.recodeValues(
      var.name = "birth_year_c",
      values2replace.vector = old_val,
      new.values.vector = new_val,
      newobj = "birth_year_c", 
      datasources = conns[names(conns) != "rhea"])
    
  })

ds.asFactor("birth_year_c", "birth_year_f")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_7")
conns <- datashield.login(logindata, restore = "env_pnd_7")

################################################################################
# 9. Recode maternal age at birth
################################################################################
ds.asNumeric("nonrep$agebirth_m_y", "mat_age")

mat_age_ref <- tibble(
  old_val = c(
    seq(15, 20, 1),
    seq(21, 25, 1), 
    seq(26, 30, 1), 
    seq(31, 35, 1), 
    seq(36, 40, 1), 
    c(seq(41, 50, 1), 55)),
  new_val = c(
    rep("15_20", 6), 
    rep("21_25", 5), 
    rep("26_30", 5), 
    rep("31_35", 5),
    rep("36_40", 5),
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

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_8")
conns <- datashield.login(logindata, restore = "env_pnd_8")

################################################################################
# 10. Create preterm birth variable
################################################################################
ds.assign(
  toAssign = "nonrep$ga_bj", 
  newobj = "ga_all",
  datasources = conns[!conns == "moba"]
) 

ds.assign(
  toAssign = "nonrep$ga_us", 
  newobj = "ga_all",
  datasources = conns["moba"]
)

ds.Boole(
  V1 = "ga_all",
  V2 = 37*7,
  Boolean.operator = ">",
  newobj = "ga_bin"
)

ds.asFactor("ga_bin", "preterm")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_9")
conns <- datashield.login(logindata, restore = "env_pnd_9")

################################################################################
# 14. Create exposure for maternal education at birth  
################################################################################

## ---- Subset to keep observations where child's age == 0 ---------------------
ds.dataFrameSubset(
  df.name = "yearrep", 
  V1.name = "yearrep$age_years",
  V2.name = "0",
  Boolean.operator = "==",
  newobj = "mat_ed_tmp")

## ---- Convert to wide format -------------------------------------------------
ds.reShape(
  data.name = "mat_ed_tmp",
  timevar.name = "age_years",
  idvar.name = "child_id",
  v.names = "edu_m_", 
  direction = "wide", 
  newobj = "mat_ed")

dh.dropCols(
  df = "mat_ed", 
  vars = c("child_id", "edu_m_.0"),
  type = "keep"
)

## ---- Rename -----------------------------------------------------------------
dh.renameVars(
  df = "mat_ed", 
  current_names = "edu_m_.0",
  new_names = "edu_m_0",
  conns = conns)

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_13")
conns <- datashield.login(logindata, restore = "env_pnd_13")

################################################################################
# 16. Create combined area deprivation variable  
################################################################################

# MoBa only has it in the first year of birth.
ds.assign(
  toAssign = "env_pnd$areases_tert_preg", 
  newobj = "areases_tert",
  datasources = conns[names(conns) != "moba"]
) 

ds.assign(
  toAssign = "env_pnd$areases_tert_1", 
  newobj = "areases_tert",
  datasources = conns["moba"]
)

## ---- Join back in -----------------------------------------------------------
ds.dataFrame(
  x = c("env_pnd", "areases_tert"), 
  newobj = "env_pnd")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_15")
conns <- datashield.login(logindata, restore = "env_pnd_15")

