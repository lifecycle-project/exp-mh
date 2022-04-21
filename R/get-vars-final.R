################################################################################
## Project: Exposome and Mental Health    
## Script purpose: Create final dataset
## Date: 13th April 2022
## Author: Tim Cadman
## Email: tica@sund.ku.dk
################################################################################

conns <- datashield.login(logindata, restore = "exp-mh")
################################################################################
# 1. Merge all created objects
################################################################################

## ---- Join with ds.dataFrame -------------------------------------------------
ds.dataFrame(
  x = c("non_rep", "parity_bin", "birth_month_f", "mat_age_f", "preterm", 
        dummy),
  newobj = "wide_merge")

## ---- Merge wide format objects ----------------------------------------------
merge_ref <- c("mat_ed", "area_ses", "iqr_split", "iqr_combine")

merge_ref %>%
  map(
    ~ds.merge(
      x.name = "wide_merge",
      y.name = .x,
      by.x.names = "child_id", 
      by.y.names = "child_id",
      all.x = T,
      all.y = T,
      newobj = "wide_merge"))

## ---- Merge into long format data --------------------------------------------
ds.merge(
  x.name = "mh_rep",
  y.name = "wide_merge",
  by.x.names = "child_id", 
  by.y.names = "child_id",
  all.x = T,
  all.y = T,
  newobj = "exp_mh")
  
datashield.workspace_save(conns, "exp-mh")
################################################################################
# 2. Fix factor variables   
################################################################################
factor <- dh.classDiscrepancy(df = "exp_mh")

factor_vec <- factor %>% 
  dplyr::filter(alspac == "factor") %>%
  pull(variable)

fixFactor <- function(df, vars){
  
  vars %>% map(
    ~ds.asFactor(
      input.var.name = paste0(df, "$", .x), 
      newobj.name = .x))
  
  dh.dropCols(
    df = df, 
    vars = vars,
    type = "remove", 
    checks = F)
  
  ds.dataFrame(
    x = c(df, vars),
    newobj = df)
}

fixFactor("exp_mh", factor_vec)

datashield.workspace_save(conns, "exp-mh")







################################################################################
# 3. Set reference levels of binary variables to 0  
################################################################################
refvars <- c("greenyn300_preg", "blueyn300_preg")

refvars %>%
  map(
    ~ds.changeRefGroup(
      x = paste0("env_pnd$", .),
      ref = "0",
      newobj = .,
      reorderByRef = FALSE)
  )

dh.dropCols(
  df = "env_pnd", 
  vars = refvars,
  type = "remove"
)

ds.dataFrame(
  x = c("env_pnd", refvars), 
  newobj = "env_pnd"
)

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_18")
conns <- datashield.login(logindata, restore = "env_pnd_18")

################################################################################
# 4. Define baseline dataset for which urban exposures were estimated
################################################################################

## ---- Identify non-missing cases ---------------------------------------------
dh.defineCases(
  df = "env_pnd", 
  vars = "urb_area_id", 
  type = "any", 
  new_obj = "baseline_valid"
)

## ---- Set Rhea to 1 ----------------------------------------------------------
ds.assign(
  toAssign = "baseline_valid+1", 
  newobj = "baseline_valid", 
  datasources = conns["rhea"]
)

## ---- Create subset ----------------------------------------------------------
ds.dataFrameSubset(
  df.name = "env_pnd", 
  V1.name = "baseline_valid", 
  V2.name = "1", 
  Boolean.operator = "==", 
  keep.NAs = FALSE, 
  newobj = "baseline_df")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_19")
conns <- datashield.login(logindata, restore = "env_pnd_19")

################################################################################
# 5. Define valid cases: subjects with outcome and >= 1 exposure
################################################################################

## ---- First we specify vectors of exposures and outcomes ---------------------
exp.vars <- c(
  "ndvi300_preg", "greenyn300_preg", "blueyn300_preg", "no2_preg", "pm25_preg", 
  "pm10_preg",  "lden_preg_f",  "frichness300_preg", "walkability_mean_preg", 
  "popdens_preg")

out.vars <- "ppd"

## ---- Now we create vars indicating whether any non-missing values are present
dh.defineCases(
  df = "baseline_df", 
  vars = exp.vars, 
  type = "any", 
  new_obj = "any_exp"
)

dh.defineCases(
  df = "baseline_df", 
  vars = out.vars, 
  type = "any", 
  new_obj = "any_out"
)

ds.make(
  toAssign = "any_exp+any_out", 
  newobj = "n_complete")

ds.Boole(
  V1 = "n_complete", 
  V2 = "2", 
  Boolean.operator = "==", 
  na.assign = 0, 
  newobj = "some_vars")

## ---- Create subset ----------------------------------------------------------
ds.dataFrameSubset(
  df.name = "baseline_df", 
  V1.name = "some_vars", 
  V2.name = "1", 
  Boolean.operator = "==", 
  keep.NAs = FALSE, 
  newobj = "some_exp_out_df")

ds.dim("some_exp_out_df")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_20")
conns <- datashield.login(logindata, restore = "env_pnd_20")

################################################################################
# 6. Restrict to live births
################################################################################

## ---- Set MoBa outcome as 1 --------------------------------------------------
ds.rep(
  x1 = 1,
  times = ds.dim("some_exp_out_df", datasources = conns["moba"])[[1]][[1]] ,
  source.x1 = "clientside",
  source.times = "c",
  newobj = "outcome",
  datasources = conns["moba"])

ds.make(
  toAssign = "some_exp_out_df$outcome",
  newobj = "outcome",
  datasources = conns[!names(conns) == c("moba")])

ds.asFactor("outcome", "outcome")

dh.dropCols(
  df = "some_exp_out_df",
  vars = "outcome",
  type = "remove",
  conns = conns["moba"])

ds.dataFrame(
  x = c("some_exp_out_df", "outcome"),
  newobj = "some_exp_out_df",
  datasources = conns["moba"])

## ---- Define cases meeting exclusion criteria --------------------------------
ds.Boole(
  V1 = "some_exp_out_df$outcome",
  V2 = 1,
  Boolean.operator = "==",
  na.assign = "0",
  newobj = "outcome_valid")

## ---- Create subset ----------------------------------------------------------
ds.dataFrameSubset(
  df.name = "some_exp_out_df", 
  V1.name = "outcome_valid", 
  V2.name = "1", 
  Boolean.operator = "==", 
  keep.NAs = FALSE, 
  newobj = "live_births_df")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_21")
conns <- datashield.login(logindata, restore = "env_pnd_21")

################################################################################
# 7. Restrict to first borns
################################################################################

## ---- Set all GEN-R child_no as 1 --------------------------------------------
ds.rep(
  x1 = 1,
  times = ds.dim("live_births_df", datasources = conns["genr"])[[1]][[1]],
  source.x1 = "clientside",
  source.times = "c",
  newobj = "child_no",
  datasources = conns[c("genr", "moba")])

ds.asInteger("child_no", "child_no", datasources = conns["genr"])

dh.dropCols(
  df = "live_births_df",
  vars = "child_no",
  type = "remove",
  conns = conns["genr"])

ds.dataFrame(
  x = c("live_births_df", "child_no"),
  newobj = "live_births_df",
  datasources = conns["genr"])

ds.Boole(
  V1 = "live_births_df$child_no",
  V2 = 1,
  Boolean.operator = "==",
  na.assign = "0",
  newobj = "child_no_valid")

## ---- Create subset ----------------------------------------------------------
ds.dataFrameSubset(
  df.name = "live_births_df", 
  V1.name = "child_no_valid", 
  V2.name = "1", 
  Boolean.operator = "==", 
  keep.NAs = FALSE, 
  newobj = "analysis_df")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_22")
conns <- datashield.login(logindata, restore = "env_pnd_22")

################################################################################
# 8. Create excluded participants dataset
################################################################################

## ---- First create vector indicating membership of analysis_df ---------------
dims <- ds.dim("analysis_df")

length.ref <- tibble(
  cohort = names(conns), 
  length = dims %>% map(~.x[[1]]) %>% unlist() %>% head(-1) %>% as.character
)

length.ref %>%
  pmap(function(cohort, length){
    ds.rep(
      x1 = 1,
      times = length,
      source.x1 = "clientside",
      source.times = "c",
      newobj = "case_def", 
      datasources = conns[cohort])
  })

ds.dataFrame(
  x = c("analysis_df$child_id", "case_def"),
  newobj = "analysis_df_tmp"
)

## ---- Now merge this vector with baseline_df ---------------------------------
ds.merge(
  x.name = "baseline_df",
  y.name = "analysis_df_tmp",
  by.x = "child_id",
  by.y = "child_id",
  all.x = TRUE,
  all.y = TRUE, 
  newobj = "exc_tmp")

## ---- Transform case_def to binary vector ------------------------------------
ds.Boole(
  V1 = "exc_tmp$case_def", 
  V2 = 1, 
  Boolean.operator = "==", 
  na.assign = "0", 
  newobj = "almost_there"
)

## ---- Create excluded subset based on this vector ----------------------------
ds.dataFrameSubset(
  df.name = "baseline_df", 
  V1.name = "almost_there", 
  V2.name = "0", 
  Boolean.operator = "==", 
  keep.NAs = FALSE, 
  newobj = "excluded_df")

ds.dim("excluded_df")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_24")
conns <- datashield.login(logindata, restore = "env_pnd_24")
