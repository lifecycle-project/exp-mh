################################################################################
## Project: Exposome and Mental Health    
## Script purpose: Create final dataset
## Date: 13th April 2022
## Author: Tim Cadman
## Email: tica@sund.ku.dk
################################################################################

conns <- datashield.login(logindata, restore = "exp-mh")

################################################################################
# 1. Create exposure dataframe  
################################################################################

## ---- Reference table --------------------------------------------------------
cont_exp.vars <- inner_join(
  all_vars %>% dplyr::filter(type == "exposure"),
  filled$non_rep %>% dplyr::filter(ninfea %in% c("integer", "numeric")), 
  by = "variable") %>%
  pull(variable)

## ---- Create definition of exposures to keep --------------------------------- 
exclude.vars <- c("natgr_preg", "other_preg", "vldres_preg", 
"bus_lines_100_preg", "connind_100_preg", "port_preg", "bus_stops_100_preg", 
"trafmajorload100_preg", "oc_preg", "op_dtt_preg", 
"op_esr_preg", "pah_preg", "pm25cu_preg", "pm25fe_preg", "pm25si_preg", 
"pm25zn_preg")

#"ln_preg", "lden_preg", 

exp_analysis.vars <- all_vars %>% 
  dplyr::filter(type == "exposure" & !variable %in% exclude.vars) %>%
  pull(variable)

non_rep_cols <- ds.colnames("non_rep_sub")[[1]] ## all dfs have same cols

avail_cols <- non_rep_cols[non_rep_cols %in% exp_analysis.vars]

## ---- Create raw exposure dataframe ------------------------------------------
dh.dropCols(
  df = "non_rep_sub", 
  vars = c("child_id", avail_cols), 
  type = "keep", 
  new_obj = "exposures_a", 
  checks = F)

## ---- Remove variables which were transformed --------------------------------
dh.dropCols(
  df = "exposures_a", 
  vars = trans_sum$original, 
  type = "remove", 
  new_obj = "exposures_b", 
  checks = F)

## ---- Join in transformed variables ------------------------------------------
ds.dataFrame(
  x = c("exposures_b", trans_sum$transformed),
  newobj = "exposures_c")

datashield.workspace_save(conns, "exp-mh")
## ---- Create dataset with IQR transformations of continuous vars -------------
exp_class <- dh.classDiscrepancy(df = "exposures_c") 

exp_cont.vars <- exp_class %>%
  dplyr::filter(abcd %in% c("integer", "numeric")) %>%
  pull(variable)

dh.makeIQR(
  df = "exposures_c", 
  vars = exp_cont.vars, 
  type = "combine", 
  new_obj = "exposures_d", 
  checks = F)

## ---- Drop columns -----------------------------------------------------------
ds.colnames("exposures_d")

dh.dropCols(
  df = "exposures_d", 
  vars = exp_cont.vars, 
  type = "remove")

ds.assign("exposures_d", "exposures")

ds.colnames("exposures")

datashield.workspace_save(conns, "exp-mh")
conns <- datashield.login(logindata, restore = "exp-mh")

################################################################################
# 2. Create covariates dataframe
################################################################################
ds.assign("non_rep_sub$birth_month", "birth_month_orig")

## ---- Join with ds.dataFrame -------------------------------------------------
ds.dataFrame(
  x = c("non_rep_sub", "parity_bin", "birth_month_f", "mat_age_f", "preterm", 
        "area_ses_tert_f", "birth_month_orig"),
  newobj = "covariates_a", 
  datasources = conns[!names(conns) %in% c("moba", "rhea")])

ds.dataFrame(
  x = c("non_rep_sub", "parity_bin", "birth_month_f", "mat_age_f", "preterm", 
        "birth_month_orig"),
  newobj = "covariates_a", 
  datasources = conns[c("rhea")])

cov.def <- c("area_ses_tert_f", "agebirth_m_y", "edu_m_.0_1", 
             "parity_bin", "birth_month_orig", "sex")


## ---- Merge wide format objects ----------------------------------------------
ds.merge(
      x.name = "covariates_a",
      y.name = "mat_ed",
      by.x.names = "child_id", 
      by.y.names = "child_id",
      all.x = F,
      all.y = F,
      newobj = "covariates")

## ---- Keep just the covariates that we need ----------------------------------

## still needed
#child age at assessment
#birth year
#eusilc
#area dep rhea moba
 
analysis_cov <- c("area_ses_tert_f", "agebirth_m_y", "edu_m_.0_1", 
                  "parity_bin", "birth_month_f", "sex")

datashield.workspace_save(conns, "exp-mh")

################################################################################
# 3. Create one analysis df  
################################################################################
ds.merge(
  x.name = "exposures", 
  y.name = "covariates", 
  by.x.names = "child_id", 
  by.y.names = "child_id",
  newobj = "exp_cov",
  all.x = TRUE,
  all.y = TRUE)

exwas_merge.ref <- tibble(
  var = c("int", "ext", "adhd", "lan", "nvi", "wm", "fm", "gm"), 
  coh = list(int_coh, ext_coh, adhd_coh, lan_coh, nvi_coh, wm_coh, fm_coh, 
             gm_coh))

ds.assign("exp_cov", "baseline_df")

exwas_merge.ref %>%
  pmap(function(var, coh){
    ds.merge(
      x.name = "baseline_df", 
      y.name = paste0(var, "_sub_z"), 
      by.x.names = "child_id", 
      by.y.names = "child_id",
      newobj = "baseline_df",
      all.x = TRUE,
      all.y = FALSE, 
      datasources = conns["rhea"])
    
  })

datashield.workspace_save(conns, "exp-mh")
conns <- datashield.login(logindata, restore = "exp-mh")

ds.dataFrameFill("baseline_df", "baseline_df")
ds.colnames("baseline_df")

datashield.workspace_save(conns, "exp-mh")

## ---- Fix levels for area ses ------------------------------------------------
dh.columnCast(
  df = "baseline_df", 
  target_vars = "area_ses_tert_f", 
  target_class = "factor")

test <- dh.getStats(
  df = "baseline_df", 
  vars = c(analysis_cov, "ndvi300_preg"))

test$categorical %>% print(n = Inf)
test$continuous %>% print(n = Inf)

datashield.workspace_save(conns, "exp-mh")

ds.colnames("baseline_df")
ds.summary("baseline_df$ndvi300_preg")


## Can remove this step once have the variable in MoBa & Rhea




## ---- Remove some objects ----------------------------------------------------
to_keep.vars <- c(
  "adhd_df", "adhd_long", "adhd_sub", "adhd_sub_z", "adhd_sub_z_long", "adhd_t", 
  "adhd_t_long", "adhd_trans", "adhd_z", "area_ses_tert", "area_ses_tert_f", 
  "baseline_df", "covariates", "covariates_a", "covariates_b", "exp_cov", 
  "exposures", "exposures_a", "exposures_b", "exposures_c", "exposures_d", 
  "ext_df", "ext_long", "ext_sub", "ext_sub_z", "ext_sub_z_long", "ext_t", 
  "ext_t_long", "ext_trans", "ext_z", "fm_df", "fm_sub", "fm_sub_z", "fm_t", 
  "fm_z", "gm_df", "gm_sub", "gm_sub_z", "gm_t", "gm_z", "int_df", "int_long",
  "int_sub", "int_sub_z", "int_sub_z_long", "int_t", "int_t_long", "int_z",
  "iqr_combine", "lan_df", "lan_sub", "lan_sub_z", "lan_t", "lan_z", "mh_rep",
  "mh_rep_fill", "mh_rep_sub", "non_rep", "non_rep_fill", "non_rep_sub", 
  "nvi_df", "nvi_sub", "nvi_sub_z", "nvi_z", "wm_df", "wm_sub", "wm_sub_z", 
  "wm_t", "wm_z", "year_rep", "year_rep_fill", "year_rep_sub")

dh.tidyEnv(
  obj = to_keep.vars, 
  type = "keep")









################################################################################
# 2. Create phenotype subsets  
################################################################################

## ---- Internalising ----------------------------------------------------------
ds.merge(
  x.name = "covariates", 
  y.name = "int_sub_z", 
  by.x.names = "child_id", 
  by.y.names = "child_id",
  newobj = "int_pheno",
  all.x = TRUE,
  all.y = FALSE, 
  datasources = conns[int_coh])

## ---- Externalising ----------------------------------------------------------
ds.merge(
  x.name = "covariates", 
  y.name = "ext_sub_z", 
  by.x.names = "child_id", 
  by.y.names = "child_id",
  newobj = "ext_pheno",
  all.x = TRUE,
  all.y = FALSE, 
  datasources = conns[ext_coh])

## ---- ADHD -------------------------------------------------------------------
ds.merge(
  x.name = "covariates", 
  y.name = "adhd_sub_z", 
  by.x.names = "child_id", 
  by.y.names = "child_id",
  newobj = "adhd_pheno",
  all.x = TRUE,
  all.y = FALSE, 
  datasources = conns[adhd_coh])

## ---- Language ---------------------------------------------------------------
ds.merge(
  x.name = "covariates", 
  y.name = "lan_sub_z", 
  by.x.names = "child_id", 
  by.y.names = "child_id",
  newobj = "lan_pheno",
  all.x = TRUE,
  all.y = FALSE, 
  datasources = conns[lan_coh])

## ---- Non-verbal intelligence ------------------------------------------------
ds.merge(
  x.name = "covariates", 
  y.name = "nvi_sub_z", 
  by.x.names = "child_id", 
  by.y.names = "child_id",
  newobj = "nvi_pheno",
  all.x = TRUE,
  all.y = FALSE, 
  datasources = conns[nvi_coh])

## ---- Working memory ---------------------------------------------------------
ds.merge(
  x.name = "covariates", 
  y.name = "wm_sub_z", 
  by.x.names = "child_id", 
  by.y.names = "child_id",
  newobj = "wm_pheno",
  all.x = TRUE,
  all.y = FALSE, 
  datasources = conns[wm_coh])

## ---- Fine motor -------------------------------------------------------------
ds.merge(
  x.name = "covariates", 
  y.name = "fm_sub_z", 
  by.x.names = "child_id", 
  by.y.names = "child_id",
  newobj = "fm_pheno",
  all.x = TRUE,
  all.y = FALSE, 
  datasources = conns[fm_coh])

## ---- Gross motor ------------------------------------------------------------
ds.merge(
  x.name = "covariates", 
  y.name = "gm_sub_z", 
  by.x.names = "child_id", 
  by.y.names = "child_id",
  newobj = "gm_pheno",
  all.x = TRUE,
  all.y = FALSE, 
  datasources = conns[gm_coh])

datashield.workspace_save(conns, "exp-mh")

################################################################################
# Tidy up a little  
################################################################################
keep_vars <- c(
  "non_rep", "non_rep_fill", "year_rep", "year_rep_fill", "mh_rep", 
  "mh_rep_fill", "non_rep_sub", "year_rep_sub", "mh_rep_sub", "iqr_combine", 
  "fdens_trans", "frich_trans", "hdres_trans", "indtr_trans", "ldres_trans",
  "trans_trans", "parity_bin", "birth_month", "birth_month_f", "mat_age", 
  "mat_age_f", "ga_bin", "preterm", "mat_ed", "area_ses", "urb_area_id", 
  "covariates", "ext_sub", "int_sub", "adhd_sub", "fm_sub", "gm_sub", "lan_sub", 
  "nvi_sub", "wm_sub", "ext_sub_z", "int_sub_z", "adhd_sub_z", "fm_sub_z",
  "gm_sub_z", "lan_sub_z", "nvi_sub_z", "wm_sub_z", "ext_pheno", "int_pheno", 
  "adhd_pheno", "fm_pheno", "gm_pheno", "lan_pheno", "nvi_pheno", "wm_pheno", 
  "exposures_a", "exposures_b", "exposures_c", "exposures_d", "covariates_a",
  "covariates", "exp_mh_wide", "exp_mh_long", "any_exp", "any_out", 
  "n_complete", "some_vars", "some_exp_out_df", "outcome", "outcome_valid", 
  "live_births_df", "child_no", "child_no_valid", "analysis_df", "case_def", 
  "analysis_df_tmp", "exc_tmp", "almost_there", "excluded_df", 
  "exposures_valid", "ext_exp_set", "int_exp_set", "adhd_exp_set", "fm_exp_set", 
  "gm_exp_set", "lan_exp_set", "nvi_exp_set", "wm_exp_set", "int_inst", 
  "ext_inst", "adhd_inst", "nvi_inst", "lan_inst", "fm_inst", "gm_inst")

dh.tidyEnv(
  obj = keep_vars,
  type = "keep")

datashield.workspace_save(conns, "exp-mh")










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





)




################################################################################
# 5. Merge with wide MH outcomes  
################################################################################
ds.merge(
  x.name = "exp_mh_wide",
  y.name = "lan_sub",
  by.x.names = "child_id", 
  by.y.names = "child_id",
  all.x = T,
  all.y = F,
  newobj = "pheno_wide")



################################################################################
# 4. Create long format version  
################################################################################

## ---- Merge into long format data --------------------------------------------
ds.merge(
  x.name = "mh_rep_sub",
  y.name = "exp_mh_wide",
  by.x.names = "child_id", 
  by.y.names = "child_id",
  all.x = T,
  all.y = T,
  newobj = "exp_mh_long")

datashield.workspace_save(conns, "exp-mh")

ds.summary("exp_mh_long")