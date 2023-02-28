################################################################################
## Project: Exposome and Mental Health
## Script purpose: Run analysis
## Date: 27th July 2022
## Author: Tim Cadman
## Email: tica@sund.ku.dk
################################################################################

conns <- datashield.login(logindata, restore = "exp-mh")

ds.colnames("analysis_df_w")

ds.cor("analysis_df_w$edu_m_.0_1", "analysis_df_w$pm25_preg")


test <- ds.glmSLMA(
  formula = "no2_preg_iqr_c~edu_m_.0_1", 
  dataName = "analysis_df_w", 
  family = "gaussian")

################################################################################
# Descriptive statistics: full analysis sample
################################################################################
exp.vars <- all_vars %>% 
  dplyr::filter(type == "exposure") %>% 
  dplyr::filter(!variable %in% c("blueyn300_preg", "greenyn300_preg")) %>% 
  pull(variable)

desc_all_exp <- dh.getStats(
  df = "analysis_df_w", 
  vars = exp.vars, 
  checks = F)

desc_all_cov <- dh.getStats(
  df = "analysis_df_w", 
  vars = c(cov.def, "birth_month_f"),
  checks = F)

descriptives_all <- list(desc_all_exp, desc_all_cov) %>%
  pmap(bind_rows)

save.image()

################################################################################
# Descriptive statistics: excluded participants
################################################################################
exc_exp <- dh.getStats(
  df = "excluded_df_w", 
  vars = exp.vars, 
  checks = F)

exc_cov <- dh.getStats(
  df = "excluded_df_w", 
  vars = c(cov.def, "birth_month_f"),
  checks = F)

exc.desc <- list(exc_exp, exc_cov) %>%
  pmap(bind_rows)

save.image()

################################################################################
# Correlations between pregnancy exposures
################################################################################

## ---- Create subsets with required variables ---------------------------------
ds.dataFrame(
  x = paste0("analysis_df_w$", exp_cont.vars[exp_cont.vars != "airpt_preg"], sep = ""), 
  newobj = "heat_preg")

datashield.workspace_save(conns, "exp-mh")
# Work out what's going on with this variable

## ---- Correlation matrices: combined -----------------------------------------
exp_cor_preg_comb <- ds.cor(
  x = "heat_preg", 
  type = "combine",
  naAction = "pairwise.complete")

## ---- Correlation matrices: separate -----------------------------------------
exp_cor_preg_split <- ds.cor(
  x = "heat_preg", 
  type = "split",
  naAction = "pairwise.complete")

save.image()


################################################################################
# 1. Define fixed effect models  
################################################################################
ds.class("analysis_df_w$lden_preg_iqr_c")

## ---- Available data ---------------------------------------------------------
avail_exp <- dh.anyData(
  df = "analysis_df_w", 
  vars = paste0(exp_cont.vars, "_iqr_c"))

avail_cov <- dh.anyData(
  df = "analysis_df_w", 
  vars = analysis_cov)

save.image()

## ---- Model definitions ------------------------------------------------------
fixed_exwas.mod <- list(
  int = dt.buildModels(
    avail_exp = avail_exp, 
    avail_cov = avail_cov, 
    outcome = "int_z", 
    id_seed = 0), 
  ext_n = dt.buildModels(
    avail_exp = avail_exp, 
    avail_cov = avail_cov, 
    outcome = "ext_z", 
    id_seed = 1), 
  adhd = dt.buildModels(
    avail_exp = avail_exp, 
    avail_cov = avail_cov, 
    outcome = "adhd_z", 
    id_seed = 2))

################################################################################
# 2. Run fixed effect models  
################################################################################

## ---- Internalising ----------------------------------------------------------
int.fit <- dh.multGLM(
  df = "analysis_df_w", 
  ref = fixed_exwas.mod$int, 
  family = "gaussian")

save.image()

## ---- Externalising ----------------------------------------------------------
ext.fit <- dh.multGLM(
  df = "analysis_df_w", 
  ref = fixed_exwas.mod$ext, 
  family = "gaussian")

save.image()

## ---- ADHD -------------------------------------------------------------------
adhd.fit <- dh.multGLM(
  df = "analysis_df_w", 
  ref = fixed_exwas.mod$adhd, 
  family = "gaussian")

save.image()

################################################################################
# 3. Meta-analyse models  
################################################################################

## ---- Internalising ----------------------------------------------------------
int_conv.fit <- int.fit %>% dplyr::filter(converged == TRUE)

int.mdata <- dh.metaSepModels(
  ref = int_conv.fit,
  exp = FALSE, 
  method = "REML", 
  output = "both")

## ---- Externalising ----------------------------------------------------------
ext_conv.fit <- ext.fit %>% dplyr::filter(converged == TRUE)

ext.mdata <- dh.metaSepModels(
  ref = ext_conv.fit,
  exp = FALSE, 
  method = "REML", 
  output = "both")

## ---- ADHD -------------------------------------------------------------------
adhd_conv.fit <- adhd.fit %>% dplyr::filter(converged == TRUE)

adhd.mdata <- dh.metaSepModels(
  ref = adhd_conv.fit,
  exp = FALSE, 
  method = "REML", 
  output = "both")

save.image()

################################################################################
# SENSITIVITY  
################################################################################
################################################################################
# Unadjusted model on full sample  
################################################################################
fixed_exwas_s1.mod <- list(
  int = dt.buildModels(
    avail_exp = avail_exp, 
    outcome = "int_z", 
    id_seed = 3), 
  ext_n = dt.buildModels(
    avail_exp = avail_exp, 
    outcome = "ext_z", 
    id_seed = 4), 
  adhd = dt.buildModels(
    avail_exp = avail_exp, 
    outcome = "adhd_z", 
    id_seed = 5))


## ---- Internalising ----------------------------------------------------------
int_s_1.fit <- dh.multGLM(
  df = "any_out_df_w", 
  ref = fixed_exwas_s1.mod$int, 
  family = "gaussian")

save.image()

## ---- Externalising ----------------------------------------------------------
ext_s_1.fit <- dh.multGLM(
  df = "any_out_df_w", 
  ref = fixed_exwas_s1.mod$ext, 
  family = "gaussian")

save.image()

## ---- ADHD -------------------------------------------------------------------
adhd_s_1.fit <- dh.multGLM(
  df = "any_out_df_w", 
  ref = fixed_exwas_s1.mod$adhd, 
  family = "gaussian")

save.image()

################################################################################
# Unadjusted model on complete case sample  
################################################################################

## ---- Internalising ----------------------------------------------------------
int_s_2.fit <- dh.multGLM(
  df = "analysis_df_w", 
  ref = fixed_exwas_s1.mod$int, 
  family = "gaussian")

save.image()

## ---- Externalising ----------------------------------------------------------
ext_s_2.fit <- dh.multGLM(
  df = "analysis_df_w", 
  ref = fixed_exwas_s1.mod$ext, 
  family = "gaussian")

save.image()

## ---- ADHD -------------------------------------------------------------------
adhd_s_2.fit <- dh.multGLM(
  df = "analysis_df_w", 
  ref = fixed_exwas_s1.mod$adhd, 
  family = "gaussian")

save.image("analysis.RData")

################################################################################
# META-ANALYSE
################################################################################
################################################################################
# Unadjusted model on full sample  
################################################################################

## ---- Internalising ----------------------------------------------------------
int_conv_s_1.fit <- int_s_1.fit %>% dplyr::filter(converged == TRUE)

int_s_1.mdata <- dh.metaSepModels(
  ref = int_conv_s_1.fit,
  exp = FALSE, 
  method = "REML", 
  output = "both")

## ---- Externalising ----------------------------------------------------------
ext_conv_s_1.fit <- ext_s_1.fit %>% dplyr::filter(converged == TRUE)

ext_s_1.mdata <- dh.metaSepModels(
  ref = ext_conv_s_1.fit,
  exp = FALSE, 
  method = "REML", 
  output = "both")

## ---- ADHD -------------------------------------------------------------------
adhd_conv_s_1.fit <- adhd_s_1.fit %>% dplyr::filter(converged == TRUE)

adhd_s_1.mdata <- dh.metaSepModels(
  ref = adhd_conv_s_1.fit,
  exp = FALSE, 
  method = "REML", 
  output = "both")

save.image("analysis.RData")

################################################################################
# Unadjusted model on complete case sample  
################################################################################

## ---- Internalising ----------------------------------------------------------
int_conv_s_2.fit <- int_s_2.fit %>% dplyr::filter(converged == TRUE)

int_s_2.mdata <- dh.metaSepModels(
  ref = int_conv_s_2.fit,
  exp = FALSE, 
  method = "REML", 
  output = "both")

## ---- Externalising ----------------------------------------------------------
ext_conv_s_2.fit <- ext_s_2.fit %>% dplyr::filter(converged == TRUE)

ext_s_2.mdata <- dh.metaSepModels(
  ref = ext_conv_s_2.fit,
  exp = FALSE, 
  method = "REML", 
  output = "both")

## ---- ADHD -------------------------------------------------------------------
adhd_conv_s_2.fit <- adhd_s_2.fit %>% dplyr::filter(converged == TRUE)

adhd_s_2.mdata <- dh.metaSepModels(
  ref = adhd_conv_s_2.fit,
  exp = FALSE, 
  method = "REML", 
  output = "both")

save.image("analysis.RData")
save.image("plots.RData")
save.image("tables.RData")











################################################################################
# 4. Work out best polynomial combination for rm outcomes  
################################################################################

## ---- Create polynomial terms ------------------------------------------------
dh.makeAgePolys(
  df = "int_sub_z_long", 
  age_var = "int_age_", 
  conns = conns[int_coh])

dh.makeAgePolys(
  df = "ext_sub_z_long", 
  age_var = "ext_age_", 
  conns = conns[ext_coh])

dh.makeAgePolys(
  df = "adhd_sub_z_long", 
  age_var = "adhd_age_", 
  conns = conns[adhd_coh])

## ---- Make formulae ----------------------------------------------------------
int.mod <- dh.makeLmerForm(
  outcome = "int_z", 
  id_var = "child_id_int", 
  age_vars = c("int_age_", "int_age_m__2", "int_age_m__1", "int_age__m_0_5", 
               "int_age__log", "int_age__0_5", "int_age__2", "int_age__3"), 
  random = "intercept") %>%
  slice(1:7)

ext.mod <- dh.makeLmerForm(
  outcome = "ext_z", 
  id_var = "child_id_int", 
  age_vars = c("ext_age_", "ext_age_m__2", "ext_age_m__1", "ext_age__m_0_5", 
               "ext_age__log", "ext_age__0_5", "ext_age__2", "ext_age__3"), 
  random = "intercept") %>%
  slice(1:7)

adhd.mod <- dh.makeLmerForm(
  outcome = "int_z", 
  id_var = "child_id_int", 
  age_vars = c("adhd_age_", "adhd_age_m__2", "adhd_age_m__1", "adhd_age__m_0_5", 
               "adhd_age__log", "adhd_age__0_5", "adhd_age__2", "adhd_age__3"), 
  random = "intercept") %>%
  slice(1:7)

## ---- Make integer ID terms --------------------------------------------------
c("int_sub_z_long", "ext_sub_z_long", "ext_sub_z_long") %>%
  map(function(x){
    
    ds.asInteger(paste0(x, "$child_id"), "child_id_int")
    ds.dataFrame(c(x, "child_id_int"))
    
  })

datashield.workspace_save(conns, "exp-mh")

################################################################################
# 5. Fit minimal model to find shape of outcome  
################################################################################
int.fit <- dh.lmeMultPoly(
  df = "int_sub_z_long",
  formulae = int.mod$formula, 
  poly_names = int.mod$polys, 
  conns = conns[int_coh])




ds.colnames("int_sub_z_long")









test <- dh.getStats(
  df = "non_rep", 
  vars = avail_exp
)

ds.colnames("non_rep", datasources = conns["rhea"])
  
  
  "lan", "nvi", "wm", "fm", "gm"
  
  
)

test <- dt.buildModels(
  avail_exp = avail_exp, 
  avail_cov = avail_cov, 
  outcome = "int_z")

test$formula

single.mod <- list(
  int = dt.buildModels(
    avail_exp = avail_exp, 
    avail_cov = avail_cov, 
    outcome = "", 
    id_seed = 0)
  
  
  
  
  
  , 
  model_2 = dt.buildModels(
    avail_exp = avail_exp, 
    avail_cov = avail_cov$model_2,
    outcome = "ppd", 
    id_seed = 1), 
  model_3 = dt.buildModels(
    avail_exp = avail_exp, 
    avail_cov = avail_cov$model_3, 
    outcome = "ppd", 
    id_seed = 2), 
  model_4 = dt.buildModels(
    avail_exp = avail_exp,
    avail_cov = avail_cov$model_4, 
    outcome = "ppd", 
    id_seed = 3))





















library(dsExposomeClient)
library(dsMTLBase)

ds.histogram("int_sub$int_raw_.0_17")
ds.histogram("int_sub_z$int_z")


ds.histogram("ext_sub_z$ext_z")
ds.histogram("adhd_sub_z$adhd_z", datasources = conns["adhd_coh"])
ds.histogram("lan_sub_z$lan_z", datasources = conns["lan_coh"])

################################################################################
# 1. Define exposures for each cohort  
################################################################################
max_vars <- ds.colnames("exposures")[[1]]

exp.stats <- dh.getStats(
  df = "exposures", 
  vars = max_vars)

save.image()

exp_valid.stats <- exp.stats$continuous %>%
  dplyr::filter(mean != "Inf" & mean != "NaN")

exp_inma_gip.vars <- exp_valid.stats %>%
  dplyr::filter(cohort == "inma_gip") %>%
  pull(variable)

exp_inma_val.vars <- exp_valid.stats %>%
  dplyr::filter(cohort == "inma_val") %>%
  pull(variable)

exp_inma_sab.vars <- exp_valid.stats %>%
  dplyr::filter(cohort == "inma_sab") %>%
  pull(variable)

exp_ninfea.vars <- exp_valid.stats %>%
  dplyr::filter(cohort == "ninfea") %>%
  pull(variable)

dh.dropCols(
  df = "exposures", 
  vars = c("child_id", exp_inma_val.vars), 
  type = "keep",
  new_obj = "exposures_valid", 
  conns = conns["inma_gip"])

dh.dropCols(
  df = "exposures", 
  vars = c("child_id", exp_inma_val.vars), 
  type = "keep",
  new_obj = "exposures_valid", 
  conns = conns["inma_val"])

dh.dropCols(
  df = "exposures", 
  vars = c("child_id", exp_inma_sab.vars), 
  type = "keep",
  new_obj = "exposures_valid", 
  conns = conns["inma_sab"])

dh.dropCols(
  df = "exposures", 
  vars = c("child_id", exp_ninfea.vars), 
  type = "keep",
  new_obj = "exposures_valid", 
  conns = conns["ninfea"])


## ---- Remove additional variables that wouldn't converge ---------------------
dh.dropCols(
  df = "exposures_valid", 
  vars = "water_preg_iqr_c", 
  typ = "remove", 
  conns = conns[c("inma_gip", "inma_sab", "inma_val")])

dh.dropCols(
  df = "exposures_valid", 
  vars = c("traf_near_trans_iqr_c", "trafload100_preg_iqr_c", "water_preg_iqr_c", 
           "indtr_trans_iqr_c"), 
  typ = "remove", 
  conns = conns["ninfea"])

datashield.workspace_save(conns, "exp-mh")
save.image()


################################################################################
# 2. Create exposome sets  
################################################################################

## ---- Internalising ----------------------------------------------------------
ds.loadExposome(
  exposures = "exposures_valid",
  phenotypes = "int_pheno", 
  exposures.idcol = "child_id", 
  phenotypes.idcol = "child_id", 
  object_name = "int_exp_set", 
  datasources = conns[int_coh])

## ---- Externalising ----------------------------------------------------------
ds.loadExposome(
  exposures = "exposures_valid",
  phenotypes = "ext_pheno", 
  exposures.idcol = "child_id", 
  phenotypes.idcol = "child_id", 
  object_name = "ext_exp_set", 
  datasources = conns[ext_coh])

## ---- ADHD -------------------------------------------------------------------
ds.loadExposome(
  exposures = "exposures_valid",
  phenotypes = "adhd_pheno", 
  exposures.idcol = "child_id", 
  phenotypes.idcol = "child_id", 
  object_name = "adhd_exp_set", 
  datasources = conns[adhd_coh])

## ---- ASD --------------------------------------------------------------------

# Not available in these cohorts

## ---- Language ---------------------------------------------------------------
ds.loadExposome(
  exposures = "exposures_valid",
  phenotypes = "lan_pheno", 
  exposures.idcol = "child_id", 
  phenotypes.idcol = "child_id", 
  object_name = "lan_exp_set", 
  datasources = conns[lan_coh])

## ---- Non-verbal intelligence ------------------------------------------------
ds.loadExposome(
  exposures = "exposures_valid",
  phenotypes = "nvi_pheno", 
  exposures.idcol = "child_id", 
  phenotypes.idcol = "child_id", 
  object_name = "nvi_exp_set", 
  datasources = conns[nvi_coh])

## ---- Working memory ---------------------------------------------------------
ds.loadExposome(
  exposures = "exposures_valid",
  phenotypes = "wm_pheno", 
  exposures.idcol = "child_id", 
  phenotypes.idcol = "child_id", 
  object_name = "wm_exp_set", 
  datasources = conns[wm_coh])

## ---- Fine motor -------------------------------------------------------------
ds.loadExposome(
  exposures = "exposures_valid",
  phenotypes = "fm_pheno", 
  exposures.idcol = "child_id", 
  phenotypes.idcol = "child_id", 
  object_name = "fm_exp_set", 
  datasources = conns[fm_coh])

## ---- Gross motor ------------------------------------------------------------
ds.loadExposome(
  exposures = "exposures_valid",
  phenotypes = "gm_pheno", 
  exposures.idcol = "child_id", 
  phenotypes.idcol = "child_id", 
  object_name = "gm_exp_set", 
  datasources = conns[gm_coh])

datashield.workspace_save(conns, "exp-mh")

################################################################################
# Perform EWASs  
################################################################################
cov_form <- "agebirth_m_y+sex+parity_bin+birth_month_f+edu_m_.0_1+area_ses_tert_f"
cov_form_min <- "edu_m_.0_1+area_ses_tert_f"


ds.exposome_variables("int_exp_set", target = "exposures")


ds.exposome_variables("test", target = "exposures", datasources = conns["inma_val"])

## ---- Internalising ----------------------------------------------------------
int.ewas <- int_coh %>%
  map(
    ~ds.exwas(
      paste0("int_z~", cov_form_min),
      type = "meta",
      Set = "int_exp_set", 
      family = "gaussian", 
      tef = FALSE,
      datasources = conns[.x])) %>%
  set_names(int_coh)

save.image()

## ---- Externalising -----------------------------------------------------------
ext.ewas <- ext_coh %>%
  map(
    ~ds.exwas(
      paste0("ext_z~1"),
      type = "meta",
      Set = "ext_exp_set", 
      family = "gaussian", 
      tef = FALSE,
      datasources = conns[.x])) %>%
  set_names(ext_coh)

save.image()

## ---- ADHD -------------------------------------------------------------------
adhd.ewas <- adhd_coh %>%
  map(
    ~ds.exwas(
      paste0("adhd_z~", cov_form_min),
      type = "meta",
      Set = "adhd_exp_set", 
      family = "gaussian", 
      tef = FALSE,
      datasources = conns[.x])) %>%
  set_names(adhd_coh)

save.image()

## ---- Language ---------------------------------------------------------------
lan.ewas <- lan_coh %>%
  map(
    ~ds.exwas(
      paste0("lan_z~", cov_form_min),
      type = "meta",
      Set = "lan_exp_set", 
      family = "gaussian", 
      tef = FALSE,
      datasources = conns[.x])) %>%
  set_names(lan_coh)

save.image()

## ---- Non-verbal intelligence ------------------------------------------------
nvi.ewas <- nvi_coh %>%
  map(
    ~ds.exwas(
      paste0("nvi_z~", cov_form_min),
      type = "meta",
      Set = "nvi_exp_set", 
      family = "gaussian", 
      tef = FALSE,
      datasources = conns[.x])) %>%
  set_names(nvi_coh)

save.image()

## ---- Working memory ---------------------------------------------------------
wm.ewas <- wm_coh %>%
  map(
    ~ds.exwas(
      paste0("wm_z~", cov_form_min),
      type = "meta",
      Set = "wm_exp_set", 
      family = "gaussian", 
      tef = FALSE,
      datasources = conns[.x])) %>%
  set_names(wm_coh)

save.image()

## ---- Fine motor -------------------------------------------------------------
fm.ewas <- fm_coh %>%
  map(
    ~ds.exwas(
      paste0("fm_z~", cov_form_min),
      type = "meta",
      Set = "fm_exp_set", 
      family = "gaussian", 
      tef = FALSE,
      datasources = conns[.x])) %>%
  set_names(fm_coh)

save.image()

## ---- Gross motor ------------------------------------------------------------
gm.ewas <- gm_coh %>%
  map(
    ~ds.exwas(
      paste0("gm_z~", cov_form_min),
      type = "meta",
      Set = "gm_exp_set", 
      family = "gaussian", 
      tef = FALSE,
      datasources = conns[.x])) %>%
  set_names(gm_coh)

save.image()



library(dsExposomeClient)

ds.loadExposome(
  exposures = "exposures_d",
  phenotypes = "int_pheno", 
  exposures.idcol = "child_id", 
  phenotypes.idcol = "child_id", 
  object_name = "test", 
  datasources = conns["inma_val"])


ds.exwas(
  paste0("int_z_t~", cov_form_min),
  type = "meta",
  Set = "test", 
  family = "gaussian", 
  tef = FALSE,
  datasources = conns["inma_val"])

ds.colnames("exposures_d")

################################################################################
# Check data  
################################################################################
int_stats <- dh.getStats(
  df = "int_sub_z", 
  vars = "int_z")

cov_stats <- dh.getStats(
  df = "covariates",
  vars = c("agebirth_m_y", "sex", "parity_bin", "birth_month_f", "edu_m_.0_1", 
           "areases_tert_.0_1"))

cov_stats$categorical %>% print(n = Inf)
cov_stats$continuous %>% print(n = Inf)





exp_inma_val.vars %>% print(n = Inf)

exp.stats$continuous %>% print(n = Inf)




## ---- Externalising ----------------------------------------------------------







## ---- Internalising ----------------------------------------------------------
exwas_int <- ds.exwas(
  paste0("int_z~", cov_form_min),
  type = "meta",
  Set = "int_exp_set", 
  family = "gaussian", 
  datasources = conns["inma_sab"])

exwas_int_val <- ds.exwas(
  paste0("int_z~", cov_form_min),
  type = "meta",
  Set = "int_exp_set", 
  family = "gaussian", 
  tef = F,
  datasources = conns["inma_val"])

exwas_int_ninfea <- ds.exwas(
  paste0("int_z~", cov_form_min),
  type = "meta",
  Set = "int_exp_set", 
  family = "gaussian", 
  tef = F,
  datasources = conns["ninfea"])






exp.vars <- ds.colnames("exposures")[[1]]



exp.stats$continuous %>% print(n = Inf)
exp.stats$categorical %>% print(n = Inf)

ds.colnames("ext_sub_z")

ds.exposome_variables("int_exp_set", target = "exposures")
ds.exposome_variables("ext_exp_set", target = "phenotypes")


int_exp_set

ds.dim("int_exp_set")

ninfea
trafload100_preg
trafnear_preg


## ---- Internalising ----------------------------------------------------------
exwas_int <- ds.exwas(
  paste0("int_z~", cov_form_min),
  type = "meta",
  Set = "int_exp_set", 
  family = "gaussian", 
  datasources = conns["inma_sab"])

exwas_int_val <- ds.exwas(
  paste0("int_z~1"),
  type = "meta",
  Set = "int_exp_set", 
  family = "gaussian", 
  tef = F,
  datasources = conns["inma_val"])

exwas_int_ninfea <- ds.exwas(
  paste0("int_z~1"),
  type = "meta",
  Set = "int_exp_set", 
  family = "gaussian", 
  tef = F,
  datasources = conns["ninfea"])


ds.histogram("exposures$bus_stops_100_preg_iqr_c")
  
  
str(exwas_int_val)

exwas_int <- ds.exwas(
  paste0("int_z~", cov_form_min),
  type = "meta",
  Set = "int_exp_set", 
  family = "gaussian", 
  datasources = conns["ninfea"])

save.image()

str(exwas_int_val)

## ---- Externalising ----------------------------------------------------------
exwas_ext <- ds.exwas(
  paste0("ext_z~", cov_form_min),
  type = "meta",
  Set = "ext_exp_set", 
  family = "gaussian", 
  datasources = conns[ext_coh])

## ---- ADHD -------------------------------------------------------------------
exwas_adhd <- ds.exwas(
  paste0("adhd_z~", cov_form_min),
  type = "meta",
  Set = "adhd_exp_set", 
  family = "gaussian", 
  datasources = conns[adhd_coh])

## ---- Language ---------------------------------------------------------------
exwas_lan <- ds.exwas(
  paste0("lan_z~", cov_form_min),
  type = "meta",
  Set = "lan_exp_set", 
  family = "gaussian", 
  datasources = conns[lan_coh])

## ---- Non-verbal intelligence ------------------------------------------------
exwas_nvi <- ds.exwas(
  paste0("nvi_z~", cov_form_min),
  type = "meta",
  Set = "nvi_exp_set", 
  family = "gaussian", 
  datasources = conns[nvi_coh])

## ---- Working memory ---------------------------------------------------------
exwas_wm <- ds.exwas(
  paste0("wm_z~", cov_form_min),
  type = "meta",
  Set = "wm_exp_set", 
  family = "gaussian", 
  datasources = conns[wm_coh])

## ---- Fine motor -------------------------------------------------------------
exwas_fm <- ds.exwas(
  paste0("fm_z~", cov_form_min),
  type = "meta",
  Set = "fm_exp_set", 
  family = "gaussian", 
  datasources = conns[fm_coh])

## ---- Gross motor ------------------------------------------------------------
exwas_fm <- ds.exwas(
  paste0("gm_z~", cov_form_min),
  type = "meta",
  Set = "gm_exp_set", 
  family = "gaussian", 
  datasources = conns[gm_coh])

save.image()
















## ---- Summarise variables ----------------------------------------------------
ds.exposome_variables("exposome_set", target = "exposures")
ds.exposome_variables("exposome_set", target = "phenotypes")


## ---- Summarise missingness --------------------------------------------------
missing <- ds.tableMissings("exposome_set", set = "exposures")
ds.plotMissings(missing, "exposures", "p")


## ---- Impute missing ---------------------------------------------------------
#ds.imputation("exposome_set", "exposome_set_imputed")



ds.loadExposome(
  exposures = "exposures",
  phenotypes = "lan_pheno", 
  exposures.idcol = "child_id", 
  phenotypes.idcol = "child_id", 
  object_name = "exposome_set")

## ---- Perform an EXWAS -------------------------------------------------------
exwas_results <- ds.exwas(
  "lan_raw_.0_17 ~ agebirth_m_y + sex + parity_bin + birth_month_f + edu_m_.0_1 +
  areases_tert_.0_1", Set = "exposome_set", family = "gaussian")

str(exwas_results)


exwas_results

download.file("https://raw.githubusercontent.com/biocorecrg/CRG_RIntroduction/master/de_df_for_volcano.rds", "de_df_for_volcano.rds", method="curl")

# The RDS format is used to save a single R object to a file, and to restore it.
# Extract that object in the current session:
tmp <- readRDS("de_df_for_volcano.rds")

# remove rows that contain NA values
de <- tmp[complete.cases(tmp), ]



exwas_plot <- ds.plotExwas(exwas_results, "manhattan")

ggsave(
  exwas_plot, 
  file = here("figures", "exwas_ninfea.png"), 
  device = "png")


ds.colnames("lan_sub")


ds.loadExposome(
  exposures = "exp_data",
  phenotypes = "exp_mh_wide", 
  exposures.idcol = "child_id",
  phenotypes.idcol = "child_id")

datashield.assign.resource(conns, symbol = 'exposures', resource = list(server1 = 'EXPOSOME.exposures'))
datashield.assign.expr(conns, symbol = "exposures", expr = quote(as.resource.data.frame(exposures)))

datashield.assign.resource(conns, symbol = 'phenotypes', resource = list(server1 = 'EXPOSOME.phenotypes'))
datashield.assign.expr(conns, symbol = "phenotypes", expr = quote(as.resource.data.frame(phenotypes)))

ds.ls()


ds.colnames("exp_mh_long")

exp_mh_long

datashield.assign.resource(conns, symbol = 'exposures', resource = list(server1 = 'EXPOSOME.exposures'))

datashield.assign.expr(conns, symbol = "exposures", expr = quote(as.resource.data.frame(exposures)))

datashield.assign.resource(conns, symbol = 'phenotypes', resource = list(server1 = 'EXPOSOME.phenotypes'))
datashield.assign.expr(conns, symbol = "phenotypes", expr = quote(as.resource.data.frame(phenotypes)))