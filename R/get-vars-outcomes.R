################################################################################
## Project: Exposome and Mental Health    
## Script purpose: Prepare outcomes 
## Date: 13th April 2022
## Author: Tim Cadman
## Email: tica@sund.ku.dk
################################################################################

conns <- datashield.login(logindata, restore = "exp-mh")

################################################################################
# 1. Fill  
################################################################################
ds.dataFrameFill("mh_rep_sub", "mh_rep_sub") 

datashield.workspace_save(conns, "exp-mh")
################################################################################
# 2. Fix levels  
################################################################################
dh.columnCast(
  df = "mh_rep_sub", 
  target_vars = c("ext_instr_", "int_instr_", "adhd_instr_", "wm_instr_", 
                  "lan_instr_", "nvi_instr_", "fm_instr_", "gm_instr_"),
  target_class = "factor", 
  checks = FALSE)

new_conns <- c("abdc", "alspac", "dnbc", "eden_nan", "eden_poit", "genr")

datashield.workspace_save(conns, "exp-mh")


## Let's re-write the boxcox function to use the anonymised data for now. 
## Can give as output a table with the best transformation

################################################################################
# 3. Identify cohorts  
################################################################################

## ---- Helper function --------------------------------------------------------
cohAvail <- function(df, var){

  stats <- dh.getStats(
    df = df,
    vars = var, 
    checks = F)
  
    stats$continuous %>%
    dplyr::filter(variable == var & !is.na(mean) & cohort != "combined") %>%
    pull(cohort) %>%
    as.character()
    
}

## ---- Define cohorts with data -----------------------------------------------
int_coh <- cohAvail(df = "mh_rep_sub", var = "int_raw_")
ext_coh <- cohAvail(df = "mh_rep_sub", var = "ext_raw_")
adhd_coh <- cohAvail(df = "mh_rep_sub", var = "adhd_raw_")
lan_coh <- cohAvail(df = "mh_rep_sub", var = "lan_raw_")
nvi_coh <- cohAvail(df = "mh_rep_sub", var = "nvi_raw_")
wm_coh <- cohAvail(df = "mh_rep_sub", var = "wm_raw_")
fm_coh <- cohAvail(df = "mh_rep_sub", var = "fm_raw_")
gm_coh <- cohAvail(df = "mh_rep_sub", var = "gm_raw_")

save.image()

################################################################################
# 4. Define subsets with correct instruments  
################################################################################

## ---- Create copies of mh df -------------------------------------------------
c("int_inst", "ext_inst", "adhd_inst", "lan_inst", "nvi_inst", "wm_inst", 
  "fm_inst", "gm_inst") %>%
  map(
    ~ds.assign(toAssign = "mh_rep_sub", newobj = .x))

datashield.workspace_save(conns, "exp-mh")

## ---- Subset to correct instrument -------------------------------------------
instr.ref <- tribble(
  ~cohort, ~outcome, ~inst_var, ~inst, ~new_obj, 
  "rhea", "int", "int_instr_", "13", "int_inst",
  "inma_gip", "int", "int_instr_", "13", "int_inst", 
  "inma_sab", "int", "int_instr_", "13", "int_inst", 
  "inma_val", "int", "int_instr_", "13", "int_inst", 
  "rhea", "ext", "ext_instr_", "13", "ext_inst", 
  "inma_gip", "ext", "ext_instr_", "13", "ext_inst", 
  "inma_sab", "ext", "ext_instr_", "13", "ext_inst", 
  "inma_val", "ext", "ext_instr_", "13", "ext_inst", 
  "inma_gip", "adhd", "adhd_instr_", "20", "adhd_inst",
  "inma_sab", "adhd", "adhd_instr_", "20", "adhd_inst",
  "inma_val", "adhd", "adhd_instr_", "20", "adhd_inst",
  "moba", "adhd", "adhd_instr", "25", "adhd_inst", 
  "eden_nan", "nvi", "nvi_instr_", "60", "nvi_inst", 
  "eden_poit", "nvi", "nvi_instr_", "60", "nvi_inst", 
  "inma_gip", "nvi", "nvi_instr_", "45", "nvi_inst", 
  "inma_sab", "nvi", "nvi_instr_", "45", "nvi_inst", 
  "inma_val", "nvi", "nvi_instr_", "45", "nvi_inst", 
  "rhea", "nvi", "nvi_instr_", "45", "nvi_inst", 
  "inma_gip", "lan", "lan_instr_", "52", "lan_inst",
  "inma_sab", "lan", "lan_instr_", "52", "lan_inst",
  "inma_val", "lan", "lan_instr_", "52", "lan_inst",
  "rhea", "lan", "lan_instr_", "59", "lan_inst",
  "alspac", "lan", "lan_instr_", "60", "lan_inst",
  "eden_nan", "lan", "lan_instr_", "60", "lan_inst",
  "eden_poit", "lan", "lan_instr_", "60", "lan_inst",
  "inma_gip", "fm", "fm_instr_", "39", "fm_inst",
  "inma_sab", "fm", "fm_instr_", "39", "fm_inst",
  "inma_val", "fm", "fm_instr_", "39", "fm_inst",
  "rhea", "fm", "fm_instr_", "10", "fm_inst",
  "eden_nan", "fm", "fm_instr_", "4", "fm_inst",
  "eden_poit", "fm", "fm_instr_", "4", "fm_inst",
  "eden_nan", "gm", "gm_instr_", "4", "gm_inst",
  "eden_poit", "gm", "gm_instr_", "4", "gm_inst",
  "rhea", "gm", "gm_instr_", "39", "gm_inst")

instr.ref %>%
  dplyr::filter(outcome == "gm") %>%
  pmap(function(cohort, outcome, inst_var, inst, new_obj){
    
    ds.dataFrameSubset(
      df.name = new_obj, 
      V1.name = paste0(new_obj, "$", inst_var),
      V2.name = inst, 
      Boolean.operator = "==", 
      newobj = new_obj, 
      datasources = conns[cohort])
    
  })
    
datashield.workspace_save(conns, "exp-mh")

################################################################################
# 3. Subset for oldest age point
################################################################################

## ---- Reference table --------------------------------------------------------
out_sub_ref <- tibble(
  var = c("int", "ext", "adhd", "lan", "nvi", "wm", "fm", "gm"), 
  coh = list(int_coh, ext_coh, adhd_coh, lan_coh, nvi_coh, wm_coh, fm_coh, 
             gm_coh))

save.image()

## ---- Internalising ----------------------------------------------------------
conns <- datashield.login(logindata, restore = "exp-mh")

out_sub_ref %>%
  dplyr::filter(var == "int") %>%
  pmap(function(var, coh){
    
    dh.makeStrata(
      df = paste0(var, "_inst"), 
      id_var = "child_id", 
      age_var = paste0(var, "_age_"), 
      var_to_subset = paste0(var, "_raw_"), 
      bands = c(0, 17), 
      mult_action = "latest", 
      band_action = "g_le", 
      keep_vars = "int_instr_",
      new_obj = paste0(var, "_sub"), 
      checks = FALSE,
      conns = conns[unlist(coh)])
    
  })

datashield.workspace_save(conns, "exp-mh")

## ---- Externalising ----------------------------------------------------------
conns <- datashield.login(logindata, restore = "exp-mh")

out_sub_ref %>%
  dplyr::filter(var == "ext") %>%
  pmap(function(var, coh){
    
    dh.makeStrata(
      df = paste0(var, "_inst"),
      id_var = "child_id", 
      age_var = paste0(var, "_age_"), 
      var_to_subset = paste0(var, "_raw_"), 
      bands = c(0, 17), 
      mult_action = "latest", 
      band_action = "g_le", 
      keep_vars = "ext_instr_",
      new_obj = paste0(var, "_sub"), 
      checks = FALSE,
      conns = conns[unlist(coh)])
    
  })

datashield.workspace_save(conns, "exp-mh")

## ---- ADHD ----------------------------------------------------------
conns <- datashield.login(logindata, restore = "exp-mh")

out_sub_ref %>%
  dplyr::filter(var == "adhd") %>%
  pmap(function(var, coh){
    
    dh.makeStrata(
      df = paste0(var, "_inst"),
      id_var = "child_id", 
      age_var = paste0(var, "_age_"), 
      var_to_subset = paste0(var, "_raw_"), 
      bands = c(0, 17), 
      mult_action = "latest", 
      band_action = "g_le", 
      keep_vars = "adhd_instr_",
      new_obj = paste0(var, "_sub"), 
      checks = FALSE,
      conns = conns[unlist(coh)])
    
  })

datashield.workspace_save(conns, "exp-mh")

## ---- Non-verbal -------------------------------------------------------------
conns <- datashield.login(logindata, restore = "exp-mh")

out_sub_ref %>%
  dplyr::filter(var == "nvi") %>%
  pmap(function(var, coh){
    
    dh.makeStrata(
      df = paste0(var, "_inst"),
      id_var = "child_id", 
      age_var = paste0(var, "_age_"), 
      var_to_subset = paste0(var, "_raw_"), 
      bands = c(0, 17), 
      mult_action = "latest", 
      band_action = "g_le", 
      keep_vars = "nvi_instr_",
      new_obj = paste0(var, "_sub"), 
      checks = FALSE,
      conns = conns[unlist(coh)])
    
  })

datashield.workspace_save(conns, "exp-mh")

## ---- Language ----------------------------------------------------------------
conns <- datashield.login(logindata, restore = "exp-mh")

out_sub_ref %>%
  dplyr::filter(var == "lan") %>%
  pmap(function(var, coh){
    
    dh.makeStrata(
      df = paste0(var, "_inst"),
      id_var = "child_id", 
      age_var = paste0(var, "_age_"), 
      var_to_subset = paste0(var, "_raw_"), 
      bands = c(0, 17), 
      mult_action = "latest", 
      band_action = "g_le", 
      keep_vars = "lan_instr_",
      new_obj = paste0(var, "_sub"), 
      checks = FALSE,
      conns = conns[unlist(coh)])
    
  })

datashield.workspace_save(conns, "exp-mh")

## ---- Working memory ---------------------------------------------------------
conns <- datashield.login(logindata, restore = "exp-mh")

out_sub_ref %>%
  dplyr::filter(var == "wm") %>%
  pmap(function(var, coh){
    
    dh.makeStrata(
      df = "mh_rep_sub",
      id_var = "child_id", 
      age_var = paste0(var, "_age_"), 
      var_to_subset = paste0(var, "_raw_"), 
      bands = c(0, 17), 
      mult_action = "latest", 
      band_action = "g_le", 
      keep_vars = "wm_instr_",
      new_obj = paste0(var, "_sub"), 
      checks = FALSE,
      conns = conns[unlist(coh)])
    
  })

datashield.workspace_save(conns, "exp-mh")

## ---- Fine motor -------------------------------------------------------------
conns <- datashield.login(logindata, restore = "exp-mh")

out_sub_ref %>%
  dplyr::filter(var == "fm") %>%
  pmap(function(var, coh){
    
    dh.makeStrata(
      df = paste0(var, "_inst"),
      id_var = "child_id", 
      age_var = paste0(var, "_age_"), 
      var_to_subset = paste0(var, "_raw_"), 
      bands = c(0, 17), 
      mult_action = "latest", 
      band_action = "g_le", 
      keep_vars = "fm_instr_",
      new_obj = paste0(var, "_sub"), 
      checks = FALSE,
      conns = conns[unlist(coh)])
    
  })

datashield.workspace_save(conns, "exp-mh")

## ---- Gross motor ------------------------------------------------------------
conns <- datashield.login(logindata, restore = "exp-mh")

out_sub_ref %>%
  dplyr::filter(var == "gm") %>%
  pmap(function(var, coh){
    
    dh.makeStrata(
      df = paste0(var, "_inst"),
      id_var = "child_id", 
      age_var = paste0(var, "_age_"), 
      var_to_subset = paste0(var, "_raw_"), 
      bands = c(0, 17), 
      mult_action = "latest", 
      band_action = "g_le", 
      keep_vars = "gm_instr_",
      new_obj = paste0(var, "_sub"), 
      checks = FALSE,
      conns = conns[unlist(coh)])
    
  })

ds.ls()
datashield.workspace_save(conns, "exp-mh")

################################################################################
# 4. Transform where skewed  
################################################################################
conns <- datashield.login(logindata, restore = "exp-mh")

## ---- Internalising ----------------------------------------------------------
ds.assign(
  toAssign = "int_sub$int_raw_.0_17^0.5", 
  newobj = "int_t", 
  datasources = conns[int_coh])

ds.dataFrame(
  x = c("int_sub", "int_t"), 
  newobj = "int_sub")

ds.histogram(
  x = "int_t", 
  num.breaks = 20, 
  datasources = conns[int_coh])

## ---- Externalising ----------------------------------------------------------
ds.assign(
  toAssign = "ext_sub$ext_raw_.0_17^0.5", 
  newobj = "ext_t", 
  datasources = conns[ext_coh])

ds.dataFrame(
  x = c("ext_sub", "ext_t"), 
  newobj = "ext_sub")

ds.histogram(
  x = "ext_t", 
  num.breaks = 20, 
  datasources = conns[ext_coh])

datashield.workspace_save(conns, "exp-mh")

## ---- ADHD -------------------------------------------------------------------
ds.assign(
  toAssign = "adhd_sub$adhd_raw_.0_17^0.5", 
  newobj = "adhd_t", 
  datasources = conns[adhd_coh])

ds.dataFrame(
  x = c("adhd_sub", "adhd_t"), 
  newobj = "adhd_sub", 
  datasources = conns[adhd_coh])

ds.histogram(
  x = "adhd_t", 
  num.breaks = 20, 
 datasources = conns[adhd_coh])

datashield.workspace_save(conns, "exp-mh")


## ---- Language ---------------------------------------------------------------
ds.assign(
  toAssign = "lan_sub$lan_raw_.0_17^0.5", 
  newobj = "lan_t", 
  datasources = conns[lan_coh])

ds.dataFrame(
  x = c("lan_sub", "lan_t"), 
  newobj = "lan_sub", 
  datasources = conns[lan_coh])

ds.histogram(
  x = "lan_t", 
  num.breaks = 20,  
  datasources = conns[lan_coh])

## ---- NVI --------------------------------------------------------------------
ds.colnames("nvi_sub", datasources = conns[nvi_coh])

## ---- Working memory ---------------------------------------------------------
ds.assign(
  toAssign = "wm_sub$wm_raw_.0_17^0.5", 
  newobj = "wm_t", 
  datasources = conns[wm_coh])

ds.dataFrame(
  x = c("wm_sub", "wm_t"), 
  newobj = "wm_sub", 
  datasources = conns[wm_coh])

ds.histogram(
  x = "wm_t",
  num.breaks = 20,  
  datasources = conns[wm_coh])

## ---- Fine motor -------------------------------------------------------------
ds.assign(
  toAssign = "fm_sub$fm_raw_.0_17^0.5", 
  newobj = "fm_t", 
  datasources = conns[fm_coh])

ds.dataFrame(
  x = c("fm_sub", "fm_t"), 
  newobj = "fm_sub", 
  datasources = conns[fm_coh])

ds.histogram(
  x = "fm_t", 
  num.breaks = 20, 
  datasources = conns[fm_coh])

## ---- Gross motor ------------------------------------------------------------
ds.assign(
  toAssign = "gm_sub$gm_raw_.0_17^0.5", 
  newobj = "gm_t", 
  datasources = conns[gm_coh])

ds.dataFrame(
  x = c("gm_sub", "gm_t"), 
  newobj = "gm_sub", 
  datasources = conns[gm_coh])

ds.histogram(
  x = "gm_t", 
  num.breaks = 20, 
  datasources = conns[gm_coh])

datashield.workspace_save(conns, "exp-mh")

################################################################################
# 5. Convert to z-scores  
################################################################################

## ---- Helper function --------------------------------------------------------
makeZscore <- function(df, var, new_var, new_obj, conns){
  
  stats <- dh.getStats(
    df = df,
    var = var, 
    conns = conns)
  
  ref <- stats$continuous %>%
    dplyr::select(cohort, variable, mean, std.dev) %>%
    dplyr::filter(cohort != "combined")
  
  ref %>% 
    pmap(function(cohort, variable, mean, std.dev){
      
      assign_form <- paste0("((", paste0(df, "$", variable), "-", mean, ")", 
                            "/", std.dev, ")")
      ds.assign(
        toAssign = assign_form,
        newobj = new_var, 
        datasources = conns[cohort])
      
      ds.dataFrame(
        x = c(df, new_var), 
        newobj = new_obj, 
        datasources = conns[cohort])
      
    })
      
}

## ---- Make z-scores ----------------------------------------------------------
out_sub_ref %>%
  mutate(z_var = c("int_t", "ext_t", "adhd_t", "lan_t", "nvi_raw_.0_17",
                   "wm_t", "fm_t", "gm_t")) %>%
  pmap(function(var, z_var, coh, ...){
    
    makeZscore(
      df = paste0(var, "_sub"),
      var = z_var, 
      new_var = paste0(var, "_z"),
      new_obj = paste0(var, "_sub_z"),
      conns = conns[unlist(coh)])
      
    })

datashield.workspace_save(conns, "exp-mh")

################################################################################
# Transform long-form data  
################################################################################

ds.colnames("int_inst")

## ---- Internalising ----------------------------------------------------------
ds.assign(
  toAssign = "int_inst$int_raw_^0.5", 
  newobj = "int_t_long", 
  datasources = conns[int_coh])

ds.dataFrame(
  x = c("int_inst", "int_t_long"), 
  newobj = "int_long", 
  datasources = conns[int_coh])

ds.histogram(
  x = "int_t_long", 
  num.breaks = 20, 
  datasources = conns[int_coh])

## ---- Externalising ----------------------------------------------------------
ds.assign(
  toAssign = "ext_inst$ext_raw_^0.5", 
  newobj = "ext_t_long", 
  datasources = conns[ext_coh])

ds.dataFrame(
  x = c("ext_inst", "ext_t_long"), 
  newobj = "ext_long", 
  datasources = conns[ext_coh])

ds.histogram(
  x = "ext_t_long", 
  num.breaks = 20, 
  datasources = conns[ext_coh])

## ---- ADHD -------------------------------------------------------------------
ds.assign(
  toAssign = "adhd_inst$adhd_raw_^0.5", 
  newobj = "adhd_t_long", 
  datasources = conns[adhd_coh])

ds.dataFrame(
  x = c("adhd_inst", "adhd_t_long"), 
  newobj = "adhd_long", 
  datasources = conns[adhd_coh])

ds.histogram(
  x = "adhd_t_long", 
  num.breaks = 20, 
  datasources = conns[adhd_coh])

datashield.workspace_save(conns, "exp-mh")

################################################################################
# Z-scores long-format data  
################################################################################
out_sub_ref %>%
  dplyr::filter(var %in% c("int", "ext", "adhd")) %>%
  mutate(z_var = c("int_t_long", "ext_t_long", "adhd_t_long")) %>%
  pmap(function(var, z_var, coh, ...){
    
    makeZscore(
      df = paste0(var, "_long"),
      var = z_var, 
      new_var = paste0(var, "_z"),
      new_obj = paste0(var, "_sub_z_long"),
      conns = conns[unlist(coh)])
    
  })

ds.histogram(
  x = "int_sub_z_long$int_z", 
  num.breaks = 20, 
  datasources = conns[int_coh])

ds.histogram(
  x = "ext_sub_z_long$ext_z", 
  num.breaks = 20, 
  datasources = conns[ext_coh])

ds.histogram(
  x = "adhd_sub_z_long$adhd_z", 
  num.breaks = 20, 
  datasources = conns[adhd_coh])

datashield.workspace_save(conns, "exp-mh")








c("int_inst", "ext_inst", "adhd_inst", "lan_inst", "nvi_inst", "wm_inst", 
  "fm_inst", "gm_inst") %>%










## ---- Internalising ----------------------------------------------------------
int_pre <- ds.histogram("int_sub$int_raw_")

int_trans <- dh.boxCox(
  df = "int_sub", 
  var = "int_raw_", 
  lamda = seq(-2, 2, 0.2),
  type = "combine",
  transform = TRUE,
  new_obj = "int_t", 
  checks = FALSE)

int_post <- ds.histogram("int_t")

ds.dataFrame(
  x = c("int_sub", "int_t"), 
  newobj = "int_sub")

datashield.workspace_save(conns, "exp-mh")

## ---- Externalising ----------------------------------------------------------
ext_pre <- ds.histogram("ext_sub$ext_raw_")

int_trans <- dh.boxCox(
  df = "ext_sub", 
  var = "ext_raw_", 
  lamda = seq(-2, 2, 0.2),
  type = "combine",
  transform = TRUE,
  new_obj = "ext_t", 
  checks = FALSE)

ext_post <- ds.histogram("ext_t")

ds.dataFrame(
  x = c("ext_sub", "ext_t"), 
  newobj = "ext_sub")

datashield.workspace_save(conns, "exp-mh")

## ---- ADHD ----------------------------------------------------------
ext_pre <- ds.histogram("adhd_sub$adhd_raw_")

int_trans <- dh.boxCox(
  df = "adhd_sub", 
  var = "adhd_raw_", 
  lamda = seq(-2, 2, 0.2),
  type = "combine",
  transform = TRUE,
  new_obj = "adhd_t", 
  checks = FALSE)

ext_post <- ds.histogram("adhd_t")

ds.dataFrame(
  x = c("adhd_sub", "adhd_t"), 
  newobj = "adhd_sub")

datashield.workspace_save(conns, "exp-mh")

## ---- Language ---------------------------------------------------------------
lan_pre <- ds.histogram("lan_sub$lan_raw_")

lan_trans <- dh.boxCox(
  df = "lan_sub", 
  var = "lan_raw_", 
  lamda = seq(-2, 2, 0.2),
  type = "combine",
  transform = TRUE,
  new_obj = "lan_t", 
  checks = FALSE)

lan_post <- ds.histogram("lan_t")

ds.dataFrame(
  x = c("lan_sub", "lan_t"), 
  newobj = "lan_sub")

## ---- NVI --------------------------------------------------------------------
nvi_pre <- ds.histogram("nvi_sub$nvi_raw_", datasources = conns[nvi_coh])

## ---- Working memory ---------------------------------------------------------
wm_pre <- ds.histogram("wm_sub$wm_raw_", datasources = conns[wm_coh])

wm_trans <- dh.boxCox(
  df = "wm_sub", 
  var = "wm_raw_", 
  lamda = seq(-2, 2, 0.2),
  type = "combine",
  transform = TRUE,
  new_obj = "wm_t", 
  checks = FALSE, 
  conns = conns[wm_coh])

wm_post <- ds.histogram("wm_t", datasources = conns[wm_coh])

ds.dataFrame(
  x = c("wm_sub", "wm_t"), 
  newobj = "wm_sub", datasources = conns[wm_coh])

## ---- Fine motor -------------------------------------------------------------
fm_pre_inma_g <- ds.histogram("fm_sub$fm_raw_", datasources = conns["inma_gip"])
fm_pre_ninfea <- ds.histogram("fm_sub$fm_raw_", datasources = conns["ninfea"])

fm_trans <- dh.boxCox(
  df = "fm_sub", 
  var = "fm_raw_", 
  lamda = seq(-2, 2, 0.2),
  type = "combine",
  transform = TRUE,
  new_obj = "fm_t", 
  checks = FALSE, 
  conns = conns[fm_coh])

fm_post <- ds.histogram("fm_t", datasources = conns["inma_gip"])
fm_post <- ds.histogram("fm_t", datasources = conns["ninfea"])

ds.dataFrame(
  x = c("fm_sub", "fm_t"), 
  newobj = "fm_sub", 
  datasources = conns[fm_coh])

## ---- Gross motor ------------------------------------------------------------
gm_pre_inma_g <- ds.histogram("gm_sub$gm_raw_", datasources = conns["inma_gip"])
gm_pre_ninfea <- ds.histogram("gm_sub$gm_raw_", datasources = conns["ninfea"])

gm_trans <- dh.boxCox(
  df = "gm_sub", 
  var = "gm_raw_", 
  lamda = seq(-2, 2, 0.2),
  type = "combine",
  transform = TRUE,
  new_obj = "gm_t", 
  checks = FALSE, 
  conns = conns[gm_coh])

gm_post_inma_g <- ds.histogram("gm_t", datasources = conns["inma_gip"])
gm_post_ninfea <- ds.histogram("gm_t", datasources = conns["ninfea"])

ds.dataFrame(
  x = c("gm_sub", "gm_t"), 
  newobj = "gm_sub", 
  datasources = conns[gm_coh])

datashield.workspace_save(conns, "exp-mh")


