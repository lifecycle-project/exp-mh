################################################################################
## Project: Exposome and Mental Health    
## Script purpose: Assign variables and check 
## Date: 13th April 2022
## Author: Tim Cadman
## Email: tica@sund.ku.dk
################################################################################

################################################################################
# 1. Define tables
################################################################################
cohort_tables <- read_csv(file = here("reference", "table-ref.csv"))

################################################################################
# 2. Assign variables
################################################################################
cohorts_tables %>%
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




