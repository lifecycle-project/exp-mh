################################################################################
## Project: Exposome and Mental Health    
## Script purpose: Assign variables and check 
## Date: 13th April 2022
## Author: Tim Cadman
## Email: tica@sund.ku.dk
################################################################################



library(here)

library(DSMolgenisArmadillo)

install.packages("MolgenisArmadillo")
library(MolgenisArmadillo)

library(arma)

ls("package:DSMolgenisArmadillo")
ls("package:MolgenisArmadillo")

conns <- datashield.login(logindata, restore = "exp-mh")

opal_coh <- c("dnbc", "genr", "inma_val", "ninfea", "rhea")
arm_coh <- c("abcd", "alspac", "bib", "eden_nan")

datashield.pkg_status(conns[opal_coh])
datashield.pkg_status(conns)

armadillo.whitelist_packages(conns)

dsListPackages("arm_coh")


arm_coh <- 

conns <- datashield.login(logindata, restore = "exp-mh")

################################################################################
# 1. Define tables
################################################################################
all_vars <- read_csv(file = here("reference", "var-ref.csv"))

vars_collapse <- tibble(
  df_name = sort(unique(all_vars$df_name)), 
  vars = all_vars %>% 
    group_by(df_name) %>%
    group_split %>%
    map(function(x){x %>% pull(variable)}))

cohort_tables <- left_join(
  read_csv(file = here("reference", "table-ref.csv")), 
  vars_collapse, 
  by = "df_name")

################################################################################
# 2. Assign variables
################################################################################
current_coh <- names(conns)
## ---- All cohorts with initial access ----------------------------------------
cohort_tables %>%
  dplyr::filter(cohort %in% current_coh) %>%
  pwalk(function(cohort, table, df_name, vars){
    
    datashield.assign(
      conns = conns[cohort], 
      symbol = df_name, 
      value = table, 
      variables = unlist(vars))
  })

datashield.workspace_save(conns, "exp-mh")

################################################################################
# 3. Merge outcome tables where they are separate  
################################################################################
sep_out_coh <- cohort_tables %>%
  dplyr::filter(df_name == "mh_rep_2") %>%
  pull(cohort)

sep_out_coh <- names(conns)

ds.merge(
  x.name = "mh_rep", 
  y.name = "mh_rep_2",
  by.x.names = "child_id", 
  by.y.names = "child_id",
  all.x = TRUE,
  all.y = TRUE,
  newobj = "mh_rep", 
  datasources = conns[sep_out_coh])

datashield.workspace_save(conns, "exp-mh")

################################################################################
# 4. Check what is currently available  
################################################################################
dfs <- c("non_rep", "year_rep", "mh_rep")

avail.vars <- dfs %>%
  map(function(x){
    
    out <- ds.colnames(x) %>% 
      unlist() %>% 
      unique
    
    out <- out[out != "cohort_id"]
    
    return(out)
    }) %>%
  set_names(dfs)

save.image()

################################################################################
# 3. Check data
################################################################################
df_valid <- vars_collapse %>%
  dplyr::filter(df_name != "mh_rep_2")

available <- df_valid %>%
  pmap(function(df_name, vars){
    
    dh.classDiscrepancy(
      df = df_name,
      vars = unlist(vars))
  }) %>%
  set_names(sort(df_valid$df_name))

available$non_rep %>% print(n = Inf)
available$year_rep %>% print(n = Inf)
available$mh_rep %>% print(n = Inf)

save.image()

################################################################################
# 4. Fill missing variables  
################################################################################
ds.dataFrameFill("non_rep", "non_rep_fill")
ds.dataFrameFill("mh_rep", "mh_rep_fill")
ds.dataFrameFill("year_rep", "year_rep_fill")

#ds.assign("non_rep", "non_rep_fill")
#ds.assign("year_rep", "year_rep_fill")
#ds.assign("mh_rep", "mh_rep_fill")

datashield.workspace_save(conns, "exp-mh")

################################################################################
# 5. See what is available  
################################################################################
non_rep_avail <- dh.getStats(
  df = "non_rep",
  vars = avail.vars$non_rep)

year_rep_avail <- dh.getStats(
  df = "year_rep",
  vars = avail.vars$year_rep)

mh_avail <- dh.getStats(
  df = "mh_rep",
  vars = c("adhd_raw_", "ext_raw_", "int_raw_", "nvi_raw_", "lan_raw_", 
           "wm_raw_", "fm_raw_", "gm_raw_"))

save.image()

missing_exp <- non_rep_avail$continuous %>%
  dplyr::filter(is.na(std.dev)) %>%
  dplyr::select(variable, cohort) %>%
  arrange(cohort)

missing_out <- mh_avail$continuous %>%
  dplyr::filter(is.na(std.dev)) %>%
  dplyr::select(variable, cohort) %>%
  arrange(cohort)

avail_out <- mh_avail$continuous %>%
  dplyr::filter(!is.na(std.dev)) %>%
  dplyr::select(variable, cohort) %>%
  arrange(cohort)

missing_out %>% print(n = Inf)

write_csv(missing_exp, file = here("tables", "miss_exp_24_08_22.csv"))
write_csv(missing_out, file = here("tables", "miss_out_24_08_22.csv"))

missing_exp %>% print(n = Inf)

ds.colnames("mh_rep")

missing_out %>%
  print(n = Inf)


datashield.workspace_save(conns, "exp-mh")
################################################################################
# 5. Check filled data
################################################################################
fill_ref <- df_valid %>%
  mutate(df_name = paste0(df_name, "_fill"))

filled <- fill_ref %>%
  pmap(function(df_name, vars){
    
    dh.classDiscrepancy(
      df = df_name,
      vars = unlist(vars))
  }) %>%
  set_names(sort(df_valid$df_name))

filled$non_rep %>% dplyr::filter(discrepancy == "yes")
filled$year_rep %>% dplyr::filter(discrepancy == "yes")
filled$mh_rep %>% dplyr::filter(discrepancy == "yes")

save.image()

################################################################################
# 6. Subset to include only those with urban environment data  
################################################################################
conns <- datashield.login(logindata, restore = "exp-mh")

## ---- Create dataframe with urb_id info --------------------------------------
dh.dropCols(
  df = "non_rep", 
  vars = c("child_id", "urb_area_id"), 
  type = "keep",
  new_obj = "area_id", 
  checks = F)

## ---- Merge this into datasets where it doesn't exist ------------------------
ds.merge(
  x.name = "year_rep_fill", 
  y.name = "area_id", 
  by.x.names = "child_id", 
  by.y.names = "child_id", 
  all.x = TRUE, 
  newobj = "year_rep_urb")

ds.merge(
  x.name = "mh_rep_fill", 
  y.name = "area_id", 
  by.x.names = "child_id", 
  by.y.names = "child_id", 
  all.x = TRUE, 
  newobj = "mh_rep_urb")

## ---- Do the business --------------------------------------------------------
urb_sub <- tibble(
  df = c("non_rep_fill", "year_rep_urb", "mh_rep_urb"), 
  new_obj = c("non_rep_sub", "year_rep_sub", "mh_rep_sub"))

urb_sub %>%
  pmap(function(df, new_obj){
    
    dh.defineCases(
      df = df, 
      vars = "urb_area_id", 
      type = "any", 
      new_obj = new_obj)
    
    ds.dataFrameSubset(
      df.name = df, 
      V1.name = new_obj, 
      V2.name = "1", 
      Boolean.operator = "==", 
      keep.NAs = FALSE, 
      newobj = new_obj)
    
  })

datashield.workspace_save(conns, "exp-mh")
save.image()

################################################################################
# 7. Create sub-cohorts for inma and eden  
################################################################################
cohortSubset <- function(df){
  
  ref <- tibble(
    cohort = c("inma_gip", "inma_sab", "inma_val", "eden_nan", "eden_poit"),
    value = c("1102", "1103", "1104", "1801", "1802")) 
  
  ref_actual <- ref %>%
    dplyr::filter(cohort %in% names(conns))
  
  ref_actual %>%
    pmap(function(cohort, value){
      
      ds.dataFrameSubset(
        df.name = df, 
        V1.name = paste0(df, "$urb_area_id"),
        V2.name = value,
        Boolean.operator = "==",
        newobj = df, 
        datasources = conns[cohort])
      
    })
  
}

cohortSubset("non_rep_sub")
cohortSubset("year_rep_sub")
cohortSubset("mh_rep_sub")

ds.summary("non_rep_sub")
ds.dim("non_rep_sub")

datashield.workspace_save(conns, "exp-mh")

