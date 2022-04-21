################################################################################
## Project: Exposome and Mental Health    
## Script purpose: Assign variables and check 
## Date: 13th April 2022
## Author: Tim Cadman
## Email: tica@sund.ku.dk
################################################################################

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

## ---- All cohorts with initial access ----------------------------------------
cohort_tables %>%
  dplyr::filter(cohort %in% c("alspac", "dnbc", "moba", "ninfea", "inma_gip", 
                              "inma_sab", "inma_val")) %>%
  pwalk(function(cohort, table, df_name, vars){
    
    datashield.assign(
      conns = conns[cohort], 
      symbol = df_name, 
      value = table, 
      variables = unlist(vars))
  })

## ---- BiB --------------------------------------------------------------------
cohort_tables %>%
  dplyr::filter(cohort == "genr" & df_name == "mh_rep") %>%
  dplyr::filter(df_name == "mh_rep") %>%
  pwalk(function(cohort, table, df_name, vars){
    
    datashield.assign(
      conns = conns[cohort], 
      symbol = df_name, 
      value = table, 
      variables = unlist(vars))
  })

datashield.assign(
  conns = conns["genr"], 
  symbol = "non_rep", 
  value = "lc_genr_core_2_2.2_2_core_non_rep_MG_ACB _ECCNLC202049")

datashield.workspace_save(conns, "exp-mh")
################################################################################
# 3. Check data
################################################################################
available <- vars_collapse %>%
  pmap(function(df_name, vars){
    
    dh.classDiscrepancy(
      df = df_name,
      vars = unlist(vars))
  }) %>%
  set_names(sort(vars_collapse$df_name))

available$non_rep
available$year_rep
available$mh_rep

save.image()

################################################################################
# 4. Fill missing variables  
################################################################################
ds.dataFrameFill("non_rep", "non_rep")
ds.dataFrameFill("year_rep", "year_rep")
ds.dataFrameFill("mh_rep", "mh_rep")

datashield.workspace_save(conns, "exp-mh")
################################################################################
# 5. Check filled data
################################################################################
filled <- vars_collapse %>%
  pmap(function(df_name, vars){
    
    dh.classDiscrepancy(
      df = df_name,
      vars = unlist(vars))
  }) %>%
  set_names(sort(vars_collapse$df_name))

filled$non_rep %>% dplyr::filter(discrepancy == "yes")
filled$year_rep %>% dplyr::filter(discrepancy == "yes")
filled$mh_rep %>% dplyr::filter(discrepancy == "yes")

save.image()

################################################################################
# 6. Subset to include only those with urban environment data  
################################################################################

## ---- Create dataframe with urb_id info --------------------------------------
dh.dropCols(
  df = "non_rep", 
  vars = c("child_id", "urb_area_id"), 
  type = "keep",
  new_obj = "area_id")

## ---- Merge this into datasets where it doesn't exist ------------------------
ds.merge(
  x.name = "year_rep", 
  y.name = "area_id", 
  by.x.names = "child_id", 
  by.y.names = "child_id", 
  all.x = TRUE, 
  newobj = "year_rep")

ds.merge(
  x.name = "mh_rep", 
  y.name = "area_id", 
  by.x.names = "child_id", 
  by.y.names = "child_id", 
  all.x = TRUE, 
  newobj = "mh_rep")

## ---- Do the business --------------------------------------------------------
urb_sub <- tibble(
  df = c("non_rep", "year_rep", "mh_rep"), 
  new_obj = paste0(df, "_urb_val"))

urb_sub %>%
  dplyr::filter(df == "year_rep") %>%
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
      newobj = df)
    
  })

datashield.workspace_save(conns, "exp-mh")
save.image()

################################################################################
# 7. Create sub-cohorts for inma and eden  
################################################################################
cohortSubset <- function(df){
  
  tibble(
    cohort = c("inma_gip", "inma_sab", "inma_val"),
    value = c("1102", "1103", "1104")) %>%
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

cohortSubset("non_rep")
cohortSubset("year_rep")
cohortSubset("mh_rep")

datashield.workspace_save(conns, "exp-mh")

