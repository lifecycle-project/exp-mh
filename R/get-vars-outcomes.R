################################################################################
## Project: Exposome and Mental Health    
## Script purpose: Prepare outcomes 
## Date: 13th April 2022
## Author: Tim Cadman
## Email: tica@sund.ku.dk
################################################################################

conns <- datashield.login(logindata, restore = "exp-mh")
################################################################################
# Create transformations of age terms  
################################################################################
age_vars <- c("ext_age_", "int_age_", "adhd_age_",  "lan_age_", 
             "nvi_age_", "wm_age_", "fm_age_", "gm_age_")

#"asd_age_"

age_avail <- dh.getStats(
  df = "mh_rep", 
  vars = age_vars)

age_avail <- age_avail$continuous %>%
  dplyr::filter(!is.na(perc_50) & cohort != "combined")

## ---- Add a small amount to the age term -------------------------------------
age_avail %>%
  pmap(function(variable, cohort, ...){

    ds.assign(
      toAssign = paste0("mh_rep$", variable, "+0.01"), 
      newobj = variable, 
      datasources = conns[cohort])
  })

dh.dropCols(
  df = "mh_rep", 
  vars = age_vars, 
  type = "remove", 
  new_obj = "mh_rep", 
  checks = F)

age_conns <- age_avail %>%
  group_by(cohort) %>%
  group_split %>%
  map(., ~pull(., variable)) %>%
  set_names(names(conns))
  
age_conns %>%
  imap(
    ~ds.dataFrame(
       x = c("mh_rep", .x), 
       newobj = "mh_rep", 
       datasources = conns[.y]))

ds.dataFrameFill("mh_rep", "mh_rep")

## ---- Do the business --------------------------------------------------------
age_vars %>%
  map(
    ~dh.makeAgePolys(
      df = "mh_rep", 
      age_var = .x))

datashield.workspace_save(conns, "exp-mh")
