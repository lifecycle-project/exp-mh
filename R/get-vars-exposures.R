################################################################################
## Project: Exposome and Mental Health    
## Script purpose: Prepare exposures 
## Date: 13th April 2022
## Author: Tim Cadman
## Email: tica@sund.ku.dk
################################################################################

withr::with_libpaths(new = "~/R/userlib",
                     devtools::install_github("lifecycle-project/ds-helper",
                                              ref = "new-function"))


conns <- datashield.login(logindata, restore = "exp-mh")
################################################################################
# 2. Create IQR versions of variables  
################################################################################

## ---- Reference table --------------------------------------------------------
cont_exp.vars <- inner_join(
  all_vars %>% dplyr::filter(type == "exposure"),
  filled$non_rep %>% dplyr::filter(alspac %in% c("integer", "numeric")), 
  by = "variable") %>%
  pull(variable)

## ---- Do the business --------------------------------------------------------
dh.makeIQR(
  df = "non_rep_sub", 
  vars = cont_exp.vars, 
  type = "split", 
  new_obj = "iqr_split")

dh.makeIQR(
  df = "non_rep_sub", 
  vars = cont_exp.vars, 
  type = "combine", 
  new_obj = "iqr_combine")

## ---- Remove variables we don't need -----------------------------------------
getTransVarnames <- function(df, suffix){
  
  out <- ds.colnames(df) %>% map(~str_subset(., suffix)) %>%
    map(~c(., "child_id"))
  
}

split_names <- getTransVarnames("iqr_split", "iqr_s")
  
split_names %>% 
  imap(
    ~dh.dropCols(
      df = "iqr_split", 
      vars = .x, 
      type = "keep",
      conns = conns[.y],
      checks = F))

combine_names <- getTransVarnames("iqr_combine", "iqr_c")
  
combine_names %>% 
  imap(
    ~dh.dropCols(
      df = "iqr_combine", 
      vars = .x, 
      type = "keep",
      conns = conns[.y],
      checks = F))

datashield.workspace_save(conns, "exp-mh")
################################################################################
# 4. Create quartiles of continuous variables
################################################################################
#cont_exp.vars %>%
#  map(
#    ~dh.quartileSplit(
#      df = "non_rep",
#      var = .x, 
#      type = "split",
#      band_action = "ge_l",
#      var_suffix = "_q_c", 
#      new_obj = "quartiles"
#    ))

#quart_vars <- getTransVarnames("quartiles", "_q_c")

#datashield.workspace_save(conns, "exp-mh")





