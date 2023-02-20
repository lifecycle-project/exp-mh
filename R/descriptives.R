################################################################################
## Project: Exposome and Mental Health    
## Script purpose: Descriptive statistics
## Date: 27.04.22
## Author: Tim Cadman
## Email: tica@sund.ku.dk
################################################################################

conns <- datashield.login(logindata, restore = "exp-mh")

ds.colnames("non_rep")

library(see)

source("https://raw.githubusercontent.com/timcadman/useful-code-r/master/code/themes/themes.R")
source("https://raw.githubusercontent.com/timcadman/useful-code-r/master/code/themes/lc-names-neat.R")
################################################################################
# 1. Define variables  
################################################################################
exp.vars <- all_vars %>%
  dplyr::filter(type == "exposure") %>%
  pull(variable)

exp_green.vars <- str_subset(
  exp.vars, paste(c("green","natgr","ndvi","agrgr","hdres","indtr",
                    "ldres","other","ubgr"),collapse="|"))

exp_pol.vars <- str_subset(
  exp.vars, paste(c("pm10","pm25","pmcoarse","pm25abs","no2","nox","op_dtt",
                    "op_esr","oc","pah", "pm25cu","pm25fe","pm25si","pm25zn"), 
                  collapse = "|"))

exp_land.vars <- str_subset(
  exp.vars, paste(c("agrgr","hdres","indtr","ldres","other","ubgr","bdens","fdens",
                    "frich","foodenv","vldres","airpt"),collapse="|"))

exp_trans.vars <- str_subset(
  exp.vars, paste(c("bus_","connind","trans","walkab"),collapse="|"))

exp_blue.vars <- str_subset(
  exp.vars, paste(c("blue","water","port"),collapse="|"))

exp_noise.vars <- str_subset(exp.vars, paste(c("ln","lden"),collapse="|"))

exp_build.vars <- str_subset(exp.vars, "bdens")

exp_area.vars <- str_subset(exp.vars, "area")

transformed.vars <- tibble(
  variable = c("parity", "birth_month_f", "mat_age_f", "preterm", "edu_m_.0_1",
               "areases_tert_.0_1", "areases_quint_.0_1"), 
  df_name = "exp_mh_wide", 
  type = c(rep("covariate", 5), rep("exposure", 2)), 
  short_name = variable, 
  full_name = c("Parity", "Birth month", "Maternal age at birth", 
                "Preterm birth", "Maternal education at birth", 
                "Area SES at birth (tertiles)", "Area SES at birth (quintiles)")
)

full.vars <- bind_rows(all_vars, transformed.vars)

out.vars <- c("int_raw_", "ext_raw_", "asd_raw_", "adhd_raw_", "nvi_raw_", 
              "lan_raw_", "wm_raw_", "fm_raw_", "gm_raw_")

cov_exc.vars <- c("areases_quint_preg", "areases_tert_preg", "birth_month", 
                  "birth_year", "breastfed_any", "child_id", "child_no", 
                  "coh_country", "cohort_id", "ga_us", "mother_id", 
                  "preg_alc_unit", "preg_no", "urb_area_id", "child_id", "edu_m", 
                  "age_years", "areases_quint_", "areases_tert")

cov.vars <- c("agebirth_m_y", "breastfed_ever", "con_anomalies", "ethn3_m",
              "eusilc_income_quintiles", "ga_bj", "outcome", "parity_m",
              "ppd", "preg_alc", "preg_cig", "preg_dia", "preg_ht", 
              "preg_psych", "preg_smk", "prepreg_anx", "prepreg_dep", 
              "prepreg_psych", "sex", "edu_m_", "areases_tert_", "parity",
              "birth_month_f", "mat_age_f", "preterm", "edu_m_.0_1",
              "areases_tert_.0_1", "areases_quint_.0_1") 

################################################################################
# 2. Extract descriptives  
################################################################################
exp.desc <- dh.getStats(
  df = "non_rep_sub", 
  vars = exp.vars)

area.desc_sub <- dh.getStats(
  df = "non_rep_sub", 
  vars = c("areases_quint_preg","areases_tert_preg"))

area.desc_full <- dh.getStats(
  df = "non_rep", 
  vars = c("areases_quint_preg","areases_tert_preg"))

area.desc_full$categorical %>% dplyr::filter(cohort == "dnbc")
area.desc$categorical %>% dplyr::filter(cohort == "dnbc")

ds.colnames("non_rep_sub")

out.desc <- dh.getStats(
  df = "exp_mh_long",
  vars = out.vars)

cov.desc <- dh.getStats(
  df = "exp_mh_long", 
  vars = cov.vars)

save.image()

exp.desc$categorical %>%
  print(n = Inf)
  
  dplyr::filter(cohort == "dnbc")

################################################################################
# 3. Helper functions  
################################################################################
vars <- cov.vars

makeContTable <- function(vars, stats = exp.desc){ 
  
  coh_names <- unique(stats$continuous$cohort)
  
  ref <- all_vars %>%
    dplyr::filter(variable %in% vars) %>%
    dplyr::select(full_name)
  
  stats <- stats$continuous %>%
    dplyr::filter(variable %in% vars) %>%
    dplyr::select(variable, cohort, mean, std.dev, valid_n, missing_perc) %>%
    left_join(., full.vars, by = "variable") %>%
    mutate(across(mean:std.dev, ~round(., 1))) %>%
    mutate(
      mean_sd = paste0(mean, " (", std.dev, ")"), 
      n_perc = paste0(valid_n, " (", 100-missing_perc, ")")) %>%
    dplyr::select(full_name, cohort, mean_sd, n_perc) %>%
    pivot_wider(
      names_from = "cohort", 
      values_from = c(mean_sd, n_perc)) %>%
    dplyr::select(full_name, mean_sd_combined, paste0("mean_sd_", coh_names))
                  
  out <- left_join(ref, stats, by = "full_name")
  
  return(out)
  
}

makeCatTable <- function(vars, stats = exp.desc){
  
  stats$categorical %>%
    dplyr::select(variable, cohort, category, value, perc_total) %>%
    mutate(across(perc_total, ~round(., 1))) %>%
    mutate(
      n_perc = paste0(value, " (", perc_total, ")")) %>%
    dplyr::select(variable, cohort, category, n_perc) %>%
    pivot_wider(
      names_from = "cohort", 
      values_from = n_perc) %>%
    dplyr::select(variable, category, combined, coh_names) 
}

################################################################################
# 5. Get within-group correlations  
################################################################################
ds.dataFrameFill("non_rep_fill", "non_rep_fill")
datashield.workspace_save(conns, "exp-mh")

ds.colnames("non_rep_sub")

getCor <- function(vars, conns){
  
  dh.dropCols(
    df = "non_rep_sub", 
    vars = vars, 
    type = "keep", 
    checks = F,
    new_obj = "cor_df", 
    conns = conns)
  
  out <- ds.cor(
    x = "cor_df", 
    type = "split", 
    naAction = "pairwise.complete", 
    datasources = conns)
  
}

exp_cor.vars <- unique(c(
  exp_green.vars, "lden_preg", "ln_preg", exp_build.vars, exp_area.vars, 
  exp_trans.vars[c(1:3, 5:10)], exp_land.vars[c(1, 3:12)]))

exp_cor <- getCor(
  vars = exp_cor.vars,
  conns = conns)

green.cor <- getCor(exp_green.vars, conns = conns)
#blue.cor <- getCor(exp_blue.vars)
noise.cor <- getCor(c("lden_preg", "ln_preg"), conns = conns)
build.cor <- getCor(exp_build.vars, conns = conns)
area.cor <- getCor(exp_area.vars, conns = conns)
land.cor <- getCor(exp_land.vars[c(1, 3:12)], conns = conns)
trans.cor <- getCor(exp_trans.vars[c(1:3, 5:10)], conns = conns)

################################################################################
# 5. Output
################################################################################

## ---- Green  -----------------------------------------------------------------
green.tab <- makeContTable(exp_green.vars)
write_csv(green.tab, file = here("tables", "green_desc.csv"))

## ---- Blue -------------------------------------------------------------------
blue.tab <- makeContTable(exp_blue.vars)
write_csv(blue.tab, file = here("tables", "blue_desc.csv"))

## ---- Pollution --------------------------------------------------------------
pol.tab <- makeContTable(exp_pol.vars)
write_csv(pol.tab, file = here("tables", "pol_desc.csv"))

## ---- Noise ------------------------------------------------------------------
noise.tab <- makeContTable(exp_noise.vars)
write_csv(noise.tab, file = here("tables", "noise_desc.csv"))

## ---- Land -------------------------------------------------------------------
land.tab <- makeContTable(exp_land.vars)
write_csv(land.tab, file = here("tables", "land_desc.csv"))

## ---- Build ------------------------------------------------------------------
build.tab <- makeContTable(exp_build.vars)
write_csv(build.tab, file = here("tables", "build_desc.csv"))

## ---- Transport --------------------------------------------------------------
trans.tab <- makeContTable(exp_trans.vars)
write_csv(trans.tab, file = here("tables", "trans_desc.csv"))

## ---- Area -------------------------------------------------------------------
area.tab <- makeCatTable(exp_area.vars)
write_csv(area.tab, file = here("tables", "area_desc.csv"))

## ---- Outcome ----------------------------------------------------------------
out.tab <- out.desc$continuous %>%
  dplyr::select(variable, cohort, valid_n) %>%
  pivot_wider(
    names_from = cohort,
    values_from = valid_n) %>%
  dplyr::select(variable, combined, everything()) %>%
  mutate(variable = factor(variable, 
         labels = out.vars[!out.vars == "asd_raw_"], 
         ordered = TRUE)) 

write_csv(out.tab, file = here("tables", "outcome.csv"))

ds.scatterPlot("exp_mh_long$ext_age_", "exp_mh_long$ext_raw_", 
               datasources = conns[!names(conns) %in% c("genr", "ninfea")])

ds.scatterPlot("exp_mh_long$int_age_", "exp_mh_long$int_raw_", 
               datasources = conns[!names(conns) %in% c("genr", "ninfea")])

ds.scatterPlot("exp_mh_long$adhd_age_", "exp_mh_long$adhd_raw_", 
               datasources = conns[!names(conns) %in% c("genr", "ninfea")])

ds.scatterPlot("exp_mh_long$nvi_age_", "exp_mh_long$nvi_raw_", 
               datasources = conns[!names(conns) %in% c(
                "inma_gip", "inma_sab", "inma_val", "moba", "ninfea")])

ds.scatterPlot("exp_mh_long$lan_age_", "exp_mh_long$lan_raw_", 
               datasources = conns[!names(conns) %in% c(
                 "inma_gip", "inma_sab", "inma_val")])

ds.scatterPlot("exp_mh_long$wm_age_", "exp_mh_long$wm_raw_", 
               datasources = conns[!names(conns) %in% c(
                 "genr", "inma_gip", "inma_sab", "inma_val", "moba", "ninfea")])

ds.scatterPlot("exp_mh_long$fm_age_", "exp_mh_long$fm_raw_", 
               datasources = conns[!names(conns) %in% c(
                 "inma_gip", "inma_sab", "inma_val")])

ds.scatterPlot("exp_mh_long$gm_age_", "exp_mh_long$gm_raw_", 
               datasources = conns[!names(conns) %in% c(
                 "inma_gip", "inma_sab", "inma_val")])
               
## ---- Covariates -------------------------------------------------------------
cov_cont.tab <- makeContTable(vars = cov.vars, stats = cov.desc) %>%
  dplyr::filter(!is.na(mean_sd_combined))
write_csv(cov_cont.tab, file = here("tables", "cov_cont.csv"))

cov_cat.tab <- makeCatTable(vars = cov.vars, stats = cov.desc) 
write_csv(cov_cat.tab, file = here("tables", "cov_cat.csv"))

################################################################################
# Correlations  
################################################################################
exp_cor_clean <- exp_cor %>%
  map(function(x){
    
    out <- x[["Correlation Matrix"]] %>%
      as_tibble %>%
      set_names(dimnames(x[[4]])[[1]])
  }) %>%
  set_names(names(conns))

## ---- Make the plots --------------------------------------------------------
cor.plots <- exp_cor_clean %>%
  map(
    ~ggcorr(
      data = NULL, 
      cor_matrix = ., 
      label = TRUE, 
      label_round = 2, 
      hjust = 1, 
      layout.exp = 1, 
      label_size = 3, 
      low = palette_ext[2], 
      mid = "white", 
      high = palette_std[5], 
      legend.size = 10) +
      theme_cor + 
      theme(legend.position = "none"))

cor.plots %>%
  imap(
    ~ggsave(
      plot = .x,
      h = word_half, 
      width = word_full,
      filename = here("figures", paste0("cor_", .y, ".png")),
      dpi=1200, type="cairo"))

################################################################################
# Summarise correlations >.9  
################################################################################
highCor <- function(x, type = c("high", "low", "both")){
  
  if(type == "high"){
    
    tmp <- x %>%
      map(~.> 0.9)
    
  } else if(type == "low"){
    
    tmp <- x %>%
      map(~.< -0.9)
  
  } else if(type == "both"){
    
  tmp <- x %>%
    map(~.> 0.9 | .< -0.9)
  
  }
  
  cor_names <- colnames(x)
  
  with_dups <- tmp %>%
    map(~cor_names[.]) %>%
    map(~.[!is.na(.)]) %>%
    map(as.tibble) %>%
    bind_rows(.id = "var_1") %>%
    dplyr::rename(var_2 = value) %>%
    dplyr::filter(var_1!=var_2)
  
  if(nrow(with_dups) == 0){
    
    return(with_dups)
    
  } else if (nrow(with_dups) >0){
    
  out <- with_dups[!duplicated(t(apply(with_dups, 1, sort))), ]
  
  }
  
  return(out)
  
}

x <- exp_cor_clean[[2]]
type <- "low"

high_cor.tab <- exp_cor_clean[1:10] %>%
  map(highCor, type = "high") %>%
  bind_rows(.id = "cohort")

low_cor.tab <- exp_cor_clean[1:10] %>%
  map(highCor, type = "low") %>%
  bind_rows(.id = "cohort")

write_csv(high_cor.tab, file = here("tables", "high_cor.csv"))
write_csv(low_cor.tab, file = here("tables", "low_cor.csv"))

save.image()

################################################################################
# Tables for categorical variables to assess small cell count  
################################################################################
conns <- datashield.login(logindata, restore = "exp-mh")

ds.dataFrameFill("non_rep_sub", "non_rep_sub")

fixFactor(
  df = "non_rep_sub", 
  vars = c("areases_quint_preg", "areases_tert_preg", "greenyn300_preg"))

datashield.workspace_save(conns, "exp-mh")

core_var_stats <- dh.getStats(
  df = "non_rep_sub", 
  vars = exp_cor.vars)

check_cell <- core_var_stats$categorical %>%
  dplyr::filter(cohort != "combined" & !is.na(category)) %>%
  dplyr::select(variable, cohort, category, value)

write_csv(check_cell, file = here("tables", "check_cell.csv"))

################################################################################
# Half violin plots: prepare data
################################################################################
conns <- datashield.login(logindata, restore = "exp-mh")

violin_ref <- core_var_stats$continuous %>%
  dplyr::filter(cohort_n != missing_n & cohort != "combined") %>%
  dplyr::select(variable, cohort) %>%
  dplyr::filter(!(variable == "vldres_preg" & cohort == "dnbc")) %>%
  group_by(variable) %>%
  group_split %>%
  map(
    ~mutate(., cohort = list(.x$cohort)) %>%
      slice(1)) %>%
  bind_rows
  
violin.data <- violin_ref %>%
  pmap(function(variable, cohort){
    
    dh.getAnonPlotData(
      df = "non_rep_sub", 
      var_1 = variable, 
      checks = F, 
      conns = conns[unlist(cohort)])
    
  })

violin_clean.data <- violin.data %>%
  map(
    ~pivot_longer(., 
    cols = -cohort,
    names_to = "variable", 
    values_to = "value")) %>%
  bind_rows %>%
  dplyr::filter(cohort != "combined")

################################################################################
# Function  
################################################################################
hVPlot <- function(data = violin.plotData, y_label = NULL, limits = NULL, 
                   title = var){
  
  if(is.null(limits)){
    y_scale <- coord_flip(     
      expand = FALSE,
      clip = "on")
    
  } else if(!is.null(limits)){
    
    y_scale <- coord_flip(     
      ylim = limits,
      expand = FALSE,
      clip = "on")
    
  }
  
  data %>%
    ggplot(aes(x = reorder(cohort, desc(cohort)), y = value)) +
    geom_violinhalf(aes(fill = cohort, alpha = 0.75)) + 
    geom_boxplot(width=0.15, outlier.size = 0.3) +
    y_scale + 
    ylab(y_label) +
    theme_std + 
    scale_fill_manual(values = c(palette_ext, "black")) +  
    theme_hv +
    ggtitle(title) + 
    theme(
      axis.title.y = element_blank(), 
      plot.title = element_text(size = 28), 
      text = element_text(size = 28), 
      axis.title.x = element_text(size = 28), 
      axis.text.x = element_text(size = 28), 
      axis.text.y = element_text(size = 28))
  
}

################################################################################
# Make plots  
################################################################################
violin_plots <- violin_clean.data %>%
  group_by(variable) %>%
  group_split %>%
  map(~hVPlot(data = .x, title = .x$variable[1]))

violin_plots %>%
  map(
    ~ggsave(
      plot = .x,
      h = word_full, 
      width = word_land,
      filename = here("figures", paste0("violin_", .x$labels$title, ".png")),
      dpi=300, type="cairo"))

save.image()

## ---- Redo plot for green distances ------------------------------------------
green_size.plot <- violin_clean.data %>%
  dplyr::filter(variable == "green_size_preg") %>%
  hVPlot(y_label = NULL, limits = c(0, 500000), title = "green_size_preg")

ggsave(
  plot = green_size.plot,
  h = word_full, 
  width = word_land,
  filename = here("figures", "green_size_violin.png"),
  dpi=300, type="cairo")
  
unique(violin_clean.data$variable)


################################################################################
# >30% missing  
################################################################################
cont_miss_30 <- core_var_stats$continuous %>%
  dplyr::filter(between(missing_perc, 30, 99) & cohort != "combined") %>%
  dplyr::select(variable, cohort)

cat_miss_30 <- core_var_stats$categorical %>%
  dplyr::filter(between(perc_missing, 30, 99) & cohort != "combined") %>%
  dplyr::select(variable, cohort)

missing_30 <- bind_rows(cont_miss_30, cat_miss_30)

write_csv(missing_30, file = here("tables", "missing_30.csv"))

################################################################################
# >50% missing  
################################################################################
cont_miss_50 <- core_var_stats$continuous %>%
  dplyr::filter(between(missing_perc, 50, 99) & cohort != "combined") %>%
  dplyr::select(variable, cohort)

cat_miss_50 <- core_var_stats$categorical %>%
  dplyr::filter(between(perc_missing, 50, 99) & cohort != "combined") %>%
  dplyr::select(variable, cohort)

missing_50 <- bind_rows(cont_miss_50, cat_miss_50)

write_csv(missing_30, file = here("tables", "missing_30.csv"))

################################################################################
# Outcome  
################################################################################

objs <- ds.ls()

getCoh <- function(outcome){
  
  tmp <- map(objs, function(x){any(x[[2]] %in% outcome)})
      
      names(conns)[unlist(tmp)]
  
}

lazyWrap <- function(sub, var, title, limits){
  
data <- dh.getAnonPlotData(
    df = sub, 
    var_1 = var, 
    checks = F, 
    conns = conns[getCoh(sub)])

clean <- data %>% pivot_longer(
  cols = var,
  names_to = "variable", 
  values_to = "value") %>%
  dplyr::filter(cohort != "combined")

plot <- hVPlot(data = clean, title = title, limits = limits)

return(plot)

}
    
  
  
## ---- Plots ------------------------------------------------------------------
ext_cbcl.vplot <- lazyWrap("cbcl_sub_old", "ext_raw_.0_17", "Externalising CBCL")
ext_sdq.vplot <- lazyWrap("sdq_sub_old", "ext_raw_.0_17", "Externalising SDQ")
int_cbcl.vplot <- lazyWrap("cbcl_sub_old", "int_raw_.0_17", "Internalising CBCL")
int_sdq.vplot <- lazyWrap("sdq_sub_old", "int_raw_.0_17", "Internalising SDQ")
adhd_sdq.vplot <- lazyWrap("sdq_sub_old", "adhd_raw_.0_17", "ADHD SDQ")
adhd_cprs.vplot <- lazyWrap("cprs_sub_old", "adhd_raw_.0_17", "ADHD CPRS")
adhd_disc.vplot <- lazyWrap("disc_sub_old", "adhd_raw_.0_17", "ADHD CPRS")

wm_nback.vplot <- lazyWrap("nback_sub_old", "wm_raw_.0_17", "Working Memory NBACK", limits = c(-3, 5))
wm_cst.vplot <- lazyWrap("cst_sub_old", "wm_raw_.0_17", "Working Memory CST", limits = c(0, 6))

list(
  ext_cbcl.vplot, ext_sdq.vplot, int_cbcl.vplot, int_sdq.vplot, adhd_sdq.vplot,
  adhd_cprs.vplot, adhd_disc.vplot, wm_nback.vplot, wm_cst.vplot) %>%
  map(
    ~ggsave(
      plot = .x,
      h = word_full, 
      width = word_land,
      filename = here("figures", paste0("violin_", .x$labels$title, ".png")),
      dpi=300, type="cairo"))



## ---- Mean ages --------------------------------------------------------------
sdq_stats <- dh.getStats(
  df = "cbcl_sub_old", 
  vars = "ext_age_.0_17", 
  conns = conns[cbcl_coh])

sdq_age <- sdq_stats$continuous %>% 
  dplyr::select(cohort, mean, std.dev) %>%
  mutate(var = "sdq")

cbcl_stats <- dh.getStats(
  df = "sdq_sub_old", 
  vars = "ext_age_.0_17", 
  conns = conns[sdq_coh])

cbcl_age <- cbcl_stats$continuous %>% 
  dplyr::select(cohort, mean, std.dev) %>%
  mutate(var = "cbcl")

cprs_stats <- dh.getStats(
  df = "cprs_sub_old", 
  vars = "adhd_age_.0_17", 
  conns = conns[cprs_coh]) 

cprs_age <- cprs_stats$continuous %>% 
  dplyr::select(cohort, mean, std.dev) %>%
  mutate(var = "cbrs")

disc_stats <- dh.getStats(
  df = "disc_sub_old", 
  vars = "adhd_age_.0_17", 
  conns = conns[disc_coh])

disc_age <- disc_stats$continuous %>% 
  dplyr::select(cohort, mean, std.dev) %>%
  mutate(var = "disc")

nback_stats <- dh.getStats(
  df = "nback_sub_old", 
  vars = "wm_age_.0_17", 
  conns = conns[nback_coh])

nback_age <- nback_stats$continuous %>% 
  dplyr::select(cohort, mean, std.dev) %>%
  mutate(var = "nback")

cst_stats <- dh.getStats(
  df = "cst_sub_old", 
  vars = "wm_age_.0_17", 
  conns = conns[cst_coh])

cst_age <- cst_stats$continuous %>% 
  dplyr::select(cohort, mean, std.dev) %>%
  mutate(var = "cst")

ages <- bind_rows(sdq_age, cbcl_age, cprs_age, disc_age, nback_age, 
                  cst_age)

write_csv(ages, here("tables", "outcome_ages.csv"))







ext.data <- dh.getAnonPlotData(
  df = "ext_sub", 
  var_1 = "ext_raw_", 
  checks = F, 
  conns = conns[getCoh("ext_sub")])

ext.data <- ext.data %>% pivot_longer(
  cols = "ext_raw_",
  names_to = "variable", 
  values_to = "value") %>%
  dplyr::filter(cohort != "combined")

ext.vplot <- hVPlot(data = ext.data, title = "Externalising")

ggsave(
  plot = ext.vplot,
  h = word_full, 
  width = word_land,
  filename = here("figures", "ext_violin.png"),
  dpi=300, type="cairo")

## ---- Internalising ----------------------------------------------------------
int.data <- dh.getAnonPlotData(
  df = "int_sub", 
  var_1 = "int_raw_", 
  checks = F, 
  conns = conns[getCoh("int_sub")])

int.data <- int.data %>% pivot_longer(
  cols = "int_raw_",
  names_to = "variable", 
  values_to = "value") %>%
  dplyr::filter(cohort != "combined")

ext.vplot <- hVPlot(data = ext.data, title = "Externalising")

ggsave(
  plot = ext.vplot,
  h = word_full, 
  width = word_land,
  filename = here("figures", "ext_violin.png"),
  dpi=300, type="cairo")


  })


\
