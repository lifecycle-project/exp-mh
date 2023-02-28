################################################################################
## Project: Exposome menta health
## Script purpose: Tables
## Date: 4th August 2022
## Author: Tim Cadman
## Email: tica@sund.ku.dk
################################################################################
library(GGally)  

source("~/wp6-traj-ineq/code/datashield/bmj/var-reference.R")
cohort.ref <- read_csv("~/cohort_info.csv")

conns <- datashield.login(logindata, restore = "exp-mh")
################################################################################
# METHODS  
################################################################################
################################################################################
# Study population  
################################################################################

## ---- Participating cohorts --------------------------------------------------
cohorts <- names(conns) %>% sort()
length(cohorts)

## ---- N exposures ------------------------------------------------------------
exposure.vars <- ds.colnames("exposures")[[1]]

length(exposure.vars)-1

################################################################################
# Table 1: Included cohorts  
################################################################################
cohort.tab <- dt.cohortTable(
  ref = "~/cohort_info.csv",
  dataset = "analysis_df_w", 
  conns = conns)

cohort.tab <- cohort.tab %>%
  dplyr::select(cohort_neat, country, city_area, cohort_ns, birth_years)

write_csv(cohort.tab, here("tables", "cohort_info.csv"))

################################################################################
# Figure 1: Inclusion criteria and participating cohorts
################################################################################
################################################################################
# Left column
################################################################################

## ---- Baseline sample --------------------------------------------------------
baseline_n <- ds.dim("baseline_df", type = "c")[[1]][[1]]

## ---- Some exposures ---------------------------------------------------------
any_exp_n <- ds.dim("any_exp_df_w", type = "c")[[1]][[1]]

## ---- Some outcomes ----------------------------------------------------------
any_out_n <- ds.dim("any_out_df_w", type = "c")[[1]][[1]]

## ---- All covariates ---------------------------------------------------------
analysis_n <- ds.dim("analysis_df_w", type = "c")[[1]][[1]]

################################################################################
# Right column  
################################################################################

## ---- No exposures -----------------------------------------------------------
baseline_n - any_exp_n

## ---- No outcomes ------------------------------------------------------------
any_exp_n - any_out_n

## ---- No covariates ----------------------------------------------------------
any_out_n - analysis_n

## ---- Complete cases ---------------------------------------------------------

## Get max and min complete cases once done regression

################################################################################
# Summarise exclusions and transformations 
################################################################################

## ---- Included ---------------------------------------------------------------
included.tab <- all_vars %>%
  dplyr::filter(!variable %in% c(trans_sum, exclude.vars))

## ---- Transformed ------------------------------------------------------------
trans_sum %>% print(n = Inf)

## ---- Excluded ---------------------------------------------------------------
excluded.vars <- all_vars %>%
  dplyr::filter(variable %in% exclude.vars)

################################################################################
# EXPOSURES
################################################################################
################################################################################
# Included exposures  
################################################################################

## ---- Built environment ------------------------------------------------------
built.vars <- included.tab %>%
  dplyr::filter(family == "built_env")

#write_csv(built.vars, here("tables", "built_inc.csv"))

## ---- Natural spaces ---------------------------------------------------------
natural.vars <- included.tab  %>%
  dplyr::filter(family == "natural")

#write_csv(natural.vars, here("tables", "nat_inc.csv"))

## ---- Pollution --------------------------------------------------------------
pollution.vars <- included.tab %>%
  dplyr::filter(family == "pollution")

#write_csv(pollution.vars, here("tables", "pol_inc.csv"))

## ---- Noise ------------------------------------------------------------------
noise.vars <- included.tab %>% 
  dplyr::filter(family == "noise")

#write_csv(noise.vars, here("tables", "noise_inc.csv"))

################################################################################
# OUTCOMES  
################################################################################
################################################################################
# Text: instruments used for mental health assessment  
################################################################################
dh.columnCast(
  df = "analysis_df_w", 
  target_vars = "adhd_instr_.0_17", 
  target_class = "factor")

datashield.workspace_save(conns, "exp-mh")

mh_instr.stats <- dh.getStats(
  df = "analysis_df_w", 
  vars = c("int_instr_.0_17", "ext_instr_.0_17", "adhd_instr_.0_17"))

mh_instr_comb.tab <- mh_instr.stats$categorical %>%
  dplyr::filter(!is.na(category) & value > 0 & cohort != "combined") %>%
  dplyr::select(variable, category) %>%
  unique %>%
  left_join(., instr.vals)

################################################################################
# Table S2: Mental Health Measure per cohort  
################################################################################
mh_ages <- dh.getStats(
  df = "analysis_df_w", 
  vars = c("int_age_.0_17", "ext_age_.0_17", "adhd_age_.0_17"))

mh_instr_coh <- mh_instr.stats$categorical %>%
  dplyr::filter(!is.na(category) & value > 0 & cohort != "combined") %>%
  left_join(., instr.vals, by = "category") %>%
  mutate(
    outcome = case_when(
      variable == "int_instr_.0_17" ~ "int",
      variable == "ext_instr_.0_17" ~ "ext",
      variable == "adhd_instr_.0_17" ~ "adhd")) %>%
  mutate(type = "instrument") %>%
  dplyr::rename(tab_value = instrument) %>%
  dplyr::select(cohort, outcome, tab_value, type)

mh_age_coh <- mh_ages$continuous %>%
  mutate(
    outcome = case_when(
      variable == "int_age_.0_17" ~ "int",
      variable == "ext_age_.0_17" ~ "ext",
      variable == "adhd_age_.0_17" ~ "adhd")) %>%
  mutate(tab_value = paste0(mean, " (", std.dev, ")")) %>%
  mutate(type = "age") %>%
  dplyr::select(cohort, outcome, tab_value, type)
  
inst_age_coh.tab <- bind_rows(mh_instr_coh, mh_age_coh) %>%
  mutate(
    outcome_neat = case_when(
    outcome == "int" ~ "Internalising",
    outcome == "ext" ~ "Externalising",
    outcome == "adhd" ~ "ADHD")) %>%
left_join(., cohort.ref %>% dplyr::select(cohort, cohort_neat), by = "cohort") %>%
  dplyr::select(-cohort, -outcome) %>%
  pivot_wider(
    names_from = cohort_neat, 
    values_from = tab_value) %>%
  mutate(outcome_neat = factor(outcome_neat,
    levels = c("Internalising", "Externalising", "ADHD"), ordered = T)) %>%
  arrange(outcome_neat, desc(type)) %>%
  dplyr::select(-type, outcome_neat, sort(tidyselect::peek_vars()))

write_csv(inst_age_coh.tab, here("tables", "mh_instr_age_coh.csv"))

## ---- Average ages -----------------------------------------------------------
inst_age_coh.tab %>%
  dplyr::filter(type == "age") %>%
  dplyr::select(outcome_neat, "NA")


save.image("file_2.RData")


descriptives_all

ds.colnames("analysis_df_w")

################################################################################
# Text: available information on ethnicity  
################################################################################
#eth_coh <- c("abcd", "alspac", "bib", "genr")

#eth_n.stats <- dh.getStats(
#  df = "analysis_df_w", 
#  vars = "eth_bin")

## ---- Cohorts ----------------------------------------------------------------
#eth_coh 

## ---- N cohorts --------------------------------------------------------------
#eth_n_coh <- length(eth_coh)

## ---- Sample size ------------------------------------------------------------
#eth_n.stats$categorical %>%
#  dplyr::filter(cohort %in% eth_coh & category %in% c(1, 2)) %>%
#  pull(value) %>%
#  sum

## ---- Percentage -------------------------------------------------------------
#eth_n.stats$categorical %>%
#  dplyr::filter(cohort == "combined" & category %in% c(1, 2)) %>%
#  pull(perc_total) %>%
#  sum

save.image()


################################################################################
# Table S3: Sample characteristics analysis sample vs excluded
################################################################################
exp_s3.ref <- bind_rows(built.vars, natural.vars, pollution.vars, noise.vars) %>%
  dplyr::select(variable, full_name) %>%
  dplyr::rename(var_label = full_name)

parent_s3.ref <- all_vars %>%
  dplyr::filter(analysis_name %in% c("parity_bin", "agebirth_m_y", "edu_m_.0_1", 
    "area_ses_tert_f")) %>%
  dplyr::select(analysis_name, full_name) %>%
  dplyr::rename(
    variable = analysis_name, 
    var_label = full_name)

child_s3.ref <- all_vars %>%
  dplyr::filter(analysis_name %in% c("birth_month_f", "sex")) %>%
  dplyr::select(analysis_name, full_name) %>%
  dplyr::rename(
    variable = analysis_name, 
    var_label = full_name)

vars_ordered <- bind_rows(exp_s3.ref, parent_s3.ref, child_s3.ref) %>%
  dplyr::filter(!variable %in% c(
  "blueyn300_preg", "greenyn300_preg", "lden_c_preg", "ln_c_preg"))

sample_comp <- list(descriptives_all, exc.desc) %>%
  map(
    ~dh.createTableOne(
      stats = .x, 
      vars = vars_ordered$variable, 
      var_labs = vars_ordered, 
      type = "combined", 
      inc_missing = TRUE, 
      round_digits = 3, 
      perc_denom = "valid", 
      cont_stats = "mean_sd")) %>%
  set_names("included", "excluded") %>%
  map(~dplyr::select(., variable, category, value)) %>%
  bind_rows(.id = "sample") %>%
  pivot_wider(
    names_from = sample, 
    values_from = value)

write_csv(sample_comp, here("tables", "sample-comparison.csv"))

## ---- Ns for header ----------------------------------------------------------
analysis_n
excluded_n <- ds.dim("excluded_df_w", type = "c")[[1]][[1]]

################################################################################
# Table 2: Study sample  
################################################################################
tab_2.ref <- c(exp_s3.ref$variable, parent_s3.ref$variable, child_s3.ref$variable)

tab_2.ref <- tab_2.ref[!tab_2.ref %in% c(
  "blueyn300_preg", "greenyn300_preg", "lden_c_preg", "ln_c_preg")]

study_sample.tab <- dh.createTableOne(
  stats = descriptives_all, 
  vars = tab_2.ref, 
  var_labs = vars_ordered, 
  type = "combined", 
  inc_missing = FALSE, 
  round_digits = 3, 
  perc_denom = "valid", 
  cont_stats = "mean_sd") %>%
  dplyr::filter(category != "missing_n") %>%
  dplyr::select(-category, -cohort)

write_csv(study_sample.tab, here("tables", "study_sample.csv"))

################################################################################
# Table S4: Combined & cohort descriptive statistics  
################################################################################
cohort.labs <- cohort.tab %>%
  mutate(cohort_neat = paste0(cohort_neat, " (n = ", cohort_ns, ")")) %>%
  dplyr::rename(coh_label = cohort_neat) %>%
  mutate(cohort = names(conns)) %>%
  dplyr::select(cohort, coh_label) %>%
  bind_rows(., tibble(
    cohort = "combined", 
  coh_label = paste0("Combined (n = ", analysis_n, ")")))

cohort_stats.tab <- dh.createTableOne(
  stats = descriptives_all, 
  vars = vars_ordered$variable, 
  var_labs = vars_ordered,
  coh_direction = "cols",
  type = "both", 
  inc_missing = TRUE, 
  round_digits = 3, 
  perc_denom = "valid", 
  cont_stats = "mean_sd", 
  coh_labs = cohort.labs) %>%
  mutate(variable = factor(
    variable, levels = vars_ordered$var_label), ordered = T) %>%
  arrange(variable) %>%
  dplyr::select(-old_var)

write_csv(cohort_stats.tab, file = here("tables", "cohort_stats.csv"))

################################################################################
# Compute correlations between exposures  
################################################################################
################################################################################
# Combined  
################################################################################

## ---- Prepare data -----------------------------------------------------------
cor_preg.plotdata <- exp_cor_preg[["Correlation Matrix"]] %>%
  as_tibble %>%
  set_names(exp_cont.vars[exp_cont.vars != "airpt_preg"])

## ---- Make the plots --------------------------------------------------------
cor_comb <- ggcorr(
      data = cor_preg.plotdata, 
      cor_matrix = ., 
      label = TRUE, 
      label_round = 2, 
      hjust = 1, 
      layout.exp = 20, 
      label_size = 2, 
      low = palette_ext[2], 
      mid = "white", 
      high = palette_std[5], 
      legend.size = 10) +
      theme_cor + 
      theme(legend.position = "none") +
  ggplot2::labs(
    title = "Supplementary Figure 2: combined correlations between exposures")

## ---- Save the plots -------------------------------------------------------
ggsave(
  plot = cor_comb,
  filename = here("figures", paste0("exp_cor_comb.png")),
  h = word_full, w = word_land, 
  units="cm", 
  dpi=150, type="cairo")

################################################################################
# Per cohort  
################################################################################

## ---- Prepare data -----------------------------------------------------------
cor_preg_split.plotdata <- exp_cor_preg_split %>%
  set_names(names(conns)) %>%
  map(function(x){
    
    x[["Correlation Matrix"]] %>%
      as_tibble %>%
      set_names(exp_cont.vars[exp_cont.vars != "airpt_preg"])
  })
  

## ---- Make the plots --------------------------------------------------------
cor.plots <- cor_preg_split.plotdata %>%
  imap(
    ~ggcorr(
      data = NULL, 
      cor_matrix = .x, 
      label = TRUE, 
      label_round = 2, 
      hjust = 1, 
      layout.exp = 20, 
      label_size = 2, 
      low = palette_ext[2], 
      mid = "white", 
      high = palette_std[5], 
      legend.size = 10) +
      theme_cor + 
      theme(legend.position = "none") +
      ggplot2::labs(
        title = paste0(
          "Supplementary Figure ", 
          2+match(.y, names(cor_preg_split.plotdata)), 
          ": correlations between exposures for ", 
          .y)))
  

## ---- Save the plots ---------------------------------------------------------
cor.plots %>%
  imap(
    ~ggsave(
      plot = .x,
      filename = here("figures", paste0("cor_", .y, ".png")),
      h = word_full, w = word_land, 
      units="cm", 
      dpi=150, type="cairo"))


ds.ls("all_df")





################################################################################
# MAIN PLOTS  
################################################################################
################################################################################
# Prepare data  
################################################################################
prepMainPlot <- function(mdata, out_name, coh_type){
  
  if(coh_type == "combined"){
    
    mdata <- mdata %>% dplyr::filter(cohort == "combined")
    
  } else if(coh_type == "cohort"){
    
    mdata <- mdata %>% dplyr::filter(cohort != "combined")
    
  }
  
  mdata %>% 
    dplyr::rename(analysis_name = variable) %>%
    dplyr::filter(analysis_name %in% paste0(exp_cont.vars, "_iqr_c")) %>%
    left_join(., all_vars, by = "analysis_name") %>%
    left_join(., cohort.ref, by = "cohort") %>%
    mutate(outcome := out_name) 
}

int.pdata <- prepMainPlot(int.mdata, "INTERNALISING")
ext.pdata <- prepMainPlot(ext.mdata, "EXTERNALSING")
adhd.pdata <- prepMainPlot(adhd.mdata, "ADHD")

mh_exp.pdata <- bind_rows(int.pdata, ext.pdata, adhd.pdata) %>%
  mutate(family = case_when(
    family == "built_env" ~ "Built environment",
    family == "natural" ~ "Natural space", 
    family == "pollution" ~ "Air pollution", 
    family == "noise" ~ "Traffic noise")) %>%
  mutate(family = factor(family, levels = c(
    "Built environment", "Natural space", "Air pollution", "Traffic noise"), 
    ordered = T)) %>%
  arrange(outcome, family, full_name) %>%
  mutate(full_name = factor(full_name, levels = unique(full_name))) 


mh_exp.pdata$family

################################################################################
# Make plots  
################################################################################
.exwasPlot <- function(data, facet, facet_var = "outcome"){
  
  plot <- ggplot(data, aes(
    y = full_name, x = est, xmin = lowci, xmax = uppci, colour = family)) + 
    geom_point() + 
    geom_errorbarh(height = .1) + 
    geom_vline(xintercept = 0, 
               linetype=2, 
               size=.3,
               colour = "black") +
    scale_size_manual(values = c(3, 6)) + 
    scale_x_continuous(limits = c(-0.1, 0.1), breaks = c(-0.1,0.1)) +
    labs(x ="Mean change in score for IQR increase in exposure") +
    coord_flip() + 
    labs(colour = 'Exposure family') +
    theme(
      text = element_text(size = 12,family = "Arial"),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 10),
      axis.text.y = element_text(hjust = 1),
      axis.text.x = element_text(size = 6, hjust = 1, angle = 90),
      axis.ticks = element_line(colour = "black"),
      axis.line = element_line(size = .2, linetype = "solid"),
      panel.background = element_blank(),
      plot.title=element_text(hjust = .5),
      strip.text = element_text(size=10)) + 
    scale_colour_manual(values = palette_std[c(1, 5, 2, 3)])
  
  if(facet == TRUE){
    
    plot <- plot + 
      facet_wrap(~facet_var, ncol = 1)
    
  }
  
  return(plot)
  
}

## ---- Combined ---------------------------------------------------------------
exwas_comb.plot <- .exwasPlot(mh_exp.pdata, T)
      
ggsave(exwas.plot, file = here("figures", "exwas.jpg"), 
       device = "jpeg", dpi = 300, unit = "cm", 
       height = 16, width = 26)

## ---- Internalising only -----------------------------------------------------
int.plot <- .exwasPlot(
  mh_exp.pdata %>% dplyr::filter(outcome == "INTERNALISING"), F)

ggsave(int.plot, file = here("figures", "exwas_int.jpg"), 
       device = "jpeg", dpi = 150, unit = "cm", 
       height = word_full, width = word_land)

## ---- Externalising only -----------------------------------------------------
ext.plot <- .exwasPlot(
  mh_exp.pdata %>% dplyr::filter(outcome == "EXTERNALSING"), F)

ggsave(ext.plot, file = here("figures", "exwas_ext.jpg"), 
       device = "jpeg", dpi = 150, unit = "cm", 
       height = word_full, width = word_land)

## ---- ADHD only --------------------------------------------------------------
adhd.plot <- .exwasPlot(
  mh_exp.pdata %>% dplyr::filter(outcome == "ADHD"), F)

ggsave(adhd.plot, file = here("figures", "exwas_adhd.jpg"), 
       device = "jpeg", dpi = 150, unit = "cm", 
       height = word_full, width = word_land)

################################################################################
# Subset to significant hits  
################################################################################
exp_sig.tab <- mh_exp.pdata %>%
  mutate(sig = ifelse(lowci < 0 & uppci > 0, "no", "yes")) %>%
  dplyr::filter(sig == "yes") %>%
  dplyr::select(outcome, full_name, est, lowci, uppci, metafor_obj)

exp_sig.tab$metafor_obj
################################################################################
# Calculate number of effective tests 
################################################################################
tmp <- exp_cor_preg[["Correlation Matrix"]]

n_e_t <- meff2(tmp, method = "galwey")

0.05 / (n_e_t*3) 

################################################################################
# COHORT-SPECIFIC PLOTS  
################################################################################
################################################################################
# Prepare data  
################################################################################
coh.pdata <- list(
  list(int.mdata, ext.mdata, adhd.mdata), 
  c("INTERNALISING", "EXTERNALSING", "ADHD")) %>%
  pmap(~prepMainPlot(.x, .y, "cohort")) %>%
  set_names(c("int", "ext", "adhd"))

################################################################################
# Make plots  
################################################################################
coh.plots <- coh.pdata %>%
  map(
  ~.x %>%
    group_by(full_name) %>%
    group_split %>%
    map(
      ~ggplot(., aes(
        y = cohort, x = est, xmin = lowci, xmax = uppci)) + 
        geom_point(size = 3) + 
        geom_errorbarh(height = .1) + 
        geom_vline(xintercept = 0, 
                   linetype=2, 
                   size=0.5,
                   colour = "black") +
        scale_size_manual(values = c(3, 6)) + 
        scale_x_continuous(limits = c(-0.2, 0.2), breaks = c(-0.2,0.2), 
                           oob = scales::rescale_none) +
        coord_flip() + 
        facet_wrap(~full_name, nrow = 2, ncol = 2) +
        theme(
          text = element_text(size = 20), 
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 24),
          axis.text.y = element_text(hjust = 1),
          axis.text.x = element_text(size = 20, hjust = 1, angle = 90),
          axis.ticks = element_line(colour = "black"),
          axis.line = element_line(size = .2, linetype = "solid"),
          panel.background = element_blank(),
          plot.title=element_text(hjust = .5),
          strip.text = element_text(size=20))
      )
  )

################################################################################
# Save plots  
################################################################################
coh.plots %>%
  imap(
    ~ggsave(
      filename = here("figures", paste0(.y, "_coh.pdf")), 
      plot = marrangeGrob(grobs = .x, nrow=3, ncol=3),
      device = "pdf", 
      h = word_full-0.2, 
      w = word_land)
    )

################################################################################
# Interpreting significant hits by cohort  
################################################################################
.cohDir <- function(data, var){
  
  data %>% 
    dplyr::filter(exposure == var) %>%
    dplyr::select(cohort, est, lowci, uppci) %>%
    print(n = Inf)
  
}

## ---- Internalising ----------------------------------------------------------
.cohDir(coh.pdata$int, "popdens_preg_iqr_c")

## ---- Externalising ----------------------------------------------------------
.cohDir(coh.pdata$ext, "landuseshan300_preg_iqr_c")
.cohDir(coh.pdata$ext, "urbgr_trans_iqr_c")
.cohDir(coh.pdata$ext, "distinvnear1_preg_iqr_c")
.cohDir(coh.pdata$ext, "walkability_mean_preg_iqr_c")

## ---- Externalising ----------------------------------------------------------
.cohDir(coh.pdata$adhd, "landuseshan300_preg_iqr_c")


################################################################################
# Selection bias plot  
################################################################################

h2
int_s.pdata <- prepMainPlot(
  mdata = int.mdata, 
  out_name = "Complete cases, adjusted", 
  coh_type = "combined")

int_s1.pdata <- prepMainPlot(
  mdata = int_s_1.mdata, 
  out_name = "Full sample, unadjusted", 
  coh_type = "combined")

int_s2.pdata <- prepMainPlot(
  mdata = int_s_2.mdata, 
  out_name = "Complete cases, unadjusted", 
  coh_type = "combined")

ext_s.pdata <- prepMainPlot(
  mdata = ext.mdata, 
  out_name = "Complete cases, adjusted", 
  coh_type = "combined")

ext_s1.pdata <- prepMainPlot(
  mdata = ext_s_1.mdata, 
  out_name = "Full sample, unadjusted", 
  coh_type = "combined")

ext_s2.pdata <- prepMainPlot(
  mdata = ext_s_2.mdata, 
  out_name = "Complete cases, unadjusted", 
  coh_type = "combined")
  
adhd_s1.pdata <- prepMainPlot(
  mdata = adhd_s_1.mdata, 
  out_name = "Full sample, unadjusted", 
  coh_type = "combined")

adhd_s.pdata <- prepMainPlot(
  mdata = adhd.mdata, 
  out_name = "Complete cases, adjusted", 
  coh_type = "combined")

adhd_s2.pdata <- prepMainPlot(
  mdata = adhd_s_2.mdata, 
  out_name = "Complete cases, unadjusted", 
  coh_type = "combined")
  






################################################################################
# Fixed effect models  
################################################################################

## ---- Function ---------------------------------------------------------------
exwasTab <- function(fit){
  
coefs <- fit %>%
  pmap(function(exposure, cohort, fit, ...){
    
     tmp <- dh.lmTab(
      model = fit, 
      type = "glm_slma", 
      coh_names = cohort, 
      direction = "wide", 
      ci_format = "separate") %>%
       dplyr::filter(variable %in% exposure & cohort != "combined")
    }) %>%
  set_names(fit$exposure) %>%
  bind_rows 

out <- coefs %>%
  dplyr::rename(analysis_name = variable) %>%
  left_join(., all_vars, by = "analysis_name") %>%
  dplyr::select(full_name, family, cohort, est, pvalue) %>%
  dplyr::filter(pvalue < 0.05) %>%
  mutate(pvalue = round(pvalue, 2)) %>%
  pivot_wider(
    names_from = cohort, 
    values_from = c(est, pvalue), 
    names_glue = "{cohort}_{.value}") %>%
  arrange(family, full_name) %>%
  dplyr::select(family, full_name, order(colnames(.))) 

}

## ---- Language ---------------------------------------------------------------
lan.exTab <- exwasTab(lan.fit %>% dplyr::filter(converged == TRUE))
write_csv(lan.exTab, file = here("tables", "lan_exwas.csv"))

## ---- Non-verbal intelligence ------------------------------------------------
nvi.exTab <- exwasTab(nvi.fit %>% dplyr::filter(converged == TRUE))
write_csv(nvi.exTab, file = here("tables", "nvi_exwas.csv"))

## ---- Working memory ---------------------------------------------------------
wm.exTab <- exwasTab(wm.fit %>% dplyr::filter(converged == TRUE))
write_csv(wm.exTab, file = here("tables", "wm_exwas.csv"))

## ---- Fine motor -------------------------------------------------------------
fm.exTab <- exwasTab(fm.fit %>% dplyr::filter(converged == TRUE))
write_csv(fm.exTab, file = here("tables", "fm_exwas.csv"))

## ---- Gross motor ------------------------------------------------------------
gm.exTab <- exwasTab(gm.fit %>% dplyr::filter(converged == TRUE))
write_csv(gm.exTab, file = here("tables", "gm_exwas.csv"))










################################################################################
# Language: NINFEA  
################################################################################
ds.colnames("lan_sub")

ninfea_lan <- dh.getStats(
  df = "lan_sub",
  vars = c("lan_raw_.0_17", "lan_age_.0_17"))

ninfea_lan.tab <- ninfea_lan$continuous %>%
  dplyr::filter(cohort == "ninfea") %>%
  dplyr::select(variable, mean, std.dev, valid_n, cohort_n, missing_n, 
                missing_perc)

write_csv(ninfea_lan.tab, file = here("tables", "ninfea_lan.csv"))

################################################################################
# Covariates: NINFEA  
################################################################################
ds.colnames("covariates")

ninfea_covs <- dh.getStats(
  df = "covariates", 
  vars = c("agebirth_m_y", "sex", "parity_bin", "birth_month_f", "edu_m_.0_1",       
           "areases_tert_.0_1"))

ninfea_cat_covs.tab <- ninfea_covs$categorical %>% 
  dplyr::filter(cohort == "ninfea") %>%
  dplyr::select(variable, category, value, valid_n, cohort_n, perc_total)

ninfea_cont_covs.tab <- ninfea_covs$continuous %>%
  dplyr::filter(cohort == "ninfea") %>%
  dplyr::select(variable, mean, std.dev, valid_n, cohort_n, missing_n, 
                missing_perc)

write_csv(ninfea_cat_covs.tab, file = here("tables", "ninfea_cat_covs.csv"))
write_csv(ninfea_cont_covs.tab, file = here("tables", "ninfea_cont_covs.csv"))



ds.colnames("lan_sub")


lan_hist <- ds.histogram("lan_sub$lan_raw_.0_17")

dh.boxCox(
  df = "lan_sub", 
  var = "lan_raw_.0_17", 
  lamda = seq(-2, 2, 0.1),
  type = "combine",
  transform = TRUE,
  new_obj = "lan_trans", 
  checks = FALSE)

ds.histogram("lan_trans")


