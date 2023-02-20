################################################################################
## Project: Exposome and Mental Health
## Script purpose: Visualise exposures
## Date: 13th September 2022
## Author: Tim Cadman
## Email: tica@sund.ku.dk
################################################################################

library(see)
source("https://raw.githubusercontent.com/timcadman/useful-code-r/master/code/themes/themes.R")

################################################################################
# 1. Violin function  
################################################################################
hVPlot <- function(data = NULL, y_label = NULL, limits = NULL, 
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
# 2. Reference table to identify continuous variables  
################################################################################
cont_exp.vars <- inner_join(
  all_vars %>% dplyr::filter(type == "exposure"),
  filled$non_rep %>% dplyr::filter(ninfea %in% c("integer", "numeric")), 
  by = "variable") %>%
  pull(variable)


################################################################################
# 3. EXPOSURES  
################################################################################
################################################################################
# Prepare data  
################################################################################

## ---- Identify valid variables -----------------------------------------------
exp.stats <- dh.getStats(
  df = "non_rep_sub", 
  vars = cont_exp.vars)

violin_exp.ref <- exp.stats$continuous %>%
  dplyr::filter(cohort_n != missing_n & cohort != "combined") %>%
  dplyr::filter(variable != "port_preg") %>%
  dplyr::select(variable, cohort) 

## ---- Get anonymised data ----------------------------------------------------
violin_exp.data <- violin_exp.ref %>%
  pmap(function(variable, cohort){
    
    dh.getAnonPlotData(
      df = "non_rep_sub", 
      var_1 = variable, 
      checks = F, 
      conns = conns[unlist(cohort)])
    
  })

save.image()

## ---- Transform to plotable format -------------------------------------------
violin_exp_clean.data <- violin_exp.data %>%
  map(
    ~pivot_longer(., 
                  cols = -cohort,
                  names_to = "variable", 
                  values_to = "value")) %>%
  bind_rows %>%
  dplyr::filter(cohort != "combined")

save.image()

################################################################################
# Make violin plots  
################################################################################
exp_violin_plots <- violin_exp_clean.data %>%
  group_by(variable) %>%
  group_split

length(exp_violin_plots)

## ---- Already removed --------------------------------------------------------
hVPlot(data = exp_violin_plots[[27]], title = exp_violin_plots[[27]]$variable[1])
hVPlot(data = exp_violin_plots[[7]], title = exp_violin_plots[[7]]$variable[1])
hVPlot(data = exp_violin_plots[[33]], title = exp_violin_plots[[33]]$variable[1])
hVPlot(data = exp_violin_plots[[44]], title = exp_violin_plots[[44]]$variable[1])
hVPlot(data = exp_violin_plots[[14]], title = exp_violin_plots[[14]]$variable[1])
hVPlot(data = exp_violin_plots[[10]], title = exp_violin_plots[[10]]$variable[1])
hVPlot(data = exp_violin_plots[[40]], title = exp_violin_plots[[40]]$variable[1])

## ---- Already transformed ----------------------------------------------------
hVPlot(data = exp_violin_plots[[18]], title = exp_violin_plots[[18]]$variable[1])
hVPlot(data = exp_violin_plots[[21]], title = exp_violin_plots[[21]]$variable[1])
hVPlot(data = exp_violin_plots[[22]], title = exp_violin_plots[[22]]$variable[1])
hVPlot(data = exp_violin_plots[[25]], title = exp_violin_plots[[25]]$variable[1])
hVPlot(data = exp_violin_plots[[16]], title = exp_violin_plots[[16]]$variable[1])
hVPlot(data = exp_violin_plots[[9]], title = exp_violin_plots[[9]]$variable[1])
hVPlot(data = exp_violin_plots[[1]], title = exp_violin_plots[[1]]$variable[1])
hVPlot(data = exp_violin_plots[[8]], title = exp_violin_plots[[8]]$variable[1])
hVPlot(data = exp_violin_plots[[17]], title = exp_violin_plots[[17]]$variable[1])

## ---- Problematic ------------------------------------------------------------
hVPlot(data = exp_violin_plots[[41]], title = exp_violin_plots[[41]]$variable[1])
hVPlot(data = exp_violin_plots[[43]], title = exp_violin_plots[[43]]$variable[1])

## ---- Not problematic --------------------------------------------------------
hVPlot(data = exp_violin_plots[[3]], title = exp_violin_plots[[3]]$variable[1])
hVPlot(data = exp_violin_plots[[4]], title = exp_violin_plots[[4]]$variable[1])
hVPlot(data = exp_violin_plots[[5]], title = exp_violin_plots[[5]]$variable[1])
hVPlot(data = exp_violin_plots[[12]], title = exp_violin_plots[[12]]$variable[1])
hVPlot(data = exp_violin_plots[[13]], title = exp_violin_plots[[13]]$variable[1])
hVPlot(data = exp_violin_plots[[24]], title = exp_violin_plots[[24]]$variable[1])
hVPlot(data = exp_violin_plots[[26]], title = exp_violin_plots[[26]]$variable[1])
hVPlot(data = exp_violin_plots[[28]], title = exp_violin_plots[[28]]$variable[1])
hVPlot(data = exp_violin_plots[[29]], title = exp_violin_plots[[29]]$variable[1])
hVPlot(data = exp_violin_plots[[30]], title = exp_violin_plots[[30]]$variable[1])
hVPlot(data = exp_violin_plots[[31]], title = exp_violin_plots[[31]]$variable[1])
hVPlot(data = exp_violin_plots[[32]], title = exp_violin_plots[[32]]$variable[1])
hVPlot(data = exp_violin_plots[[34]], title = exp_violin_plots[[34]]$variable[1])
hVPlot(data = exp_violin_plots[[35]], title = exp_violin_plots[[35]]$variable[1])
hVPlot(data = exp_violin_plots[[36]], title = exp_violin_plots[[36]]$variable[1])
hVPlot(data = exp_violin_plots[[37]], title = exp_violin_plots[[37]]$variable[1])
hVPlot(data = exp_violin_plots[[42]], title = exp_violin_plots[[42]]$variable[1])
hVPlot(data = exp_violin_plots[[45]], title = exp_violin_plots[[45]]$variable[1])
hVPlot(data = exp_violin_plots[[19]], title = exp_violin_plots[[19]]$variable[1])
hVPlot(data = exp_violin_plots[[23]], title = exp_violin_plots[[23]]$variable[1])
hVPlot(data = exp_violin_plots[[11]], title = exp_violin_plots[[11]]$variable[1])
hVPlot(data = exp_violin_plots[[39]], title = exp_violin_plots[[39]]$variable[1])

## ---- Check scale ------------------------------------------------------------
hVPlot(data = exp_violin_plots[[15]], title = exp_violin_plots[[15]]$variable[1])
hVPlot(data = exp_violin_plots[[2]], title = exp_violin_plots[[2]]$variable[1])
hVPlot(data = exp_violin_plots[[6]], title = exp_violin_plots[[6]]$variable[1])
hVPlot(data = exp_violin_plots[[20]], title = exp_violin_plots[[20]]$variable[1])
hVPlot(data = exp_violin_plots[[46]], title = exp_violin_plots[[46]]$variable[1])

################################################################################
# 4. OUTCOMES  
################################################################################
################################################################################
# Prepare data  
################################################################################

## ---- Identify valid variables -----------------------------------------------
out_dist.stats <- out_sub_ref %>%
  pmap(function(var, coh){
    
    dh.getStats(
      df = paste0(var, "_sub"), 
      vars = paste0(var, "_raw_"), 
      conns = conns[unlist(coh)])
    
  }) 

violin_out.ref <- out_dist.stats %>% 
  map(~.x$continuous) %>%
  bind_rows() %>%
  dplyr::filter(cohort != "combined") %>%
  dplyr::select(variable, cohort) 

out_df_ref <- out_sub_ref %>%
  mutate(
    variable = paste0(var, "_raw_"), 
    df = paste0(var, "_sub")) %>%
  dplyr::select(-coh, -var)

violin_out.ref %>% print(n = Inf)

## ---- Get anonymised data ----------------------------------------------------
violin_out.data <- violin_out.ref %>%
  left_join(., out_df_ref) %>%
  pmap(function(variable, df, cohort){
    
    dh.getAnonPlotData(
      df = df, 
      var_1 = variable, 
      checks = F, 
      conns = conns[cohort])
    
  })

save.image()

## ---- Transform to plotable format -------------------------------------------
violin_out_clean.data <- violin_out.data %>%
  map(
    ~pivot_longer(., 
                  cols = -cohort,
                  names_to = "variable", 
                  values_to = "value")) %>%
  bind_rows %>%
  dplyr::filter(cohort != "combined")

save.image()

################################################################################
# Make violin plots  
################################################################################
out_violin_plots <- violin_out_clean.data %>%
  group_by(variable) %>%
  group_split

length(out_violin_plots)

hVPlot(data = out_violin_plots[[1]], title = out_violin_plots[[1]]$variable[1])
hVPlot(data = out_violin_plots[[2]], title = out_violin_plots[[2]]$variable[1])
hVPlot(data = out_violin_plots[[3]], title = out_violin_plots[[3]]$variable[1])
hVPlot(data = out_violin_plots[[4]], title = out_violin_plots[[4]]$variable[1])
hVPlot(data = out_violin_plots[[5]], title = out_violin_plots[[5]]$variable[1])
hVPlot(data = out_violin_plots[[6]], title = out_violin_plots[[6]]$variable[1])
hVPlot(data = out_violin_plots[[7]], title = out_violin_plots[[7]]$variable[1])
hVPlot(data = out_violin_plots[[8]], title = out_violin_plots[[8]]$variable[1])


################################################################################
# 4. Look at distributions  
################################################################################

str(exp_violin_plots)


plot(exp_violin_plots[1])




################################################################################
# 5. Visualise variables with skewed distributions
################################################################################

## ---- Prepare data -----------------------------------------------------------
trans.vars <- c("fdensity300_preg", "frichness300_preg", "hdres_preg", 
                "indtr_preg", "ldres_preg", "trans_preg")

trans.stats <- dh.getStats(
  df = "non_rep_sub", 
  vars = trans.vars)

violin_alsp.ref <- trans.stats$continuous %>%
  dplyr::filter(cohort_n != missing_n & cohort != "combined") %>%
  dplyr::select(variable, cohort) 

violin_alsp.data <- violin_alsp.ref %>%
  pmap(function(variable, cohort){
    
    dh.getAnonPlotData(
      df = "non_rep_sub", 
      var_1 = variable, 
      checks = F, 
      conns = conns[unlist(cohort)])
    
  })

violin_alsp_clean.data <- violin_alsp.data %>%
  map(
    ~pivot_longer(., 
                  cols = -cohort,
                  names_to = "variable", 
                  values_to = "value")) %>%
  bind_rows %>%
  dplyr::filter(cohort != "combined")





################################################################################
# 5. Visualise variables with skewed distributions
################################################################################

## ---- Prepare data -----------------------------------------------------------
trans.vars <- c("fdensity300_preg", "frichness300_preg", "hdres_preg", 
                "indtr_preg", "ldres_preg", "trans_preg")

trans.stats <- dh.getStats(
  df = "non_rep_sub", 
  vars = trans.vars)

violin_alsp.ref <- trans.stats$continuous %>%
  dplyr::filter(cohort_n != missing_n & cohort != "combined") %>%
  dplyr::select(variable, cohort) 

violin_alsp.data <- violin_alsp.ref %>%
  pmap(function(variable, cohort){
    
    dh.getAnonPlotData(
      df = "non_rep_sub", 
      var_1 = variable, 
      checks = F, 
      conns = conns[unlist(cohort)])
    
  })

violin_alsp_clean.data <- violin_alsp.data %>%
  map(
    ~pivot_longer(., 
                  cols = -cohort,
                  names_to = "variable", 
                  values_to = "value")) %>%
  bind_rows %>%
  dplyr::filter(cohort != "combined")


## ---- Make plots -------------------------------------------------------------
violin_plots <- violin_alsp_clean.data %>%
  group_by(variable) %>%
  group_split %>%
  map(~hVPlot(data = .x, title = .x$variable[1]))


violin_plots[[2]]
violin_plots[[3]]
violin_plots[[4]]
violin_plots[[5]]
violin_plots[[6]]