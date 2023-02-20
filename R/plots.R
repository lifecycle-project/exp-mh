################################################################################
## Project: Exposome and Mental Health
## Script purpose: Plots
## Date: 4th August 2022
## Author: Tim Cadman
## Email: tica@sund.ku.dk
################################################################################
library(here)
library(gridExtra)

source("https://raw.githubusercontent.com/timcadman/useful-code-r/master/code/themes/lc-names-neat.R")
source("https://raw.githubusercontent.com/timcadman/useful-code-r/master/code/themes/themes.R")

################################################################################
#   
################################################################################
mdata = lan.mdata
var = "water_preg_iqr_c" 

all_vars



prepMainPlot <- function(mdata, var){
  
  dat <- mdata %>% 
    dplyr::rename(analysis_name = variable) %>%
    dplyr::filter(analysis_name == var) %>%
    left_join(., all_vars, by = "analysis_name") %>%
    left_join(., names_neat, by = "cohort")
    
    coh_names <- c(sort(dat$cohort_neat[dat$cohort_neat != "Combined"]), "Combined") 
    
    dat %>%
    mutate(
      title = paste0(full_name, ", I2 = ", i2, "%"), 
      coh_type = ifelse(cohort == "combined", "combined", "cohort"), 
      cohort_neat = factor(cohort_neat, levels = coh_names), ordered = TRUE) %>%
    mutate(sig = case_when(
      cohort == "combined" & (lowci > 0 & uppci > 0) ~ "yes", 
      cohort == "combined" & (lowci < 0 & uppci < 0) ~ "yes",
      cohort == "combined" & (lowci > 0 & uppci < 0) ~ "no", 
      cohort == "combined" & (lowci < 0 & uppci > 0) ~ "no", 
      cohort != "combined" ~ "no")) %>%
      mutate(across(c(est, lowci, uppci), ~round(., 2)))
      
}


prep_data <- lan.pdata[[1]]

exwasPlot <- function(prep_data){
  
ggplot(prep_data, aes(y = fct_rev(cohort_neat), x = est, xmin = lowci, xmax = uppci)) + 
  geom_point(aes(shape = coh_type, size = coh_type, colour = sig)) + 
  geom_errorbarh(height = .1) + 
  geom_text(
    aes(
      x = 1.5, 
      label = paste0(est, " (", lowci, ", ", uppci, ")")), size=3, hjust=0) + 
  geom_vline(xintercept = 0, 
             linetype=2, 
             size=.3,
             colour = "black") +
  geom_hline(yintercept = 1.5, 
             linetype=1, size=.9,
             colour = "black") +
  scale_colour_manual(values = c("black", "green")) +
  scale_shape_manual(values = c(16,18)) +
  scale_size_manual(values = c(3, 6)) + 
  scale_x_continuous(breaks  =c (-1,0,1)) +
  expand_limits(x = c(-1.5, 3)) +
  labs(x ="Mean change (95%CI) in score for an IQR increase in exposure")+
  theme(
    text = element_text(size = 12,family = "Times"),
    axis.text.y = element_text(hjust = 1),
    axis.title.y = element_blank(),
    axis.title.x = element_text(),
    axis.ticks = element_line(colour = "black"),
    axis.line = element_line(size = .2, linetype = "solid"),
    panel.background = element_blank(),
    plot.title=element_text(hjust = .5))+
  guides(shape = "none", size = "none", colour = "none") +
  ggtitle(prep_data %>% slice_tail %>% pull(title)) + 
  theme(
    strip.text = element_text(size=13),
    strip.background = element_rect(
      color="black", fill="white", size=.5, linetype="solid"))

}

## ---- Language ---------------------------------------------------------------
exp_names <- avail_exp %>%
  dplyr::filter(variable != "airpt_preg_iqr_c") %>%
  pull(variable)

lan.pdata <- exp_names %>%
  map(~prepMainPlot(
    mdata = lan.mdata, 
    var = .x)) 

lan.plots <- lan.pdata %>% map(~exwasPlot(prep_data = .x))
    
ggsave("lan_exwas.pdf", marrangeGrob(grobs = lan.plots, nrow = 2, ncol = 2),
       device = "pdf", height = 210, width = 297, units = "mm")

## ---- NVI --------------------------------------------------------------------
nvi.pdata <- exp_names %>%
  map(~prepMainPlot(
    mdata = nvi.mdata, 
    var = .x)) 

nvi.plots <- nvi.pdata %>% map(~exwasPlot(prep_data = .x))

ggsave("nvi_exwas.pdf", marrangeGrob(grobs = nvi.plots, nrow = 2, ncol = 2),
       device = "pdf", height = 210, width = 297, units = "mm")

## ---- Working memory ---------------------------------------------------------
wm.pdata <- exp_names %>%
  map(~prepMainPlot(
    mdata = wm.mdata, 
    var = .x)) 

wm.plots <- wm.pdata %>% map(~exwasPlot(prep_data = .x))

ggsave("wm_exwas.pdf", marrangeGrob(grobs = wm.plots, nrow = 2, ncol = 2),
       device = "pdf", height = 210, width = 297, units = "mm")


## ---- Fine motor -------------------------------------------------------------
fm.pdata <- exp_names %>%
  map(~prepMainPlot(
    mdata = fm.mdata, 
    var = .x)) 

fm.plots <- fm.pdata %>% map(~exwasPlot(prep_data = .x))

ggsave("fm_exwas.pdf", marrangeGrob(grobs = fm.plots, nrow = 2, ncol = 2),
       device = "pdf", height = 210, width = 297, units = "mm")


## ---- Gross motor ------------------------------------------------------------
gm.pdata <- exp_names %>%
  map(~prepMainPlot(
    mdata = gm.mdata, 
    var = .x)) 

gm.plots <- gm.pdata %>% map(~exwasPlot(prep_data = .x))

ggsave("gm_exwas.pdf", marrangeGrob(grobs = gm.plots, nrow = 2, ncol = 2),
       device = "pdf", height = 210, width = 297, units = "mm")


################################################################################
# 1. Data preparation  
################################################################################
prepEPlot <- function(x, name){
  
  x %>%
  map(., ~.x[[1]]$exwas_results) %>%
    bind_rows(.id = "cohort") %>%
    dplyr::select(-family) %>%
    left_join(., family_ref, by = "exposure") %>%
    arrange(family) %>%
    as_tibble %>%
    dplyr::filter(p.value < 0.05)
  
}

family_ref <- all_vars %>% dplyr::rename(exposure = analysis_name)

int.pdata <- prepEPlot(int.ewas, "internalising")
ext.pdata <- prepEPlot(ext.ewas, "externalising")
adhd.pdata <- prepEPlot(adhd.ewas, "adhd")
lan.pdata <- prepEPlot(lan.ewas, "language")
nvi.pdata <- prepEPlot(nvi.ewas, "nvi")
wm.pdata <- prepEPlot(wm.ewas, "working_memory")
fm.pdata <- prepEPlot(fm.ewas, "fine_motor")
gm.pdata <- prepEPlot(gm.ewas, "gross_motor")


exwas.tab <- list(int.pdata, ext.pdata, adhd.pdata, lan.pdata, nvi.pdata,
     wm.pdata, fm.pdata, gm.pdata) %>%
  set_names(c("int", "ext", "adhd", "lan", "nvi", "wm", "fm", "gm")) %>%
  bind_rows(.id = "outcome") %>% 
  mutate(family = factor(family,  ordered = T)) %>%
  mutate(across(coefficient:p.value, ~round(., 2))) %>%
  dplyr::select(cohort, exposure, family, coefficient, p.value, outcome) %>%
  pivot_wider(
    names_from = c(cohort),
    values_from = c(coefficient, p.value)) %>%
  dplyr::select(exposure, family, outcome, "coefficient_inma_sab", 
                "p.value_inma_sab", "coefficient_inma_val", "p.value_inma_val",
                "coefficient_inma_gip", "p.value_inma_gip", 
                "coefficient_ninfea", "p.value_ninfea")

write_csv(exwas.tab, file = here("tables", "exwas_6_10_22.csv"))


exwas.tab %>% print(n = Inf)




all.data <- bind_rows(
  list(int.pdata, ext.pdata, adhd.pdata, lan.pdata, nvi.pdata,
                      wm.pdata, fm.pdata, gm.pdata), 
  .id = "outcome") %>%
  mutate(across(coefficient:p.value, ~round(., 2))) %>%
  dplyr::select(cohort, exposure, p.value) %>%
  pivot_wider(
    names_from = c(cohort, outcome),
    values_from = p.value)
  
  mutate(coef_ci = paste0(coefficient, " (", minE, ", ", maxE, ")"))

int.pdata %>% 
  dplyr::filter(cohort == "ninfea") %>%
  as_tibble %>%
  print(n = Inf)
  
  
save.image()

################################################################################
# 2. Externalising  
################################################################################

## ---- inma gip ---------------------------------------------------------------
ext.plot_1 <- ggplot(int.pdata %>% dplyr::filter(cohort == "inma_gip"), 
       aes(
         x = fct_inorder(exposure), 
         y = coefficient, 
         ymin = minE, 
         ymax = maxE, 
         colour = family)) +
  geom_point(size = 0.8) +
  geom_errorbar(width= 0.1) +
  geom_hline(yintercept = 0) +
  theme_std + 
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
    panel.grid.minor = element_line(colour = "white"),                                     ,
    panel.grid.major = element_line(colour = "grey"),
    axis.ticks = element_line(colour = scales::alpha("#CCCCCC", 0.3))) +
  ylim(c(-0.6, 0.6)) +
  scale_color_discrete(palette_std)

## ---- inma sab ---------------------------------------------------------------
ext.plot_2 <- ggplot(int.pdata %>% dplyr::filter(cohort == "inma_sab"), 
                     aes(
                       x = fct_inorder(exposure), 
                       y = coefficient, 
                       ymin = minE, 
                       ymax = maxE, 
                       colour = family)) +
  geom_point(size = 0.8) +
  geom_errorbar(width= 0.1) +
  geom_hline(yintercept = 0) +
  theme_std + 
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
    panel.grid.minor = element_line(colour = "white"),                                     ,
    panel.grid.major = element_line(colour = "grey"),
    axis.ticks = element_line(colour = scales::alpha("#CCCCCC", 0.3))) +
  ylim(c(-0.6, 0.6)) +
  scale_color_discrete(palette_std)

## ---- inma sab ---------------------------------------------------------------
ext.plot_3 <- ggplot(int.pdata %>% dplyr::filter(cohort == "inma_val"), 
                     aes(
                       x = fct_inorder(exposure), 
                       y = coefficient, 
                       ymin = minE, 
                       ymax = maxE, 
                       colour = family)) +
  geom_point(size = 0.8) +
  geom_errorbar(width= 0.1) +
  geom_hline(yintercept = 0) +
  theme_std + 
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
    panel.grid.minor = element_line(colour = "white"),                                     ,
    panel.grid.major = element_line(colour = "grey"),
    axis.ticks = element_line(colour = scales::alpha("#CCCCCC", 0.3))) +
  ylim(c(-0.6, 0.6)) +
  scale_color_discrete(palette_std)

## ---- ninfea -----------------------------------------------------------------
ext.plot_4 <- ggplot(int.pdata %>% dplyr::filter(cohort == "ninfea"), 
                     aes(
                       x = fct_inorder(exposure), 
                       y = coefficient, 
                       ymin = minE, 
                       ymax = maxE, 
                       colour = family)) +
  geom_point(size = 0.8) +
  geom_errorbar(width= 0.1) +
  geom_hline(yintercept = 0) +
  theme_std + 
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
    panel.grid.minor = element_line(colour = "white"),                                     ,
    panel.grid.major = element_line(colour = "grey"),
    axis.ticks = element_line(colour = scales::alpha("#CCCCCC", 0.3))) +
  ylim(c(-0.6, 0.6)) +
  scale_color_discrete(palette_std)

## ---- Save -------------------------------------------------------------------
ggsave(
  plot = ext.plot_1,
  filename = here("figures", "ext_ewas_1.png"),
  h = 15, w = word_land, 
  units="cm", 
  dpi=300, type="cairo")

ggsave(
  plot = ext.plot_2,
  filename = here("figures", "ext_ewas_2.png"),
  h = 15, w = word_land, 
  units="cm", 
  dpi=300, type="cairo")

ggsave(
  plot = ext.plot_3,
  filename = here("figures", "ext_ewas_3.png"),
  h = 15, w = word_land, 
  units="cm", 
  dpi=300, type="cairo")

ggsave(
  plot = ext.plot_4,
  filename = here("figures", "ext_ewas_4.png"),
  h = 15, w = word_land, 
  units="cm", 
  dpi=300, type="cairo")

ds.histogram("int_sub_z$ext_z_t", datasources = conns[int_coh])
ds.histogram("ext_sub_z$ext_z_t", datasources = conns[ext_coh])
ds.histogram("adhd_sub_z$ext_z_t", datasources = conns[adhd_coh])
ds.histogram("lan_sub_z$ext_z_t", datasources = conns[lan_coh])
ds.histogram("nvi_sub_z$ext_z_t", datasources = conns[nvi_coh])
ds.histogram("wm_sub_z$ext_z_t", datasources = conns[wm_coh])
ds.histogram("gm_sub_z$ext_z_t", datasources = conns[gm_coh])
ds.histogram("fm_sub_z$ext_z_t", datasources = conns[fm_coh])


################################################################################
# 1. Transformation plots NINFEA  
################################################################################
hist_plots <- list(fdens_pre, fdens_post, frich_pre, frich_post, hdres_pre, 
                hdres_post, indtr_pre, indtr_post, trans_pre, trans_post)

#ldres_pre ldres_post,

hist_names <- list("fdens_pre", "fdens_post", "frich_pre", "frich_post", 
                 "hdres_pre", "hdres_post", "indtr_pre", "indtr_post", 
                 "trans_pre", "trans_post")

#"ldres_pre", "ldres_post", 

list(plots = hist_plots, names = hist_names) %>%
  pmap(function(plots, names){
    
    png(here("figures", paste0(names, ".png")))
    plot(plots)
    dev.off()
    
  })

plot(fdens_post)

str(fdens_post)
################################################################################
# 2. Lanuage distribution  
################################################################################
png(here("figures", "lan_hist.png"))
plot(lan_hist)
dev.off()


