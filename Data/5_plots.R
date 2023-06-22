library(forestplot)
library(data.table)
library(tidyverse)
library(magrittr)
library(grid)
library(gridExtra)

ind_vars <- c("rutter_5", "rutter_10", "cds", "malaise", "beh_emo", "int_beh", "ext_beh")

load_data <- function(ind_var){
  df <- fread(paste0(".\\Results\\", ind_var, ".csv"))
  df %<>% 
    select(-ind_var) %>%
    mutate(ind_var = ind_var, .before = dep_var)
  return(df)
}

results <- lapply(ind_vars, load_data) %>% reduce(rbind)

results %<>% 
  rename(mean = RR, lower = RR.LCI, upper = RR.UCI) %>%
  mutate(p.rounded = sprintf("%.3f", round(p, 3)),
         E.rounded = sprintf("%.3f", round(Evalue, 2))) %>%
  mutate(ind_var = case_when(
    ind_var == "beh_emo" ~ "Behavioural & emotional problems (16)",
    ind_var == "cds" ~ "Child Development Scale (10)",
    ind_var == "ext_beh" ~ "Externalising behaviours (16)",
    ind_var == "int_beh" ~ "Internalising behaviours (16)",
    ind_var == "malaise" ~ "Malaise score (16)",
    ind_var == "rutter_5" ~ "Rutter score (5)",
    ind_var == "rutter_10" ~ "Rutter score (10)"
  ))

results_fp <- function(current_dep_var, title){
  forestplot(
    results[results$dep_var == current_dep_var,],
    labeltext = c(ind_var, p.rounded, E.rounded),
    boxsize = 0.2,
    xlab = "Risk ratio",
    zero = 1,
    title = title
  ) %>%
    fp_add_lines() %>%
    fp_set_style(align = "lcc") %>%
    fp_add_header(
      ind_var = "Variable", 
      p.rounded = fp_align_center("p"), 
      E.rounded = fp_align_center("E-value")
    )
}

fp1 <- grid.grabExpr(print(results_fp("dv_sr_sleep_abn", "Self-reported (summary)")))

fp2 <- grid.grabExpr(print(results_fp("dv_sd_sleep_abn", "Self-reported (diary)")))

fp3 <- grid.grabExpr(print(results_fp("dv_pal_sleep_abn", "activPAL algorithm")))
                     
fp4 <- grid.grabExpr(print(results_fp("dv_vdb_sleep_abn", "van der Berg et al. algorithm")))

fp5 <- grid.grabExpr(print(results_fp("dv_winkler_sleep_abn", "Winkler et al. algorithm")))

plotgrid <- gridExtra::grid.arrange(fp1, fp2, fp3, fp4, fp5, ncol = 1)
ggsave(".\\Results\\forestplots.svg", plotgrid, height = 29.7, width = 21, units = "cm")

load_multi_data <- function(ind_var){
  df <- fread(paste0(".\\Results\\", ind_var, "_multi.csv"))
  df %<>%
    select(-ind_var) %>%
    mutate(ind_var = ind_var, .before = dep_var)
  return(df)
}

multi_results <- lapply(ind_vars, load_multi_data) %>% reduce(rbind)

multi_results %<>% 
  rename(mean = RRR, lower = RRR.LCI, upper = RRR.UCI) %>%
  mutate(p.rounded = sprintf("%.3f", round(p, 3)),
         E.rounded = sprintf("%.3f", round(Evalue, 2))) %>%
  mutate(ind_var = case_when(
    ind_var == "beh_emo" ~ "Behavioural & emotional problems (16)",
    ind_var == "cds" ~ "Child Development Scale (10)",
    ind_var == "ext_beh" ~ "Externalising behaviours (16)",
    ind_var == "int_beh" ~ "Internalising behaviours (16)",
    ind_var == "malaise" ~ "Malaise score (16)",
    ind_var == "rutter_5" ~ "Rutter score (5)",
    ind_var == "rutter_10" ~ "Rutter score (10)"
  ))

multi_results_fp <- function(current_dep_var, current_group, title){
  forestplot(
    multi_results[multi_results$dep_var == current_dep_var & multi_results$group == current_group,],
    labeltext = c(ind_var, p.rounded, E.rounded),
    boxsize = 0.2,
    xlab = "Relative risk ratio",
    zero = 1,
    title = title
  ) %>%
    fp_add_lines() %>%
    fp_set_style(align = "lcc") %>%
    fp_add_header(
      ind_var = "Variable",
      p.rounded = fp_align_center("p"), 
      E.rounded = fp_align_center("E-value")
    )
}

fp6 <- grid.grabExpr(print(multi_results_fp("dv_sr_sleep_abn_cat", "Short", "Self-reported (summary)")))

fp7 <- grid.grabExpr(print(multi_results_fp("dv_sd_sleep_abn_cat", "Short", "Self-reported (diary)")))

fp8 <- grid.grabExpr(print(multi_results_fp("dv_pal_sleep_abn_cat", "Short", "activPAL algorithm")))

fp9 <- grid.grabExpr(print(multi_results_fp("dv_vdb_sleep_abn_cat", "Short", "van der Berg et al. algorithm")))

fp10 <- grid.grabExpr(print(multi_results_fp("dv_winkler_sleep_abn_cat", "Short", "Winkler et al. algorithm")))

short_plotgrid <- gridExtra::grid.arrange(fp6, fp7, fp8, fp9, fp10, top = textGrob("Abnormally short sleep duration", gp = gpar(fontsize = 18)), ncol = 1)

fp11 <- grid.grabExpr(print(multi_results_fp("dv_sr_sleep_abn_cat", "Long", "Self-reported (summary)")))

fp12 <- grid.grabExpr(print(multi_results_fp("dv_sd_sleep_abn_cat", "Long", "Self-reported (diary)")))

fp13 <- grid.grabExpr(print(multi_results_fp("dv_pal_sleep_abn_cat", "Long", "activPAL algorithm")))

fp14 <- grid.grabExpr(print(multi_results_fp("dv_vdb_sleep_abn_cat", "Long", "van der Berg et al. algorithm")))

fp15 <- grid.grabExpr(print(multi_results_fp("dv_winkler_sleep_abn_cat", "Long", "Winkler et al. algorithm")))

long_plotgrid <- gridExtra::grid.arrange(fp11, fp12, fp13, fp14, fp15, top = textGrob("Abnormally long sleep duration", gp = gpar(fontsize = 18)), ncol = 1)

combinedmulti_plotgrid <- gridExtra::grid.arrange(short_plotgrid, long_plotgrid, ncol = 1)
ggsave(".\\Results\\combinedmulti_forestplots.svg", combinedmulti_plotgrid, height = 59.4, width = 21, units = "cm")

load_med_data <- function(ind_var){
  df <- fread(paste0(".\\Results\\Mediation\\", ind_var, "_med2.csv"))
  df %<>% 
    select(-ind_var) %>%
    mutate(ind_var = ind_var, .before = dep_var)
  return(df)
}

med_results <- lapply(ind_vars, load_med_data) %>% reduce(rbind)

med_results %<>% 
  rename(mean = RR, lower = RR.LCI, upper = RR.UCI) %>%
  mutate(p.rounded = sprintf("%.3f", round(p, 3)),
         E.rounded = sprintf("%.3f", round(Evalue, 3))) %>%
  mutate(ind_var = case_when(
    ind_var == "beh_emo" ~ "Behavioural & emotional problems (16)",
    ind_var == "cds" ~ "Child Development Scale (10)",
    ind_var == "ext_beh" ~ "Externalising behaviours (16)",
    ind_var == "int_beh" ~ "Internalising behaviours (16)",
    ind_var == "malaise" ~ "Malaise score (16)",
    ind_var == "rutter_5" ~ "Rutter score (5)",
    ind_var == "rutter_10" ~ "Rutter score (10)"
  ))

med_results_fp <- function(current_dep_var, title){
  forestplot(
    med_results[med_results$dep_var == current_dep_var,],
    labeltext = c(ind_var, p.rounded, E.rounded),
    boxsize = 0.2,
    xlab = "Risk ratio",
    zero = 1,
    title = title
  ) %>%
    fp_add_lines() %>%
    fp_set_style(align = "lcc") %>%
    fp_add_header(
      ind_var = "Variable", 
      p.rounded = fp_align_center("p"),
      E.rounded = fp_align_center("E-value")
    )
}

fp16 <- grid.grabExpr(print(med_results_fp("dv_sr_sleep_abn", "Self-reported (summary)")))

fp17 <- grid.grabExpr(print(med_results_fp("dv_sd_sleep_abn", "Self-reported (diary)")))

fp18 <- grid.grabExpr(print(med_results_fp("dv_pal_sleep_abn", "activPAL algorithm")))

fp19 <- grid.grabExpr(print(med_results_fp("dv_vdb_sleep_abn", "van der Berg et al. algorithm")))

fp20 <- grid.grabExpr(print(med_results_fp("dv_winkler_sleep_abn", "Winkler et al. algorithm")))

med_plotgrid <- gridExtra::grid.arrange(fp16, fp17, fp18, fp19, fp20, ncol = 1)
ggsave(".\\Results\\Mediation\\med_forestplots.svg", med_plotgrid, height = 29.7, width = 21, units = "cm")
