### Absolute Job Loss Calculations #####
library(tidyverse)
library(readxl)
library(here)
library(openxlsx)
library(modelsummary)
library(conflicted)
library(fixest)
library(multcomp)
library(kableExtra)
conflict_prefer_all("dplyr", quiet = TRUE)

#### Data
allcomp_final <- read_excel(here("data/allcomp_final.xlsx")) %>% 
    mutate(pos_lag_diff = ifelse(lag_diff >= 0, 1, 0),
           pos_lag_diff2 = ifelse(lag_diff2 >= 0, 1, 0),
           neg_lag_diff = ifelse(lag_diff < 0, 1, 0),
           neg_lag_diff2 = ifelse(lag_diff2 < 0, 1, 0))

# Create subset of coal counties (CC) only (with active mines at some point)
cclist <- unique(allcomp_final$fips[which(allcomp_final$active_mines != 0)])
allcomp_cc <- subset(allcomp_final, fips %in% cclist)

FE_diffuer <- as.formula("diff_uer ~ mines_diff + lag_diff + lag_diff2 + diff_log_realgdp_pc | fips + year")
model_mines <- feols(FE_diffuer, allcomp_final, se = "twoway")
model_neg= feols(diff_uer ~ mines_diff + neg_diff:mines_diff + 
                   lag_diff + neg_lag_diff:lag_diff +
                   lag_diff2 + neg_lag_diff2:lag_diff2 + 
                   diff_log_realgdp_pc | 
                   fips + year, allcomp_final, se = 'twoway')

model_pos = feols(diff_uer ~ mines_diff + pos_diff:mines_diff + 
                    lag_diff + pos_lag_diff:lag_diff + 
                    lag_diff2 + pos_lag_diff2:lag_diff2 + 
                    diff_log_realgdp_pc | 
                    fips + year, allcomp_final, se = 'twoway')

model_9_both = feols(diff_uer ~ neg_diff:mines_diff + neg_lag_diff:lag_diff + neg_lag_diff2:lag_diff2 + pos_diff:mines_diff + pos_lag_diff:lag_diff + pos_lag_diff2:lag_diff2 +  diff_log_realgdp_pc | fips + year, allcomp_final, se = 'twoway')

#glht(model_9_both, linfct = "neg_diff:mines_diff + neg_lag_diff:lag_diff + neg_lag_diff2:lag_diff2 = 0") %>% summary
#glht(model_9_both, linfct = "mines_diff:pos_diff + lag_diff:pos_lag_diff + lag_diff2:pos_lag_diff2 = 0") %>% summary
#glht(model_9_both, linfct = "neg_diff:mines_diff + neg_lag_diff:lag_diff + neg_lag_diff2:lag_diff2 + mines_diff:pos_diff + lag_diff:pos_lag_diff + lag_diff2:pos_lag_diff2 = 0") %>% summary


### 1. Absolute coal job loss numbers
coef_all <- model_mines$coefficients[1]/100 %>% unname
coef_neg <- model_9_both$coefficients['neg_diff:mines_diff']/100 %>% unname
coef_pos <- model_9_both$coefficients['mines_diff:pos_diff']/100 %>% unname

# Calculation of job loss estimates per county
job_loss_ests <- allcomp_final %>% 
  select(fips, year, mines_diff, employed, unemployed, labour_force, neg_diff, pos_diff) %>% 
  mutate(ue_effect_neg = ifelse(sign(mines_diff) == -1, labour_force*coef_all*mines_diff, 0), 
         ue_effect_pos = ifelse(sign(mines_diff) == 1, labour_force*coef_all*mines_diff, 0), 
         asymm_effect_neg = ifelse(sign(mines_diff) == -1, labour_force*coef_neg*mines_diff, 0),
         asymm_effect_pos = ifelse(sign(mines_diff) == 1, labour_force*coef_pos*mines_diff, 0),
         ue_effect_net = ue_effect_neg + ue_effect_pos,
         asymm_effect_net = asymm_effect_neg + asymm_effect_pos,
         ue_effect_neg_single = ifelse(sign(mines_diff) == -1, labour_force*coef_all*sign(mines_diff), 0), 
         ue_effect_pos_single = ifelse(sign(mines_diff) == 1, labour_force*coef_all, 0), 
         asymm_effect_neg_single = ifelse(sign(mines_diff) == -1, labour_force*coef_neg*sign(mines_diff), 0),
         asymm_effect_pos_single = ifelse(sign(mines_diff) == 1, labour_force*coef_pos, 0)) 

total_absolute <- job_loss_ests %>% 
  group_by(year) %>% 
  summarise(across(c(mines_diff, ue_effect_neg, ue_effect_pos, ue_effect_net,  asymm_effect_neg, asymm_effect_pos, asymm_effect_net), ~sum(., na.rm = TRUE))) %>% 
  ungroup

# Total job loss estimates on annual basis
total_absolute %>% 
  select(year, contains ("ue")) %>% 
  pivot_longer(!year) %>% 
  ggplot(aes(x = year, y = value, colour = name)) + 
  geom_line()

total_absolute %>% 
  #select(year, mines_diff, contains("net")) %>% 
  mutate(across(ue_effect_neg:asymm_effect_net, ~round(., digits = -2))) %>% 
  kable(format = "latex")

total_absolute %>% 
  summarise(across(mines_diff:asymm_effect_net, ~round(sum(., na.rm = TRUE), digits = -2))) %>% 
  kable(format = "latex")

# Calculation of job loss estimates per county
job_loss_ests %>% 
  select(fips, ue_effect_neg, ue_effect_pos, asymm_effect_neg, asymm_effect_pos) %>% 
  pivot_longer(!fips) %>% 
  mutate(grouping = ifelse(grepl( "asymm", name), "Asymmetric Effect", "Absolute Effect"),
         Key = case_when(name == "ue_effect_neg" ~ "Closure(s)", 
                                name == "ue_effect_pos" ~ "Opening(s)", 
                                name == "asymm_effect_neg" ~ "Closure(s) - Asymmetric Effect", 
                                name == "asymm_effect_pos" ~ "Opening(s) - Asymmetric Effect"),
         Key = factor(Key, levels = c("Closure(s)",  "Opening(s)", "Closure(s) - Asymmetric Effect", "Opening(s) - Asymmetric Effect"))) %>% 
  filter(value != 0) %>% 
  ggplot() +
  geom_histogram(aes(x = value, fill = Key), bins = 100, alpha = 0.9) +
  geom_vline(aes(xintercept = mean(value) + sd(value)), linetype = "dotted") +
  geom_vline(aes(xintercept = mean(value) - sd(value)), linetype = "dotted") +
  #geom_abline(v= mean(value), lty=1, lwd=3,col="blue") +
  #abline(v= mean.weight - (sd.weight), lty=2, lwd=1, col="red")
  #abline(v= mean.weight + (sd.weight), lty=2, lwd=1, col="red") 
  #geom_density(aes(x = value, fill = name)) +
  facet_wrap(~grouping, ncol = 1) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  guides(fill = guide_legend(ncol = 2)) +
  labs(x = "Change in Number of Persons Unemployed", y = "Number of Observations") +
  scale_fill_brewer(palette = "Set1", direction = -1)

ggsave(filename = here("output/job_loss_effects.jpg"), width = 10, height = 7, units = "in")
   #scale_y_continuous(trans='log10')

# Calculation of job loss estimates per county for a single mine closure
job_loss_ests %>% 
  select(fips, contains("single")) %>% 
  pivot_longer(!fips) %>% 
  mutate(grouping = ifelse(grepl( "asymm", name), "Asymmetric Effect", "Absolute Effect"),
         Key = case_when(name == "ue_effect_neg_single" ~ "Single Closure",
                         name == "ue_effect_pos_single" ~ "Single Opening",
                         name == "asymm_effect_neg_single" ~ "Single Closure - Asymmetric Effect", 
                         name == "asymm_effect_pos_single" ~ "Single Opening - Asymmetric Effect"),
         Key = factor(Key, levels = c("Single Opening", "Single Closure",  "Single Opening - Asymmetric Effect", "Single Closure - Asymmetric Effect"))) %>% 
  filter(value != 0) %>% 
  ggplot() +
  geom_histogram(aes(x = value, fill = Key), bins = 100) +
  geom_vline(aes(xintercept = mean(value) + sd(value)), linetype = "dotted") +
  geom_vline(aes(xintercept = mean(value) - sd(value)), linetype = "dotted") +
  #geom_abline(v= mean(value), lty=1, lwd=3,col="blue") +
  #abline(v= mean.weight - (sd.weight), lty=2, lwd=1, col="red")
  #abline(v= mean.weight + (sd.weight), lty=2, lwd=1, col="red") 
  #geom_density(aes(x = value, fill = name)) +
  facet_wrap(~grouping, ncol = 1) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(x = "Change in Number of Persons Unemployed", y = "Number of Observations") + 
  scale_fill_brewer(palette = "Set2")

ggsave(here("output/job_loss_effects_single.jpg"), width = 10, height = 7, units = "in")

summary_stats <- job_loss_ests %>% 
  select(fips, contains("effect_")) %>% 
  mutate(across(everything(), ~ifelse(. == 0, NA, .))) %>% 
  mutate(across(contains("pos"), ~.*-1))

summary_stats %>% 
  select(!contains("asymm")) %>% 
  data.frame %>% 
  stargazer(type = "latex",
            covariate.labels = c("Detected Effect of Closure(s)",
                                 "Detected Effect of Opening(s)",
                                 "Detected Effect of Single Closure",
                                 "Detected Effect of Single Opening"),
            title = "Detected Treatment Effects", digits = 0)

summary_stats %>% 
    select(contains("asymm")) %>% 
    data.frame %>% 
    stargazer(type = "latex",
    covariate.labels = c("Detected Effect of Closure(s)",
            "Detected Effect of Opening(s)",
            "Detected Effect of Single Closure",
            "Detected Effect of Single Opening"),
            title = "Detected Treatment Effects using Asymmetric Treatment Coefficients", digits = 0)

# # In logs!
# job_loss_ests %>%
#   select(fips, ue_effect_neg, ue_effect_pos, asymm_effect_neg, asymm_effect_pos) %>%
#   pivot_longer(!fips) %>%
#   mutate(grouping = ifelse(grepl("neg", name), "Closures", "Openings"),
#          Key = case_when(name == "ue_effect_neg" ~ "Closure(s)",
#                          name == "ue_effect_pos" ~ "Opening(s)",
#                          name == "asymm_effect_neg" ~ "Closure(s) - Asymmetric Effect",
#                          name == "asymm_effect_pos" ~ "Opening(s) - Asymmetric Effect"),
#          Key = factor(Key, levels = c("Closure(s)",  "Closure(s) - Asymmetric Effect", "Opening(s)", "Opening(s) - Asymmetric Effect")),
#          value = log(abs(value) + 1)*sign(value)) %>%
#   filter(value != 0) %>%
#   ggplot() +
#   geom_histogram(aes(x = value, fill = Key), bins = 50) +
#   geom_vline(aes(xintercept = 0), linetype = "dotted", alpha = 0.5) +
#   #geom_vline(aes(xintercept = mean(value) - sd(value)), linetype = "dotted") +
#   #geom_abline(v= mean(value), lty=1, lwd=3,col="blue") +
#   #abline(v= mean.weight - (sd.weight), lty=2, lwd=1, col="red")
#   #abline(v= mean.weight + (sd.weight), lty=2, lwd=1, col="red")
#   #geom_density(aes(x = value, fill = name)) +
#   facet_wrap(~grouping, ncol = 1) +
#   theme_minimal() +
#   theme(legend.position = "bottom", legend.title = element_blank()) +
#   guides(fill = guide_legend(ncol = 2)) +
#   labs(x = "Change in Number of Persons Unemployed", y = "Number of Counties") +
#   scale_fill_brewer(palette = "Set2")
# 
# ggsave(here("output/job_loss_effects_log.jpg"), width = 10, height = 12, units = "in")
# 
# job_loss_ests %>%
#   select(fips, contains("single")) %>%
#   pivot_longer(!fips) %>%
#   mutate(grouping = ifelse(grepl("pos", name), "Openings", "Closures"),
#          Key = case_when(name == "ue_effect_neg_single" ~ "Closure(s)",
#                          name == "ue_effect_pos_single" ~ "Opening(s)",
#                          name == "asymm_effect_neg_single" ~ "Closure(s) - Asymmetric Effect",
#                          name == "asymm_effect_pos_single" ~ "Opening(s) - Asymmetric Effect"),
#          Key = factor(Key, levels = c("Closure(s)",  "Closure(s) - Asymmetric Effect", "Opening(s)", "Opening(s) - Asymmetric Effect")), 
#          value = log(abs(value) + 1)*sign(value)) %>%
#   filter(value != 0) %>%
#   ggplot() +
#   geom_histogram(aes(x = value, fill = Key), bins = 50) +
#   #geom_vline(aes(xintercept = mean(value) + sd(value)), linetype = "dotted") +
#   #geom_vline(aes(xintercept = mean(value) - sd(value)), linetype = "dotted") +
#   #geom_abline(v= mean(value), lty=1, lwd=3,col="blue") +
#   #abline(v= mean.weight - (sd.weight), lty=2, lwd=1, col="red")
#   #abline(v= mean.weight + (sd.weight), lty=2, lwd=1, col="red")
#   #geom_density(aes(x = value, fill = name)) +
#   facet_wrap(~grouping, ncol = 1, scales = "free_x") +
#   theme_minimal() +
#   theme(legend.position = "bottom", legend.title = element_blank()) +
#   guides(fill = guide_legend(ncol = 2)) +
#   labs(x = "Change in Number of Persons Unemployed", y = "Number of Counties") +
#   scale_fill_brewer(palette = "Set2")
# 
# 
# ggsave(here("output/job_loss_effects_log_single.jpg"), width = 10, height = 12, units = "in")

### 2. Persistent effects? (lght)
for(sample in list(allcomp_final, allcomp_cc)){
  model_mines <- feols(FE_diffuer, sample, se = 'twoway')
  
  model_neg= feols(diff_uer ~ mines_diff + neg_diff:mines_diff + 
                     lag_diff + neg_lag_diff:lag_diff +
                     lag_diff2 + neg_lag_diff2:lag_diff2 + 
                     diff_log_realgdp_pc | 
                     fips + year, sample, se = 'twoway')
  
  model_pos = feols(diff_uer ~ mines_diff + pos_diff:mines_diff + 
                      lag_diff + pos_lag_diff:lag_diff + 
                      lag_diff2 + pos_lag_diff2:lag_diff2 + 
                      diff_log_realgdp_pc | 
                      fips + year, sample, se = 'twoway')
  
  model_list <- list()
  hyps <- data.frame()
  for(k in c("mines", "neg", "pos")){
    temp_mod <- eval(parse(text = paste0("model_", k)))
    temp_names <- names(temp_mod$coefficients)[!(names(temp_mod$coefficients) == "diff_log_realgdp_pc")]
    coef_ht = temp_names[!grepl("lag", temp_names)] %>% paste(., collapse = " + ") %>% paste(., " = 0") 
    persistence_ht = temp_names %>% paste(., collapse = " + ") %>% paste(., " = 0") 
    
    coef_test = glht(temp_mod, linfct = coef_ht)
    persistence_test = glht(temp_mod, linfct = persistence_ht)
    print(coef_test)
    print(persistence_test)
    
    model_list <- c(model_list, list(coef_test), list(persistence_test))
    hyps <- rbind(hyps, 
                  c("Contemporaneous Effect", coef_ht), 
                  c("SE", NA),
                  c("p-value", NA),
                  c("Persistent Effect", persistence_ht),
                  c("SE", NA),
                  c("p-value", NA))
  }
  
  rename_vector <- c("","","","","","") 
  colnames(hyps) <- c("Test", "Null Hypothesis")
  hyps <- hyps %>% 
    mutate(test = gsub("mines_diff ", "Beta_{1} ", `Null Hypothesis`),
           test = gsub(' lag_diff ', " Beta_{2} ", test),
           test = gsub(' lag_diff2 ', " Beta_{3} ", test),
           test = gsub("mines_diff:neg_diff", "Beta_{4}", test),
           test = gsub("lag_diff:neg_lag_diff", "Beta_{5}", test),
           test = gsub("lag_diff2:neg_lag_diff2", "Beta_{6}", test),
           test = gsub("mines_diff:pos_diff", "Beta_{7}", test),
           test = gsub("lag_diff:pos_lag_diff", "Beta_{8}", test),
           test = gsub("lag_diff2:pos_lag_diff2", "Beta_{9}", test)) %>% 
    select(-`Null Hypothesis`) %>% 
    rename(`Null Hypothesis` = test)
  attr(hyps, 'position') <- c(1, 2)
  model_list %>% modelsummary(stars= TRUE, statistic = c("std.error", "p.value"), coef_rename =  rename_vector, add_columns = hyps, type = "latex") print
}



