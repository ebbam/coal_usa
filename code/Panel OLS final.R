library(conflicted)
library(tidyverse)
library(fixest)

conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(dplyr::lag)
# Loading our data --------------------------------------------------------

source(here("dicts.R"))
rep_data <- readRDS("rep_data.RDS")
rep_data <- read_excel(here("data/allcomp_final.xlsx"))

# using fixest ------------------------------------------------------------

rep_data %>% 
  select(year, fips, uer, active_mines, log_realgdp_pc) %>% 
  mutate(uer_lag1 = lag(uer, 1),
         uer_lag2 = lag(uer, 2),
         mines_lag1 = lag(active_mines, 1),
         mines_lag2 = lag(active_mines, 2),
         duer = uer - lag(uer, 1),
         dmines = active_mines - lag(active_mines, 1),
         dgdp = log_realgdp_pc - lag(log_realgdp_pc, 1), 
         .by = fips) %>% 
  drop_na() -> panel_ecm_data

# Estimate the ECM
ecm_model <- feols(duer ~ uer_lag1 + dmines + mines_lag1 + mines_lag2 + dgdp | fips + year, data = panel_ecm_data)

# Display the model summary
summary(ecm_model) %>% etable(tex = TRUE)


'Coefficient Interpretations

Lagged Dependent Variable (uer_lag1) Interpretation:

The coefficient of uer_lag1 is -0.252678, which is highly significant (p-value < 2.2e-16).
This negative coefficient indicates that approximately 25.27% of the deviation from the long-run equilibrium is corrected in each period. This suggests a fairly rapid adjustment towards equilibrium when there is a deviation.

Differenced Independent Variable (dmines) Interpretation:

The coefficient of dmines is -0.066292, which is highly significant (p-value = 4.8601e-14).
This negative coefficient indicates that a one-unit increase in the change of mines (Δmines) leads to a 0.0663 unit decrease in the change of uer (Δuer) in the short run. Thus, increases in mines are associated with short-run decreases in uer.

First Lag of Independent Variable (mines_lag1) Interpretation:

The coefficient of mines_lag1 is -0.025017, which is significant (p-value = 3.9057e-04).
This negative coefficient indicates that the first lag of mines has a negative long-run effect on uer. Specifically, a one-unit increase in mines from the previous period (t-1) is associated with a 0.025 unit decrease in uer in the current period.

Second Lag of Independent Variable (mines_lag2) Interpretation:

The coefficient of mines_lag2 is 0.031655, which is significant (p-value = 2.7527e-06).
This positive coefficient indicates that the second lag of mines has a positive long-run effect on uer. Specifically, a one-unit increase in mines from two periods ago (t-2) is associated with a 0.032 unit increase in uer in the current period.


Summary
Speed of Adjustment: The uer_lag1 coefficient of -0.252678 indicates that about 25.27% of any deviation from the long-run equilibrium is corrected each period.
Short-Run Dynamics:
dmines: A negative and significant coefficient (-0.066292) suggests that increases in mines reduce uer in the short run.
Long-Run Dynamics:
mines_lag1: A significant negative coefficient (-0.025017) indicates a negative long-run effect of mines lagged by one period on uer.
mines_lag2: A significant positive coefficient (0.031655) indicates a positive long-run effect of mines lagged by two periods on uer.



Combined Interpretation
Immediate Impact (Short-Run):

dmines: The immediate effect of a change in mining activities is to reduce the unemployment rate. This could be due to the direct creation of jobs and increased economic activities resulting from new mining projects or expansions.
Short-Term Lagged Effect:

mines_lag1: The positive effects of mining activities persist into the next period, continuing to reduce the unemployment rate. This suggests that the benefits of job creation and economic activity do not dissipate immediately but last into the near future.
Long-Term Lagged Effect:

mines_lag2: After two periods, the impact of increased mining activity reverses, leading to an increase in the unemployment rate. This could be due to several reasons:
Economic Adjustment: The initial boost from mining may lead to eventual layoffs or reduced hiring once initial projects are completed.
Environmental and Social Costs: Over time, negative externalities such as environmental degradation or health impacts may become apparent, reducing the long-term sustainability of the initial economic benefits.
Resource Depletion: Intensive mining activity might lead to resource depletion, which could negatively impact future mining prospects and related employment.
Relevance and Implications
Policy Implications:

Short-Term Benefits: Policymakers might leverage mining activities to achieve immediate reductions in unemployment, especially in regions with high unemployment rates.
Sustainability Concerns: Long-term planning must consider the potential adverse effects after the initial benefits fade. This includes addressing environmental concerns and ensuring that economic benefits are sustainable.
Diversification Strategies: To mitigate long-term negative effects, strategies to diversify the local economy and reduce dependency on mining might be necessary.
Economic Planning:

Job Creation: Short-term policies could focus on maximizing job creation and economic activities associated with new mining projects.
Long-Term Stability: Long-term economic policies should aim to address potential negative impacts and create a more balanced economic environment that does not solely rely on mining.
Conclusion
The coefficients for dmines, mines_lag1, and mines_lag2 provide a nuanced picture of how mining activities impact the unemployment rate over time. While mining activities have an immediate and short-term positive impact by reducing unemployment, there are potential negative long-term consequences that must be managed to ensure sustainable economic development.
'


# Pedroni Cointegration Test -----------------------------------------------------------------

library(pco)

new_ar <- array(dim = c(panel_ecm_data$year %>% unique %>% length,
                        panel_ecm_data$fips %>% unique %>% length,
                        5))
vars_pco <- c("duer","dmines","mines_lag1","mines_lag2","dgdp")

for(i in 1:length(vars_pco)){
  # i = 1
  panel_ecm_data %>% 
    select(year, fips, all_of(vars_pco[i])) %>% 
    arrange(year) %>% 
    pivot_wider(values_from = all_of(vars_pco[i]), names_from = fips) %>% 
    select(-year) %>% 
    as.matrix() -> to_be_added
  
  new_ar[,,i] <- to_be_added
}

# This is the pedroni test for cointegration (Pedroni 1999)
# these tests are described in Table 1 of the paper
# Null is no cointegration - reject for cointegration

# Mark and Sul in the paper use the "panel t-statistic" - this is probably tpanelpar    
# but why they choose that one is unclear
# we can plug the "standardised" column into a pnorm() and use the value for option "q"


pedroni_result <- pedroni99m(new_ar)


# see also Pedroni: 
# Under the alternative hypothesis, the panel variance statistic
# diverges to positive infinity, and consequently the right tail of the normal
# distribution is used to reject the null hypothesis. Consequently, for the panel
# variance statistic, large positive values imply that the null of no cointegration
# is rejected. For each of the other six test statistics, these diverge to
# negative infinity under the alternative hypothesis, and consequently the left
# tail of the normal distribution is used to reject the null hypothesis. Thus, for
# any of these latter tests, large negative values imply that the null of no
# cointegration is rejected.

# meaning: large positive values in tpanelvar --> reject null of no-cointegration
# meaning: if you have negative values, then 

# get the p-value of the test
pnorm(pedroni_result$STATISTIC[4,2])

# this is the only one we find evidence for cointegration for
pnorm(pedroni_result$STATISTIC[4,1])


# for the paper, we could use this table: 
pedroni_result$STATISTIC[1:4,] %>% 
  # we could change  use greek letters for the row.names
  kableExtra::kable() %>%
  kableExtra::kable_styling()




# can you create a ggplot showing the function that underlies pnorm()
ggplot() +
  stat_function(fun = pnorm, args = list(mean = 0, sd = 1), geom = "line") +
  geom_vline(xintercept = 1.96, linetype = "dashed") +
  geom_vline(xintercept = -1.96, linetype = "dashed") +
  theme_minimal() + 
  xlim(c(-5,5))

