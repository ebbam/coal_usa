#### Variable labels for latex output ####

setFixest_dict(c(emp = "Persons Employed in Coal", employed = "Employed Persons",
                 uer = "Unemployment Rate", active_mines = "Active Mines", 
                 p_inc_1000 = "County Personal Income (PI) in USD Millions", 
                 p_inc_X = "County Personal Income (PI) in USD Billions",
                 pop_1000 = "County Population (Thousands)", 
                 fips = "County FIPS Code", year = "Year",
                 lag_mines = "Active Mines (t-1)",  lag_mines2 = "Active Mines (t-2)",
                 lag_mines3 = "Active Mines (t-3)",lag_mines4 = "Active Mines (t-4)",
                 mines_diff = "Change Active Mines", lag_diff = "Change Active Mines (t-1)",
                 lag_diff2 = "Change Active Mines (t-2)", lag_diff3 = "Change Active Mines (t-3)",
                 diff_emp = "Change Persons Employed in Coal",
                 lag_diff_emp = "Change Persons Employed in Coal (t-1)",
                 lag_diff_emp2 = "Change Persons Employed in Coal (t-2)",
                 diff_employed = "Change Employed Persons", 
                 diff_uer = "Change Unemployment Rate",
                 green_emp = "Persons Employed in Green Sectors",
                 diff_green_emp = "Change Persons Employed in Green Sectors",
                 lag_emp = "Persons Employed in Coal (t-1)",
                 lag_emp2 = "Persons Employed in Coal (t-2)",
                 ff_emp = "Persons Employed in FF (not incl. Coal)",
                 diff_ff_emp = "Change Persons Employed in FF (not incl. Coal)",
                 lag_diff_ffemp = "Change Persons Employed in FF (not incl. Coal) (t-1)",
                 lag_diff_ffemp2 = "Change Persons Employed in FF (not incl. Coal) (t-2)",
                 diff_total_ff_emp = "Change Persons Employed in FF (incl. Coal)",
                 lag_diff_total_ffemp = "Change Persons Employed in FF (incl. Coal) (t-1)",
                 lag_diff_total_ffemp2 = "Change Persons Employed in FF (incl. Coal) (t-2)",
                 type_factor1 = "Type 1",  type_factor2 = "Type 2", type_factor3 = "Type 3",
                 mine_closure = "Mine Closed (t)", lag_closure = "Mine Closed (t-1)",
                 ruc_bin = "Rural",
                 REE_inv_scaled_pinc = "REE Investments (Proportion of County PI)",
                 logrealgdp_pc = "Real GDP per capita (log)",
                 diff_log_realgdp_pc = "Change in (log) Real GDP pc",
                 log_pop = "Population (log)",
                 log_realgdp = "Real GDP (log)",
                 diff_log_employed = "Change in Employed Persons (log)",
                 diff_log_unemployed = "Change in Unemployed Persons (log)",
                 diff_log_pop = "Change in Population (log)",
                 diff_log_lf = "Change in Labour Force (log)",
                 diff_log_realgdp = "Change in Real GDP (log)",
                 prod_diff = "Change in Production (short tons)",
                 lag_prod_diff = "Change in Production (short tons) (t-1)",
                 lag_prod_diff2 = "Change in Production (short tons) (t-2)"
))

sum_dict <- c("Active Mines (no.)","Unemployment Rate","Employed Persons", 
              "Unemployed Persons", "Labour Force", "Population", 
              "Total USDA RE Investments in USD","Coal Production (in short tons)",
              "Real GDP","Real GDP Per Capita", "Rural-Urban Code", 
              "Rural-Urban (binary)","Total TAA Allocation in USD",
              "USDA RE Investments scaled by Real GDP")

sum_dict_trans <-  c("Δ Unemployment Rate", "Δ Active Mines","Δ (log) Real GDP", 
                     "Δ (log) Real GDP per capita", "Δ (log) Employed Persons",
                     "Δ (log) Unemployed Persons", "Δ (log) Labour Force", "Δ (log) Population", 
                     "REE ≥ .1% of GDP")

cm <- c('lag_diff2' = 'Δ Active Mines (t-2)',
        'lag_diff' = 'Δ Active Mines (t-1)', 
        'mines_diff' = 'Δ Active Mines')

plot_dict = c("f(mines_diff,3)" = "-3", "f(mines_diff,2)" = "-2", "f(mines_diff, 1)" = "-1", 
              "mines_diff" = "0", "lag_diff" = "+1", "lag_diff2" = "+2", "lag_diff3" = "+3")