# The following script compiles the production and state productive capacity data and combiens with main econometrics input data
# this dataset is called in the robustness_checks.Rmd script
# author: Ebba Mark

# Libraries
library(tidyverse)
library(readxl)
library(here)
library(openxlsx)
library(gdata)
library(janitor)
library(tidycensus)
library(zoo)
library(splm)
library(spdep)
library(modelsummary)
library(kableExtra)
library(conflicted)
library(fixest)
library(plm)
library(zoo)
conflict_prefer_all("dplyr", quiet = TRUE)
setwd(here("replication_code"))

## Production and Capacity

### Production
# Clean production data
clean_prod <- function(df){
  return(
    df %>% tibble %>% 
      clean_names %>% 
      select(mine_state, mine_county, mine_type, mine_status, operation_type, union_code, production_short_tons, average_employees, labor_hours) %>% 
      mutate(across(production_short_tons:labor_hours, ~as.numeric(gsub(",", "", ., fixed = TRUE))),
             active = ifelse(mine_status %in% c("Active","Active, men working, not producing", "Active, men not working, not producing"), "active", "inactive")) %>% 
      group_by(mine_state, mine_county, mine_type, active) %>% 
      summarise(n = n(), prod = sum(production_short_tons, na.rm = TRUE)/1000, avg_emp = sum(average_employees, na.rm = TRUE), labour_hours = sum(labor_hours, na.rm = TRUE), n_unionised = sum(!is.na(union_code))) %>% 
      ungroup %>% 
      mutate(mine_type = tolower(mine_type)) %>% 
      filter(mine_type != "refuse") %>% 
      arrange(mine_county, mine_state, active) %>% 
      pivot_wider(id_cols = c(mine_county, mine_state), names_from = c(mine_type, active), names_glue = "{mine_type}_{active}_{.value}", values_from = c(n, prod, avg_emp, labour_hours, n_unionised)) %>% 
      mutate(across(surface_active_n:underground_inactive_n_unionised, ~replace(., is.na(.), 0)),
             total_active_n = surface_active_n + underground_active_n,
             total_active_prod = surface_active_prod + underground_active_prod,
             surface_n = rowSums(across(c(surface_active_n, surface_inactive_n)), na.rm = TRUE),
             surface_prod = surface_active_prod + surface_inactive_prod,
             underground_n = underground_active_n + underground_inactive_n,
             underground_prod = underground_active_prod + underground_inactive_prod,
             total_n = surface_n + underground_n,
             total_prod = surface_prod + underground_prod,
             across(surface_active_n:total_prod, ~round(.))) %>%
      rename(county = mine_county,
             state = mine_state) %>%
      select(state, county, underground_active_n, underground_active_prod, surface_active_n, surface_active_prod, total_active_n, total_active_prod, surface_n, surface_prod, underground_n, underground_prod, total_n, total_prod) %>%
      mutate(state = gsub(" \\(Anthracite\\)| \\(Bituminous\\)| \\(Northern\\)| \\(Southern\\)| \\(East\\)| \\(West\\)", "", state)) %>%
      arrange(state, county) %>%
      mutate(year = yr,
             county = case_when(state == "Louisiana" ~ paste0(county, " Parish"),
                                TRUE ~ paste0(county, " County"))) %>%
      filter(state != "Alaska") %>%
      relocate(year)
  )
}

full_prod <- tibble()
for(yr in 2001:2020){
  print(yr)
  # In years 2005, 2006, 2011, 2012, 2018, 2019 various observations eitehr have trailing quotation marks or other formatting issues. Static files downloaded. Otherwise, data is downloaded directl y from the EIA.
  if(yr %in% c(2005,2006,2011,2012,2018,2019)){
    print("entered manual")
    full_prod <- read.xls(paste0("data/production_capacity/coalpublic", as.character(yr), ".xls"), skip = 2) %>%
      clean_prod %>% 
      rbind(full_prod, .)
  }else{
    url <- paste0("https://www.eia.gov/coal/data/public/xls/coalpublic", as.character(yr), ".xls")
    full_prod <- read.xls(url, skip = 2) %>%
      clean_prod %>% 
      rbind(full_prod, .)}
}

# Requires renaming of various counties in order to assign appropriate FIPS codes
rm(fips_codes)
fips_codes <- fips_codes %>% 
  mutate(fips = paste0(state_code, county_code)) %>%
  select(state_name, county, fips) %>%
  rename(state = state_name) %>%
  mutate(county = case_when(county == "DeKalb County" ~ "De Kalb County",
                            county == "McCreary County" ~ "Mccreary County",
                            county == "McDowell County" ~ "Mcdowell County",
                            county == "McDonough County" ~ "Mcdonough County",
                            county == "McLean County" ~ "Mclean County",
                            county == "McKinley County" ~ "Mckinley County",
                            TRUE ~ county))

full_prod <- full_prod %>% 
  mutate(county = case_when(county == "Bighorn County" ~ "Big Horn County",
                            county == "Athans County" ~ "Athens County",
                            county == "Clairborne County" ~ "Claiborne County",
                            TRUE ~ county),
         state = case_when(county == "Monongalia County" ~ "West Virginia",
                           TRUE ~ state)) %>%
  group_by(year, state, county) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  ungroup %>%
  left_join(., fips_codes, by = c("state", "county")) %>% 
  mutate(fips = ifelse(fips == "51195", "51955", fips)) %>% 
  filter(year <= 2019) %>% 
  complete(fips, year) %>% 
  group_by(fips) %>% 
  mutate(across(c("total_active_prod", "total_prod"), .fns = list(diff = ~c(0,diff(.))), .names = "{.fn}_{.col}")) %>%
  mutate(across(contains("diff"), .fns = list(l1 = ~lag(.x, 1), l2 = ~lag(.x, 2)), .names = "{.fn}_{.col}")) %>% 
  ungroup

### Capacity
# State productive capacity data
cap <- read.csv('data/production_capacity/Productive_capacity.csv', skip = 4) %>% 
  tibble %>%
  slice(-1) %>% 
  select(-source.key, -units) %>% 
  rename(state = description) %>% 
  mutate(state = gsub("Total : ", "", state),
         across(X2001:X2022, as.numeric)) %>%
  filter(!(state %in% c("Total", "Middle Atlantic", "East North Central", 
                        "West North Central", "South Atlantic", "East South Central", 
                        "West South Central", "Pacific Contiguous", "Pacific Noncontiguous", "Mountain"))) %>% 
  pivot_longer(cols = !state, names_to = "year", values_to = "prod_cap_state", names_transform = list(year = ~as.numeric(gsub("X","", .)))) %>% 
  mutate(prod_cap_state = round(prod_cap_state/1000)) %>% arrange(state) %>% filter(state != "Alaska") %>% 
  group_by(state) %>% 
  mutate(diff_prod_cap_state = lag(prod_cap_state) - prod_cap_state) %>% 
  ungroup


prod_cap <- full_prod %>% 
  group_by(year, state) %>%
  mutate(total_state_prod = sum(total_prod, na.rm = TRUE)) %>% 
  ungroup %>% 
  mutate(prop_prod = total_prod/total_state_prod) %>% 
  select(year, state, fips, total_prod, prop_prod, total_state_prod) %>%
  group_by(fips) %>% 
  fill(state, .direction = "updown") %>% 
  ungroup %>% 
  left_join(., cap, by = c("state", "year")) %>% 
  mutate(prod_cap = prop_prod*prod_cap_state,
         diff_prod_cap = prop_prod*diff_prod_cap_state) %>% 
  complete(fips, year) %>% 
  group_by(fips) %>% 
  mutate(across(c("prod_cap", "diff_prod_cap"), .fns = list(l1 = ~lag(.x, 1), l2 = ~lag(.x, 2)), .names = "{.fn}_{.col}")) %>% 
  ungroup %>% 
  select(year, fips, prop_prod, total_state_prod, prod_cap_state, diff_prod_cap_state, prod_cap, diff_prod_cap, contains("l1"), contains("l2"))

# Combine with data from main econometrics specifications
allcomp <- read_excel("data/Econometrics_Final.xlsx") %>% 
  left_join(., full_prod, by = c("fips", "year")) %>% 
  left_join(., prod_cap, by = c("fips", "year")) %>% 
  mutate(across(diff_total_active_prod:l2_diff_total_prod, ~ifelse(is.na(.x), 0, .x)/1000),
         across(c(diff_prod_cap, l1_diff_prod_cap, l2_diff_prod_cap), ~ifelse(is.na(.x), 0, .x)/1000)) %>% 
  mutate(active_prod_diff = as.logical(abs(mines_diff))*prod_diff,
         active_lag_prod_diff = as.logical(abs(lag_diff))*lag_prod_diff,
         active_lag_prod_diff2 = as.logical(abs(lag_diff2))*lag_prod_diff) %>% 
  mutate(across(c(production_shorttons, prod_diff, lag_prod_diff, lag_prod_diff2, active_prod_diff, active_lag_prod_diff, active_lag_prod_diff2), ~ifelse(is.na(.x), 0, .x)/1000000)) %>% 
  mutate(diff_prod_cap2 = ifelse(sign(mines_diff) == sign(diff_prod_cap_state), diff_prod_cap_state*prop_prod, 0)) %>% 
  group_by(fips) %>% 
  mutate(across(c("diff_prod_cap2"), .fns = list(l1 = ~lag(.x, 1), l2 = ~lag(.x, 2)), .names = "{.fn}_{.col}"),
         across(c(diff_prod_cap2, l1_diff_prod_cap2, l2_diff_prod_cap2), ~ifelse(is.na(.x), 0, .x)/1000)) %>% 
  ungroup %>% 
  group_by(fips) %>% 
  mutate(across(c("surface_active_n", "underground_active_n"), .fns = list(diff = ~c(0,diff(.))), .names = "{.fn}_{.col}")) %>%
  mutate(across(c("surface_active_n", "underground_active_n", "diff_surface_active_n", "diff_underground_active_n"), .fns = list(l1 = ~lag(.x, 1), l2 = ~lag(.x, 2)), .names = "{.fn}_{.col}")) %>% 
  ungroup %>% 
  mutate(across(c(surface_active_n, 
                  diff_surface_active_n,
                  l1_surface_active_n,
                  l2_surface_active_n,
                  l1_diff_surface_active_n,
                  l2_diff_surface_active_n,
                  underground_active_n,
                  diff_underground_active_n,
                  l1_underground_active_n,
                  l2_underground_active_n,
                  l1_diff_underground_active_n,
                  l2_diff_underground_active_n),~ifelse(is.na(.x), 0, .x))) %>% 
  ungroup

#allcomp %>% saveRDS("data/production_capacity/allcomp_cap_prod.RDS")
