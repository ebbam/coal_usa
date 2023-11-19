library(tidyverse)
library(readxl)
library(here)
library(openxlsx)
library(gdata)
library(janitor)

read.xls("https://www.eia.gov/coal/data/public/xls/coalpublic2002.xls", skip = 2) %>% 
  tail

temp_2022 <- read.xlsx(here("data/production_coal/coalpublic2022.xlsx"), startRow = 3) %>% 
  select(-c(MSHA.ID, Mine.Name, Company.Type, Operating.Company, Operating.Company.Address, Coal.Supply.Region)) %>% 
  tibble %>% 
  clean_names %>% 
  filter(operation_type != "Preparation Plant") %>%
  group_by(mine_state, mine_county, mine_type) %>% 
  summarise(n = n(), prod = sum(production_short_tons, na.rm = TRUE)/1000, avg_emp = sum(average_employees, na.rm = TRUE), labour_hours = sum(labor_hours, na.rm = TRUE), n_unionised = sum(!is.na(union_code))) %>% 
  ungroup %>% 
  mutate(mine_type = tolower(mine_type)) %>% 
  filter(mine_type != "refuse") %>% 
  pivot_wider(id_cols = c(mine_county, mine_state), names_from = mine_type, names_glue = "{mine_type}_{.value}", values_from = c(n, prod, avg_emp, labour_hours, n_unionised)) %>% 
  mutate(across(surface_n:underground_n_unionised, ~replace(., is.na(.), 0)), 
         total_n = surface_n + underground_n,
         total_prod = surface_prod + underground_prod,
         across(surface_n:total_prod, ~round(.))) %>% 
  rename(County = mine_county,
         State = mine_state) %>% 
  select(State, County, underground_n, underground_prod, surface_n, surface_prod, total_n, total_prod) %>% 
  mutate(State = gsub(" \\(Anthracite\\)| \\(Bituminous\\)| \\(Northern\\)| \\(Southern\\)| \\(East\\)| \\(West\\)", "", State)) %>% 
  arrange(State, County)

read.xlsx(here("data/production_coal/table2.xlsx"), startRow = 3) %>% 
  tibble %>% 
  mutate(across(!c(State, County), ~as.numeric(.)),
         County = trimws(County)) %>% 
  filter(State != County) %>% arrange(State, County) %>% 
  mutate(across(underground_n:total_prod, ~replace(., is.na(.), 0))) %>% slice(-84) %>% 
  identical(slice(temp_2022, -84))

https://www.eia.gov/coal/data/public/xls/coalpublic2022.xls

read.xls("https://www.eia.gov/coal/data/public/xls/coalpublic2022.xls", skip = 2)

for(yr in 2002:2020){
  print(yr)
  url <- paste0("https://www.eia.gov/coal/data/public/xls/coalpublic", as.character(yr), ".xls")
  read.xls(url, skip = 2) %>%
    tibble %>% 
    clean_names %>% 
    select(mine_state, mine_county, mine_type, operation_type, union_code, production_short_tons, average_employees, labor_hours) %>% 
    mutate(across(production_short_tons:labor_hours, ~as.numeric(.))) %>% 
    filter(operation_type != "Preparation Plant") %>%
    group_by(mine_state, mine_county, mine_type) %>% 
    summarise(n = n(), prod = sum(production_short_tons, na.rm = TRUE)/1000, avg_emp = sum(average_employees, na.rm = TRUE), labour_hours = sum(labor_hours, na.rm = TRUE), n_unionised = sum(!is.na(union_code))) %>% 
    ungroup %>% 
    mutate(mine_type = tolower(mine_type)) %>% 
    filter(mine_type != "refuse") %>% 
    pivot_wider(id_cols = c(mine_county, mine_state), names_from = mine_type, names_glue = "{mine_type}_{.value}", values_from = c(n, prod, avg_emp, labour_hours, n_unionised)) %>% 
    mutate(across(surface_n:underground_n_unionised, ~replace(., is.na(.), 0)), 
           total_n = surface_n + underground_n,
           total_prod = surface_prod + underground_prod,
           across(surface_n:total_prod, ~round(.))) %>% 
    rename(County = mine_county,
           State = mine_state) %>% 
    select(State, County, underground_n, underground_prod, surface_n, surface_prod, total_n, total_prod) %>% 
    mutate(State = gsub(" \\(Anthracite\\)| \\(Bituminous\\)| \\(Northern\\)| \\(Southern\\)| \\(East\\)| \\(West\\)", "", State)) %>% 
    arrange(State, County)
}


