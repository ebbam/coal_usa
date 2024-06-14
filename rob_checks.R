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

# DATA CLEANING ###

# Clean production data
clean_prod <- function(df){
  return(
    df %>% tibble %>% 
    clean_names %>% 
    select(mine_state, mine_county, mine_type, mine_status, operation_type, union_code, production_short_tons, average_employees, labor_hours) %>% 
    mutate(across(production_short_tons:labor_hours, ~as.numeric(gsub(",", "", ., fixed = TRUE))),
           active = ifelse(mine_status %in% c("Active","Active, men working, not producing", "Active, men not working, not producing"), "active", "inactive")) %>% 
    #filter(!(operation_type %in% c("Preparation Plant", ""))) %>%
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
  # In years 2005, 2006, 2011 - two mine observations have a trailing quotation mark - this has been rectified in static files
  # In 2012, there is no column recorded for region and tehrefore needs to be added manually
  if(yr %in% c(2005,2006,2011,2012,2018,2019)){
    print("entered manual")
    full_prod <- read.xls(here(paste0("data/coalpublic", as.character(yr), ".xls")), skip = 2) %>%
      clean_prod %>% 
      rbind(full_prod, .)
  }else{
    url <- paste0("https://www.eia.gov/coal/data/public/xls/coalpublic", as.character(yr), ".xls")
    full_prod <- read.xls(url, skip = 2) %>%
    clean_prod %>% 
    rbind(full_prod, .)}
}

# test <- list()
# for(yr in 2001:2020){
#   print(yr)
#   if(yr %in% c(2005, 2006, 2011, 2012)){
#     test <- read.xls(here(paste0("data/coalpublic", as.character(yr), ".xls")), skip = 2) %>% pull(Mine.Status) %>% unique %>% c(test, .)
#   }else{
#     url <- paste0("https://www.eia.gov/coal/data/public/xls/coalpublic", as.character(yr), ".xls")
#     test <- read.xls(url, skip = 2) %>% pull(Mine.Status) %>% unique %>% c(test, .)}
# }


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

cap <- read.csv(here('data/production_coal/Productive_capacity.csv'), skip = 4) %>% 
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

# RUNS ###
# Dataset of relevant variables for analysis
allcomp <- read_excel(here("data/allcomp_final.xlsx")) %>% 
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

allcomp %>% mutate(diff = active_mines - total_active_n) %>% ggplot() + geom_histogram(aes(diff))

#allcomp %>% saveRDS(here("data/allcomp_cap_prod.RDS"))

# Unmodified lookup table to adjust FIPS codes between various standards
bea_fips_mods <- read_excel(here("data/FIPSModificationsVA.xlsx"), skip = 1, col_types = c("text", "text","text","text"))

# Create subset of coal counties (CC) only (with active mines at some point)
cclist <- unique(allcomp$fips[which(allcomp$active_mines != 0)])
allcomp_cc <- subset(allcomp, fips %in% cclist)

# Dataset including the "type" of county as found in the Typology work (Typology Work_New.Rmd)
# Merged with CC dataset
cc_cluster <- read_excel(here("data/cc_clusters_251.xlsx"))
allcomp_cc_types <- merge(allcomp_cc, cc_cluster, by = "fips", all.x = TRUE)

# Load required functions and dictionaries
source("useful_functions.R")
source("dicts.R")
source("ref_lists.R")

# Create adjacency matrix for spatial model
##############################################################
# Import adjacency matrix from US Census Bureau: 
# https://www.census.gov/programs-surveys/geography/library/reference/county-adjacency-file.html
xmat1 <- read.csv("data/county_adjacency.txt", sep="\t", stringsAsFactors = FALSE, header = FALSE)

# Retain only columns with FIPS codes of each county (V2) and its neighbors (V4)
xmat1 <- xmat1[,c("V2","V4")]

# Replace NA values in V2 such that each row is a pair of neighbors
xmat1$V2 <- na.locf(xmat1$V2)

# Convert from integer to character fips code for appropriate matching
xmat1$V2 <- lapply(xmat1$V2, function(x) sapply(x, fips_format))
xmat1$V4 <- lapply(xmat1$V4, function(x) sapply(x, fips_format))

# Create lookup table to convert VA FIPS codes
getfips <- bea_fips_mods$`BEA FIPS`
names(getfips) <- bea_fips_mods$FIPS

xmat1[xmat1 == "46113"] <- "46102"
xmat1[xmat1 == "51515"] <- "51019"
xmat1$V2 <- lapply(xmat1$V2, function(x) ifelse(x %in% bea_fips_mods$FIPS, getfips[x], x))
xmat1$V4 <- lapply(xmat1$V4, function(x) ifelse(x %in% bea_fips_mods$FIPS, getfips[x], x))

# Removes Alaska, Hawaii, DC, PR, Guam, American Samoa
xmat1 <- subset(xmat1, substr(V2, 1, 2) != "60" & 
                  substr(V2, 1, 2) != "66" &
                  substr(V2, 1, 2) != "69" &
                  substr(V2, 1, 2) != "72" &
                  substr(V2, 1, 2) != "78" &
                  substr(V2, 1, 2) != "15" &
                  substr(V2, 1, 2) != "02")

LA_list <- c(LA_list_missing, "11001")
xmat1 <- subset(xmat1, !(V2 %in% LA_list) & !(V4 %in% LA_list))

length(unique(xmat1$V2))
#as.list(unique(xmat1$V2[which(!(xmat1$V2 %in% allcomp$fips))]))

# Unnest list elements for easier manipulation
xmat1 <- unnest(xmat1)
# Create a list of each unique FIPS code for matching
fipslist <- unique(xmat1$V2)

# Subset dataframe to only include FIPS present in the distance matrix
allcomp_nomissing <- subset(allcomp, fips %in% fipslist)
#allcomp_nomissing <- subset(allcomp_nomissing, fips != "51770")
allcomp_full <- allcomp_nomissing %>%
  arrange(fips) %>%
  select(fips,year,everything())

xmatfips <- unique(xmat1$V2)

# Create an empty (all cells = 0) n x n distance matrix with n = no of FIPS codes/counties
fmat <- matrix(data=0, nrow = length(xmatfips), ncol = length(xmatfips), dimnames = list(xmatfips,xmatfips))

# Iterate through xmatfips, populating matrix with 1 if two counties are neighbors
# Census bureau lists counties as neighbors of itself so the loop will fill cell 
# with 0 for row-pair that lists a county as a neighbor of itself
for(i in 1:nrow(xmat1)){
  a = as.character(xmat1$V2[i])
  b = as.character(xmat1$V4[i])
  if(identical(a,b)){fmat[a,b] <- 0}else{fmat[a,b] <- 1}
}

# Row standardises the distance matrix
fmat <- fmat/rowSums(fmat)
sum(is.na(fmat))
dim(fmat)

# Creates listw object for use in splm package
fmatlw <- mat2listw(fmat, style = "W")

# Also a test for errors
testfmatcdg <- listw2dgCMatrix(fmatlw)

# Sanity checks
### Ensure panel is balanced
is.pbalanced(allcomp_full)
### No NAs
sum(is.na(fmatlw))


FE_diffuer <- as.formula("diff_uer ~ mines_diff + lag_diff + lag_diff2 + diff_log_realgdp_pc | fips + year")
FE_prod_orig <- as.formula("diff_uer ~ prod_diff + lag_prod_diff + lag_prod_diff2 + diff_log_realgdp_pc | fips + year")
#FE_prod_active_orig <- as.formula("diff_uer ~ active_prod_diff + active_lag_prod_diff + active_lag_prod_diff2 + diff_log_realgdp_pc | fips + year")
#FE_prod_new <- as.formula("diff_uer ~ diff_total_prod + l1_diff_total_prod + l2_diff_total_prod + diff_log_realgdp_pc | fips + year")
#FE_prod_active_new <- as.formula("diff_uer ~ diff_total_active_prod + l1_diff_total_active_prod + l2_diff_total_active_prod + diff_log_realgdp_pc | fips + year")
FE_prod_cap <- as.formula("diff_uer ~ diff_prod_cap + l1_diff_prod_cap + l2_diff_prod_cap + diff_log_realgdp_pc | fips + year")
FE_prod_cap2 <- as.formula("diff_uer ~ diff_prod_cap2 + l1_diff_prod_cap2 + l2_diff_prod_cap2 + diff_log_realgdp_pc | fips + year")

#FE_diff_cap <- as.formula("diff_uer ~ cap_diff + lag_cap_diff + lag_cap_diff2 + diff_log_realgdp_pc | fips + year")
main <- feols(FE_diffuer, allcomp, se = 'twoway')
main_prod_orig <- feols(FE_prod_orig, allcomp, se = 'twoway')
#main_prod_active_orig <- feols(FE_prod_active_orig, allcomp, se = 'twoway')
#main_prod_new <- feols(FE_prod_new, allcomp, se= "twoway")
#main_prod_active_new <- feols(FE_prod_active_new, allcomp, se= "twoway")
main_cap <- feols(FE_prod_cap, allcomp, se = 'twoway')
main_cap2 <- feols(FE_prod_cap2, allcomp, se = 'twoway')
modelsummary(list(main, main_prod_orig, main_cap, main_cap2), stars = TRUE)


allcomp_full$diff_log_realgdp_pc[which(allcomp_full$fips == "08014" & allcomp_full$year == 2002)] <- 0
allcomp_full$diff_log_realgdp[which(allcomp_full$fips == "08014" & allcomp_full$year == 2002)] <- 0
allcomp_full$diff_log_pop[which(allcomp_full$fips == "08014" & allcomp_full$year == 2002)] <- 0


fmdiff_prod <- diff_uer ~ prod_diff + lag_prod_diff + lag_prod_diff2 + diff_log_realgdp_pc
fmdiff_prodcap <- diff_uer ~ diff_prod_cap + l1_diff_prod_cap + l2_diff_prod_cap + diff_log_realgdp_pc
fmdiff_prodcap2 <- diff_uer ~ diff_prod_cap2 + l1_diff_prod_cap2 + l2_diff_prod_cap2 + diff_log_realgdp_pc

sp_err_lag_prod <- spml(fmdiff_prod, data = allcomp_full, index = NULL,  listw = fmatlw, 
                         lag = TRUE, na.action = na.fail, spatial.error = "b",
                         model = "within", effect = "twoways", quiet = FALSE)

sp_err_lag_cap <- spml(fmdiff_prodcap, data = allcomp_full, index = NULL,  listw = fmatlw, 
                        lag = TRUE, na.action = na.fail, spatial.error = "b",
                        model = "within", effect = "twoways", quiet = FALSE)

sp_err_lag_cap2 <- spml(fmdiff_prodcap2, data = allcomp_full, index = NULL,  listw = fmatlw, 
                       lag = TRUE, na.action = na.fail, spatial.error = "b",
                       model = "within", effect = "twoways", quiet = FALSE)

allcomp_pdf <- pdata.frame(allcomp_full, index = c("fips", "year"))
allcomp_pdf$sl_prod_diff <- slag(allcomp_pdf$prod_diff, fmatlw)
allcomp_pdf$sl_lag_prod_diff <- slag(allcomp_pdf$lag_prod_diff, fmatlw)
allcomp_pdf$sl_lag_prod_diff2 <- slag(allcomp_pdf$lag_prod_diff2, fmatlw)

allcomp_pdf$sl_diff_prod_cap <- slag(allcomp_pdf$diff_prod_cap, fmatlw)
allcomp_pdf$sl_l1_diff_prod_cap <- slag(allcomp_pdf$l1_diff_prod_cap, fmatlw)
allcomp_pdf$sl_l2_diff_prod_cap<- slag(allcomp_pdf$l2_diff_prod_cap, fmatlw)

allcomp_pdf$sl_diff_prod_cap2 <- slag(allcomp_pdf$diff_prod_cap2, fmatlw)
allcomp_pdf$sl_l1_diff_prod_cap2 <- slag(allcomp_pdf$l1_diff_prod_cap2, fmatlw)
allcomp_pdf$sl_l2_diff_prod_cap2 <- slag(allcomp_pdf$l2_diff_prod_cap2, fmatlw)

fmdiff_prod_spl <-  diff_uer ~ prod_diff + lag_prod_diff + lag_prod_diff2 + diff_log_realgdp_pc + sl_prod_diff + sl_lag_prod_diff + sl_lag_prod_diff2
fmdiff_cap_spl <-  diff_uer ~ diff_prod_cap + l1_diff_prod_cap + l2_diff_prod_cap + diff_log_realgdp_pc + sl_diff_prod_cap + sl_l1_diff_prod_cap + sl_l2_diff_prod_cap
fmdiff_cap2_spl2 <-  diff_uer ~ diff_prod_cap2 + l1_diff_prod_cap2 + l2_diff_prod_cap2 + diff_log_realgdp_pc + sl_diff_prod_cap2 + sl_l1_diff_prod_cap2 + sl_l2_diff_prod_cap2



# run model
durbin_error_prod <- spml(fmdiff_prod_spl, data = allcomp_pdf, listw = fmatlw, model="within", effect = "twoways", quiet = FALSE, spatial.error = "b")
summary(durbin_error_prod)
# durbin_lag_prod <- spml(fmdiff_prod_spl, data = allcomp_pdf, listw = fmatlw, model="within", effect = "twoways", quiet = FALSE, lag = TRUE)
# summary(durbin_lag_prod)

durbin_error_cap <- spml(fmdiff_cap_spl, data = allcomp_pdf, listw = fmatlw, model="within", effect = "twoways", quiet = FALSE, spatial.error = "b")
summary(durbin_error_cap)
# durbin_lag_cap <- spml(fmdiff_cap_spl, data = allcomp_pdf, listw = fmatlw, model="within", effect = "twoways", quiet = FALSE, lag = TRUE)
# summary(durbin_lag_cap)

durbin_error_cap2 <- spml(fmdiff_cap2_spl2, data = allcomp_pdf, listw = fmatlw, model="within", effect = "twoways", quiet = FALSE, spatial.error = "b")
summary(durbin_error_cap2)
# durbin_lag_cap2 <- spml(fmdiff_cap2_spl2, data = allcomp_pdf, listw = fmatlw, model="within", effect = "twoways", quiet = FALSE, lag = TRUE)
# summary(durbin_lag_cap2)

sparse.W <- listw2dgCMatrix(fmatlw)
time <- length(unique(allcomp_full$year))
s.lwcounties <- kronecker(Matrix::Diagonal(time), sparse.W)
trMatc <- spatialreg::trW(s.lwcounties, type="mult")
#implag <- impacts(sp_lag_prod, tr = trMatc, R = 200)
imperrlag_prod <- impacts(sp_err_lag_prod, tr = trMatc, R = 200)
imperrlag_cap <- impacts(sp_err_lag_cap, tr = trMatc, R = 200)
imperrlag_cap2 <- impacts(sp_err_lag_cap2, tr = trMatc, R = 200)

summary(imperrlag_prod, zstats=TRUE, short=TRUE)
summary(imperrlag_cap, zstats=TRUE, short=TRUE)
summary(imperrlag_cap2, zstats=TRUE, short=TRUE)
summary(durbin_error_prod)
summary(durbin_error_cap)
summary(durbin_error_cap2)

# Spatial model summary results
# sums_prod <- summary(imperrlag_prod, zstats=TRUE, short=TRUE)
# sums_cap <- summary(imperrlag_cap, zstats=TRUE, short=TRUE)
# sums_cap2 <- summary(imperrlag_cap2, zstats=TRUE, short=TRUE)


# Spatial Durbin Summary
# Spatial error model output table

sum_tbl <- function(mods, var_names, durbin){
  if(durbin){
    temp <- summary(mods) %>% .$CoefTable %>% as.data.frame
    var_names <- c("Spatial Error Parameter", var_names)
    }else{
      temp <- summary(mods) %>% .$coeftable %>% as.data.frame
    }
  names(temp)[4] <- "p_values"
  
  temp1 = temp %>% 
    mutate_at("p_values", function(x) ifelse(x <= 0.001, "***",
                                             ifelse(x > 0.001 & x <= 0.01, "**", 
                                                    ifelse(x > 0.01 & x <= 0.05, "*", 
                                                           ifelse(x > 0.05 & x <= 0.1, ".",""))))) %>%
    mutate_if(is.numeric, round, 3) %>% 
    mutate(Estimate = paste0(Estimate, p_values), var = var_names, se = paste0("(",`Std. Error`,")")) %>%
    select(1,5,6) %>% 
    pivot_longer(!var, names_to = "measure", values_to = "estimate")
  
  return(temp1)
  
}


names_prod <- c("Production_{i,t}", "Production_{i,t-1}", "Production_{i,t-2}", "(log) Real GDPPC")

names_cap <- c("Capacity_{i,t}", "Capacity_{i,t-1}", "Capacity_{i,t-2}", "(log) Real GDPPC")

names_cap2 <- c("Capacity*_{i,t}", "Capacity*_{i,t-1}", "Capacity*_{i,t-2}", "(log) Real GDPPC")

names_durbin_prod <- c("Production_{i,t}", "Production_{i,t-1}", "Production_{i,t-2}", "(log) Real GDPPC", "Production_{-i,t}", "Production_{-i,t-1}", "Production_{-i,t-2}")

names_durbin_cap <- c("Capacity_{i,t}", "Capacity_{i,t-1}", "Capacity_{i,t-2}", "(log) Real GDPPC", "Capacity_{-i,t}", "Capacity_{-i,t-1}", "Capacity_{-i,t-2}")

names_durbin_cap2 <- c("Capacity*_{i,t}", "Capacity*_{i,t-1}", "Capacity*_{i,t-2}", "(log) Real GDPPC", "Capacity*_{-i,t}", "Capacity*_{-i,t-1}", "Capacity*_{-i,t-2}")

# sum_tbl(durbin_error_prod, names_durbin_prod, TRUE) %>% 
#   left_join(sum_tbl(main_prod_orig, names_prod, FALSE), by = c("var", "measure")) %>% 
#   mutate(var = ifelse(row_number()%%2, var, "")) %>%
#   select(-measure) %>% 
#   kable(., booktabs=TRUE, format = "latex") %>%
#     kable_styling(position="center")
# 
# sum_tbl(durbin_error_cap, names_durbin_cap, TRUE) %>% 
#   left_join(sum_tbl(main_cap, names_cap, FALSE), by = c("var", "measure")) %>% 
#   mutate(var = ifelse(row_number()%%2, var, "")) %>%
#   select(-measure) %>% 
#   kable(., booktabs=TRUE, format = "latex") %>%
#   kable_styling(position="center")
# 
# sum_tbl(durbin_error_cap2, names_durbin_cap2, TRUE) %>% 
#   left_join(sum_tbl(main_cap2, names_cap2, FALSE), by = c("var", "measure")) %>% 
#   mutate(var = ifelse(row_number()%%2, var, "")) %>%
#   select(-measure) %>% 
#   kable(., booktabs=TRUE, format = "latex") %>%
#   kable_styling(position="center")


# SARAR Results
sarar_tbl <-cbind(spat_output_rob(imperrlag_prod), 
                    spat_output_rob(imperrlag_cap), 
                    spat_output_rob(imperrlag_cap2))

sarar_tbl[1:5] %>% 
  tibble %>% 
  mutate(var = case_when(var == "prod_diff" ~ "Production_{i,t}",
                         var == "lag_prod_diff" ~ "Production_{i,t-1}",
                         var == "lag_prod_diff2" ~ "Production_{i,t-2}",
                         var == "diff_log_realgdp_pc" ~ "(log) Real GDPPC")) %>% 
  left_join(sum_tbl(durbin_error_prod, names_durbin_prod, TRUE), ., by = c("var", "measure")) %>% 
  left_join(., sum_tbl(main_prod_orig, names_prod, FALSE), by = c("var", "measure")) %>% 
  relocate(var, measure, estimate.y, estimate.x, Direct, Indirect, Total) %>% 
  mutate(var = ifelse(row_number()%%2, var, "")) %>% 
  select(-measure) %>% 
  kable(., booktabs=TRUE, format = "latex") %>% 
  kable_styling(position="center")

sarar_tbl[6:10] %>% 
  tibble %>% 
  mutate(var = case_when(var == "diff_prod_cap" ~ "Capacity_{i,t}",
                         var == "l1_diff_prod_cap" ~ "Capacity_{i,t-1}",
                         var == "l2_diff_prod_cap" ~ "Capacity_{i,t-2}",
                         var == "diff_log_realgdp_pc" ~ "(log) Real GDPPC")) %>% 
  left_join(sum_tbl(durbin_error_cap, names_durbin_cap, TRUE), ., by = c("var", "measure")) %>% 
  left_join(., sum_tbl(main_cap, names_cap, FALSE), by = c("var", "measure")) %>% 
  relocate(var, measure, estimate.y, estimate.x, Direct, Indirect, Total) %>% 
  mutate(var = ifelse(row_number()%%2, var, "")) %>% 
  select(-measure) %>% 
  kable(., booktabs=TRUE, format = "latex") %>% 
  kable_styling(position="center")

sarar_tbl[11:15] %>% 
  tibble %>% 
  mutate(var = case_when(var == "diff_prod_cap2" ~ "Capacity*_{i,t}",
                         var == "l1_diff_prod_cap2" ~ "Capacity*_{i,t-1}",
                         var == "l2_diff_prod_cap2" ~ "Capacity*_{i,t-2}",
                         var == "diff_log_realgdp_pc" ~ "(log) Real GDPPC")) %>% 
  left_join(sum_tbl(durbin_error_cap2, names_durbin_cap2, TRUE), ., by = c("var", "measure")) %>% 
  left_join(sum_tbl(main_cap2, names_cap2, FALSE), by = c("var", "measure")) %>% 
  relocate(var, measure, estimate.y, estimate.x, Direct, Indirect, Total) %>% 
  mutate(var = ifelse(row_number()%%2, var, "")) %>% 
  select(-measure) %>% 
  kable(., booktabs=TRUE, format = "latex") %>% 
  kable_styling(position="center")

# PRODUCTIVE CAPACITY

# test <- read.xls('https://www.eia.gov/coal/annual/xls/table11.xls', skip = 3) %>%
#   tibble %>%
#   slice(-c(28:31)) %>%
#   rename(state = Coal.Producing,
#          X2022 = Total,
#          X2021 = Total.1) %>%
#   select(state, contains("x")) %>%
#   filter(!grepl("      ", state) & state != "State") %>%
#   mutate(state = gsub(" Total", "", state),
#          across(!state, ~as.numeric(gsub(",", "", .)))) %>%
#     pivot_longer(cols = !state, names_to = "year", values_to = "prod_cap_state", names_transform = list(year = ~as.numeric(gsub("X","", .)))) %>%
#   arrange(year, state)


  
  # Conclusion: active production versus total production are not that different...likely mislabeled things...
  # ggplot() +
  # geom_line(aes(x = year, y = total_state_prod)) +
  # geom_line(aes(x = year, y = total_active_state_prod), color = "blue") + 
  # facet_wrap(~state, scales  = "free")


#####################################
##### SURFACE VS. UNDEGROUND ########
#####################################


##### TWFE OLS ##

FE_diffuer <- as.formula("diff_uer ~ mines_diff + lag_diff + lag_diff2 + diff_log_realgdp_pc | fips + year")
FE_diff_surface <- as.formula("diff_uer ~ diff_surface_active_n + l1_diff_surface_active_n + l2_diff_surface_active_n + diff_log_realgdp_pc | fips + year")
FE_diff_underground <-as.formula("diff_uer ~ diff_underground_active_n + l1_diff_underground_active_n + l2_diff_underground_active_n + diff_log_realgdp_pc | fips + year")
#FE_surface <- as.formula("uer ~ surface_active_n + l1_surface_active_n + l2_surface_active_n + log_realgdp_pc | fips + year")
#FE_underground <-as.formula("uer ~ underground_active_n + l1_underground_active_n + l2_underground_active_n + log_realgdp_pc | fips + year")

#FE_diff_cap <- as.formula("diff_uer ~ cap_diff + lag_cap_diff + lag_cap_diff2 + diff_log_realgdp_pc | fips + year")
main <- feols(FE_diffuer, allcomp, se = 'twoway')
main_surface <- feols(FE_diff_surface, allcomp, se = 'twoway')
main_underground <- feols(FE_diff_underground, allcomp, se = 'twoway')
#lev_surface <- feols(FE_surface, allcomp, se = 'twoway')
#lev_underground <- feols(FE_underground, allcomp, se = 'twoway')
modelsummary(list(main, main_surface, main_underground), stars = TRUE)
  
############ SPATIAL ########
allcomp_full$diff_log_realgdp_pc[which(allcomp_full$fips == "08014" & allcomp_full$year == 2002)] <- 0
allcomp_full$diff_log_realgdp[which(allcomp_full$fips == "08014" & allcomp_full$year == 2002)] <- 0
allcomp_full$diff_log_pop[which(allcomp_full$fips == "08014" & allcomp_full$year == 2002)] <- 0


fmdiff_surface <- diff_uer ~ diff_surface_active_n + l1_diff_surface_active_n + l2_diff_surface_active_n + diff_log_realgdp_pc
fmdiff_underground <- diff_uer ~ diff_underground_active_n + l1_diff_underground_active_n + l2_diff_underground_active_n + diff_log_realgdp_pc

sp_err_lag_surface <- spml(fmdiff_surface, data = allcomp_full, index = NULL,  listw = fmatlw, 
                        lag = TRUE, na.action = na.fail, spatial.error = "b",
                        model = "within", effect = "twoways", quiet = FALSE)

sp_err_lag_underground <- spml(fmdiff_underground, data = allcomp_full, index = NULL,  listw = fmatlw, 
                       lag = TRUE, na.action = na.fail, spatial.error = "b",
                       model = "within", effect = "twoways", quiet = FALSE)

#allcomp_pdf <- pdata.frame(allcomp_full, index = c("fips", "year"))
allcomp_pdf$sl_diff_surface_active_n <- slag(allcomp_pdf$diff_surface_active_n, fmatlw)
allcomp_pdf$sl_l1_diff_surface_active_n <- slag(allcomp_pdf$l1_diff_surface_active_n, fmatlw)
allcomp_pdf$sl_l2_diff_surface_active_n <- slag(allcomp_pdf$l2_diff_surface_active_n, fmatlw)

allcomp_pdf$sl_diff_underground_active_n <- slag(allcomp_pdf$diff_underground_active_n, fmatlw)
allcomp_pdf$sl_l1_diff_underground_active_n <- slag(allcomp_pdf$l1_diff_underground_active_n, fmatlw)
allcomp_pdf$sl_l2_diff_underground_active_n <- slag(allcomp_pdf$l2_diff_underground_active_n, fmatlw)

fmdiff_surface_spl <-  diff_uer ~ diff_surface_active_n + l1_diff_surface_active_n + l2_diff_surface_active_n + diff_log_realgdp_pc + sl_diff_surface_active_n + sl_l1_diff_surface_active_n + sl_l2_diff_surface_active_n
fmdiff_underground_spl <-  diff_uer ~ diff_underground_active_n + l1_diff_underground_active_n + l2_diff_underground_active_n + diff_log_realgdp_pc + sl_diff_underground_active_n + sl_l1_diff_underground_active_n + sl_l2_diff_underground_active_n


# run model
durbin_error_surface <- spml(fmdiff_surface_spl, data = allcomp_pdf, listw = fmatlw, model="within", effect = "twoways", quiet = FALSE, spatial.error = "b")
summary(durbin_error_surface)
# durbin_lag_prod <- spml(fmdiff_prod_spl, data = allcomp_pdf, listw = fmatlw, model="within", effect = "twoways", quiet = FALSE, lag = TRUE)
# summary(durbin_lag_prod)

durbin_error_underground <- spml(fmdiff_underground_spl, data = allcomp_pdf, listw = fmatlw, model="within", effect = "twoways", quiet = FALSE, spatial.error = "b")
summary(durbin_error_underground)
# durbin_lag_cap <- spml(fmdiff_cap_spl, data = allcomp_pdf, listw = fmatlw, model="within", effect = "twoways", quiet = FALSE, lag = TRUE)
# summary(durbin_lag_cap)


sparse.W <- listw2dgCMatrix(fmatlw)
time <- length(unique(allcomp_full$year))
s.lwcounties <- kronecker(Matrix::Diagonal(time), sparse.W)
trMatc <- spatialreg::trW(s.lwcounties, type="mult")
#implag <- impacts(sp_lag_prod, tr = trMatc, R = 200)
imperrlag_surface <- impacts(sp_err_lag_surface, tr = trMatc, R = 200)
imperrlag_underground <- impacts(sp_err_lag_underground, tr = trMatc, R = 200)


summary(imperrlag_surface, zstats=TRUE, short=TRUE)
summary(imperrlag_underground, zstats=TRUE, short=TRUE)

summary(durbin_error_surface)
summary(durbin_error_underground)


# Spatial model summary results
# sums_prod <- summary(imperrlag_prod, zstats=TRUE, short=TRUE)
# sums_cap <- summary(imperrlag_cap, zstats=TRUE, short=TRUE)
# sums_cap2 <- summary(imperrlag_cap2, zstats=TRUE, short=TRUE)


# Spatial Durbin Summary
# Spatial error model output table

sum_tbl <- function(mods, var_names, durbin){
  if(durbin){
    temp <- summary(mods) %>% .$CoefTable %>% as.data.frame
    var_names <- c("Spatial Error Parameter", var_names)
  }else{
    temp <- summary(mods) %>% .$coeftable %>% as.data.frame
  }
  names(temp)[4] <- "p_values"
  
  temp1 = temp %>% 
    mutate_at("p_values", function(x) ifelse(x <= 0.001, "***",
                                             ifelse(x > 0.001 & x <= 0.01, "**", 
                                                    ifelse(x > 0.01 & x <= 0.05, "*", 
                                                           ifelse(x > 0.05 & x <= 0.1, ".",""))))) %>%
    mutate_if(is.numeric, round, 3) %>% 
    mutate(Estimate = paste0(Estimate, p_values), var = var_names, se = paste0("(",`Std. Error`,")")) %>%
    select(1,5,6) %>% 
    pivot_longer(!var, names_to = "measure", values_to = "estimate")
  
  return(temp1)
  
}


names_surface <- c("Diff Active Surface Mines_{i,t}", "Diff Active Surface Mines_{i,t-1}", "Diff Active Surface Mines_{i,t-2}", "Diff (log) Real GDPPC")

names_durbin_surface <- c("Diff Active Surface Mines_{i,t}", "Diff Active Surface Mines_{i,t-1}", "Diff Active Surface Mines_{i,t-2}", "Diff (log) Real GDPPC", "Diff Active Surface Mines_{-i,t}", "Diff Active Surface Mines_{-i,t-1}", "Diff Active Surface Mines_{-i,t-2}")

names_underground <- c("Diff Active Underground Mines_{i,t}", "Diff Active Underground Mines_{i,t-1}", "Diff Active Underground Mines_{i,t-2}", "Diff (log) Real GDPPC")

names_durbin_underground <- c("Diff Active Underground Mines_{i,t}", "Diff Active Underground Mines_{i,t-1}", "Diff Active Underground Mines_{i,t-2}", "Diff (log) Real GDPPC", "Diff Active Underground Mines_{-i,t}", "Diff Active Underground Mines_{-i,t-1}", "Diff Active Underground Mines_{-i,t-2}")


# SARAR Results
sarar_tbl <-cbind(spat_output_rob(imperrlag_surface), 
                  spat_output_rob(imperrlag_underground))
                  
sarar_tbl[1:5] %>% 
  tibble %>% 
  mutate(var = case_when(var == "diff_surface_active_n" ~ "Diff Active Surface Mines_{i,t}",
                         var == "l1_diff_surface_active_n" ~ "Diff Active Surface Mines_{i,t-1}",
                         var == "l2_diff_surface_active_n" ~ "Diff Active Surface Mines_{i,t-2}",
                         var == "diff_log_realgdp_pc" ~ "Diff (log) Real GDPPC")) %>% 
  left_join(sum_tbl(durbin_error_surface, names_durbin_surface, TRUE), ., by = c("var", "measure")) %>% 
  left_join(., sum_tbl(main_surface, names_surface, FALSE), by = c("var", "measure")) %>% 
  relocate(var, measure, estimate.y, estimate.x, Direct, Indirect, Total) %>% 
  mutate(var = ifelse(row_number()%%2, var, "")) %>% 
  select(-measure) %>% 
  kable(., booktabs=TRUE, format = "latex") %>% 
  kable_styling(position="center")

sarar_tbl[6:10] %>% 
  tibble %>% 
  mutate(var = case_when(var == "diff_underground_active_n" ~ "Diff Active Underground Mines_{i,t}",
                         var == "l1_diff_underground_active_n" ~ "Diff Active Underground Mines_{i,t-1}",
                         var == "l2_diff_underground_active_n" ~ "Diff Active Underground Mines_{i,t-2}",
                         var == "diff_log_realgdp_pc" ~ "Diff (log) Real GDPPC")) %>% 
  left_join(sum_tbl(durbin_error_underground, names_durbin_underground, TRUE), ., by = c("var", "measure")) %>% 
  left_join(., sum_tbl(main_underground, names_underground, FALSE), by = c("var", "measure")) %>% 
  relocate(var, measure, estimate.y, estimate.x, Direct, Indirect, Total) %>% 
  mutate(var = ifelse(row_number()%%2, var, "")) %>% 
  select(-measure) %>% 
  kable(., booktabs=TRUE, format = "latex") %>% 
  kable_styling(position="center")


### Employment numbers #####
# This data is pulled from QCEW and compiled to "qcew_compiled_raw_ff.RDS" in "Documents/Documents - Nuff-Malham/Oxford DPhil/Public Finance/Data Scoping/pub_fin_schools/code/qcew_shift_share"
# Source: https://www.bls.gov/cew/downloadable-data-files.htm.  - CSVs Single Files: Annual Averages

# Industry codes provided by QCEW BLS: https://www.bls.gov/cew/classifications/industry/industry-titles.htm
ind_codes <- read_excel("~/Documents/Documents - Nuff-Malham/Oxford DPhil/Public Finance/Data Scoping/pub_fin_schools/data/raw/QCEW/industry-titles.xlsx")

# df_list <- list()
# for(y in 2000:2022){
#   print(y)
#   df_list[[as.character(y)]] <- read.csv(paste0("~/Documents/Documents - Nuff-Malham/Oxford DPhil/Public Finance/Data Scoping/pub_fin_schools/data/raw/QCEW/", y, ".annual.singlefile.csv")) %>%
#     tibble %>%
#     mutate(year = y) %>%
#     left_join(., ind_codes, by = "industry_code") %>% 
#     filter(nchar(industry_code) <= 2 | keep == 1)
# }

#df_list %>% do.call("rbind", .) %>% saveRDS(., here("data/qcew_compiled_raw_2000_2022.RDS"))

temp_test <- readRDS(here("data/qcew_compiled_raw_2000_2022.RDS"))

temp <- temp_test %>% 
  filter(substr(area_fips, 3,5) != "000" & substr(area_fips, 3,5) != "999") %>%  
  mutate(fips_state = substr(area_fips, 1,2)) %>% 
  rename(fips = area_fips) %>% 
  filter(!(fips_state %in% c("72","78", "C1", "C2", "C3", "C4", "CS"))) %>% 
  mutate(fips = ifelse(fips %in% names(getfips), unname(getfips[fips]), fips)) %>% 
  filter(!(fips %in% c("51560", "51515")) & !(fips == "46113" & year == 2015)) %>% 
  mutate(fips = ifelse(fips == "46113", "46102", fips)) %>%
  # Agglevel codes: https://www.bls.gov/cew/classifications/aggregation/agg-level-titles.htm  
  #       only "total industry" has multiple agg level codes - 70 is the correct one for county totals
  filter(!(industry_code == 10 & agglvl_code != 70)) %>% 
  select(fips, year, industry_code, industry_title, annual_avg_estabs, annual_avg_emplvl, total_annual_wages, avg_annual_pay) %>% 
  # Any counties that have multiple observations DO NOT HAVE A TOTAL "OWN-CODE = 0" or TOTAL GOVERNMENT "OWN-CODE = 8" record - therefore we group by fips, year, industry_code and sum total
  group_by(fips, year, industry_code, industry_title) %>% 
  summarise(across(c(annual_avg_estabs, annual_avg_emplvl, total_annual_wages, avg_annual_pay), ~sum(., na.rm = TRUE))) %>% 
  ungroup 

# There are 12 exceptions ot the above filtering of industrycode == 10 - 0 reported for total but there are lower level estimates. The below creates this supplement
temp_supp <- temp_test %>% 
  filter(substr(area_fips, 3,5) != "000" & substr(area_fips, 3,5) != "999") %>%  
  mutate(fips_state = substr(area_fips, 1,2)) %>% 
  rename(fips = area_fips) %>% 
  filter(!(fips_state %in% c("72","78", "C1", "C2", "C3", "C4", "CS"))) %>% 
  mutate(fips = ifelse(fips %in% names(getfips), unname(getfips[fips]), fips)) %>% 
  filter(!(fips %in% c("51560", "51515")) & !(fips == "46113" & year == 2015)) %>% 
  mutate(fips = ifelse(fips == "46113", "46102", fips)) %>%
  # Agglevel codes: https://www.bls.gov/cew/classifications/aggregation/agg-level-titles.htm  
  #       only "total industry" has multiple agg level codes - 70 is the correct one for county totals
  group_by(fips, year) %>% 
  filter(industry_code == "10" & any(agglvl_code == "70" & annual_avg_emplvl == 0) & fips %in% unique(allcomp$fips)) %>% 
  ungroup %>% 
  select(fips, year, industry_code, industry_title, annual_avg_estabs, annual_avg_emplvl, total_annual_wages, avg_annual_pay) %>% 
  # Any counties that have multiple observations DO NOT HAVE A TOTAL "OWN-CODE = 0" or TOTAL GOVERNMENT "OWN-CODE = 8" record - therefore we group by fips, year, industry_code and sum total
  group_by(fips, year, industry_code, industry_title) %>% 
  summarise(across(c(annual_avg_estabs, annual_avg_emplvl, total_annual_wages, avg_annual_pay), ~sum(., na.rm = TRUE))) %>% 
  ungroup

emp_df <- temp %>% 
  filter(!(industry_code == "10" & annual_avg_emplvl == 0)) %>% 
  rbind(temp_supp) %>% 
 # "42352" ~ Coal and other mineral and ore merchant wholesalers
 # "213113" ~ Support activities for coal mining
 # "2121" ~ Coal mining
 # "221112" Fossil fuel electric power generation
  filter(nchar(industry_code) <= 2 | industry_code %in% c("42352", "213113", "2121", "221112")) %>% 
  select(fips, year, industry_code, annual_avg_emplvl) %>% 
  mutate(nc = nchar(industry_code)) %>% 
  arrange(nc) %>% 
  select(-nc) %>% 
  pivot_wider(id_cols = c("fips", "year"), names_from = industry_code, values_from = annual_avg_emplvl, names_prefix = "emp_") %>% 
  mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .))) %>% 
  mutate(emp_coal = emp_2121 + emp_213113,
         emp_coal_sales = emp_coal + emp_42352,
         emp_coal_ff_gen = emp_coal_sales + emp_221112,
         emp_non_coal = emp_10 - emp_coal_sales) %>% 
  select(-c(emp_42352, emp_213113, emp_2121, emp_221112, emp_99)) %>% 
  complete(fips, year) %>% 
  mutate(across(contains("emp"), ~./1000)) %>% 
  mutate(across(contains("emp"), .fns = list(log = ~log(. + 1)), .names = "{.fn}_{.col}")) %>% 
  group_by(fips) %>% 
  mutate(across(contains("emp"), .fns = list(diff = ~c(NA,diff(.))), .names = "{.fn}_{.col}")) %>% 
  ungroup %>% 
  mutate(coal_share_emp = emp_coal/emp_10,
         coal_sales_share_emp = emp_coal_sales/emp_10) %>% 
  group_by(fips) %>% 
  mutate(across(c(coal_share_emp, coal_sales_share_emp), .fns = list(l1 = ~lag(.x, 1), l2 = ~lag(.x, 2)), .names = "{.fn}_{.col}")) %>% 
            ungroup

is.pbalanced(emp_df)

allcomp_final <- read_excel(here("data/allcomp_final.xlsx"))
# All fips codes present
allcomp_final %>% pull(fips) %>% unique %>% setdiff(unique(emp_df$fips)) %>% length(.) == 0
allcomp_final %>% pull(year) %>% unique %>% setdiff(unique(emp_df$year)) %>% length(.) == 0

allcomp_emp <- allcomp_final %>% 
  left_join(., emp_df, by = c("fips", "year"))

allcomp_emp_old <- allcomp_emp

# industry_code industry_title                                                                   
# <chr>         <chr>                                                                            
# 10            10 Total, all industries                                                         
# 22            NAICS 22 Utilities                                                               
# 23            NAICS 23 Construction                                                            
# 42            NAICS 42 Wholesale trade                                                         
# 51            NAICS 51 Information                                                             
# 52            NAICS 52 Finance and insurance                                                   
# 53            NAICS 53 Real estate and rental and leasing                                      
# 54            NAICS 54 Professional, scientific, and technical services                        
# 71            NAICS 71 Arts, entertainment, and recreation                                     
# 72            NAICS 72 Accommodation and food services                                         
# 81            NAICS 81 Other services (except public administration)                           
# 92            NAICS 92 Public administration                                                   
# 11            NAICS 11 Agriculture, forestry, fishing and hunting                              
# 21            NAICS 21 Mining, quarrying, and oil and gas extraction                           
# 55            NAICS 55 Management of companies and enterprises                                 
# 56            NAICS 56 Administrative and support and waste management and remediation services
# 61            NAICS 61 Educational services                                                    
# 62            NAICS 62 Health care and social assistance                                       
# 99            NAICS 99 Unclassified         

# Also create a variable for the coal share of total employment
deps <- emp_df %>% select(-c(fips, year)) %>% names 

coal_emp_mods <- feols(.[deps[grepl("coal", deps)]] ~ mines_diff + lag_diff + lag_diff2 + diff_log_pop + diff_log_realgdp | fips + year, allcomp_emp, se = 'twoway')

other_emp_mods <- feols(.[deps[!grepl("coal", deps)]] ~ mines_diff + lag_diff + lag_diff2 + diff_log_pop + diff_log_realgdp | fips + year, allcomp_emp, se = 'twoway')

for(tt in list("emp_coal$", "emp_coal_sales", "emp_coal_ff_gen", "emp_non_coal")){
  coal_emp_mods[lhs = tt] %>% etable(title = tt) %>% print
}

codes <- ind_codes %>% filter(nchar(industry_code) == 2 & industry_code != "99") %>% select(industry_code, industry_title) %>% unique
codes_list <- as.list(codes$industry_code)
names(codes_list) <- as.list(codes$industry_title)


for(k in codes_list){
  other_emp_mods[lhs = k] %>% etable(title = names(codes_list[codes_list == k])) %>% print
}

# differences only
c(coal_emp_mods[lhs = c("diff_emp_coal", "diff_emp_coal_sales", "diff_emp_coal_ff_gen", "diff_emp_non_coal")], other_emp_mods[lhs = c("diff_emp_10", "diff_emp_55", "diff_emp_71", "diff_emp_72") ]) %>% etable(title = "Change in Employment Levels", tex = TRUE) %>% print

# difference in logs only
c(coal_emp_mods[lhs = c("diff_log_emp_coal", "diff_log_emp_coal_sales", "diff_log_emp_coal_ff_gen", "diff_log_emp_non_coal")], other_emp_mods[lhs = c("diff_log_emp_10", "diff_log_emp_61", "diff_log_emp_62", "diff_log_emp_71", "diff_log_emp_72")]) %>% etable(title = "Change in (log) Employment Levels", tex = TRUE) %>% print

#### Code for fixest_dict of robustness checks
tttttt <- deps %>% 
  data.frame("dep_var" = .) %>% 
  mutate(sector_code = substr(dep_var, nchar(dep_var) - 1, nchar(dep_var)))

matched_sectors <- match(tttttt$sector_code, codes_list)
tttttt$sector_name <- names(codes_list)[matched_sectors]

### Missing data ##
missing_emp_nos <- allcomp_emp %>% 
  select(fips, year, labour_force, starts_with("emp_")) %>% 
  mutate(across(contains("emp"), ~ifelse(. == 0, NA, .))) %>% 
  group_by(fips) %>% 
  summarise(mean_labour_force = mean(labour_force), across(contains('emp'), ~sum(is.na(.)))) %>% 
  pivot_longer(cols = !c(fips, mean_labour_force)) #%>% 
  # #filter(value > 0) %>%  #& !(name %in% c("emp_coal","emp_coal_sales", "emp_coal_ff_gen", "emp_11", "emp_21", "emp_22"))) %>% 
  # ggplot() +
  # geom_histogram(aes(x = mean_labour_force)) + 
  # geom_jitter(aes(x = mean_labour_force, y = value, color = name)) +
  # scale_y_continuous(
  #   "count", 
  #   sec.axis = sec_axis(~./10000, name = "count")
  # )

library(ggridges)
library(viridis)
missing_emp_nos %>% 
  pivot_longer(cols = !fips) %>% 
  ggplot () +
  geom_histogram(aes(x = value, group = name)) +
  facet_grid(~name)

missing_emp_nos %>% 
  ggplot(aes(x = value, y = name)) +
  geom_density_ridges(alpha = 0.7) +
  theme_minimal() + 
  scale_fill_viridis() +
  theme(legend.position = "none") +
  scale_y_discrete(limits = rev(levels(as.factor(name)))) +
  labs(title = "Histogram of Education Expenditure per Pupil per Year", y = "Year", x = "(log) Total Education Expenditure per Pupil (Real USD)")

################################################################################
# Controlling appropriately  population and log_realgdp
# Also create a variable for the coal share of total employment

deps <- emp_df %>% select(contains("diff_emp"), contains("diff_log_emp")) %>% names 

coal_emp_mods <- feols(.[deps[grepl("coal", deps)]] ~ mines_diff + lag_diff + lag_diff2 + diff_log_pop + diff_log_realgdp | fips + year, allcomp_emp, se = 'twoway')

other_emp_mods <- feols(.[deps[!grepl("coal", deps)]] ~ mines_diff + lag_diff + lag_diff2 + diff_log_pop + diff_log_realgdp | fips + year, allcomp_emp, se = 'twoway')

for(tt in list("emp_coal$", "emp_coal_sales", "emp_coal_ff_gen", "emp_non_coal")){
  coal_emp_mods[lhs = tt] %>% etable(title = tt) %>% print
}

codes <- ind_codes %>% filter(nchar(industry_code) == 2 & industry_code != "99") %>% select(industry_code, industry_title) %>% unique
codes_list <- as.list(codes$industry_code)
names(codes_list) <- as.list(codes$industry_title)


for(k in codes_list){
  other_emp_mods[lhs = k] %>% etable(title = names(codes_list[codes_list == k])) %>% print
}

# differences only
coal_emp_mods[lhs = c("diff_emp_coal", "diff_emp_coal_sales", "diff_emp_coal_ff_gen", "diff_emp_non_coal")] %>% etable(title = "Change in Employment Levels", tex = TRUE) %>% print
other_emp_mods[lhs = c("diff_emp_10", "diff_emp_23", "diff_emp_71", "diff_emp_72", "diff_emp_21", "diff_emp_55")] %>% etable(title = "Change in Employment Levels (Secondary Sectors)", tex = TRUE) %>% print

# difference in logs only
coal_emp_mods[lhs = c("diff_log_emp_coal", "diff_log_emp_coal_sales", "diff_log_emp_coal_ff_gen", "diff_log_emp_non_coal")] %>% etable(title = "Change in (log) Employment Levels", tex = TRUE) %>% print
other_emp_mods[lhs = c("diff_log_emp_10", "diff_log_emp_23", "diff_log_emp_72", "diff_log_emp_21", "diff_log_emp_55", "diff_log_emp_61")] %>% etable(title = "Change in (log) Employment Levels (Secondary Sectors)", tex = TRUE) %>% print

#### Code for fixest_dict of robustness checks
tttttt <- deps %>% 
  data.frame("dep_var" = .) %>% 
  mutate(sector_code = substr(dep_var, nchar(dep_var) - 1, nchar(dep_var)))

matched_sectors <- match(tttttt$sector_code, codes_list)
tttttt$sector_name <- names(codes_list)[matched_sectors]


#### CONTROLLING FOR SHARE IN EMPLOYMENT ######
temp_emp_interpol <- allcomp_emp %>% 
  group_by(state.x, year) %>% 
  mutate(lab_prod_state = sum(emp_coal, .na.rm = TRUE)/sum(active_mines, na.rm = TRUE)) %>% 
  ungroup %>%
  group_by(fips, year) %>%
  mutate(lab_prod_county = emp_coal/active_mines) %>% 
  #select(fips, year, emp_coal, active_mines, lab_prod_state, lab_prod_county) %>% 
  group_by(fips) %>% 
  mutate(lab_prod_county = ifelse(lab_prod_county == 0, NA, lab_prod_county),
         lab_prod_county_interpol = na.approx(lab_prod_county, na.rm=FALSE), 
         lab_prod_county_interpol = ifelse(is.infinite(lab_prod_county_interpol), NA, lab_prod_county_interpol),
         lab_prod_county_mean = mean(lab_prod_county, na.rm = TRUE),
         lab_prod_county_mean = ifelse(is.nan(lab_prod_county_mean) | is.infinite(lab_prod_county_mean), NA, lab_prod_county_mean)) %>% 
  fill(lab_prod_county_interpol, .direction = "updown") %>% 
  mutate(lab_prod_county_interpol = ifelse(is.na(lab_prod_county_interpol), lab_prod_state, lab_prod_county_interpol),
         lab_prod_county_mean = ifelse(is.na(lab_prod_county_mean), lab_prod_state, lab_prod_county_mean)) %>% 
  ungroup %>%
  mutate(coal_share_emp_interpol = ifelse(coal_share_emp == 0, (active_mines*lab_prod_county_interpol)/emp_10, coal_share_emp),
         coal_share_emp_mean = ifelse(coal_share_emp == 0, (active_mines*lab_prod_county_mean)/emp_10, coal_share_emp),
         coal_share_emp_interpol = ifelse(is.na(coal_share_emp_interpol), 0, coal_share_emp_interpol),
         coal_share_emp_mean = ifelse(is.na(coal_share_emp_mean), 0, coal_share_emp_mean),
         across(c(coal_share_emp, coal_share_emp_interpol, coal_share_emp_mean), ~case_when(. <= 0.1 ~ "low",
                                       . > 0.1 & . <= 0.2 ~ "medium",
                                       . > 0.2 ~ "high"), .names = "{.col}_discrete"), 
         across(contains("discrete"), ~factor(., levels = c("low", "medium", "high"))))
         #across(contains("discrete"), ~factor(., levels = c("2 - medium", "1 - low",  "3 - high"))))
         #across(contains("discrete"), ~factor(., levels = c("3 - high", "1 - low", "2 - medium"))))

FE_diffuer_main <- feols(diff_uer ~ mines_diff + lag_diff + lag_diff2 + diff_log_realgdp_pc | fips + year, allcomp_emp, se = "twoway")
control_share_raw <- "diff_uer ~ mines_diff + lag_diff + lag_diff2 + diff_log_realgdp_pc + coal_share_emp | fips + year"
share_forms <- list(control_share_raw,
                    gsub("coal_share_emp", "coal_share_emp_interpol", control_share_raw),
                    gsub("coal_share_emp", "coal_share_emp_mean", control_share_raw),
                    "diff_uer ~ coal_share_emp_discrete*mines_diff + coal_share_emp_discrete*lag_diff + coal_share_emp_discrete*lag_diff2 + diff_log_realgdp_pc | fips + year",
                    "diff_uer ~ coal_share_emp_interpol_discrete*mines_diff + coal_share_emp_interpol_discrete*lag_diff + coal_share_emp_interpol_discrete*lag_diff2 + diff_log_realgdp_pc | fips + year",
                    "diff_uer ~ coal_share_emp_mean_discrete*mines_diff + coal_share_emp_mean_discrete*lag_diff + coal_share_emp_mean_discrete*lag_diff2 + diff_log_realgdp_pc | fips + year")


share_results <- list(FE_diffuer_main)
for(el in share_forms){
  share_results <- c(share_results, list(feols(as.formula(el), data = temp_emp_interpol, se = "twoway")))
}

share_results[7]

# Orders by variable for coal share of employment used
share_results[c(1,2,5,3,6,4,7)] %>% etable(tex = TRUE)

glht_contemp <- list()
contemp_tests <- c()
glht_persistent <- list()
persistence_tests <- c()
for(k in share_results[c(1,2,5,3,6,4,7)]){
  k$coefficients
  
  #model_list <- c(model_list, list(coef_test), list(persistence_test))
  glht_low <- glht(k, linfct = "mines_diff = 0")
  # Contemporaneous tests
  tests_med <- k$coefficients %>% purrr::keep(grepl("medium:mines_diff", names(.), fixed = TRUE)) %>% names
  #print(tests_med)
  tests_high <- k$coefficients %>% purrr::keep(grepl("high:mines_diff", names(.), fixed = TRUE)) %>% names
 # print(tests_high)
  if(length(tests_med) != 0 & length(tests_high) != 0){
    hyp_test_contemp_med <- paste0("mines_diff + ", tests_med, " = 0")
    #print(hyp_test_contemp_med)
    glht_med <- glht(k, linfct = hyp_test_contemp_med)
    hyp_test_contemp_high <- paste0("mines_diff + ", tests_high, " = 0")
    #print(hyp_test_contemp_high)
    glht_high <- glht(k, linfct = hyp_test_contemp_high)
    glht_contemp <- c(glht_contemp, list(glht_low), list(glht_med), list(glht_high))
    contemp_tests <- c(contemp_tests, hyp_test_contemp_med, hyp_test_contemp_high)
  }else{
    glht_contemp <- c(glht_contemp, list(glht_low))
  }

  # Persistence tests
  glht_low <- glht(k, linfct = "mines_diff + lag_diff + lag_diff2 = 0")
  tests_med <- k$coefficients %>% purrr::keep(grepl("medium:", names(.), fixed = TRUE)) %>% names
  #print(paste0(tests_med, collapse = "+"))
  tests_high <- k$coefficients %>% purrr::keep(grepl("high:", names(.), fixed = TRUE)) %>% names
 # print(paste0(tests_high, collapse = "+"))
  if(length(tests_med) != 0 & length(tests_high) != 0){
    hyp_test_persistent_med <- paste0("mines_diff + ", paste0(tests_med, collapse = " + "), " = 0")
    glht_med <- glht(k, linfct = hyp_test_persistent_med) %>% print
    hyp_test_persistent_high <- paste0("mines_diff + lag_diff + lag_diff2 + ", paste0(tests_high, collapse = " + "), " = 0")
    glht_high <- glht(k, linfct = hyp_test_persistent_high) %>% print
    glht_persistent <- c(glht_persistent, list(glht_low), list(glht_med), list(glht_high))
    persistence_tests <- c(persistence_tests, hyp_test_persistent_med, hyp_test_persistent_high)
  }else{
    glht_persistent <- c(glht_persistent, list(glht_low))
  }
}

rename_fun <- function(name_list){
  new <- lapply(name_list, function(x) gsub("discrete|interpol_|mean_", "", x)) %>% 
    unlist
  return(new)}

options(modelsummary_format_numeric_latex = "plain")
glht_contemp %>% 
  modelsummary(stars= TRUE, statistic = c("std.error", "p.value"), 
               coef_rename = rename_fun, output = "latex")

glht_persistent %>% 
  modelsummary(stars= TRUE, statistic = c("std.error", "p.value"), 
               coef_rename = rename_fun, output = "latex")

# Weighted Regression
feols(as.formula("diff_uer ~ mines_diff + lag_diff + lag_diff2 + diff_log_realgdp_pc | fips + year"), data = temp_emp_interpol, weights = ~(1-coal_share_emp), se= "twoway")
feols(as.formula("diff_uer ~ mines_diff + lag_diff + lag_diff2 + diff_log_realgdp_pc | fips + year"), data = temp_emp_interpol, weights = ~(1-coal_share_emp_interpol), se= "twoway")
feols(as.formula("diff_uer ~ mines_diff + lag_diff + lag_diff2 + diff_log_realgdp_pc | fips + year"), data = temp_emp_interpol, weights = ~(1-coal_share_emp_mean), se= "twoway")

### Secondary effect #### 

coal_emp_mods_disc <- feols(.[deps[grepl("coal", deps)]] ~ i(coal_share_emp_interpol_discrete, mines_diff) + i(coal_share_emp_interpol_discrete, lag_diff) + i(coal_share_emp_interpol_discrete, lag_diff2) + diff_log_pop + diff_log_realgdp | fips + year, temp_emp_interpol, se = 'twoway')

other_emp_mods_disc <- feols(.[deps[!grepl("coal", deps)]] ~ i(coal_share_emp_interpol_discrete, mines_diff) + i(coal_share_emp_interpol_discrete, lag_diff) + i(coal_share_emp_interpol_discrete, lag_diff2) + diff_log_pop + diff_log_realgdp | fips + year, temp_emp_interpol, se = 'twoway')

for(tt in list("emp_coal$", "emp_coal_sales", "emp_coal_ff_gen", "emp_non_coal")){
  coal_emp_mods_disc[lhs = tt] %>% etable(title = tt) %>% print
}


for(k in codes_list){
  other_emp_mods_disc[lhs = k] %>% etable(title = names(codes_list[codes_list == k])) %>% print
}

# differences only
coal_emp_mods_disc[lhs = c("diff_emp_coal", "diff_emp_coal_sales", "diff_emp_coal_ff_gen", "diff_emp_non_coal")] %>% etable(title = "Change in Employment Levels", tex = TRUE) %>% print
other_emp_mods_disc[lhs = c("diff_emp_10", "diff_emp_23", "diff_emp_71", "diff_emp_72", "diff_emp_21", "diff_emp_55")] %>% etable(title = "Change in Employment Levels (Secondary Sectors)", tex = TRUE) %>% print

# difference in logs only
coal_emp_mods_disc[lhs = c("diff_log_emp_coal", "diff_log_emp_coal_sales", "diff_log_emp_coal_ff_gen", "diff_log_emp_non_coal")] %>% etable(title = "Change in (log) Employment Levels", tex = TRUE) %>% print
other_emp_mods_disc[lhs = c("diff_log_emp_10", "diff_log_emp_54", "diff_log_emp_61", "diff_log_emp_62", "diff_log_emp_71", "diff_log_emp_72")] %>% etable(title = "Change in (log) Employment Levels (Secondary Sectors)", tex = TRUE) %>% print

other_emp_mods_disc[lhs = "diff_log_emp"]

glht(share_results[[7]], linfct = "mines_diff = 0") %>% summary
for(type in c("2 - medium", "3 - high")){
  glht(share_results[[7]], linfct = paste0("mines_diff + `coal_share_emp_mean_discrete", type, ":mines_diff` = 0")) %>% summary() %>% print()
}

# #### Asymmetric attempt with total employment numbers
# asymm_emp <- allcomp_emp %>% 
#   mutate(pos_difflag = ifelse(lag_diff >= 0, 1, 0),
#          pos_difflag2 = ifelse(lag_diff2 >= 0, 1, 0),
#          neg_difflag = ifelse(lag_diff < 0, 1, 0),
#          neg_difflag2 = ifelse(lag_diff2 < 0, 1, 0))
# 
# coal_emp_mods_neg <- feols(.[deps[grepl("coal", deps)]] ~ mines_diff + lag_diff + lag_diff2 + neg_diff:mines_diff + 
#                                                                            neg_difflag:lag_diff + neg_difflag2:lag_diff2 + diff_log_realgdp_pc | 
#                                                                            fips + year, asymm_emp, se = 'twoway')
#                        
# coal_emp_mods_pos <- feols(.[deps[grepl("coal", deps)]] ~ mines_diff + lag_diff + lag_diff2 + pos_diff:mines_diff + 
#                                              pos_difflag:lag_diff + pos_difflag2:lag_diff2 + diff_log_realgdp_pc | 
#                                              fips + year, asymm_emp, se = 'twoway')
# 
# other_emp_mods_neg <- feols(.[deps[!grepl("coal", deps)]] ~ mines_diff + lag_diff + lag_diff2 + neg_diff:mines_diff + 
#                              neg_difflag:lag_diff + neg_difflag2:lag_diff2 + diff_log_realgdp_pc | 
#                              fips + year, asymm_emp, se = 'twoway')
# 
# other_emp_mods_pos <- feols(.[deps[!grepl("coal", deps)]] ~ mines_diff + lag_diff + lag_diff2 + pos_diff:mines_diff + 
#                              pos_difflag:lag_diff + pos_difflag2:lag_diff2 + diff_log_realgdp_pc | 
#                              fips + year, asymm_emp, se = 'twoway')
# 
# 
# for(tt in list("emp_coal$")){
#   coal_emp_mods_pos[lhs = tt] %>% etable(title = tt) %>% print
# }
# 
# codes <- ind_codes %>% filter(nchar(industry_code) == 2 & industry_code != "99") %>% select(industry_code, industry_title) %>% unique
# codes_list <- as.list(codes$industry_code)
# names(codes_list) <- as.list(codes$industry_title)
# 
# 
# for(k in codes_list){
#   other_emp_mods[lhs = k] %>% etable(title = names(codes_list[codes_list == k])) %>% print
# }
# 
# # differences only
# c(coal_emp_mods[lhs = c("diff_emp_coal", "diff_emp_coal_sales", "diff_emp_coal_ff_gen", "diff_emp_non_coal")], other_emp_mods[lhs = c("diff_emp_10", "diff_emp_55", "diff_emp_71", "diff_emp_72") ]) %>% etable(title = "Change in Employment Levels", tex = TRUE) %>% print
# 
# # difference in logs only
# c(coal_emp_mods[lhs = c("diff_log_emp_coal", "diff_log_emp_coal_sales", "diff_log_emp_coal_ff_gen", "diff_log_emp_non_coal")], other_emp_mods[lhs = c("diff_log_emp_10", "diff_log_emp_61", "diff_log_emp_62", "diff_log_emp_71", "diff_log_emp_72")]) %>% etable(title = "Change in (log) Employment Levels", tex = TRUE) %>% print
# 

# dep_lists <- tttttt$dep_var
# names(dep_lists) <- tttttt$sector_name
# dep_lists <- dep_lists[!is.na(names(dep_lists))]

##### Controlling for coal employment
#coal_control <-
#feols(diff_log_emp_coal ~ diff_underground_active_n + l1_diff_underground_active_n + l2_diff_underground_active_n + diff_log_realgdp + diff_log_pop  | fips + year, allcomp_emp, se = 'twoway')


# # Data for Moritz
# 
# allcomp_sub <- allcomp %>% 
#   left_join(., emp_df, by = c("fips", "year")) %>% 
#   select(-c(state.y, county.y, fipstate.y, state, county_name)) %>% 
#   rename(state = state.x, fipstate = fipstate.x, county = county.x)
# 
# FE_diffuer <- as.formula("diff_uer ~ mines_diff + lag_diff + lag_diff2 + diff_log_realgdp_pc | fips + year")
# feols(FE_diffuer, allcomp_sub, se = 'twoway') %>% summary
# # Equivalent to above
# feols(FE_diffuer, allcomp_sub, cluster = c("fips", "year")) %>% summary
# 
# #allcomp_sub %>% saveRDS(here("rep_data.RDS"))
# 
# test <- readRDS(here("rep_data.RDS"))
# feols(diff_uer ~ mines_diff + lag_diff + lag_diff2 + diff_log_realgdp_pc | fips + year, test, se = 'twoway') %>% summary
# 
