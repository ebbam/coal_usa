################################################################################
# 2021 CE Efforts
# Plant Dataset Cleaning
# Ebba Mark ebba.mark@nuffield.ox.ac.uk
#
# 21 March 2022
################################################################################

rm(list = ls())
library(tidyverse)
library(rstatix)
library(dplyr)
library(readxl)
library(usmap)
library(pmdplyr)
library(writexl)

################################################################################

############################################
################# HELPERS ##################
############################################

load("/Users/ebbamark/OneDrive - Nexus365/Ebba Dissertation Materials/Preliminary Regression Components/countyfipstool20190120-1.RData")
fips_addons <- read_excel("/Users/ebbamark/OneDrive - Nexus365/Ebba Dissertation Materials/Preliminary Regression Components/fips_matching_addons.xlsx")
colnames(table)[1] <- "state"
colnames(table)[2] <- "state_abbrev"
colnames(table)[5] <- "county"
fips_matching <- table[,c("state", "state_abbrev", "county","fips")]
fips_matching_full <- rbind(fips_matching, fips_addons)
fips_matching_full <- subset(fips_matching_full, fips != "999999")


## Function to clean column for matching ##
cleanDf <- function(df){
  colnames(df)[which(names(df) == "Mine County")] <- "county"
  colnames(df)[which(names(df) == "Mine State")] <- "state"
  colnames(df)[which(names(df) == "State")] <- "state"
  colnames(df)[which(names(df) == "County")] <- "county"
  colnames(df)[which(names(df) == "Coal Supply Region" | names(df) == "Mine Basin")] <- "region"
  colnames(df)[which(names(df) == "Production (short tons)")] <- "production_shorttons"
  if("Average Employees" %in% names(df)){
    colnames(df)[which(names(df) == "Average Employees")] <- "avg_employees"
    df$avg_employees <- as.numeric(df$avg_employees)}else{df$avg_employees <- 0}
  if("Labor Hours" %in% names(df)){
    colnames(df)[which(names(df) == "Labor Hours")] <- "labor_hours"
    df$labor_hours <- as.numeric(df$labor_hours)}else{df$labor_hours <- 0}
  colnames(df)[which(names(df)=="Mine Status")] <- "status"
  
  df$county <- gsub("/[^/]*$", "", df$county)
  df$county <- tolower(df$county)
  df$state <- gsub("\\s*\\([^\\)]+\\)","", df$state)
  
  return(df)
}

## Function to count observations by key ##
countByKey <- function(df){
  return(count(df, fips))
}

## Function to add state abbreviations

state_to_abbrev <- function(st){
  return(state.abb[which(state.name == toString(st))])
}

## Function to identify FIPS code

fips_format <- function(fipscode){
  if (is.na(fipscode)) return("00000")
  fips_c = as.character(fipscode)
  test_chars = nchar(fips_c)
  if (is.null(fipscode) | is.na(fipscode)) {
    return (nchar(trunc(fipscode)))
  }
  else if (test_chars < 4) {
    return (fips_c)
  }
  else if (test_chars < 5) {
    return (paste0(0, fips_c, sep = ""))
  }
  else {
    return (fips_c)
  }
}


## Function to identify n last chars of a string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


################################################################################
################### ####################################### ####################
################### ################### ################### ####################
################### ## Load in plant data from 1990-2000 ## ####################
################### ################### ################### ####################
################### ################### ################### ####################
################################################################################
###############################################
###############################################
###############################################
############# MINE CODE #######################
## 3. Load & clean Mine Data ##################

setwd('/Users/ebbamark/OneDrive - Nexus365/Ebba Dissertation Materials/Preliminary Regression Components/Coal Mines MSHA EI-7A Dataset')


# Create empty dataframe to fill with observations of active mines, closed mines, planned mines, total mines, region, labor hours, production, average employees, labor hours
final_mine_df <- data.frame()
on_patterns = c("Active", "active", "-", "Inactive")
off_patterns = c("closed", "abandoned")
# Create empty df to accommodate counties that could not be assigned a correct FIPS code
unmatched <- data.frame()
# Iterate through each excel file containing coal mine production info from 1990 to 2020
# for (year in 1990:2020) {
for (year in 2000:2020) {
  # Skip first rows with empty values
  skip = 2
  if(year %in% c(2005,2006,2011,2012,2018, 2019)){
    df_m <- read.xls(here(paste0("data/coalpublic",as.character(year), ".xls")), skip = skip, check.names = FALSE)
    if(year == 2012){
      df_m <- df_m %>% mutate(region = NA)
    }
  }else{
    url <- paste0("https://www.eia.gov/coal/data/public/xls/coalpublic", as.character(year), ".xls")
    df_m <- read.xls(url, skip = skip, check.names = FALSE)
  }
  #df_m <- read_excel(paste0("coalpublic", as.character(year), ".xlsx"), skip=skip)
  #df_m <- read_excel("coalpublic2001.xlsx", skip=skip)
  # Changes variable names and simplify values (lowercase, remove apostrophes, etc)
  df_m <- cleanDf(df_m)
  
  print(unique(df_m$status))
  # Sometimes a "Refuse Recovery" is written instead of a state and county name, 
  # the following loop extracts the state from the Company Address variable.
  for(i in 1:length(df_m$state)){
    st = df_m$state[i]
    st_clean <- gsub("\\s*\\([^\\)]+\\)","", as.character(st))
    if (st_clean == "Refuse Recovery" ) {
      df_m$state_abbrev[i] <- regmatches(df_m$`Operating Company Address`[i],regexpr("[A-Z][A-Z]",df_m$`Operating Company Address`[i]))
    }
    else{
      df_m$state_abbrev[i] <- state_to_abbrev(st_clean)
    }
  }
  
  # Assign FIPS code to each observation
  df_m_fips <- merge(df_m, fips_matching_full, by=c("state_abbrev", "county"), all.x = TRUE)

  # Identify observations that failed to match with a correct FIPS code
  # NOTE: 53 unmatched observations between 1990 and 1997. Need to be handled if planning to include prior to 1997.
  n_unmatched <- subset(df_m_fips, is.na(df_m_fips$fips))
  n_unmatched <- n_unmatched[,c("state_abbrev", "county", "Year", "Operating Company Address")]
  unmatched <- rbind(unmatched, n_unmatched)
                     
  df_m_fips <- subset(df_m_fips, !is.na(df_m_fips$fips))
  colnames(df_m_fips)[which(names(df_m_fips)=="fips")] <- "fips1"
  df_m_fips$fips <- lapply(df_m_fips$fips1, function(x) sapply(x, fips_format))
  #unmatched <- subset(df_m_fips, df_m_fips$fips == "00000")
  
  fips_list <- list(unique(df_m_fips$fips))
  df_mine <- data.frame('fips' = unlist(fips_list))
  
  for(i in 1:length(df_mine$fips)){
    m = df_mine$fips[i]
    df_mine$state[i] <- fips_info(m)$abbr
    df_mine$active_mines[i] <- length(unique(which(df_m_fips$fips == m & grepl(paste(on_patterns, collapse = "|"), df_m_fips$status))))
    df_mine$closed_mines[i] <-  length(unique(which(df_m_fips$fips == m & grepl(paste(off_patterns, collapse = "|"), df_m_fips$status))))
    df_mine$planned_mines[i] <- sum(df_m_fips$fips == m & df_m_fips$status == "New, under construction")
    df_mine$total_mines[i] <- sum(df_m_fips$fips == m)
    df_mine$region[i] <- as.list(unique(df_m_fips$region[which(df_m_fips$fips == m)]))
    df_mine$production_shorttons[i] <- sum(as.numeric(df_m_fips$production_shorttons[which(df_m_fips$fips == m)]))
    df_mine$avg_employees[i] <- sum(as.numeric(df_m_fips$avg_employees[which(df_m_fips$fips == m)]))
    df_mine$labor_hours[i] <- sum(as.numeric(df_m_fips$labor_hours[which(df_m_fips$fips == m)]))
  }
  
  df_mine$year <- year
  
  print(year)
  print(nrow(df_mine))
  
  final_mine_df <- rbind(final_mine_df, df_mine)
}

# Remove observations for Alaska
final_mine_df <- subset(final_mine_df, state != "AK")
colnames(final_mine_df)[which(names(final_mine_df) == "active_mines")] <- "active_mines_new"
final_mine_df$fips[which(final_mine_df$fips == "51195")] <- "51955"

# Plot
state_mines <- final_mine_df %>%
  group_by(state, year) %>%
  summarize(active_mines_new = sum(active_mines_new, na.rm = TRUE), closed_mines = sum(closed_mines, na.rm = TRUE), total_mines = sum(total_mines, na.rm = TRUE))

total_mines <- final_mine_df %>% 
  group_by(year) %>%
  summarize(active_mines_new = sum(active_mines_new, na.rm = TRUE), closed_mines = sum(closed_mines, na.rm = TRUE), total_mines = sum(total_mines, na.rm = TRUE))

state_mines %>%
  ggplot(aes(x=year, y=active_mines_new, color=state))+
  geom_line()+
  geom_point()

ggplot(total_mines, aes(x=year, y=active_mines_new))+
  geom_line()+
  geom_point()

state_mines %>%
  ggplot(aes(x=year, y=total_mines, color=state))+
  geom_line()+
  geom_point()

# 303 counties and 27 states have at one point had a coal mine
length(unique(final_mine_df$fips))
length(unique(final_mine_df$state))

regions <- unnest(final_mine_df)
regions <- subset(regions, select = c(fips, year, region))

################################################################################
############################## FINAL PRODUCTS ##################################
################################################################################

#write_xlsx(final_plant_df, "/Users/ebbamark/OneDrive - Nexus365/Ebba Dissertation Materials/FINAL_REG_DATASETS/plant_clean_2003_2020.xlsx")

write_xlsx(final_mine_df, "/Users/ebbamark/OneDrive - Nexus365/Ebba Dissertation Materials/FINAL_REG_DATASETS/mine_clean_1990_2020.xlsx")

################################################################################
################################################################################
################################################################################
####################### SCRATCH WORK/TESTING (DISREGARD) #######################
################################################################################

# st = "Kentucky"
# state_to_abbrev(st)
# state_test = state_to_abbrev(st)
# df_m$state[3] = state_test

#plant_2003$yr_county_and_state = paste("2003_",plant_2003$State, "_", plant_2003$County, sep = "")
#plant_count_2003 <- count(plant_2003, yr_county_and_state)
#plant_count_2003$yr_county_and_state <- gsub(" Borough", "", plant_count_2003$yr_county_and_state)
#plant_count_2003$yr_county_and_state <- tolower(gsub("[[:space:]]", "", plant_count_2003$yr_county_and_state))

#emp_county_result <- merge(emp_county,plant_count_2003,by="yr_county_and_state", all.x = TRUE)

#matched_counties <- emp_county_result %>% filter(!is.na(plants_2004))

# Check matching
#diff <- setdiff(plant_count_2004$yr_county_and_state, matched_counties$yr_county_and_state)

#sum(!is.na(emp_county_result$plants_2005))
# missing 18 matches -- minor errors

##### State Abbreviation Testing
#list_t = c()
#for(i in 1:1360){
#  state = df_m[i,4]
#  print(state)
#  st = state_to_abbrev(state)
#  print(st)
#  list_t
#}

#state.abb[which(state.name == st)]
#df_m$state <- df_m$state_to_abbrev(state)
#df_m <- df_m  %>% mutate(state = state_to_abbrev(state))
#df_m <- df_m  %>% mutate(state = state.abb[which(state.name == toString(st))])

###### Regex to match state name to Refuse Recovery site if necessary to include!
#/
# for(i in 1:length(df_m_clean$state)){
#   print(i)
#   st = df_m_clean$state[i]
#   st_clean <- gsub("\\s*\\([^\\)]+\\)","", as.character(st))
#   if (st_clean == "Refuse Recovery" ) {
#     df_m$state = regmatches(df_m$`Operating Company Address`,regexpr("[A-Z][A-Z]",df_m$`Operating Company Address`))
#   }
#   else
#     df_m_clean$state[i] = state_to_abbrev(st_clean)
#   # sal <- lapply(df$state, state_to_abbrev)
# }
# /#

# merge_test1 <- subset(cbp_emp_total, !is.na(emp))
# merge_test2 <- subset(cbp_coal, !is.na(emp))
# match(merge_test1$fips, merge_test2$fips)
# 
# results1 <- subset(merge_test2, !(merge_test2$fips %in% merge_test1$fips))
# print(results1)
# <- merge_test2$fips[!merge_test1$fips]
# sum(!is.na(cbp_emp_total$emp))
# 
# sum(!is.na(cbp_coal$emp))

# Test for statewide observations in large dataset
#sum(substrRight(cbp_emp_total$fips, 3) == "999")

# fips_1 = list()
# for (el in df_p_fips$fips1) {
#   t <- fips_format(el)
#   if( !is.na(t) && ( nchar(t) > 4 )) {print (t)}
# }
# 
# x = "13067"
# y = 12033
# l = "123456"
# z = NA
# fips_format(x)
# fips_format(y)
# fips_format(z)
# fips_format(l)
# 
# df_p_fips$fips <- lapply(df_p_fips$fips1, function(x) sapply(x, fips_format))
# 
# df_p_fips$fips <- apply(df_p_fips$fips1, 1, fips_format)
# 
# sum((nchar(df_p_fips$fips1 ) != 5))
# typeof(df_p_fips$fips)
# 
# sum(df_m_fips$fips == "01073")
# sum(df_m_fips$fips == "01073" & grepl("Active", df_m_fips$status, fixed = TRUE))
# sum(df_m_fips$fips == "01073" & which(grepl(paste(patterns, collapse = "|"), df_m_fips$status)))
# #grepl("closed", df_m_fips$status, fixed = TRUE) | grepl("abandoned", df_m_fips$status, fixed = TRUE))
# 
# patterns = c("closed", "abandoned")
# length(unique(which(df_m_fips$fips == "01073" & grepl(paste(patterns, collapse = "|"), df_m_fips$status))))


#unbal_test <- table(index(emp_county_bal), useNA = "ifany")

#unbal_test <- as.data.frame(table(emp_county_bal$fips))

# unbalanced_test <- as.data.frame(names(which(table(emp_county_fin$fips) != 30)))

# unbal_df <- data_frame()
# unbalanced_list <- list("02105","02195","02198","02201","02230","02232","02275","02280","02282","11001")

# for (el in unbalanced_list) {
#   sub <- subset(emp_county_fin, emp_county_fin$fips == el)
#   unbal_df <- rbind(unbal_df, sub)
# }
# 
# unbal_df$year <- as.numeric(unbal_df$year)
# 
# balanced_test <- panel_fill(unbal_df, .set_NA=c("labour_force", "employed", "unemployed", "uer"),
#                             .min = min(as.numeric(emp_county_fin$year)), 
#                             .max = max(as.numeric(emp_county_fin$year)), 
#                             .i = fips, .t = year)
# 
# is.pbalanced(balanced_test)


# # Mine only merged dataset:
# cbp_emp_m <- merge(cbp_emp_total, final_mine_df , by=c("year","fips"), all.x = TRUE)
# cbp_emp_m$active_mines[is.na(cbp_emp_m$active_mines)] <- 0
# cbp_emp_m$closed_mines[is.na(cbp_emp_m$closed_mines)] <- 0
# cbp_emp_m$planned_mines[is.na(cbp_emp_m$planned_mines)] <- 0
# cbp_emp_m$total_mines[is.na(cbp_emp_m$total_mines)] <- 0
# cbp_emp_m$emp[is.na(cbp_emp_m$emp)] <- 0
# cbp_emp_m$est[is.na(cbp_emp_m$est)] <- 0
# 
# write_xlsx(cbp_emp_m, "/Users/ebbamark/OneDrive - Nexus365/Ebba Dissertation Materials/cbp_emp_mines_only.xlsx")
# 
# # Mine & plants merged dataset:
# cbp_mp_all <- merge(cbp_emp_p, final_mine_df, by=c("year","fips"), all.x = TRUE)
# cbp_mp_all$active_mines[is.na(cbp_mp_all$active_mines)] <- 0
# cbp_mp_all$closed_mines[is.na(cbp_mp_all$closed_mines)] <- 0
# cbp_mp_all$planned_mines[is.na(cbp_mp_all$planned_mines)] <- 0
# cbp_mp_all$total_mines[is.na(cbp_mp_all$total_mines)] <- 0
# cbp_mp_all$emp[is.na(cbp_mp_all$emp)] <- 0
# cbp_mp_all$est[is.na(cbp_mp_all$est)] <- 0
# 
# # (!) Only 645 observations have both mines and plants; 5536 observations with mines; 3635 observations with plants
# sum(cbp_mp_all$total_mines > 0 & cbp_mp_all$total_plants > 0)
# sum(cbp_mp_all$total_mines > 0)
# sum(cbp_mp_all$total_plants > 0)
# write_xlsx(cbp_emp_p, "/Users/ebbamark/OneDrive - Nexus365/Ebba Dissertation Materials/cbp_emp_plants_only.xlsx")
# write_xlsx(cbp_mp_all, "/Users/ebbamark/OneDrive - Nexus365/Ebba Dissertation Materials/cbp_emp_mines_plants.xlsx")
# 
# # Subset of mine & plants merged dataset for counties with mines and/or plants
# # coal_counties_only <- subset(cbp_mp_all, total_mines > 0 | total_plants > 0)
# 
# overlap_years <- subset(cbp_mp_all,year >= 2003)
# 
# active_counties <- data.frame()
# for (yr in 1990:2019) { 
#   # Active counties in yr
#   active_counties = sum(emp_county_result_m$mines != 0 & emp_county_result_m$year == yr)
#   # Active mines in yr
#   if (emp_county_result_m$year == yr) {
#     sum(emp_county_result_m$mines)
#   }
#   a_c = paste(yr,active_counties)
#   print(a_c)
#   df_ac <- data.frame(yr, a_c, a_m)
#   active_counties <- rbind(active_counties, df_ac)
# }
