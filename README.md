# README: Replicating "Spatial-temporal dynamics of employment shocks in declining coal mining regions and potentialities of the 'just transition'"
### Working paper currently available on arxiv: https://arxiv.org/abs/2211.12619
### Authors: Ebba Mark, Ryan Rafaty, Moritz Schwarz
### Repository Author: Ebba Mark


The following repository allows for the replication of "Spatial-temporal dynamics of employment shocks in declining coal mining regions and potentialities of the 'just transition'" - Mark, Rafaty, Schwarz. Currently available on arxiv: https://arxiv.org/abs/2211.12619

Although the repository includes additional files with various scoping work, it is recommended that anyone wishing to replicate the work/results use the "replication_code" folder which provides all code files and data needed to replicate all results of the manuscript. 

The replication_code folder contains 3 folders:

**code** containing 6 files:
**main_econometrics.Rmd**
- Summary: Conducts the econometric results reported in the main text and a few robustness checks reported in the Supplementary Materials.
- Input: Econometrics_Final.xlsx, FIPSModificationsVA.xlsx,cc_clusters_251.xlsx, county_adjacency.txt, useful_functions.R, dicts.R
- Output: figure_1a.jpg, figure_1b.jpg, figure_2.jpg, figure_3.jpg, supp_fig_s5a.jpg, supp_fig_s5b.jpg, supp_fig_s5c.jpg, figure_6.jpg

**main_typology.Rmd**
- Summary: Conducts the clustering analysis used to construct the vulnerability typology reported in the main text and Supplementary Materials.
- Input: Econometrics_Final.xlsx; Typology_Final.xlsx; shape files in CoalFieldsUS
- Output: cc_clusters_251.xlsx, supp_fig_s2.jpg, supp_fig_s3.jpg, supp_fig_s4.jpg, figure_4.jpg, figure_5.jpg

**robustness_checks.Rmd**
- Summary: Conducts the various additional robustness checks reported
in the Supplementary Materials of the main manuscript including analysis of mine size heterogeneity, county-level dependence on coal mining, calculation of total employment loss numbers, exploration of persistent unemplyment rate effects, panel error corrected model, renewable energy investments. Each code block is labelled as it is in the Supplementary Materials to ease reading/searching.
- Input:  Econometrics_Final.xlsx, allcomp_cap_prod.RDS, FIPSModificationsVA.xlsx, cc_clusters_251.xlsx, county_adjacency.txt, industry-titles.xlsx, qcew_compiled_raw_2000_2022.RDS
- Output: supp_fig_s7.jpg, supp_fig_s6

**useful_functions.R**
- Summary: Provides various helper functions for conducting analysis in main_econometrics.Rmd and robustness_checks.Rmd. Mainly these are functions that make certain regression output compatible with summary table functions and coefficient plot functions. This also outlines the Louisiana counties that have missing values in our main dataset to ensure that these are excluded in the spatial neighbor matrix generated in main_econometrics.Rmd
- Input: none

**dicts.R**
- Summary: Provides dictionaries needed in fixest package, model summary tables, and summary statistics tables.
- Input: none

**prod_cap_cleaning.R**:
- Summary: cleans and compiles county-level coal production data and state-level productive capacity data available from the Energy Information Admistration. Resulting file allcomp_cap_prod.RDS is used in robustness_checks.Rmd
- Input: Econometrics_Final.xls, calls annual data on coal production by mine either directly from the EIA via URL or, where necessary, from locally saved files where syntax errors make automatic file reading via url difficult. 
- Output: allcomp_cap_prod.RDS

**data** containing 7 files, 2 subfolders:
- Econometrics_Final.xlsx: input data to analysis in econometrics and robustness checks.
- Typology_Final.xlsx: input data to clustering/typology analysis in main_typology.Rmd
- cc_clusters_251.xlsx: lookup table for fips codes with identified county "types" in the clustering analysis.
- CoalFieldsUS/...: folder with relevant shape files for including US coal fields in maps.
- county_adjacency.txt: county adjacency matrix used for spatial econometric specifications
- FIPSModificationsVA.xlsx: crosswalk between selected Virginia fips codes that differ between the Bureau of Economic Analysis and Census Bureau
- industry-titles.xlsx: NAICS code lookup table
- production_capacity/...: includes files used in prod_cap_cleaning.R as well as final compiled allcomp_cap_prod.RDS used for robustness checkes
- qcew_compiled_raw_2000_2022.RDS: compiled sector-level employment numbers compiled from QCEW.

**output** folder contains any figures used in the main text and supplementary materials of the manuscript labeled "figure_" or "supp_fig_", respectively.
