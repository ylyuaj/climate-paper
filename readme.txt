README
This repository contains the processed data and R scripts needed to replicate the main analysis for the paper Climate-induced Citizen Noncompliance Undermines State Capacity in Africa.

Software
Version: R-4.3.0
Download at https://cran.r-project.org/bin/windows/base/old/4.3.0/

Contents
1. Processed Data: The processed data used in the analysis is located in the file Data/working_data/afb_working.RData.
	a. afb_working.RData: The primary dataset used for regression analysis in the study.
	b. country_panel.rds: Country-level panel data, used to visualize the relationship between tax capacity, wealth, and state capacity.
	c. tax_gdp.rds: A dataset for mapping the global relationship between tax capacity and state capacity.
2. R Scripts: The R script "data_analysis.R" in the Codes directory contains all the code necessary to generate regression tables and figures for the data analysis.
3. Output: All resulting figures and tables from the analysis will be saved in the Output directory.

Instructions
1. Open R script "data_analysis.R"
2. Load Processed Data: Begin by loading the processed data from Data-working_data/afb_working into your R environment.
3. Run Analysis: Execute the R script data_analysis.R to perform the data analysis. This script will generate all required regression tables and figures.
4. View Outputs: After running the analysis, check the Output directory for the generated figures and tables.
(it may take around 40 mins to run on data)

Acknowledgment
We do not have the right to distribute the data obtained from the Afrobarometer team. The Afrobarometer geolocation data is available for review and replication at https://www.afrobarometer.org/geocoded-data/. Please note that the data is provided solely for the purpose of replication and review and is not authorized for further distribution.