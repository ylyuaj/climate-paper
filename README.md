README:
This repository contains the processed data (afb_working.Rdata) and R scripts needed to replicate the main analysis for the paper Climate-induced Citizen Noncompliance Undermines State Capacity in Africa.

Software:
Version: R-4.3.0
Download at https://cran.r-project.org/bin/windows/base/old/4.3.0/

Data availability:
The Afrobarometer Public Opinion Survey data is available from the Afrobarometer repository at https://www.afrobarometer.org/data/merged-data/. Geolocation data can be accessed upon approval from Afrobarometer to ensure participant privacy. The Standardized Precipitation Evapotranspiration Index (SPEI) data is available at https://spei.csic.es/database.html. Country-level tax revenue data is sourced from the UNU-WIDER Government Revenue Dataset at https://www.wider.unu.edu/project/grd-government-revenue-dataset. The State Capacity Dataset can be accessed at https://websites.umich.edu/~jkhanson/state_capacity.html. Dam data is obtained from the GOODD Global Database at https://www.globaldamwatch.org/goodd. Renewable energy consumption data is available from the World Bank at https://data.worldbank.org/. Disaster event data is available from NASA's Socioeconomic Data and Applications Center at https://www.earthdata.nasa.gov/data/catalog/sedac-ciesin-sedac-pend-gdis-1.00. GlobCover 2009 (Global Land Cover Map) can be accessed at https://due.esrin.esa.int/page_globcover.php. 

Instructions:
1. Open R script "data_analysis.R"
2. Load Processed Data: Begin by loading the processed data from afb_working.RData into your R environment.
3. Run Analysis: Execute the R script data_analysis.R to perform the data analysis. This script will generate all required regression tables and figures.
4. View Outputs: After running the analysis, check the Output directory for the generated figures and tables.
(it may take around 40 mins to run on data)
