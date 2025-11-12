# Code Description -------------------------------------------------------
# This script replicates the results of the paper： Climate-induced Citizen Noncompliance Undermines State Capacity in Africa
  
# Clear the workspace
rm(list = ls())

# Load required packages
pkgs <- c("dplyr", "tidyr", "broom", "haven", "readr", "tidyverse", "stargazer",
          "mfx", "texreg", "lfe", "cowplot", "readxl","future.apply","gridExtra","RColorBrewer")
sapply(pkgs, require, character.only = TRUE)

# Set working directory
getwd()
setwd(dirname(getwd()))

# Load the working dataset
load("Data/working_data/afb_working.RData")

# Attach the dataset for easier reference
attach(afb)



# Define Weather Extremes ------------------------------------------------

# Categorize SPEI_24_d (24-month SPEI from 1 year before the interview)
# Exceptional drought: SPEI ≤ -1.5
afb$edry <- ifelse(afb$spei_24_d <= -1.5, 1, 0)
# Limited drought: -1.5 < SPEI < -0.5
afb$ldry <- ifelse(afb$spei_24_d > -1.5 & afb$spei_24_d < -0.5, 1, 0)
# Exceptional wet: SPEI ≥ 1.5
afb$ewet <- ifelse(afb$spei_24_d >= 1.5, 1, 0)
# Limited wet: 0.5 < SPEI < 1.5
afb$lwet <- ifelse(afb$spei_24_d > 0.5 & afb$spei_24_d < 1.5, 1, 0)
# Normal: -0.5 ≤ SPEI ≤ 0.5
afb$normal <- ifelse(afb$spei_24_d >= -0.5 & afb$spei_24_d <= 0.5, 1, 0)

# Categorize SPEI_6_d (6-month SPEI from 1 year before the interview)
afb$edry6 <- ifelse(afb$spei_6_d <= -1.5, 1, 0)
afb$ldry6 <- ifelse(afb$spei_6_d > -1.5 & afb$spei_6_d <= -0.5, 1, 0)
afb$ewet6 <- ifelse(afb$spei_6_d >= 1.5, 1, 0)
afb$lwet6 <- ifelse(afb$spei_6_d > 0.5 & afb$spei_6_d < 1.5, 1, 0)

# Categorize SPEI_12_d (12-month SPEI from 1 year before the interview)
afb$edry12 <- ifelse(afb$spei_12_d <= -1.5, 1, 0)
afb$ldry12 <- ifelse(afb$spei_12_d > -1.5 & afb$spei_12_d <= -0.5, 1, 0)
afb$ewet12 <- ifelse(afb$spei_12_d >= 1.5, 1, 0)
afb$lwet12 <- ifelse(afb$spei_12_d > 0.5 & afb$spei_12_d < 1.5, 1, 0)

# Categorize SPEI_12_i (concurrent weather: 12-month SPEI at the time of the interview)
afb$edryi <- ifelse(afb$spei_12_i <= -1.5, 1, 0)
afb$ldryi <- ifelse(afb$spei_12_i > -1.5 & afb$spei_12_i <= -0.5, 1, 0)
afb$eweti <- ifelse(afb$spei_12_i >= 1.5, 1, 0)
afb$lweti <- ifelse(afb$spei_12_i > 0.5 & afb$spei_12_i < 1.5, 1, 0)

# Categorize SPEI_48_d (48-month SPEI from 1 year before the interview)
afb$edry48 <- ifelse(afb$spei_48_d <= -1.5, 1, 0)
afb$ldry48 <- ifelse(afb$spei_48_d > -1.5 & afb$spei_48_d <= -0.5, 1, 0)
afb$ewet48 <- ifelse(afb$spei_48_d >= 1.5, 1, 0)
afb$lwet48 <- ifelse(afb$spei_48_d > 0.5 & afb$spei_48_d < 1.5, 1, 0)



# Baseline Models ------------------------------------------------------------

# Model 1: Base model without region and month fixed effects
base1 <- felm(refuse_to_pay ~ edry + ewet + ldry + lwet + age + I(age^2) |
                gender + edu + rural_sample + race + employ + occupation |
                0 | cluster, data = afb)

# Model 2: Base model with region fixed effects
base2 <- felm(refuse_to_pay ~ edry + ewet + ldry + lwet + age + I(age^2) |
                region + gender + edu + rural_sample + race + employ + occupation |
                0 | cluster, data = afb)

# Model 3: Base model with region and month fixed effects
base3 <- felm(refuse_to_pay ~ edry + ewet + ldry + lwet + age + I(age^2) |
                region + month + gender + edu + rural_sample + race + employ + occupation |
                0 | cluster, data = afb)

# Model 4: Base model with region, month, and country-year fixed effects
base4 <- felm(refuse_to_pay ~ edry + ewet + ldry + lwet + age + I(age^2) |
                region + year_month + gender + edu + rural_sample + race + employ + occupation |
                0 | cluster, data = afb)

# Model 5: Base model with region, year-month, and country-year fixed effects
base5 <- felm(refuse_to_pay ~ edry + ewet + ldry + lwet + age + I(age^2) |
                region + year_month + country_year + gender + edu + rural_sample + race + employ + occupation |
                0 | cluster, data = afb)

models <- list(base1, base2, base3, base4, base5)

# Display model summary
stargazer(models, type = "text",
          omit = c("age"),
          digits = 3)
          

# Robustness: Fixed Effects Sensitivity ----------------------------------------------------
# Define base formula as a character string
afb$country_month <- paste0(afb$country,"",afb$month) # create a country-month variable
base_formula_str <- "refuse_to_pay ~ edry + ewet + ldry + lwet + age + I(age^2)"
controls <- "gender + edu + rural_sample + race + employ + occupation"

# Create model formulas as character strings
model_list <- list(
  base6  = paste0(base_formula_str, " | region + country_year + ", controls, " | 0 | cluster"),
  base7  = paste0(base_formula_str, " | region + country_year + month+ ", controls, " | 0 | cluster"),
  base8 = paste0(base_formula_str, " | region + country_year + country_month+ ", controls, " | 0 | cluster"),
  base9 = paste0(base_formula_str, " | region  + country_year + country_month+ year_month+ ", controls, " | 0 | cluster")
)

# Run models using as.formula()
model_results <- lapply(model_list, function(fml_str) {
  felm(as.formula(fml_str), data = afb)
})

# Assign model names
names(model_results) <- names(model_list)

stargazer(model_results, type = "text",
          omit = c("age"),digits = 3)

## Table: Robustness Checks: Alternative Fixed Effects Specifications ------
# Text output with additional formatting
stargazer(model_results, type = "text",
          omit = c("age"),
          digits = 3,
          dep.var.caption = "Dependent variable: Refuse to Pay",
          dep.var.labels.include = FALSE,
          add.lines = list(
            c("Region FE",        "Y", "Y", "Y", "Y"),
            c("Country-Year FE",  "Y", "Y", "Y", "Y"),
            c("Month FE",         "N", "Y", "N", "N"),
            c("Country-Month FE", "N", "N", "Y", "Y"),
            c("Year-Month FE",    "N", "N", "N", "Y")),
          covariate.labels = c("Exceptional Drought", "Exceptional Wet", "Limited Drought", "Limited Wet"),
          omit.stat = c("rsq", "adj.rsq", "ser"))

stargazer(model_results, type = "html",
          out = "Output/Tables/robust_FEs.doc",
          omit = c("age"),
          digits = 3,
          dep.var.caption = "Dependent variable: Refuse to Pay",
          dep.var.labels.include = FALSE,
          add.lines = list(
            c("Region FE",        "Y", "Y", "Y", "Y"),
            c("Country-Year FE",  "Y", "Y", "Y", "Y"),
            c("Month FE",         "N", "Y", "N", "N"),
            c("Country-Month FE", "N", "N", "Y", "Y"),
            c("Year-Month FE",    "N", "N", "N", "Y")),
          covariate.labels = c("Exceptional Drought", "Exceptional Wet", "Limited Drought", "Limited Wet"),
          omit.stat = c("rsq", "adj.rsq", "ser"))


# Robustness: Weather Controls (Preferred Specification) ----------------------------------------------------

# Model with fixed effects, individual controls, and additional weather controls
final <- felm(refuse_to_pay ~ edry + ewet + ldry + lwet +
                edry6 + ewet6 + ldry6 + lwet6 +
                edry12 + ewet12 + ldry12 + lwet12 +
                edryi + eweti + ldryi + lweti +
                n_drought_2ys + n_flood_2ys + n_earthquake_2ys + 
                n_extreme_temp_2ys + n_storm_2ys + sd_24m+
                age + I(age^2) |
                region + year_month + country_year + 
                gender + edu + rural_sample + race + employ + occupation |
                0 | cluster, data = afb)

stargazer(final, type = "text")

## Table: Impact of Weather Extremes on Citizen Noncompliance ----
# List of models to be summarized
models <- list(base1, base2, base3, base4, base5, final)

# Display model summary with additional formatting options
stargazer(models, type = "text",
          omit = c("age", "n_", "6", "12", "i", "sd"),
          digits = 3,
          dep.var.caption = "Dependent variable: Refuse to Pay",
          dep.var.labels.include = FALSE,
          add.lines = list(c('Region FE', 'N', 'Y', 'Y', 'Y', 'Y', 'Y'),
                           c('Month FE', 'N', 'N', 'Y', 'N', 'N', 'N', 'N'),
                           c('Year-Month FE', 'N', 'N', 'N', 'Y', 'Y', 'Y'),
                           c('Country-Year FE', 'N', 'N', 'N', 'N', 'Y', 'Y'),
                           c('Weather Control', 'N', 'N', 'N', 'N', 'N', 'Y')),
          covariate.labels = c("Exceptional Drought", "Exceptional Wet", "Limited Drought", "Limited Wet"),
          omit.stat = c("rsq", "adj.rsq", "ser"))

# Export model summary to an HTML file
stargazer(models, type = "html",
          out = "Output/Tables/baseline.doc",
          omit = c("age", "n_", "6", "12", "i", "sd"),
          digits = 3,
          dep.var.caption = "Dependent variable: Refuse to Pay",
          dep.var.labels.include = FALSE,
          add.lines = list(c('Region FE', 'N', 'Y', 'Y', 'Y', 'Y', 'Y'),
                           c('Month FE', 'N', 'N', 'Y', 'N', 'N', 'N', 'N'),
                           c('Year-Month FE', 'N', 'N', 'N', 'Y', 'Y', 'Y'),
                           c('Country-Year FE', 'N', 'N', 'N', 'N', 'Y', 'Y'),
                           c('Weather Control', 'N', 'N', 'N', 'N', 'N', 'Y')),
          covariate.labels = c("Exceptional Drought", "Exceptional Wet", "Limited Drought", "Limited Wet"),
          omit.stat = c("rsq", "adj.rsq", "ser"))



# Tax Institutional Change ---------------------------------------------
# diff_avoid_tax: =1 if it is difficult to avoid paying taxes
# diff_pay: =1 if it is difficult to find out what taxes or fees to pay
# tax_info = 1 if it is not difficult to find out what taxes or fees to pay
afb$tax_info <- ifelse(afb$diff_pay==1,0,1) # This transformation is for easier interpretation

# Impact of institutional variables on refusal to pay taxes/fees
instituion <- felm(refuse_to_pay ~ diff_avoid_tax + tax_info +
                     age + I(age^2) + sd_24m +
                     n_drought_2ys + n_flood_2ys + n_earthquake_2ys + 
                     n_extreme_temp_2ys + n_storm_2ys |
                     edry6 + ewet6 + ldry6 + lwet6 +
                     edry12 + ewet12 + ldry12 + lwet12 +
                     edryi + eweti + ldryi + lweti +
                     region + year_month + country_year +
                     gender + edu + rural_sample + race + employ + occupation |
                     0 | cluster, data = afb)

# Impact of weather variables on difficulty in avoiding taxes
diff_avoid_tax <- felm(diff_avoid_tax ~ edry + ewet + ldry + lwet +
                         age + I(age^2) + sd_24m +
                         n_drought_2ys + n_flood_2ys + n_earthquake_2ys + 
                         n_extreme_temp_2ys + n_storm_2ys |
                         edry6 + ewet6 + ldry6 + lwet6 +
                         edry12 + ewet12 + ldry12 + lwet12 +
                         edryi + eweti + ldryi + lweti +
                         region + year_month + country_year +
                         gender + edu + rural_sample + race + employ + occupation |
                         0 | cluster, data = afb)

# Impact of weather variables on difficulty to find out what taxes or fees to pay
tax_info <- felm(tax_info ~ edry + ewet + ldry + lwet +
                   age + I(age^2) + sd_24m +
                   n_drought_2ys + n_flood_2ys + n_earthquake_2ys + 
                   n_extreme_temp_2ys + n_storm_2ys |
                   edry6 + ewet6 + ldry6 + lwet6 +
                   edry12 + ewet12 + ldry12 + lwet12 +
                   edryi + eweti + ldryi + lweti +
                   region + year_month + country_year +
                   gender + edu + rural_sample + race + employ + occupation |
                   0 | cluster, data = afb)

# Impact of weather variables on refusal to pay with institutional controls
instituion_control <- felm(refuse_to_pay ~ diff_avoid_tax + tax_info + edry + ewet + 
                             ldry + lwet +
                             age + I(age^2) + sd_24m +
                             n_drought_2ys + n_flood_2ys + n_earthquake_2ys + 
                             n_extreme_temp_2ys + n_storm_2ys |
                             edry6 + ewet6 + ldry6 + lwet6 +
                             edry12 + ewet12 + ldry12 + lwet12 +
                             edryi + eweti + ldryi + lweti +
                             region + year_month + country_year +
                             gender + edu + rural_sample + race + employ + occupation |
                             0 | cluster, data = afb)

### Table: Weather Extremes, Institutional Change, and Tax Noncompliance ----
# Summary of institutional impact models
stargazer(diff_avoid_tax, tax_info, type = "text",
          keep = c("dry", "wet", "tax"),
          digits = 3,
          dep.var.caption = "Dependent variable: Tax/Fee Institution",
          dep.var.labels.include = FALSE,
          add.lines = list(c('Region FE', 'Y', 'Y'),
                           c('Year-Month FE', 'Y', 'Y'),
                           c('Country-Year FE', 'Y', 'Y'),
                           c('Control', 'Y', 'Y')),
          covariate.labels = c("Exceptional Drought", "Exceptional Wet", "Limited Drought", "Limited Wet"),
          column.labels = c("Tax Enforcement", "Tax Transparency"),
          omit.stat = c("rsq", "adj.rsq", "ser"))

# Export institutional impact models to HTML
stargazer(diff_avoid_tax, tax_info, type = "html",
          out = "Output/Tables/robust_instituion_change1.doc",
          keep = c("dry", "wet", "tax"),
          digits = 3,
          dep.var.caption = "Dependent variable: Tax/Fee Institution",
          dep.var.labels.include = FALSE,
          add.lines = list(c('Region FE', 'Y', 'Y'),
                           c('Year-Month FE', 'Y', 'Y'),
                           c('Country-Year FE', 'Y', 'Y'),
                           c('Control', 'Y', 'Y')),
          covariate.labels = c("Exceptional Drought", "Exceptional Wet", "Limited Drought", "Limited Wet"),
          column.labels = c("Tax Enforcement", "Tax Transparency"),
          omit.stat = c("rsq", "adj.rsq", "ser"))

# Summary of refusal to pay models with institutional controls
stargazer(instituion, instituion_control, type = "text",
          keep = c("dry", "wet", "tax"),
          digits = 3,
          dep.var.caption = "Dependent variable: Refuse to Pay",
          dep.var.labels.include = FALSE,
          add.lines = list(c('Region FE', 'Y', 'Y'),
                           c('Year-Month FE', 'Y', 'Y'),
                           c('Country-Year FE', 'Y', 'Y'),
                           c('Control', 'Y', 'Y')),
          covariate.labels = c("Tax Enforcement", "Tax Transparency", "Exceptional Drought", "Exceptional Wet", "Limited Drought", "Limited Wet"),
          omit.stat = c("rsq", "adj.rsq", "ser"))

# Export refusal to pay models to HTML
stargazer(instituion, instituion_control, type = "html",
          out = "Output/Tables/robust_instituion_change2.doc",
          keep = c("dry", "wet", "tax"),
          digits = 3,
          dep.var.caption = "Dependent variable: Refuse to Pay",
          dep.var.labels.include = FALSE,
          add.lines = list(c('Region FE', 'Y', 'Y'),
                           c('Year-Month FE', 'Y', 'Y'),
                           c('Country-Year FE', 'Y', 'Y'),
                           c('Control', 'Y', 'Y')),
          covariate.labels = c("Tax Enforcement", "Tax Transparency", "Exceptional Drought", "Exceptional Wet", "Limited Drought", "Limited Wet"),
          omit.stat = c("rsq", "adj.rsq", "ser"))


# Willingness or Behavior? ----------------------------------------------------------------

# Define dependent variables
dependent_vars <- c("would_do", "behavior")

# Initialize a list to store models
models <- list()

# Loop through dependent variables to estimate models
for (dep_var in dependent_vars) {
  # Construct the formula
  formula <- as.formula(paste0(
    dep_var, " ~ edry + ewet + ldry + lwet +
    age + I(age^2) + sd_24m +
    n_drought_2ys + n_flood_2ys + n_earthquake_2ys + 
    n_extreme_temp_2ys + n_storm_2ys |
    edry6 + ewet6 + ldry6 + lwet6 +
    edry12 + ewet12 + ldry12 + lwet12 +
    edryi + eweti + ldryi + lweti +
    region + year_month + country_year +
    gender + edu + rural_sample + race + employ + occupation |
    0 | cluster"
  ))
  
  # Estimate the model and store in the list
  models[[dep_var]] <- felm(formula, data = afb)
}

## Table: Impact of Weather Extremes on Noncompliance Willingness and Stated Behavior ----
# Generate text table
stargazer(models, type = "text",
          keep = c("dry", "wet"),
          digits = 3,
          dep.var.caption = "Dependent variable: Refuse to Pay",
          dep.var.labels.include = FALSE,
          add.lines = list(
            c('Region FE', 'Y', 'Y'),
            c('Year-Month FE', 'Y', 'Y'),
            c('Country-Year FE', 'Y', 'Y'),
            c('Control', 'Y', 'Y')
          ),
          covariate.labels = c("Exceptional Drought", "Exceptional Wet", "Limited Drought", "Limited Wet"),
          column.labels = c("Willingness", "Stated Behavior"),
          omit.stat = c("rsq", "adj.rsq", "ser"))

# Export HTML table
stargazer(models, type = "html",
          out = "Output/Tables/robust_willingness_behavior.doc",
          keep = c("dry", "wet"),
          digits = 3,
          dep.var.caption = "Dependent variable: Refuse to Pay",
          dep.var.labels.include = FALSE,
          add.lines = list(
            c('Region FE', 'Y', 'Y'),
            c('Year-Month FE', 'Y', 'Y'),
            c('Country-Year FE', 'Y', 'Y'),
            c('Control', 'Y', 'Y')
          ),
          covariate.labels = c("Exceptional Drought", "Exceptional Wet", "Limited Drought", "Limited Wet"),
          column.labels = c("Willingness", "Stated Behavior"),
          omit.stat = c("rsq", "adj.rsq", "ser"))


# Heterogeneity Analysis -----------------------------------------------------------

## Occupational/Sectoral Heterogeneity  -------------------------------------------------------------
# Set up parallel processing
plan(multisession, workers = parallel::detectCores() - 1)  # leave 1 core free

# Define the list of occupations for analysis
occupations <- c("Agriculture / Farming / Fishing", 
                 "Formal Employment & Professionals", 
                 "Informal & Small-Scale Business", 
                 "Manual Labor", 
                 "No Job")

# Define model estimation function
estimate_model <- function(occ) {
  subset_data <- subset(afb, occupation == occ)
  felm(refuse_to_pay ~ edry + ewet + ldry + lwet +
         age + I(age^2) + sd_24m +
         n_drought_2ys + n_flood_2ys + n_earthquake_2ys + 
         n_extreme_temp_2ys + n_storm_2ys |
         edry6 + ewet6 + ldry6 + lwet6 +
         edry12 + ewet12 + ldry12 + lwet12 +
         edryi + eweti + ldryi + lweti +
         region + year_month + country_year +
         gender + edu + rural_sample + race + employ |
         0 | cluster, data = subset_data)
}

# Run models in parallel
models <- future_lapply(occupations, estimate_model)
names(models) <- occupations

# Output results
stargazer(models, type = "text",
          keep = c("dry", "wet"),
          digits = 3,
          dep.var.caption = "Dependent variable: Refuse to Pay",
          dep.var.labels.include = FALSE,
          add.lines = list(c('Region FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
                           c('Year-Month FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
                           c('Country-Year FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
                           c('Control', 'Y', 'Y', 'Y', 'Y', 'Y')),
          covariate.labels = c("Exceptional Drought", 
                               "Exceptional Wet", 
                               "Limited Drought", 
                               "Limited Wet"),
          column.labels = c("Agriculture / Farming / Fishing", 
                            "Formal Employment / Professionals", 
                            "Informal / Small-Scale Business", 
                            "Manual Labor", 
                            "No Job"),
          omit.stat = c("rsq", "adj.rsq", "ser"))

# Output occupation models in HTML format
stargazer(models, type = "html",
          out = "Output/Tables/heterogeneity_occupation.doc",
          keep = c("dry", "wet"),
          digits = 3,
          dep.var.caption = "Dependent variable: Refuse to Pay",
          dep.var.labels.include = FALSE,
          add.lines = list(c('Region FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
                           c('Year-Month FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
                           c('Country-Year FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
                           c('Control', 'Y', 'Y', 'Y', 'Y', 'Y')),
          covariate.labels = c("Exceptional Drought", 
                               "Exceptional Wet", 
                               "Limited Drought", 
                               "Limited Wet"),
          column.labels = c("Agriculture / Farming / Fishing", 
                            "Formal Employment / Professionals", 
                            "Informal / Small-Scale Business", 
                            "Manual Labor", 
                            "No Job"),
          omit.stat = c("rsq", "adj.rsq", "ser"))

# Extract coefficients and confidence intervals
results <- lapply(models, function(model) {
  tidy(model, conf.int = TRUE, conf.level = 0.95) %>%
    filter(term %in% c("edry", "ewet", "ldry", "lwet")) %>%
    mutate(occupation = model$call$afb$occupation[2])
})

# Combine results into one data frame
results_df <- bind_rows(lapply(names(results), function(occupation) {
  df <- results[[occupation]]
  df$occupation <- occupation
  return(df)
}))
# Reorder the term factor
results_df$term <- factor(results_df$term, levels = c(  "lwet","ldry","ewet", "edry"))
results_df$occupation <- factor(results_df$occupation, levels = c("No Job", 
                                                                  "Formal Employment & Professionals", 
                                                                  "Manual Labor", 
                                                                  "Agriculture / Farming / Fishing", 
                                                                  "Informal & Small-Scale Business"))

##  Figure: Heterogeneous Effect of Weather Extremes on Citizen \nNoncompliance across Occupations  -----

# Plot
p_heterogeneity_occupation <- ggplot(results_df, aes(x = estimate, y = occupation, color = term)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +  # Dodge points, increased size for visibility
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), 
                width = 0.3, 
                position = position_dodge(width = 0.5),
                size = 2, show.legend = FALSE) +  # Larger error bars, no legend for error bars
  theme_minimal(base_size = 20) +
  labs(
    x = "Coefficient Estimates",
    y = "Occupation",
    color = "Variable") +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0),  # Left-align title
        legend.position = "bottom",
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        axis.title = element_text(size = 22, face = "bold"),
        axis.text.x = element_text(size = 22, face = "bold"),
        axis.text.y = element_text(face = "bold", color = "black", size = 18)) +  # Bold Y-axis text
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 2) +  # Vertical line at x = 0
  scale_color_manual(
    values = c(
      "edry" = "#b2182b",  # Exceptional Drought
      "ewet" = "#2166ac",  # Exceptional Wet
      "ldry" = "#f4a582",  # Limited Drought
      "lwet" = "#92c5de"   # Limited Wet
    ),
    labels = c(
      "edry" = "Exceptional Drought",
      "ewet" = "Exceptional Wet",
      "ldry" = "Limited Drought",
      "lwet" = "Limited Wet"))
ggsave("Output/Figures/heterogeneity_occupation.jpg", p_heterogeneity_occupation,dpi = 300, width = 16, height = 8)

p_heterogeneity_occupation=p_heterogeneity_occupation+
  ggtitle("A. Heterogeneous Effect of Weather Extremes on Citizen \nNoncompliance across Occupations")


# Interaction model for agriculture occupation
afb$agr_occu <- ifelse(afb$occupation == "Agriculture / Farming / Fishing", 1, 0)

agr <- felm(refuse_to_pay ~ edry + ewet + ldry + lwet + agr_occu +
              edry:agr_occu + ewet:agr_occu + ldry:agr_occu + lwet:agr_occu +
              age + I((age)^2) + sd_24m +
              n_drought_2ys + n_flood_2ys + n_earthquake_2ys + 
              n_extreme_temp_2ys + n_storm_2ys |
              edry6 + ewet6 + ldry6 + lwet6 +
              edry12 + ewet12 + ldry12 + lwet12 +
              edryi + eweti + ldryi + lweti +
              region + year_month + country_year +
              gender + edu + rural_sample + race + employ |
              0 | cluster, data = afb)


# Interaction model for informal & small-scale business occupation
afb$informal_small <- ifelse(afb$occupation == "Informal & Small-Scale Business", 1, 0)

informal_small <- felm(refuse_to_pay ~ edry + ewet + ldry + lwet+ informal_small +
                         edry:informal_small + ewet:informal_small + ldry:informal_small + lwet:informal_small +
                         age + I((age)^2) + sd_24m +
                         n_drought_2ys + n_flood_2ys + n_earthquake_2ys + 
                         n_extreme_temp_2ys + n_storm_2ys |
                         edry6 + ewet6 + ldry6 + lwet6 +
                         edry12 + ewet12 + ldry12 + lwet12 +
                         edryi + eweti + ldryi + lweti +
                         region + year_month + country_year +
                         gender + edu + rural_sample + race + employ |
                         0 | cluster, data = afb)


# Interaction model for weather-vulnerable population (agriculture or informal/small-scale business)
afb$vulnerable <- ifelse(afb$occupation %in% c("Agriculture / Farming / Fishing", 
                                               "Informal & Small-Scale Business"), 1, 0)

vulnerable <- felm(refuse_to_pay ~ edry + ewet + ldry + lwet + vulnerable +
                     edry:vulnerable + ewet:vulnerable + ldry:vulnerable + lwet:vulnerable +
                     age + I((age)^2) + sd_24m +
                     n_drought_2ys + n_flood_2ys + n_earthquake_2ys + 
                     n_extreme_temp_2ys + n_storm_2ys |
                     edry6 + ewet6 + ldry6 + lwet6 +
                     edry12 + ewet12 + ldry12 + lwet12 +
                     edryi + eweti + ldryi + lweti +
                     region + year_month + country_year +
                     gender + edu + rural_sample + race + employ |
                     0 | cluster, data = afb)


# Interaction model for cropland as supplementary for agricultural disruption

summary(afb$crop_land) # whether the EA is within crop land or not (1= cropland)

crop_land <- felm(
  refuse_to_pay ~ edry + ewet + ldry + lwet + crop_land +
    edry:crop_land + ewet:crop_land + ldry:crop_land + lwet:crop_land +
    age + I(age^2) + sd_24m +
    n_drought_2ys + n_flood_2ys + n_earthquake_2ys + 
    n_extreme_temp_2ys + n_storm_2ys |
    edry6 + ewet6 + ldry6 + lwet6 +
    edry12 + ewet12 + ldry12 + lwet12 +
    edryi + eweti + ldryi + lweti +
    region + year_month + country_year +
    gender + edu + rural_sample + race + employ + occupation | 
    0 | cluster,
  data = afb
)


## Educational Heterogeneity (as a Proxy of Tax Morale)  -------------------------------------------------------------

# Create a binary variable for lower education
afb$lower_edu <- ifelse(afb$edu %in% c("No Formal Education", "Primary"), 1, 0)

lower_edu <- felm(
  refuse_to_pay ~ edry + ewet + ldry + lwet + lower_edu+
    edry:lower_edu + ewet:lower_edu + ldry:lower_edu + lwet:lower_edu +
    age + I(age^2) + sd_24m +
    n_drought_2ys + n_flood_2ys + n_earthquake_2ys + 
    n_extreme_temp_2ys + n_storm_2ys |
    edry6 + ewet6 + ldry6 + lwet6 +
    edry12 + ewet12 + ldry12 + lwet12 +
    edryi + eweti + ldryi + lweti +
    region + year_month + country_year +
    gender + rural_sample + race + employ + occupation | 
    0 | cluster,
  data = afb
)

##  Table: Heterogeneous Impact of Weather Extremes on Citizen Noncompliance  -----
# Create a list of the models to be included in the stargazer output
models <- list(agr,crop_land, informal_small, vulnerable, lower_edu)

stargazer(models, type = "text",
          keep = c("dry", "wet","agr","crop","infor","vul","edu"),
          digits = 3,
          dep.var.caption = "Dependent variable: Refuse to Pay",
          dep.var.labels.include = FALSE,
          add.lines = list(c('Region FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
                           c('Year-Month FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
                           c('Country-Year FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
                           c('Control', 'Y', 'Y', 'Y', 'Y', 'Y')),
          covariate.labels = c("Exceptional Drought", "Exceptional Wet","Limited Drought","Limited Wet",
                               "Agriculture","Cropland","Informal/Small","Weather-dependent","Less Educated",
                               "Exceptional Drought*Agriculture", "Exceptional Wet*Agriculture","Limited Drought*Agriculture","Limited Wet*Agriculture",
                               "Exceptional Drought*Cropland", "Exceptional Wet*Cropland","Limited Drought*Cropland","Limited Wet*Cropland", 
                               "Exceptional Drought*Informal/Small", "Exceptional Wet*Informal/Small","Limited Drought*Informal/Small","Limited Wet*Informal/Small",
                               "Exceptional Drought*Weather-Dependent", "Exceptional Wet*Weather-dependent","Limited Drought*Weather-dependent","Limited Wet*Weather-dependent",
                               "Exceptional Drought*Less Educated", "Exceptional Wet*Less Educated","Limited Drought*Less Educated","Limited Wet*Less Educated"),
          omit.stat  = c("rsq", "adj.rsq","ser")) 

# Output all interaction models in HTML format
stargazer(models, type = "html",
          out = "Output/Tables/heterogeneity_all.doc",
          keep = c("dry", "wet","agr","crop","infor","vul","edu"),
          digits = 3,
          dep.var.caption = "Dependent variable: Refuse to Pay",
          dep.var.labels.include = FALSE,
          add.lines = list(c('Region FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
                           c('Year-Month FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
                           c('Country-Year FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
                           c('Control', 'Y', 'Y', 'Y', 'Y', 'Y')),
          covariate.labels = c("Exceptional Drought", "Exceptional Wet","Limited Drought","Limited Wet",
                               "Agriculture","Cropland","Informal/Small","Weather-dependent","Less Educated",
                               "Exceptional Drought*Agriculture", "Exceptional Wet*Agriculture","Limited Drought*Agriculture","Limited Wet*Agriculture",
                               "Exceptional Drought*Cropland", "Exceptional Wet*Cropland","Limited Drought*Cropland","Limited Wet*Cropland", 
                               "Exceptional Drought*Informal/Small", "Exceptional Wet*Informal/Small","Limited Drought*Informal/Small","Limited Wet*Informal/Small",
                               "Exceptional Drought*Weather-dependent", "Exceptional Wet*Weather-dependent","Limited Drought*Weather-dependent","Limited Wet*Weather-dependent",
                               "Exceptional Drought*Less Educated", "Exceptional Wet*Less Educated","Limited Drought*Less Educated","Limited Wet*Less Educated"),
          omit.stat  = c("rsq", "adj.rsq","ser")) 




# Climate-induced Life Hardships  -------------------------------------------------------

## Impact of Weather Extremes on Life Hardships (Full Sample) --------
# Define the hardship variables
hardship <- c("water_shortage", "food_shortage", "medical_shortage", "fuel_shortage")

# Define the model estimation function
estimate_hardship_model <- function(mediator) {
  # Construct the formula
  formula <- as.formula(paste0(
    mediator, " ~ edry + ewet + ldry + lwet +
    age + I(age^2) + sd_24m +
    n_drought_2ys + n_flood_2ys + n_earthquake_2ys + 
    n_extreme_temp_2ys + n_storm_2ys |
    edry6 + ewet6 + ldry6 + lwet6 +
    edry12 + ewet12 + ldry12 + lwet12 +
    edryi + eweti + ldryi + lweti +
    region + year_month + country_year +
    gender + edu + rural_sample + race + employ + occupation |
    0 | cluster"
  ))
  
  # Fit the model
  felm(formula, data = afb)
}

plan(multisession, workers = 6)  # adjust the number of workers as needed

# Run all models in parallel
models <- future_lapply(hardship, estimate_hardship_model)
names(models) <- hardship

# Output the results table
stargazer(models, type = "text",
          keep = c("dry", "wet"),
          digits = 3,
          dep.var.caption = "Dependent variable: Hardship/Shortage",
          dep.var.labels.include = FALSE,
          add.lines = list(
            c('Region FE', 'Y', 'Y', 'Y', 'Y'),
            c('Year-Month FE', 'Y', 'Y', 'Y', 'Y'),
            c('Country-Year FE', 'Y', 'Y', 'Y', 'Y'),
            c('Control', 'Y', 'Y', 'Y', 'Y')
          ),
          covariate.labels = c("Exceptional Drought", "Exceptional Wet", "Limited Drought", "Limited Wet"),
          column.labels = c("Clean Water", "Food", "Medical Care", "Cooking Fuel"),
          omit.stat = c("rsq", "adj.rsq", "ser"))

# Generate an HTML version of the table
stargazer(models, type = "html",
          out = "Output/Tables/mechanism_hardship.doc",
          keep = c("dry", "wet"),
          digits = 3,
          dep.var.caption = "Dependent variable: Hardship/Shortage",
          dep.var.labels.include = FALSE,
          add.lines = list(
            c('Region FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
            c('Year-Month FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
            c('Country-Year FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
            c('Control', 'Y', 'Y', 'Y', 'Y', 'Y')
          ),
          covariate.labels = c("Exceptional Drought", "Exceptional Wet", "Limited Drought", "Limited Wet"),
          column.labels = c("Clean Water", "Food", "Medical Care", "Cooking Fuel"),
          omit.stat = c("rsq", "adj.rsq", "ser"))

# Extract coefficients and confidence intervals
results <- lapply(models, function(model) {
  tidy(model, conf.int = TRUE, conf.level = 0.95) %>%
    filter(term %in% c("edry", "ewet", "ldry", "lwet")) %>%
    mutate(hardship = model$call$afb$hardship[2])
})

# Combine results into one data frame
results_df <- bind_rows(lapply(names(results), function(hardship) {
  df <- results[[hardship]]
  df$hardship <- hardship
  return(df)
}))
# Rename hardship categories
results_df$hardship <- recode(results_df$hardship,
                              "water_shortage" = "Clean Water",
                              "food_shortage" = "Food",
                              "medical_shortage" = "Medical Care",
                              "fuel_shortage" = "Cooking Fuel"
)
# Reorder the term factor
results_df$term <- factor(results_df$term, levels = c(  "lwet","ldry","ewet", "edry"))
# Set factor levels in the desired order (excluding "Cash Income" if not needed)
results_df$hardship <- factor(results_df$hardship,
                              levels = c("Medical Care","Clean Water",  "Cooking Fuel", "Food")
)

# Plot
p_hardship <- ggplot(results_df, aes(x = estimate, y = hardship, color = term)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +  # Dodge points for visibility
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), 
                width = 0.3, 
                position = position_dodge(width = 0.5),
                size = 2, show.legend = FALSE) +  # Dodge error bars without legend
  theme_minimal(base_size = 20) +
  labs(
    x = "Coefficient Estimates",
    y = "Hardships",
    color = "Variable") +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0),  # Left-align title
        legend.position = "bottom",
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        axis.title = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 22, face = "bold"),
        axis.text.y = element_text(face = "bold", color = "black", size = 20)) +  # Consistent y-axis text
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 2) +  # Vertical line at x=0
  scale_color_manual(
    values = c(
      "edry" = "#b2182b",  # Exceptional Drought
      "ewet" = "#2166ac",  # Exceptional Wet
      "ldry" = "#f4a582",  # Limited Drought
      "lwet" = "#92c5de"   # Limited Wet
    ),
    labels = c(
      "edry" = "Exceptional Drought",
      "ewet" = "Exceptional Wet",
      "ldry" = "Limited Drought",
      "lwet" = "Limited Wet"
    )
  ) 
ggsave("Output/Figures/cost of living (full).jpg",p_hardship,dpi = 300, width = 16, height = 8)



## Impact of Weather Extremes on Life Hardships (Vulnerable Sample) --------
system.time({library(future.apply)
plan(multisession, workers = parallel::detectCores() - 1)  # leave 1 core free
hardship <- c("medical_shortage", "water_shortage", "food_shortage", "fuel_shortage")

# Define mediators
mediators <- c(hardship)

# Subset the data for the vulnerable group
vul_group <- subset(afb, vulnerable == 1)
fit_model <- function(mediator) {
  formula <- as.formula(paste0(
    mediator, " ~ edry + ewet + ldry + lwet + 
    age + I(age^2) + sd_24m +
    n_drought_2ys + n_flood_2ys + n_earthquake_2ys + 
    n_extreme_temp_2ys + n_storm_2ys | 
    edry6 + ewet6 + ldry6 + lwet6 + 
    edry12 + ewet12 + ldry12 + lwet12 + 
    edryi + eweti + ldryi + lweti + 
    region + year_month + country_year + 
    gender + edu + rural_sample + race + employ + occupation | 
    0 | cluster"
  ))
  
  felm(formula, data = vul_group)
}
models_vul <- future_lapply(mediators, fit_model)
names(models_vul) <- mediators
})

# Display the model results
stargazer(models_vul, type = "text",
          keep = c("dry","wet"),
          digits = 3,
          dep.var.caption = "Dependent variable: Hardship/Shortage (Weather-dependent)",
          dep.var.labels.include = FALSE,
          add.lines=list(c('Region FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
                         c('Year-Month FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
                         c('Country-Year FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
                         c('Control', 'Y', 'Y', 'Y', 'Y', 'Y')),
          column.labels = c("Clean Water", "Food", "Medical Care", "Cooking Fuel"),
          omit.stat  = c("rsq", "adj.rsq","ser"))

stargazer(models_vul, type = "html",
          out = "Output/Tables/mechanism_hardship_vulnerable.doc",
          keep = c("dry","wet"),
          digits = 3,
          dep.var.caption = "Dependent variable: Hardship/Shortage (Weather-dependent)",
          dep.var.labels.include = FALSE,
          add.lines=list(c('Region FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
                         c('Year-Month FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
                         c('Country-Year FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
                         c('Control', 'Y', 'Y', 'Y', 'Y', 'Y')),
          covariate.labels = c("Exceptional Drought", "Exceptional Wet","Limited Drought","Limited Wet"),
          omit.stat  = c("rsq", "adj.rsq","ser"))

# Extract coefficients and confidence intervals
results <- lapply(models_vul, function(model) {
  tidy(model, conf.int = TRUE, conf.level = 0.95) %>%
    filter(term %in% c("edry", "ewet", "ldry", "lwet")) %>%
    mutate(hardship = model$call$afb$hardship[2])
})

# Combine results into one data frame
results_df <- bind_rows(lapply(names(results), function(hardship) {
  df <- results[[hardship]]
  df$hardship <- hardship
  return(df)
}))
# Rename hardship categories
results_df$hardship <- recode(results_df$hardship,
                              "no_cash" = "Cash Income",
                              "water_shortage" = "Clean Water",
                              "food_shortage" = "Food",
                              "medical_shortage" = "Medical Care",
                              "fuel_shortage" = "Cooking Fuel"
)
# Reorder the term factor
results_df$term <- factor(results_df$term, levels = c(  "lwet","ldry","ewet", "edry"))
# Set factor levels in the desired order (excluding "Cash Income" if not needed)
results_df$hardship <- factor(results_df$hardship,
                              levels = c("Medical Care","Clean Water",  "Cooking Fuel", "Food")
)

##  Figure: Effects of Weather Extremes on Life Hardship among Weather-Vulnerable Populations  -----
p_vul_hardship <- ggplot(results_df, aes(x = estimate, y = hardship, color = term)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +  # Dodge points for visibility
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), 
                width = 0.3, 
                position = position_dodge(width = 0.5),
                size = 2, show.legend = FALSE) +  # Dodge error bars without legend
  theme_minimal(base_size = 20) +
  labs(
    x = "Coefficient Estimates",
    y = "Hardships",
    color = "Variable") +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0),  # Left-align title
        legend.position = "bottom",
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        axis.title = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 22, face = "bold"),
        axis.text.y = element_text(face = "bold", color = "black", size = 20)) +  # Consistent y-axis text
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 2) +  # Vertical line at x=0
  scale_color_manual(
    values = c(
      "edry" = "#b2182b",  # Exceptional Drought
      "ewet" = "#2166ac",  # Exceptional Wet
      "ldry" = "#f4a582",  # Limited Drought
      "lwet" = "#92c5de"   # Limited Wet
    ),
    labels = c(
      "edry" = "Exceptional Drought",
      "ewet" = "Exceptional Wet",
      "ldry" = "Limited Drought",
      "lwet" = "Limited Wet"
    )
  ) 
ggsave("Output/Figures/cost of living (weather-vulnerable).jpg",p_vul_hardship,dpi = 300, width = 16, height = 8)

p_vul_hardship=p_vul_hardship+
  ggtitle("B. Effects of Weather Extremes on Life Hardship among \nWeather-Vulnerable Populations")
 
## Figure: Impact of Weather Extremes on Weather-Vulnerable Populations ------------------------------------------------
p_vul <- gridExtra::grid.arrange(p_heterogeneity_occupation,p_vul_hardship, ncol = 1)
ggsave("Output/Figures/impact_vul.jpg",p_vul, width = 15, height = 17)



# Climate-induced Economic Pessimism  -------------------------------------------------------

# Define economic prospect variables
economy <- c("country_econ_back", "country_econ_ahead")

# Define mediators
mediators <- c(economy)

# Initialize an empty list to store models
models_econ <- list()

# Loop through each mediator and estimate the model
for (mediator in mediators) {
  # Create the formula
  formula <- as.formula(paste0(
    mediator, " ~ edry + ewet + ldry + lwet + 
    age + I(age^2) + sd_24m + 
    n_drought_2ys + n_flood_2ys + n_earthquake_2ys + 
    n_extreme_temp_2ys + n_storm_2ys | 
    edry6 + ewet6 + ldry6 + lwet6 + 
    edry12 + ewet12 + ldry12 + lwet12 + 
    edryi + eweti + ldryi + lweti + 
    region + year_month + country_year + 
    gender + edu + rural_sample + race + employ + occupation | 
    0 | cluster"
  ))
  
  # Estimate the model
  model <- felm(formula, data = afb)
  
  # Append the model to the list
  models_econ[[mediator]] <- model
}

## Table: Impact of Weather Extremes on Perceptions of Economic Trends ----
stargazer(models_econ, type = "text",
          keep = c("dry","wet"),
          digits = 3,
          dep.var.caption = "Dependent variable: Perception on Economic Trends (=1 if worse)",
          dep.var.labels.include = FALSE,
          add.lines=list(c('Region FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
                         c('Year-Month FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
                         c('Country-Year FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
                         c('Control', 'Y', 'Y', 'Y', 'Y', 'Y')),
          covariate.labels = c("Exceptional Drought", "Exceptional Wet","Limited Drought","Limited Wet"),
          column.labels = c( "Present VS Past","Future VS Present"),
          omit.stat  = c("rsq", "adj.rsq","ser"))

stargazer(models_econ, type = "html",
          out = "Output/Tables/mechanism_economy.doc",
          keep = c("dry","wet"),
          digits = 3,
          dep.var.caption = "Dependent variable: Perception on Economic Trends (=1 if worse)",
          dep.var.labels.include = FALSE,
          add.lines=list(c('Region FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
                         c('Year-Month FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
                         c('Country-Year FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
                         c('Control', 'Y', 'Y', 'Y', 'Y', 'Y')),
          covariate.labels = c("Exceptional Drought", "Exceptional Wet","Limited Drought","Limited Wet"),
          column.labels = c( "Present VS Past","Future VS Present"),
          omit.stat  = c("rsq", "adj.rsq","ser"))


# Extract coefficients and confidence intervals
results <- lapply(models_econ, function(model) {
  tidy(model, conf.int = TRUE, conf.level = 0.95) %>%
    filter(term %in% c("edry", "ewet", "ldry", "lwet")) %>%
    mutate(economic_perception = model$call$afb$economic_perception[1])
})

# Combine results into one data frame
results_df <- bind_rows(lapply(names(results), function(economic_perception) {
  df <- results[[economic_perception]]
  df$economic_perception <- economic_perception
  return(df)
}))
# Rename hardship categories
results_df$economic_perception <- recode(results_df$economic_perception,
                                         "country_econ_back" = "Present VS Past",
                                         "country_econ_ahead" = "Future VS Present"
)


# Reorder the term factor
results_df$term <- factor(results_df$term, levels = c(  "lwet","ldry","ewet", "edry"))

## Figure: Impact of Weather Extremes on Perceptions of Economic Trends ----
# Plotting coefficient estimates
p_econ <- ggplot(results_df, aes(x = estimate, y = economic_perception, color = term)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +  # Dodge points for better visibility
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), 
                width = 0.3, 
                position = position_dodge(width = 0.5),
                size = 2, show.legend = FALSE) +  # Error bars without legend
  theme_minimal() +
  labs(x = "Coefficient Estimates",
       y = NULL,  # Omit Y-axis label
       color = "Variable") +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0),  # Align title to the left
        legend.position = "bottom",
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        axis.title = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 22, face = "bold")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 2) +  # Vertical line at x=0
  scale_color_manual(
    values = c(
      "edry" = "#b2182b",  # Exceptional Drought
      "ewet" = "#2166ac",  # Exceptional Wet
      "ldry" = "#f4a582",  # Limited Drought
      "lwet" = "#92c5de"   # Limited Wet
    ),
    labels = c(
      "edry" = "Exceptional Drought",
      "ewet" = "Exceptional Wet",
      "ldry" = "Limited Drought",
      "lwet" = "Limited Wet"
    )
  ) +
  ggtitle("Perception on Economic Trends (=1 if worse)")

ggsave("Output/Figures/economic_perception.jpg",p_econ, width = 15, height = 8)

p_econ=p_econ+
  ggtitle("C. Impact of Weather Extremes on Perceptions of Economic Trends")

## Figure: Economic Channel: Heterogeneous Effects, Life Hardships, and Pessimism ------------------------------------------------
p_econ_channel <- gridExtra::grid.arrange(p_heterogeneity_occupation,p_vul_hardship, p_econ, ncol = 1)
ggsave("Output/Figures/p_econ_channel.jpg",p_econ_channel, width = 15, height = 20)


# Political Channel -------------------------------------------------------

## Job Performance Satisfaction -------------------------------------------------------
# Define government performance variables
performance <- c("president_performance", "parliament_performance", "local_gov_performance")

# Define mediators
mediators <- c(performance)

# Initialize an empty list to store models
models_performance <- list()

# Loop through each mediator and estimate the model
for (mediator in mediators) {
  # Create the formula
  formula <- as.formula(paste0(
    mediator, " ~ edry + ewet + ldry + lwet + 
    age + I(age^2) + sd_24m + 
    n_drought_2ys + n_flood_2ys + n_earthquake_2ys + 
    n_extreme_temp_2ys + n_storm_2ys | 
    edry6 + ewet6 + ldry6 + lwet6 + 
    edry12 + ewet12 + ldry12 + lwet12 + 
    edryi + eweti + ldryi + lweti + 
    region + year_month + country_year + 
    gender + edu + rural_sample + race + employ + occupation | 
    0 | cluster"
  ))
  
  # Estimate the model
  model <- felm(formula, data = afb)
  
  # Append the model to the list
  models_performance[[mediator]] <- model
}


## Table: Impact of Weather Extremes on Citizens' Disapproval of Public Authorities' Job Performance  ------
stargazer(models_performance, type = "text",
          keep = c("dry", "wet"),
          digits = 3,
          dep.var.caption = "Dependent variable: Disapprove Job Performance (Past 12 Months)",
          dep.var.labels.include = FALSE,
          add.lines=list(c('Region FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
                         c('Year-Month FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
                         c('Country-Year FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
                         c('Control', 'Y', 'Y', 'Y', 'Y', 'Y')),
          covariate.labels = c("Exceptional Drought", "Exceptional Wet","Limited Drought","Limited Wet"),
          column.labels = c("President/Prime Minister", "Parliament Member/National Assembly Rep.", "Local Government Councilor"),
          omit.stat  = c("rsq", "adj.rsq","ser"))

stargazer(models_performance, type = "html",
          out = "Output/Tables/mechanism_disapprove.doc",
          keep = c("dry", "wet"),
          digits = 3,
          dep.var.caption = "Dependent variable: Disapprove Job Performance (Past 12 Months)",
          dep.var.labels.include = FALSE,
          add.lines=list(c('Region FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
                         c('Year-Month FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
                         c('Country-Year FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
                         c('Control', 'Y', 'Y', 'Y', 'Y', 'Y')),
          covariate.labels = c("Exceptional Drought", "Exceptional Wet","Limited Drought","Limited Wet"),
          column.labels = c("President/Prime Minister", "Parliament Member/National Assembly Rep.", "Local Government Councilor"),
          omit.stat  = c("rsq", "adj.rsq","ser"))

# Extract coefficients and confidence intervals
results <- lapply(models_performance, function(model) {
  tidy(model, conf.int = TRUE, conf.level = 0.95) %>%
    filter(term %in% c("edry", "ewet", "ldry", "lwet")) %>%
    mutate(performance = model$call$afb$performance[1])
})

# Combine results into one data frame
results_df <- bind_rows(lapply(names(results), function(performance) {
  df <- results[[performance]]
  df$performance <- performance
  return(df)
}))
# Rename hardship categories
results_df$performance <- recode(results_df$performance,
                                 "president_performance" = "President",
                                 "parliament_performance" = "Parliament",
                                 "local_gov_performance" = "Local Government"
)


# Reorder the term factor
results_df$term <- factor(results_df$term, levels = c(  "lwet","ldry","ewet", "edry"))

## Figure: Impact of Weather Extremes on Citizens' Disapproval of Public Authorities' Job Performance  ------
# Updated ggplot
p_performance <- ggplot(results_df, aes(x = estimate, y = performance, color = term)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +  # Dodge points for better visibility
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), 
                width = 0.3, 
                position = position_dodge(width = 0.5),
                size = 2, show.legend = FALSE) +  # Error bars without legend
  theme_minimal() +
  labs(
       x = "Coefficient Estimates",
       y = NULL,
       color = "Variable") +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0),  # Align title to the left
        legend.position = "bottom",
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        axis.title = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 22, face = "bold")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 2) +  # Vertical line at x=0
  scale_color_manual(
    values = c(
      "edry" = "#b2182b",  # Exceptional Drought
      "ewet" = "#2166ac",  # Exceptional Wet
      "ldry" = "#f4a582",  # Limited Drought
      "lwet" = "#92c5de"   # Limited Wet
    ),
    labels = c(
      "edry" = "Exceptional Drought",
      "ewet" = "Exceptional Wet",
      "ldry" = "Limited Drought",
      "lwet" = "Limited Wet"
    )
  ) +
  ggtitle("A. Dissatisfaction with Public Officials' Performance (=1 if disapprove)")


## Political Trust -------------------------------------------------------

# Define trust variables
trust <- c("no_trust_president", "no_trust_parliament", "no_trust_local")

# Define mediators
mediators <- c(trust)

# Initialize an empty list to store models
models_trust <- list()

# Loop through each mediator and estimate the model
for (mediator in mediators) {
  # Create the formula
  formula <- as.formula(paste0(
    mediator, " ~ edry + ewet + ldry + lwet + 
    age + I(age^2) + sd_24m + 
    n_drought_2ys + n_flood_2ys + n_earthquake_2ys + 
    n_extreme_temp_2ys + n_storm_2ys | 
    edry6 + ewet6 + ldry6 + lwet6 + 
    edry12 + ewet12 + ldry12 + lwet12 + 
    edryi + eweti + ldryi + lweti + 
    region + year_month + country_year + 
    gender + edu + rural_sample + race + employ + occupation | 
    0 | cluster"
  ))
  
  # Estimate the model
  model <- felm(formula, data = afb)
  
  # Append the model to the list
  models_trust[[mediator]] <- model
}

## Table:Impact of Weather Extremes on Citizens' Disapproval of Public Authorities' Political Trust ----
stargazer(models_trust, type = "text",
          keep = c("dry", "wet"),
          digits = 3,
          dep.var.caption = "Dependent variable: Trust in State Institutions (=1 if not trust at all)",
          dep.var.labels.include = FALSE,
          add.lines=list(c('Region FE', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y'),
                         c('Year-Month FE', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y'),
                         c('Country-Year FE', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y'),
                         c('Control', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y')),
          covariate.labels = c("Exceptional Drought", "Exceptional Wet","Limited Drought","Limited Wet"),
          column.labels = c("President", "Parliament", "Local Government", "Ruling Party", "Police", "Electoral", "Opposite Party", "Army", "Courts"),
          omit.stat  = c("rsq", "adj.rsq","ser"))

stargazer(models_trust, type = "html",
          out = "Output/Tables/mechanism_trust.doc",
          keep = c("dry", "wet"),
          digits = 3,
          dep.var.caption = "Dependent variable: Trust in State Institutions (=1 if not trust at all)",
          dep.var.labels.include = FALSE,
          add.lines=list(c('Region FE', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y'),
                         c('Year-Month FE', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y'),
                         c('Country-Year FE', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y'),
                         c('Control', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'Y')),
          covariate.labels = c("Exceptional Drought", "Exceptional Wet","Limited Drought","Limited Wet"),
          column.labels = c("President", "Parliament", "Local Government", "Ruling Party", "Police", "Electoral", "Opposite Party", "Army", "Courts"),
          omit.stat  = c("rsq", "adj.rsq","ser"))


# Extract coefficients and confidence intervals
results <- lapply(models_trust, function(model) {
  tidy(model, conf.int = TRUE, conf.level = 0.95) %>%
    filter(term %in% c("edry", "ewet", "ldry", "lwet")) %>%
    mutate(trust = model$call$afb$trust[1])
})

# Combine results into one data frame
results_df <- bind_rows(lapply(names(results), function(trust) {
  df <- results[[trust]]
  df$trust <- trust
  return(df)
}))
# Rename hardship categories
results_df$trust <- recode(results_df$trust,
                           "no_trust_president" = "President",
                           "no_trust_parliament" = "Parliament",
                           "no_trust_local" = "Local Government"
)


# Reorder the term factor
results_df$term <- factor(results_df$term, levels = c(  "lwet","ldry","ewet", "edry"))

## Figure:Impact of Weather Extremes on Citizens' Disapproval of Public Authorities' Political Trust ----
# Updated ggplot
p_trust <- ggplot(results_df, aes(x = estimate, y = trust, color = term)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +  # Dodge points for better visibility
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), 
                width = 0.3, 
                position = position_dodge(width = 0.5),
                size = 2, show.legend = FALSE) +  # Error bars without legend
  theme_minimal() +
  labs(
       x = "Coefficient Estimates",
       y = NULL,
       color = "Variable") +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0),  # Align title to the left
        legend.position = "bottom",
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        axis.title = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 22, face = "bold")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 2) +  # Vertical line at x=0
  scale_color_manual(
    values = c(
      "edry" = "#b2182b",  # Exceptional Drought
      "ewet" = "#2166ac",  # Exceptional Wet
      "ldry" = "#f4a582",  # Limited Drought
      "lwet" = "#92c5de"   # Limited Wet
    ),
    labels = c(
      "edry" = "Exceptional Drought",
      "ewet" = "Exceptional Wet",
      "ldry" = "Limited Drought",
      "lwet" = "Limited Wet"
    )
  ) +
  ggtitle("B. Distrust in State Institutions (=1 if not trust at all)")


## Figure: Impact of Weather Extremes on Citizens' Disapproval and Distrust  ------
p_attribution <- gridExtra::grid.arrange(p_performance,p_trust, ncol = 1)
ggsave("Output/Figures/attribution.jpg",p_attribution, , width = 16.5, height = 16)



# Perceived Reasons for Tax Noncompliance ------------------------------

# Define dependent variables
dependent_vars <- c("unfair_tax", "tax_too_high", "cannot_afford",
  "poor_service", "gov_not_listen", "gov_waste", "gov_steal_tax", "will_not_caught",
  "greed_selfish", "ignorance"
)

# Assign labels
dep_labels <- c(
  unfair_tax = "Unfair Tax System",
  tax_too_high = "Tax Too High",
  cannot_afford = "Cannot Afford to Pay",
  poor_service = "Poor Public Services",
  gov_not_listen = "Government Doesn’t Listen",
  gov_waste = "Government Wastes Tax",
  gov_steal_tax = "Government Officials Steal Tax Money",
  will_not_caught = "Will Not Be Caught",
  greed_selfish = "Greed / Selfishness",
  ignorance = "Ignorance: Don’t Know How to Pay\nor Don’t Understand the Need to Pay"
)

# Summary
for (var in dependent_vars) {
  print(var)
  print(summary(afb[[var]]))}


plan(multisession, workers = 7) 
# Define function to run one model
run_model <- function(dep_var) {
  formula <- as.formula(paste0(
    dep_var, " ~ edry + ewet + ldry + lwet + 
    age + I(age^2) + sd_24m + 
    n_drought_2ys + n_flood_2ys + n_earthquake_2ys + 
    n_extreme_temp_2ys + n_storm_2ys | 
    edry6 + ewet6 + ldry6 + lwet6 + 
    edry12 + ewet12 + ldry12 + lwet12 + 
    edryi + eweti + ldryi + lweti + 
    region + year_month + country_year + 
    gender + edu + rural_sample + race + employ + occupation | 
    0 | cluster"
  ))
  felm(formula, data = afb)
}

# Run models in parallel
models <- future_lapply(dependent_vars, run_model)

# Name list elements for later reference
names(models) <- dependent_vars

# Generate summary table
stargazer(models, type = "text", keep = c("dry", "wet"), digits = 3)

# Extract tidy results for all models and combine
results_df <- bind_rows(
  lapply(names(models), function(dep_var) {
    broom::tidy(models[[dep_var]], conf.int = TRUE) %>%
      filter(term %in% c("edry", "ewet", "ldry", "lwet")) %>%
      mutate(reason = dep_var)
  }),
  .id = "model"
)
results_df$reason <- dep_labels[results_df$reason]

ggplot(results_df, aes(x = estimate, y = factor(reason, levels = rev(unique(reason))), color = term)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                width = 0.3,
                position = position_dodge(width = 0.5),
                size = 2, show.legend = FALSE) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 2) +
  theme_minimal() +
  labs(
    x = "Coefficient Estimates",
    y = NULL,
    color = "Weather Extreme"
  ) +
  scale_color_manual(
    values = c(
      "edry" = "#b2182b",  # Exceptional Drought
      "ewet" = "#2166ac",  # Exceptional Wet
      "ldry" = "#f4a582",  # Limited Drought
      "lwet" = "#92c5de"   # Limited Wet
    ),
    labels = c(
      "edry" = "Exceptional Drought",
      "ewet" = "Exceptional Wet",
      "ldry" = "Limited Drought",
      "lwet" = "Limited Wet"
    )
  ) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 18, face = "bold")
  )

## Figure: Impact of Weather Extremes on Reasons for Tax Noncompliance  ------
ggsave("Output/Figures/reasons_people_refuse.jpg", , width = 18, height = 12,dpi=300)


# Impact on Alternative Civic Behaviors -------------------------------------------------------

# Define mediators for alternative behaviors
mediators <- c( "raise_issue", "protest", "political_vio")

# Initialize an empty list to store models
models <- list()

# Loop through each mediator and estimate the model
for (mediator in mediators) {
  # Create the formula
  formula <- as.formula(paste0(
    mediator, " ~ edry + ewet + ldry + lwet + 
    age + I(age^2) + sd_24m + 
    n_drought_2ys + n_flood_2ys + n_earthquake_2ys + 
    n_extreme_temp_2ys + n_storm_2ys | 
    edry6 + ewet6 + ldry6 + lwet6 + 
    edry12 + ewet12 + ldry12 + lwet12 + 
    edryi + eweti + ldryi + lweti + 
    region + year_month + country_year + 
    gender + edu + rural_sample + race + employ + occupation | 
    0 | cluster"
  ))
  
  # Estimate the model
  model <- felm(formula, data = afb)
  
  # Append the model to the list
  models[[mediator]] <- model
}

## Table: Impact of Weather Extremes on Public Participation ----
stargazer(models, type = "text",
          keep = c("dry", "wet"),
          digits = 3,
          dep.var.caption = "Dependent variable: Citizen Behaviors",
          dep.var.labels.include = FALSE,
          add.lines=list(c('Region FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
                         c('Year-Month FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
                         c('Country-Year FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
                         c('Control', 'Y', 'Y', 'Y', 'Y', 'Y')),
          covariate.labels = c("Exceptional Drought", "Exceptional Wet","Limited Drought","Limited Wet"),
          column.labels = c("Raise an Issue", "Demonstration or Protest","Political Violence"),
          omit.stat  = c("rsq", "adj.rsq","ser"))


stargazer(models, type = "html",
          out = "Output/Tables/citizen_behavior.doc",
          keep = c("dry", "wet"),
          digits = 3,
          dep.var.caption = "Dependent variable: Citizen Behaviors",
          dep.var.labels.include = FALSE,
          add.lines=list(c('Region FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
                         c('Year-Month FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
                         c('Country-Year FE', 'Y', 'Y', 'Y', 'Y', 'Y'),
                         c('Control', 'Y', 'Y', 'Y', 'Y', 'Y')),
          covariate.labels = c("Exceptional Drought", "Exceptional Wet","Limited Drought","Limited Wet"),
          column.labels = c("Raise an Issue", "Demonstration or Protest","Political Violence"),
          omit.stat  = c("rsq", "adj.rsq","ser"))



# Mitigating Effect of Climate Governance -------------------------------------------------------

# Create an empty list to store models
adaptation <- c("piped_water", "dam", "water_source", "renewable_consum", "sewage","school")
mediators <- c(adaptation)

plan(multisession, workers = parallel::detectCores() - 1)  # leave 1 core free

system.time({
# Define a function to estimate the model for a given mediator
estimate_model <- function(mediator) {
  # Adjust formula for 'renewable_consum' mediator
  if (mediator == "renewable_consum") {
    formula_str <- paste0(
      "refuse_to_pay ~ ", mediator, " + edry + ewet + ldry + lwet + ",
      "edry:", mediator, " + ewet:", mediator, " + ldry:", mediator, " + lwet:", mediator, " + ",
      "age + I(age^2) + sd_24m + ",
      "n_drought_2ys + n_flood_2ys + n_earthquake_2ys + n_extreme_temp_2ys + n_storm_2ys | ",
      "edry6 + ewet6 + ldry6 + lwet6 + ",
      "edry12 + ewet12 + ldry12 + lwet12 + ",
      "edryi + eweti + ldryi + lweti + ",
      "region + year_month + ",
      "gender + edu + rural_sample + race + employ + occupation | ",
      "0 | cluster"
    )
  } else {
    formula_str <- paste0(
      "refuse_to_pay ~ ", mediator, " + edry + ewet + ldry + lwet + ",
      "edry:", mediator, " + ewet:", mediator, " + ldry:", mediator, " + lwet:", mediator, " + ",
      "age + I(age^2) + sd_24m + ",
      "n_drought_2ys + n_flood_2ys + n_earthquake_2ys + n_extreme_temp_2ys + n_storm_2ys | ",
      "edry6 + ewet6 + ldry6 + lwet6 + ",
      "edry12 + ewet12 + ldry12 + lwet12 + ",
      "edryi + eweti + ldryi + lweti + ",
      "region + year_month + country_year + ",
      "gender + edu + rural_sample + race + employ + occupation | ",
      "0 | cluster"
    )
  }
  
  # Convert the formula string to a formula object
  formula <- as.formula(formula_str)
  
  # Fit the model
  model <- felm(formula, data = afb)
  
  return(model)
}

# Perform parallel model estimation
models <- future_lapply(mediators, estimate_model, future.seed = TRUE)

# Assign names to the models
names(models) <- mediators
}) # 3 mins

## Figure: Mitigating Effect of Climate Governance  ---------------------------------------------------------

# Function to create individual plots
create_plot <- function(model_name, term_labels, title_text) {
  model_summary <- broom::tidy(models[[model_name]]) %>% slice(-c(1:13))
  model_summary$term <- factor(model_summary$term, levels = model_summary$term)
  model_summary$term <- term_labels
  
  ggplot(model_summary, aes(x = term, y = estimate, ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error)) +
    geom_point(color = "#1984c5", size = 3) +
    geom_errorbar(width = 0.2, size = 1) +
    labs(x = "", y = "") +
    theme_minimal(base_size = 30) +
    coord_flip() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#c23728", size = 2) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "black"), 
          axis.text.y = element_text(size = 12, color = "black", face = "bold"),  
          plot.title = element_text(size = 16, face = "bold")) +
    ggtitle(title_text)
}

# Define terms and titles for each plot
plot_info <- list(
  list("piped_water", c("Exceptional Drought*Piped Water", "Exceptional Wet*Piped Water", "Limited Drought*Piped Water", "Limited Wet*Piped Water"), "A. Piped Water"),
  list("water_source", c("Exceptional Drought*Water Source", "Exceptional Wet*Water Source", "Limited Drought*Water Source", "Limited Wet*Water Source"), "B. In-House Water Source"),
  list("dam", c("Exceptional Drought*Dam", "Exceptional Wet*Dam", "Limited Drought*Dam", "Limited Wet*Dam"), "C. Dam"),
  list("renewable_consum", c("Exceptional Drought*Renewable", "Exceptional Wet*Renewable", "Limited Drought*Renewable", "Limited Wet*Renewable"), "D. Renewable"),
  list("sewage", c("Exceptional Drought*Sewage System", "Exceptional Wet*Sewage", "Limited Drought*Sewage", "Limited Wet*Sewage"), "E. Sewage System (Placebo)"),
  list("school", c("Exceptional Drought*School", "Exceptional Wet*School", "Limited Drought*School", "Limited Wet*School"), "F. School (Placebo)")
)

# Create individual plots
plots <- lapply(plot_info, function(info) {
  create_plot(info[[1]], info[[2]], info[[3]])
})

# Combine and save the plots
p_combine <- gridExtra::grid.arrange(grobs = plots, ncol = 2)
ggsave("Output/Figures/mitigating_effect.jpg", p_combine, width = 13, height = 12, dpi = 300)



# Table: Summary Statistics ------------------------------------------------------

# Select relevant variables
afb_stat <- afb %>% 
  dplyr::select("spei_24_d", "edry", "ewet", "ldry", "lwet", "normal",
                "refuse_to_pay", "age", "gender", "rural_sample", "employ",
                "edu", "race", "occupation", "electricity", "piped_water", "sewage",
                "renewable_consum", "dam", "water_source")

# Convert categorical variables to binary
afb_stat <- afb_stat %>%
  mutate(
    Rural = ifelse(rural_sample == "Rural", 1, 0),
    Urban_semiurban = ifelse(rural_sample %in% c("Urban", "Semi-Urban"), 1, 0),
    
    # Education
    No_Formal_Education = ifelse(edu == "No Formal Education", 1, 0),
    Primary_Education = ifelse(edu == "Primary", 1, 0),
    Secondary_Education = ifelse(edu == "Secondary", 1, 0),
    Post_secondary_Education = ifelse(edu == "Post-secondary", 1, 0),
    
    # Race
    Arab_Lebanese_North_African = ifelse(race == "Arab/Lebanese/North", 1, 0),
    Black = ifelse(race == "Black", 1, 0),
    Others = ifelse(race %in% c("Colored/Mixed Race", "Asian", "White/European", "Other"), 1, 0),
    
    # Occupation
    Agriculture_Farming_Fishing = ifelse(occupation == "Agriculture / Farming / Fishing", 1, 0),
    Formal_Employment_Professionals = ifelse(occupation == "Formal Employment & Professionals", 1, 0),
    Informal_Small_Scale_Business = ifelse(occupation == "Informal & Small-Scale Business", 1, 0),
    Large_Scale_Business = ifelse(occupation == "Large-Scale Business", 1, 0),
    Manual_Labor = ifelse(occupation == "Manual Labor", 1, 0),
    No_Job = ifelse(occupation == "No Job", 1, 0),
    Other_Job = ifelse(occupation == "Uncategorized", 1, 0)
  )

# Reorder columns for clarity
afb_stat <- afb_stat %>% 
  dplyr::select("spei_24_d", "edry", "ewet", "ldry", "lwet", "normal",
                "refuse_to_pay", "age", "gender", "Rural", "Urban_semiurban", "employ",
                "No_Formal_Education", "Primary_Education", "Secondary_Education", "Post_secondary_Education",
                "Arab_Lebanese_North_African", "Black", "Others",
                "Agriculture_Farming_Fishing", "Formal_Employment_Professionals", 
                "Informal_Small_Scale_Business", "Large_Scale_Business", "Manual_Labor", "No_Job", "Other_Job",
                "piped_water", "dam", "water_source", "renewable_consum", "sewage")

# Create and save summary statistics tables
stargazer(afb_stat,
          type = "text", summary.stat = c("n", "mean", "sd", "min", "max"), digits = 2)

stargazer(afb_stat,
          type = "text", summary.stat = c("n", "mean", "sd", "min", "max"), digits = 2,
          covariate.labels = c("SPEI", "Exceptional Drought (SPEI ≤ -1.5)", "Exceptional Wet (SPEI ≥ 1.5)",
                               "Limited Drought (-1.5 < SPEI ≤ -0.5)", "Limited Wet (0.5 ≤ SPEI < 1.5)", "Normal (-0.5 < SPEI < 0.5)",
                               "Refuse to Pay", "Age", "Gender", "Rural", "Urban and Semi-Urban",
                               "Employed", "No Formal Education", "Primary", "Secondary", "Post-secondary",
                               "Arab/Lebanese/North", "Black", "Others",
                               "Agriculture / Farming / Fishing", "Formal Employment / Professionals", 
                               "Informal / Small-Scale Business", "Large-Scale Business", "Manual Labor", "No Job", "Others",
                               "Piped Water", "Dam", "Water Source (In-house)", "Renewable Consumption", "Sewage System"))

stargazer(afb_stat, type = "html", out = "Output/Tables/summary_stats.doc",
          summary.stat = c("n", "mean", "sd", "min", "max"), digits = 2,
          covariate.labels = c("SPEI", "Exceptional Drought (SPEI ≤ -1.5)", "Exceptional Wet (SPEI ≥ 1.5)",
                               "Limited Drought (-1.5 < SPEI ≤ -0.5)", "Limited Wet (0.5 ≤ SPEI < 1.5)", "Normal (-0.5 < SPEI < 0.5)",
                               "Refuse to Pay", "Age", "Gender", "Rural", "Urban and Semi-Urban",
                               "Employed", "No Formal Education", "Primary", "Secondary", "Post-secondary",
                               "Arab/Lebanese/North", "Black", "Others",
                               "Agriculture / Farming / Fishing", "Formal Employment / Professionals", 
                               "Informal / Small-Scale Business", "Large-Scale Business", "Manual Labor", "No Job", "Others",
                               "Piped Water", "Dam", "Water Source (In-house)", "Renewable Consumption", "Sewage System"))




# Visualization -----------------------------------------------------------

# Acknowledgment: The geolocation data used to produce "Figure: Distribution of Sample and Weather Conditions in Africa" 
# require approval from Afrobarometer and are therefore not included in this replication package. 
# Researchers interested in accessing these data should contact the Afrobarometer team directly.


## Figure: Distribution of SPEI in Sampled Populations -----------------------------------------------
# Create the categories
categories <- cut(afb$spei_24_d, breaks = c(-Inf, -2, -1.5, -1, -0.5, 0.5, 1, 1.5, 2, Inf),
                  labels = c("Extreme Drought: SPEI ≤ -2", "Severe Drought: (-2 < SPEI ≤ -1.5",
                             "Moderate Drought: (-1.5 < SPEI ≤ -1", "Mild Drought: (-1 < SPEI < -0.5",
                             "Normal: -0.5 ≤ SPEI ≤ 0.5",  "Mild Wet: 0.5 < SPEI < 1",
                             "Moderate Wet: 1 ≤ SPEI < 1.5", "Severe Wet: 1.5 ≤ SPEI < 2", "Extreme Wet: SPEI ≥ 2"))
category_labels <- c("Extreme Drought", "Severe Drought",
                     "Moderate Drought", "Mild Drought",
                     "Normal",  "Mild Wet",
                     "Moderate Wet", "Severe Wet", "Extreme Wet")
# Calculate the counts for each category
category_counts <- table(categories)

# Calculate the percentage for each category
category_percentages <- prop.table(category_counts) * 100
# Create a data frame from the category counts and percentages
category_data <- data.frame(
  Category = category_labels,
  Count = as.vector(category_counts),
  Percentage = as.vector(category_percentages)
)
category_data$Category <- factor(category_data$Category, levels = c("Extreme Drought", "Severe Drought",
                                                                    "Moderate Drought", "Mild Drought",
                                                                    "Normal",  "Mild Wet",
                                                                    "Moderate Wet", "Severe Wet", "Extreme Wet"))

# Define a color palette
colors <- brewer.pal(9, "RdYlBu")

# Create the barplot using ggplot2
p_spei_hist <- ggplot(category_data, aes(x = Category, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", color = "grey") +
  scale_fill_manual(
    values = colors,
    name = "SPEI",  # Set the legend title
    labels = c("Extreme Drought: SPEI ≤ -2", "Severe Drought: (-2 < SPEI ≤ -1.5)",
               "Moderate Drought: (-1.5 < SPEI ≤ -1)", "Mild Drought: (-1 < SPEI < -0.5)",
               "Normal: -0.5 ≤ SPEI ≤ 0.5", "Mild Wet: 0.5 < SPEI < 1",
               "Moderate Wet: 1 ≤ SPEI < 1.5", "Severe Wet: 1.5 ≤ SPEI < 2", "Extreme Wet: SPEI ≥ 2")
  ) +
  labs(x = "Weather", y = "Percentage") +
  ylim(0, 40) +
  theme_minimal()  +
  ggtitle("B") +
  theme(legend.position = c(0.8, 0.9),
        legend.justification = c("center", "top") ,
        plot.title = element_text(size = 30, face = "bold", hjust = 0),
        axis.title = element_text(size = 28, face = "bold"),
        axis.text = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 24),
        legend.title = element_text(size = 24, face = "bold")
  ) +
  geom_text(aes(label = round(Percentage, 1)), vjust = -0.5, size = 7) +  # Add percentage labels
  annotate("text", x = 1.5, y = 12, label = "Exceptional Drought", size = 7, fontface = "bold") +
  annotate("text", x = 3.4, y = 24, label = "Limited Drought", size = 7, fontface = "bold") +
  annotate("text", x = 8.5, y = 5, label = "Exceptional Wet", size = 7, fontface = "bold") +
  annotate("text", x = 6.5, y = 11, label = "Limited Wet", size = 7, fontface = "bold")

ggsave("Output/Figures/spei_hist.png", width=28, height=14)


## Figure: Global Distribution of Tax Capacity and the Correlation with State Capacity --------------------------
tax_gdp <- readRDS("Data/working_data/tax_gdp.rds")
# Plot the map 

# First Plot (p1)
p1 = ggplot(data = tax_gdp) +
  geom_sf(aes(fill = mean_tax_of_gdp)) +
  scale_fill_gradientn(colors = c("#67001f", "#b2182b", "#f4a582", "#d1e5f0", 
                                  "#4393c3", "#2166ac", "#053061"), na.value = "grey50") +  # Custom color palette
  labs(fill = "% of GDP") +
  theme_minimal() +
  ggtitle("A. Tax Revenue (% of GDP)") +  
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0),  # Title text style
    legend.title = element_text(size = 18, face = "bold"),  
    legend.text = element_text(size = 18),
    legend.key = element_rect(fill = "grey50")
  )


# Second Plot (p2)
p2 = ggplot(data = tax_gdp) +
  geom_sf(aes(fill = mean_income_tax_of_gdp)) +
  scale_fill_gradientn(colors = c("#67001f", "#b2182b", "#f4a582", "#d1e5f0",
                                  "#92c5de","#6BACD0","#4393c3", "#2166ac", "#053061"), na.value = "grey50") +  # Custom color palette
  labs(fill = "% of GDP") +
  theme_minimal() +
  ggtitle("B. Taxes on Incomes of Individuals and Corporations (% of GDP)") +  
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0),  # Title text style
    legend.title = element_text(size = 18, face = "bold"),  
    legend.text = element_text(size = 18)
  )

# Import country panel data
panel <- readRDS("Data/working_data/country_panel.rds")

# Relationship between tax capacity and wealth
correlation <- cor(panel$tax_of_gdp, log(panel$gdppc), use = "complete.obs", method = "pearson")
p3=ggplot(panel, aes(x = income_tax_of_gdp, y = log(gdppc))) +
  geom_point() +
  geom_smooth(method = "lm", color = "#b2182b", fill = "#92c5de", se = TRUE) +
  theme_minimal() +
  annotate("text", x = Inf, y = Inf, label = paste("Correlation: ", round(correlation, 2)), 
           hjust = 1.1, vjust = 2, size = 6, color = "#053061", fontface = "bold") + 
  labs(x = "Taxes on Incomes (% of GDP)",
       y = "Log GDP per Capita")+
  ggtitle("C") +  
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0),  # Title text style
    legend.title = element_text(size = 26, face = "bold"),  
    legend.text = element_text(size = 18),
    axis.title = element_text(size = 22, face = "bold")
  )

# Relationship between tax capacity and state capacity
correlation <- cor(panel$income_tax_of_gdp, panel$state_capacity, use = "complete.obs", method = "pearson")
p4 = ggplot(panel, aes(x = income_tax_of_gdp, y = state_capacity)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#b2182b", fill = "#92c5de", se = TRUE) +
  theme_minimal() +
  annotate("text", x = Inf, y = Inf, label = paste("Correlation: ", round(correlation, 2)), 
           hjust = 1.1, vjust = 2, size = 6, color = "#053061", fontface = "bold") +  
  labs(x = "Taxes on Incomes (% of GDP)",
       y = "State Capacity") +
  ggtitle("D") +  
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0),  # Title text style
    legend.title = element_text(size = 26, face = "bold"),  
    legend.text = element_text(size = 18),
    axis.title = element_text(size = 22, face = "bold")
  )

# Define layout matrix
layout_matrix <- rbind(c(1, 1), c(2, 2),c(3,4))
tax_state_capacity <- gridExtra::grid.arrange(p1,p2,p3,p4, nrow = 3, layout_matrix = layout_matrix)
ggsave("Output/Figures/tax_state_capacity.jpg",tax_state_capacity, width = 18, height = 20)


## Figure: Correlation between Citizen Noncompliance with State/Tax Capacity  --------------------------

# Function to create the plot
create_plot <- function(x, y, x_label, y_label, data, title_label) {
  correlation <- cor(data[[x]], data[[y]], use = "complete.obs", method = "pearson")
  
  ggplot(data, aes_string(x = x, y = y)) +
    geom_point(aes(size=15)) +
    geom_smooth(method = "lm", color = "#b2182b", fill = "#92c5de", se = TRUE, size=5) +
    theme_minimal() +
    annotate("text", x = Inf, y = Inf, label = paste("Correlation: ", round(correlation, 2)), 
             hjust = 1.1, vjust = 2, size = 14, color = "#053061", fontface = "bold") +
    labs(x = x_label, y = y_label) +
    ggtitle(title_label) +
    theme(
      plot.title = element_text(size = 56, face = "bold", hjust = 0),
      axis.title = element_text(size = 44, face = "bold"),
      axis.text = element_text(size = 34, face = "bold")
    )
}

# Define the variable list with the labels
variables <- list(
  c("noncompliance", "state_capacity", "Noncompliance", "State Capacity", "A"),
  c("noncompliance", "income_tax_of_gdp", "Noncompliance", "Tax Capacity", "B"),
  c("noncompliance", "next_year_income_tax", "Noncompliance", "Tax Capacity (Year+1)", "C"),
  c("noncompliance_behave", "state_capacity", "Noncompliance (Stated Behavior)", "State Capacity", "D"),
  c("noncompliance_behave", "income_tax_of_gdp", "Noncompliance (Stated Behavior)", "Tax Capacity", "E"),
  c("noncompliance_behave", "next_year_income_tax", "Noncompliance (Stated Behavior)", "Tax Capacity (Year+1)", "F"),
  c("noncompliance_will", "state_capacity", "Noncompliance (Willingness)", "State Capacity", "G"),
  c("noncompliance_will", "income_tax_of_gdp", "Noncompliance (Willingness)", "Tax Capacity", "H"),
  c("noncompliance_will", "next_year_income_tax", "Noncompliance (Willingness)", "Tax Capacity (Year+1)", "I")
)

# Generate the list of plots
plots <- lapply(variables, function(var) {
  create_plot(var[1], var[2], var[3], var[4], panel, var[5])
})

# Arrange the plots into a grid (3 rows x 3 columns)
noncompliance_state_capacity <- grid.arrange(
  grobs = plots,
  nrow = 3,
  ncol = 3
)

ggsave("Output/Figures/noncompliance_state_capacity.jpg",noncompliance_state_capacity, width = 32, height = 30, dpi = 300)

