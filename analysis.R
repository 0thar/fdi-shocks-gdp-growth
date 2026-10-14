## --------------------------------------------------------------------------------------------------------
#| label: setup
#| message: false
#| warning: false
#| include: false
#| results: hide
# Setup Chunk
knitr::opts_chunk$set(
  echo = FALSE,
  warning = FALSE,
  message = FALSE,
  results = 'hold',
  fig.align = "center",
  cache = T
)

knitr::knit_hooks$set(document = function(x) {
  gsub("(\n)(#)", "\n\n\\2", x)
})
##----------------------------------------------------------------------------
library(tidyverse)              # Packages
library(psych)
library(psychTools)
library(dplyr)
library(knitr)
library(kableExtra)
library(epitools)
library(broom)
library(modelsummary)
library(kableExtra)
library(plm)
library(fixest)
library(stargazer)
library(naniar)
library(vtable)
library(ggcorrplot)
library(GGally)
library(marginaleffects)
library(tinytable)
library(ggfortify)
library(ggplot2)
set.seed(24)                    # Seed for reproducibility
options(                        # Custom Model Summary
  modelsummary_stars = TRUE,
  modelsummary_output = "markdown",
  modelsummary_shape = term ~ model,
  modelsummary_fmt = 2,
  modelsummary_gof_omit = "")

ms <- function(models, ...) {
  if (!is.list(models)) models <- list(models)
  gof_map <- data.frame(
    raw = c("r.squared", "adj.r.squared", "statistic", "p.value", "nobs"),
    clean = c("R²", "Adj. R²", "F", "p", "N"),
    fmt = c("%.3f", "%.3f", "%.2f", "%.3f", "%.0f")
  )
  modelsummary(models, stars = FALSE, output = "markdown",
               shape = term ~ model, fmt = 2,
               gof_map = gof_map,
               statistic = c("({std.error})", "p = {p.value}"), ...)
}

ms2 <- function(models, ...) {
  if (!is.list(models)) models <- list(models)
  modelsummary(models, stars = TRUE, output = "markdown",
               fmt = 2,
               estimate = "{estimate}{stars} ({std.error})",
               statistic = NULL,
               gof_omit = "AIC|BIC|Log|RMSE",
               coef_rename = c(
                "nat_resources" = "Natural Resources (% of GDP)",
                "corruption" = "Control of Corruption",
                "gov_effectiveness" = "Gov. Effectiveness",
                "pol_stability" = "Political Stability",
                "gini" = "Gini Coefficient",
                "tax_revenue" = "Tax Revenue (% of GDP)",
                "gov_expense" = "Gov. Expenditure (% of GDP)",
                "rule_of_law" = "Rule of Law",
                "reg_quality" = "Regulatory Quality",
                "literacy" = "Literacy Rate (%)",
                "hci" = "Human Capital Index",
                "energy_use" = "Energy Use (kg oil eq.)",
                "trade" = "Trade (% of GDP)",
                "gdp_pc" = "GDP per Capita (2015 USD)",
                "fdi" = "FDI (% of GDP)",
                "fdi_pos" = "FDI (positive)"
               ), ...)
}

diagnostic <- function(mod) {
  require(ggplot2)
  require(ggfortify)
  mod_name <- deparse(substitute(mod))
  obj_name <- paste0("diagnostic_", mod_name)
  p <- autoplot(mod) + theme_minimal()
  assign(obj_name, p, envir = parent.frame())
  invisible(p)
}

feols_diag <- function(model) {
  name <- deparse(substitute(model))
  df <- data.frame(
    fitted = fitted(model),
    resid = residuals(model)
  )
  
  p1 <- ggplot(df, aes(fitted, resid)) +
    geom_point(alpha = 0.3, size = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_smooth(se = FALSE) +
    labs(title = paste("Residuals vs Fitted:", name),
         x = "Fitted Values", y = "Residuals") +
    theme_minimal()
  
  p2 <- ggplot(df, aes(sample = resid)) +
    stat_qq(alpha = 0.3, size = 0.5) +
    stat_qq_line(linetype = "dashed") +
    labs(title = paste("Normal Q-Q:", name),
         x = "Theoretical Quantiles", y = "Standardized Residuals") +
    theme_minimal()
  
  library(patchwork)
  p1 + p2
}




## ----Code------------------------------------------------------------------------------------------------
#| include: false

knitr::opts_chunk$set(
  cache = T
)
## ----load-reshape--------------------------------------------------------
# May need to uncomment if files aren't loading properly
# Place Raw_Data_1.csv and developing-countries-2026.csv in this folder before running.
FDI_Data4 <- read.csv("Raw_Data_1.csv", comment.char = "#")

raw <- FDI_Data4
names(raw) <- gsub("\\.", " ", names(raw)) |> trimws()

long <- raw |>
  pivot_longer(
    cols      = matches("^X\\d{4}"),
    names_to  = "year_label",
    values_to = "value"
  ) |>
  mutate(
    year  = as.integer(str_extract(year_label, "\\d{4}")),
    value = suppressWarnings(as.numeric(value)),
    var   = case_when(
      `Series Name` == "Total natural resources rents (% of GDP)"                  ~ "nat_resources",
      `Series Name` == "Control of Corruption: Estimate"                           ~ "corruption",
      `Series Name` == "Government Effectiveness: Estimate"                        ~ "gov_effectiveness",
      `Series Name` == "Political Stability and Absence of Violence/Terrorism: Estimate" ~ "pol_stability",
      `Series Name` == "Gini index"                                                ~ "gini",
      `Series Name` == "Tax revenue (% of GDP)"                                    ~ "tax_revenue",
      `Series Name` == "Expense (% of GDP)"                                        ~ "gov_expense",
      `Series Name` == "Rule of Law: Estimate"                                     ~ "rule_of_law",
      `Series Name` == "Regulatory Quality: Estimate"                              ~ "reg_quality",
      `Series Name` == "Literacy rate, adult total (% of people ages 15 and above)" ~ "literacy",
      grepl("Human capital", `Series Name`)                                        ~ "hci",
      `Series Name` == "Energy use (kg of oil equivalent per capita)"              ~ "energy_use",
      `Series Name` == "Trade (% of GDP)"                                          ~ "trade",
      `Series Name` == "GDP per capita (constant 2015 US$)"                        ~ "gdp_pc",
      `Series Name` == "Foreign direct investment, net inflows (% of GDP)"         ~ "fdi",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(var)) %>%
  select(`Country Name`, `Country Code`, year, var, value)

panel_full <- long %>%
  pivot_wider(
    id_cols     = c(`Country Name`, `Country Code`, year),
    names_from  = var,
    values_from = value,
    values_fn   = mean
  ) %>%
  rename(country = `Country Name`, iso3 = `Country Code`) %>%
  rename_with(~ gsub("[^a-zA-Z0-9_]", "_", .x)) %>%
  filter(year <= 2023)


panel <- long %>%
  pivot_wider(
    id_cols     = c(`Country Name`, `Country Code`, year),
    names_from  = var,
    values_from = value,
    values_fn   = mean
  ) %>%
  rename(country = `Country Name`, iso3 = `Country Code`) %>%
  rename_with(~ gsub("[^a-zA-Z0-9_]", "_", .x)) %>%
  filter(year >= 1996, year <= 2023)


##-------------------------------------------------------------------------
# Creating Lagged GDP_pc
panel <- panel |> 
  arrange(iso3, year) |> 
  group_by(iso3) |> 
  mutate(gdp3 = dplyr::lead(gdp_pc, 3)) |> 
  ungroup()

# GDP Growth
panel <- panel |> 
  arrange(iso3, year) |> 
  group_by(iso3) |> 
  mutate(gdp_growth = 1-(gdp3/gdp_pc)) |> 
  ungroup()

# FDI Per Capita
panel$fdi.pp = panel$gdp_pc * panel$fdi / 100

# FDI Factor
panel <- panel |> 
  mutate(fdi_pos = as.integer(fdi > 0))

panel.pos <- panel |> filter(fdi > 0)



##-------------------------------------------------------------------------
# Bivariate Model
bivariate.model <- lm(gdp3 ~ fdi, data = panel)
summary(bivariate.model)

# Bivariate Factor Model
bivariate.model2 <- lm(gdp_growth ~ fdi_pos, data = panel)
summary(bivariate.model2)

# Bivariate Factor Model 2
bivariate.model3 <- lm(gdp_growth ~ fdi, data = panel)
summary(bivariate.model3)


##----------------------------------------------------------------------------
# Full Model
full <- lm(gdp3 ~ . - gdp_pc - country - year - iso3 - fdi_pos - fdi.pp - gdp_growth, data = panel)
summary(full)

#Full model for fdi_pos
full <- lm(gdp3 ~ . - gdp_pc - country - year - iso3 - fdi - fdi.pp - gdp_growth, data = panel)
summary(full)

# Manual Stepwise Regression
# Removing Variable With Highest p-value
m2 = update(full, . ~ . - gov_effectiveness)
summary(m2)

m3 = update(m2, . ~ . - gini)
summary(m3)

m4 = update(m3, . ~ . - rule_of_law)
summary(m4)

m5 = update(m4, . ~ . - pol_stability)
summary(m5)

m6 = update(m5, . ~ . - gdp_growth)
summary(m6)

m7 = update(m6, . ~ . - fdi_pp)
summary(m7)


##----------------------------------------------------------------------------

# 1. Create a vector of all variable names you are using
vars_to_keep <- c("gdp3", "rule_of_law", "gov_effectiveness", "corruption", 
                  "energy_use", "reg_quality", "gov_expense", "literacy", 
                  "pol_stability", "nat_resources", "gini", "tax_revenue", 
                  "fdi", "trade")

# 2. Subset your data to include only these columns and remove all NAs
clean_data <- na.omit(panel[, vars_to_keep])

# 3. Re-run the models using this clean_data
full_model <- lm(gdp3 ~ ., data = clean_data)
null_model <- lm(gdp3 ~ 1, data = clean_data)

# 4. Run the stepwise selection again
forward_model <- step(null_model, 
                      scope = list(lower = null_model, upper = full_model), 
                      direction = "forward")



##----------------------------------------------------------------------------
foward_step_lm <- lm(gdp3 ~ rule_of_law + energy_use + trade + nat_resources + fdi + tax_revenue + gov_expense + corruption + pol_stability + 
    reg_quality, data = clean_data)
summary(foward_step_lm)


##----------------------------------------------------------------------------
did_lm <- feols(gdp3 ~ fdi + rule_of_law + energy_use + trade + 
                nat_resources + tax_revenue + gov_expense + 
                corruption + pol_stability + reg_quality 
                | country + year, 
                data = panel, cluster = "country")

summary(did_lm)

# Extract the coefficient for FDI
fdi_coef <- coef(did_lm)["fdi"]

# Extract the p-value for FDI
fdi_p <- pvalue(did_lm)["fdi"]

# Print them nicely
cat("Coefficient:", fdi_coef, "\nP-value:", fdi_p)



##----------------------------------------------------------------------------
ggplot(data=panel)+geom_point(aes(x=year,y=fdi))



##----------------------------------------------------------------------------

tax_havens <- c("Luxembourg", "Bermuda", "Cayman Islands", "British Virgin Islands", "Switzerland", "Netherlands", "Singapore", "Macao SAR, China",
               "Panama", "Ireland", "Mauritius", "Bahamas", "Jersey", 
               "Isle of Man", "Belgium", "Hungary", "Liechtenstein", "Malta",
               "Guernsey", "Seychelles", "Marshall Islands", "Cyprus", "Monaco")

# 2. Filter the dataset
# This keeps only countries that are NOT in the tax_havens list
panel_clean <- panel %>%
  filter(!(country %in% tax_havens))

# 3. Check how many countries were removed
n_removed <- length(unique(panel$country)) - length(unique(panel_clean$country))
print(paste("Removed", n_removed, "tax havens from the study."))




##----------------------------------------------------------------------------
# 1. Create a vector of all variable names you are using
vars_to_keep2 <- c("gdp3", "rule_of_law", "gov_effectiveness", "corruption", 
                  "energy_use", "reg_quality", "gov_expense", "literacy", 
                  "pol_stability", "nat_resources", "gini", "tax_revenue",
                  "trade", "country", "year")

# 2. Subset your data to include only these columns and remove all NAs
panel_clean2 <- na.omit(panel_clean[, vars_to_keep2])

full_model <- lm(gdp3 ~ ., data = panel_clean2)
null_model <- lm(gdp3 ~ 1, data = panel_clean2)

# 4. Run the stepwise selection again
backward_model <- step(null_model, 
                      scope = list(lower = null_model, upper = full_model), 
                      direction = "backward")



##----------------------------------------------------------------------------
lmtest <- panel_clean2 |> 
  feols(gdp3 ~ rule_of_law + energy_use + trade + nat_resources + corruption + 
     tax_revenue + gov_expense + pol_stability + reg_quality
     + literacy + gini| year + country, data = _)

summary(lmtest)


##----------------------------------------------------------------------------
# Filter for outliers and get a unique list of countries
fdi_outliers <- panel |> 
  filter(fdi > 100 | fdi < -50) |> 
  select(country, year, fdi) |> 
  arrange(country, year)

# To see the full list in your console
print(fdi_outliers, n = Inf)

# If you just want a unique list of country names
unique_outlier_countries <- fdi_outliers |> 
  distinct(country) |> 
  pull(country)

print(unique_outlier_countries)




##----------------------------------------------------------------------------
tax_havens <- c("Luxembourg", "Bermuda", "Cayman Islands", "British Virgin Islands", "Switzerland", "Netherlands", "Singapore", "Macao SAR, China", "Panama", "Ireland", "Mauritius", "Bahamas", "Jersey", "Isle of Man", "Belgium", "Hungary", "Liechtenstein", "Malta", "Guernsey", "Seychelles", "Marshall Islands", "Cyprus", "Monaco", "Equatorial Guinea")

panel_clean4 <- panel %>%
  filter(!(country %in% tax_havens))

# 3. Check how many countries were removed
n_removed <- length(unique(panel$country)) - length(unique(panel_clean4$country))
print(paste("Removed", n_removed, "tax havens from the study."))



summary(panel_clean4$fdi)
sd_fdi<-sd(panel_clean4$fdi,na.rm=T)
fdi_mean<-mean(panel_clean4$fdi,na.rm=T)

#1SD above mean
one5_sd = fdi_mean+1.5*(sd_fdi)


#2SD above mean
two5_sd = fdi_mean+2.5*(sd_fdi)



#Add new vars to the dataset with diff fdi ranges as 1
panel_clean4 <- panel_clean4 |> mutate(fdi_ind2 = ifelse(fdi >= one5_sd & fdi <= two5_sd, 1, 0))

tab <- count(panel_clean4, country, fdi_ind2)

#Look at which countries are being selected in our FDI range. These are the treatment groups
treated_countries2 <- panel_clean4 |>
  filter(fdi_ind2 == 1) |>
  select(country) |>
  distinct(country) |>
  pull(country)

treated_countries2

panel_clean4 <- panel_clean4 |>
  mutate(treatment_group2 = case_when(country %in% treated_countries2 ~ 1,
                                    TRUE ~ 0))

# Just seeing how many countries are in the treatment group
tab <- panel_clean4 |>
  distinct(country, treatment_group2) |>
  count(treatment_group2)



#Make a new variable that is 1 any time a country is within the fdi shock range
panel_clean4 <- panel_clean4 |>
  group_by(country, treatment_group2) |>
  mutate(treatment_year = case_when(fdi_ind2 == 1 ~ year,
                                    TRUE ~ 0))

#Uses variable we just made and finds the minimum year each country has a 1 for that var. This is the first yr the country gets the fdi shock
first_shock <- panel_clean4 |>
  filter(fdi_ind2 == 1) |>
  group_by(country, treatment_group2) |>
  summarise(first_shock = min(year))

#Merge the new vars into the panel data
panel_clean4 <- panel_clean4 |>
  left_join(first_shock)

#Finally create a new var that once a country recieves a shock for the first time it will always be 1 after that year. This solves countries fdi levels changing causing them to enter and leave the range. This is our indicator variable
panel_clean4 <- panel_clean4 |>
  mutate(treat_post = case_when(treatment_group2 == 1 & year >= first_shock ~ 1,
                          TRUE ~ 0))

#Just looking at the data to make sure it worked
tab3 <- panel_clean4 |>
  filter(treat_post == 1) |> select(country, year, fdi_ind2, first_shock, treat_post) |>
distinct(country, .keep_all = TRUE)


  

#I need to change this model. Replace fdi_bi with treat_post but other than that it should be ok. This is a 2 way fixed effects model NOT DiD. Maybe review notes on 2 way fixed effects. DiD model for what we want is to complicated and actually not that much better
SLS_lm2 <- feols(gdp3 ~ treat_post + rule_of_law + energy_use + trade + 
                nat_resources + tax_revenue + gov_expense + 
                corruption + pol_stability + reg_quality
                | country + year, 
                data = panel_clean4, na.rm=T, cluster = "country")


modelsummary(SLS_lm2)
summary(SLS_lm2)



##----------------------------------------------------------------------------
# Calculate the average GDP per year for each group
trend_data_world <- panel_clean4 %>%
  group_by(year, treatment_group2) %>%
  summarize(mean_log_gdp = mean(log(gdp3), na.rm = TRUE)) %>%
  mutate(Group = if_else(treatment_group2 == 1, "Treated", "Control"))

# Plot the trends
trend_plot = ggplot(trend_data_world, aes(x = year, y = mean_log_gdp, color = Group, group = Group)) +
  geom_line(size = 1.2) +
  geom_point() +
  theme_minimal() +
  labs(
    title = "Raw GDP Trends: Treated vs. Control Groups",
    subtitle = "Checking for parallel slopes in the pre-treatment period",
    x = "Year",
    y = "Average GDP",
    color = "Group"
  ) +
  scale_color_manual(values = c("Control" = "#2c3e50", "Treated" = "#e74c3c")) +
  theme(legend.position = "bottom")



##----------------------------------------------------------------------------
african_countries <- c(
  "Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", 
  "Cabo Verde", "Cameroon", "Central African Republic", "Chad", "Comoros", 
  "Congo, Dem. Rep.", "Congo, Rep.", "Cote d'Ivoire", "Djibouti", "Egypt", 
  "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia", 
  "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Libya", 
  "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", 
  "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", 
  "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", 
  "South Sudan", "Sudan", "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe"
)
panel_africa <- panel %>%
  filter(!(country %in% tax_havens)) |> filter(country %in% african_countries)


length(unique(panel_africa$country))




summary(panel_africa$fdi)
sd_fdi_af<-sd(panel_africa$fdi,na.rm=T)
fdi_mean_af<-mean(panel_africa$fdi,na.rm=T)

#1SD above mean
one5_sd_af = fdi_mean_af+1.5*(sd_fdi_af)


#2SD above mean
two5_sd_af = fdi_mean_af+2.5*(sd_fdi_af)



#Add new vars to the dataset with diff fdi ranges as 1
panel_africa <- panel_africa |> mutate(fdi_ind = ifelse(fdi >= one5_sd_af & fdi <= two5_sd_af, 1, 0))

tab <- count(panel_africa, country, fdi_ind)

#Look at which countries are being selected in our FDI range. These are the treatment groups
treated_countries_af <- panel_africa |>
  filter(fdi_ind == 1) |>
  select(country) |>
  distinct(country) |>
  pull(country)

treated_countries_af

panel_africa <- panel_africa |>
  mutate(treatment_group2 = case_when(country %in% treated_countries_af ~ 1,
                                    TRUE ~ 0))

# Just seeing how many countries are in the treatment group
tab <- panel_africa |>
  distinct(country, treatment_group2) |>
  count(treatment_group2)



#Make a new variable that is 1 any time a country is within the fdi shock range
panel_africa <- panel_africa |>
  group_by(country, treatment_group2) |>
  mutate(treatment_year = case_when(fdi_ind == 1 ~ year,
                                    TRUE ~ 0))

#Uses variable we just made and finds the minimum year each country has a 1 for that var. This is the first yr the country gets the fdi shock
first_shock <- panel_africa |>
  filter(fdi_ind == 1) |>
  group_by(country, treatment_group2) |>
  summarise(first_shock = min(year))

#Merge the new vars into the panel data
panel_africa <- panel_africa |>
  left_join(first_shock)

#Finally create a new var that once a country recieves a shock for the first time it will always be 1 after that year. This solves countries fdi levels changing causing them to enter and leave the range. This is our indicator variable
panel_africa <- panel_africa |>
  mutate(treat_post = case_when(treatment_group2 == 1 & year >= first_shock ~ 1,
                          TRUE ~ 0))

#Just looking at the data to make sure it worked
tab3 <- panel_africa |>
  filter(treat_post == 1) |> select(country, year, fdi_ind, first_shock, treat_post) |>
distinct(country, .keep_all = TRUE)


  

#I need to change this model. Replace fdi_bi with treat_post but other than that it should be ok. This is a 2 way fixed effects model NOT DiD. Maybe review notes on 2 way fixed effects. DiD model for what we want is to complicated and actually not that much better
SLS_africa <- feols(gdp3 ~ treat_post + rule_of_law + energy_use + trade + 
                nat_resources + tax_revenue + gov_expense + 
                corruption + pol_stability + reg_quality 
                | country + year, 
                data = panel_africa, na.rm=T, cluster = "country")

summary(SLS_africa)
modelsummary(SLS_africa)

# Extract the coefficient for FDI
fdi_coef_af <- coef(SLS_africa)["treat_post"]

# Extract the p-value for FDI
fdi_p_af <- pvalue(SLS_africa)["treat_post"]

# Print them nicely
cat("Coefficient:", fdi_coef_af, "\nP-value:", fdi_p_af)




##----------------------------------------------------------------------------
SLS_lm_slides <- feols(gdp3 ~ treat_post + rule_of_law + energy_use + trade + 
                nat_resources + tax_revenue + gov_expense + 
                corruption + pol_stability + reg_quality 
                | country + year, 
                data = panel_clean4, na.rm=T, cluster = "country")



##----------------------------------------------------------------------------

# 1. Define your target subset of countries
target_countries <- c(
  "Angola", "Congo, Rep.", "Djibouti", "Guinea", "Liberia", 
  "Mozambique", "Mauritania", "Namibia", "Senegal", "Sierra Leone"
)

# 2. Filter the panel_africa data for these specific countries
plot_data <- panel_africa %>%
  filter(country %in% target_countries)

# 3. Create the plot
FDI_inflow = ggplot(plot_data, aes(x = year, y = fdi, color = country)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ country, scales = "free_y") +
  scale_x_continuous(breaks = seq(1995, 2025, 7)) +
  theme_minimal() +
  labs(
    title = "FDI Inflow Over Time (Selected African Countries)",
    subtitle = "Trend analysis for specific non-tax haven countries",
    x = "Year",
    y = "FDI Amount",
    color = "Country"
  ) +
  theme(legend.position = "none")
FDI_inflow

##----------------------------------------------------------------------------
# Define your list of explanatory variables
vars_to_avg <- c("gdp3", "rule_of_law", "gov_effectiveness", "corruption", 
                 "energy_use", "reg_quality", "gov_expense", "literacy", 
                 "pol_stability", "nat_resources", "gini", "tax_revenue",
                 "trade")

#Make new var that is year is before first shock is 1 or 1 if none shocked
panel_clean4 <- panel_clean4 %>%
  mutate(control = if_else(
    treatment_group2 == 0 | (treatment_group2 == 1 & year < first_shock), 
    1, 0))

#Make the wide data
panel_wide <- panel_clean4 |>
  # 1. Filter only controls
  filter(control == 1) |> 
  # 2. Group by country to collapse the time dimension
  group_by(country) |> 
  # 3. Apply mean() to all variables in your list
  summarize(
    across(all_of(vars_to_avg), ~mean(.x, na.rm = TRUE)),
    # Ensure you keep your treatment indicator (1 if they received the shock)
    treatment = max(treatment_group2, na.rm = TRUE))

datasummary_balance(~treatment_group2, data = panel_clean4, fmt = 2)

# Empty responses break the model later to remove worst(most empty vars)
colSums(is.na(panel_wide))

#New var list
vars_to_avg2 <- c("gdp3", "rule_of_law", "gov_effectiveness", "corruption", 
                  "reg_quality", "literacy", 
                 "pol_stability", "nat_resources",
                 "trade")

#Remake wide data
panel_wide2 <- panel_clean4 |>
  filter(control == 1) |> 
  group_by(country) |> 
  summarize(
    across(all_of(vars_to_avg2), ~mean(.x, na.rm = TRUE)),
    treatment = max(treatment_group2, na.rm = TRUE))

colSums(is.na(panel_wide2))

panel_wide2 <- panel_wide2 |> 
  filter(!country %in% c("Channel Islands", "Faroe Islands"))
#Was looking at the data and these 2 had literally no responses causing them to have tiny IPW so we revmoved them


#Now set any missing values to the median val for that var to not skew data but also solve the missing data problem. Most vars missing <25 responses outside of energy use and literacy

#Just omit NA's

panel_wide_full <- panel_wide2 |>
  mutate(across(all_of(vars_to_avg2), 
                ~ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)))

#Is there a better way to fix this???
#The missing data made it so I couldn't merge the data back into the other data set



logit_treatment <- glm(treatment ~ gdp3 + rule_of_law + gov_effectiveness + corruption + reg_quality + pol_stability + nat_resources + trade,
  data = panel_wide_full, family = binomial("logit"))

summary(logit_treatment)

panel_wide_weight <- panel_wide_full |>
  mutate(pscore = predict(logit_treatment, type = "response")) |>
  mutate(ipw = ifelse(treatment == 1, 1, pscore / (1 - pscore)))

panel_wide_weight <- panel_wide_weight |>
  mutate(weights = ipw)


#put IPW weights into panel data
panel_final <- panel_clean4 |>
  left_join(panel_wide_weight |> select(country, ipw), by = "country")

colSums(is.na(panel_final))

#Make our same model but now weigh it
SLS_weighted <- feols(gdp3 ~ treat_post
                | country + year, 
                data = panel_final, na.rm=T, cluster = "country", weights = ~ipw)

# This shows you how many rows are actually used in the model vs the original data
summary(SLS_weighted)
# Check specifically for "Observations removed" in the output
modelsummary(SLS_weighted)
#This is a model that weights the countries with IPW scores


##----------------------------------------------------------------------------
# Calculate the average GDP per year for each group
trend_data <- panel_final %>%
  group_by(year, treatment_group2) %>%
  summarize(mean_log_gdp = mean(log(gdp3), na.rm = TRUE)) %>%
  mutate(Group = if_else(treatment_group2 == 1, "Treated", "Control"))

# Plot the trends
ggplot(trend_data, aes(x = year, y = mean_log_gdp, color = Group, group = Group)) +
  geom_line(size = 1.2) +
  geom_point() +
  theme_minimal() +
  labs(
    title = "Raw GDP Trends: Treated vs. Control Groups",
    subtitle = "Checking for parallel slopes in the pre-treatment period",
    x = "Year",
    y = "Average GDP",
    color = "Group"
  ) +
  scale_color_manual(values = c("Control" = "#2c3e50", "Treated" = "#e74c3c")) +
  theme(legend.position = "bottom")

mean(panel_final$first_shock, na.rm = T)


##----------------------------------------------------------------------------
hdi <- read.csv("developing-countries-2026.csv", comment.char = "#")

# 1. Filter the data: HDI < 0.5 and not NA
developing_countries <- hdi$country[hdi$HumanDevelopmentIndex_1990 < 0.6 & !is.na(hdi$HumanDevelopmentIndex_1990)]
#0.6 was the world hdi in 1990 according to https://hdr.undp.org/data-center/human-development-index#/indicies/HDI 


# 2. View the list
print(developing_countries)

# 3. Optional: Save to a text file
write.table(developing_countries, "developing_list.txt", row.names = FALSE, col.names = FALSE)

panel_dev <- panel %>%
  filter(!(country %in% tax_havens)) |> filter(country %in% developing_countries)


##----------------------------------------------------------------------------
# 1. Calculate FDI thresholds for the developing subset
sd_fdi_dev <- sd(panel_dev$fdi, na.rm = TRUE)
fdi_mean_dev <- mean(panel_dev$fdi, na.rm = TRUE)

one5_sd_dev = fdi_mean_dev + 1.5 * (sd_fdi_dev)
two5_sd_dev = fdi_mean_dev + 2.5 * (sd_fdi_dev)

# 2. Add indicator based on the NEW thresholds
panel_dev <- panel_dev |> 
  mutate(fdi_ind = ifelse(fdi >= one5_sd_dev & fdi <= two5_sd_dev, 1, 0))

# 3. Identify Treated Countries
treated_countries_dev <- panel_dev |>
  filter(fdi_ind == 1) |>
  select(country) |>
  distinct(country) |>
  pull(country)

panel_dev <- panel_dev |>
  mutate(treatment_group2 = case_when(country %in% treated_countries_dev ~ 1,
                                     TRUE ~ 0))

# 4. Temporal Shock Logic
panel_dev <- panel_dev |>
  group_by(country, treatment_group2) |>
  mutate(treatment_year = case_when(fdi_ind == 1 ~ year,
                                    TRUE ~ 0))

first_shock_dev <- panel_dev |>
  filter(fdi_ind == 1) |>
  group_by(country, treatment_group2) |>
  summarise(first_shock = min(year), .groups = "drop")

panel_dev <- panel_dev |>
  left_join(first_shock_dev) |>
  mutate(treat_post = case_when(treatment_group2 == 1 & year >= first_shock ~ 1,
                                TRUE ~ 0))

# 5. Propensity Score Estimation
panel_wide_dev <- panel_dev |>
  filter(treat_post == 0) |> 
  group_by(country) |> 
  summarize(
    across(all_of(vars_to_avg2), ~mean(.x, na.rm = TRUE)),
    treatment = max(treatment_group2, na.rm = TRUE),
    .groups = "drop"
  )

colSums(is.na(panel_wide_dev))

# 4. Handle Missing Data via Median Imputation
panel_wide_dev <- panel_wide_dev |>
  mutate(across(all_of(vars_to_avg2), 
                ~if_else(is.na(.x), median(.x, na.rm = TRUE), .x)))


# Fix: Data source changed from panel_wide_africa to panel_wide_dev
logit_treatment2 <- glm(
  treatment ~ gdp3 + rule_of_law + gov_effectiveness + corruption + 
             reg_quality + pol_stability + nat_resources + trade,
  data = panel_wide_dev, 
  family = binomial("logit")
)

panel_weight_dev <- panel_wide_dev |>
  mutate(
    pscore = predict(logit_treatment2, type = "response"),
    ipw = if_else(treatment == 1, 1, pscore / (1 - pscore))
  )

# 6. Final Merge and Model
panel_final_dev <- panel_dev |>
  left_join(panel_weight_dev |> select(country, ipw), by = "country") |>
  filter(!is.na(ipw))

# 7. Weighted Fixed Effects Regression
# Fix: Summary call updated to match the model name
SLS_weighted_dev <- feols(
  gdp3 ~ treat_post
  | country + year, 
  data = panel_final_dev, cluster = ~country, weights = ~ipw)

modelsummary(SLS_weighted_dev, stars = TRUE)


##----------------------------------------------------------------------------
# Calculate the average GDP per year for each group
trend_data2 <- panel_final_dev %>%
  group_by(year, treatment_group2) %>%
  summarize(mean_log_gdp = mean(log(gdp3), na.rm = TRUE)) %>%
  mutate(Group = if_else(treatment_group2 == 1, "Treated", "Control"))

# Plot the trends
trend_plot = ggplot(trend_data2, aes(x = year, y = mean_log_gdp, color = Group, group = Group)) +
  geom_line(size = 1.2) +
  geom_point() +
  theme_minimal() +
  labs(
    title = "Raw GDP Trends: Treated vs. Control Groups",
    subtitle = "Checking for parallel slopes in the pre-treatment period",
    x = "Year",
    y = "Average GDP",
    color = "Group"
  ) +
  scale_color_manual(values = c("Control" = "#2c3e50", "Treated" = "#e74c3c")) +
  theme(legend.position = "bottom")

mean(panel_final$first_shock, na.rm = T)


##----------------------------------------------------------------------------
# 1. Setup Variable Lists and Constants
vars_to_avg <- c("gdp3", "rule_of_law", "gov_effectiveness", "corruption", 
                 "energy_use", "reg_quality", "gov_expense", "literacy", 
                 "pol_stability", "nat_resources", "gini", "tax_revenue", "trade")

# Updated list (removing vars with high missingness)
vars_to_avg2 <- c("gdp3", "rule_of_law", "gov_effectiveness", "corruption", 
                  "energy_use", "reg_quality", "pol_stability", "nat_resources", "trade")

african_countries <- c(
  "Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", 
  "Cabo Verde", "Cameroon", "Central African Republic", "Chad", "Comoros", 
  "Congo, Dem. Rep.", "Congo, Rep.", "Cote d'Ivoire", "Djibouti", "Egypt", 
  "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia", 
  "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Libya", 
  "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", 
  "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", 
  "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", 
  "South Sudan", "Sudan", "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe"
)

# 2. Filter for African non-tax-haven countries first
panel_africa <- panel_clean4 |>
  filter(country %in% african_countries) |>
  filter(!(country %in% tax_havens))

# 3. Create Wide Data for Propensity Scores (Pre-treatment only)
panel_wide_africa <- panel_africa |>
  filter(treat_post == 0) |> 
  group_by(country) |> 
  summarize(
    across(all_of(vars_to_avg2), ~mean(.x, na.rm = TRUE)),
    treatment = max(treatment_group2, na.rm = TRUE),
    .groups = "drop"
  )

colSums(is.na(panel_wide_africa))

# 4. Handle Missing Data via Median Imputation
panel_wide_africa <- panel_wide_africa |>
  mutate(across(all_of(vars_to_avg2), 
                ~if_else(is.na(.x), median(.x, na.rm = TRUE), .x)))

# 5. Calculate Propensity Scores and IPW
# Note: Using the trimmed variable list (vars_to_avg2) to match the data
logit_treatment2 <- glm(
  treatment ~ gdp3 + rule_of_law + gov_effectiveness + corruption + 
              energy_use + reg_quality + pol_stability + nat_resources + trade,
  data = panel_wide_africa, 
  family = binomial("logit")
)

panel_weight_africa <- panel_wide_africa |>
  mutate(
    pscore = predict(logit_treatment2, type = "response"),
    ipw = if_else(treatment == 1, 1, pscore / (1 - pscore))
  )

# 6. Merge Weights back to the Main Africa Panel
panel_final_africa <- panel_africa |>
  left_join(panel_weight_africa |> select(country, ipw), by = "country") |>
  # Drop countries that couldn't get a weight (if any)
  filter(!is.na(ipw))

# 7. Weighted Fixed Effects Regression
SLS_weighted_africa <- feols(
  gdp3 ~ treat_post + rule_of_law + energy_use + trade + 
         nat_resources + tax_revenue + gov_expense + 
         corruption + pol_stability + reg_quality 
  | country + year, 
  data = panel_final_africa, cluster = ~country, weights = ~ipw)

# Display Results
modelsummary(SLS_weighted_africa, stars = TRUE)




## ------------------------------------------------------------------------
# Diagnosing listwise deletion in a TWFE model.
# feols() drops any row with an NA in the outcome, covariates, or FEs —
# so the "effective sample" can differ a lot from the full panel.

## Estimate the TWFE
SLS_lm2 <- feols(gdp3 ~ treat_post + rule_of_law + energy_use + trade + 
                nat_resources + tax_revenue + gov_expense + 
                corruption + pol_stability + reg_quality
                | country + year, 
                data = panel_clean4, na.rm=T, cluster = "country")

## Identify the rows of data where no columns used in TWFE are missing
# all.vars() grabs every variable in the model (y, x's, and FEs) so we
# check missingness against exactly the columns feols() looked at.
vars <- all.vars(formula(SLS_lm2))

# TRUE = row was used in the regression; FALSE = silently dropped.
panel_clean4$used <- complete.cases(panel_clean4[, vars])

sum(panel_clean4$used)   # should equal m$nobs


## Summarize coverage by country
# Countries at the top of the sorted table contribute least to the
# regression — if those are the units you care about, the TWFE is
# effectively answering a different question than you think.

panel_clean4 %>%
  group_by(country) %>%
  summarise(
    n_total = n(),
    n_used  = sum(used),
    pct_used = round(100 * mean(used), 1)
  ) %>%
  arrange(pct_used) %>%
  print(n = Inf)



theme_set(theme_minimal() + 
          theme(plot.title = element_text(size = rel(0.9))))

# End of Setup Code


## ----Creation of Data Visualizations---------------------------------------------------------------------
# ── Summary Statistics Table ──────────────────────────────────────────
sum_vars <- panel_clean4 |>
  select(
    `GDP Per Capita (t+3)`      = gdp3,
    `FDI (% of GDP)`            = fdi,
    `Rule of Law`               = rule_of_law,
    `Energy Use (kg oil eq.)`   = energy_use,
    `Trade (% of GDP)`          = trade,
    `Natural Resources (% GDP)` = nat_resources,
    `Tax Revenue (% GDP)`       = tax_revenue,
    `Gov. Expense (% GDP)`      = gov_expense,
    `Corruption Control`        = corruption,
    `Political Stability`       = pol_stability,
    `Regulatory Quality`        = reg_quality
  )

summary_table = datasummary_skim(sum_vars,
                 histogram = FALSE,
                 fmt = 2,
                 output = "markdown")

sum_df <- panel_clean4 |>
  select(gdp3, fdi, rule_of_law, energy_use, trade,
         nat_resources, tax_revenue, gov_expense,
         corruption, pol_stability, reg_quality) |>
  as.data.frame()



## ----TWFE Unweighted-------------------------------------------------------------------------------------
#| tbl-cap: "Unweighted Two-Way Fixed-Effects Models Across Samples"

tbl_twfe_unweighted = ms2(list(
  "Global"     = SLS_lm2,
  "Africa"     = SLS_africa
))


## ----TWFE IPW--------------------------------------------------------------------------------------------
#| tbl-cap: "IPW-Weighted Two-Way Fixed-Effects Models"

tbl_twfe_ipw = ms2(list(
  "Global (IPW)"     = SLS_weighted,
  "Developing (IPW)" = SLS_weighted_dev,
  "Africa (IPW)"     = SLS_weighted_africa
))


## ----Missing Data Heatmap--------------------------------------------------------------------------------
#| fig-cap: Missing Data Heatmap by Variable and Year
#| fig-height: 5
#| fig-width: 6.5
#| include: false

missing_map = panel_full |>
  ungroup() |>
  select(year, fdi, rule_of_law, energy_use, trade,
         nat_resources, tax_revenue, gov_expense,
         corruption, pol_stability, reg_quality,
         literacy, gini) |>
  group_by(year) |>
  summarise(across(everything(), \(x) mean(is.na(x)) * 100)) |>
  pivot_longer(-year, names_to = "variable", values_to = "pct_missing") |>
  mutate(variable = case_match(variable,
    "fdi" ~ "FDI (% of GDP)",
    "rule_of_law" ~ "Rule of Law",
    "energy_use" ~ "Energy Use (kg oil eq.)",
    "trade" ~ "Trade (% of GDP)",
    "nat_resources" ~ "Natural Resources (% of GDP)",
    "tax_revenue" ~ "Tax Revenue (% of GDP)",
    "gov_expense" ~ "Gov. Expenditure (% of GDP)",
    "corruption" ~ "Control of Corruption",
    "pol_stability" ~ "Political Stability",
    "reg_quality" ~ "Regulatory Quality",
    "literacy" ~ "Literacy Rate (%)",
    "gini" ~ "Gini Coefficient"
  )) |>
  ggplot(aes(x = year, y = variable, fill = pct_missing)) +
  geom_tile() +
  geom_vline(xintercept = 1996, color = "steelblue", linewidth = 1, linetype = "dashed") +
  annotate("text", x = 1996.5, y = Inf, label = "1996: Sample Start",
           hjust = 0, vjust = 1.5, color = "steelblue", size = 3, fontface = "bold") +
  scale_fill_gradient(low = "#2ecc71", high = "#e74c3c",
                      name = "% Missing") +
  labs(
    title = "Missing Data by Variable and Year",
    x = "Year",
    y = NULL
  ) +
  theme_minimal()


## ----Model Summary 1-------------------------------------------------------------------------------------
#| eval: false
#| include: false
# m2_biv_null = ms2(list("Model 2" = m2,"Bivariate" = bivariate.model, "Null Model" = null_model))


## ----Model Building Table--------------------------------------------------------------------------------
#| tbl-cap: "Model Evolution: Bivariate and Variable Selection"

tbl_model_building = ms2(list(
  "Bivariate"    = bivariate.model,
  "Full"         = full,
  "Forward Step" = foward_step_lm,
  "FE (cont.)"   = did_lm
))


## ----Collinearity Check----------------------------------------------------------------------------------
# 2. Subset the dataset to only include your specific variables
collinearity_vars <- panel %>% 
  select(energy_use, trade, 
         nat_resources, tax_revenue, gov_expense, 
         corruption, pol_stability, reg_quality, fdi)

# 3. Calculate the correlation matrix (ignores missing values pairwise)
corr_matrix <- cor(collinearity_vars, use = "pairwise.complete.obs")

# 4. Generate the Heatmap (Recommended for clear visualization)
correlation_plot = ggcorrplot(corr_matrix, 
           hc.order = TRUE,           # Automatically clusters highly correlated variables together
           type = "lower",            # Cuts out duplicate top half
           lab = TRUE,                # Shows the actual correlation numbers
           lab_size = 3.5,            
           colors = c("#E46726", "white", "#6D9EC1"), # Orange (negative) to Blue (positive)
           title = "Collinearity Check for Selected Variables",
           ggtheme = theme_minimal())


## --------------------------------------------------------------------------------------------------------
diagnostic(full_model)


## --------------------------------------------------------------------------------------------------------
#| label: tbl-sum-stats
#| tbl-cap: "Summary Statistics for Key Variables"

datasummary_skim(sum_vars,
                 histogram = FALSE,
                 fmt = 2)


## --------------------------------------------------------------------------------------------------------
#| label: missing-var-map
#| fig-cap: "Missing Data Heatmap"
missing_map


## --------------------------------------------------------------------------------------------------------
#| label: correlation-plot
#| fig-cap: Collinearity Plot
correlation_plot


## --------------------------------------------------------------------------------------------------------
spec_lm <- lm(gdp3 ~ fdi + rule_of_law + energy_use + trade + 
                nat_resources + tax_revenue + gov_expense + 
                corruption + pol_stability + reg_quality, 
                data = clean_data, cluster = "country")

models_list <- list(
  "Bivariate Model" = bivariate.model,
  "Full Model"      = full_model,
  "Specified Variables"  = spec_lm
)


## --------------------------------------------------------------------------------------------------------
#| label: prelim-models
#| tbl-cap: "Preliminary Model Results"
msummary(
  models_list,
  shape = term ~ model,
  estimate = "estimate",
  statistic = NULL,
  stars = TRUE,
  gof_map = NA,
  output = "tinytable",
  title = "Basic Linear Regressions Table",
  coef_omit = "country"
) %>% 
  theme_tt("booktabs")


## --------------------------------------------------------------------------------------------------------
msummary(
  SLS_lm2,
  shape = term ~ model,
  estimate = "estimate",
  statistic = NULL,
  stars = TRUE,
  gof_map = NA,
  output = "tinytable",
  title = "TWFE Coefficients Table"
) %>% 
  theme_tt("booktabs")


## --------------------------------------------------------------------------------------------------------
models_list2 <- list(
  "Global IPW TWFE" = SLS_weighted,
  "Developing Countries IPW TWFE"      = SLS_weighted_dev)

msummary(
  models_list2,
  shape = term ~ model,
  estimate = "estimate",
  statistic = "p.value",
  stars = TRUE,
  gof_map = NA,
  output = "tinytable",
  title = "IPW TWFE Models"
) %>% 
  theme_tt("booktabs")


## --------------------------------------------------------------------------------------------------------
#| label: fig-trend-global
#| fig-cap: "Parallel Trends: Global Treated vs. Control"
#| fig-width: 6.5
#| fig-height: 4
#| out-width: "70%"

trend_plot


## --------------------------------------------------------------------------------------------------------
#| label: fig-trend-dev
#| fig-cap: "Parallel Trends: Developing Countries Treated vs. Control"
#| fig-width: 6.5
#| fig-height: 4
#| out-width: "70%"

trend_plot_dev <- ggplot(trend_data2, aes(x = year, y = mean_log_gdp, color = Group, group = Group)) +
  geom_line(size = 1.2) +
  geom_point() +
  theme_minimal() +
  labs(
    title = "Raw GDP Trends: Treated vs. Control (Developing Countries)",
    x = "Year",
    y = "Average ln(GDP)",
    color = "Group"
  ) +
  scale_color_manual(values = c("Control" = "#2c3e50", "Treated" = "#e74c3c")) +
  theme(legend.position = "bottom")

trend_plot_dev


## --------------------------------------------------------------------------------------------------------
#| eval: false
#| include: false
# trend_plot
# FDI_inflow


## --------------------------------------------------------------------------------------------------------
tbl_twfe_unweighted
tbl_twfe_ipw


## ----All Pairs-------------------------------------------------------------------------------------------
#| fig-cap: "Pairwise Correlations and Distributions of Key Variables"
#| fig-height: 10.5
#| fig-width: 10.5
#| include: true

panel_clean4 |>
  ungroup() |>
  filter(fdi > -50, fdi < 100) |>
  select(gdp3, fdi, rule_of_law, energy_use, trade,
         nat_resources, tax_revenue, gov_expense,
         corruption, pol_stability, reg_quality) |>
  mutate(
    gdp3 = log(gdp3),
    energy_use = log(energy_use)
  ) |>
  rename(`ln(gdp3)` = gdp3, `ln(energy)` = energy_use) |>
  drop_na() |>
  mutate(across(everything(), \(x) {
    q <- quantile(x, c(0.05, 0.95), na.rm = TRUE)
    pmin(pmax(x, q[1]), q[2])
  })) |>
  pairs.panels(
    method = "pearson",
    hist.col = "steelblue",
    density = TRUE,
    ellipses = FALSE,
    lm = TRUE,
    stars = TRUE,
    pch = ".",
    col = adjustcolor("#000000", alpha.f = 0.2),
    gap = 0,
    cex.labels = 1.2
  )


## ----Econ Pairs------------------------------------------------------------------------------------------
#| fig-cap: "Pairwise Correlations: Economic Variables"
#| fig-height: 10.5
#| fig-width: 10.5
#| include: true

panel_clean4 |>
  ungroup() |>
  filter(fdi > -50, fdi < 100) |>
  select(gdp3, fdi, energy_use, trade,
         nat_resources, tax_revenue, gov_expense) |>
  mutate(
    gdp3 = log(gdp3),
    energy_use = log(energy_use)
  ) |>
  rename(`ln(gdp3)` = gdp3, `ln(energy)` = energy_use) |>
  drop_na() |>
  mutate(across(everything(), \(x) {
    q <- quantile(x, c(0.05, 0.95), na.rm = TRUE)
    pmin(pmax(x, q[1]), q[2])
  })) |>
  pairs.panels(
    method = "pearson",
    hist.col = "steelblue",
    density = TRUE,
    ellipses = FALSE,
    lm = TRUE,
    stars = TRUE,
    pch = ".",
    col = adjustcolor("#000000", alpha.f = 0.2),
    gap = 0,
    cex.labels = 1.2
  )


## ----Inst Pairs------------------------------------------------------------------------------------------
#| fig-cap: "Pairwise Correlations: Institutional Variables"
#| fig-height: 10.5
#| fig-width: 10.5
#| include: true

 panel_clean4 |>
  ungroup() |>
  filter(fdi > -50, fdi < 100) |>
  select(gdp3, fdi, rule_of_law, corruption,
         pol_stability, reg_quality) |>
  mutate(gdp3 = log(gdp3)) |>
  rename(`ln(gdp3)` = gdp3) |>
  drop_na() |>
  mutate(across(everything(), \(x) {
    q <- quantile(x, c(0.05, 0.95), na.rm = TRUE)
    pmin(pmax(x, q[1]), q[2])
  })) |>
  pairs.panels(
    method = "pearson",
    hist.col = "steelblue",
    density = TRUE,
    ellipses = FALSE,
    lm = TRUE,
    stars = TRUE,
    pch = ".",
    col = adjustcolor("#000000", alpha.f = 0.2),
    gap = 0,
    cex.labels = 1.2
  )


## --------------------------------------------------------------------------------------------------------
#| ref.label: !expr knitr::all_labels()
#| echo: false
#| eval: false
# NA

