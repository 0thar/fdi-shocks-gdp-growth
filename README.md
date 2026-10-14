# FDI Shocks and GDP Growth

Do large inflows of foreign direct investment actually raise GDP per capita in the countries that receive them? Using a World Bank panel of 111 countries from 1996 to 2023, we find no meaningful effect once country and year differences are controlled for.

**[Read the paper (PDF)](paper.pdf)** · Econometrics final paper, Colby College, May 2026 · with Alex Ment and Tucker Reinhard

## Key findings

- A simple regression shows a positive, significant link between FDI and GDP per capita, but it explains almost nothing (R² = 0.03) and is driven by differences between countries.
- With country and year fixed effects and clustered standard errors, an FDI shock (inflow 1.5 to 2.5 SD above a country's mean) is associated with GDP per capita **$845 lower**, only marginally significant (p = 0.076).
- After inverse propensity weighting, the sign flips positive and the effect is small and insignificant in every sample. The consistent null across models points to a genuinely negligible effect rather than a lack of power.
- Treated and control countries follow nearly identical GDP paths, which supports the parallel trends assumption.

![Average log GDP per capita, treated vs. control countries](figures/gdp-trends-treated-vs-control.png)

## Methods

- **Data:** World Bank Open Data (FDI, GDP per capita, 13 governance and macro controls) plus HDI rankings to define developing countries. Reshaped from long format to a country-year panel; known tax havens dropped.
- **Models:** bivariate OLS, full OLS, stepwise selection, two-way fixed effects, and IPW-weighted TWFE, run on global and developing-country samples.
- **Tools:** R (`fixest`, `plm`, `modelsummary`, `tidyverse`), Quarto.

## Files

- `paper.pdf`: full paper with tables and diagnostics
- `code/analysis.R`: data cleaning, models, and figures
- `figures/`: key chart
