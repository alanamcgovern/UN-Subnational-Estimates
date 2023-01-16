
# Crisis Adjustments

## General strategy

- Benchmarking is done to UN-IGME national estimates without their crisis adjustment
- UN-IGME national excess deaths are split into admin1 and admin2 units
- Compute admin1 and admin2 crisis-related probabilities of death from excess deaths and population
- Final crisis-adjusted admin1 and admin2 mortality rates are the sum of benchmarked and crisis-specific probabilities

1. Ebola 2014-2016

**Countries:** Liberia, Sierra Leone, Guinea

**Notes:**

- Adjust under-5 mortality only, not neonatal mortality rate, per UN-IGME protocol
- Data source: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5145133/
- Steps:
  - `ebola.R` script used to save `crisis_{ISO3}.rda` files containing excess deaths
  - `crisis_deaths_to_qx.R` contains function which can be used to convert excess deaths to qx provided population denominators
  - Compute final results as benchmarked 5q0 + crisis 5q0

2. Haiti 2010 earthquake

**Notes:** all excess deaths to Ouest Admin1, then divided proportionally by population

3. Myanmar 2008 Cyclone

**Notes:** Two Admin 1 areas impacted; excess deaths distributed proportionally by under-5 population to each Admin 2 within these areas.
