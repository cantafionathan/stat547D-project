# Disentangling Regulatory and Pandemic Effects

## Project Overview

In January 2021, Seattle implemented a minimum compensation ordinance for ride-share drivers. This study utilizes a **Bayesian synthetic control framework** to estimate the "lift" in transit ridership following the policy, using Portland, Oregon, as the primary donor city.

## Methodology

The analysis compares three Bayesian specifications to construct the counter-factual ridership trajectory:

-   **Bayesian Linear Regression (BLR):** A baseline linear combination of predictors.
-   **Generalized Additive Models (GAM):** Incorporates a cubic B-spline for non-linear temporal trends. This was the **preferred model** due to its balance of flexibility and numerical stability.
-   **Bayesian Structural Time Series (BSTS):** A state-space approach capturing latent levels and seasonality (noted for high instability in this application).

## Technical Stack

-   **Probabilistic Programming:** Models implemented in **Stan** via the **`cmdstanr`** interface.
-   **Diagnostics:** Performance evaluated using **PSIS-LOO (ELPD)**, **Pareto** $k$, and MCMC convergence metrics ($\hat{R}$ and ESS).

## Key Findings

-   The preferred GAM specification identified a **30.9% mean increase** in ridership relative to the counter-factual (95% CI: [9.1%, 53.6%]).
-   **Critical Note:** The magnitude of this lift likely reflects structural confounding from concurrent transit infrastructure expansions, such as the Northgate Link Extension.

------------------------------------------------------------------------

*This project was completed for **STAT 547D: Bayesian Workflow** at the University of British Columbia.*
