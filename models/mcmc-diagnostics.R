library(tidyverse)
library(cmdstanr)
library(posterior)

# Helper function to get diagnostics
get_mcmc_report <- function(fit_obj, model_name) {
  
  # 1. Get full summary for ALL parameters
  # We exclude log_lik and y_rep/y_forecast to focus on the structural parameters,
  # but you can remove the filter if you want to check every single generated quantity.
  full_summary <- fit_obj$summary() %>%
    filter(!str_detect(variable, "log_lik|y_rep|y_forecast"))
  
  # 2. Extract Divergences (Specific to cmdstanr)
  diags <- fit_obj$diagnostic_summary()
  total_divs <- sum(diags$num_divergent)
  
  # 3. Create the "Worst Case" Summary row
  summary_row <- tibble(
    Model = model_name,
    Max_Rhat = max(full_summary$rhat, na.rm = TRUE),
    Min_Bulk_ESS = min(full_summary$ess_bulk, na.rm = TRUE),
    Min_Tail_ESS = min(full_summary$ess_tail, na.rm = TRUE),
    Total_Divergences = total_divs
  )
  
  return(list(details = full_summary, summary = summary_row))
}

# Assuming your fit objects are named: fit (for BLR), fit (for GAM), fit (for BSTS)
# Since you used the variable 'fit' in each script, make sure to save them 
# as separate names (e.g., fit_blr <- fit) before running the next script.

blr_diag <- get_mcmc_report(blr_fit, "BLR")
gam_diag <- get_mcmc_report(gam_fit, "GAM")
bsts_diag <- get_mcmc_report(bsts_fit, "BSTS")

# --- TABLE 1: Master Convergence Summary ---
mcmc_master_table <- bind_rows(
  blr_diag$summary,
  gam_diag$summary,
  bsts_diag$summary
)

print("--- MCMC Convergence: Worst-Case Diagnostics ---")
print(mcmc_master_table)

# --- TABLE 2: Detailed Parameters (Example: GAM) ---
# This gives you R-hat and ESS for every single beta, alpha, and spline coefficient
print("--- Detailed Diagnostics (GAM) ---")
print(gam_diag$details %>% select(variable, mean, rhat, ess_bulk))