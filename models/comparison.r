#####################################################################
## Master Comparison Table Construction

library(gt) # Optional: for a beautiful table

# Helper function to extract Pareto k counts
count_k <- function(loo_obj) {
  pk <- loo_obj$diagnostics$pareto_k
  tibble(
    k_good = sum(pk < 0.7),
    k_warn = sum(pk >= 0.7 & pk <= 1),
    k_bad  = sum(pk > 1)
  )
}

# Combine results into one tibble
all_results <- list(blr_results, gam_results, bsts_results)

comparison_table <- all_results %>%
  map_dfr(function(res) {
    k_counts <- count_k(res$loo_obj)
    
    tibble(
      Model = res$model_name,
      ELPD = res$elpd,
      `ELPD (SE)` = res$elpd_se,
      `k < 0.7` = k_counts$k_good,
      `0.7 < k < 1` = k_counts$k_warn,
      `k > 1` = k_counts$k_bad,
      `RMSE (Pre)` = res$rmse_pre,
      `ATE (Post)` = res$ate,
      `Portland Wgt` = res$portland_weight,
      `Spec. Param` = res$spec_param
    )
  }) %>%
  arrange(desc(ELPD))

# Print the final result
print("--- Final Model Comparison ---")
comparison_table %>%
  mutate(across(where(is.numeric), ~round(., 4))) %>%
  print()

# Logic for interpreting the 'Spec. Param' column in your paper:
# For GAM: Spec. Param = Tau (Higher means trend is more flexible/wiggly)
# For BSTS: Spec. Param = Trend-to-Noise Ratio (Higher means trend is more volatile)
# For BLR: Spec. Param = NA
