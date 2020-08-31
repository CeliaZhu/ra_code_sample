# This script estimates the effect of being enrolled into the program on the likelihood of being re-arrested before disposition.

# Specification 1: OLS
ols <- lm_robust(re_arrest ~ treat + prior_arrests + gender + race + age, data = analysis_data)
tb1 <- tbl_regression(ols)

# Specification 2: logit
logit <- glm(re_arrest ~ treat + prior_arrests + gender + race + age, data = analysis_data, family = binomial)
tb2 <- tbl_regression(logit)

# Specification 3: Propensity score matching (weighting)
pscore_estimate <- glm(treat ~ prior_arrests + gender + race + age, data = analysis_data, family = binomial)
pscore <- predict(pscore_estimate, type = "response")

analysis_data$pscore <- pscore

analysis_data$ate_weight <- if_else(analysis_data$treat == 1, (1 / analysis_data$pscore), (1 / (1 - analysis_data$pscore)))
analysis_data$atet_weight <- if_else(analysis_data$treat == 1, 1, (analysis_data$pscore / (1 - analysis_data$pscore)))

psm_ate <- glm(re_arrest ~ treat, data = analysis_data, weights = ate_weight, family = binomial)
psm_atet <- glm(re_arrest ~ treat, data = analysis_data, weights = atet_weight, family = binomial)
tb3 <- tbl_regression(psm_ate)
tb4 <- tbl_regression(psm_atet)

# Merge regression table
reg_results <- tbl_merge(list(tb1, tb2, tb3, tb4),
  tab_spanner = c("OLS ", "Logit", "Weighted Propensity Score Matching (ATE)", "Weighted Propensity Score Matching (ATET)")
) %>%
  as_gt() %>%
  as_latex() %>%
  as.character() %>%
  writeLines(con = "estimation/output/regression_results.tex")
