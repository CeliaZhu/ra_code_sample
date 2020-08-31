# Descriptive statistics for demographic variables (output in html)
summary <- tbl_summary(
  analysis_data %>%
    select(black, asian, white, male, female, prior_arrests, age),
  statistic = list(all_continuous() ~ "{mean}({sd})")
) %>% 
  as_gt() %>% 
  as_latex() %>% 
  as.character() %>%
  writeLines(con = "summary_statistics/output/summary.tex")


