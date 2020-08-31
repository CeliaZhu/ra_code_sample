# Balance table of covariates
balancevar <- analysis_data %>%
  select(prior_arrests, age, treat, black, asian, white, male, female)

balance <- tbl_summary(
  balancevar,
  by = treat # split table by group
) %>%
  add_p() %>% # test if there's difference between groups
  as_gt() %>% 
  as_latex() %>% 
  as.character() %>%
  writeLines(con = "balance_test/output/balance_test.tex")


# Choose one observable characteristic and visualize the difference between those who were enrolled in the program and those who were not.
# Get rgb color for different shades of maroon
fill_pal <- rep(c("#800000E6", "#800000B3", "#80000080"), times = 3)
sig_levl <- 0.05

prior_arrests_chart <- analysis_data %>%
  group_by(treat, race) %>%
  summarise(
    avg_arrests = mean(prior_arrests),
    sd = sd(prior_arrests),
    n = n()
  ) %>%
  mutate(ci = 1.96*(sd/sqrt(n))) %>% 
  ungroup() %>% 
  mutate(
    plot_id = c(1, 2, 3, 5, 6, 7), 
    fill_pal = rep(c("#800000E6", "#800000B3", "#80000080"), times = 2)
  ) %>% 
  ggplot() +
  geom_col(aes(x = plot_id, y = avg_arrests, fill = race), position = "dodge") +
  scale_fill_manual(values = fill_pal, name = "Race", labels = c("Asian", "Black", "White")) +
  geom_errorbar(aes(x = plot_id, ymin = avg_arrests - ci, ymax = avg_arrests + ci), width = 0.2, colour = "#666666", alpha = 0.9, size = 1) +
  scale_x_continuous(breaks = c(2, 6), label = c("Unenrolled", "Enrolled")) +
  labs(
    x = "Enrollment status",
    y = "Average number of prior arrests",
    title = "Number of prior arrests is imbalanced between enrolled and unenrolled group",
    subtitle = "Average number of prior arrests and 95% confidence interval by race and enrollment status",
    caption = "Source: Cook County State's Attorney's Office"
  ) +
  theme_minimal() 

ggsave(prior_arrests_chart, "balance_test/output/prior_arrests_chart.png")

