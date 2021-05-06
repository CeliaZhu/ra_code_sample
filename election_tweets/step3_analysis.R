# Step 3: Analysis
# The goal of this step is to find whether campaign visits increase a candidate's twitter presence using event study.
# To replicate output files, please first run the .Rporj file, and execute R scripts in the project environment.
library(tidyverse)
library(data.table)
library(plm)
library(lmtest)
library(here)

# Relative paths----
clean_hashtag_biden_trump_path <- here("data", "built", "clean_hashtag_biden_trump.csv")
campaign_events_clean_path <- here("data", "built", "campaign_events_2020.csv")
event_study_basic_plot <- here("output", "figures", "event_study_basic_plot.png")
event_study_reg_plot_biden <- here("output", "figures", "event_study_reg_plot_biden.png")
event_study_reg_plot_trump <- here("output", "figures", "event_study_reg_plot_trump.png")
event_study_reg_restuls <- here("output", "tables", "event_study_reg_results.tex")

# Parameters----
vp_list <- c("Harris", "Pence")
# event window +- 2 days
event_window <- c(event_lag = -2, event_lead = 2)
# State codes
# Read state names and codes
states <- read_csv(here("data", "raw", "state_names.csv"))
state_names <- states$State
# Colors
trump_col <- "#fc4f30"
biden_col <- "#30a2da"

# Read data----
campaign_events <- read_csv(campaign_events_clean_path)
clean_hashtag_biden_trump <- read_csv(clean_hashtag_biden_trump_path)

# Downsize tweets data
selected_hashtag_biden_trump <- clean_hashtag_biden_trump %>%
  select(created_at, tweet_id, state, hashtag) %>%
  # remove tweets that mentioned both candidates for a cleaner comparison
  # remove tweets w.o state info
  filter(hashtag != "Both" & state %in% state_names) %>%
  # transform dates to number of days since 1970-01-01 so that it's easier to calculate lags and leads
  mutate(
    n_date = as.integer(difftime(created_at, as.Date("1970-01-01", "%Y-%m-%d"))),
    id = paste(state, hashtag, sep = "_")
  ) %>%
  count(id, n_date)
# Downsize campaign events data
selected_campaign_events <- campaign_events %>%
  filter(campaign_event == "Yes" & !candidate_s_involved %in% vp_list) %>%
  select(state, date, candidate_s_involved)

# Transform tweet data for event study----
suffix <- 1
for (s in unique(selected_campaign_events$state)) {
  for (c in c("Biden", "Trump")) {
    assign(paste0("df", suffix), selected_campaign_events %>%
      filter(state == s & candidate_s_involved == c) %>%
      mutate(
        treat = c,
        treatment_time = as.integer(difftime(date, as.Date("1970-01-01", "%Y-%m-%d"))),
        id = paste(state, candidate_s_involved, sep = "_")
      ) %>%
      select(-date, -candidate_s_involved))
    suffix <- suffix + 1
  }
}
# get function https://stackoverflow.com/a/14954589/14216571
time_list <- lapply(ls(pattern = "df[0-9]+"), function(x) get(x))
treatment_lags_leads <- rbindlist(time_list)
# Prepare the dataframe for event study
event_study_df <- left_join(selected_hashtag_biden_trump, treatment_lags_leads, by = "id") %>%
  mutate(lag_lead = n_date - treatment_time) %>%
  filter(lag_lead >= event_window[["event_lag"]] & lag_lead <= event_window[["event_lead"]]) %>%
  # aggregate multiple visits to the same state on the same day as one
  distinct(id, n_date, .keep_all = TRUE) %>%
  select(-treatment_time, -n_date) %>%
  rename(outcome = n) %>%
  mutate(
    lag_2 = if_else(lag_lead == -2, 1, 0),
    lag_1 = if_else(lag_lead == -1, 1, 0),
    lag_0 = if_else(lag_lead == 0, 1, 0),
    lead_1 = if_else(lag_lead == 1, 1, 0),
    lead_2 = if_else(lag_lead == 2, 1, 0)
  )

# # Descriptive plot: how the number of twitter hashtags changes
# event_study_df %>%
#   group_by(treat, lag_lead) %>%
#   summarise(mean = mean(outcome)) %>%
#   ggplot(aes(x = as.factor(lag_lead), y = mean, col = treat)) +
#   geom_point(size = 5) +
#   theme(
#     panel.background = element_rect(fill = "white"),
#     panel.grid.major = element_line(colour = "grey"),
#     panel.grid.minor = element_line(colour = "grey"),
#     axis.text = element_text(size = 12),
#     axis.title = element_text(size = 14, face = "bold"),
#     plot.title = element_text(face = "bold", hjust = 0.5)
#   ) +
#   labs(
#     x = "Number of days after the campaign event",
#     y = "Average number of twitter hashtags",
#     title = "Relationship between Presence on Twitter and Event Time",
#     col = "Candidate",
#     caption = "Data courtesy of FairVote and Kaggle user Manch Hui"
#   ) +
#   scale_color_manual(values = artyfarty::pal("five38"))
# ggsave(event_study_basic_plot)

# Event study regression----
# use state fixed effect b/c I am interested in short-run variation
# generalize regression function
es <- function(df, name) {
  naive <- plm(outcome ~ lag_2 + lag_1 + lag_0 + lead_1 + lead_2, data = df %>% filter(treat == {{ name }}), model = "within", index = "state")
  se <- coeftest(naive, vcov = vcovHC(naive, type = "HC0", cluster = c("group")))
  ci <- confint(se, level = 0.95)
  return(list(naive = naive, se = se, ci = ci))
}

# Biden sample and Trump sample
for (name in c("Biden", "Trump")) {
  assign(
    paste("es", name, sep = "_"),
    es(event_study_df, name)
  )
}

# # results
# stargazer::stargazer(es_Biden$naive, es_Trump$naive,
#   se = list(es_Biden$se[, 2], es_Trump$se[, 2]),
#   dep.var.caption = "",
#   dep.var.labels = "Number of twitter hashtags",
#   model.numbers = FALSE,
#   column.labels = c("Tweets hashtagged Biden", "Tweets hashtagged Trump"),
#   covariate.labels = c(
#     "Event time -2", "Event time -1",
#     "Event time 0", "Event time 1", "Constant"
#   ),
#   omit.stat = c("adj.rsq", "f"),
#   type = "latex",
#   header = FALSE,
#   float = TRUE,
#   table.placement = "h",
#   label = "tab:es_reg_results",
#   out = event_study_reg_restuls
# )

# plot event study regression results for biden hashtags
es_biden_plot <- tibble(
  var = c(-2, -1, 0, 1),
  coef = es_Biden$naive$coefficients,
  ci_low = es_Biden$ci[, 1],
  ci_high = es_Biden$ci[, 2],
  candidate = "Biden"
)
# normalize the treatment effects to 0 at event time -1
biden_phi_pre_one <- es_biden_plot[[2, 2]]
es_biden_plot <- es_biden_plot %>% 
  mutate(coef = coef - biden_phi_pre_one,
         ci_low = ci_low - biden_phi_pre_one, 
         ci_high = ci_high - biden_phi_pre_one)

# plot event study regressions results for biden hashtags
es_trump_plot <- tibble(
  var = c(-2, -1, 0, 1),
  coef = es_Trump$naive$coefficients,
  ci_low = es_Trump$ci[, 1],
  ci_high = es_Trump$ci[, 2],
  candidate = "Trump"
)
# normalize the treatment effect to 0 at event time -1
trump_phi_pre_one <- es_trump_plot[[2, 2]]
es_trump_plot <- es_trump_plot %>% 
  mutate(coef = coef - trump_phi_pre_one,
         ci_low = ci_low - trump_phi_pre_one,
         ci_high = ci_high - trump_phi_pre_one)

# biden sample
ggplot(es_biden_plot) +
  geom_point(aes(x = as.factor(var), y = coef), col = biden_col) +
  geom_line(aes(x = as.factor(var), y = coef, group = 1), col = biden_col, size = 1) +
  geom_errorbar(aes(x = as.factor(var), y = coef, ymin = ci_low, ymax = ci_high), 
                col = biden_col, width = .2, position = "dodge") +
  geom_hline(yintercept = 0, col = "grey") +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.ticks = element_blank(),
    axis.title = element_text(size = 14)  
    ) +
  labs(
    x = "Number of days relative to Biden's campaign event",
    y = "Average treatment effect on treated",
    caption = "Impose homogeneous treatment effect across states \n Data courtesy of FairVote and Kaggle user Manch Hui"
  ) 
ggsave(event_study_reg_plot_biden)

# trump sample
ggplot(es_trump_plot) +
  geom_point(aes(x = as.factor(var), y = coef), col = trump_col) +
  geom_line(aes(x = as.factor(var), y = coef, group = 1), col = trump_col, size = 1) +
  geom_errorbar(aes(x = as.factor(var), y = coef, ymin = ci_low, ymax = ci_high), 
                col = trump_col, width = 0.3, position = "dodge") +
  geom_hline(yintercept = 0, col = "grey") +
  theme(
    axis.title = element_text(size = 14),
    panel.background = element_rect(fill = "white"),
    axis.ticks = element_blank()
  ) +
  labs(
    x = "Number of days relative to Trump's campaign event",
    y = "Average treatment effect on treated",
    caption = "Impose homogeneous treatment effect across states \n Data courtesy of FairVote and Kaggle user Manch Hui"
  ) 
ggsave(event_study_reg_plot_trump)

