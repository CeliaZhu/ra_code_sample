# Create a data set for statistical analysis
analysis_data <- case_demo_chi %>%
  mutate(black = (race == "BLACK"), asian = (race == "ASIAN"), white = (race == "WHITE")) %>%
  mutate(male = (gender == "M"), female = (gender == "F"))

# label data set to creat formatted tables
analysis_data <- expss::apply_labels(analysis_data,
  re_arrest = "Rearrested",
  prior_arrests = "Number of prior arrests",
  gender = "Gender",
  age = "Age",
  black = "Black",
  asian = "Asian",
  white = "White",
  male = "Male",
  female = "Female",
  treat = "Treatment",
  treat = c(
    "Treatment" = 1,
    "Control" = 0
  )
)

# Check that the study population has 25,000 subjects.
test_that(
  "The study population has 25,000 subjects",
  expect_equal(
    25000,
    nrow(case_reg)
  )
)

# From now on I will use analysis_data for statistical analysis. Save the cleaned data.
saveRDS(analysis_data, "prepare_analysis_data/output/analysis_data.rds")
