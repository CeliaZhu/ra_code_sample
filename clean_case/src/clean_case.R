# This script cleans case data and merge with demographic information

# Import case data and cleaned demo data
case <- read_csv("data/case.csv")
demo_clean <- read_csv("clean_demo/output/demo_clean.csv")

# Make sure no missing in case_id
# test_na function prepared in "general_functions.r" under folder "R"
test_na(case, "person_id")

# Check how many observations in cases do not have a match in demo
anti_join(case, demo_clean, by = "person_id")

# Join with cleaned demo data
case_demo <- left_join(case, demo_clean, by = "person_id")

# Check that if the numbers of rows before and after join can match
test_that(
  "demo and case are merged correctly",
  expect_equal(
    nrow(case),
    nrow(case_demo)
  )
)

# Restrict to residents in Chicago
case_demo_chi <- case_demo %>%
  filter(endsWith(address, "CHICAGO") | endsWith(address, "Chicago") | endsWith(address, "chicago"))

# Calculate age
case_demo_chi <- case_demo_chi %>%
    mutate(byear = ymd(bdate), arrest_year = ymd(arrest_date)) %>%
    mutate(age = round((arrest_year - byear)/362.5), 2)

# Write out cleaned data
case_demo_chi %>% write_csv("clean_case/output/case_demo_clean.csv")
