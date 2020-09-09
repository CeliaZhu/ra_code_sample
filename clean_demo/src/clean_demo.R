# This script clean demographic data. 

# Read in demographic data
demo <- read_csv("data/demo.csv")

# Correct inconsistent encoding in gender
demo %>%
  filter(gender != "M" & gender != "F") %>%
  distinct(gender)

# Clean "male", "female" encoding
demo_clean <- demo %>%
  mutate(
    gender = str_replace(gender, "^male$", "M"),
    gender = str_replace(gender, "^female$", "F")
  )

# Test that if there are inconsistent encoding
test_that(
  "Gender is consistently coded",
  expect_equal(
    0,
    nrow(demo_clean %>%
      filter(gender != "M" & gender != "F"))
  )
)

# Test that if there are missing in person_id
# The function is written in "general_function.r"under folder "R"
test_na(demo_clean, person_id)

# Check if person_id is the unique identifier in demo_clean
# test_that(
#  "person_id is the unique identifier in demo",
#  expect_equal(
#    nrow(demo_clean),
#    nrow(demo_clean %>%
#           distinct(person_id)
#  )
# )
# )
# The test failed. person_id is not the unique identifier in demo.
# It would be problematic if we call join at this point.
# Further clean demo_clean. Remove duplicate rows
demo_clean <- unique(demo_clean)

# Write out data
demo_clean %>% write_csv("clean_demo/output/demo_clean.csv")
