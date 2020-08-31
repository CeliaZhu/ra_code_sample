# This script clean grades data set and calculate gpa

# Import grades data
grades <- read_csv("data/grades.csv")

# Translate letter grades to number grades. 
grades_clean <- grades %>%
  mutate_at(
    vars(starts_with("gr")),
    funs(case_when(
      . == "A" ~ 4,
      . == "B" ~ 3,
      . == "C" ~ 2,
      . == "D" ~ 1,
      . == "F" ~ 0
    ))
  ) 

for (i in 9:10) {
  grades_clean[, paste0("gpa", i)] <- rowMeans(select(grades_clean, starts_with(paste0("gr", i))), na.rm = TRUE)
}

# Save the cleaned data in case colleagues need to use it.
write_csv(grades_clean, "clean_grades/output/grades_clean.csv")
