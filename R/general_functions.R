# The function `test_na()` checks if there are missing values in uid
test_na <- function(df, x) {
  test_that(
    "No missing value in primary key person_id",
    expect_equal(
      0,
      nrow(df %>%
        filter(is.na(x)))
    )
  )
}

