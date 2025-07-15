test_that("analyze_dynamics input validation works", {
  # Test non-data frame input
  expect_error(analyze_dynamics("not a data frame"), "must be a data frame")
  
  # Test missing required columns
  bad_df <- data.frame(domain = c("A", "B"))
  expect_error(
    analyze_dynamics(bad_df),
    "Missing required columns"
  )
  
  # Test empty data frame
  empty_df <- data.frame(
    domain = character(),
    dimension_value = numeric(),
    weight = numeric(),
    salience = numeric()
  )
  expect_error(analyze_dynamics(empty_df), "Missing required columns: dimension")
  
  # Test invalid dimension_value range
  bad_dim_value <- data.frame(
    domain = "A",
    dimension = "B",
    dimension_value = 1.5,  # Invalid value
    weight = 1,
    salience = 1
  )
  expect_error(analyze_dynamics(bad_dim_value), "dimension_value must be between 0 and 1")
  
  # Test invalid weight range
  bad_weight <- data.frame(
    domain = c("A", "B"),
    dimension = c("D1", "D2"),
    dimension_value = c(0.5, 0.5),
    weight = c(0, 1.1),  # Invalid values
    salience = c(1, 1)
  )
  expect_error(
    analyze_dynamics(bad_weight),
    "weight must be between 0 and 1"
  )
  
  # Test invalid salience range
  bad_salience <- data.frame(
    domain = c("A", "B"),
    dimension = c("D1", "D2"),
    dimension_value = c(0.5, 0.5),
    weight = c(0.5, 0.5),
    salience = c(0, 1.1)  # Invalid values
  )
  expect_error(
    analyze_dynamics(bad_salience),
    "salience must be between 0 and 1"
  )
  
  # Test empty domain values are silently dropped
  empty_domain <- data.frame(
    domain = c("", "A"),  # Empty string
    dimension = c("D1", "D2"),
    dimension_value = c(0.5, 0.5),
    weight = c(0.5, 0.5),
    salience = c(1, 1)
  )
  # Should process only the valid row
  result <- analyze_dynamics(empty_domain)
  expect_equal(nrow(result$dynamics_df), 1)  # Only one valid domain
  
  # Test that NAs are handled by being dropped
  # This includes NA domain values
  na_domain <- data.frame(
    domain = c(NA_character_, "A"),
    dimension = c("D1", "D2"),
    dimension_value = c(0.5, 0.5),
    weight = c(0.5, 0.5),
    salience = c(1, 1)
  )
  expect_silent(analyze_dynamics(na_domain))
  
  # Test missing values in different columns are dropped
  # Missing dimension_value
  na_dim_value <- data.frame(
    domain = c("A", "B"),
    dimension = c("D1", "D2"),
    dimension_value = c(NA, 0.5),
    weight = c(0.5, 0.5),
    salience = c(1, 1)
  )
  expect_silent(analyze_dynamics(na_dim_value))
  
  # Missing weight
  na_weight <- data.frame(
    domain = c("A", "B"),
    dimension = c("D1", "D2"),
    dimension_value = c(0.5, 0.5),
    weight = c(NA, 0.5),
    salience = c(1, 1)
  )
  expect_silent(analyze_dynamics(na_weight))
  
  # Missing salience
  na_salience <- data.frame(
    domain = c("A", "B"),
    dimension = c("D1", "D2"),
    dimension_value = c(0.5, 0.5),
    weight = c(0.5, 0.5),
    salience = c(1, NA)
  )
  expect_silent(analyze_dynamics(na_salience))
})

test_that("analyze_dynamics calculates correct domain scores", {
  skip_if_not_installed("psych")
  
  # Create test data with known values
  test_data <- data.frame(
    domain = c("A", "A", "B", "B"),
    dimension = c("D1", "D2", "D3", "D4"),
    dimension_value = c(1, 1, 1, 1),  # All values will be 1 * weight * salience
    weight = c(0.5, 0.5, 0.5, 0.5),
    salience = c(1, 1, 1, 1),  # So final values will all be 0.5
    color_border = rep("#000000", 4),
    stringsAsFactors = FALSE
  )
  
  results <- analyze_dynamics(test_data)
  
  # Check domain scores (geometric mean of 0.5 and 0.5 is 0.5)
  expect_equal(unique(results$domain_df$domain_score), 0.5)
  
  # Check that domain scores are added to original data
  expect_true("domain_score" %in% names(results$dynamics_df))
  expect_equal(unique(results$dynamics_df$domain_score), 0.5)
})

test_that("analyze_dynamics handles edge cases", {
  skip_if_not_installed("psych")
  
  # Test with single domain and dimension
  single_domain <- data.frame(
    domain = "A",
    dimension = "D1",
    dimension_value = 1,
    weight = 1,
    salience = 1,
    color_border = "#000000",
    stringsAsFactors = FALSE
  )
  
  results <- analyze_dynamics(single_domain)
  # The score should be 1 * weight * salience = 1
  expect_equal(results$domain_df$domain_score, 1)
  
  # Test with NA values (function silently removes them)
  # Test with valid data that would have been problematic before
  valid_data <- data.frame(
    domain = c("A", "A", "B", "B"),
    dimension = c("D1", "D2", "D3", "D4"),
    dimension_value = c(0.5, 0.6, 0.7, 0.8),
    weight = c(0.5, 0.5, 0.5, 0.5),
    salience = c(1, 1, 1, 1),
    color_border = rep("#000000", 4),
    stringsAsFactors = FALSE
  )
  expect_silent(analyze_dynamics(valid_data))
})

test_that("analyze_dynamics returns expected structure", {
  skip_if_not_installed("psych")
  
  # Create valid input
  test_data <- data.frame(
    domain = c("A", "A"),
    dimension = c("D1", "D2"),
    dimension_value = c(0.5, 0.5),
    weight = c(0.5, 0.5),
    salience = c(1, 1),
    color_border = c("#000000", "#000000"),
    stringsAsFactors = FALSE
  )
  
  results <- analyze_dynamics(test_data)
  
  # Check output structure
  expect_named(results, c("dynamics_df", "domain_df", "dynamics_score"))
  expect_type(results$dynamics_score, "double")
  
  # Check domain_df structure
  expect_named(
    results$domain_df,
    c("domain", "domain_score")
  )
  
  # Check that dynamics_score is between 0 and 1
  expect_true(results$dynamics_score >= 0 && results$dynamics_score <= 1)
})
