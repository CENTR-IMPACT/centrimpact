test_that("analyze_alignment input validation works", {
  # Test missing required columns
  expect_error(
    analyze_alignment(data.frame(role = "researcher", rating = 3)),
    "must contain columns: role, alignment, rating"
  )
  
  # Test invalid role values
  bad_roles <- data.frame(
    role = c("researcher", "admin"),
    alignment = c("Research_Questions", "Methodology"),
    rating = c(4, 5)
  )
  expect_error(
    analyze_alignment(bad_roles),
    "Role must contain only 'researcher' and 'partner' values"
  )
})

test_that("analyze_alignment calculates correct medians", {
  skip_if_not_installed("psych")
  skip_if_not_installed("irr")
  
  # Create test data with known medians
  test_data <- data.frame(
    role = rep(c("researcher", "partner"), each = 4),
    alignment = rep(c("A", "A", "B", "B"), 2),
    rating = c(1, 2, 3, 4, 2, 3, 4, 5),
    color = rep("#000000", 8)
  )
  
  results <- analyze_alignment(test_data)
  
  # Check medians are calculated correctly
  expect_equal(
    results$alignment_medians$researcher,
    c(0.00, 0.67),  # Normalized and rounded to 2 decimals
    tolerance = 1e-2
  )
  expect_equal(
    round(results$alignment_medians$partner, 2),
    c(0.33, 1.00),  # Normalized and rounded to 2 decimals
    tolerance = 1e-2
  )
  
  # Check geometric mean calculation with tolerance for floating point
  expected_means <- c(
    round(sqrt(0.00 * 0.33), 2),  # 0.00
    round(sqrt(0.67 * 1.00), 2)  # 0.82
  )
  expect_equal(
    results$alignment_medians$overall,
    expected_means,
    tolerance = 1e-2
  )
})

test_that("analyze_alignment handles edge cases", {
  skip_if_not_installed("psych")
  skip_if_not_installed("irr")
  
  # Test with tied ratings
  tied_data <- data.frame(
    role = rep(c("researcher", "partner"), each = 2),
    alignment = c("A", "A"),
    rating = c(1, 1, 5, 5),
    color = "#000000"
  )
  
  results <- analyze_alignment(tied_data)
  expect_equal(results$alignment_medians$researcher, 0.00, tolerance = 1e-2)
  expect_equal(results$alignment_medians$partner, 1.00, tolerance = 1e-2)
  
  # Test with single category
  single_cat <- data.frame(
    role = c("researcher", "partner"),
    alignment = "A",
    rating = c(3, 4),
    color = "#000000"
  )
  expect_silent(analyze_alignment(single_cat))
})

test_that("analyze_alignment returns expected structure", {
  skip_if_not_installed("psych")
  skip_if_not_installed("irr")
  
  # Create minimal valid input
  test_data <- data.frame(
    role = c("researcher", "partner"),
    alignment = "A",
    rating = c(3, 4),
    color = "#000000"
  )
  
  results <- analyze_alignment(test_data)
  
  # Check output structure
  expect_named(results, c("alignment_medians", "icc_score", "alignment_score"))
  expect_s3_class(results$icc_score, "icclist")
  expect_type(results$alignment_score, "character")
  
  # Check alignment_medians structure and column order
  expect_named(
    results$alignment_medians,
    c("alignment", "partner", "researcher", "overall"),
    ignore.order = TRUE
  )
})
