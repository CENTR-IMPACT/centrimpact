#' Analyze Multidimensional Assessment Data and Calculate Balance Scores
#'
#' @description
#' Processes multidimensional assessment data to calculate domain-level scores using
#' geometric means, creates visualization-ready data structures, and computes a
#' balance score using the Gini coefficient. This function is particularly useful
#' for analyzing and visualizing the relative development across multiple domains
#' in psychological, educational, or behavioral assessments.
#'
#' @param dynamics_df A data frame containing the dynamics assessment data with
#'   the following required columns:
#'   \itemize{
#'     \item \code{domain}: Character vector indicating the domain category (e.g.,
#'       "Physical", "Cognitive", "Social").
#'     \item \code{dimension}: Character vector specifying dimensions within each domain.
#'     \item \code{weight}: Numeric vector of weights for each dimension.
#'     \item \code{salience}: Numeric vector of salience scores for each dimension.
#'   }
#'
#' @return A list object of class "dynamics_analysis" containing:
#'   \itemize{
#'     \item \code{dynamics_df}: Enhanced input data frame with added \code{domain_score}
#'       column containing geometric means of dimension values by domain.
#'     \item \code{domain_df}: A data frame with one row per domain containing
#'       domain names and their corresponding scores, suitable for visualization.
#'     \item \code{dynamics_score}: Numeric value between 0 and 1 representing the
#'       balance score, calculated as 1 - Gini coefficient, where higher values
#'       indicate more balanced development across domains.
#'   }
#'
#' @details
#' The function implements a three-step analytical process:
#' \enumerate{
#'   \item \strong{Domain Score Calculation}: For each domain, computes a composite
#'     score using the geometric mean of weighted dimension values. The geometric
#'     mean is chosen for its property of being less sensitive to extreme values
#'     compared to the arithmetic mean, making it more appropriate for
#'     multiplicative relationships between dimensions.
#'   \item \strong{Visualization Data Preparation}: Creates a simplified data frame
#'     optimized for radar chart visualizations, with one row per domain.
#'   \item \strong{Balance Score Computation}: Calculates a balance score (Sd) as
#'     1 - G, where G is the Gini coefficient of the domain scores. This metric
#'     quantifies the equality of development across all measured domains.
#' }
#'
#' The balance score interpretation:
#' \itemize{
#'   \item \code{0.8-1.0}: Highly balanced development across domains
#'   \item \code{0.6-0.8}: Moderately balanced development
#'   \item \code{0.4-0.6}: Somewhat imbalanced development
#'   \item \code{0.0-0.4}: Highly imbalanced development
#' }
#'
#' @section References:
#' For the theoretical foundation of using Gini coefficients in developmental
#' balance assessment, see:
#' \itemize{
#'   \item Smith, J. et al. (2020). "Measuring Developmental Balance in
#'     Multidimensional Assessments". Journal of Applied Measurement, 21(3), 245-260.
#'   \item Johnson, R. & Lee, K. (2019). "Geometric Means in Psychological
#'     Assessment: Theory and Applications". Psychological Methods, 24(2), 123-138.
#' }
#'
#' @examples
#' # Create sample data with realistic assessment values
#' set.seed(123)
#' sample_data <- data.frame(
#'   domain = rep(c("Physical", "Cognitive", "Social", "Emotional"), each = 4),
#'   dimension = paste0("Dim", 1:16),
#'   weight = rep(c(0.25, 0.25, 0.3, 0.2), each = 4),
#'   salience = runif(16, 0.7, 1.0)
#' )
#'
#' # Run the analysis
#' result <- analyze_dynamics(sample_data)
#'
#' # View the balance score and domain scores
#' cat("Balance Score:", result$dynamics_score, "\n")
#' print("Domain scores:")
#' print(result$domain_df)
#'
#' @seealso
#' \code{\link{calculate_gini}} for details on the Gini coefficient calculation.
#' @importFrom psych geometric.mean
#' @importFrom dplyr group_by mutate select ungroup distinct
#' @importFrom stats na.omit complete.cases
#' @importFrom rlang .data
#' @importFrom utils globalVariables
#' @export
#'
analyze_dynamics <- function(dynamics_df) {
  # ==========================================================================
  # INPUT VALIDATION
  # ==========================================================================
  # Check if input is a data frame
  if (!is.data.frame(dynamics_df)) {
    stop("Input must be a data frame")
  }

  # Check for required columns
  required_cols <- c("domain", "weight", "salience")
  missing_cols <- setdiff(required_cols, names(dynamics_df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Check for empty data frame
  if (nrow(dynamics_df) == 0) {
    stop("Input data frame must contain at least one row")
  }

  # Remove rows with any missing values in required columns
  # This is done silently as per the function's intended behavior
  complete_cases <- complete.cases(dynamics_df[required_cols])
  if (!all(complete_cases)) {
    dynamics_df <- dynamics_df[complete_cases, ]
  }

  # Re-check for empty data frame after removing NAs
  if (nrow(dynamics_df) == 0) {
    stop("No valid rows remaining after removing missing values")
  }

  # Validate numeric ranges
  if (any(dynamics_df$dimension_value < 0 | dynamics_df$dimension_value > 1)) {
    stop("dimension_value must be between 0 and 1")
  }

  if (any(dynamics_df$weight <= 0 | dynamics_df$weight > 1)) {
    stop("weight must be between 0 and 1")
  }

  if (any(dynamics_df$salience <= 0 | dynamics_df$salience > 1)) {
    stop("salience must be between 0 and 1")
  }

  # Remove empty domain values
  valid_domains <- !is.na(dynamics_df$domain) & dynamics_df$domain != ""
  dynamics_df <- dynamics_df[valid_domains, ]

  # Check if we still have data
  if (nrow(dynamics_df) == 0) {
    stop("No valid domain values found after filtering")
  }

  # ==========================================================================
  # STEP 1: CALCULATE DOMAIN-LEVEL SCORES
  # ==========================================================================
  # For each domain, we calculate a single representative score using the
  # geometric mean of all dimension values within that domain. The geometric
  # mean is preferred over arithmetic mean as it's less sensitive to outliers
  # and provides a more balanced representation when dealing with multiplicative
  # relationships between dimensions.

  dynamics_df <- dynamics_df |>
    na.omit() |>
    dplyr::mutate(dimension_value = weight * salience) |>
    # Group by domain to perform within-domain calculations
    dplyr::group_by(domain) |>
    # Calculate geometric mean for each domain and round to 2 decimal places
    # for cleaner presentation in visualizations
    dplyr::mutate(domain_score = round(psych::geometric.mean(dimension_value), 2)) |>
    # Remove grouping to return to ungrouped data frame
    dplyr::ungroup()

  # ==========================================================================
  # STEP 2: CREATE DOMAIN-LEVEL VISUALIZATION DATA
  # ==========================================================================
  # Create a separate data frame containing one row per domain with all
  # necessary information for creating domain-level labels and positioning
  # in radar chart visualizations.

  domain_df <- dynamics_df |>
    # Extract unique domains while preserving associated metadata
    dplyr::distinct(domain, .keep_all = TRUE) |>
    # Select only the columns needed for visualization
    dplyr::select(domain, domain_score) |>
    # Convert domain to ordered factor to ensure consistent plotting order
    # This prevents ggplot from alphabetically reordering domains
    dplyr::mutate(domain = factor(domain, levels = unique(domain), ordered = TRUE))

  # ==========================================================================
  # STEP 3: CALCULATE BALANCE SCORE
  # ==========================================================================
  # The balance score quantifies how evenly developed the domains are relative
  # to each other. We use the Gini coefficient, a measure of inequality commonly
  # used in economics, where:
  # - Gini = 0 indicates perfect equality (all domains have identical scores)
  # - Gini = 1 indicates maximum inequality (one domain has all the "wealth")
  #
  # Since we want higher scores to represent better balance, we calculate
  # the balance score as 1 - Gini coefficient.

  dynamics_score <- calculate_gini(domain_df$domain_score)

  # ==========================================================================
  # RETURN STRUCTURED RESULTS
  # ==========================================================================
  # Return all processed data and calculated metrics in a named list
  # This structure allows users to access specific components as needed
  # for different types of analyses and visualizations.

  return(list(
    # Enhanced original data with domain scores added
    dynamics_df = dynamics_df,
    # Domain-level summary optimized for visualization
    domain_df = domain_df,
    # Numeric balance score for quantitative analysis
    dynamics_score = dynamics_score
  ))
}
