#' Calculate Alignment Between Researchers and Partners
#'
#' @description
#' This function calculates and visualizes the alignment between researcher and partner ratings
#' across different alignment categories. It computes median ratings, inter-rater agreement (ICC),
#' and prepares data for visualization. The function implements a two-way random effects model
#' to assess absolute agreement between raters.
#'
#' @details
#' The function performs the following key operations:
#' 1. Validates input data structure and values
#' 2. Calculates interpolated medians for each alignment category by role
#' 3. Computes geometric means of researcher and partner medians for overall scores
#' 4. Calculates intraclass correlation coefficient (ICC) to quantify inter-rater agreement
#' 5. Returns structured results for further analysis and visualization
#'
#' @param alignment_df A data frame containing the following columns:
#' \describe{
#'   \item{role}{Character vector indicating 'researcher' or 'partner'}
#'   \item{alignment}{Character vector of alignment categories}
#'   \item{rating}{Numeric vector of ratings (typically on a Likert scale)}
#'   \item{color}{Character vector of hex color codes for each alignment category (optional)}
#' }
#'
#' @return A list with the following components:
#' \describe{
#'   \item{alignment_medians}{Data frame containing median ratings by alignment category and role}
#'   \item{icc_score}{ICC object from `irr::icc()` with inter-rater reliability statistics}
#'   \item{alignment_score}{Formatted ICC value as a character string}
#' }
#'
#' @section Note:
#' The ICC is calculated using a two-way random effects model for absolute agreement (single rater).
#' The function uses interpolation to handle tied values when calculating medians.
#'
#' @references
#' McGraw, K. O., & Wong, S. P. (1996). Forming inferences about some intraclass
#' correlation coefficients. Psychological Methods, 1(1), 30-46.
#'
#' @importFrom dplyr group_by summarize mutate select distinct left_join rowwise ungroup
#' @importFrom psych geometric.mean interp.median
#' @importFrom irr icc
#' @importFrom tidyr pivot_wider
#' @importFrom stats na.omit
#' @importFrom rlang .data
#' @importFrom utils globalVariables
#' @export
#'
#' @examples
#' # Create example data with realistic alignment categories
#' set.seed(123)
#' alignment_data <- data.frame(
#'   role = rep(c("researcher", "partner"), each = 20),
#'   alignment = rep(c("Research_Questions", "Methodology", "Timeline", 
#'                   "Data_Needs", "Impact_Goals"), 8),
#'   rating = sample(1:5, 40, replace = TRUE, prob = c(0.1, 0.2, 0.4, 0.2, 0.1)),
#'   color = rep(c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2"), 8)
#' )
#'
#' # Run analysis
#' results <- analyze_alignment(alignment_data)
#' 
#' # View alignment score (ICC)
#' cat("Inter-rater agreement (ICC):", results$alignment_score, "\n")
#' 
#' # View median ratings by role and alignment
#' print(results$alignment_medians)

analyze_alignment <- function(alignment_df) {

  # ===========================================================================
  # [1] Input Validation
  # ===========================================================================
  # Ensure required columns are present
  required_cols <- c("role", "alignment", "rating")
  if (!all(required_cols %in% names(alignment_df))) {
    stop("Input data must contain columns: ",
         paste(required_cols, collapse = ", "))
  }

  # Ensure role has expected values
  if (!all(unique(alignment_df$role) %in% c("researcher", "partner"))) {
    stop("Role must contain only 'researcher' and 'partner' values")
  }

  # ===========================================================================
  # [2] Calculate Summary Statistics
  # ===========================================================================
  # Calculate median ratings for each alignment category by role
  # Using psych::interp.median which handles ties by interpolation
  medians <- alignment_df |>
    dplyr::group_by(role, alignment) |>
    dplyr::summarize(
      int_median = round(psych::interp.median(rating, w = 1), 2),
      .groups = "drop"
    ) |>
    dplyr::mutate(int_median = normalize(int_median)) |>
    tidyr::pivot_wider(
      names_from = role,
      values_from = int_median
    ) |>
    # Calculate geometric mean of researcher and partner medians for overall score
    dplyr::rowwise() |>
    dplyr::mutate(
      overall = round(psych::geometric.mean(c(researcher, partner), na.rm = TRUE), 2)
    ) |>
    dplyr::ungroup() |>
    # Add back color information for plotting
    dplyr::left_join(
      dplyr::distinct(dplyr::select(alignment_df, alignment)),
      by = "alignment"
    )

  # ===========================================================================
  # [3] Calculate Inter-rater Agreement (ICC)
  # ===========================================================================
  # Calculate Intraclass Correlation Coefficient (ICC) to quantify the agreement
  # between researchers and partners across all alignment categories.
  # Using two-way random effects model for absolute agreement (single rater)
  icc_score <- irr::icc(
    dplyr::select(medians, researcher, partner),
    type = "agreement",  # Absolute agreement rather than consistency
    model = "twoway",    # Both rater and target effects are random
    unit = "single"      # Single rater reliability
  )

  # Format the ICC value for display
  # Using absolute value since we're interested in strength of agreement
  alignment_score <- sprintf("%.2f", abs(icc_score$value))

  # ===========================================================================
  # [5] Return Results
  # ===========================================================================
  # Compile all results into a structured list
  results <- list(
    alignment_medians = medians,            # Median ratings by alignment and role
    icc_score = icc_score,        # Full ICC model object
    alignment_score = alignment_score  # Formatted ICC value
  )

  return(results)
}
