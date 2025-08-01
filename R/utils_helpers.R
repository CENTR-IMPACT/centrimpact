# ==============================================================================
# SECTION 2: MATHEMATICAL AND STATISTICAL UTILITY FUNCTIONS
# ==============================================================================
# Core mathematical functions for data transformation, scaling, and
# statistical calculations used throughout the analysis pipeline.

#' Normalize a numeric vector to 0-1 range
#'
#' @description
#' Performs min-max normalization to scale values between 0 and 1.
#' This is useful for standardizing different metrics to a common scale.
#'
#' @param x Numeric vector. Values to normalize.
#'
#' @return Numeric vector. Normalized values between 0 and 1.
#'
#' @details Uses the formula: (x - min(x)) / (max(x) - min(x))
#'   The minimum value becomes 0, the maximum becomes 1, and all others
#'   are proportionally scaled between these bounds.
#'
#' @examples
#' values <- c(10, 20, 30, 40, 50)
#' normalize(values)
#' # Returns: c(0.0, 0.25, 0.5, 0.75, 1.0)
#'
#' @family utility functions
#' @export
normalize <- function(x) {
  # Apply min-max normalization formula
  # Subtract minimum to shift range to start at 0
  # Divide by range to scale maximum to 1
  (x - min(x)) / (max(x) - min(x))
}

#' Calculate a proportional linear multiplier based on count
#'
#' @description
#' Maps counts in a vector linearly to a specified multiplier range.
#' The minimum count gets the min_multiplier, the maximum count gets the max_multiplier.
#' This function is useful for scaling visual elements (like point sizes, line widths,
#' or alpha values) proportionally to data values.
#'
#' @param count Numeric. The raw count for the current item/layer.
#' @param all_counts Numeric vector. All raw counts to determine min/max for scaling.
#' @param min_multiplier Numeric. The minimum multiplier value (applied to the min count). Default is 0.5.
#' @param max_multiplier Numeric. The maximum multiplier value (applied to the max count). Default is 1.0.
#'
#' @return Numeric. A multiplier between min_multiplier and max_multiplier.
#'
#' @details The function performs linear interpolation between min_multiplier and max_multiplier
#'   based on where the count falls within the range of all_counts. If all counts are identical,
#'   the function returns max_multiplier to avoid division by zero.
#'
#' @examples
#' counts <- c(5, 10, 15, 20)
#' # Get multiplier for count of 10
#' calculate_proportional_multiplier(10, counts, 0.5, 1.0)
#' # Returns: 0.67 (10 is 1/3 of the way from 5 to 20)
#'
#' @family utility functions
#' @export
calculate_proportional_multiplier <- function(count, all_counts, min_multiplier = 0.5, max_multiplier = 1.0) {
  # Find the minimum and maximum values in the count distribution
  # These define the range for our linear scaling
  min_count <- min(all_counts, na.rm = TRUE)
  max_count <- max(all_counts, na.rm = TRUE)

  # Handle edge case with single layer or all counts are identical
  # Without this check, we would get division by zero in the scaling formula
  if (min_count == max_count) {
    return(max_multiplier) # No discount/amplification if only one layer or counts are uniform
  }

  # Linear scaling: maps min_count to min_multiplier and max_count to max_multiplier
  # Formula: min_multiplier + (count - min_count) * (range_multiplier / range_count)
  multiplier <- min_multiplier + (count - min_count) * (max_multiplier - min_multiplier) / (max_count - min_count)

  return(multiplier)
}

#' Calculate the Gini-Simpson diversity index (balance score)
#'
#' @description
#' The Gini-Simpson index measures the diversity of values in a distribution.
#' It is calculated as 1 - Gini coefficient, where higher values indicate
#' more balanced (less concentrated) distributions. In the CEnTR-IMPACT framework,
#' higher balance scores indicate more equitable development across domains.
#'
#' @param category_values Numeric vector. Values to calculate diversity for.
#'
#' @return Character. Formatted balance score as a string with 2 decimal places.
#'
#' @details The function:
#'   \enumerate{
#'     \item Calculates the Gini coefficient using \code{DescTools::Gini()}
#'     \item Subtracts from 1 to get the Simpson diversity index
#'     \item Formats to 2 decimal places as a character string
#'   }
#'
#'   The Gini coefficient measures inequality (0 = perfect equality, 1 = maximum inequality).
#'   By subtracting from 1, we get a "balance" measure where:
#'   \itemize{
#'     \item 0 = completely unbalanced (all values concentrated in one category)
#'     \item 1 = perfectly balanced (all categories have equal values)
#'   }
#'
#' @examples
#' # Perfectly balanced distribution
#' balanced <- c(25, 25, 25, 25)
#' calculate_gini(balanced) # Returns "1.00"
#'
#' # Unbalanced distribution
#' unbalanced <- c(90, 5, 3, 2)
#' calculate_gini(unbalanced) # Returns a lower value (e.g., "0.38")
#'
#' @seealso \code{\link[DescTools]{Gini}} for the underlying Gini coefficient calculation
#' @family statistical functions
#' @export
calculate_gini <- function(category_values) {
  # Calculate the Gini coefficient which measures inequality (concentration)
  # Returns a value between 0 (perfect equality) and 1 (perfect inequality)
  gini <- DescTools::Gini(category_values, na.rm = TRUE)

  # Ensure the result is a numeric value between 0 and 1
  gini <- max(0, min(1, gini, na.rm = TRUE))

  # Return 1 - Gini so that higher values = more balanced (better balance score)
  balance_score <- 1 - gini

  return(balance_score)
}


# ==============================================================================
# SECTION 3: NETWORK ANALYSIS FUNCTIONS
# ==============================================================================
# Specialized functions for network topology analysis and multi-layer
# network processing within the CEnTR-IMPACT framework.

#' Calculate dynamic discount for network topology
#'
#' Implements a dynamic discounting mechanism that adjusts influence measures based on
#' network topology characteristics including layer depth, density, and clustering.
#'
#' @description
#' The discount factor is calculated using the formula:
#' \deqn{\text{discount} = \alpha^L \times (1 - \beta \times D) \times (1 - \gamma \times C)}
#' where:
#' \itemize{
#'   \item \eqn{L} is the layer number (1 to n)
#'   \item \eqn{D} is the network density (0 to 1)
#'   \item \eqn{C} is the global clustering coefficient (0 to 1)
#'   \item \eqn{\alpha} is the base decay rate (default: 0.8)
#'   \item \eqn{\beta} is the density adjustment factor (default: 0.5)
#'   \item \eqn{\gamma} is the clustering adjustment factor (default: 0.3)
#' }
#'
#' The discount factor decreases with:
#' \itemize{
#'   \item Increasing layer depth (exponentially, controlled by \code{alpha})
#'   \item Higher network density (linearly, controlled by \code{beta})
#'   \item Higher clustering coefficient (linearly, controlled by \code{gamma})
#' }
#'
#' @param layer Integer. The layer number in the network cascade (1 to n).
#' @param graph An igraph object. The network graph to analyze.
#' @param alpha Numeric. Base decay rate (0-1). Higher values mean faster decay with layer depth.
#' @param beta Numeric. Density adjustment factor (0-1). Higher values increase the penalty for dense networks.
#' @param gamma Numeric. Clustering adjustment factor (0-1). Higher values increase the penalty for clustered networks.
#'
#' @return A numeric value in \[0,1\] representing the calculated discount factor.
#'
#' @references
#' Add relevant citation to your methodology paper here
#'
#' @examples
#' \dontrun{
#' library(igraph)
#' # Create a random graph with 100 nodes and 10% connection probability
#' g <- sample_gnp(n = 100, p = 0.1)
#' # Calculate discount for layer 2
#' discount <- calculate_dynamic_discount_topology(
#'   layer = 2,
#'   graph = g,
#'   alpha = 0.8, # Standard decay rate
#'   beta = 0.5, # Moderate density penalty
#'   gamma = 0.3 # Mild clustering penalty
#' )
#' print(discount)
#' }
#'
#' @seealso
#'   \code{\link[igraph]{edge_density}} for network density calculation
#'   \code{\link[igraph]{transitivity}} for clustering coefficient calculation
#'
#' @importFrom igraph edge_density transitivity
#' @importFrom stats complete.cases
#'
#' @export
#'
calculate_dynamic_discount_topology <- function(
    layer, graph, alpha = 0.8, beta = 0.5, gamma = 0.3) {
  # Calculate network density (ratio of actual edges to possible edges)
  # Range: 0 (no edges) to 1 (complete graph)
  density <- edge_density(graph)

  # Calculate global clustering coefficient (transitivity)
  # Measures the tendency of nodes to cluster together
  # Range: 0 (no clustering) to 1 (perfect clustering)
  clustering_coefficient <- transitivity(graph, type = "global")

  # Calculate the discount using the combined formula
  # alpha^layer: Exponential decay with layer depth
  # (1 - beta * density): Density adjustment (denser networks get less discount)
  # (1 - gamma * clustering_coefficient): Clustering adjustment (more clustered networks get less discount)
  discount <- alpha^layer *
    (1 - beta * density) *
    (1 - gamma * clustering_coefficient)

  return(discount)
}
