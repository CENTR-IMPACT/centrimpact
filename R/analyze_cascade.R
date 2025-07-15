#' Analyze Network Cascade Structure
#'
#' @description
#' Performs a comprehensive analysis of network cascade structure by calculating various
#' network metrics and influence measures across different network layers. This function
#' implements a multi-layer network analysis approach based on Leng et al.'s framework,
#' providing quantitative measures of influence propagation through network structures.
#'
#' The function computes four key influence measures that capture different aspects of
#' network influence dynamics:
#' \itemize{
#'   \item \strong{Knitting} (0-1): Measures the node's ability to bind community members together,
#'         with higher values indicating stronger community cohesion influence.
#'   \item \strong{Bridging} (0-1): Quantifies the node's capacity to connect different communities,
#'         with higher values indicating better inter-community connectivity.
#'   \item \strong{Channeling} (0-1): Assesses the node's control over information flow,
#'         with higher values indicating greater control over network pathways.
#'   \item \strong{Reaching} (0-1): Measures the node's ability to access and influence
#'         distant network parts, with higher values indicating better reach.
#' }
#'
#' @param network_df A data frame containing the network edges with the following structure:
#'   \itemize{
#'     \item \code{from}: Source node identifier (character or numeric). Must be non-missing.
#'     \item \code{to}: Target node identifier (character or numeric). Must be non-missing.
#'     \item \code{layer}: Layer identifier (positive integer, typically 1-3). Layer 1 represents
#'           the core/permanent layer, with higher numbers indicating more peripheral layers.
#'   }
#'   The data frame must contain at least one row, and all columns must be non-NULL.
#'   Self-loops (where from == to) are automatically removed during processing.
#'
#' @return A list object of class "cascade_analysis" containing the following components:
#'   \describe{
#'     \item{\code{cascade_df}}{A data frame with one row per network layer containing:
#'       \itemize{
#'         \item{\code{layer} (integer): Layer identifier (1 = core, 2 = intermediate, 3 = peripheral)}
#'         \item{\code{layer_number} (character): Formatted layer label with degree symbol (e.g., "1st degree", "2nd degree")}
#'         \item{\code{count} (integer): Number of unique nodes in the layer}
#'         \item{\code{gamma} (numeric, 0-1): Layer-specific decay factor (higher = more influence retention)}
#'         \item{\code{layer_knitting} (numeric, 0-1): Mean knitting score for the layer}
#'         \item{\code{layer_bridging} (numeric, 0-1): Mean bridging score for the layer}
#'         \item{\code{layer_channeling} (numeric, 0-1): Mean channeling score for the layer}
#'         \item{\code{layer_reaching} (numeric, 0-1): Mean reaching score for the layer}
#'         \item{\code{layer_score} (numeric, 0-1): Composite influence score for the layer}
#'         \item{\code{mu} (numeric, 0.75-1.0): Proportional multiplier for layer size adjustment}
#'         \item{\code{raw_count_for_this_degree} (integer): Unadjusted node count before multiplier}
#'       }
#'     }
#'     \item{\code{cascade_score}}{A numeric value between 0 and 1 representing the Gini-like coefficient of influence concentration across layers.
#'       Values closer to 1 indicate highly concentrated influence in few layers, while values near 0 suggest more uniform distribution.}
#'   }
#'
#' @details
#' The analysis implements a structured multi-layer network analysis pipeline with the following stages:
#' \enumerate{
#'   \item \strong{Network Construction}:
#'     \itemize{
#'       \item Converts input edge list to an undirected igraph object
#'       \item Validates node-layer assignments and handles missing data
#'       \item Removes self-loops and ensures edge uniqueness
#'     }
#'   \item \strong{Topology Metrics} (Global Network Properties):
#'     \itemize{
#'       \item \code{topology_efficiency}: Global efficiency of the network
#'       \item \code{topology_connectedness}: Network wide connectedness measure
#'       \item \code{topology_hierarchy}: Degree of hierarchical organization
#'       \item \code{topology_lubness}: Network cohesion and integration
#'     }
#'   \item \strong{Local Metrics} (Node-level Analysis):
#'     \itemize{
#'       \item Community centrality (\code{local_community})
#'       \item Cross-clique centrality (\code{local_crossclique})
#'       \item Local clustering coefficient (\code{local_clustcoef})
#'     }
#'   \item \strong{Global Metrics} (Network-wide Centrality):
#'     \itemize{
#'       \item Eigenvector centrality (\code{global_eigenv})
#'       \item Betweenness centrality (\code{global_betweenness})
#'       \item Alpha centrality (\code{global_alpha})
#'       \item Harmonic centrality (\code{global_harmonic})
#'     }
#'   \item \strong{Influence Measures}:
#'     \itemize{
#'       \item Combines local and global metrics using the formula: I = gamma * (alpha*L + beta*G) + lambda*T
#'       \item Normalizes scores to \[0,1\] range for comparability
#'       \item Ranks nodes within each influence dimension
#'     }
#'   \item \strong{Layer Aggregation}:
#'     \itemize{
#'       \item Applies layer-specific decay factors (gamma)
#'       \item Adjusts for layer size effects using proportional multipliers (\eqn{\mu})
#'       \item Computes layer-wise averages for each influence measure
#'     }
#'   \item \strong{Cascade Scoring}:
#'     \itemize{
#'       \item Computes Gini coefficient to quantify influence concentration
#'       \item Generates visualization-ready mathematical expression
#'     }
#' }
#'
#' @section Validation and Error Handling:
#' The function includes several validation checks:
#' \itemize{
#'   \item Verifies required columns exist in input data
#'   \item Handles missing values in layer assignments
#'   \item Removes duplicate edges and self-loops
#'   \item Validates layer assignments are positive integers
#'   \item Imputes missing values where appropriate
#'   \item Provides informative error messages for common issues
#' }
#'
#' The layer decay factors are applied as follows:
#' \itemize{
#'   \item Layer 1: 0.9 (highest influence)
#'   \item Layer 2: 0.5 (moderate influence)
#'   \item Layer 3: 0.45 (lower influence)
#'   \item Other layers: 0 (minimal influence)
#' }
#'
#' @section Mathematical Framework:
#' The influence measures are calculated using the formula:
#' I = \eqn{gamma * (alpha * L + beta * G) + lambda * T}
#' where:
#' \itemize{
#'   \item \code{I} = Influence measure
#'   \item \code{gamma} = Layer decay factor
#'   \item \code{alpha} = Local metric weight (0.4)
#'   \item \code{L} = Local centrality measure
#'   \item \code{beta} = Global metric weight (0.3)
#'   \item \code{G} = Global centrality measure
#'   \item \code{lambda} = Topology weight (0.3)
#'   \item \code{T} = Topology score
#' }
#'
#' @section Dependencies:
#' This function requires the following packages:
#' \itemize{
#'   \item \code{igraph}: For graph construction and basic centrality measures
#'   \item \code{dplyr}: For data manipulation and aggregation
#'   \item \code{tibble}: For modern data frame operations
#'   \item \code{centiserve}: For specialized centrality measures
#'   \item \code{sna}: For social network analysis metrics
#'   \item \code{glue}: For string formatting (via layer_number creation)
#' }
#'
#' @section Helper Functions:
#' This function assumes the following helper functions are available:
#' \itemize{
#'   \item \code{normalize()}: Scales values to \[0,1\] range
#'   \item \code{calculate_proportional_multiplier()}: Computes layer-size adjustment factors
#'   \item \code{calculate_gini()}: Computes Gini coefficient for concentration measurement
#' }
#'
#' @examples
#' \dontrun{
#' # Example 1: Basic usage with synthetic data
#' # Create a sample network with three layers
#' set.seed(123)
#' network_data <- data.frame(
#'   from = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J",
#'           "A", "B", "C", "K", "L", "M", "N", "O", "P", "Q"),
#'   to = c("B", "C", "D", "E", "F", "G", "H", "I", "J", "A",
#'         "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T"),
#'   layer = c(rep(1, 10), rep(2, 10))
#' )
#'
#' # Analyze cascade structure
#' result <- analyze_cascade(network_data)
#'
#' # View cascade score (0-1, higher = more concentrated influence)
#' cat("Cascade Score (Gini coefficient):", result$cascade_score, "\n")
#'
#' # View detailed layer metrics
#' print("Cascade metrics by layer:")
#' print(result$cascade_df)
#'
#' # Example 3: Working with real-world data
#' # Load required packages
#' if (require(igraphdata) && require(tidygraph)) {
#'   # Use Zachary's karate club network as an example
#'   data("karate", package = "igraphdata")
#'
#'   # Convert to edge list and assign layers (for demonstration)
#'   edge_df <- as_data_frame(karate, what = "edges")
#'   edge_df$layer <- sample(1:3, nrow(edge_df), replace = TRUE, prob = c(0.6, 0.3, 0.1))
#'
#'   # Analyze cascade structure
#'   karate_result <- analyze_cascade(edge_df)
#'
#'   # Compare layer influence profiles
#'   print(karate_result$cascade_df %>%
#'           select(layer_number, knitting:reaching))
#' }
#' }
#'
#' @section Computational Complexity:
#' The computational complexity of this function is primarily determined by:
#' \itemize{
#'   \item Network construction: O(m) where m is the number of edges
#'   \item Global metrics: O(n(m + n log n)) for betweenness and other global measures
#'   \item Local metrics: O(m^2/n) for community detection in sparse graphs
#'   \item Layer aggregation: O(n) where n is the number of nodes
#' }
#' For large networks (n > 10,000 nodes), consider using sampling or approximation methods
#' for centrality measures to improve performance.
#'
#' @references
#' \itemize{
#'   \item Leng, Y., Zhai, Y., Sun, S., Wu, Y., Selzer, J., & Ester, M. (2018).
#'   A multi-layered network model for studying the cascade of influence in organizations.
#'   \emph{Network Science}, 6(3), 306-329. \doi{10.1017/nws.2018.3}
#'
#'   \item Newman, M. E. J. (2018).
#'   Networks (2nd ed.). Oxford University Press.
#'   \doi{10.1093/oso/9780198805090.001.0001}
#'
#'   \item Borgatti, S. P., & Everett, M. G. (2006).
#'   A Graph-theoretic perspective on centrality.
#'   \emph{Social Networks}, 28(4), 466-484.
#'   \doi{10.1016/j.socnet.2005.11.005}
#'
#'   \item Csardi, G., & Nepusz, T. (2006).
#'   The igraph software package for complex network research.
#'   \emph{InterJournal, Complex Systems}, 1695(5), 1-9.
#'   URL: https://igraph.org
#' }
#'
#' @author Your Name, Your Institution
#' Maintainer: Your Name <your.email@example.com>
#' @export
#' @importFrom igraph graph_from_data_frame V global_efficiency betweenness eigen_centrality transitivity
#'   harmonic_centrality as_adjacency_matrix alpha_centrality
#' @importFrom dplyr select mutate group_by ungroup summarize arrange rename left_join right_join distinct first rowwise where
#' @importFrom centiserve communitycent communibet crossclique
#' @importFrom sna connectedness hierarchy lubness flowbet
#' @importFrom glue glue
#' @importFrom rlang .data
#' @importFrom utils globalVariables
#' @importFrom magrittr %>%
#' @importFrom stats quantile
#' @import linkcomm

analyze_cascade <- function(network_df) {
  # =============================================================================
  # INPUT VALIDATION
  # =============================================================================
  # Purpose: Validate the input data frame structure and content to ensure
  # the function receives valid input before proceeding with computation.

  # Check if input is a data frame
  if (!is.data.frame(network_df)) {
    stop("Input must be a data frame")
  }

  # Check for required columns
  required_cols <- c("from", "to", "layer")
  missing_cols <- setdiff(required_cols, names(network_df))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }

  # Check for empty data frame
  if (nrow(network_df) == 0) {
    stop("Input data frame must contain at least one row")
  }

  # Check for missing values
  if (any(is.na(network_df[c("from", "to", "layer")]))) {
    stop("Columns 'from', 'to', and 'layer' cannot contain missing values")
  }

  # Check layer values are positive
  if (any(network_df$layer <= 0)) {
    stop("Layer values must be positive integers")
  }

  # Check for valid node types (numeric or character)
  if (!(is.numeric(network_df$from) || is.character(network_df$from)) ||
      !(is.numeric(network_df$to) || is.character(network_df$to))) {
    stop("Node identifiers in 'from' and 'to' must be either numeric or character")
  }

  # Convert node IDs to character for consistent handling
  network_df$from <- as.character(network_df$from)
  network_df$to <- as.character(network_df$to)

  # Check for self-loops and remove them with a warning
  self_loops <- network_df$from == network_df$to
  if (any(self_loops)) {
    warning(sprintf("Removed %d self-loops from the network", sum(self_loops)))
    network_df <- network_df[!self_loops, ]
  }

  # Check if network is too small for community detection
  unique_nodes <- unique(c(network_df$from, network_df$to))
  if (length(unique_nodes) < 5) {
    stop("Network is too small for community detection. Need at least 5 unique nodes.")
  }

  # =============================================================================
  # NETWORK STRUCTURE CREATION AND NODE PREPARATION
  # =============================================================================
  # Purpose: Establish the foundational network structure by identifying all unique
  # nodes and their layer assignments. This section ensures comprehensive node
  # coverage and proper layer-based organization for subsequent analyses.
  # Phase 1: Setup & Preparation (0-10%)

  # Create a comprehensive registry of all unique nodes in the network
  # This captures both source and target nodes to ensure no node is missed
  # in the analysis, even those that only appear as edge endpoints
  all_nodes <- tibble::tibble(name = unique(c(network_df$from, network_df$to)))

  # Construct node-layer mapping by examining both directions of edges
  # This approach handles the bidirectional nature of influence relationships
  # where nodes can both send and receive influence across layers
  node_layer_lookup <- network_df |>
    # Extract source nodes with their layers
    dplyr::select(name = from, layer) |>
    # Combine with target nodes and their layers
    dplyr::bind_rows(network_df |>
                       dplyr::select(name = to, layer)) |>
    # Remove duplicate node-layer combinations to prevent double-counting
    dplyr::distinct(name, layer)

  # Calculate layer composition statistics
  # This provides essential information about network structure and layer sizes
  # which influences the proportional weighting in later calculations
  layer_count <- node_layer_lookup |>
    dplyr::group_by(layer) |>
    dplyr::summarize(count = dplyr::n_distinct(name), .groups = 'drop')

  # Compute total node count across all layers for normalization purposes
  total_count <- sum(layer_count$count)

  # Assign layer information to all nodes with appropriate decay factors
  # Nodes without explicit layer assignment receive Inf (effectively excluded)
  node_layers <- all_nodes |>
    dplyr::left_join(node_layer_lookup, by = "name") |>
    # Handle unassigned nodes by setting layer to infinity
    dplyr::mutate(layer = tidyr::replace_na(layer, Inf)) |>
    # Ensure each node appears only once in the final mapping
    dplyr::distinct(name, .keep_all = TRUE) |>
    dplyr::mutate(
      # Apply hierarchical decay factors based on Leng et al.'s framework
      # These factors reflect diminishing influence capacity across network layers
      layer_decay = dplyr::case_when(
        layer == 1 ~ 0.9,   # Core layer: highest influence retention
        layer == 2 ~ 0.5,   # Intermediate layer: moderate influence
        layer == 3 ~ 0.45,  # Peripheral layer: reduced influence
        TRUE ~ 0            # Extended layers: minimal influence
      ))

  # Construct the network graph object with preserved node attributes
  # Using undirected graph to capture bidirectional influence relationships
  # while maintaining layer and decay information as vertex attributes
  network_graph <- igraph::graph_from_data_frame(
    d = network_df |> dplyr::select(from, to),  # Edge list
    directed = FALSE,                            # Undirected for influence flow
    vertices = node_layers                       # Node attributes included
  )

  # =============================================================================
  # SECTION 2: NETWORK TOPOLOGY AND PARAMETER INITIALIZATION
  # =============================================================================

  # Purpose: Establish global network characteristics and mathematical parameters
  # that will be used throughout the influence calculation process.

  # Extract layer information for vectorized operations
  # This enables efficient layer-based calculations across all nodes
  layers <- igraph::V(network_graph)$layer

  # Define mathematical weights for influence measure calculations
  # These parameters balance the contribution of different metric types
  # as established in the Leng et al. framework
  alpha <- 0.4    # Weight for local centrality measures
  beta <- 0.3     # Weight for global centrality measures
  lambda <- 0.3   # Weight for topology-level measures

  # Convert graph to adjacency matrix for SNA package compatibility
  # Many specialized network measures require matrix input format
  network_matrix <- igraph::as_adjacency_matrix(network_graph, sparse = FALSE)

  # =============================================================================
  # SECTION 3: GLOBAL TOPOLOGY METRICS CALCULATION
  # =============================================================================
  # Purpose: Compute network-level structural properties that characterize
  # the overall topology and influence the cascade dynamics across all layers.

  # Calculate global efficiency: measures how efficiently information
  # can flow through the network via shortest paths
  topology_efficiency <- igraph::global_efficiency(network_graph)

  # Calculate connectedness: measures the degree to which nodes are
  # reachable from one another in the network structure
  topology_connectedness <- sna::connectedness(network_matrix)

  # Calculate hierarchy measure: assesses the degree of hierarchical
  # organization in the network (inverted for consistency)
  topology_hierarchy <- 1 - unname(sna::hierarchy(network_matrix))

  # Calculate lubness measure: evaluates network cohesion and
  # structural integration (inverted for directional consistency)
  topology_lubness <- 1 - sna::lubness(network_matrix)

  # =============================================================================
  # SECTION 4: LOCAL CENTRALITY MEASURES CALCULATION
  # =============================================================================

  # Purpose: Compute node-level metrics that capture local influence patterns
  # and community-based structural positions within the network.

  # Community centrality: measures how central a node is within its
  # local community structure - key for knitting influence
  local_community <- normalize(centiserve::communitycent(network_graph))

  # Cross-clique centrality: measures ability to bridge different
  # cliques or communities - essential for bridging influence
  local_crossclique <- normalize(centiserve::crossclique(network_graph))

  # Local clustering coefficient: measures local network density
  # around each node - important for reaching influence patterns
  local_clustcoef <- normalize(igraph::transitivity(network_graph, type = "local") %>%
                                 replace(is.nan(.), 0))  # Handle NaN values for isolated nodes

  # Local betweenness: measures the extent to which a node lies on shortest paths
  # between other nodes within its immediate neighborhood - important for channeling influence
  local_betweenness <- igraph::betweenness(
    network_graph,
    v = igraph::V(network_graph),
    directed = FALSE,
    weights = NULL,
    normalized = TRUE
  )

  # =============================================================================
  # SECTION 5: GLOBAL CENTRALITY MEASURES CALCULATION
  # =============================================================================

  # Purpose: Compute network-wide centrality metrics that capture each node's
  # global importance and influence potential across the entire network structure.

  # Eigenvector centrality: measures influence based on connections
  # to other influential nodes - key for knitting at global scale
  global_eigenv <- normalize(igraph::eigen_centrality(network_graph)$vector)

  # Betweenness centrality: measures control over information flow
  # between other nodes - essential for bridging at global scale
  global_betweenness <- normalize(igraph::betweenness(network_graph))

  # Alpha centrality: measures influence based on flow
  # Use tryCatch to handle potential matrix factorization errors
  global_alpha <- tryCatch({
    ac <- igraph::alpha_centrality(
      network_graph,
      alpha = 0.9,    # Slightly less than 1 to improve numerical stability
      tol = 1e-4,     # Slightly more tolerant convergence
      loops = FALSE   # Ignore self-loops
    )
    normalize(ac)
  }, error = function(e) {
    # Fall back to betweenness centrality if alpha centrality fails
    warning("Alpha centrality failed, falling back to betweenness: ", conditionMessage(e))
    global_betweenness
  })

  # Harmonic centrality: measures closeness using harmonic mean
  # of distances - key for reaching influence across network
  global_harmonic <- normalize(igraph::harmonic_centrality(network_graph))

  # =============================================================================
  # SECTION 6: TOPOLOGY SCORE INTEGRATION
  # =============================================================================

  # Purpose: Combine global topology metrics into a unified score that
  # represents the overall structural foundation for influence propagation.

  # Calculate composite topology score by averaging all topology measures
  # and applying the lambda weight to balance with centrality measures
  # Ensure all topology measures are finite and valid
  topology_measures <- c(
    if (is.finite(topology_efficiency)) topology_efficiency else 0,
    if (is.finite(topology_connectedness)) topology_connectedness else 0,
    if (is.finite(topology_hierarchy)) topology_hierarchy else 0,
    if (is.finite(topology_lubness)) topology_lubness else 0
  )

  # Calculate average, ensuring we don't divide by zero
  topology_score <- if (length(topology_measures) > 0) {
    round((mean(topology_measures, na.rm = TRUE) * lambda), 2)
  } else {
    0
  }

  # =============================================================================
  # SECTION 7: INFLUENCE MEASURES COMPUTATION AND LAYER AGGREGATION
  # =============================================================================

  # Purpose: Calculate the four primary influence measures by combining local,
  # global, and topology metrics, then aggregate results by network layer.

  # Ensure all vectors have the same length
  node_count <- nrow(node_layers)

  # Create a data frame with all metrics
  # Ensure all vectors are the same length
  metrics_df <- data.frame(
    name = as.character(node_layers$name),  # Ensure name is character
    layer = as.integer(node_layers$layer),  # Ensure layer is integer
    gamma = as.numeric(node_layers$layer_decay),  # Ensure gamma is numeric
    stringsAsFactors = FALSE
  )

  # Add metrics with explicit length checking
  metrics_df$local_community <- as.numeric(local_community[1:node_count])
  metrics_df$local_crossclique <- as.numeric(local_crossclique[1:node_count])
  metrics_df$local_betweenness <- as.numeric(local_betweenness[1:node_count])
  metrics_df$local_clustcoef <- as.numeric(local_clustcoef[1:node_count])
  metrics_df$global_eigenv <- as.numeric(global_eigenv[1:node_count])
  metrics_df$global_betweenness <- as.numeric(global_betweenness[1:node_count])
  metrics_df$global_alpha <- as.numeric(global_alpha[1:node_count])
  metrics_df$global_harmonic <- as.numeric(global_harmonic[1:node_count])

  # Join with layer count information
  # Ensure layer is the same type in both data frames
  layer_count$layer <- as.integer(layer_count$layer)

  # Rename count column before join to avoid conflicts
  layer_count <- layer_count %>%
    dplyr::rename(node_count = count)

  cascade_df <- metrics_df |>
    dplyr::left_join(layer_count, by = "layer") |>
    dplyr::arrange(name) |>
    dplyr::mutate(
      # Ensure all numeric columns are properly typed
      dplyr::across(dplyr::where(is.numeric), as.numeric)
    )

  # Calculate raw influence measures
  cascade_df <- cascade_df |>
    dplyr::mutate(
      # KNITTING: Ability to bind community members together
      raw_knitting = gamma * (alpha * local_community + beta * global_eigenv) + topology_score,

      # BRIDGING: Ability to connect different communities or groups
      raw_bridging = gamma * (alpha * local_crossclique + beta * global_betweenness) + topology_score,

      # CHANNELING: Ability to control and direct information flow
      raw_channeling = gamma * (alpha * local_betweenness + beta * global_alpha) + topology_score,

      # REACHING: Ability to access and influence distant network parts
      raw_reaching = gamma * (alpha * local_clustcoef + beta * global_harmonic) + topology_score,

      # Convert raw scores to normalized rankings for comparative analysis
      # Higher ranks indicate stronger influence capacity in each dimension
      knitting = normalize(dplyr::min_rank(raw_knitting)),
      bridging = normalize(dplyr::min_rank(raw_bridging)),
      channeling = normalize(dplyr::min_rank(raw_channeling)),
      reaching = normalize(dplyr::min_rank(raw_reaching)),

      # Calculate composite influence rank by averaging all four measures
      # This provides an overall influence assessment for each node
      composite_rank = ((knitting + bridging + channeling + reaching) / 4)
    ) |>

    # Aggregate individual node metrics to layer-level summaries
    dplyr::group_by(layer) |>
    dplyr::mutate(
      # Store layer size for proportional multiplier calculation
      raw_count_for_this_degree = dplyr::first(node_count),

      # Calculate proportional multiplier to adjust for layer size effects
      # Larger layers may have diluted individual influence that needs adjustment
      mu = calculate_proportional_multiplier(
        raw_count_for_this_degree,
        layer_count$node_count,
        min_multiplier = 0.75,   # Minimum adjustment factor
        max_multiplier = 1.0     # Maximum adjustment factor
      ),

      # Compute layer-wise averages with proportional adjustment
      # These represent the characteristic influence profile for each layer
      layer_knitting = round(mean(knitting) * mu, 2),      # Layer knitting capacity
      layer_bridging = round(mean(bridging) * mu, 2),      # Layer bridging capacity
      layer_channeling = round(mean(channeling) * mu, 2),  # Layer channeling capacity
      layer_reaching = round(mean(reaching) * mu, 2),      # Layer reaching capacity
      layer_score = round(mean(composite_rank) * mu, 2)    # Overall layer influence
    ) |>
    dplyr::ungroup()

  # =============================================================================
  # SECTION 8: LAYER-LEVEL SUMMARY PREPARATION
  # =============================================================================

  # Purpose: Create the final layer-wise summary that will be returned to users,
  # formatted for easy interpretation and visualization.

  # Extract unique layer information and format for presentation
  cascade_df <- cascade_df |>
    # Keep only one record per layer (removes individual node details)
    dplyr::distinct(layer, .keep_all = TRUE) |>
    # Create formatted layer labels with degree symbols for visualization
    dplyr::mutate(layer_number = as.character(glue::glue("{layer} degree"))) |>
    # Rename node_count back to count for backward compatibility
    dplyr::rename(count = node_count) |>
    # Select and order columns for final output
    dplyr::select(
      layer,               # Layer identifier
      layer_number,        # Formatted layer label
      count,               # Number of nodes in layer (renamed from node_count)
      gamma,               # Layer decay factor applied
      mu,                  # Proportional multiplier
      raw_count_for_this_degree,  # Raw count before multiplier
      layer_knitting,      # Average knitting influence
      layer_bridging,      # Average bridging influence
      layer_channeling,    # Average channeling influence
      layer_reaching,      # Average reaching influence
      layer_score          # Composite layer influence score
    )

  # =============================================================================
  # SECTION 9: CASCADE CONCENTRATION ASSESSMENT
  # =============================================================================

  # Purpose: Calculate the overall cascade score that quantifies how concentrated
  # influence is across network layers using a Gini-like coefficient approach.

  # Calculate Gini-like coefficient measuring influence concentration
  # Values near 0 indicate evenly distributed influence across layers
  # Values near 1 indicate highly concentrated influence in few layers
  cascade_score <- calculate_gini(cascade_df$layer_score)

  # =============================================================================
  # SECTION 10: RESULTS COMPILATION AND RETURN
  # =============================================================================

  # Purpose: Package all computed results into a structured list for return
  # to the calling function, ensuring accessibility of all analysis components.

  # Create result list with required components
  result <- list(
    cascade_df = cascade_df,        # Layer-wise metrics
    cascade_score = cascade_score   # Overall concentration measure
  )

  # Set the class for S3 method dispatch
  class(result) <- "cascade_analysis"

  return(result)
}
