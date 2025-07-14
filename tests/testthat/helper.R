# Load the package under test
library(testthat)
library(centrimpact)

# Set up test environment
set.seed(123)  # For reproducibility

# Helper function to create test networks
create_test_network <- function() {
  data.frame(
    from = c(1,1,2,2,3,3,4,4,5,5,  # First community
             6,6,7,7,8,8,9,9,10,10, # Second community
             11,11,12,12,13,13,14,14,15,15, # Third community
             1,6,11,2,7,12), # Some inter-community connections
    to = c(2,3,3,4,4,5,5,1,1,2,  # First community (ring with some extra edges)
           7,8,8,9,9,10,10,6,6,7, # Second community (ring with some extra edges)
           12,13,13,14,14,15,15,11,11,12, # Third community (ring with some extra edges)
           6,11,1,7,12,2), # Inter-community connections
    layer = c(rep(1, 15), rep(2, 10), rep(3, 11)) # Distribute across layers
  )
}
