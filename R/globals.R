# Global variable definitions
utils::globalVariables(c(
  # analyze_alignment
  "role", "alignment", "rating", "int_median", "researcher", "partner",

  # analyze_cascade
  "from", "to", "layer", "name", ".", "local_betweenness", "raw_knitting",
  "raw_bridging", "raw_channeling", "raw_reaching", "knitting", "bridging",
  "channeling", "reaching", "count", "raw_count_for_this_degree", "mu",
  "composite_rank", "layer_number", "layer_bridging", "layer_channeling",
  "layer_knitting", "layer_reaching", "layer_score",

  # analyze_dynamics
  "weight", "salience", "domain", "dimension", "dimension_value", "dimension_score", "domain_score"
))
