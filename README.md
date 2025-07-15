# centrimpact

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.XXXXXXX.svg)](https://doi.org/10.5281/zenodo.XXXXXXX)
<!-- badges: end -->

## Overview

The `centrimpact` R package provides a comprehensive toolkit for analyzing and visualizing community-engaged research metrics within the CEnTR-IMPACT framework. It offers specialized functions for network analysis, diversity calculations, proportional scaling, and creating publication-ready visualizations with consistent styling.

## Installation

You can install the development version of `centrimpact` from GitHub with:

```r
# Install devtools if not already installed
if (!require("devtools")) install.packages("devtools")

# Install centrimpact
devtools::install_github("CENTR-IMPACT/centrimpact")
```

## Key Features

- **Network Analysis**: Tools for analyzing community-engaged research networks
- **Diversity Metrics**: Calculate and visualize diversity across multiple dimensions
- **Proportional Scaling**: Functions for proportional analysis of research impact metrics
- **Visualization**: Customizable, publication-ready visualizations with consistent styling
- **Statistical Utilities**: Specialized statistical functions for research impact analysis

## Usage

### Basic Example

```r
library(centrimpact)

# Load example data
data(example_network)


# Analyze network dynamics
results <- analyze_dynamics(example_network)


# Visualize results
plot_network_impact(results)
```

### Advanced Analysis

```r
# Calculate balance scores across domains
balance_scores <- calculate_balance(example_data$domains)

# Generate impact report
impact_report <- generate_report(
  data = example_data,
  output_format = "html"
)
```

## Documentation

Full documentation is available at [https://centr-impact.github.io/centrimpact/](https://centr-impact.github.io/centrimpact/).

## Getting Help

If you encounter any issues or have questions, please:

1. Check the [documentation](https://centr-impact.github.io/centrimpact/)
2. Search the [issue tracker](https://github.com/CENTR-IMPACT/centrimpact/issues)
3. Open a [new issue](https://github.com/CENTR-IMPACT/centrimpact/issues/new/choose) if your question hasn't been asked before

## Contributing

We welcome contributions! Please see our [Contributing Guidelines](CONTRIBUTING.md) for more information on how to get involved.

## Citation

If you use `centrimpact` in your research, please cite it as:

> Price, J. (2023). centrimpact: Supporting the Analysis and Visualization of Community Engaged Research Metrics. R package version 0.1.0. https://github.com/CENTR-IMPACT/centrimpact

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- The CEnTR-IMPACT team
- Funding agencies and institutions supporting this work
