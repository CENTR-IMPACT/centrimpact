% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_helpers.R
\name{calculate_proportional_multiplier}
\alias{calculate_proportional_multiplier}
\title{Calculate a proportional linear multiplier based on count}
\usage{
calculate_proportional_multiplier(
  count,
  all_counts,
  min_multiplier = 0.5,
  max_multiplier = 1
)
}
\arguments{
\item{count}{Numeric. The raw count for the current item/layer.}

\item{all_counts}{Numeric vector. All raw counts to determine min/max for scaling.}

\item{min_multiplier}{Numeric. The minimum multiplier value (applied to the min count). Default is 0.5.}

\item{max_multiplier}{Numeric. The maximum multiplier value (applied to the max count). Default is 1.0.}
}
\value{
Numeric. A multiplier between min_multiplier and max_multiplier.
}
\description{
Maps counts in a vector linearly to a specified multiplier range.
The minimum count gets the min_multiplier, the maximum count gets the max_multiplier.
This function is useful for scaling visual elements (like point sizes, line widths,
or alpha values) proportionally to data values.
}
\details{
The function performs linear interpolation between min_multiplier and max_multiplier
based on where the count falls within the range of all_counts. If all counts are identical,
the function returns max_multiplier to avoid division by zero.
}
\examples{
counts <- c(5, 10, 15, 20)
# Get multiplier for count of 10
calculate_proportional_multiplier(10, counts, 0.5, 1.0)
# Returns: 0.67 (10 is 1/3 of the way from 5 to 20)

}
\seealso{
Other utility functions: 
\code{\link{normalize}()}
}
\concept{utility functions}
