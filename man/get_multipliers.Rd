\name{get_multipliers}
\alias{get_multipliers}
\title{Calculate the lagrange multipliers from the PME}
\usage{
get_multipliers(list_param)
}

\description{
*Calculate the lagrange multipliers from the PME
}

\arguments{
\item{df}{a list of parameters of calibrated distributions, preferably use the output of function \code{get_param}}
}

\details{
The estimation of the Lagrange multipliers follows the Principle of Maximum Entropy (Singh, 1998 - Entropy-based parameter estimation in hydrology)
}

\examples{
param <- get_spi(df, k = 3) \%>\% get_data() \%>\% get_parameters()

get_multipliers(param)
}
