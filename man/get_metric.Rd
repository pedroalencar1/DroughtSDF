\name{get_metric}
\alias{get_metric}
\title{Assess parameterisation of the probability distribution functions to the data. More in details}
\usage{
get_metric(param, series)
}

\description{
*Assess probability distribution parameters
}

\arguments{
\item{param}{a list with the parameters. Should use the output from \code{get_parameters()} only}
\item{series}{a dataframe column or vector with values for the drought feature under analyse (severity or duration)}
}

\details{
4 metrics are used, the root of the mean squared error (RMSE), the Nash-Sutcliffe Efficiency (NSE), the Percentual of Bias (PBIAS) and the Kolmogorov-Smirnov Distance (KS_dist)

For more info, Alencar et al., 2021 (Entropy-Based Method for Temporal Downscaling of Precipitation to Improve Sediment Delivery Ratio Assessment)
}

\examples{
get_metric(param, series)
}
