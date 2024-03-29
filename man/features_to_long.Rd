% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/features_to_long.R
\name{features_to_long}
\alias{features_to_long}
\title{features_to_long}
\usage{
features_to_long(df, features)
}
\arguments{
\item{df}{data.frame}

\item{features}{vector containing column names storing features,}
}
\description{
Convenience wrapper function for \link[tidyr]{pivot_longer} to to pivot a data.frame with one column per feature into its long form.
}
