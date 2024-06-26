% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mylm.R
\name{mylm}
\alias{mylm}
\title{mylm}
\usage{
mylm(x, y, alpha = 0.05)
}
\arguments{
\item{x}{a numeric vector with the explanatory variable}

\item{y}{a numeric vector with the response variable}

\item{alpha}{the alpha level for the ci}
}
\value{
a scatter plot with LSRL and an invisible list with above named components
}
\description{
This function produces a plot of the data with fitted LSRL. There is also a list containing: beta estimates and confidence intervals, the design and response matrices, and sample variance, s^2.
}
\examples{
\dontrun{mylm(x = spruce$BHDiameter, y = spruce$Height)}
}
