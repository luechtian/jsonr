% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{compare_json}
\alias{compare_json}
\title{Compare two JSON structures and return differences}
\usage{
compare_json(json1, json2, path = "")
}
\arguments{
\item{json1}{First JSON object (as an R list)}

\item{json2}{Second JSON object (as an R list)}

\item{path}{Current path in the JSON structure (used for recursion)}
}
\value{
A list of differences, each entry containing the path and the values in both structures
}
\description{
This function compares two JSON structures and returns a list of differences.
}
\keyword{internal}
