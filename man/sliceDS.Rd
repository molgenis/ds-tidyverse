% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sliceDS.R
\name{sliceDS}
\alias{sliceDS}
\title{Subset rows using their positions}
\usage{
sliceDS(expr, .data, .by, .preserve)
}
\arguments{
\item{expr}{Provide either positive values to keep, or negative values to drop. The values
provided must be either all positive or all negative. Indices beyond the number of rows in the
input are silently ignored.}

\item{.data}{A data frame, data frame extension (e.g. a tibble), or a lazy data frame
(e.g. from dbplyr or dtplyr).}

\item{.by}{Optionally, a selection of columns to group by for just this operation, functioning as
an alternative to \code{group_by}.}

\item{.preserve}{Relevant when the .data input is grouped. If .preserve = FALSE (the default),
the grouping structure is recalculated based on the resulting data, otherwise the grouping is
kept as is.}
}
\value{
the object specified by the \code{newobj} argument of \code{ds.slice} which is written
to the serverside.
}
\description{
This function is similar to R function \code{dplyr::slice}.
}
