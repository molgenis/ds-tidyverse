% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/groupByDS.R
\name{groupByDS}
\alias{groupByDS}
\title{Group by one or more variables}
\usage{
groupByDS(tidy_expr, df.name, .add, .drop)
}
\arguments{
\item{tidy_expr}{Diffused grouping expression.}

\item{df.name}{A data frame, data frame extension (e.g. a tibble), or a lazy data frame
(e.g. from dbplyr or dtplyr).}

\item{.add}{When FALSE, the default, \code{group_by()} will override existing groups. To add to
the existing groups, use .add = TRUE.}

\item{.drop}{Drop groups formed by factor levels that don't appear in the data? The default is
TRUE except when df.name has been previously grouped with .drop = FALSE.}
}
\value{
A grouped data frame with class grouped_df, unless the combination of \code{tidy_expr}
and \code{.add} yields a empty set of grouping columns, in which case a tibble will be returned.
}
\description{
DataSHIELD implentation of \code{dplyr::group_by}.
}
