% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sorter.R
\name{sort1}
\alias{sort1}
\title{Helper function for \code{\link{sorter}}, doing within group sorting}
\usage{
sort1(
  id1,
  y1,
  times1,
  events1,
  event.times1,
  group1,
  ascending = TRUE,
  whichColumns = NULL,
  initFirst = FALSE
)
}
\arguments{
\item{id1}{The identification variable for one group, analogous to \code{id} in \code{\link{longCat}}.}

\item{y1}{The longitudinal data for one group, analogous to \code{y} in \code{\link{longCat}}.}

\item{times1}{The time data for one group, analogous to \code{times} in \code{\link{longCat}}.}

\item{events1}{The events data for one group, analogous to \code{events} in \code{\link{longCat}}.}

\item{event.times1}{The event times for one group, analogous to \code{event.times} in \code{\link{longCat}}.}

\item{group1}{The group identification variable for one group, analogous to \code{group} in \code{\link{sorter}}.}

\item{ascending}{Logical, indicating if sorting should be in ascending order. This parameter is consistent with the \code{ascending} option in \code{\link{sorter}}.}

\item{whichColumns}{Specifies which columns of \code{y1} should be considered in the sorting process, consistent with the \code{whichColumns} option in \code{\link{sorter}}.}

\item{initFirst}{Logical, indicating whether to initialize sorting with the first column, consistent with the \code{initFirst} option in \code{\link{sorter}}.}
}
\description{
This function is a helper for \code{\link{sorter}}, designed to handle sorting within individual groups as \code{\link{sorter}} loops through groups (even if there is only one group) using \code{sort1}.
}
\author{
Stephen J. Tueller
}
