\name{sort1}
\alias{sort1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
sort1
}
\description{
Helper function for \code{\link{sorter}}, doing within group sorting with \code{\link{sorter}} looping through groups (even if there is only one group) using \code{sort1}.
}
\usage{
sort1(id1, y1, times1, events1, event.times1, group1, 
ascending = TRUE, whichColumns = NULL, initFirst = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id1}{
The identification variable for one group. See also \code{id} for \code{\link{longCat}}.
}
  \item{y1}{
The longitudinal data for one group. See also \code{y} for \code{\link{longCat}}.
}
  \item{times1}{
The time data for one group. See also \code{times} for \code{\link{longCat}}.
}
  \item{events1}{
The events data for one group. See also \code{events} for \code{\link{longCat}}.
}
  \item{event.times1}{
The event times for one group. See also \code{event.times} for \code{\link{longCat}}.
}
  \item{group1}{
The group identification variable for one group. See also \code{group} for \code{\link{sorter}}.
}
  \item{ascending}{
See \code{ascending} for \code{\link{sorter}}.
}
  \item{whichColumns}{
See \code{whichColumns} for \code{\link{sorter}}.
}
  \item{initFirst}{
See \code{initFirst} for \code{\link{sorter}}.
}
}
\author{
Stephen Tueller
}
