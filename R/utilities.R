#colChoose <- function(colScheme, nfactors, reverse=FALSE)
#{
#  if(colScheme=='gray'){cols <- gray((nfactors-1):0/nfactors)}
#  if(colScheme=='rainbow'){cols <- rainbow(nfactors)}
#  if(colScheme=='heat'){cols <- heat.colors(nfactors, alpha = 1)[nfactors:1]}
#  if(colScheme=='terrain'){cols <- terrain.colors(nfactors, alpha = 1)}
#  if(colScheme=='topo'){cols <- topo.colors(nfactors, alpha = 1)}
#  if(colScheme=='cm'){cols <- cm.colors(nfactors, alpha = 1)}
#  # some finessing to make sure contrast is sufficient when nFactors < 9
#  if(reverse) cols <- cols[nfactors:1]
#  return(cols)
#}

#' @title Clean Events Data
#' @description Removes columns with only NA values from event data and corresponding event times.
#'
#' This function iterates through each row of the `events` data and identifies columns containing only NA values. It then removes those columns from both the `events` and `event.times` data, while maintaining the corresponding row order.
#'
#' @param events A data frame containing event data.
#' @param event.times A data frame containing the timestamps associated with each event in the `events` data. The dimensions (number of rows and columns) of `event.times` should match those of `events`.
#'
#' @return A list containing the cleaned `events` data frame and the cleaned `event.times` data frame.
#'
#' @export
clean.events <- function(events, event.times)
{
  nc <- ncol(events)
  for(i in 1:nrow(events))
  {
    w <- !is.na(events[i,])
    temp.events <- events[i,w]
    temp.event.times <- event.times[i,w]
    nas <- rep(NA, (nc-length(temp.events)))
    events[i,] <- c(temp.events, nas)
    event.times[i,] <- c(temp.event.times, nas)
    rm(w, temp.events, temp.event.times, nas)
  }
  rm(nc)
  list(events=events, event.times=event.times)
}
