colChoose <- function(colScheme, nfactors, reverse=FALSE)
{
  if(colScheme=='gray'){cols <- gray((nfactors-1):0/nfactors)}
  if(colScheme=='rainbow'){cols <- rainbow(nfactors)}
  if(colScheme=='heat'){cols <- heat.colors(nfactors, alpha = 1)[nfactors:1]}
  if(colScheme=='terrain'){cols <- terrain.colors(nfactors, alpha = 1)}
  if(colScheme=='topo'){cols <- topo.colors(nfactors, alpha = 1)}
  if(colScheme=='cm'){cols <- cm.colors(nfactors, alpha = 1)}
  # some finessing to make sure contrast is sufficient when nFactors < 9
  if(reverse) cols <- cols[nfactors:1]
  return(cols)
}


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
