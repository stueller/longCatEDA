#' Plotting of \code{longCat} Objects
#'
#' Function to plot \code{longCat} objects created by \code{\link{longCat}}.
#'
#' @param lc An object of class \code{longCat} created by \code{\link{longCat}}.
#' @param xlab A label for the x-axis. Default is "Days".
#' @param ylab A label for the y-axis. Default is \code{NULL} which is changed to "Each Line Represents a Participant" with the sample size appended (\code{lc$dim[1]}).
#' @param cols A numeric or character list of colors. To use internal color schemes, use \code{colScheme}.
#' @param colScheme Select a color scheme. See \code{\link{colChoose}} for available options.
#' @param reverse Color schemes are applied from the lowest to highest level of categorical data in \code{lc$y} or \code{lc$y.sorted}. Set \code{reverse=TRUE} to reverse this. Default is \code{FALSE}.
#' @param lwd Set the width of horizontal lines. Default is .5.
#' @param lcex Character expansion factor for the legend text. Default is 1.
#' @param llwd Set the width of lines in the legend. Default is 3.
#' @param legendBuffer Set proportion of the plot to retain for legends, must be in [0,1]. Default is .12.
#' @param groupBuffer Similar to legendBuffer, but for group labels on the left side of the plot. Default is 0.
#' @param groupRotation Rotation of the labels to reduce the needed size of \code{groupBuffer}. Default is 90 degrees.
#' @param gcex Character expansion factor for group labels. Default is 1.
#' @param seg.len Length of lines in the upper legend. Default is 1.
#' @param xlas Applied to the x-axis when \code{tLabels} are provided. See \code{las} in \code{\link{par}}.
#' @param xcex Applied to the x-axis when \code{tLabels} are provided. Default is 1.
#' @param ecex Used to size the points used to plot event points if \code{events} is not \code{NULL}. Default is .5.
#' @param event.col Color for plotting event indicators.
#' @param plot.events Logical - should events be plotted? Default is \code{TRUE}.
#' @param which.events Numeric vector - which events should be plotted.
#' @param n.events How many events should be plotted, e.g., \code{n.events=3} plots the first three events for each participant.
#' @param event.pch What plotting characters should be used. See \code{\link{points}}.
#' @param texclude A vector indicating the range of \code{times} and \code{event.times} to be plotted.
#' @param sort Logical - should \code{longCatPlot} sort the data on the fly using intelligent defaults? Default is \code{TRUE}.
#' @param which.state See \code{\link{alignTime}}.
#' @param nth.state See \code{\link{alignTime}}.
#' @param not.state See \code{\link{alignTime}}.
#' @param which.event See \code{\link{alignTime}}.
#' @param nth.event See \code{\link{alignTime}}.
#' @param not.event See \code{\link{alignTime}}.
#' @param ... Arguments to be passed to \code{\link{plot}}.
#' @examples
#' # Illustrate longCatPlot with the legend outside the plot
#' par(mfrow=c(1,1), bg='cornsilk3', mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
#' cols <- longCatPlot(
#'   longCat(example3),
#'   legendBuffer=0,
#'   main='Horizontal Line Plot')
#' legend(7.1, 100, legend=1:5, col=cols, lty=1, lwd=2)
#' par(bg='transparent', mar = c(5, 4, 4, 2) + 0.1, xpd=FALSE)
#'
#' ### visualizing multivariate data: 3 items at 4 time points
#' library(MASS)
#' Sigma <- matrix(.25, 12, 12)
#' diag(Sigma) <- 1
#' set.seed(9845)
#' mu <- rep(c(-.5, 0, .5), 4) + rnorm(12, 0, .25)
#' set.seed(539)
#' ymv <- apply(mvrnorm(n=100, mu=mu, Sigma = Sigma), 2, cut, breaks=c(-Inf, 0, Inf), labels=c(0,1))
#' apply(ymv, 2, table)
#' (items <- rep(1:3, 4))
#' (times <- sort(rep(1:4, 3)))
#' tLabels <- paste('Time', 1:4)
#' Labels <- paste('Item', 1:3)
#'
#' # plot time points within items
#' par(mfrow=c(2,2), bg='cornsilk3', mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
#'
#' item1  <- longCat(y=ymv[,items==1], tLabels=tLabels)
#' cols <- longCatPlot(item1, ylab='', main='Item 1', legendBuffer=0, xlab="", xlas=2)
#' legend(length(unique(times))+.1, nrow(ymv), legend=0:1, col=cols, lty=1, lwd=2, title='Response')
#'
#' item2 <- longCat(y=ymv[,items==2], tLabels=tLabels)
#' longCatPlot(item2, ylab='', main='Item 2', legendBuffer=0, xlab="", xlas=2)
#' legend(length(unique(times))+.1, nrow(ymv), legend=0:1, col=cols, lty=1, lwd=2, title='Response')
#'
#' item3 <- longCat(y=ymv[,items==3], tLabels=tLabels)
#' longCatPlot(item3, ylab='', main='Item 3', legendBuffer=0, xlab="", xlas=2)
#' legend(length(unique(times))+.1, nrow(ymv), legend=0:1, col=cols, lty=1, lwd=2, title='Response')
#'
#' par(bg='transparent', mar = c(5, 4, 4, 2) + 0.1, xpd=FALSE)
#'
#' # plot items within time points
#' par(mfrow=c(2,2), bg='cornsilk3', mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
#'
#' time1  <- longCat(y=ymv[,times==1], tLabels=Labels)
#' cols <- longCatPlot(time1, ylab='', main='Time 1', legendBuffer=0, xlab="")
#' legend(length(unique(times))+.1, nrow(ymv), legend=0:1, col=cols, lty=1, lwd=2, title='Response')
#'
#' time2  <- longCat(y=ymv[,times==2], tLabels=Labels)
#' cols <- longCatPlot(time2, ylab='', main='Time 2', legendBuffer=0, xlab="")
#' legend(length(unique(times))+.1, nrow(ymv), legend=0:1, col=cols, lty=1, lwd=2, title='Response')
#'
#' time3  <- longCat(y=ymv[,times==3], tLabels=Labels)
#' cols <- longCatPlot(time3, ylab='', main='Time 3', legendBuffer=0, xlab="")
#' legend(length(unique(times))+.1, nrow(ymv), legend=0:1, col=cols, lty=1, lwd=2, title='Response')
#'
#' time4  <- longCat(y=ymv[,times==4], tLabels=Labels)
#' cols <- longCatPlot(time4, ylab='', main='Time 4', legendBuffer=0, xlab="")
#' legend(length(unique(times))+.1, nrow(ymv), legend=0:1, col=cols, lty=1, lwd=2, title='Response')
#'
#' par(mfrow=c(1,1), bg='transparent', mar = c(5, 4, 4, 2) + 0.1, xpd=FALSE)
#'
#' \dontrun{
#'   # for data sets with many rows, writing directly to a file
#'   # is much faster and unaffected by device resizing, see ?pdf
#'   pdf('C:/mydir/mysubdir/myfile.pdf')
#'   par(bg='cornsilk3')
#'   longCatPlot(f3lc, main='Sorted', colScheme='heat', lwd=2)
#'   par(mfrow=c(1,1), bg='transparent')
#'   dev.off()
#'   # see ?jpeg for picture file options
#' }
#' @seealso \code{\link{longCat}}, for creating the object to be plotted.
#' @export
longCatPlot <- function(lc,
                        xlab            =  "Days" ,
                        ylab            =  NULL   ,
                        cols            =  NULL   ,
                        colScheme       =  'heat' ,
                        reverse         =  FALSE  ,
                        lwd             =  .5     ,
                        lcex            =  1      ,
                        llwd            =  3      ,
                        legendBuffer    =  .12    ,
                        groupBuffer     =  0      ,
                        groupRotation   =  90     ,
                        gcex            =  1      ,
                        seg.len         =  1      ,
                        xlas            =  0      ,
                        xcex            =  1      ,
                        ecex            =  .5     ,
                        event.col       =  1      ,
                        plot.events     =  TRUE   ,
                        which.events    =  NULL   ,
                        n.events        =  NULL   ,
                        event.pch       =  NULL   ,
                        texclude        =  NULL   ,
                        sort            =  TRUE   ,
                        which.state     =  NULL   ,
                        nth.state       =  NULL   ,
                        not.state       =  FALSE  ,
                        which.event     =  NULL   ,
                        nth.event       =  NULL   ,
                        not.event       =  FALSE  ,
                        ...)
{
  if( is(lc) != 'longCat' ){ stop('longCatPlot requires an object of class longCat.')  }
  if(is.null(cols)){ cols <- colChoose(colScheme, lc$nfactors, reverse) }
  if(legendBuffer < 0 | legendBuffer > 1){stop('legendBuffer must be in [0,1]')}
  if(groupBuffer < 0 | groupBuffer > 1){stop('groupBuffer must be in [0,1]')}
  if(is.null(ylab)) ylab = paste("Each Line Represents a Participant, N =", lc$dim[1])
  if(!is.null(lc$events))
  {
    if(ncol(lc$events)==1) # override if the user has one column event matrix
    {
      which.events=NULL
      n.events=NULL
    }
  }

  # on the fly sorting
  if(sort==TRUE)
  {
    if( !lc$sorted & ( any(is.na(lc$y)) |  lc$IndTime) ) lc <- sorter(lc, num=TRUE)
    if( !lc$sorted &  !any(is.na(lc$y)) & !lc$IndTime  ) lc <- sorter(lc, num=FALSE)
  }

  # populate empty objects to avoid if checks below
  if(is.null(lc$y.sorted          )) lc$y.sorted           = lc$y
  if(is.null(lc$times.sorted      )) lc$times.sorted       = lc$times
  if(is.null(lc$events.sorted     )) lc$events.sorted      = lc$events
  if(is.null(lc$event.times.sorted)) lc$event.times.sorted = lc$event.times

  # check whether a subset of events is wanted
  if(plot.events & !is.null(lc$events))
  {
    u.events <- unique( unlist(c(lc$events.sorted)) )
    u.events <- u.events[!is.na(u.events)]
    if(!is.null(which.events))
    {
      if(!all(which.events%in%u.events))
      {
        stop('which.events contains values not in the events matrix')
      }
      if( all(which.events%in%u.events))
      {
        not.events <- u.events[!u.events%in%which.events]
        for(i in 1:length(not.events))
        {
          lc$events.sorted[lc$events.sorted==not.events[i]] <- NA
        }
        rm(not.events)
      }
    }
    rm(u.events)
    # clean the events by removing NAs and collapsing
    temp.events <- clean.events(lc$events.sorted, lc$event.times.sorted)
    lc$events.sorted <- temp.events$events
    lc$event.times.sorted <- temp.events$event.times
    rm(temp.events)
  }

  # restrict range if requested
  if(!is.null(texclude))
  {
    if(length(texclude)!=2) stop("texclude must be a vector of length 2")
    lc$times.sorted[lc$times.sorted<texclude[1]] <- texclude[1]
    lc$times[lc$times<texclude[1]] <- texclude[1]
    lc$times.sorted[lc$times.sorted>texclude[2]] <- texclude[2]
    lc$times[lc$times>texclude[2]] <- texclude[2]
    if(!is.null(lc$event.times.sorted))
    {
      lc$event.times.sorted[lc$event.times.sorted<texclude[1]] <- NA
      lc$event.times.sorted[lc$event.times.sorted>texclude[2]] <- NA
    }
  }

  # apply time alignment if any alignment options are non-NULL
  if(!is.null(which.state) |
     !is.null(nth.state  ) |
     !is.null(which.event) |
     !is.null(nth.event  ) )
  {
    temp.times <- alignTime(y=lc$y.sorted,
                            times=lc$times.sorted,
                            which.state=which.state,
                            nth.state=nth.state,
                            not.state=not.state,
                            events=lc$events.sorted,
                            event.times=lc$event.times.sorted,
                            which.event=which.event,
                            nth.event=nth.event,
                            not.event=not.event)
    lc$times.sorted <- temp.times$aligned.times
    lc$event.times.sorted <- temp.times$aligned.event.times
    rm(temp.times)
  }

  # restrict to the first n events if requested
  if(!is.null(n.events))
  {
    if(!is.numeric(n.events)) stop('n.events must be numeric')
    if(n.events > ncol(lc$events.sorted)) stop('n.events cannot exceed ncol(lc$events.sorted)')
    lc$events.sorted[,(n.events+1):ncol(lc$events.sorted)] <- NA
  }

  # set up plot, pre-allocating a region for labels using ymax
  lo = min(lc$times.sorted, na.rm=T)
  up = max(lc$times.sorted, na.rm=T)
  xrange = lo:up

  # reps is used to automatically scale the x-axis
  reps = nrow(lc$y.sorted)-length(xrange)
  # fix reps if negative, will occur when the number of cases is fewer than
  # the number of time points. This happens when plotting a small # of cases
  if( reps < 0 ) reps <- 0

  # set additional plotting parameters
  xbuffer <- .25*mean(xrange[2:length(xrange)]-xrange[1:(length(xrange)-1)])
  groupBuffer <- ceiling( groupBuffer*up )
  tx <- c(lo-.5, xrange, rep( lo, reps), up+.5 )
  if( !is.null(lc$group) ){ tx[1] <- tx[1] - groupBuffer*xbuffer }
  ymax <- nrow(lc$y.sorted) + ceiling( legendBuffer*nrow(lc$y.sorted) )

  # initiate the empty plot
  if(!exists(deparse(substitute(ylim)))) ylim <- c(0,ymax)
  if("function" %in% class(ylim))  ylim <- c(0,ymax)
  plot(tx,y=rep(NA,length(tx)),col='transparent', ylim=ylim,
       xlab=xlab, ylab='', axes=FALSE, ...)
  if( !exists("cex.axis") ) cex.axis <- 1
  if(groupBuffer >0 | !is.null(lc$group)) title(ylab=ylab, mgp=c(1   ,1,0), cex.axis)
  if(groupBuffer==0 &  is.null(lc$group)) title(ylab=ylab, mgp=c(0.25,1,0), cex.axis)

  # add axes
  tat <- unique(lc$times)
  tat <- tat[1:(length(tat)-1)]
  if(!is.null(lc$tLabels) ) axis( 1, at = tat,
                                  labels=lc$tLabels, las=xlas, cex.axis=xcex )
  if( is.null(lc$tLabels) ) axis( 1, at = NULL )

  # plot loops
  for(r in nrow(lc$y.sorted):1) # loop over cases
  {
    # select plotting y for the r^th case
    pdat  <- as.numeric(lc$y.sorted[r,])
    txx   <- as.numeric(lc$times.sorted[r,])
    tempy <- rep(r,2)
    # loop over observations
    for(j in 1:length(pdat))
    {
      # if observation is NA, skip
      if( !is.na( pdat[j] ) )
      {
        tempx <- c(txx[j], txx[j+1])
        # horizontal line plot
        lines(tempx, tempy, lwd=lwd, col=cols[ as.numeric(pdat[j]) ] )
      }
    }
  }

  # if event.pch are given, convert lc$events.sorted
  if(!is.null(event.pch))
  {
    u.events  <- unique( unlist(c(lc$events.sorted)) )
    u.events  <- sort(u.events[!is.na(u.events)])
    event.pch <- event.pch[1:length(u.events)]
    for(i in 1:length(event.pch))
    {
      lc$events.sorted[lc$events.sorted==u.events[i]] <- event.pch[i]
    }
  }

  # plot event points
  if(!is.null(lc$events) & plot.events)
  {
    for(i in 1:ncol(lc$events))
    {
      points(lc$event.times.sorted[,i], 1:nrow(lc$events.sorted),
             pch=lc$events.sorted[,i], cex=ecex, col=event.col)
    }
  }

  # add legend at top
  if(legendBuffer > 0)
  {
    legend(min(lc$times.sorted, na.rm=T), ymax + .1, legend=lc$Labels, lty=1,
           cex=lcex, col=cols, bty='n', lwd=llwd, seg.len=seg.len, horiz=T)
  }

  # if there is grouping add group labels
  if(!is.null(lc$group))
  {
    if( var(as.numeric(lc$group), na.rm=TRUE)!=0 )
    {
      #g <- data.frame(g=lc$group.sorted,
      #                gn=1:length(lc$group.sorted))
      g   <- as.numeric(unlist( lc$group.sorted ))
      gn  <- 1:length(g)
      gag <- aggregate(gn, list(group=g), mean, na.rm=T)
      u.g <- unique(g[!is.na(g)])
      for(i in 1:length(u.g))
      {
        text(tx[1], gag$x[i], labels = lc$groupLabels[i], srt=groupRotation, cex=gcex)
      }
    }
  }
  # return colors
  invisible(cols)
}
