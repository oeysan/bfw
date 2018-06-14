#' @title Mean Data
#' @description Compute means and standard deviations.
#' @param y criterion variable(s), Default: NULL
#' @param y.names optional names for criterion variable(s), Default: NULL
#' @param x categorical variable(s), Default: NULL
#' @param x.names optional names for categorical variable(s), Default: NULL
#' @param DF User defined data frame, Default: NULL
#' @param params define parameters to observe, Default: NULL
#' @param initial.list Initial values for simulations, Default: list()
#' @param ... further arguments passed to or from other methods
#' @return mean and standard deviation
#' @rdname StatsMean
#' @export 

StatsMean <- function(y,
                      y.names,
                      x,
                      x.names,
                      DF,
                      params,
                      initial.list,
                      ...
) {

  # Select variable(s) to analyze
  y <- TrimSplit(y)
  # Make unique if duplicate names
  if ( any(duplicated(y)) ) y <- make.unique((y))
  # Check if variables are in data frame
  y.check <- y %in% colnames(DF)
  if (any(y.check == FALSE)) {
    lapply(which(y.check == FALSE), function (i) warning(y[i]," does not exist. Deleting.") )
    y <- y[y.check]
  }

  # Create y names, either from specified names or variable names
  y.names <- if (!is.null(y.names)) TrimSplit(y.names) else CapWords(y)
  # If y.names and y are of unequal length
  if ( length(y.names) != length(y) ) {
    warning("y.names and y have unequal length. Using variables names.")
    y.names <- CapWords(y)
  }

  # If any, select group(s) to analyze by
  if (!is.null(x)) {

    # Split groups
    x <- TrimSplit(x)

    # Make unique if duplicate names
    if ( any(duplicated(x)) ) x <- make.unique((x))

    # Check if variables are in data frame
    x.check <- x %in% colnames(DF)
    if (any(x.check == FALSE)) {
      lapply(which(x.check == FALSE), function (i) warning(y[i]," does not exist. Deleting.") )
      x <- x[x.check]
    }

    # Create x names, either from specified names or variable names
    x.names <- if (!is.null(x.names)) TrimSplit(x.names) else CapWords(x)
    # If x.names and x are of unequal length
    if ( length(x.names) != length(x) ) {
      warning("x.names and x have unequal length. Using variable names.")
      x.names <- CapWords(x)
    }

    # Select levels from x variables and remove NA
    x.li <- lapply(x, function (x) {
      x.li <- levels(DF[,x])
      x.li[unlist(lapply(x.li, function (li) {
        do.call(any,lapply(y, function (z) {
          !all(is.na(DF[ DF[,x] == li, z]))
        }))
      }))]
    })

    # Create list of data for y
    y.data <- unlist ( lapply(1:length(y), function (i) {

      x <- unlist(lapply(1:length(x), function (j) {
        lapply(x.li[[j]], function (z) {
          sort( DF[ DF[, x[j]  ] == z , y[i] ] , na.last=TRUE )
        })
      }), recursive=FALSE)

      y <- sort(DF[,y[[i]]],na.last=TRUE)

      return( c(list(y),x))

    }), recursive=FALSE)

    # Create list of names for y
    job.names <- unlist( lapply(1:length(y.names), function (i) {
      y <- y.names[[i]]
      x <- lapply(1:length(x.names), function (j) {
        lapply(x.li[[j]], function (z) {
          sprintf("%s vs. %s @ %s", y.names[[i]], x.names[[j]], z)
        })
      })
      return (c(y,x))
    }) )

    # If By is not selected
  } else {

    y.data <-  lapply(1:length(y), function (i) {
      y <- sort(DF[, y[[i]]], na.last=TRUE)
    })

    x.li <- NULL
    job.names <- y.names
  }

  # Max length of y
  n.max <- max(sapply(y.data, length))

  # Make all data equal length
  y.data <- lapply(y.data, function(x) c( x, rep(NA, n.max - length(x)) ) )

  # Create final data matrix of y
  y <- do.call(cbind,y.data)

  # Number of observations in each y
  n <- colSums(!is.na(y))

  # Number of columns / analyzes
  q <- ncol(y)

  # Create crosstable for x parameters
  n.data <- if (is.null(x)) data.frame(list(y.names,n))  else as.data.frame(table(DF[, x]))

  # Paramter(s) of interest
  params <- if(length(params)) TrimSplit(params) else c("mu", "sigma")

    # Create data for Jags
  data.list <- list(
    n = n,
    y = y,
    q = q
  )

  # Create name list
  name.list <- list(
    job.names = job.names,
    y.names = y.names,
    x.names = x.names,
    x.li = x.li
  )

  # Return data list
  return ( list (
    params = params,
    data.list = data.list,
    name.list = name.list,
    n.data = n.data
  ))

}
