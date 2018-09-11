#' @title Add Names
#' @description Add names to columns from naming list
#' @param par defined parameter to analyze (e.g., "cor[1,2]")
#' @param job.names names of all parameters in analysis, Default: NULL
#' @param job.group for some hierarchical models with several layers of parameter names (e.g., latent and observed parameters), Default: NULL
#' @param keep.par logical, indicating whether or not to keep parameter name (e.g., "cor[1,2]"), Default: TRUE
#' @param names.only logical, indicating whether or not to return vector (TRUE) or string with separator (e.g., "cor[1,2]: A vs. B"), Default: FALSE
#' @param ... further arguments passed to or from other methods
#' @examples
#' par <- "cor[1,2]"
#' job.names <- c("A","B")
#' AddNames(par, job.names, keep.par = TRUE)
#' # [1]  "cor[1,2]: A vs. B"
#' AddNames(par, job.names, keep.par = FALSE)
#' # [1]  "A vs. B"
#' AddNames(par, job.names, names.only = TRUE)
#' # [1]  "A" "B"
#' @rdname AddNames
#' @export

AddNames <- function(par, job.names, job.group = NULL, keep.par = TRUE, names.only = FALSE, ...) {

  if (length(job.names) & length(grep("\\[",par))) {

    # check if list
    if (typeof(job.names) != "list") {
      job.names <- list(list(job.names))
    } else if (typeof(job.names[[1]]) != "list") {
      job.names <- list(job.names)
    }

    # Remove brackets
    pre.bracket <- gsub("\\[.*","",par)

    # Position in job groups
    pos.value <- max ( which( lapply(job.group, function (x) any ( x %in% pre.bracket) ) == TRUE) , 1)

    # If job.names contain guiding values in parameter name
    if (!0 %in% ParseNumber(pre.bracket)) {
      pre.values <- ParseNumber(pre.bracket)
    } else {
      pre.values <- seq(length(job.names[[pos.value]]))
    }

    # Get bracket values
    post.values <- ParseNumber(gsub(".*\\[(.*)\\].*", "\\1", par))

    # create pre and pos values of equal length
    if ( length(pre.values) > length(post.values) ) {
      post.values <- rep(post.values,length(pre.values))
    } else if ( length(pre.values) < length(post.values) ) {
      pre.values <- rep(pre.values,length(post.values))
    }

    # Fetch names
    add.name <- lapply(1:length(post.values), function (i) {
      job.names[[pos.value]][[pre.values[i]]][[post.values[i]]]
    })

    # Seperate names if there are multiple parameters (e.g., A vs. B)
    add.name <- if (names.only) unlist(add.name) else paste(add.name,collapse=" vs. ")

    # Create new parameter name
    if (exists("add.name")) par <- if (keep.par & !names.only) paste(par, add.name, sep = ": ") else add.name

  }
  return (par)
}

#' @title Compute HDI
#' @description Compute highest density interval (HDI) from posterior output
#' @param data data to compute HDI from
#' @param credible.region summarize uncertainty by defining a region of most credible values (e.g., 95 percent of the distribution), Default: 0.95
#' @return Return HDI
#' @details values within the HDI have higher probability density than values outside the HDI, and the values inside the HDI have a total probability equal to the credible region (e.g., 95 percent).
#' @examples
#' set.seed(1)
#' data <-rnorm(100,0,1)
#' credible.region <- 0.95
#' ComputeHDI(data,credible.region)
#' # hdi.min hdi.max
#' # -1.99    1.60
#' @rdname ComputeHDI
#' @export

ComputeHDI <- function(data, credible.region) {
  data <- sort(data)
  ci.interval <- credible.region * length(data)
  length.ci <- max(length(data) - ci.interval,1)
  ci.mass <- lapply(1:length.ci, function(i) {
    data[min(i + ci.interval,length(data))] - data[i]
  })
  hdi.min <- data[which.min(ci.mass)]
  hdi.max <- data[which.min(ci.mass) + min(ci.interval,length(data)-1)]
  hdi <- c(hdi.min = hdi.min, hdi.max = hdi.max)

  return (hdi)
}

#' @title Contrast Names
#' @description utilize the AddNames function to create contrast names
#' @param items items to create names for
#' @param job.names names of all parameters in analysis, Default: NULL
#' @param col.names columns in MCMC to create names from
#' @rdname ContrastNames
#' @export

ContrastNames <- function(items , job.names , col.names) {

  items <- as.numeric(TrimSplit(items))
  if (length(items)==2) {
    a <- AddNames(col.names[items[1]], job.names, names.only = TRUE)
    b <- AddNames(col.names[items[2]], job.names, names.only = TRUE)

    names <- paste(lapply(1:length(a) , function (j) {
      ab <- if (a[j]!=b[j]) paste(a[j],b[j],sep="/") else a[j]
      if (j>1 & a[j]!=b[j]) paste0(" vs. ",ab) else if (j>1) paste0(" @ ",ab) else ab
    }),collapse="")
  }

  return (names)
  
}

#' @title Matrix Combinations
#' @description Create matrices from combinations of columns
#' @param m matrix to combine
#' @param s stem name of columns to use (e.g., "m" for mean)
#' @param lvl number of levels per column
#' @param rm.last logical, indicating whether or not to remove last combination (i.e., m1m2m3m4) , Default: TRUE
#' @param row.means logical, indicating whether or not to compute row means from combined columns, else use row sums, Default: TRUE
#' @rdname MatrixCombn
#' @export

MatrixCombn <- function(m,s,lvl,rm.last=TRUE,row.means=TRUE) {
  s <- TrimSplit(s)
  grid <- expand.grid(lapply(lvl, function (x) seq(x)))
  q <- ncol(grid)

  matrix.list <- lapply(seq(q-as.numeric(rm.last)), function (i) {
    q.combn <- t(combn(as.numeric(paste0(seq(q))),i))
    q.combn <- split(q.combn, 1:nrow(q.combn))
    lapply(q.combn, function (x) {
      cols <- expand.grid(lapply(x, function (j) seq(lvl[[j]] ) ) )
      colnames(cols) <- c(x)
      lapply(1:nrow(cols), function (k) {
        col <- paste(sprintf("grid[,%s]==%s",colnames(cols),cols[k,]),collapse="&")
        lapply(s, function (y) {

          s <- paste0(y,seq(q),collapse="")
          s.names <- colnames(m[, grep(paste0("\\b",s,"\\b"),colnames(m))])

          new.col <- as.matrix(m[,s.names[eval(parse(text=paste0(col)))]])
          if (ncol(new.col)>1) {
            new.col <- if (row.means) rowMeans(new.col) else rowSums(new.col)
          }
          new.col <- as.matrix(new.col)
          new.colname <- paste0(y,colnames(cols),collapse="")
          new.colname <- sprintf("%s[%s]",new.colname,paste(cols[k,],collapse=","))
          colnames(new.col) <- new.colname

          return (new.col)

        })
      })
    })
  })
  do.call(cbind,FlattenList(matrix.list))
}

#' @title Merge MCMC
#' @description Merge two or more MCMC simulations
#' @param pat pattern to select MCMC chain from
#' @param project.dir define where to save data, Default: 'Results/'
#' @param data.sets data sets to combine
#' @return Merged MCMC chains
#' @seealso
#'  \code{\link[utils]{tail}}
#'  \code{\link[runjags]{combine.mcmc}}
#' @rdname MergeMCMC
#' @export
#' @importFrom utils tail
#' @importFrom runjags combine.mcmc

MergeMCMC <- function (pat , project.dir = "Results/" , data.sets) {

  # Stop function if MCMC data is not found
  if ( length ( list.files(project.dir, pattern = pat ) ) < 2 ) {
    stop("MCMC chains not found. Quitting")
  }

  # Else merge MCMC chains
  merge.MCMC <- lapply(data.sets, function (x) {
    get.pat <- sprintf(".*(%s.*%s).*", x, pat)
    find.file <- list.files(paste0(project.dir,"MCMC/"), pattern = toString(get.pat))
    got.file <- utils::tail(find.file[order(find.file)],1)
    return ( readRDS(paste0(project.dir,"MCMC/",got.file))$MCMC )
  })

  return ( runjags::combine.mcmc(merge.MCMC) )

}

#' @title Run Contrasts
#' @description Compute contrasts from mean and standard deviation (Cohen's d) or frequencies (odds ratio)
#' @param contrast.type type of contrast: "m" indicate means and standard deviations, "o" indicate frequency
#' @param q.levels Number of levels of each variable/column
#' @param use.contrast choose from "between", "within" and "mixed". Between compare groups at different conditions. Within compare a group at different conditions. Mixed compute all comparisons
#' @param contrasts specified contrasts columns
#' @param data data to compute contrasts from
#' @param job.names names of all parameters in analysis, Default: NULL
#' @seealso
#'  \code{\link[utils]{combn}}
#' @rdname RunContrasts
#' @export
#' @importFrom utils combn

RunContrasts <- function(contrast.type, 
                         q.levels, 
                         use.contrast, 
                         contrasts, 
                         data,
                         job.names) {
  
  col.names <- colnames(data)
  q.seq <- seq(length(q.levels))
  if (!is.null(contrasts)) q.seq <- q.seq[q.seq %in% as.numeric(TrimSplit(contrasts)) ]
    
  contrasts.col <- unlist(lapply(1:length(q.seq), function (i) {
    apply(utils::combn(q.seq,i),2,function (y) paste0(contrast.type,y,collapse=""))
  }))
  
  if (contrast.type == "b" ) {
    print.type <- "sum-to-zero coefficients"
  } else if (contrast.type == "m" ) {
    print.type <- "mean differences"
  } else if (contrast.type == "o" ) {
    print.type <- "odds and odds-ratios" 
  }
  
  cat(paste0("\nCalculate " , print.type , "\n")) 
  contrasts.start.time  <- Sys.time()
  done.contrasts <- lapply(1:length(contrasts.col), function (col) {
   
    x <- contrasts.col[[col]]
    var <- ParseNumber(x)
    q <- length(var)
    select.cols <- grep(paste0("\\b",x,"\\b"),colnames(data))
        
    if (contrast.type == "m")  select.sigma <- grep(paste0("\\b",gsub("m", "s", x),"\\b"),colnames(data))
    
    if ( (use.contrast == "between" & contrast.type != "o") | q == 1) {
      
      grid <- matrix(1:length(select.cols),ncol=q.levels[var[1]],byrow = TRUE)
      done.contrasts <- lapply(1:nrow(grid), function (i) {
        
        x <- t(utils::combn(grid[i,],2))
        
        lapply(1:nrow(x), function (i) {
          
          if (contrast.type == "m") {
            m <- as.matrix((( data[ , select.cols[x[i,1]] ]   - data[ , select.cols[x[i,2]] ]  ) /
                              sqrt((data[ , select.sigma[x[i,1]] ]^2 + data[ , select.sigma[x[i,2]] ]^2) / 2)))
          } else if (contrast.type == "o") {
            m <- as.matrix( data[ , select.cols[x[i,1] ] ] / data[ , select.cols[x[i,2] ] ] )
          } else {
            m <- as.matrix( data[ , select.cols[x[i,1] ] ] - data[ , select.cols[x[i,2] ] ] )
          }
          
          colnames(m) <- ContrastNames(select.cols[x[i,]],job.names,col.names)
          if (contrast.type == "o") colnames(m) <- paste0("Odds: ", colnames(m)) 
          
          return (m)
        })
      })
    } else if (use.contrast == "within" | contrast.type == "o") {
      
      grid <- expand.grid(lapply(q.levels[var], function (x) seq(x)))
      grid.combn <- expand.grid(lapply(seq(var)[-2], function (i) {
        lapply(seq(q.levels[var[i]]), function (j) {
          sprintf("grid[,%s] == %s",i,j)
        })
      }))
      done.contrasts <- lapply(1:nrow(grid.combn), function (k) {
        x <- utils::combn( select.cols[ eval(parse(text=paste(unlist(grid.combn[k,]),collapse="&"))) ] , 2 )
        if (contrast.type == "m") x.sigma <- utils::combn( select.sigma[ eval(parse(text=paste(unlist(grid.combn[k,]),collapse="&"))) ] , 2 )
        lapply(1:ncol(x), function (l) {
          
          if (contrast.type == "m") {
            m <- as.matrix((( data[, x[1,l] ] - data[, x[2,l] ]  ) /
                              sqrt((data[, x.sigma[1,l] ]^2 + data[, x.sigma[2,l] ]^2) / 2)))
          } else if (contrast.type == "o") {
            m <- as.matrix( data[, x[1,l] ] / data[, x[2,l] ] )
          } else {
            m <- as.matrix( data[, x[1,l] ] - data[, x[2,l] ] )
          }
          
          colnames(m) <- ContrastNames(x[,l],job.names,col.names)
          
          return (m)
        })
      })
    } else if (use.contrast == "mixed") {
      
      x <- as.matrix(t(utils::combn(select.cols,2)))
      if (contrast.type == "m") x.sigma <- as.matrix(t(combn(select.sigma,2)))
      
      done.contrasts <- lapply(1:nrow(x), function (i) {
        
        if (contrast.type == "m") {
          m <- as.matrix((( data[ , x[i,1] ]  - data[ , x[i,2] ] ) /
                            sqrt((data[ , x.sigma[i,1] ]^2 + data[ , x.sigma[i,2] ]^2) / 2)))
        } else {
          m <- as.matrix( data[ , x[i,1] ] - data[ , x[i,2] ] )
        }
        
        colnames(m) <- ContrastNames(x[i,],job.names,col.names)
        
        return (m)
      })
    }
    
    if (contrast.type == "o" & q>1) {
      
      # Sort odds by first column in matrix
      odds <- lapply(seq(q.levels[var[1]]), function (l) {
        do.call(cbind,FlattenList(done.contrasts[ seq(l,length(done.contrasts),
                                                  length(seq(q.levels[var[1]]))) ] ))
      })
      
      # Compute odds ratio from odds
      odds.ratio <- combn(seq(q.levels[var[1]]),2)
      odds.ratio <- apply(odds.ratio, 2, function (x) {
        lapply(1:ncol(odds[[1]]), function (i) {
          
          # Odds ratio
          m <- as.matrix(odds[[x[1]]][,i] / odds[[x[2]]][,i])
          # Reversed odds ratio
          m.rev <- as.matrix(odds[[x[2]]][,i] / odds[[x[1]]][,i])
          
          # Add names (pretty code...)
          a <- TrimSplit(colnames(odds[[x[1]]])[i],sep="vs.")
          b <- TrimSplit(colnames(odds[[x[2]]])[i],sep="vs.")
          colnames(m) <- paste( paste(a[1],b[1],sep="/"), a[2], sep=" vs. ")
          colnames(m.rev) <- paste( paste(b[1],a[1],sep="/"), a[2], sep=" vs. ")
          cbind(m,m.rev)
        })
      })
      
      # Flatten and cbind odds
      odds <- do.call(cbind,FlattenList(odds))
      
      # Flatten and cbind odds ratios
      odds.ratio <-  do.call(cbind,FlattenList(odds.ratio))
      
      # Compute effect size from odds ratio (Chinn, 2000)
      effect.size <- apply(odds.ratio, 2, function (x) log(x) / ( pi / sqrt(3) ))
      
      # Define columns
      colnames(odds) <- paste0("Odds: ", colnames(odds))
      colnames(odds.ratio) <- paste0("Odds ratio: ", colnames(odds.ratio))
      colnames(effect.size) <- paste0("Effect size: ", colnames(effect.size))
      
      # Final matrix
      done.contrasts <- list(odds,odds.ratio,effect.size)
    }
    
    done.contrasts <- do.call(cbind,FlattenList(done.contrasts))
    if (contrast.type == "b") colnames(done.contrasts) <- paste0("Beta difference: ",colnames(done.contrasts))
    if (contrast.type == "m") colnames(done.contrasts) <- paste0("Effect size: ",colnames(done.contrasts))
    
    ETA( contrasts.start.time , col , length(contrasts.col) )
    
    return (done.contrasts)
    
  })

  # cbind and return contrasts
  do.call(cbind,FlattenList(done.contrasts))
  
}

#' @title Sum to Zero
#' @description Compute sum to zero values across all levels of a data matrix
#' @param q.levels number of levels of each variable/column
#' @param data data matrix to combine from
#' @param contrasts specified contrasts columns
#' @examples
#'  data <- matrix(c(1,2),ncol=2)
#'  colnames(data) <- c("m1[1]","m1[2]")
#'  SumToZero( 2 , data , contrasts = NULL )
#'  #               b0[1] b1[1] b1[2]
#'  #       m1[1]   1.5  -0.5   0.5
#' @rdname SumToZero
#' @export

SumToZero <- function(q.levels, data, contrasts) {

  q <- length(q.levels)
  x <- paste0("m",seq(q),collapse="")
  grid <- expand.grid(lapply(q.levels[seq(q)], function (x) seq(x)))
  data.col <- grep(paste0("\\b",x,"\\b"),colnames(data))

  b0 <- mean(data[, data.col])
  b <- lapply(seq(q), function (i) {
    do.call(cbind,lapply(seq(q.levels[[i]]), function (j) {
      select <- paste(sprintf("grid[,%s]==%s",i,j),collapse="&")
      b.mean <- data [ , data.col[eval(parse(text=paste0(select))) ]]
      if (!is.null(ncol(b.mean))) b.mean <- rowMeans(b.mean)
      m <- as.matrix(b.mean - b0)
      colnames(m) <- sprintf("b%s[%s]",i,j)

      return (m)
    }))
  })


  bs <- do.call(cbind,FlattenList(lapply(seq(q)[-1], function (i) {
    q.combn <- t(combn(as.numeric(paste0(seq(q))),i))
    q.combn <- split(q.combn, 1:nrow(q.combn))
    lapply(q.combn, function (x) {
      cols <- expand.grid(lapply(x, function (j) seq(q.levels[[j]] ) ) )
      colnames(cols) <- x
      lapply(1:nrow(cols), function (k) {
        present <- paste(sprintf("b[[%s]][,%s]",colnames(cols),cols[k,]),collapse="+")
        if(length(x)<q) {
          absent <- paste(sprintf("rowSums(b[[%s]])",seq(q)[!(seq(q) %in% x)]),collapse="+")
          li <- paste("b0",present,absent,sep="+")
        } else {
          li <- paste("b0",present,sep="+")
        }
        select <- paste(sprintf("grid[,%s]==%s",colnames(cols),cols[k,]),collapse="&")
        b.mean <- data [ , data.col[eval(parse(text=paste0(select))) ]]
        if (!is.null(ncol(b.mean))) b.mean <- rowMeans(b.mean)
        m <- as.matrix(b.mean - eval(parse(text=li)))
        colnames(m) <- paste0( paste0("b",colnames(cols),collapse=""),
                               sprintf("[%s]",paste(cols[k,],collapse=",")) )

        return (m)
      })
    })
  })))

  b <- cbind(do.call(cbind,b),bs)

  if (!is.null(contrasts)) {
    q.seq <- seq(length(q.levels))
    q.seq <- q.seq[q.seq %in% as.numeric(TrimSplit(contrasts)) ]
    contrasts.col <- unlist(lapply(1:length(q.seq), function (i) {
      apply(combn(q.seq,i),2,function (x) paste0("b",x,collapse=""))
    }))
    b <- b[, sub('\\[.*', '', colnames(b)) %in% contrasts.col]
  }

  return (b)
}
