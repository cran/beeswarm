# beeswarm.R
#
# 2010-05-02
# Aron Charles Eklund
#
#


beeswarm <- function (x, ...) 
  UseMethod("beeswarm")


beeswarm.numeric <- function(x, ...) {
  x.list <- list(x)
  names(x.list) <- deparse(substitute(x))
  beeswarm(x.list, ...)
}

beeswarm.default <- function(x, 
    method = c("center", "hex", "square", "smile"), 
    vertical = TRUE, horizontal = !vertical, 
    cex = par("cex"), spacing = 1, breaks = NULL,
    labels, at = NULL, 
    pch = 1, col = 1, bg = col, 
    pwpch = NULL, pwcol = NULL, pwbg = pwcol,
    do.plot = TRUE, add = FALSE, 
    xlim, ylim, log = FALSE, 
    xlab = NULL, ylab = NULL, dlab = "", glab = "",
    ...) {
    
  method = match.arg(method)
  if(method == 'smile' && log) {
    stop('The combination of method="smile" and log=TRUE is not supported')
  }
  n.groups <- length(x)

  if(missing(labels)) {
    if(is.null(names(x))) {
      labels <- 1:n.groups
    } else {
      labels <- names(x)
    }
  }

  if (is.null(at)) 
    at <- 1:n.groups
  else if (length(at) != n.groups) 
    stop(gettextf("'at' must have length equal to %d, the number of groups", 
      n.groups), domain = NA)

  if (is.null(dlab)) 
     dlab <- deparse(substitute(x))

  # this provides a "group" vector to complement the usual "unlist"
  unlist2 <- function(x, nms = names(x)) rep(nms, sapply(x, length))

  x.val <- unlist(x)
  x.gp <- unlist2(x)
  if((range(x.val, finite = TRUE)[1] <= 0) && log)
    warning('values <= 0 omitted from logarithmic plot')
  
  n.obs <- length(x.val)
  n.obs.per.group <- sapply(x, length)
  if(missing(xlim)) xlim <- c(min(at) - 0.5, max(at) + 0.5)
  if(missing(ylim)) {
    if(log) {
      ylim <- 10 ^ (extendrange(log10(x.val[x.val > 0])))
    } else {
      ylim <- extendrange(x.val, f = 0.01)
    }
  }

  if(is.null(pwpch)) {
    pch.out <- unlist2(x, nms = rep(pch, length.out = n.groups))
  } else {
    if(is.list(pwpch)) {
      names(pwpch) <- names(x)
      stopifnot(all(sapply(pwpch, length) == n.obs.per.group))
      pch.out <- unlist(pwpch)
    } else {
      pch.out <- pwpch
    }
  }
  stopifnot(length(pch.out) == n.obs)

  if(is.null(pwcol)) {
    col.out <- unlist2(x, nms = rep(col, length.out = n.groups))
  } else {
    if(is.list(pwcol)) {
      names(pwcol) <- names(x)
      stopifnot(all(sapply(pwcol, length) == n.obs.per.group))
      col.out <- unlist(pwcol)
    } else {
      col.out <- pwcol
    }
  }
  stopifnot(length(col.out) == n.obs)

  if(is.null(pwbg)) {
    bg.out <- unlist2(x, nms = rep(bg, length.out = n.groups))
  } else {
    if(is.list(pwbg)) {
      names(pwbg) <- names(x)
      stopifnot(all(sapply(pwbg, length) == n.obs.per.group))
      bg.out <- unlist(pwbg)
    } else {
      bg.out <- pwbg
    }
  }
  stopifnot(length(bg.out) == n.obs)
  
  if(do.plot & !add) {
    if(horizontal) {
      if (is.null(xlab)) 
        xlab <- dlab
      if (is.null(ylab)) 
        ylab <- glab
      plot(ylim, xlim, 
        type = 'n', axes = FALSE, 
        log = ifelse(log, 'x', ''),
        xlab = xlab, ylab = ylab, ...)
    } else {     # vertical
      if (is.null(ylab)) 
         ylab <- dlab
      if (is.null(xlab)) 
         xlab <- glab
      plot(xlim, ylim, 
        type = 'n', axes = FALSE,  
        log = ifelse(log, 'y', ''),
        xlab = xlab, ylab = ylab, ...)
    }
  }

  if(horizontal) {
    size.g <- yinch(0.08, warn.log = FALSE) * cex * spacing
    size.d <- xinch(0.08, warn.log = FALSE) * cex * spacing
  } else {    # vertical
    size.g <- xinch(0.08, warn.log = FALSE) * cex * spacing
    size.d <- yinch(0.08, warn.log = FALSE) * cex * spacing
  }
  
  if(method == 'smile') {

    x.offset <- lapply(x, smile, xsize = size.d, ysize = size.g)
    x.pos <- lapply(1:n.groups, function(i) at[i] + (x.offset[[i]]))

    y.pos <- x

  } else {
      
      if(method == 'hex') size.d <- size.d * sqrt(3) / 2
    
      if(log) {
        if(is.null(breaks))
          breaks <- 10 ^ seq(log10(ylim[1]), log10(ylim[2]) + size.d, by = size.d)
        if(length(breaks) == 1 && is.na(breaks[1])) {
          y.index <- x
          y.pos <- x
        } else {
          mids <- 10 ^ ((log10(head(breaks, -1)) + log10(tail(breaks, -1))) / 2)
          y.index <- lapply(x, cut, breaks = breaks, labels = FALSE)
          y.pos <- lapply(y.index, function(a) mids[a])  
        }
      } else {
        if(is.null(breaks))
          breaks <- seq(ylim[1], ylim[2] + size.d, by = size.d)
        if(length(breaks) == 1 && is.na(breaks[1])) {
          y.index <- x
          y.pos <- x
        } else {
          mids <- (head(breaks, -1) + tail(breaks, -1)) / 2
          y.index <- lapply(x, cut, breaks = breaks, labels = FALSE)
          y.pos <- lapply(y.index, function(a) mids[a])  
        }
      }  
    
      x.index <- lapply(y.index, function(v) {
        if(length(v) == 0) return(v)
        v.s <- lapply(split(v, v), seq_along)
        if(method == 'center')
          v.s <- lapply(v.s, function(a) a - mean(a))
        else if(method == 'square')
          v.s <- lapply(v.s, function(a) a - floor(mean(a)))
        else if(method == 'hex') {
          odd.row <- (as.numeric(names(v.s)) %% 2) == 1
          v.s[odd.row] <- lapply(v.s[odd.row], function(a) a - floor(mean(a)) - 0.25)
          v.s[!odd.row] <- lapply(v.s[!odd.row], function(a) a - ceiling(mean(a)) + 0.25)
        }
        unsplit(v.s, v)
      })  
      
      x.pos <- lapply(1:n.groups, function(i) at[i] + (x.index[[i]] * size.g))
    
  }

  out <- data.frame(x = unlist(x.pos), 
                    y = unlist(y.pos), 
                    pch = pch.out, col = col.out, bg = bg.out,
                    x.orig = x.gp, y.orig = x.val,
                    stringsAsFactors = FALSE)

  if(do.plot) {
    if(horizontal) { 
      points(out$y, out$x, pch = out$pch, col = out$col, bg = out$bg, cex = cex)  
      if(!add) {
        axis(1, ...)
        axis(2, at = at, labels = labels, tick = FALSE, ...)
        box(...)
      }
    } else {
      points(out$x, out$y, pch = out$pch, col = out$col, bg = out$bg, cex = cex)  
      if(!add) {
        axis(2, ...)
        axis(1, at = at, labels = labels, tick = FALSE, ...)
        box(...)
      }
    }
  }
  invisible(out)
}

  
beeswarm.formula <- function (formula, data = NULL, subset, na.action = NULL, 
    pwpch = NULL, pwcol = NULL, pwbg = NULL, dlab, glab, ...) 
{
    if (missing(formula) || (length(formula) != 3)) 
        stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame()))) 
        m$data <- as.data.frame(data)
    m$... <- NULL
    m$dlab <- NULL
    m$glab <- NULL
    m$na.action <- na.action
    require(stats, quietly = TRUE)
    m[[1]] <- as.name("model.frame")
    mf <- eval(m, parent.frame())
    response <- attr(attr(mf, "terms"), "response")
    if (missing(dlab)) 
        dlab <- names(mf)[response]
    # assume second term is the grouping factor!!!!
    if (missing(glab)) 
        glab <- names(mf)[2]
    # assume second term is the grouping factor!!!!
    if(!is.null(mf$'(pwpch)')) pwpch <- split(mf$'(pwpch)', mf[[2]])
    if(!is.null(mf$'(pwcol)')) pwcol <- split(mf$'(pwcol)', mf[[2]])
    if(!is.null(mf$'(pwbg)')) pwbg <- split(mf$'(pwbg)', mf[[2]])
    beeswarm(split(mf[[response]], mf[[2]]), 
      pwpch = pwpch, pwcol = pwcol, pwbg = pwbg,
      dlab = dlab, glab = glab, ...)
}
  
smile <- function(x, xsize, ysize) {
  out <- data.frame(x = x / xsize, y = 0, i = seq(along = x))
  out <- out[order(out$x), ]
  if(nrow(out) > 1) {
    for (i in 2:nrow(out)) {
      xi <- out$x[i]
      yi <- out$y[i]
      pre <- out[1:(i - 1), ] # previous points
      wh <- xi - pre$x < 1  # which ones are potentially overlapping
      wh[is.na(wh)] <- FALSE  # missing values are not potentially overlapping
      if(any(wh)) {
        pre <- pre[wh, ]
        pre <- pre[order(abs(pre$y)), ]
        poty.off <- sqrt(1 - ((xi - pre$x) ^ 2)) # potential y offset
        poty <- c(0, pre$y + poty.off, pre$y - poty.off) # potential y values
        poty.bad <- sapply(poty, function(y) { # check for overlaps
          any(((xi - pre$x) ^ 2 + (y - pre$y) ^ 2) < 0.999)
        })
        poty[poty.bad] <- Inf
        out$y[i] <- poty[which.min(abs(poty))]
      } else {
        out$y[i] <- 0
      }
    }
  }
  out <- out[order(out$i), ]
  out[is.na(out$x), 'y'] <- NA  # missing x values should have missing y values
  out$y * ysize
}


