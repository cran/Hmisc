ecdf <- function(x, ...) UseMethod('ecdf')


ecdf.default <- function(x, what=c('F','1-F','f'), 
                         weights=rep(1,length(x)), normwt=FALSE,
                         xlab, ylab, q, pl=TRUE, add=FALSE, lty=1,
                         col=1, group=rep(1,length(x)), 
                         label.curves=TRUE, xlim, subtitles=TRUE, 
                         datadensity=c('none','rug','hist','density'), 
                         side=1, 
                         frac=switch(datadensity,
                                     none=NA,rug=.03,hist=.1,density=.1),
                         dens.opts=NULL, lwd=1, ...)
{
  datadensity <- match.arg(datadensity)
  colspec <- FALSE
  if(datadensity != 'none') {
    if(side %in% c(2,4))
      stop('side must be 1 or 3 when datadensity is specified')
    
    if('frac' %nin% names(dens.opts))
      dens.opts$frac <- frac
    
    if('side' %nin% names(dens.opts))
      dens.opts$side <- side
    
    if('col' %in%   names(dens.opts))
      colspec <- TRUE
  }

  if(missing(xlab)) {
    ##xlab <- attr(x,"label")  26sep02
    ##if(is.null(xlab) || xlab=="")xlab <- deparse(substitute(x))
    xlab <- label(x, units=TRUE, plot=TRUE, default=deparse(substitute(x)))
  }
  
  what <- match.arg(what)
  if(missing(ylab)) ylab <- switch(what,
                                   'F'='Proportion <= x',
                                   '1-F'='Proportion > x',
                                   'f'='Frequency <= x')
  
  group <- as.factor(group)
  nna <- !(is.na(x)|is.na(group)|is.na(weights))
  if(length(x) != length(group))
    stop('length of x != length of group')
  
  X <- x[nna]
  group <- group[nna]

  lev <- levels(group)
  nlev <- length(lev)
  curves <- vector('list',nlev)
  names(curves) <- lev

  lty <- rep(lty, length=nlev)
  col <- rep(col, length=nlev)
  lwd <- rep(lwd, length=nlev)

  if(missing(xlim))
    xlim <- range(X)

  n <-
    if(normwt)
      length(X)
    else
      sum(weights[nna])
  
  m <- (if(normwt)
          length(nna)
        else
          sum(weights, na.rm=TRUE)) - n
  
  weights <- weights[nna]

  for(i in 1:nlev) {
    s <- group == lev[i]
    x <- X[s]
    wt <- weights[s]
    xorig <- x

    z <- wtd.ecdf(x, wt, type='i/n', normwt=normwt, na.rm=FALSE)
    x <- z$x; y <- z$ecdf
    switch(what,
           '1-F' = {y <- 1-y},
           'f'   = {y <- y * sum(wt)})

    if(pl) {
      if(i==1 && !add)
        plot(x, y, xlab=xlab, ylab=ylab, xlim=xlim, type='n', ...)
      
      lines(x,y, type="s", lty=lty[i], col=col[i], lwd=lwd[i])
      if(subtitles && i==1) {
        pm <- paste("n:",n," m:",m,sep="")
        title(sub=pm,adj=0,cex=.5)
      }

      if(!missing(q)) {
        if(what=='f') q <- q*y[length(y)] else if(what=='1-F') q <- 1-q
        q <- switch(what,
                    'f'   = q*sum(wt),
                    '1-F' = 1 - q,
                    'F'   = q)
        
        a <- par("usr")
        for(w in q) {
          quant <-
            if(what=='1-F')
              min(x[y<=w])
            else
              min(x[y>=w])
          
          lines(c(a[1],quant),c(w,w),lty=2,col=1)
          lines(c(quant,quant),c(w,a[3]),lty=2,col=col[i])
        }
      }
    }

    curves[[i]] <- list(x=x, y=y)
    if(datadensity!='none') {
      if(!colspec)
        dens.opts$col <- col[i]

      do.call(switch(datadensity, 
                     rug    ='scat1d', hist='histSpike',
                     density='histSpike'),
              c(list(x=xorig,add=TRUE),if(datadensity=='density')list(type='density'), dens.opts))
    }
  }

  if(nlev > 1 && (is.list(label.curves) || label.curves))
    labcurve(curves, type='s', lty=lty, col=col, opts=label.curves)

  invisible(structure(if(nlev==1)
                        list(x = x, y = y)
                      else
                        curves, 
                      N=list(n=n, m=m)))
}


ecdf.data.frame <- function(x, group=rep(1,nrows), 
                            weights=rep(1,nrows), normwt=FALSE,
                            label.curves=TRUE, n.unique=10, na.big=FALSE, 
                            subtitles=TRUE,  vnames=c("labels","names"),
                            ...)
{
  vnames <- match.arg(vnames)
  mf <- par('mfrow')
  if(length(mf)==0)
    mf <- c(1,1)

  g <- function(v, n.unique)  ## 7sep02
  {
    if(is.character(v) || is.category(v))
      return(FALSE)
    
    length(unique(v[!is.na(v)])) >= n.unique
  }
  
  use <- sapply(x, g, n.unique=n.unique)
  automf <- FALSE  ## 22sep02
  if((la <- sum(use)) > 1 & max(mf)==1) {
    mf <-
      if(la<=4)
        c(2,2)
      else if(la<=6)
        c(2,3)
      else if(la<=9)
        c(3,3)
      else if(la<=12)
        c(3,4)
      else if(la<=16)
        c(4,4)
      else
        c(4,5)
    
    automf <- TRUE
  }
  
  oldmf <- par(mfrow=mf)
  on.exit(par(oldmf))
  
  nam <- names(x)
  nrows <- nrow(x)
  i <- 0
  j <- 0

  group <- as.factor(group)
  
  for(j in (1:length(x))[use]) {
    v <- x[[j]]
    i <- i+1
    ##lab <- attr(v,"label") 26sep02
    lab <-
      if(vnames=='names')
        nam[j]
      else
        label(v, units=TRUE, plot=TRUE, default=nam[j])
    
    z <- ecdf(v, group=group, weights=weights, normwt=normwt, 
              xlab=lab, label.curves=label.curves, 
              subtitles=subtitles, ...)
    if(na.big) {
      m <- attr(z,'N')$m
      if(m > 0)
        mtext(paste(m,"NAs"),line=-2,cex=1)
    }
    
    if(automf && interactive() && 
       names(dev.list()) %nin% c('postscript','win.printer') &&
       (i %% prod(mf)==0)) {
      cat("click left mouse button to proceed\n")
      locator(1)
    }
  }
  
  invisible(ceiling(sum(use) / prod(mf)))
}


prepanel.ecdf <- function(x, y, fun, ...)
{
  xlim <- range(x,na.rm=TRUE)
  ylim <- fun(c(0,1))
  if(any(is.infinite(ylim)))
    ylim <- fun(c(.001,.999))   # was inf 18Mar02
  
  list(xlim=xlim, ylim=ylim, dx=diff(xlim), dy=diff(ylim))
}


panel.ecdf <- function(x, y, subscripts, groups=NULL, 
                       q=NULL, type='s',
                       method=c('i/n','(i-1)/(n-1)','i/(n+1)'), fun,
                       label.curves=TRUE, 
                       lwd = plot.line$lwd, 
                       lty = plot.line$lty,
                       pch = plot.symbol$pch, 
                       cex = plot.symbol$cex, 
                       font= plot.symbol$font, 
                       col = NULL, ...)
{
  ## y duplicates x in S-Plus
  method <- match.arg(method)
  if(length(groups))
    groups <- as.factor(groups)

  if(!.R.)
    llines <- lines
  
  if(.R.)
    type <- 's'   # lattice histogram sets to 'percent'

  ##g <- if(length(groups)) oldUnclass(groups[subscripts]) else NULL
  g <- oldUnclass(groups)[subscripts]
  ng <-
    if(length(groups))
      max(g, na.rm=TRUE)
    else
      1  ## na.rm 8Aug00

  plot.symbol <- trellis.par.get(if(ng>1)
                                   "superpose.symbol"
                                 else
                                   "plot.symbol")
  
  plot.line   <- trellis.par.get(if(ng>1)
                                   "superpose.line"
                                 else
                                   "plot.line")

  qrefs <- function(x, q, col, fun, llines, grid)
  {
    quant <- quantile(x, probs=q, na.rm=TRUE)  # 9Dec98
    a <- parGrid(grid)$usr
    for(i in 1:length(q)) {
      llines(c(a[1],quant[i]),fun(c(q[i],q[i])),lty=2,col=1)
      llines(c(quant[i],quant[i]),fun(c(q[i],a[3])),lty=2,col=col)
    }
  }

  ppanel <- function(x, y, type, cex, pch, font, lwd, lty, col, q, 
                     qrefs, ecdf.type, fun=fun, 
                     datadensity=c('none','rug','hist','density'), 
                     side=1, 
                     frac=switch(datadensity,
                                 none=NA,
                                 rug=.03,
                                 hist=.1,
                                 density=.1),
                     dens.opts=NULL, llines, ...)
  {
    ## y ignored
    z <- wtd.ecdf(x, type=ecdf.type, na.rm=FALSE)
    
    ## For some reason S-Plus will not plot anything the following way
    ## when lwd is a variable
    ##llines(z$x, fun(z$ecdf), lwd = lwd, lty = lty, col = col,
    ##       type = type, ...)
    do.call('llines', list(z$x, fun(z$ecdf), lwd = lwd, lty = lty, col = col,
                           type = type, ...))
    if(length(q))
      qrefs(x, q, col, fun=fun, llines=llines, grid=.R.)
    
    datadensity <- match.arg(datadensity)
    if(datadensity != 'none') {
      if(side %in% c(2,4))
        stop('side must be 1 or 3 when datadensity is specified')
      
      if('frac' %nin% names(dens.opts))
        dens.opts$frac <- frac

      if('side' %nin% names(dens.opts))
        dens.opts$side <- side

      if('col'  %nin% names(dens.opts))
        dens.opts$col  <- col

      if('lwd'  %nin% names(dens.opts))
        dens.opts$lwd  <- lwd

      do.call(switch(datadensity, 
                     rug    ='scat1d',
                     hist='histSpike',
                     density='histSpike'),
              c(list(x=x,add=TRUE,grid=.R.),
                if(datadensity=='density')
                  list(type='density'),
                dens.opts))
    }
  }

  pspanel <- function(x, subscripts, groups, type, lwd, lty,
                      pch, cex, font, col, q, qrefs, 
                      ecdf.type, fun, llines, ...)
  {
    ## y ignored
    lev <- levels(groups)
    groups <- as.numeric(groups)[subscripts]
    N <- seq(along = groups)
    ##curves <- vector('list', length(lev))             ## 19Mar02
    curves <- list()  ## 31aug02
    ##names(curves) <- lev                              ## 19Mar02 31aug02
    
    ##for(i in sort(unique(groups))) {                  ## 19Mar02
    for(i in 1:length(lev)) {
      ##if(is.na(i)) next   ## 8Aug00                 ## 19Mar02
      which <- N[groups == i]	# j <- which[order(x[which])]	
      ## sort in x
      j <- which # no sorting
      if(any(j)) {  ## 31aug02 any
        z <- wtd.ecdf(x[j], type=ecdf.type, na.rm=FALSE)
        do.call('llines',list(z$x, fun(z$ecdf),
                              col = col[i], lwd = lwd[i], lty = lty[i], 
                              type = type, ...))
        if(length(q)) qrefs(x[j], q, col[i], fun=fun, llines=llines,
                            grid=.R.)
        curves[[lev[i]]] <- list(x=z$x, y=fun(z$ecdf))  ## was [i] 31aug02
      }
    }
    
    curves
  }

  lty  <- rep(lty, length = ng)
  lwd  <- rep(lwd, length = ng)
  pch  <- rep(pch, length = ng)
  cex  <- rep(cex, length = ng)
  font <- rep(font,length = ng)
  if(!length(col))
    col <- plot.line$col

  col <- rep(col, length = ng)

  if(ng > 1) {
    levnum <- sort(unique(g))
    curves <- pspanel(x, subscripts, groups,    ## rm y 19Mar02
                      lwd=lwd, lty=lty, pch=pch, cex=cex, 
                      font=font, col=col, type=type, q=q, qrefs=qrefs, 
                      ecdf.type=method, fun=fun, llines=llines)
    if(!(is.logical(label.curves) && !label.curves)) {
      lc <-
        if(is.logical(label.curves))
          list(lwd=lwd, cex=cex[1])
        else
          c(list(lwd=lwd, cex=cex[1]), label.curves)
      ##curves <- vector('list',length(levnum)); names(curves) <- levels(groups
      ## 19Mar02
      ##i <- 0
      ##for(gg in levnum) {
      ##  i <- i+1
      ##  s <- g==gg
      ##  curves[[i]] <- list(x[s], y[s])
      ##}
      labcurve(curves, lty=lty[levnum], lwd=lwd[levnum], col=col[levnum], 
               opts=lc, grid=.R., ...)
    }
  } else ppanel(x,
                lwd=lwd, lty=lty, pch=pch, cex=cex, 
                font=font, col=col, type=type, q=q, qrefs=qrefs, 
                ecdf.type=method, fun=fun, llines=llines, ...) ## rm y 19Mar02 

  if(ng>1) { ##set up for key() if points plotted
    if(.R.) {
      Key <- function(x=0, y=1, lev, col, lty, lwd, ...)
      {
        oldpar <- par(usr=c(0,1,0,1),xpd=NA)
        
        ## Even though par('usr') shows 0,1,0,1 after lattice draws
        ## its plot, it still needs resetting
        on.exit(par(oldpar))
        if(is.list(x)) {
          y <- x[[2]]; x <- x[[1]]
        }

        if(!length(x))
          x <- 0

        if(!length(y))
          y <- 1  ## because of formals()

        rlegend(x, y, legend=lev, lty=lty, lwd=lwd, col=col)
        invisible()
      }
    } else {
      Key <- function(x=NULL, y=NULL, lev, col, lty, lwd, ...)
      {
        if(length(x)) {
          if(is.list(x)) {
            y <- x$y; x <- x$x
          }

          key(x=x, y=y, text=list(lev, col=col), 
              lines=list(col=col,lty=lty,lwd=lwd),
              transparent=TRUE, ...)
        } else key(text=list(lev, col=col), 
                   lines=list(col=col,lty=lty,lwd=lwd),transparent=TRUE, ...)
        invisible()
      }
    }
    
    formals(Key) <- list(x=NULL, y=NULL, lev=levels(groups), col=col,
                         lty=lty, lwd=lwd,...=NULL)
    storeTemp(Key)
  }
}


ecdf.formula <- function(x, data = sys.frame(sys.parent()), 
                         groups = NULL, 
                         prepanel=prepanel.ecdf, panel=panel.ecdf, ..., 
                         xlab, ylab, fun=function(x)x, subset=TRUE)
{
  if(.R.) {
    require('grid')
    require('lattice')
    vars <- var.inner(x)
    xname <- vars[1]
    if(missing(xlab))
      xlab <- label(eval(parse(text=vars[1]), data),
                    units=TRUE, plot=TRUE, default=xname, grid=TRUE)
    ##xlab <- attr(eval(parse(text=vars[1]), data),'label') 26sep02
  } else {
    vars <- attr(terms.inner(x),'variables')
    xname <- as.character(vars[1])
    if(missing(xlab))
      xlab <- label(eval(vars[1], data), units=TRUE, plot=TRUE,
                    default=xname)
    ##xlab <- attr(eval(vars[1], data),'label') 26sep02
  }
  
  if(missing(ylab)) 
    ylab <-
      if(missing(fun))
        paste('Proportion <=',xname)
      else
        ''
  
  subset <- eval(substitute(subset), data)

  if(.R.)
    do.call("histogram",
            c(list(x, data=data, prepanel=prepanel, panel=panel,
                   ylab=ylab, xlab=xlab, fun=fun),
              ## was jyst groups=groups 31aug02
              if(!missing(groups))
                list(groups=eval(substitute(groups),data)),
              if(!missing(subset))
                list(subset=subset),
              list(...)))
  else  {
    prepanel$fun <- fun
    ## argument not transmitted for some reason
    setup.2d.trellis(x, data = data,
                     prepanel=prepanel, panel=panel,
                     xlab=xlab, ylab=ylab, fun=fun,
                     groups = eval(substitute(groups),  data),
                     ..., subset = subset)
  }
}
