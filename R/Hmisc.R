## Copyright (C) 2001 Frank E Harrell Jr
##
## This program is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option) any
## later version.
##
## These functions are distributed in the hope that they will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## The text of the GNU General Public License, version 2, is available
## as http://www.gnu.org/copyleft or by writing to the Free Software
## Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
##
under.unix   <- !(version$os=='Microsoft Windows' ||
                  version$os=='Win32' || version$os=='mingw32')

.R.          <- TRUE
.SV4.        <- FALSE

.noGenenerics <- TRUE  # faster loading as new methods not used

.First.lib <- function(lib, pkg) {
  cat("Hmisc library by Frank E Harrell Jr\n\n",
      "Type library(help='Hmisc'), ?Overview, or ?Hmisc.Overview')\n",
      "to see overall documentation.\n\n",
      "Hmisc redefines [.factor to drop unused levels of factor variables\n",
      "when subscripting. To prevent this behaviour, issue the command\n",
      "options(drop.unused.levels=F).\n\n",
      sep='')
  library.dynam("Hmisc", pkg, lib)
  invisible()
}
"[.factor" <- function(x, i, drop=TRUE) {  ## was ... 4nov02
## Jens Oehlschlaegel generalized to handle drop 12Oct97
  atx <- attributes(x)
  nam <- atx$names
  atx$levels <- atx$names <- NULL
  if(missing(i)) i <- TRUE  ## 4nov02
  y <- as.integer(x)[i]     ## 4nov02
  ln <- length(nam)
  nam <- if(ln) nam[i] else NULL  ## 4nov02
  opt <- .Options$drop.factor.levels
  if(!length(opt)) opt <- .Options$drop.unused.levels
  ## !missing(drop) added 31jul02
  if(drop && (!missing(drop) || (length(opt)==0 || opt))) {
	oldClass(y) <- NULL
	j <- sort(unique(y))
	y[] <- match(y,j)
	levels(y) <- levels(x)[j]
  } else if(length(y)) levels(y) <- levels(x)
  attributes(y) <- c(attributes(y), atx, if(ln)list(names=nam))
  y
}

## Replaced with one more like default R  3nov02
## With R 1.6 was getting error with ... arguments
if(FALSE) '[.factor' <- function (x, i, drop = TRUE)
{
    y <- NextMethod("[")
    class(y) <- class(x)
    attr(y, "contrasts") <- attr(x, "contrasts")
    attr(y, "levels") <- attr(x, "levels")
    opt <- .Options$drop.factor.levels
    if(!length(opt)) opt <- .Options$drop.unused.levels
    if(drop && (!missing(drop) || (length(opt)==0 || opt)))
      reFactor(y)
    else y
}


##For compatibility with SV4
if(!exists('oldUnclass'))  oldUnclass  <- unclass
if(!exists('oldClass'))    oldClass    <- class
if(!exists('oldClass<-'))
  'oldClass<-' <- function(x, value) {
    class(x) <- value
    x
  }
if(!exists('logb'))        logb        <- log

if(!exists('existsFunction')) existsFunction <- function(...)
  exists(..., mode='function')
if(!exists('getFunction')) getFunction <- function(...)
  get(..., mode='function')

if(!exists('is.category'))
  is.category <- function(x) length(attr(x,'levels')) > 0 && mode(x)=='numeric'
# R doesn't have this

if(!exists('as.category'))
  as.category <- function(x) {
    x <- as.factor(x)
    class(x) <- NULL
    x
  }


termsDrop <- function(object, drop, data) {
  trm <- terms(object, data=data)
  if(is.numeric(drop)) {
    vars <- attr(trm, 'term.labels')
    if(any(drop > length(vars))) stop('subscript out of range')
    drop <- vars[drop]
  }
  form <- update(trm,
                 as.formula(paste('~ . ',
                                  paste('-',drop,collapse=''))))
  terms(form, data=data)
}


untangle.specials <- function (tt, special, order = 1) {
  ## From survival5
  spc <- attr(tt, "specials")[[special]]
  if (length(spc) == 0)
    return(list(vars = character(0), terms = numeric(0)))
  facs <- attr(tt, "factor")
  fname <- dimnames(facs)
  ff <- apply(facs[spc, , drop = FALSE], 2, sum)
  list(vars = (fname[[1]])[spc], terms = seq(ff)[ff & match(attr(tt,
                                   "order"), order, nomatch = 0)])
}    

var.inner <- function(formula) {
  if(!inherits(formula,"formula")) formula <- attr(formula,"formula")
  if(!length(formula)) stop('no formula object found')
	if(length(formula) > 2)
		formula[[2]] <- NULL  # remove response variable
  av <- all.vars(formula)
  ## Thanks to Thomas Lumley <tlumley@u.washington.edu> 28Jul01 :
  unique(sapply(attr(terms(formula),"term.labels"),
         function(term,av)
	av[match(all.vars(parse(text=term)),av)][1],
                av=av) )
}
#Cs <- function(...)
#{
#  if(version$major > 4) as.character(sys.call()[-1]) else {
#	y <- ((sys.frame())[["..."]])[[1]][-1]
#	unlist(lapply(y, deparse))
#  }
#}  31Mar02

Cs <- function(...)
{
  if(.SV4. || .R.) as.character(sys.call())[-1] else {
	y <- ((sys.frame())[["..."]])[[1]][-1]
	unlist(lapply(y, deparse))
  }
}
prn <- function(x, txt) {
  calltext <- as.character(sys.call())[2]

  if(!missing(txt)) {
    if(nchar(txt) + nchar(calltext) +3 > .Options$width)
      calltext <- paste('\n\n  ',calltext,sep='') else
    txt <- paste(txt, '   ', sep='')
    cat('\n', txt, calltext, '\n\n', sep='') 
  } else cat('\n',calltext,'\n\n',sep='')
  invisible(print(x))
}

format.sep <- function(x, digits, ...) {
#  .Options$digits <- digits  14Sep00
#  old <- options(digits=digits)
#  on.exit(options(old))
  y <- character(length(x))
  for(i in 1:length(x)) y[i] <- if(missing(digits)) format(x[i], ...) else
                                format(x[i],digits=digits, ...)  ## 17Apr02
  names(y) <- names(x)  ## 17Apr02
  y
}

nomiss <- function(x) 
  if(is.data.frame(x)) na.exclude(x) else
  if(is.matrix(x)) x[!is.na(x %*% rep(1,ncol(x))),] else x[!is.na(x)]


fillin <- function(v, p)        {
  v.f <- ifelse(is.na(v),p,v)
  if(length(p)==1) label(v.f) <- paste(label(v),"with",sum(is.na(v)),
			 "NAs replaced with",format(p))
	else label(v.f) <- paste(label(v),"with",sum(is.na(v)),"NAs replaced")
  v.f                          }


spearman <- function(x, y)	{
  x <- as.numeric(x); y <- as.numeric(y)  ## 17Jul97
  notna <- !is.na(x+y)	##exclude NAs
  if(sum(notna) < 3) c(rho=NA) else
  c(rho=cor(rank(x[notna]), rank(y[notna])))
}

plotCorrPrecision <- function(rho=c(0,0.5), n=seq(10,400,length=100),
                              conf.int=0.95) {
  ## Thanks to Xin Wang for computations
  curves <- vector('list', length(rho))
  names(curves) <- paste('r',format(rho),sep='=')
  zcrit <- qnorm(1-(1-conf.int)/2)
  for(i in 1:length(rho)) {
    r <- rho[i]
    z <- .5*log((1+r)/(1-r))
    lo <- z - zcrit/sqrt(n-3)
    hi <- z + zcrit/sqrt(n-3)
    rlo <- (exp(2*lo)-1)/(exp(2*lo)+1)
    rhi <- (exp(2*hi)-1)/(exp(2*hi)+1)
    precision <- pmax(rhi-r, r-rlo)
    curves[[i]] <- list(N=n, Precision=precision)
  }
  labcurve(curves, pl=TRUE, xrestrict=quantile(n,c(.25,1)), offset=.025)
  invisible()
}

trap.rule <- function(x,y) sum(diff(x)*(y[-1]+y[-length(y)]))/2

uncbind <- function(x, prefix="", suffix="")
{
  nn <- dimnames(x)[[2]]
  for(i in 1:ncol(x))
    if(.R.)  assign(paste(prefix,nn[i],suffix,sep=""), x[,i], pos=1)
    else assign(paste(prefix,nn[i],suffix,sep=""), x[,i], where=1)
  invisible()
}

## Function to pick off ordinates of a step-function at user-chosen abscissas

stepfun.eval <- function(x, y, xout, type=c("left","right")) {
  s <- !is.na(x+y)
  type <- match.arg(type)
  approx(x[s], y[s], xout=xout, method="constant", f=if(type=="left")0 else 1)$y
}

km.quick <- function(S, times, q) {
  S <- S[!is.na(S),]
  n <- nrow(S)
  stratvar <- factor(rep(1,nrow(S)))
  f <- survfit.km(stratvar, S, se.fit=FALSE, conf.type='none')
  tt <- c(0, f$time)
  ss <- c(1, f$surv)
  if(missing(times)) min(tt[ss <= q]) else
  approx(tt, ss, xout=times, method='constant', f=0)$y
}

if(FALSE) changecase <- function(string, which=c('lower','upper')) {
  which <- match.arg(which)
  .C('S_casefold', result=string, as.integer(length(string)), 
	 as.integer(which=='upper'))$result
} #superceded by casefold

#if(.R. && !existsFunction('casefold'))  # not needed - added to R 1.4
#  casefold <- function(cv, upper=FALSE)
#   if(upper) toupper(cv) else tolower(cv)

oPar <- function() {
  ## Saves existing state of par() and makes changes suitable
  ## for restoring at the end of a high-level graphics functions
  oldpar <- par()
  oldpar$fin <- NULL
  oldpar$new <- FALSE
  invisible(oldpar)
}

setParNro <- function(pars) {
  ## Sets non-read-only par parameters from the input list
  i <- names(pars) %nin%
   c('cin','cra','csi','cxy','din','xlog','ylog','gamma')
  invisible(par(pars[i]))
}

mgp.axis.labels <- function(value,type=c('xy','x','y','x and y')) {
  type <- match.arg(type)
  if(missing(value)) {
	value <- .Options$mgp.axis.labels
	pr <- par(c('mgp','las'))
	mgp <- pr$mgp
	if(!length(value)) value <- c(.7, .7)
	##value <- c(mgp[2], if(pr$las==1) max(mgp[2],1.3) else mgp[2])
	return(switch(type, 
				  xy = value, 
				  x = c(mgp[1], value[1], mgp[3]),
				  y = c(mgp[1], value[2], mgp[3]),
				  'x and y' = list(x = c(mgp[1], value[1], mgp[3]),
					               y = c(mgp[1], value[2], mgp[3]))))
  }
  if(value[1]=='default') value <- c(.7,.7)
  ##c(.6, if(par('las')==1) 1.3 else .6)
  options(mgp.axis.labels=value, TEMPORARY=FALSE)
#  mgp <- par('mgp')
#  mgp[2] <- max(value)
#  par(mgp=mgp)
  invisible()
}

mgp.axis <-
  function(side, at=NULL, ...,
           mgp=mgp.axis.labels(type=if(side==1 | side==3)'x' else 'y'),
           axistitle=NULL) {
  ## Version of axis() that uses appropriate mgp from mgp.axis.labels and
  ## gets around bug in axis(2, ...) that causes it to assume las=1
  mfrow <- par('mfrow')          ## mfrow, tcl logic 28jan03
  nr <- mfrow[1]; nc <- mfrow[2]
  w <- list(side=side)
  w <- c(w, list(...))   ## 21apr03
  if(length(at)) w$at <- at
  if(side==1 || side==3) {
    w$mgp <- mgp/nr
    if(.R.) w$tcl <- -0.4/nr
    if(side==1 && length(axistitle))
      title(xlab=axistitle, mgp=mgp/min(2.25,nr))
    } else {
      w$mgp <- mgp/nc
      if(.R.) w$tcl <- -0.4/nc
      las <- par('las')
      w$srt <- 90*(las==0)
      w$adj <- if(las==0)0.5 else 1
      if(side==2 && length(axistitle))
        title(ylab=axistitle, mgp=mgp/min(2.25,nc))
    }
  do.call('axis', w)
  invisible()
}
         
trellis.strip.blank <- function() {
 s.b <- trellis.par.get("strip.background")
 s.b$col <- 0
 trellis.par.set("strip.background", s.b)
 s.s <- trellis.par.get("strip.shingle")
 s.s$col <- 0
 trellis.par.set("strip.shingle", s.s)
 invisible()
}


lm.fit.qr.bare <- function(x, y, 
						   tolerance = if(.R.)1e-7 else .Machine$single.eps, 
						   intercept=TRUE, xpxi=FALSE) {

  if(intercept) x <- cbind(1,x)
  if(storage.mode(x) != "double") storage.mode(x) <- "double"
  if(storage.mode(y) != "double") storage.mode(y) <- "double"
  dx <- dim(x)
  dn <- dimnames(x)
  qty <- y
  n <- dx[1]
  n1 <- 1:n
  p <- dx[2]
  p1 <- 1:p
  dy <- c(n, 1)
  z <- if(!.R.) .Fortran("dqrls",
                         qr = x,
                         as.integer(dx),
                         pivot = as.integer(p1),
                         qraux = double(p),
                         y,
                         as.integer(dy),
                         coef = double(p),
                         residuals = y,
                         qt = qty,
                         tol = as.double(tolerance),
                         double(2 * p),
                         rank = as.integer(p)) else

  .Fortran("dqrls", qr = x, n = as.integer(n), p = as.integer(p),
           y = y, ny = as.integer(1),
           tol = as.double(tolerance), coef = double(p),
           residuals = y, effects = y, rank = integer(1),
           pivot = as.integer(p1),
           qraux = double(p), work = double(2 * p), PACKAGE = "base")


  coef <- z$coef
  if(length(dn[[2]])) names(coef) <- dn[[2]]
  res <- z$residuals
  sse <- sum(res^2)
  sst <- sum((y-mean(y))^2)

  res <- list(coefficients=coef, residuals=res, 
              rsquared=1-sse/sst, fitted.values=y-res)
  if(xpxi) {
    if(.R.) xpxi <- chol2inv(z$qr) else {
      R <- (z$qr)[p1, , drop = FALSE]
      R[lower.tri(R)] <- 0
      rinv <- solve(R, diag(length(coef)))
      xpxi <- rinv %*% t(rinv)
    }
    res$xpxi <- xpxi
  }
  res
}

all.is.numeric <- function(x, what=c('test','vector')) {
  what <- match.arg(what)
  old <- options(warn=-1)
  on.exit(options(old))
#  .Options$warn <- -1  6Aug00
  xs <- x[x!='' & x!=' ']
  isnum <- !any(is.na(as.numeric(xs)))
  if(what=='test') isnum else if(isnum) as.numeric(x) else x
}

Lag <- function(x, shift=1) {
  # Lags vector x shift observations, padding with NAs or blank strings
  # on the left, preserving attributes of x
  # factor vectors are converted to character strings
  if(is.factor(x)) {
    isf <- TRUE
    atr <- attributes(x)
	atr$class <- if(length(atr$class)==1) NULL else
      atr$class[atr$class!='factor']
    atr$levels <- NULL
    x <- as.character(x)
  } else isf <- FALSE
  n <- length(x)
  x <- x[1:(n-shift)]
  if(!isf) atr <- attributes(x)
  if(length(atr$label)) atr$label <- 
    paste(atr$label,'lagged',shift,'observations')
  x <- c(rep(if(is.character(x))'' else NA,shift), oldUnclass(x))
  attributes(x) <- atr
  x
}

xySortNoDupNoNA <- function(x, y) {
  if(is.list(x)) { y <- x[[2]]; x <- x[[1]] }
  s <- !is.na(x + y)
  if(any(s)) { x <- x[s]; y <- y[s] }
  i <- order(x)
  x <- x[i]
  y <- y[i]
  i <- !duplicated(x)
  list(x=x[i], y=y[i])
}

# Lifted from rowsum in 4.5
rowsumFast <- function(x, group, reorder=FALSE) {
  # assumes x is a matrix
  # by default, results are in order that unique group values
  # encountered
  # is fast and solves error that reorder= omitted from S+ 2000
  
	if(!is.numeric(x))
		stop("x must be numeric")
    dd <- dim(x)
	n <- dd[1]
	if(length(group) != n)
		stop("Incorrect length for 'group'")
	if(any(is.na(group)))
		stop("Missing values for 'group'")
	na.indicator <- max(1, x[!is.na(x)]) * n	#larger than any possible sum
	x[is.na(x)] <- na.indicator
	if(!is.numeric(group))
		group <- as.factor(group)
	storage.mode(x) <- "double"
	temp <- if(.R.) .C('R_rowsum', dd=as.integer(dd),
                       as.double(na.indicator),
                       x=x, as.double(group), PACKAGE='base') else
                       .C(if(under.unix || version$major < 4 ||
                  (version$major == 4 && version$minor < 7))
               "rowsum" else "S_rowsum",
		dd = as.integer(dd),
		as.double(na.indicator),
		x = x,
		as.double(group))
    new.n <- temp$dd[1]
	x <- temp$x[1:new.n,]
    if(reorder) {
      ugroup <- unique(group)
      dimnames(x) <- list(ugroup, dimnames(x)[[2]])
      x <- x[order(ugroup),  ]
	}
    ifelse(x == na.indicator, NA, x)
  }

outerText <- function(string, y, setAside=string[1], side=4, space=1,
                      adj=1, cex=par('cex')) {
  # Use text() to put test strings in left or right margins
  # Temporarily sets par(xpd=NA) if using R
  # For adj=1 side=4, setAside is a character string used to determine
  # the space to set aside for all strings
  # space is the number of extra characters to leave to the left of
  # the string(s) (adj=0) or to the right (adj=1)
  usr <- par('usr')
  xpd <- par('xpd')
  if(.R. && !is.na(xpd)) {
    on.exit(par(xpd=xpd))
    par(xpd=NA)
  }
  ie <- is.expression(string)  ## 1sep02
  if(ie) adj <- 0  ## adj=1 not work well for expressions in R
  
  if(side!=4) stop('only side=4 implemented')
  space <- substring('                    ',1,space)
  if(adj==0) text(usr[2], y, if(ie)string else
                      paste(space,string,sep=''),
       adj=0) else {
    usr.space.needed <- strwidth(setAside, units='user', cex=cex)
      text(usr[2]+0.5*strwidth(space, units='user', cex=cex)+usr.space.needed,
         y, string, adj=1, cex=cex) # was usr[2]- 18jul02;added 0* 25jul02
    ## was 0*strwidth(space,...) 31jan03
  }
  invisible()
}

if(FALSE) {expandUsrCoord <- function() {
  ## Expands usr coordinates of current plot to entire figure region
  ## so that out of range plots may be plotted
  pr <- par()
  usr <- pr$usr
  p <- pr$plt
  invisible(pr)
}}

if(!.R.)
  strwidth <- function(string, units=c('user','figure','inches'),
                       cex=pr$cex) {
    ## Computes width of a character string in user units or inches
    ## Approximates R strwidth function for S-Plus
    units <- match.arg(units)
    if(units=='figure') stop('units="figure" not yet implemented')
    n <- nchar(string)
    pr <- par()
    usr <- pr$usr
    cin <- pr$cin[1]
    n * cin * cex / ifelse(units=='inches',1,pr$uin[1])
  }

if(!.R.)
  strheight <- function(string, units=c('user','figure','inches'),
                       cex=pr$cex) {
    ## Computes height of a character string in user units or inches
    ## Approximates R strheight function for S-Plus
    units <- match.arg(units)
    if(units=='figure') stop('units="figure" not yet implemented')
    pr <- par()
    usr <- pr$usr
    cin <- pr$cin[2]
    cin * cex / ifelse(units=='inches',1,pr$uin[2])
  }


## Author: Patrick Connolly <P.Connolly@hortresearch.co.nz>
## HortResearch
## Mt Albert
## Auckland, New Zealand

if(.R.) print.char.matrix <-
  function (x, file = "",
            col.name.align = "cen", col.txt.align = "right", 
            cell.align = "cen", hsep = "|", vsep = "-", csep = "+",
            row.names = TRUE, col.names = FALSE,
            append = FALSE, top.border = TRUE, left.border = TRUE, ...) 
{
### To print a data frame or matrix to a text file or screen
###   and having names line up with stacked cells
###
### First, add row names as first column (might be removed later)
  ndimn <- names(dimnames(x))  ## FEH
    rownames <- dimnames(x)[[1]]
    x <- cbind(rownames, x)
  names(dimnames(x)) <- ndimn  ## FEH
  cnam <- dimnames(x)[[2]]     ## FEH
  if(length(ndimn)) cnam[1] <- ndimn[1]  ## FEH
##    dimnames(x)[[1]] <- seq(nrow(x))  25Mar02 for R  FEH
    dimnames(x) <- list(as.character(seq(nrow(x))), cnam)
    names(dimnames(x)) <- ndimn  ## 26Mar02 FEH
###  Set up some padding functions:
###
    pad.left <- function(z, pads) {
### Pads spaces to left of text
        padding <- paste(rep(" ", pads), collapse = "")
        paste(padding, z, sep = "")
    }
    pad.mid <- function(z, pads) {
### Centres text in available space
        padding.right <- paste(rep(" ", pads%/%2), collapse = "")
        padding.left <- paste(rep(" ", pads - pads%/%2), collapse = "")
        paste(padding.left, z, padding.right, sep = "")
    }
    pad.right <- function(z, pads) {
### Pads spaces to right of text
        padding <- paste(rep(" ", pads), collapse = "")
        paste(z, padding, sep = "")
    }
###  (Padding happens on the opposite side to alignment)
    pad.types <- c("left", "mid", "right")
    names(pad.types) <- c("right", "cen", "left")
    pad.name <- pad.types[col.name.align]
    pad.txt <- pad.types[col.txt.align]
    pad.cell <- pad.types[cell.align]
### Padding character columns
###    Need columns with uniform number of characters
    pad.char.col.right <- function(y) {
### For aligning text to LHS of column
        col.width <- nchar(y)
        biggest <- max(col.width)
        smallest <- min(col.width)
        padding <- biggest - col.width
        out <- NULL
        for (i in seq(y)) out[i] <- pad.right(y[i], pads = padding[i])
        out
    }
    pad.char.col.left <- function(y) {
### For aligning text to RHS of column
        col.width <- nchar(y)
        biggest <- max(col.width)
        smallest <- min(col.width)
        padding <- biggest - col.width
        out <- NULL
        for (i in seq(y)) out[i] <- pad.left(y[i], pads = padding[i])
        out
    }
    pad.char.col.mid <- function(y) {
### For aligning text to centre of column
        col.width <- nchar(y)
        biggest <- max(col.width)
        smallest <- min(col.width)
        padding <- biggest - col.width
        out <- NULL
        for (i in seq(y)) out[i] <- pad.mid(y[i], pads = padding[i])
        out
    }
### which functions to use this time.
    pad.name.fn <- get(paste("pad.", pad.name, sep = ""))
    pad.txt.fn <- get(paste("pad.char.col.", pad.txt, sep = ""))
    pad.cell.fn <- get(paste("pad.", pad.cell, sep = ""))
###
### Remove troublesome factors
    x <- as.data.frame(x)
    fac.col <- names(x)[sapply(x, is.factor)]
    for (i in fac.col) x[, i] <- I(as.character(x[, i]))
### ARE ANY LINE BREAKS IN ANY COLUMNS?
    break.list <- list()
    for (i in seq(nrow(x))) {
        x.i <- unlist(x[i, ])
        rows.i <- sapply(strsplit(unlist(x[i, ]), "\n"), length)
        rows.i[rows.i < 1] <- 1
        break.list[[i]] <- rows.i
    }
    break.row <- sapply(break.list, function(x) any(x > 1))
    names(break.row) <- seq(nrow(x))
    xx <- x
    if (any(break.row)) {
### add in extra row/s
        xx <- NULL
        reprow <- lapply(break.list, unique)
        for (k in seq(nrow(x))) {
            x.k <- unlist(x[k, ])
            x.k[x.k == ""] <- " "
            if (break.row[k]) {
                l.k <- strsplit(x.k, "\n")
                add.blanks <- max(break.list[[k]]) - break.list[[k]]
                names(l.k) <- names(add.blanks) <- seq(length(l.k))
                if (any(add.blanks > 0)) {
                  for (kk in names(add.blanks[add.blanks > 0]))
                    l.k[[kk]] <- c(l.k[[kk]], rep(" ", add.blanks[kk]))
                }
                l.k.df <- as.data.frame(l.k)
                names(l.k.df) <- names(x)
                xx <- rbind(xx, as.matrix(l.k.df))
            }
            else xx <- rbind(xx, x.k)
        }
        row.names(xx) <- paste(rep(row.names(x), sapply(reprow, 
            max)), unlist(reprow), sep = ".")
### Make an index for the rows to be printed
        rn <- row.names(xx)
        rnb <- strsplit(rn, "\\.")
        rpref <- as.numeric(factor(sapply(rnb, function(z) z[1])))
        ## was codes( ) 10oct03
    }
    else rpref <- seq(nrow(x))
    x <- as.data.frame(xx)
### Character columns need different treatment from numeric columns
    char.cols <- sapply(x, is.character)
    if (any(char.cols)) 
        x[char.cols] <- sapply(x[char.cols], pad.txt.fn)
### Change numeric columns into character
    if (any(!char.cols)) 
        x[!char.cols] <- sapply(x[!char.cols], format)
### now all character columns each of which is uniform element width
###
### Lining up names with their columns
### Sometimes the names of columns are wider than the columns they name, 
###  sometimes vice versa.
###
    names.width <- nchar(names(x))
    if (!col.names) 
        names.width <- rep(0, length(names.width))
    cell.width <- sapply(x, function(y) max(nchar(as.character(y))))
### (the width of the characters in the cells as distinct
###      from their names)  
    name.pads <- cell.width - names.width
    cell.pads <- -name.pads
    name.pads[name.pads < 0] <- 0
    cell.pads[cell.pads < 0] <- 0
    pad.names <- name.pads > 0
    pad.cells <- cell.pads > 0
### Pad out the column names if necessary:
    if (any(pad.names)) {
        stretch.names <- names(x)[pad.names]
        for (i in stretch.names) {
            names(x)[names(x) == i] <- pad.name.fn(i, name.pads[i])
        }
    }
### likewise for the cells and columns
     if (any(pad.cells)) {
        stretch.cells <- names(x)[pad.cells]
        for (j in stretch.cells) x[, j] <- pad.cell.fn(x[, j], 
            cell.pads[j])
    }
### Remove row names if not required
   if (!row.names) 
        x <- x[-1]
### Put the column names on top of matrix
    if (col.names) 
        mat2 <- rbind(names(x), as.matrix(x))
    else mat2 <- as.matrix(x)
    mat.names.width <- nchar(mat2[1, ])
### character string to separate rows
    space.h <- ""
    for (k in seq(along=mat.names.width)) {  ## added along= FEH 26Mar02
        space.h <- c(space.h, rep(vsep, mat.names.width[k]), csep)
    }
    line.sep <- paste(c(ifelse(left.border, csep, ""), space.h), 
        collapse = "")
    if (col.names) 
        rpref <- c(0, rpref, 0)
    else rpref <- c(rpref, 0)
### print to screen or file
    if (top.border) {
        write(line.sep, file = file, append = append)
        append <- TRUE
    }
    for (i in 1:nrow(mat2)) {
        if (left.border) 
            write(paste(paste(c("", mat2[i, ]), collapse = hsep), 
                hsep, sep = ""), file = file, append = append)
        else write(paste(paste(mat2[i, ], collapse = hsep), hsep, 
            sep = ""), file = file, append = append)
        append <- TRUE
### print separator if row prefix is not same as next one
        if (rpref[i] != rpref[i + 1]) 
            write(line.sep, file = file, append = TRUE)
    }
}

unPaste <- if(.R.) function(str, sep='/', extended=FALSE) {
  w <- strsplit(str, sep, extended=extended)
  w <- matrix(unlist(w), ncol=length(str))
  nr <- nrow(w)
  ans <- vector('list', nr)
  for(j in 1:nr) ans[[j]] <- w[j,]
  ans
} else function(...) unpaste(...)

get2rowHeads <- if(.R.) function(str) {
  w <- strsplit(str, '\n')
  ## strsplit returns character(0) when element=""  23may03
  list(sapply(w, function(x)if(length(x))    x[[1]] else ''),
       sapply(w, function(x)if(length(x) > 1)x[[2]] else ''))
} else function(str) {
  ## make unpaste work when field does not contain \n by adding \n at end
  backn.loc <- regexpr('\n',str)
  if(all(backn.loc < 0)) return(list(str, rep('',length(str))))
  str <- ifelse(backn.loc > 0, str, paste(str,'\n',sep=''))
  unpaste(str, '\n')
}


if(!.R.) {
  subset <- function (x, ...) UseMethod("subset")
  subset.default <- function (x, subset, ...) 
    x[subset & !is.na(subset)]

  subset.data.frame <- function (x, subset, select, ...) 
    {
      if (missing(subset)) 
        r <- TRUE
      else {
        e <- substitute(subset)
        r <- eval(e, x, if(.R.)parent.frame() else sys.parent())
        r <- r & !is.na(r)
      }
      if (missing(select)) 
        vars <- TRUE
      else {
        nl <- as.list(1:ncol(x))
        names(nl) <- names(x)
        vars <- eval(substitute(select), nl,
                     if(.R.)parent.frame() else sys.parent())
      }
      x[r, vars, drop = FALSE]
    }
  NULL
}

## Note: can't say f[vector of names] <- list(...) to update args
## In R you have to put ALL arguments in list(...) so sometimes we set
## unneeded ones to NULL.  Ignore this assignment in S
if(!.R.) {
  'formals<-' <- function(f, value) {
    nv <- names(value)
    if(any(nv %nin% names(f)))
      stop(paste('function does not have arguments',
                 paste(nv[nv %nin% names(f)],collapse=' '),
                 'to update'))
    for(a in nv) {
      v <- value[[a]]
      if(length(v)) f[[a]] <- v
    }
    f
  }
  NULL
}

## Two lists of functions, one for primitives for S+ or R (either Trellis
## or low-level), one for R grid
## Note: rect is only defined in R, not S+

ordGridFun <- function(grid) {
  if(!grid)
    list(lines    = function(...) lines(...)   ,
         points   = function(..., size=NULL) {
           if(length(size)) warning('size not implemented yet')
           points(...)}  ,
         text     = function(...) text(...)    ,
         segments = function(...) segments(...),
         arrows   = if(.R.) function(..., open, size)
                     arrows(..., length=size*.8) else
                    function(...) arrows(...),
         rect     = function(...) rect(...),
         polygon  = function(...) polygon(...),
         abline   = function(...) abline(...),
         unit     = function(x, units='native') {
           if(units!='native')
             stop('units="native" is only units implemented outside of grid')
           x},
         axis     = function(...) axis(...) ) else
  list(lines = function(x, y, ...) {
    ##    if(type!='l') warning(paste('type="',type,
    ##         '" not implemented in grid.lines',sep=''))
    if(is.list(x)) { y <- x[[2]]; x <- x[[1]] }
    llines(if(is.unit(x))convertNative(x, 'x') else x,
           if(is.unit(y))convertNative(y, 'y') else y, ...)}, 
       ##    grid.lines(x, y, default.units='native', gp=gpar(...))},
       ##       
       points = function(x, y, ...) {
         ##       function(x, y, pch=1, size=unit(1,'char'), type=NULL,
         ##  col=NULL, cex=NULL, ...) {
         if(is.list(x)) { y <- x[[2]]; x <- x[[1]] }
         lpoints(if(is.unit(x))convertNative(x, 'x') else x,
                 if(is.unit(y))convertNative(y, 'y') else y, ...)},
#      gp <- gpar(...)
#      if(length(gp)) { # grid.points fails to work if these present
#        if(length(gp$lty)) gp$lty <- NULL
#        if(length(gp$lwd)) gp$lwd <- NULL
#      }
#      prn(pch)
#      lpoints(x, y, col=col, pch=pch, cex=cex, ...)
#      grid.points(x, y, pch=pch, size=size,
#                  default.units='native', gp=gpar(...))
#      if(length(type) && type=='b')
#        grid.lines(x, y, gp=gpar(...))
#    },

       text = function(x, y, ...) {
         if(is.list(x)) {
           ##           if(missing(labels)) labels <- y
           y <- x[[2]]; x <- x[[1]]
         }
         ltext(if(is.unit(x)) convertNative(x, 'x') else x,
               if(is.unit(y)) convertNative(y, 'y') else y, ...)},
#      just <- if(adj==.5)'centre' else if(adj==0)'left' else 'right'
#      grid.text(label=labels, x, y, just=just,
#                default.units='native', gp=gpar(...)) },

       segments = function(x0, y0, x1, y1, ...) {
         grid.segments(x0, y0, x1, y1, default.units='native',
                       gp=gpar(...))},
       
       arrows = function(...) larrows(...),
#    arrows= function(x0, y0, x1, y1, length, angle, code, open, size, ...) {
#      warning('grid.arrows not yet implemented.  Using grid.segments')
#      grid.segments(x0, y0, x1, y1, gp=gpar(...))
#    },

       rect = function(xleft, ybottom, xright, ytop, density, angle,
         border, xpd, ...) {
         grid.rect(xleft, ybottom, width=xright-xleft,
                   height=ytop-ybottom, just='left',
                   default.units='native', gp=gpar(...))},
       polygon = function(x, y, col=par('col'), ...)
       grid.polygon(x, y, default.units='native', gp=gpar(fill=col,...)),
       abline=function(...) panel.abline(...),
       unit = function(x, units='native', ...) unit(x, units=units, ...),
       
       axis = function(side=1, at=NULL, labels, ticks=TRUE,
         distn, line, pos, outer, ...) {
         if(!length(at))stop('not implemented for at= unspecified')
         if(side > 2) stop('not implemented for side=3 or 4')
         if(side==1) grid.xaxis(at=at, label=labels, ticks=ticks, gp=gpar(...))
         if(side==2) grid.yaxis(at=at, label=labels, ticks=ticks, gp=gpar(...))
       }
      )
}

parGrid <- function(grid=FALSE) {
  pr <- par()
  cin <- pr$cin
  cex <- pr$cex
  lwd <- pr$lwd
  if(grid) {
    cvp <- current.viewport()
    usr <- c(cvp$xscale, cvp$yscale)
    pin <- c(cvp$cur.width.cm, cvp$cur.height.cm)/2.54
    uin <- pin/c(usr[2]-usr[1], usr[4]-usr[3])
  } else {
    usr <- pr$usr
    pin <- pr$pin
    uin <- c(pin[1]/(usr[2]-usr[1]), pin[2]/(usr[4]-usr[3]))
    ## 22Mar01 - R does not have par(uin)
  }
  list(usr=usr, pin=pin, uin=uin, cin=cin, cex=cex, lwd=lwd)
}

# Replaces R's xinch, yinch, extending them to grid
# Defines these for S-Plus
# These convert inches to data units
xInch <- function(x=1, warn.log=!grid, grid=FALSE) {
  if (warn.log && par("xlog"))
    warning("x log scale:  xInch() is nonsense")
  pr <- parGrid(grid)
  x * diff(pr$usr[1:2])/pr$pin[1]
}
yInch <- function (y = 1, warn.log=!grid, grid=FALSE) {
    if (warn.log && par("ylog"))
      warning("y log scale:  yInch is nonsense")
    pr <- parGrid(grid)
    y * diff(pr$usr[3:4])/pr$pin[2]
}


if(.R.) {
  na.include <- function(obj) {
    if(inherits(obj,'data.frame'))
      for(i in seq(along=obj)) obj[[i]] <- na.include(obj[[i]]) else {
        if(length(levels(obj)) && any(is.na(obj)))
          obj <- factor(obj,exclude=NULL)
    }
    obj
  }
  NULL
}

if(FALSE) {
  whichClosest <- function(x, w) {
  ## x: vector of reference values
  ## w: vector of values to find closest matches in x
  ## Returns: subscripts in x corresponding to w
  i <- order(x)
  x <- x[i]
  n <- length(x)
  br <- c(-1e30, x[-n]+diff(x)/2,1e30)
  m <- length(w)
  if(.R.) i[.C("bincode", as.double(w), m, as.double(br),
               length(br), code = integer(m), right = TRUE, 
               include = FALSE, NAOK = TRUE, DUP = FALSE, 
               PACKAGE = "base")$code]   else
  if(.SV4.) i[.C("S_binning3", x=as.double(w), m, as.double(br),
                 length(br), 0, 0, TRUE, TRUE)$x] else
  i[.C("S_binning2", x=as.double(w), m, as.double(br),
       length(br), 0, TRUE, TRUE)$x]
}
NULL
}

## Just as good, ties shuffled to end
## function(x, w) round(approx(x,1:length(x),xout=w,rule=2,ties='ordered')$y)
## Remove ties= for S-Plus.  Note: does not work when 2nd arg to
## approx is not uniformly spaced
## NO! ties='ordered' bombs in x not ordered
## Try
## approx(c(1,3,5,2,4,2,4),1:7,xout=c(1,3,5,2,4,2,4),rule=2,ties=function(x)x[1])
## NO: only works in general if both x and y are already ordered


## The following runs the same speed as the previous S version (in R anyway)
whichClosest <- function(x, w) {
  ## x: vector of reference values
  ## w: vector of values for which to lookup closest matches in x
  ## Returns: subscripts in x corresponding to w
  ## Assumes no NAs in x or w
  if(.R.) .Fortran("wclosest",as.double(w),as.double(x),
                   length(w),length(x),
                   j=integer(length(w)),PACKAGE="Hmisc")$j else
  .Fortran("wclosest",as.double(w),as.double(x),length(w),length(x),
           j=integer(length(w)))$j
}

whichClosePW <- function(x, w, f=0.2) {
  lx <- length(x)
  lw <- length(w)
  if(.R.) .Fortran("wclosepw",as.double(w),as.double(x),
                   as.double(runif(lw)),as.double(f),
                   lw, lx, double(lx), j=integer(lw),
                   PACKAGE="Hmisc")$j else
  .Fortran("wclosepw",as.double(w),as.double(x),
                   as.double(runif(lw)),as.double(f),
                   lw, lx, double(lx), j=integer(lw))$j
}              


if(FALSE) {
  sampWtdDist <- function(x, w) {
  ## x: vector of reference values
  ## w: vector of values to find closest matches in x
  ## Returns: subscripts in x corresponding to w

  ## 25% slower but simpler method:
  ## z <- abs(outer(w, x, "-"))
  ## s <- apply(z, 1, max)
  ## z <- (1 - sweep(z, 1, s, FUN='/')^3)^3
  ## sums <- apply(z, 1, sum)
  ## z <- sweep(z, 1, sums, FUN='/')

  lx <- length(x)
  lw <- length(w)
  z <- matrix(abs( rep( x , lw ) - rep( w, each = lx ) ),
              nrow=lw, ncol=lx, byrow=TRUE) ## Thanks: Chuck Berry
  ## s <- pmax( abs( w - min(x) ), abs( w - max(x) ) )  # to use max dist
  s <- rowSums(z)/lx/3   # use 1/3 mean dist for each row
  tricube <- function(u) (1 - pmin(u,1)^3)^3
  ## z <- (1 - (z/rep(s,length=lx*lw))^3)^3   # Thanks: Tim Hesterberg
  z <- tricube(z/s)   # Thanks: Tim Hesterberg
  sums <- rowSums(z)
  z <- z/sums 
  as.vector(rMultinom(z, 1))
}
  NULL
}

approxExtrap <- function(x, y, xout, method='linear', n=50, rule=2,
                         f=0, ties='ordered', na.rm=FALSE) {

  ## Linear interpolation using approx, with linear extrapolation
  ## beyond the data

  if(is.list(x)) { y <- x[[2]]; x <- x[[1]] }

  ## remove duplicates and order so can do linear extrapolation
  if(na.rm) {
    d <- !is.na(x+y)
    x <- x[d]; y <- y[d]
  }
  d <- !duplicated(x)
  x <- x[d]
  y <- y[d]
  d <- order(x)
  x <- x[d]
  y <- y[d]

  w <- if(.R.) approx(x, y, xout=xout, method=method, n=n,
                      rule=2, f=f, ties=ties)$y else
   approx(x, y, xout=xout, method=method, n=n, rule=2, f=f)$y
  r <- range(x)
  d <- xout < r[1]
  if(any(is.na(d))) stop('NAs not allowed in xout')
  if(any(d)) w[d] <- (y[2]-y[1])/(x[2]-x[1])*(xout[d]-x[1])+y[1]
  d <- xout > r[2]
  n <- length(y)
  if(any(d)) w[d] <- (y[n]-y[n-1])/(x[n]-x[n-1])*(xout[d]-x[n-1])+y[n-1]
  list(x=xout, y=w)
}

if(!existsFunction('reorder.factor'))
  reorder.factor <- function(x, v, FUN = mean, ...)
    ordered(x, levels(x)[order(tapply(v, x, FUN, ...))])

Names2names <- function(x) {
if(is.list(x)) {
} else {
  n <- names(attributes(x))
  if(any(n=='.Names'))names(attributes(x)) <- ifelse(n=='.Names','names',n)
}
x
}

# Use R function for S-Plus, just changed to .Options
if(!.R.) {
format.pval <-
  function (pv, digits = max(1, .Options$digits - 2),
            eps = .Machine$double.eps, 
            na.form = "NA") 
{
  if ((has.na <- any(ina <- is.na(pv)))) 
    pv <- pv[!ina]
  r <- character(length(is0 <- pv < eps))
  if (any(!is0)) {
    rr <- pv <- pv[!is0]
    expo <- floor(log10(pv))
    fixp <- expo >= -3 | (expo == -4 & digits > 1)
    if (any(fixp)) 
      rr[fixp] <- format(pv[fixp], dig = digits)
    if (any(!fixp)) 
      rr[!fixp] <- format(pv[!fixp], dig = digits)
    r[!is0] <- rr
  }
  if (any(is0)) {
    digits <- max(1, digits - 2)
    if (any(!is0)) {
      nc <- max(nchar(rr))
      if (digits > 1 && digits + 6 > nc) 
        digits <- max(1, nc - 7)
      sep <- if (digits == 1 && nc <= 6) 
        ""
      else " "
    }
    else sep <- if (digits == 1) 
      ""
    else " "
    r[is0] <- paste("<", format(eps, digits = digits), sep = sep)
  }
  if (has.na) {
    rok <- r
    r <- character(length(ina))
    r[!ina] <- rok
    r[ina] <- na.form
  }
  r
}
NULL
}

if(!existsFunction('tempdir')) {
  tempdir <- function() if(under.unix) '/tmp' else '/windows/temp'
}

#xedit <- function(file, header, title, delete.file=FALSE) {
# In R, use e.g. options(pager=xedit); page(x,'p')
#  sys(paste('xedit -title "', title, '" ', file, ' &',
#            sep=''))
#  invisible()
#}

if(FALSE) {
gless <- function(x, ...) {
# Usage: gless(x) - uses print method for x, puts in window with
# gless using name of x as file name prefixed by ~, leaves window open
  nam <- substring(deparse(substitute(x)), 1, 40)
  file <- paste('/tmp/',nam,sep='~')  #tempfile('Rpage.')
  sink(file)
#  cat(nam,'\n' )
#  if(length(attr(x,'label')) && !inherits(x,'labelled'))
#    cat(attr(x,'label'),'\n')
#  cat('\n')
  print(x, ...)
  sink()
  sys(paste('gless --geometry=600x400 "',file,'" &',sep=''))
## gless does not have a title option
  invisible()
}
NULL
}

xless <- function(x, ..., title=substring(deparse(substitute(x)),1,40)){
## Usage: xless(x) - uses print method for x, puts in persistent window with
## xless using name of x as title (unless title= is specified)
file <- tempfile()
sink(file)
print(x, ...)
sink()
cmd <- paste('xless -title "',title,'" -geometry "90x40" "',
             file,'" &',sep='')
if(.R.) system(cmd) else sys(cmd)
invisible()
}

gView <- function(x, ...,
                  title=substring(deparse(substitute(x)),1,40),
                  nup=1, fancy=TRUE, fontsize=if(nup==1)9 else 8){
## Usage: gView(x) - uses print for x, converts to ps with enscript,
##        views with gv using name of x as title (unless time=specified)
##        nup = number of columns to print per page
##        fancy controls fancy headers when nup>1
##        fontsize default is 9 (8 if nup>1)
file2 <- paste(tempdir(),title,sep='/')
file <- tempfile()
sink(file)
print(x, ...)
sink()
cmd <- if(fancy) 'enscript -G' else 'enscript'
cmd <- if(nup==1) paste(cmd, '-B -p') else
            paste(cmd, ' -',nup,' -r -j -p',sep='')
font <- paste('Courier', fontsize, sep='')
sys(paste(cmd, file2, '-f', font, '-t', title, '-b', title, file))
sys(paste('gv', file2, '&'))
invisible()
}


pasteFit <- function(x, sep=',', width=.Options$width) {
## pastes as many elements of character vector x as will fit in a line
## of width 'width', starting new lines when needed
## result is the lines of pasted text
m <- nchar(x)
out <- character(0)
cur <- ''
n   <- 0
for(i in 1:length(x)) {
  if(cur=='' | (m[i] + nchar(cur) <= width))
    cur <- paste(cur, x[i], sep=if(cur=='')'' else ',') else
  {
    out <- c(out, cur)
    cur <- x[i]
  }
}
if(cur != '') out <- c(out, cur)
out
}

isChron <- function(x) {
  cl <- class(x)  # was oldClass 22jun03
  dc <- if(.R.) c('POSIXt','POSIXct','chron') else
                c('timeDate','date','dates','times','chron')
  length(cl) && any(cl %in% dc)
}

# Note that expr may contain multiple expressions in { } but you
# cannot do assignments to objects this way
if(!.R.)
  evalq <- function(expr, envir, enclos)
            eval(substitute(expr), envir)

if(!.R.) {
download.file <- function(url, destfile, quiet=FALSE, cacheOK=TRUE, ...) {
  extra <- if (quiet) " --quiet" else ""
  if (!cacheOK)
    extra <- paste(extra, "--cache=off")
  sys(paste("wget", extra, url, "-O", destfile))
  invisible()
}
NULL
}

if(.R.) {
getHdata <-
  function(file, what=c('data','contents','description','all'),
           where='http://hesweb1.med.virginia.edu/biostat/s/data/sav',
           wherehtml='http://hesweb1.med.virginia.edu/biostat/s/data/html',
           wheredes ='http://hesweb1.med.virginia.edu/biostat/s/data/descriptions') {
    what <- match.arg(what)
    fn <- as.character(substitute(file))
    
    ads <-
      scan(paste(where,'contents.txt',sep='/'),list(''),quiet=TRUE)[[1]]
    a <- unlist(strsplit(ads,'.sav'))
    if(missing(file)) return(a)

    wds <- paste(substitute(file),'sav',sep='.')
    if(wds %nin% ads)
      stop(paste(wds,'is not on the web site.\nAvailable datasets:\n',
                 paste(a, collapse=' ')))
    if(what %in% c('contents','all')) {
      w <- paste(fn,'html',sep='.')
      browseURL(paste(wherehtml,w,sep='/'))
    }
    if(what %in% c('description','all')) {
      ades <- scan(paste(wheredes,'contents.txt',sep='/'),list(''),
                   quiet=TRUE)[[1]]
      i <- grep(paste(fn,'\\.',sep=''),ades)
      if(!length(i)) warning(paste('No description file available for',fn))
      else {
        w <- ades[i[1]]
        browseURL(paste(wheredes,w,sep='/'))
      }
    }
    if(what %nin% c('data','all')) return(invisible())
    f <- paste(where,wds,sep='/')
    tf <- tempfile()
    download.file(f, tf, mode='wb', quiet=TRUE)
    load(tf, .GlobalEnv)
    invisible()
  }
} else {
getHdata <-
  function(file,
           where='http://hesweb1.med.virginia.edu/biostat/s/data/sdd') {
    tf <- tempfile()
    download.file(paste(where,'contents.txt',sep='/'), tf, quiet=TRUE)
    ads <- scan(tf,list(''))[[1]]
    a <- sedit(ads,'.sdd','')
    if(missing(file)) return(a)
    file <- as.character(substitute(file))
    wds <- paste(file,'sdd',sep='.')
    if(wds %nin% ads)
      stop(paste(wds,'is not on the web site.\nAvailable datasets:\n',
                 paste(a, collapse=' ')))

    f <- paste(where,wds,sep='/')
    tf <- tempfile()
    download.file(f, tf, quiet=TRUE)
    data.restore(tf)  # puts in search position 1
    if(.SV4.) assign(file, cleanup.import(get(file,where=1)), where=1)
    unlink(tf)
    invisible()
  }
}


hdquantile <- function(x, probs=seq(0, 1, 0.25), se=FALSE,
                       na.rm=FALSE, names=TRUE, weights=FALSE) {
if(na.rm) {
  na <- is.na(x)
  if(any(na)) x <- x[!na]
}
x <- sort(x, na.last=TRUE)
n <- length(x)
if(n < 2) return(rep(NA, length(probs)))
m  <- n + 1

ps <- probs[probs > 0 & probs < 1]
qs <- 1 - ps

a <- outer((0:n)/n, ps,
           function(x,p,m) pbeta(x, p*m, (1-p)*m), m=m)
w <- a[-1,] - a[-m,]

r <- drop(x %*% w)
rp <- range(probs)
pp <- ps
if(rp[1]==0) { r <- c(x[1], r); pp <- c(0,pp) }
if(rp[2]==1) { r <- c(r, x[n]); pp <- c(pp,1) }
r <- r[match(pp, probs)]

if(names) names(r) <- format(probs)

if(weights) attr(r,'weights') <- structure(w, dimnames=list(NULL,format(ps)))

if(!se) return(r)
if(n < 3) stop('must have n >= 3 to get standard errors')

l <- n - 1
a <- outer((0:l)/l, ps,
           function(x,p,m) pbeta(x, p*m, (1-p)*m), m=m)
w <- a[-1,] - a[-n,]

storage.mode(x) <- 'double'
storage.mode(w) <- 'double'

nq <- length(ps)
# Get all n leave-out-one quantile estimates
S <- matrix(.Fortran("jacklins", x, w, as.integer(n), as.integer(nq),
              res=double(n*nq), PACKAGE='Hmisc')$res, ncol=nq)

se <- l * sqrt(diag(var(S))/n)

if(rp[1]==0) se <- c(NA, se)
if(rp[2]==1) se <- c(se, NA)
se <- se[match(pp,probs)]
if(names) names(se) <- names(r)
attr(r, 'se') <- se
r
}

sepUnitsTrans <- function(x, 
  conversion=c(day=1, month=365.25/12, year=365.25), round=FALSE, digits=0) {

if(!any(is.present(x))) return(x)
target <- names(conversion[conversion==1])
if(!length(target))
  stop('must specify a target unit with conversion factor=1')
lab <- attr(x,'label')
x <- ifelse(is.present(x),casefold(as.character(x)),'')

for(w in names(conversion)) {
  i <- grep(w, x)
  if(length(i)) x[i] <-
    as.character(as.numeric(gsub(paste(w,'s*',sep=''), '', x[i]))*
                 conversion[w])
}

i <- grep('[a-z]', x)
if(any(i))
  warning(paste('variable contains units of measurement not in',
                paste(names(conversion), collapse=','),':',
                paste(unique(x[i]),collapse=' ')))
x <- as.numeric(x)
if(round) x <- round(x, digits)
units(x) <- target
if(length(lab)) label(x) <- lab
x
}
abs.error.pred <- function(fit, lp=NULL, y=NULL) {
  if(!length(y))  y  <- fit$y
  if(!length(lp)) lp <- fit$fitted.values
  if(!length(lp)) lp <- fit$linear.predictors
  if(!(length(y) && length(lp)))
	stop('must specify lp and y or specify y=T in the fit')
  s <- is.na(y + lp)
  if(any(s)) {
	y  <- y[!s]
	lp <- lp[!s]
  }
  my    <- median(y)
  mlp   <- median(lp)
  meanr <- mean(  abs( lp - mlp))
  meant <- mean(  abs(  y - my ))
  meane <- mean(  abs( lp -  y ))
  medr  <- median(abs( lp - mlp))
  medt  <- median(abs(  y - my ))
  mede  <- median(abs( lp -  y ))

  differences <- cbind(c(meanr,meane,meant),
					   c(medr ,mede ,medt ) )
  dimnames(differences) <- list(c('|Yi hat - median(Y hat)|',
								  '|Yi hat - Yi|',
								  '|Yi - median(Y)|'),
								c('Mean','Median'))
  ratios <- cbind(c(meanr/meant, meane/meant),
				  c( medr/ medt,  mede/ medt))
  dimnames(ratios) <- list(c('|Yi hat - median(Y hat)|/|Yi - median(Y)|',
							 '|Yi hat - Yi|/|Yi - median(Y)|'),
						   c('Mean','Median'))
  structure(list(differences=differences,ratios=ratios),class='abs.error.pred')
}

print.abs.error.pred <- function(x, ...) {
  cat('\nMean/Median |Differences|\n\n')
  print(x$differences)
  cat('\n\nRatios of Mean/Median |Differences|\n\n')
  print(x$ratios)
  invisible()
}
						   
aregImpute <- function(formula, data, subset, n.impute=5,
                       group=NULL, method=c('ace','avas'),
                       match=c('weighted','closest'), fweighted=0.2,
                       defaultLinear=FALSE, x=FALSE, pr=TRUE) {
  
  acall   <- match.call()
  method  <- match.arg(method)
  match   <- match.arg(match)
  if(.R.) require('acepack')  # provides ace, avas

## Temporarily fix bug in ace
  if(.R.) {
ace <- function (x, y, wt = rep(1, nrow(x)), cat = NULL, mon = NULL, 
    lin = NULL, circ = NULL, delrsq = 0.01) 
{
    x <- as.matrix(x)
    if (delrsq <= 0) {
        cat("delrsq must be positive")
        return()
    }
    iy <- ncol(x) + 1
    l <- matrix(1, ncol = iy)
    if (!is.null(circ)) {
        for (i in 1:length(circ)) {
            if (circ[i] < 0 || circ[i] > ncol(x)) {  # FEH nrow -> ncol
                cat("bad circ= specification")
                return()
            }
            if (circ[i] == 0) {
                cat("response spec can only be lin or ordered (default)")
                return()
            }
            else {
                nncol <- circ[i]
                if (l[nncol] != 2 & l[nncol] != 1) {
                  cat("conflicting transformation specifications")
                  return()
                }
                l[nncol] <- 2
            }
        }
    }
    if (length(mon)) {
        for (i in 1:length(mon)) {
            if (mon[i] < 0 || mon[i] > ncol(x)) {  # FEH nrow -> ncol
                cat("bad mon= specification")
                return()
            }
            if (mon[i] == 0) {
                cat("response spec can only be lin or ordered (default)")
                return()
            }
            else {
                nncol <- mon[i]
                if (l[nncol] != 3 && l[nncol] != 1) {
                  cat("conflicting transformation specifications")
                  return()
                }
                l[nncol] <- 3
            }
        }
    }
    if (length(lin)) {
        for (i in 1:length(lin)) {
            if (lin[i] < 0 || lin[i] > ncol(x)) {  # FEH nrow -> ncol
                cat("bad lin= specification")
                return()
            }
            if (lin[i] == 0) {
                nncol <- iy
            }
            else {
                nncol <- lin[i]
            }
            if (l[nncol] != 4 && l[nncol] != 1) {
                cat("conflicting transformation specifications")
                return()
            }
            l[nncol] <- 4
        }
    }
    if (length(cat)) {
        for (i in 1:length(cat)) {
            if (cat[i] < 0 || cat[i] > ncol(x)) {  # FEH nrow -> ncol
                cat("bad cat= specification")
                return()
            }
            if (cat[i] == 0) {
#  Next 2 lines commented out FEH
#                cat("response spec can only be lin or ordered (default)")
#                return()
            }
            else {
                nncol <- cat[i]
                if (l[nncol] != 4 && l[nncol] != 1) {
                  cat("conflicting transformation specifications")
                  return()
                }
                l[nncol] <- 4
            }
        }
    }
    tx <- x
    ty <- y
    m <- matrix(0, nrow = nrow(x), ncol = iy)
    z <- matrix(0, nrow = nrow(x), ncol = 12)
    z <- as.matrix(z)
    ns <- 1
    mode(x) <- "double"
    mode(y) <- "double"
    mode(tx) <- "double"
    mode(ty) <- "double"
    mode(wt) <- "double"
    mode(delrsq) <- "double"
    mode(z) <- "double"
    junk <- .Fortran("mace", p = as.integer(ncol(x)), n = as.integer(nrow(x)), 
        x = t(x), y = y, w = as.double(wt), l = as.integer(l), 
        delrsq = delrsq, ns = as.integer(ns), tx = tx, ty = ty, 
        rsq = double(1), ierr = integer(1), m = as.integer(m), 
        z = z, PACKAGE = "acepack")
    return(junk)
}
}
  
  if(!inherits(formula,'formula')) stop('formula must be a formula')
  nam <- var.inner(formula)

  m <- match.call(expand = FALSE)
  Terms <- terms(formula, specials=c('I','monotone'))
  m$formula <- formula
  m$match <- m$fweighted <- m$x <- m$n.impute <- m$defaultLinear <-
  m$group <- m$pr <- m$... <- NULL
  m$na.action <- na.retain

  m[[1]] <- as.name("model.frame")
  z <- eval(m, sys.parent())
  p <- length(z)
  n <- nrow(z)
  rnam <- row.names(z)
  if(length(rnam)==0) rnam <- as.character(1:n)

  lgroup <- length(group)
  if(lgroup) {
    if(lgroup != n)
      stop('group should have length equal to number of observations')
    ngroup <- length(unique(group[!is.na(group)]))
  }

  linear <- nam[attr(Terms,'specials')$I]
  mono <- nam[attr(Terms,'specials')$monotone]

  cat.levels <- vector('list',p)
  names(cat.levels) <- nam
  categorical <- character(0)
  na <- vector('list',p)
  names(na) <- nam
  nna <- integer(p); names(nna) <- nam

  xf <- matrix(as.double(1), nrow=n, ncol=p, dimnames=list(rnam,nam))
  imp <- vector('list',p)
  names(imp) <- nam
  if(lgroup) group.inds <- imp

  for(i in 1:p) {
	  xi <- z[[i]]
      ni <- nam[i]
      nai <- is.na(xi)
      na[[i]] <- (1:n)[nai] 
      nna[i] <- nnai <- sum(nai)
      if(nnai > 0) imp[[ni]] <-  matrix(NA, nrow=nnai, ncol=n.impute,
                                        dimnames=list(rnam[nai],NULL))
      if(lgroup) {
        if(any(is.na(group[!nai]))) stop('NAs not allowed in group')
        if(length(unique(group[!nai])) != ngroup)
          stop(paste('not all',ngroup,
                     'values of group are represented in\n',
                     'observations with non-missing values of',
                     ni))
        group.inds[[i]] <- split((1:n)[!nai], group[!nai])
      }
  
      iscat <- FALSE
	  if(is.character(xi)) {
		xi <- as.factor(xi)
        lev <- levels(xi)
        iscat <- TRUE
      } else if(is.category(xi)) {
        lev <- levels(xi)
        iscat <- TRUE
      }
      if(iscat) {
        cat.levels[[ni]] <- lev
        xi <- as.integer(xi)
        categorical <- c(categorical,ni)
      } else {
        u <- unique(xi[!nai])
        if(length(u) == 1) stop(paste(ni,'is constant')) else
         if((defaultLinear || length(u) == 2) &&
            ni %nin% linear) linear <- c(linear, ni)
      }
      xf[,i] <- xi
      ## Initialize imputed values to random sample of non-missings
      if(nnai > 0) xf[nai,i] <-
        sample(xi[!nai], nnai, replace=nnai > (n-nnai))
    }
  z <- NULL
  wna <- (1:p)[nna > 0]
  ## xf = original data matrix (categorical var -> integer codes)
  ## with current imputations
  rsq <- double(length(wna)); names(rsq) <- nam[wna]
  
  if(pr) cat('Iteration:')
  for(iter in 1:(3+n.impute)) {
    if(pr) cat(iter,'')
    for(i in wna) {
      nai <- na[[i]]
      j <- (1:n)[-nai]    ## subscripts of non-NAs on xf[i,]
      npr <- length(j)
      if(lgroup) {        ## insure orig. no. obs from each level of group
        s <- rep(NA, npr)
        for(ji in 1:ngroup) {
          gi <- (group.inds[[i]])[[ji]]
          s[gi] <- sample(gi, length(gi), replace=TRUE)
        }
      } else s <- sample(j, npr, replace=TRUE)  ## sample of non-NAs
      nami <- nam[i]
      nm <- c(nami, nam[-i])

      X <- xf[,-i,drop=FALSE]
      w <- list(x=X[s,], y=xf[s,i])
      if(length(mono))        w$mono <- match(mono, nm) - 1
      if(length(categorical)) w$cat  <- match(categorical, nm) - 1
      if(length(linear))      w$lin  <- match(linear, nm) - 1
      f <- do.call(if(method=='ace' || nami %in% categorical)
                   'ace' else 'avas', w)
      ## avas does not handle categorical response variables
      cof <- lm.fit.qr.bare(f$tx, f$ty)$coef
      rsq[nami] <- f$rsq
      ## fitter does not automatically make coefficients=1
      pti <- cof[1]    ## predicted transformed xf[,i]
      for(k in 1:(p-1)) {
        ## Transform each RHS variable, all original obs.
        if(length(unique(X[s,k]))==1) {
          cat('\n\n')
          print(table(X[,k]))
          stop(paste('Variable', dimnames(X)[[2]][k],
                     '\nhas only one unique value in a bootstrap sample.\n',
                     'See above for overall frequency distribution.'))
        }
        tk <- if(TRUE || .R.) approxExtrap(X[s,k], f$tx[,k], xout=X[,k])$y else
                      approx(X[s,k], f$tx[,k], xout=X[,k], rule=3)$y
        ## Bug in approx with rule=3 resulting in NA for 6.0 Linux
        pti <- pti + cof[k+1]*tk
      }
      whichclose <- if(match=='closest') {
        ## Jitter predicted transformed values for non-NAs to randomly
        ## break ties in matching with predictions for NAs in xf[,i]
        ## Becuase of normalization used by fitter, pti usually ranges from
        ## about -4 to 4
        pti[j] <- pti[j] + runif(npr,-.0001,.0001)
        ## For each orig. missing xf[,i] impute with non-missing xf[,i] that
        ## has closest predicted transformed value
        j[whichClosest(pti[j], pti[nai])]  ## see Misc.s
      } else j[whichClosePW(pti[j], pti[nai], f=fweighted)]
      impi <- xf[whichclose,i]
      xf[nai,i] <- impi
      if(iter > 3) imp[[nam[i]]][,iter-3] <- impi
    }
  }
  if(pr) cat('\n')

if(!x) xf <- NULL
structure(list(call=acall, formula=formula,
               method=method, match=match, fweighted=fweighted,
			   n=n, p=p, na=na, nna=nna,
			   linear=linear, categorical=categorical, monotone=mono,
			   cat.levels=cat.levels,
			   n.impute=n.impute, imputed=imp, x=xf, rsq=rsq),
          class='aregImpute')
}

print.aregImpute <- function(x, ...) {
  cat("\nMultiple Imputation using Bootstrap and PMM\n\n")
  dput(x$call)
  cat("\n")
  cat('\nMethod:',x$method,'\tn=',x$n,'\tp=',x$p,
      '\tImputations:',x$n.impute,'\n')
  cat('\nNumber of NAs:\n'); print(x$nna)
  if(length(x$linear))      cat('\nLinear:\t',x$linear,'\n')
  if(length(x$categorical)) cat('\nCategorical:\t',x$categorical,'\n')
  if(length(x$monotone))    cat('\nMonotonic:\t',  x$monotone,'\n')
  cat('\nR-squares for Predicting Non-Missing Values for Each Variable\nUsing Last Imputations of Predictors\n')
  print(round(x$rsq,3))
  invisible()
}

plot.aregImpute <- function(x, nclass=NULL, type=c('ecdf','hist'),
                            diagnostics=FALSE, maxn=10, ...) {
  type <- match.arg(type)
  i <- x$imputed
  catg <- x$categorical
  lev  <- x$cat.levels
  n.impute <- x$n.impute
  for(n in names(i)) {
    xi <- i[[n]]
    if(!length(xi)) next
    if(diagnostics) {
      r <- range(xi)
      cat(min(maxn,nrow(xi)))
      for(j in 1:min(maxn,nrow(xi))) {
        plot(1:n.impute, xi[j,], ylim=r, xlab='Imputation',
             ylab=paste("Imputations for Obs.",j,"of",n))
      }
    }
    ix <- as.vector(i[[n]])
    lab <- paste('Imputed',n)
    if(n %in% catg) {
      tab <- table(ix)
      mar <- par('mar')
      dotchart2(tab, lev[[n]], auxdata=tab, xlab='Frequency',
                ylab=lab)
      par(mar=mar)
    } else {
      if(type=='ecdf')
        ecdf(ix, xlab=lab, datadensity='hist', subtitles=FALSE) else {
        if(length(nclass)) hist(ix, xlab=n, nclass=nclass, main='') else
        hist(ix, xlab=lab, main='')
        scat1d(ix)
      }
    }
  }
  invisible()
}


as.data.frame.Surv <- function(x, ...) {
  rown <- if(length(dx1 <- dimnames(x)[[1]])) dx1 else 
	  as.character(1:nrow(x))
  ## Added names= 18Sep01
  structure(list(x), class="data.frame", names=deparse(substitute(x)),
            row.names=rown)
}
binconf <- function(x, n, alpha = 0.05,
                    method = c("wilson","exact","asymptotic","all"),
                    include.x = FALSE, include.n = FALSE, 
                    return.df = FALSE)
{
# ..modifications for printing and the addition of a 
#   method argument and the asymptotic interval
#   and to accept vector arguments were
#   made by Brad Biggerstaff on 10 June 1999
#
	method <- match.arg(method)
	bc <- function(x, n, alpha, method)
	{
		nu1 <- 2 * (n - x + 1)
		nu2 <- 2 * x
		ll <- if(x > 0) x/(x + qf(1 - alpha/2, nu1, nu2) * (n - x + 1))
			 else 0
		nu1p <- nu2 + 2
		nu2p <- nu1 - 2
		pp <- if(x < n) qf(1 - alpha/2, nu1p, nu2p) else 1
		ul <- ((x + 1) * pp)/(n - x + (x + 1) * pp)
		zcrit <-  - qnorm(alpha/2)
		z2 <- zcrit * zcrit
		p <- x/n
		cl <- (p + z2/2/n + c(-1, 1) * zcrit * sqrt((p * (1 - p) + z2/4/
			n)/n))/(1 + z2/n)
		if(x == 1)
			cl[1] <-  - log(1 - alpha)/n
		if(x == (n - 1))
			cl[2] <- 1 + log(1 - alpha)/n
		asymp.lcl <- x/n - qnorm(1 - alpha/2) * sqrt(((x/n) * (1 - x/n)
			)/n)
		asymp.ucl <- x/n + qnorm(1 - alpha/2) * sqrt(((x/n) * (1 - x/n)
			)/n)
		res <- rbind(c(ll, ul), cl, c(asymp.lcl, asymp.ucl))
		res <- cbind(rep(x/n, 3), res)	
	#dimnames(res) <- list(c("Exact", "Wilson", "Asymptotic"), c(
# "Point Estimate", "Lower", "Upper"))
		switch(method,
			wilson =     res[2,  ],
			exact =      res[1,  ],
			asymptotic = res[3,  ],
			all =        res,
			res)
	}

	if((length(x) != length(n)) & length(x) == 1)
		x <- rep(x, length(n))
	if((length(x) != length(n)) & length(n) == 1)
		n <- rep(n, length(x))
	if((length(x) > 1 | length(n) > 1) & method == "all") {
		method <- "wilson"
		warning("method=all will not work with vectors...setting method to wilson"
			)
	}
	if(method == "all" & length(x) == 1 & length(n) == 1) {
		mat <- bc(x, n, alpha, method)
		dimnames(mat) <- list(c("Exact", "Wilson", "Asymptotic"), c(
			"PointEst", "Lower", "Upper"))
		if(include.n)
			mat <- cbind(N = n, mat)
		if(include.x)
			mat <- cbind(X = x, mat)
		if(return.df)
			mat <- as.data.frame(mat)
		return(mat)
	}
	mat <- matrix(ncol = 3, nrow = length(x))
	for(i in 1:length(x))
		mat[i,  ] <- bc(x[i], n[i], alpha = alpha, method = method)
	dimnames(mat) <- list(rep("", dim(mat)[1]), c("PointEst", "Lower", 
		"Upper"))
	if(include.n)
		mat <- cbind(N = n, mat)
	if(include.x)
		mat <- cbind(X = x, mat)
	if(return.df)
		mat <- as.data.frame(mat)
	mat
}
bootkm <- function(S, q=.5, B=500, times, pr=TRUE) {
  tthere <- !missing(times)
  if(tthere && length(times)>1)
	stop('presently bootkm only works for a single time')
  S <- S[!is.na(S),]
  n <- nrow(S)
  stratvar <- factor(rep(1,nrow(S)))
  f <- survfit.km(stratvar, S)
  tt <- c(0, f$time)
  ss <- c(1, f$surv)
  if(!tthere) {
  if(ss[length(ss)] > q) 
	stop(paste('overall Kaplan-Meier estimate does not fall below',q))
} else {
  if(tt[length(tt)] < times)
	stop(paste('overall Kaplan-Meier estimate not defined to time',times))
}

  ests <- if(.R.)double(B) else single(B)

  for(i in 1:B) {
	if(pr && (i %% 10)==0) cat(i,'')
	f <- survfit.km(stratvar, S[sample(n,n,replace=TRUE),],
					se.fit=FALSE, conf.type='none')
	tt <- c(0, f$time)
	ss <- c(1, f$surv)
	ests[i] <- if(tthere)
	  approx(tt, ss, xout=times, method='constant', f=0)$y else
	  min(tt[ss <= q])  #is NA if none
  }
if(pr) cat('\n')
ests
}

bpower <- function(p1, p2, odds.ratio, percent.reduction, n, n1, n2, 
                   alpha=.05)
{
if(!missing(odds.ratio)) p2 <- p1*odds.ratio/(1-p1+p1*odds.ratio)
else if(!missing(percent.reduction)) p2 <- p1*(1-percent.reduction/100)

if(!missing(n)) { n1 <- n2 <- n/2 }
z <- qnorm(1-alpha/2)
q1 <- 1-p1
q2 <- 1-p2
pm <- (n1*p1+n2*p2)/(n1+n2)
ds <- z*sqrt((1/n1 + 1/n2)*pm*(1-pm))
ex <- abs(p1-p2)
sd <- sqrt(p1*q1/n1+p2*q2/n2)
c(Power = 1-pnorm((ds-ex)/sd)+pnorm((-ds-ex)/sd) )
}


bsamsize <- function(p1, p2, fraction=.5, alpha=.05, power=.8) {

z.alpha <- qnorm(1-alpha/2)
z.beta  <- qnorm(power)

ratio <- (1-fraction)/fraction
p <- fraction*p1+(1-fraction)*p2

n1 <- (z.alpha*sqrt((ratio+1)*p*(1-p))+z.beta*sqrt(ratio*p1*(1-p1)+
	p2*(1-p2)))^2/ratio/((p1-p2)^2)
n2 <- ratio*n1
c(n1=n1, n2=n2)
}

ballocation <- function(p1, p2, n, alpha=.05) {
q1 <- 1-p1
q2 <- 1-p2

f.minvar.diff <- 1/(1+sqrt(p2*q2/(p1*q1)))
f.minvar.ratio <- 1/(1+sqrt(p1*q2/p2/q1))

z <- c(fraction.group1.min.var.diff=f.minvar.diff,
       fraction.group1.min.var.ratio=f.minvar.ratio,
       fraction.group1.min.var.logodds=1-f.minvar.diff)

if(!missing(n)) {
  possf <- seq(.001,.999,length=1000)
  pow <- bpower(p1, p2, n1=n*possf, n2=n*(1-possf), alpha=alpha)
#  fun <- function(f, n, p1, p2, alpha) bpower(p1, p2, n1=f*n, n2=(1-f)*n, alpha=alpha)
#  f.maxpow <- optimize(fun, lower=.01, upper=.99, maximum=T,
#                       n=n, p1=p1, p2=p2, alpha=alpha)$maximum
  f <- possf[pow==max(pow)]
  f <- f[abs(f-.5)==min(abs(f-.5))]
  z <- c(z, fraction.group1.max.power=f[1])
}
z
}

bpower.sim <- function(p1, p2, odds.ratio, percent.reduction, n, n1, n2, 
                       alpha=.05, nsim=10000) {

if(!missing(odds.ratio)) p2 <- p1*odds.ratio/(1-p1+p1*odds.ratio)
else if(!missing(percent.reduction)) p2 <- p1*(1-percent.reduction/100)

if(!missing(n)) { n1 <- n2 <- round(n/2) }
n <- n1+n2

if(length(p1)+length(p2)+length(n1)+length(n2)+length(alpha)+length(nsim)!=6)
	stop('all arguments must have length 1')

chi2 <- qchisq(1-alpha, 1)

d1 <- rbinom(nsim, n1, p1)
d2 <- rbinom(nsim, n2, p2)
chisq <- n*(d1*(n2-d2)-(n1-d1)*d2)^2/(d1+d2)/(n-d1-d2)/n1/n2
power <- mean(chisq>chi2)
se <- sqrt(power*(1-power)/nsim)
c(Power=power,Lower=power-1.96*se,Upper=power+1.96*se)
}
##Modified FEH 30Jun97 - delete missing data, names default to T,
## auto names for list argument, ylab default to "" instead of Percentiles
## names -> name, added srtx
bpplot <- function(..., name = TRUE,
				   main = "Box-Percentile Plot", 
				   xlab = "", ylab = "", srtx=0) {
  all.x <- list(...)  ## FH 30Jun97
  nam <- character(0)   ## FH
##  if(is.list(...)) {  ## FH
  if(is.list(all.x[[1]])) {
	all.x <- all.x[[1]]
	if(is.logical(name) && name) name <- names(...)   ## FH
  }
  n <- length(all.x)
  centers <- seq(from = 0, by = 1.2, length = n)
  ymax <- max(sapply(all.x, max, na.rm=TRUE))  ## na.rm=T FEH
  ymin <- min(sapply(all.x, min, na.rm=TRUE))
  xmax <- max(centers) + 0.5
  xmin <- -0.5
  plot(c(xmin, xmax), c(ymin, ymax), type = "n", main = main,
       xlab = '', ylab = ylab, xaxt = "n")
  for(i in 1:n) {
	plot.values <- bpx(all.x[[i]], centers[i])
	lines(plot.values$x1, plot.values$y1)
	lines(plot.values$x2, plot.values$y2)
	lines(plot.values$q1.x, plot.values$q1.y)
	lines(plot.values$q3.x, plot.values$q3.y)
	lines(plot.values$med.x, plot.values$med.y)
  }

  if(is.logical(name)) {
	if(name)
	  mgp.axis(1, centers, 
		   sapply(substitute(list(...)), deparse)[2:(n + 1)],
				srt=srtx, adj=if(srtx==0).5 else 1,
               axistitle=xlab)
  }
	else mgp.axis(1, centers, name, srt=srtx, adj=if(srtx==0).5 else 1,
                  axistitle=xlab)
  invisible(centers)
}
bpx <- function(y, offset)
{
  y <- y[!is.na(y)]   ## FEH 30Jun97
  n <- length(y)
  delta <- 1/(n + 1)
  prob <- seq(delta, 1 - delta, delta)
  quan <- sort(y)
  med <- median(y)
  q1 <- median(y[y < med])
  q3 <- median(y[y > med])
  first.half.p <- prob[quan <= med]
  second.half.p <- 1 - prob[quan > med]
  plotx <- c(first.half.p, second.half.p)	###
###
  ## calculating the ends of the first quartile line
###
  qx <- approx(quan, plotx, xout = q1)$y
  q1.x <- c( - qx, qx) + offset	###
###
  ## calculating the ends of the third quartile line
###
  qx <- approx(quan, plotx, xout = q3)$y
  q3.x <- c( - qx, qx) + offset
  q1.y <- c(q1, q1)
  q3.y <- c(q3, q3)
  med.x <- c( - max(first.half.p), max(first.half.p)) + offset
  med.y <- c(med, med)
  return(list(x1 = ( - plotx) + offset, y1 = quan, x2 = plotx + offset,
			  y2 = quan, q1.y = q1.y, q1.x = q1.x, q3.y = q3.y, q3.x = q3.x,
			  med.y = med.y, med.x = med.x))
}
bystats <- function(y, ..., fun, nmiss, subset)				{

   x <- interaction(..., drop=TRUE, sep=" ", left=TRUE)
   l <- levels(x)
   if(any(is.na(x)))	{
      l <- c(l, "NA")
      attr(x,"class") <- NULL
      x[is.na(x)] <- length(l)
      levels(x) <- l
      attr(x,'class') <- "factor"
			}
   y <- as.matrix(y)
   if(!missing(subset)) { 
     x <- x[subset]
     y <- y[subset,,drop=FALSE]
   }

   if(missing(fun))
   {
	fun <- function(y) apply(y, 2, mean)
	r <- range(y, na.rm=TRUE)
	uy <- unique(y[!is.na(y)])  #fixed 1Jun95, 16Mar96
	funlab <- if(length(uy)==2 && r[1]==0 & r[2]==1) "Fraction" else "Mean"
   }
   else
   {
	funlab <- as.character(substitute(fun))
	funlab <- funlab[length(funlab)] #handles fun=function(x)mean(x)
	if(length(chf <- as.character(fun[[2]]))>3 && chf[1]=="apply")
	   funlab <- chf[4]
       #The preceeding gets "median" from function(y) apply(y, 2, median)
#	if(length(fun)==2 && length(fun[[2]])>1) funlab <- ""
   }
   lab <- as.character(sys.call())[-1]
   m <- (!missing(fun)) + (!missing(nmiss)) + (!missing(subset))
   lab <- lab[1:(length(lab)-m)]
   if(length(lab)>2) lab2 <- paste(lab[-1],collapse=", ")
   else lab2 <- lab[-1]
   heading <- if(funlab=="") paste(lab[1],"by",lab2) else
	paste(funlab,"of",lab[1],"by",lab2)

   nna <- !is.na(y %*% rep(1,ncol(y)))
   N <- sum(nna)
   stats <- fun(y[nna,,drop=FALSE])
   nstats <- length(stats)
   name.stats <- if(length(dn <- dimnames(stats))) 
	as.vector(outer(dn[[1]],dn[[2]],FUN=function(a,b)paste(b,a))) else 
	names(stats)
   if(length(name.stats)) funlab <- name.stats
   if(nstats>1 && length(name.stats)==0) funlab <- rep(" ", nstats)
   s <- matrix(NA,nrow=length(l)+1,ncol=2+nstats,dimnames=list(c(l,"ALL"),
	c("N","Missing",funlab)))
   j <- 0
   for(i in l)		{
      j <- j+1
      w <- y[x==i,,drop=FALSE]
      nna <- !is.na(w %*% rep(1,ncol(w)))
      n <- sum(nna)
      s[j,] <- c(n, nrow(w)-n, 
                 if(n)fun(w[nna,,drop=FALSE]) else rep(NA,nstats))
			}
   s[j+1,] <- c(N, nrow(y)-N, stats)
   if((!missing(nmiss) && !nmiss) || (missing(nmiss) && all(s[,"Missing"]==0)))
	 s <- s[,-2]
   attr(s, "heading")    <- heading
   attr(s, "byvarnames") <- lab2
   attr(s,'class')       <- "bystats"
   s
						}

print.bystats <- function(x, ...) {
cat("\n",attr(x,"heading"),"\n\n")
attr(x,"heading") <- NULL
attr(x,"byvarnames") <- NULL
attr(x,'class') <- NULL
invisible(print(x, ...))
}

latex.bystats <- function(object,
                          title=first.word(expr=substitute(object)),
                          caption=attr(object,"heading"),
			rowlabel=attr(object,"byvarnames"), ...) {
   dm <- dimnames(object)
#   inn <- c("%","<=","<",">=",">","\\[")
#   out <- c("\\\\%","$\\\\leq$","$<$","$\\\\geq$","$>$","\\\\verb|[|")
#   dm[[1]] <- translate(dm[[1]],inn,out)
#   dm[[2]] <- translate(dm[[2]],inn,out)
   inn <- c("%","<=","<",">=",">","[")
   out <- c("\\%","$\\leq$","$<$","$\\geq$","$>$","\\verb|[|")
   dimnames(object) <- dm
   caption <- sedit(caption, "cbind", "")
   latex(oldUnclass(object), title=title, caption=caption, rowlabel=rowlabel, 
	 n.rgroup=c(nrow(object)-1,1), ...)
}


bystats2 <- function(y, v, h, fun, nmiss, subset)				{
   y <- as.matrix(y)
   if(!missing(subset)) { y <- y[subset,,drop=FALSE]; v <- v[subset]; h <- h[subset] }
   v <- factor(v, exclude=NULL)
   h <- factor(h, exclude=NULL)

   lv <- levels(v)
   lh <- levels(h)
   nv <- length(lv)
   nh <- length(lh)

   if(missing(fun))
   {
	fun <- function(y) apply(y, 2, mean)
	r <- range(y, na.rm=TRUE)
	funlab <- if(length(r)==2 && r[1]==0 & r[2]==1) "Fraction" else "Mean"
   }
   else
   {
	funlab <- as.character(substitute(fun))
	funlab <- funlab[length(funlab)] #handles fun=function(x)mean(x)
	if(length(chf <- as.character(fun[[2]]))>3 && chf[1]=="apply")
	   funlab <- chf[4]
       #The preceeding gets "median" from function(y) apply(y, 2, median)

   }
   lab <- as.character(sys.call())[-1]
   m <- (!missing(fun)) + (!missing(nmiss)) + (!missing(subset))
   lab <- lab[1:(length(lab)-m)]
   if(length(lab)>2) lab2 <- paste(lab[-1],collapse=", ")
   else lab2 <- lab[-1]
   heading <- if(funlab=="") paste(lab[1],"by",lab2) else
	paste(funlab,"of",lab[1],"by",lab2)

   nna <- !is.na(y %*% rep(1,ncol(y)))
   N <- sum(nna)
   stats <- fun(y[nna,,drop=FALSE])
   nstats <- length(stats)
   name.stats <- if(length(dn <- dimnames(stats))) 
	as.vector(outer(dn[[1]],dn[[2]],FUN=function(a,b)paste(b,a))) else 
	names(stats)
   if(length(name.stats)) funlab <- name.stats
   if(nstats>1 && length(name.stats)==0) funlab <- rep(" ", nstats)
   
   s <- array(NA,dim=c(nv+1,nh+1,2+nstats),
	dimnames=list(c(lv,"ALL"), c(lh,"ALL"), c("N","Missing",funlab)))

   for(xv in c(lv,"ALL")) {
     for(xh in c(lh,"ALL")) {
       if(xv=="ALL" && xh=="ALL") st <- c(N, nrow(y)-N, stats) else {
         if(xv=="ALL") u <- h==xh else
         if(xh=="ALL") u <- v==xv else
	 u <- h==xh & v==xv
	 if(any(u)) {
           w <- y[u,,drop=FALSE]
	   nna <- !is.na(w %*% rep(1,ncol(w)))
	   n <- sum(nna)
	   st <- c(n, nrow(w)-n, fun(w[nna,,drop=FALSE]))
         } else st <- c(0, n, rep(NA, length(stats)))
         }
      s[xv,xh,] <- st
      }
    }     

   if((!missing(nmiss) && !nmiss) || (missing(nmiss) &&
	all(s[,,"Missing"]==0))) s <- s[,,-2,drop=FALSE]
   attr(s, "heading")    <- heading
   attr(s, "byvarnames") <- lab[-1]
   attr(s,'class')       <- "bystats2"
   s
						}

print.bystats2 <- function(x, abbreviate.dimnames=FALSE, 
			prefix.width=max(nchar(dimnames(x)[[1]])),...) {
cat("\n",attr(x,"heading"),"\n\n")
if(!exists("print.char.matrix")) {   # Vanilla S
  attr(x, "heading") <- attr(x, "byvarnames") <- attr(x, "class") <-
	NULL
  return(invisible(print(x)))
}
d <- dim(x)
cstats <- array("", dim=d[1:3])

header <- matrix(paste(dimnames(x)[[3]],collapse="\n"),1,1)
print.char.matrix(header)

for(k in 1:d[3]) cstats[,,k] <- format(x[,,k])
dimn <- dimnames(x)[1:2]
names(dimn) <- attr(x,"byvarnames")
cstats2 <- matrix("", nrow=d[1], ncol=d[2], dimnames=dimn)
for(i in 1:d[1]) {
  for(j in 1:d[2]) {
    cstats2[i,j] <- paste(cstats[i,j,],collapse="\n")
  }
}
invisible(if(.R.) print.char.matrix(cstats2,...) else
          print.char.matrix(cstats2, prefix.width=prefix.width,
          abbreviate.dimnames=abbreviate.dimnames,...))
}

latex.bystats2 <-
  function(object,
           title=first.word(expr=substitute(object)),
           caption=attr(object,"heading"),
           rowlabel="", ...) {
   dm <- dimnames(object)
   inn <- c("%","<=","<",">=",">","[")
   out <- c("\\%","$\\leq$","$<$","$\\geq$","$>$","\\verb|[|")
   dm[[1]] <- sedit(dm[[1]],inn,out)
   dm[[2]] <- sedit(dm[[2]],inn,out)
   dm[[3]] <- sedit(dm[[3]],inn,out)
   dimnames(object) <- dm
   caption <- sedit(caption, "cbind", "")
   d <- dim(object)
   dn <- rep(dimnames(object)[[3]], d[2])
   st <- matrix(NA, nrow=d[1], ncol=d[2]*d[3], 
                dimnames=list(dimnames(object)[[1]], dn))

   for(i in 1:d[1]) {
     l <- 0
     for(j in 1:d[2]) {
       for(k in 1:d[3]) {
         l <- l+1
         st[i,l] <- object[i,j,k]
       }
     }
   }

   latex(st, title=title, caption=caption, rowlabel=rowlabel,
	 n.rgroup=c(nrow(st)-1,1), 
	 cgroup=dimnames(object)[[2]], n.cgroup=rep(d[3],d[2]),...)
}

#tref     time at which mortalities estimated
#n1       total sample size, stratum 1
#n2       total sample size, stratum 2
#m1c      tref-year mortality, stratum 1 control
#m2c      "          "                 2  "
#r1       % reduction in m1c by intervention, stratum 1
#r2       % reduction in m2c by intervention, stratum 2
#accrual  duration of accrual period
#tmin     minimum follow-up time
#alpha    type I error
#pr       set to T to print intermediate results

ciapower <- function(tref,   
					n1,     
					n2,     
					m1c,    
					m2c,    
					r1,     
					r2,     
					accrual,
					tmin,   
					alpha=.05,  
					pr=TRUE) { 

#Find mortality in intervention groups
if(m1c>1 | m2c>1) stop("m1c and m2c must be fractions")
m1i <- (1-r1/100)*m1c
m2i <- (1-r2/100)*m2c

if(pr) {
  cat("\nAccrual duration:",accrual,"y  Minimum follow-up:",tmin,"y\n")
  cat("\nSample size Stratum 1:",n1,"  Stratum 2:",n2,"\n")
  cat("\nAlpha=",alpha,"\n")
  d <- list(c("Stratum 1","Stratum 2"), c("Control","Intervention"))
  m <- cbind(c(m1c,m2c),c(m1i,m2i))
  dimnames(m) <- d
  cat("\n",tref,"-year Mortalities\n",sep=""); print(m)
}

#Find exponential hazards for all groups
lam1c <- -logb(1-m1c)/tref
lam2c <- -logb(1-m2c)/tref
lam1i <- -logb(1-m1i)/tref
lam2i <- -logb(1-m2i)/tref

if(pr) {
  lam <- cbind(c(lam1c,lam2c),c(lam1i,lam2i))
  dimnames(lam) <- d
  cat("\nHazard Rates\n"); print(lam)
}

#Find probability that a subject will have her event observed during
#the study, for all groups
tmax <- tmin+accrual
p1c <- 1-1/accrual/lam1c*(exp(-tmin*lam1c)-exp(-tmax*lam1c))
p2c <- 1-1/accrual/lam2c*(exp(-tmin*lam2c)-exp(-tmax*lam2c))
p1i <- 1-1/accrual/lam1i*(exp(-tmin*lam1i)-exp(-tmax*lam1i))
p2i <- 1-1/accrual/lam2i*(exp(-tmin*lam2i)-exp(-tmax*lam2i))

if(pr) {
  p <- cbind(c(p1c,p2c), c(p1i,p2i))
  dimnames(p) <- d
  cat("\nProbabilities of an Event During Study\n")
  print(p)
}

#Find expected number of events, all groups
m1c <- p1c*n1/2
m2c <- p2c*n2/2
m1i <- p1i*n1/2
m2i <- p2i*n2/2

if(pr) {
  m <- cbind(c(m1c,m2c), c(m1i,m2i))
  dimnames(m) <- d
  cat("\nExpected Number of Events\n")
  print(round(m,1))
}

#Find expected value of observed log hazard ratio
delta <- logb((lam1i/lam1c)/(lam2i/lam2c))
if(pr) cat("\nRatio of hazard ratios:",format(exp(delta)),"\n")

#Find its variance
v <- 1/m1c + 1/m2c + 1/m1i + 1/m2i
sd <- sqrt(v)
if(pr) cat("Standard deviation of log ratio of ratios:",format(sd),"\n")

z <- -qnorm(alpha/2)
#if(pr) cat("\nCritical value:",format(z),"\n")

c(Power = 1 - ( pnorm(z - abs(delta)/sd) - pnorm(-z - abs(delta)/sd) ) )
}


if(!.R.) {
"comment<-"  <- function(x, value) {
    if (inherits(value,"file"))
      attr(value,'class') <- c("comment.file", attr(value, 'class'))
    attr(x, "comment") <- value
    x
}

comment <- function(x)  {
    lab <- attr(x, "comment")
    if (inherits(lab,"comment.file"))
       attr(lab,'class') <- attr(lab,'class')[attr(lab,'class') != "comment.file"]
    lab
}

print.comment.file <- function(x, ...) {
        invisible(print(oldUnclass(x)))
}
}
confbar <- function(at, est, se, width,
                    q=c(.7,.8,.9,.95,.99), 
                    col=if(.R.)   gray(c(0,.25,.5,.75,1)) else
                        if(under.unix) c(1,.8,.5,.2,.065) else c(1,4,3,2,5),
                    type=c("v","h"), labels=TRUE, ticks=FALSE,
                    cex=.5, side="l", lwd=5, clip=c(-1e30, 1e30),
                    fun=function(x)x, 
                    qfun=function(x)ifelse(x==.5, qnorm(x),
                      ifelse(x<.5,qnorm(x/2),qnorm((1+x)/2))))
{
type <- match.arg(type)
iusr <- if(type=="v") 1:2 else 3:4
if(missing(width)) width <- diff(par("usr")[iusr])*.02
if(side=="b") side <- "l"    #treat bottom as left
if(length(q)!=length(col)) stop("q and col must have same length")
q <- c(1-rev(q), .5, q)
#qe <- seq(.01, .99, length=n)
#col <- seq(.8,.01, length=n/2)
col <- c(rev(col), col)
w <- width/2
if(type=="v")
{
   polyg <- function(a, b, col, clip) {
	b[b < clip[1] | b > clip[2]] <- NA
	polygon(a, b, col=col)
   }
   Lines <- function(a, b, lwd=1, clip) {
	b[b < clip[1] | b > clip[2]] <- NA
	lines(a, b, lwd=lwd)
   }
   Text  <- function(a, b, clip, ...) {
	b[b < clip[1] | b > clip[2]] <- NA
	text(a, b, ...)
   }
   srt <- 0
}
else
{
   polyg <- function(a, b, col, clip) {
	b[b < clip[1] | b > clip[2]] <- NA
	polygon(b, a, col=col)
   }
   Lines <- function(a, b, lwd=1, clip) {
	b[b < clip[1] | b > clip[2]] <- NA
	lines(b, a, lwd=lwd)
   }
   Text  <- function(a, b, clip, ...) {
	b[b < clip[1] | b > clip[2]] <- NA
	text(b, a, ...)
   }
   srt   <- 45
}
for(i in 1:(length(q)-1))
   polyg(c(at-w,at+w,at+w,at-w),fun(est+se*qfun(c(q[i],q[i],q[i+1],q[i+1]))),
           col=col[i], clip=clip)
a <- fun(est)
z <- w*.24
Lines(c(at-w-3.5*z, at+w+3.5*z), c(a,a), lwd=lwd, clip=clip)
a <- fun(est+se*qfun(q))
do <- TRUE
if(labels || ticks) for(i in 1:length(q))
{
   b <- c(a[i], a[i])
   if(ticks)
   {
      Lines(c(at-w-z,at-w),b, clip=clip)
      Lines(c(at+w+z,at+w),b, clip=clip)
   }
   if(labels && do && q[i]!=.5)
   {
     if(side=="l") Text(at-w-2*z, a[i], format(max(1-q[i],q[i])), 
                        cex=cex, adj=1, srt=srt, clip=clip)
     else Text(at+w+2*z, a[i], format(max(1-q[i],q[i])), 
                        cex=cex, adj=0, srt=srt, clip=clip)
   }
   if(q[i]!=.5)do <- !do
}
names(a) <- format(q)
invisible(a)
}

#tref        time at which mortalities estimated
#n           total sample size
#mc          tref-year mortality, control
#r           % reduction in m1c by intervention
#accrual     duration of accrual period
#tmin        minimum follow-up time
#noncomp.c   % non-compliant in control group (drop-ins)
#noncomp.i   % non-compliant in intervention group (non-adherers)
#alpha       type I error
#nc          Sample size for control (if not n/2)
#ni          Sample size for intervention (if not n/2)
#pr          set to T to print intermediate results
#
#non-compliance handled by an approximation of Eq. 5.4 of
#Lachin JM, Foulkes MA (1986): Evaluation of sample size and power for
#analyses of survival with allowance for nonuniform patient entry,
#losses to follow-up, noncompliance, and stratification.
#Here we're using log hazard ratio instead of their hazard difference

cpower <- function(tref,   
				   n,     
				   mc,
				   r,
				   accrual,
				   tmin,   
				   noncomp.c=0,
				   noncomp.i=0,
				   alpha=.05,  
                   nc, ni,
				   pr=TRUE) { 

if(mc>1) stop("mc should be a fraction")
#Find mortality in intervention group
mi <- (1-r/100)*mc

if(missing(nc) | missing(ni)) {nc <- n/2; ni <- n/2} else n <- nc+ni

if(pr) {
  cat("\nAccrual duration:",accrual,"y  Minimum follow-up:",tmin,"y\n")
  cat("\nTotal sample size:",n,"\n")
  cat("\nAlpha=",alpha,"\n")
  d <- c("Control","Intervention")
  m <- c(mc,mi)
  names(m) <- d
  cat("\n",tref,"-year Mortalities\n",sep=""); print(m)
}

#Find exponential hazards for all groups
lamc <- -logb(1-mc)/tref
lami <- -logb(1-mi)/tref

if(pr) {
  lam <- c(lamc,lami)
  names(lam) <- d
  cat("\nHazard Rates\n"); print(lam)
}

#Find probability that a subject will have her event observed during
#the study, for all groups
tmax <- tmin+accrual
pc <- if(accrual==0)1-exp(-lamc*tmin) else
             1-1/accrual/lamc*(exp(-tmin*lamc)-exp(-tmax*lamc))
pi <- if(accrual==0)1-exp(-lami*tmin) else
             1-1/accrual/lami*(exp(-tmin*lami)-exp(-tmax*lami))

if(pr) {
  p <- c(pc,pi)
  names(p) <- d
  cat("\nProbabilities of an Event During Study\n")
  print(p)
}

#Find expected number of events, all groups
mc <- pc*nc
mi <- pi*ni

if(pr) {
  m <- c(mc,mi)
  names(m) <- d
  cat("\nExpected Number of Events\n")
  print(round(m,1))
}

#Find expected value of observed log hazard ratio
delta <- logb(lami/lamc)
if(pr) cat("\nHazard ratio:",format(exp(delta)),"\n")

if(noncomp.c+noncomp.i>0) {
  if(pr) cat("\nDrop-in rate (controls):",noncomp.c,
			 "%\nNon-adherence rate (intervention):",noncomp.i,"%\n",sep="")
  delta <- delta * (1 - (noncomp.c+noncomp.i)/100)
  if(pr) cat("Effective hazard ratio with non-compliance:",
			 format(exp(delta)),"\n")
}

#Find its variance
v <- 1/mc + 1/mi
#Get same as /sasmacro/samsizc.sas if use 4/(mc+mi)

sd <- sqrt(v)
if(pr) cat("Standard deviation of log hazard ratio:",format(sd),"\n")

z <- -qnorm(alpha/2)

c(Power = 1 - ( pnorm(z - abs(delta)/sd) - pnorm(-z - abs(delta)/sd) ) )
}


#Function like cut but left endpoints are inclusive and labels are of
#the form [lower, upper), except that last interval is [lower,upper].
#F. Harrell  3 Dec 90, modified 7 Mar 92, mod 30May95 (more efficient digits)
#Modified 2Jun95 (preserve label attribute)
#Modified 16Jun95 (categories with 1 unique value -> label=value, not interval)
#Modified 1Jul95 - if specified cuts, mindif would cause improper
#        categorization if a cut was close to but not equal an actual value

cut2 <- function(x, cuts, m=150, g, levels.mean=FALSE, digits, minmax=TRUE,
		 oneval=TRUE) {

method <- 1 ## 20may02
x.unique <- sort(unique(c(x[!is.na(x)],if(!missing(cuts))cuts))) #1Jul95
min.dif <- min(diff(x.unique))/2
min.dif.factor <- 1   # was 1.9999 27Oct00

#Make formatted values look good
if(missing(digits))digits <- if(levels.mean)5 else 3
#.Options$digits <- digits  6Aug00
oldopt <- options(digits=digits)
on.exit(options(oldopt))

xlab <- attr(x, 'label')    #2Jun95

if(missing(cuts)) {
  nnm <- sum(!is.na(x))
  if(missing(g)) g <- max(1,floor(nnm/m))
  if(g < 1) stop('g must be >=1, m must be positive')

#  .Options$digits <- 15  ## to get good resolution for names(table(x))
  options(digits=15)
  n <- table(x)
  xx <- as.double(names(n))  # was single 27Oct00
  options(digits=digits)  # .Options$digits <- digits
  cum <- cumsum(n)
  m <- length(xx)

  ##y <- as.integer(0*x)   ## to preserve NAs   10Dec00
  y <- as.integer(ifelse(is.na(x),NA,1))       #10Dec00
  labs <- character(g)
  cuts <- approx(cum, xx, xout=(1:g)*nnm/g,
                 method='constant', rule=2, f=1)$y
  cuts[length(cuts)] <- max(xx)  # 27Oct00
  lower <- xx[1]
  upper <- 1e45
  up <- low <- double(g)   # was single 27Oct00
#  variation <- logical(g) # 10Dec00
  i <- 0
  for(j in 1:g) {
    cj <- if(method==1 || j==1)cuts[j] else {
      if(i==0) stop('program logic error')
      s <- if(is.na(lower))FALSE else xx >= lower
      cum.used <- if(all(s))0 else max(cum[!s])
      if(j==m)max(xx) else
      if(sum(s)<2) max(xx) else
      approx(cum[s]-cum.used, xx[s], xout=(nnm-cum.used)/(g-j+1),
             method='constant', rule=2, f=1)$y
    }
    if(cj==upper) next
    i <- i + 1
    upper <- cj
    ## Next line 10Dec00
    y[x >= (lower-min.dif.factor*min.dif)]  <- i
#    if(j==1) y[x < (upper+min.dif.factor*min.dif)] <- i else
#    if(j==g) y[x >= (lower-min.dif.factor*min.dif)] <- i else
#    y[x >= (lower-min.dif.factor*min.dif) & x <
#      (upper+min.dif.factor*min.dif)] <- i
    low[i] <- lower
    lower <- if(j==g) upper else min(xx[xx > upper])
    if(is.na(lower)) lower <- upper
    up[i]  <- lower
#    r <- range(x[y==i], na.rm=T)   10Dec00
#    variation[i] <- diff(r) > 0    10Dec00
  }
  low  <- low[1:i]
  up   <- up[1:i]
##  variation <- variation[1:i]     10Dec00
  variation <- logical(i)
  for(ii in 1:i) {
    r <- range(x[y==ii], na.rm=TRUE)
    variation[ii] <- diff(r) > 0
  }
  flow <- format(low)
  fup  <- format(up)
  bb   <- c(rep(')',i-1),']')
  labs <- ifelse(low==up | (oneval & !variation), flow,
                 paste('[',flow,',',fup,bb,sep=''))
  ss <- y==0 & !is.na(y)
  if(any(ss)) stop(paste('categorization error in cut2.  Values of x not appearing in any interval:\n',
                         paste(format(x[ss],digits=12),collapse=' '),
                         '\nLower endpoints:',
                         paste(format(low,digits=12), collapse=' '),
                         '\nUpper endpoints:',
                         paste(format(up,digits=12),collapse=' ')))

  y <- structure(y, class='factor', levels=labs)
} else {

  if(minmax) {
    r <- range(x, na.rm=TRUE)
    if(r[1]<cuts[1]) cuts <- c(r[1], cuts)
    if(r[2]>max(cuts)) cuts <- c(cuts, r[2])
  }
  l <- length(cuts)
  k2 <- cuts-min.dif
  k2[l] <- cuts[l]
  y <- if(version$major < 5) cut(x, k2) else oldCut(x, k2)
  if(!levels.mean) {
    brack <- rep(")",l-1)
    brack[l-1] <- "]"
    fmt <- format(cuts)
    ##If any interval has only one unique value, set label for
    ##that interval to that value and not to an interval
    labs <- paste("[",fmt[1:(l-1)],",",fmt[2:l],
                  brack,sep="")   
    
    if(oneval) {
      nu <- table(if(version$major < 5)cut(x.unique,k2) else
                  oldCut(x.unique,k2))
      if(length(nu)!=length(levels(y)))stop('program logic error')
      levels(y) <- ifelse(nu==1,c(fmt[1:(l-2)],fmt[l]),labs) } else
    levels(y) <- labs
    }
  }

if(levels.mean) {
  means <- tapply(x, y, function(w)mean(w,na.rm=TRUE))
  levels(y) <- format(means)
}
attr(y,'class') <- "factor"
if(length(xlab)) label(y) <- xlab
y
}

#For every object in a data frame that has a 'label' attribute, make it
#class 'labelled'

data.frame.labelled <- function(object) {

for(n in names(object)) if(length(attr(object[[n]],'label')))
	attr(object[[n]],'class') <- c('labelled',attr(object[[n]],'class'))

object
}
dataRep <- function(formula, data, subset, na.action) {
  call <- match.call()
  nact <- NULL
  y <- match.call(expand=FALSE)
  if(missing(na.action)) y$na.action <- na.delete
  y[[1]] <- as.name("model.frame")
  ##See if Des argument exists in current model.frame.default
  if(length(model.frame.default$Des)) y$Des  <- FALSE   #turn off Design
  X <- eval(y, sys.parent())
  nact <- attr(X,"na.action")
  n <- nrow(X)
  nam <- names(X)
  p <- length(nam)
  types <- character(p)
  parms <- character(p)
  pctl  <- vector('list',p)
  margfreq <- vector('list',p)
  Xu   <- vector('list',p)
  for(j in 1:p) {
	namj <- nam[j]
	xj <- X[[j]]
	if(is.character(xj)) xj <- as.factor(xj)
	if(is.factor(xj)) {
	  parms[[j]] <- paste(levels(xj),collapse=' ')
	  types[j] <- 'exact categorical'
	} else if(inherits(xj,'roundN')) {
	  atr <- attributes(xj)
	  nam[j] <- atr$name
	  types[j] <- 'round'
	  parms[j] <- paste('to nearest',format(atr$tolerance))
	  if(length(w <- atr$clip))
		parms[j] <- paste(parms[j],', clipped to [',
						  paste(format(w),collapse=','),']',sep='')
	  pctl[[j]] <- atr$percentiles
	} else {
	  types[j] <- 'exact numeric'
	  parms[j] <- ''
	  pctl[[j]] <- quantile(xj, seq(0,1,by=.01))
	}
	margfreq[[j]] <- table(xj)
	Xu[[j]] <- sort(unique(xj))
	X[[j]] <- xj
  }
  names(types) <- names(parms) <- names(pctl) <- names(margfreq) <- 
	names(Xu) <- nam
  Xu <- expand.grid(Xu)
  m <- nrow(Xu)
  count <- integer(m)
  for(i in 1:m) {
	matches <- rep(TRUE,n)
	for(j in 1:p) matches <- matches & 
	  (as.character(X[[j]]) == as.character(Xu[[j]][i]))
	count[i] <- sum(matches)
  }
  if(any(count==0)) {
    s     <- count > 0
	Xu    <- Xu[s,]
    count <- count[s]
	m     <- sum(s)
  }

  structure(list(call=call, formula=formula, n=n, names=nam, 
				 types=types, parms=parms, margfreq=margfreq,
				 percentiles=pctl, X=Xu, count=count, na.action=nact), 
			class='dataRep')
}

roundN <- function(x, tol=1, clip=NULL) {
  pct <- quantile(x, seq(0,1,by=.01), na.rm=TRUE)
  name <- deparse(substitute(x))
  lab <- attr(x, 'label')
  if(!length(lab)) lab <- name
  if(!missing(clip)) x <- pmin(pmax(x,clip[1]),clip[2])
  structure(as.single(tol*round(x/tol)), tolerance=tol, clip=clip,
			percentiles=pct, name=name, label=lab, class='roundN')
}

if(.R.) as.data.frame.roundN <- as.data.frame.vector

'[.roundN' <- function(x, i, ...) {
  atr <- attributes(x)
  x <- oldUnclass(x)[i]
  attributes(x) <- atr
  x
}
			
print.dataRep <- function(x, long=FALSE, ...) {
  cat("\n")
  cat("Data Representativeness    n=",x$n,"\n\n", sep='')
  dput(x$call)
  cat("\n")
  if(length(z <- x$na.action)) naprint(z)
  specs <- data.frame(Type=x$types, 
					  Parameters=x$parms,
					  row.names=x$names)
  cat('Specifications for Matching\n\n')
  print.data.frame(specs)
	X <- x$X
  if(long) {
	X$Frequency <- x$count
	cat('\nUnique Combinations of Descriptor Variables\n\n')
	print.data.frame(X)
  } else cat('\n',nrow(X),
	'unique combinations of variable values were found.\n\n')
  invisible()
}

predict.dataRep <- function(object, newdata, ...) {
  n <- object$n
  count <- object$count
  if(missing(newdata)) return(count)

  pctl     <- object$percentiles
  margfreq <- object$margfreq
  p        <- length(margfreq)
  m        <- nrow(newdata)
  nam      <- object$names
  types    <- object$types
  X        <- object$X

#  Xn <- if(length(model.frame.default$Des))   3Aug02
#	model.frame(object$formula, newdata, na.action=na.keep, Des=FALSE) else
  Xn <- model.frame(object$formula, newdata, na.action=na.keep)
  names(Xn) <- nam

  worst.margfreq <- rep(1e8, m)
  pct <- matrix(NA, m, p, dimnames=list(row.names(Xn),nam))
  for(j in 1:p) {
	xj <- Xn[[j]]
	freq <- margfreq[[nam[j]]][as.character(xj)]
	freq[is.na(freq)] <- 0
	pct[,j] <- if(types[j]=='exact categorical') 100*freq/n else
	  approx(pctl[[nam[j]]], seq(0,100,by=1), xout=newdata[[nam[j]]], rule=2)$y
	worst.margfreq <- pmin(worst.margfreq, freq)
  }

  cnt <- integer(m)
  for(i in 1:m) {
	matches <- rep(TRUE,nrow(X))
	for(j in 1:p) {
	  matches <- matches & (as.character(X[[j]]) == as.character(Xn[[j]][i]))
	}
	s <- sum(matches)
  if(s > 1) 
	warning('more than one match to original data combinations')
  cnt[i] <- if(s) count[matches] else 0
  }
  
  if(any(cnt > worst.margfreq)) warning('program logic error')

  structure(list(count=cnt, percentiles=pct, worst.margfreq=worst.margfreq, 
				 newdata=newdata),	class='predict.dataRep')
}

print.predict.dataRep <- function(x, prdata=TRUE, prpct=TRUE, ...) {
  if(prdata) {
	dat <- x$newdata
	dat$Frequency     <- x$count
	dat$Marginal.Freq <- x$worst.margfreq
	cat('\nDescriptor Variable Values, Estimated Frequency in Original Dataset,\nand Minimum Marginal Frequency for any Variable\n\n')
	print.data.frame(dat)
  } else {
	cat('\nFrequency in Original Dataset\n\n')
	print(x$count)
	cat('\nMinimum Marginal Frequency for any Variable\n\n')
	print(x$worst.margfreq)
  }
  if(prpct) {
	cat('\n\nPercentiles for Continuous Descriptor Variables,\nPercentage in Category for Categorical Variables\n\n')
	print(round(x$percentiles))
  }
  invisible()
}


deff <- function(y, cluster)	{

   ss <- function(x)	{
	n <- length(x)
	xbar <- sum(x)/n
	sum((x-xbar)^2)
   }

if(!is.factor(cluster)) cluster <- as.factor(cluster)
cluster <- oldUnclass(cluster)
s <- !is.na(cluster+y)
y <- y[s]; cluster <- as.integer(cluster[s])
n <- length(y)
sst <- ss(y)
sses <- tapply(y,cluster,ss)
k  <- length(sses)
R2 <- 1-sum(sses)/sst
Fstat  <- R2*(n-k)/(1-R2)/k
g  <- (Fstat-1)*k/n
rho <- g/(1+g)
ng <- table(cluster)
B  <- sum(ng^2)/n
deff <- 1+(B-1)*rho
c(n=n, clusters=k, rho=rho, deff=deff)
}
describe <- function(x, ...) UseMethod("describe")  #13Mar99


describe.default <- function(x, descript, ...) {  #13Mar99
  if(missing(descript)) descript <- deparse(substitute(x)) #13Mar99

  if(is.matrix(x)) describe.matrix(x, descript, ...) else
  describe.vector(x, descript, ...)  #13Mar99
}

describe.vector <- function(x, descript, exclude.missing=TRUE, digits=4,
							 weights=NULL, normwt=FALSE, ...) {   #13Mar99
#.Options$digits <- digits  6Aug00
oldopt <- options(digits=digits)
on.exit(options(oldopt))

if(length(weights)==0) weights <- rep(1,length(x))

special.codes <- attr(x, "special.miss")$codes
labx <- attr(x,"label")
if(missing(descript)) descript <- as.character(sys.call())[2]

if(length(labx) && labx!=descript)descript <- 
	paste(descript,":",labx)
un <- attr(x,"units")
if(length(un) && un=='') un <- NULL  ## 8jun03 and next
#if(length(un) && un!="") descript <- paste(descript," (",un,")",sep="")
#if(length(fmt <- attr(x,"format"))) #>1 format if date-time (chron)
#if(length(fmt)==1) descript <- paste(descript,"  Format:",as.character(fmt))
#else descript <- paste(descript,"  Format:",
#     as.character(fmt[[1]]),as.character(fmt[[2]]), sep=" ")
fmt <- attr(x,'format')
if(length(fmt) && fmt=='') fmt <- NULL
if(length(fmt) > 1)
  fmt <- paste(as.character(fmt[[1]]),as.character(fmt[[2]]))
  
##descript <- paste(descript, ' (', storage.mode(x),')',sep='')

if(all(is.na(x)))present <- rep(FALSE,length(x))
else if(is.character(x)) present <- x!="" & x!=" " #5Mar93
else present <-  !is.na(x)
present <- present & !is.na(weights)
if(length(weights) != length(x)) 
  stop('length of weights must equal length of x')

if(normwt) {
  weights <- sum(present)*weights/sum(weights[present])
  n <- sum(present)
} else n <- sum(weights[present])
if(exclude.missing && n==0)return(structure(NULL, class="describe"))
missing <- sum(weights[!present], na.rm=TRUE)
atx <- attributes(x)
atx$names <- atx$dimnames <- atx$dim <- atx$special.miss <- NULL  
#added dim,dimnames 18 Dec 95, last 1 7May96
atx$class <- atx$class[atx$class!='special.miss']

# Added timeDate 3Dec00
#istimeDate <- inherits(x,'timeDate') # new for SV4   3Dec00
istimeDate <- if(.R.) inherits(x,'POSIXt') & inherits(x,'POSIXct')
else inherits(x, 'timeDate')  ## 8jun03
## added POSIX (for R) 31aug02 by writing isChron in Misc.s
isdatetime <- isChron(x)
x <- x[present,drop=FALSE]  ## drop=F 14Nov97
x.unique <- sort(unique(x))
weights <- weights[present]

n.unique <- length(x.unique)
attributes(x) <- attributes(x.unique) <- atx
isnum <- (is.numeric(x) || istimeDate) && !is.category(x)
# timeDate 3Dec00
## 31aug02:
#notime <- if(isdatetime)
#  ifelse(.R., all(format(x.unique,"%H%M%S")=='000000'),  ## 8jun03
#         all(as.numeric(x.unique)==as.integer(x.unique))) else FALSE
## Next 9 lines 22jun03, commented out last 3 lines
notime <- FALSE
if(.R.) {
  if(isdatetime) notime <- all(format(x.unique,"%H%M%S")=='000000')
} else {
  if(istimeDate) notime <-
    length(grep('00:00:00',format(x.unique)))==length(x.unique) else
  if(isdatetime) notime <-
    all(as.numeric(x.unique)==as.integer(x.unique))
}
z <- list(descript=descript, units=un, format=fmt)

counts <- c(n,missing)
lab <- c("n","missing")

if(length(special.codes))	{
   tabsc <- table(special.codes)
   counts <- c(counts, tabsc)
   lab <- c(lab, names(tabsc))	}
if(length(atx$imputed))	{
   counts <- c(counts, length(atx$imputed))
   lab <- c(lab, "imputed")
				}
if(length(pd <- atx$partial.date))	{
   if((nn <- length(pd$month))>0) 
	{counts <- c(counts, nn); lab <- c(lab,"missing month")}
   if((nn <- length(pd$day))>0)
	{counts <- c(counts, nn); lab <- c(lab,"missing day")}
   if((nn <- length(pd$both))>0)
	{counts <- c(counts, nn); lab <- c(lab,"missing month,day")}
					}

if(length(atx$substi.source)) {
   tabss <- table(atx$substi.source)
   counts <- c(counts, tabss)
   lab <- c(lab, names(tabss))
}

counts <- c(counts,n.unique)
lab <- c(lab,"unique")
x.binary <- n.unique==2 && isnum && x.unique[1]==0 && x.unique[2]==1
if(x.binary)		{
	counts <- c(counts,sum(weights[x==1]))
	lab <- c(lab,"Sum")	}
if(isnum)	{
  xnum <- if(.SV4.)as.numeric(x) else oldUnclass(x)  # 3Dec00
  if(isdatetime) {
    dd <- sum(weights*xnum)/sum(weights)  # 3Dec00
    attributes(dd) <- atx
    if(istimeDate) {
      if(.R.) {
        fval <- if(notime) as.POSIXct(round(dd, 'days')) else dd
      } else {
        if(notime) dd <- round(dd) # 3Dec00
        fval <- timeDate(julian=dd)
      }
    }
    else fval <- dd   # 3Dec00
    counts <- c(counts, format(fval, ...))	}  #was as.character(dd) 26May97
  else counts <- c(counts,format(sum(weights*x)/sum(weights),...))
  lab <- c(lab,"Mean")	}
if(n.unique>=10 & isnum)	{
  q <- if(any(weights != 1)) 
    wtd.quantile(xnum,weights,normwt=FALSE,na.rm=FALSE,  # 3Dec00
                 probs=c(.05,.1,.25,.5,.75,.90,.95)) else 
  quantile(xnum,c(.05,.1,.25,.5,.75,.90,.95),na.rm=FALSE)
## Only reason to call quantile is that the two functions can give
## different results if there are ties, and users are used to quantile()
  if(isdatetime) {
      attributes(q) <- atx
      if(istimeDate) {
        if(.R. && notime) fval <- as.POSIXct(round(q,'days')) else
        if(.R.) fval <- q else {
          ## above line 6sep03
          if(notime) q <- round(q)
          fval <- timeDate(julian=q)
        }
      } else fval <- q
      counts <- c(counts, format(fval,...))	}  #was as.character(q) 26May97
	else counts <- c(counts,format(q,...))
  lab <- c(lab,".05",".10",".25",".50",".75",".90",".95")  }
names(counts) <- lab
z$counts <- counts

counts <- NULL

if(n.unique>=20) {
  if(isnum) {  ##15Nov00 Store frequency table, 100 intervals
    r <- range(xnum)   # 3Dec00
    xg <- pmin(1 + floor((100 * (xnum - r[1]))/  # 3Dec00
                         (r[2] - r[1])), 100)
    z$intervalFreq <- list(range=as.single(r),
                           count = as.integer(tabulate(xg)))
  }
  lo <- x.unique[1:5]; hi <- x.unique[(n.unique-4):n.unique]
  if(isdatetime) {
    fval <- if(!.R. && istimeDate && notime)
      c(format(timeDate(julian=round(lo))),
        format(timeDate(julian=round(hi)))) else
    c(format(lo,...),format(hi,...))
  } else {

      
#    counts <- format(c(format(lo,...), format(hi,...))) else 
#      if(!.R. && istimeDate) counts <- if(notime)
#        format(c(format(timeDate(julian=lo)),format(timeDate(julian=hi)))) else
#  format(c(format(lo),format(hi)))
    f <- if(is.factor(x.unique))as.character else function(x) x
    ## c() function loses factor variables attributes   8Feb96
    fval <- c(f(lo), f(hi))
  }
  counts <- format(fval)
  names(counts) <- c("L1","L2","L3","L4","L5","H5","H4","H3","H2","H1")
}

if(n.unique>1 && n.unique<20 && !x.binary)	{
	## following was & !isdatetime 26May97
	tab <- wtd.table(if(isnum)format(x) else x,weights,
					 normwt=FALSE,na.rm=FALSE,type='table')  
	pct <- round(100*tab/sum(tab))
	counts <- t(as.matrix(tab))
	counts <- rbind(counts, pct)
	dimnames(counts)[[1]]<- c("Frequency","%")
  }
z$values <- counts
structure(z, class="describe")
}

describe.matrix <- function(x, descript, exclude.missing=TRUE, digits=4, ...) {

if(missing(descript)) descript <- as.character(sys.call())[2]

nam <- dimnames(x)[[2]]
if(length(nam)==0) stop('matrix does not have column names')

Z <- vector('list', length(nam))
names(Z) <- nam

d <- dim(x)
missing.vars <- NULL
for(i in 1:ncol(x)) {
  z <- describe.vector(x[,i],nam[i],exclude.missing=exclude.missing,
			digits=digits,...)  #13Mar99
  Z[[i]] <- z
  if(exclude.missing && length(z)==0) missing.vars <- c(missing.vars,nam[i])
}

attr(Z, 'descript') <- descript
attr(Z, 'dimensions') <- d
attr(Z, 'missing.vars') <- missing.vars
structure(Z, class="describe")
}

describe.data.frame <- function(x,descript,exclude.missing=TRUE,digits=4,...) {

if(missing(descript)) descript <- as.character(sys.call())[2]

nam <- names(x)
Z <- list()
nams <- character(0)

i <- 0
missing.vars <- NULL
for(xx in x) {
	mat <- is.matrix(xx)
	i <- i+1
	z <- if(mat) 
	  describe.matrix(xx,nam[i],exclude.missing=exclude.missing,
					  digits=digits,...)
	  else	  
	   describe.vector(xx,nam[i],exclude.missing=exclude.missing,
						digits=digits,...)  #13Mar99
	all.missing <- length(z)==0
	if(exclude.missing && all.missing) missing.vars <- c(missing.vars,
			nam[i]) else {
	  Z <- c(Z, if(mat) z else list(z))
	  nams <- c(nams, if(mat) names(z) else nam[i])
	}
}
names(Z) <- nams

attr(Z, 'descript') <- descript
attr(Z, 'dimensions') <- dim(x)
attr(Z, 'missing.vars') <- missing.vars
structure(Z, class="describe")
}

describe.formula <- function(x, descript, data, subset, na.action, 
							 digits=4, weights, ...) {
mf <- match.call(expand=FALSE)
mf$formula <- x
mf$x <- mf$descript <- mf$file <- mf$append <- mf$... <- mf$digits <- NULL
if(missing(na.action)) mf$na.action <- na.retain
mf[[1]] <- as.name("model.frame")
mf <- eval(mf, sys.parent())
weights <- model.extract(mf, weights)
		
if(missing(descript)) {
   ter <- attr(mf,"terms")
   d <- as.character(x)
   if(attr(ter,"response")==1) d <- c(d[2],d[1],d[-(1:2)])
   else d <- d[-1]
   d <- paste(d, collapse=" ")
   descript <- d
 }

Z <- describe.data.frame(mf, descript, digits=digits, weights=weights, ...)
if(length(z <- attr(mf,"na.action"))) attr(Z,'naprint') <- naprint(z) 

Z
}

na.retain <- function(d) d


print.describe <- function(x, condense=TRUE, ...) {

at <- attributes(x)
if(length(at$dimensions)) {
  cat(at$descript,'\n\n',at$dimensions[2],' Variables     ',at$dimensions[1],
	  ' Observations\n')
  if(length(at$naprint)) cat('\n',at$naprint,'\n')
  cat('---------------------------------------------------------------------------\n')
  for(z in x) {
	if(length(z)==0) next
	print.describe.single(z, condense=condense)
  cat('---------------------------------------------------------------------------\n')
  }
  if(length(at$missing.vars)) {
	cat('\nVariables with all observations missing:\n\n')
	print(at$missing.vars, quote=FALSE)
  }
} else print.describe.single(x, condense=condense)
invisible()
}

print.describe.single <- function(x, condense=TRUE, ...) {
wide <- .Options$width
des <- x$descript
if(length(x$units)) des <- paste(des, ' [', x$units, ']', sep='')
if(length(x$format))des <- paste(des, '  Format:', x$format, sep='')
cat(des,'\n')
print(x$counts, quote=FALSE)
if(length(val <- x$values)) {
  if(length(dim(val))==0) {
	if(condense) {
	  low <- paste('lowest :', paste(val[1:5],collapse=' '))
	  hi  <- paste('highest:', paste(val[6:10],collapse=' '))
	  cat('\n',low,sep='')
	  if(nchar(low)+nchar(hi)+2>wide) cat('\n') else cat(', ')
	  cat(hi,'\n')
	} else {cat('\n'); print(val, quote=FALSE)}
  } else {
	lev <- dimnames(val)[[2]]
	if(condense && (mean(nchar(lev))>10 | length(lev) < 5)) {
	  z <- ''; len <- 0; cat('\n')
	  for(i in 1:length(lev)) {
		w <- paste(lev[i], ' (', val[1,i], ', ', val[2,i], '%)', sep='')
		l <- nchar(w)
		if(len + l + 2 > wide) { cat(z,'\n'); len <- 0; z <- '' }
		if(len==0) { z <- w; len <- l }
		else { z <- paste(z, ', ', w, sep=''); len <- len + l + 2 }
	  }
	  cat(z, '\n')
	} else { cat('\n'); print(val, quote=FALSE) }
  }
}
invisible()
}

'[.describe' <- function(object, i, ...) {
at <- attributes(object)
object <- '['(oldUnclass(object),i)
structure(object, descript=at$descript,
          dimensions=c(at$dimensions[1], length(object)),
          class='describe')
}

latex.describe <- function(object, title=NULL, condense=TRUE,
                           file=paste('describe',
                             first.word(expr=attr(object,'descript')),
                             'tex',sep='.'),
                           append=FALSE, size='small',
                           tabular=TRUE, ...) {

at <- attributes(object)
ct <- function(..., file, append=FALSE) {
  if(file=='') cat(...) else cat(..., file=file, append=append)
  invisible()
}
ct('\\begin{spacing}{0.7}\n', file=file, append=append)
if(length(at$dimensions)) {
  ct('\\begin{center}\\bf ', at$descript, '\\\\',
      at$dimensions[2],'Variables~~~~~',at$dimensions[1],
      '~Observations\\end{center}\n', file=file, append=TRUE)
  if(length(at$naprint)) ct(at$naprint,'\\\\\n', file=file,
                            append=TRUE)
  ct('\\vspace{-.5ex}\\hrule\\smallskip{\\',size,'\n',
      sep='', file=file, append=TRUE)
  vnames <- at$names
  i <- 0
  for(z in object) {
    i <- i + 1
	if(length(z)==0) next
    ct('\\vbox{', file=file, append=TRUE)
	latex.describe.single(z, condense=condense, vname=vnames[i],
                          file=file, append=TRUE, tabular=tabular)
    ct('\\vspace{-.5ex}\\hrule\\smallskip}\n', file=file, append=TRUE)
  }
  if(length(mv <- at$missing.vars)) {
	ct('\\smallskip\\noindent Variables with all observations missing:\\ \\smallskip\n',
        file=file, append=TRUE)
    mv <- paste('\\texttt{',mv,'}',sep='')
    mv <- paste(mv, collapse=', ')
#    ct('\\texttt{',at$missing.vars, '}', sep='', file=file,
#    append=TRUE)
    ct(mv, file=file, append=TRUE)
  }
  ct('}', file=file, append=TRUE)  # added 23oct02
} else latex.describe.single(object,
                             vname=first.word(expr=at$descript),
                             condense=condense,
                             file=file, append=TRUE, size=size,
                             tabular=tabular)
# was append=append 23oct02; also removed } in cat below
ct('\\end{spacing}\n', file=file, append=TRUE)

#if(!.SV4.)   18Oct01
  structure(list(file=file,  style=c('setspace','relsize')),
                 class='latex')
}

latex.describe.single <- function(object, title=NULL, condense=TRUE, vname,
                                  file, append=FALSE, size='small',
                                  tabular=TRUE, ...) {

ct <- function(..., file, append=FALSE) {
  if(file=='') cat(...) else cat(..., file=file, append=append)
  invisible()
}

oldw <- options(width=85)
on.exit(options(oldw))

wide <- switch(size,
               normalsize=66,
               small=73,
               scriptsize=93,
               73)

intFreq <- object$intervalFreq

## Put graph on its own line if length of label > 3.5 inches
## For normalsize there are 66 characters per 4.8 in. standard width

des <- paste('\\textbf{',latexTranslate(object$descript),'}',sep='')
if(length(object$units))
  des <- paste(des, '{\\smaller[1] [',
               latexTranslate(object$units),']}', sep='')
if(length(object$format))
  des <- paste(des, '{\\smaller~~Format:', latexTranslate(object$format),
               '}',sep='')
desbas <- paste(object$descript,
                if(length(object$units)) paste(' [',object$units,']',sep=''),
                if(length(object$format))paste('  Format:',object$format,sep=''))

ct('\\noindent', des, sep='', file=file, append=append)
if(length(intFreq)) {
  counts <- intFreq$count
  maxcounts <- max(counts)
  ## \mbox{~~~} makes \hfill work
  ct(if(nchar(desbas)/(wide/4.8) > (4.8-1.5))' \\\\ \\mbox{~~~} \n',
      '\\setlength{\\unitlength}{0.001in}\\hfill',
      '\\begin{picture}(1.5,.1)(1500,0)',
      '\\linethickness{0.6pt}\n', sep='', file=file, append=TRUE)
  for(i in (1:100)[counts > 0]) {
    ct('\\put(',round(1000*(i-1)*1.5/100),',0){\\line(0,1){',
        max(1,round(1000*counts[i]/maxcounts*.1)),'}}\n',
        sep='', file=file, append=TRUE)
  }
  ct('\\end{picture}\n', file=file, append=TRUE)
}

sz <- ''
if(tabular) {
  ml <- nchar(paste(object$counts,collapse='  '))
  if(ml > 90) tabular <- FALSE else if(ml > 80) sz <- '[2]'
}
ct('{\\smaller', sz, sep='', file=file, append=TRUE)
if(tabular) {
  ct('\\\\ \\begin{tabular}{',
     paste(rep('r',length(object$counts)),collapse=''),'}\n',
     file=file, append=TRUE)
  ct(paste(names(object$counts), collapse='&'), '\\\\ \n',
     file=file, append=TRUE)
  ct(paste(object$counts, collapse='&'), '\\end{tabular}',
     file=file, append=TRUE)
}
ct('\\begin{verbatim}\n', file=file, append=TRUE)
if(file!='') sink(file, append=TRUE)  ## 22dec02
if(!tabular) print(object$counts, quote=FALSE)
if(length(val <- object$values)) {
  if(length(dim(val))==0) {
	if(condense) {
	  low <- paste('lowest :', paste(val[1:5],collapse=' '))
	  hi  <- paste('highest:', paste(val[6:10],collapse=' '))
	  cat('\n',low,sep='')
	  if(nchar(low)+nchar(hi)+2 > wide) cat('\n') else cat(', ')
	  cat(hi,'\n')
	} else {cat('\n'); print(val, quote=FALSE)}
  } else {
	lev <- dimnames(val)[[2]]
	if(condense && (mean(nchar(lev))>10 | length(lev) < 5)) {
	  z <- ''; len <- 0; cat('\n')
	  for(i in 1:length(lev)) {
		w <- paste(lev[i], ' (', val[1,i], ', ', val[2,i], '%)', sep='')
		l <- nchar(w)
		if(len + l + 2 > wide) { cat(z,'\n'); len <- 0; z <- '' }
		if(len==0) { z <- w; len <- l }
		else { z <- paste(z, ', ', w, sep=''); len <- len + l + 2 }
	  }
	  cat(z, '\n')
	} else { cat('\n'); print(val, quote=FALSE) }
  }
}
cat('\\end{verbatim}}\n')
if(file!='') sink()
invisible()
}

if(FALSE && .SV4.) {
  setMethod('latex', 'describe', latex.describe)
  remove('latex.describe')
}

dataDensityString <- function(x, nint=30) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  if(length(x) < 2) return('')
  r <- range(x)
  x <- floor(nint * (x-r[1])/(r[2]-r[1]))
  x <- pmin(tabulate(x), 37)
  paste(format(r[1]),' <', paste(
   substring(' 1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ',
             x+1,x+1), collapse=''),
  '> ',format(r[2]),sep='')
}


## Unused code from latex.describe.single
if(FALSE && length(intFreq)) {
  psthere <- TRUE
  psfile <- paste(psBase,vname,'.ps',sep='')
  x <- seq(intFreq$range[1], intFreq$range[2], length=100)
  counts <- intFreq$count
  oldopt <- options(warn=-1)
  if(under.unix)
    postscript(file = psfile, horizontal = FALSE,
               width = 1.5, height = .1, 
               maximize = TRUE,
               onefile = FALSE, print.it = FALSE) else
  postscript(file = psfile, horizontal = FALSE, width=1.5, height =.1)
  oldpar <- par(mar=rep(0,4),oma=rep(0,4))  # add mex=.5 to prevent
                                            # error msgs.  Need this
   # in 2nd par call.
  on.exit(par(oldpar))
  options(oldopt)
  plot(x, freqFun(counts), type='n', axes=FALSE, xlab='', ylab='')
  j <- counts > 0
  segments(x[j], 0, x[j], freqFun(counts[j]))
  dev.off()
}

contents <- function(object, ...) UseMethod('contents')
contents.data.frame <- function(object, ...) {
  dfname <- deparse(substitute(object))
  nam <- names(object)
  d <- dim(object)
  n <- length(nam)
  fl <- nas <- integer(n)
  cl <- sm <- lab <- un <- character(n)
  Lev <- list()
  for(i in 1:n) {
    x <- object[[i]]
    at <- attributes(x)
    if(length(at$label)) lab[i] <- at$label
    if(length(at$units)) un[i] <- at$units
    atl <- at$levels
    fl[i] <- length(atl)
    cli <- at$class[at$class %nin% c('labelled','factor')]
#    if(length(at$class) && at$class[1] %nin%c('labelled','factor'))
#      cl[i] <- at$class[1]  11aug03
    if(length(cli)) cl[i] <- cli[1]
    sm[i] <- storage.mode(x)
    nas[i] <- sum(is.na(x))
    if(length(atl)) Lev[[nam[i]]] <- atl 
  }
  w <- list(Labels=if(any(lab!='')) lab,
            Units=if(any(un!=''))   un,
            Levels=if(any(fl>0)) fl,
            Class=if(any(cl!='')) cl,
            Storage=sm,
            NAs=if(any(nas>0))nas)
  if(.R.) w <- w[sapply(w, function(x)length(x)>0)]
  # R does not remove NULL elements from a list
  structure(list(contents=data.frame(w, row.names=nam),
                 dim=d, maxnas=max(nas), dfname=dfname,
                 Levels=Lev),
           class='contents.data.frame')
 }

print.contents.data.frame <-
  function(x, sort=c('none','names','labels','NAs'), prlevels=TRUE, ...) {
    sort <- match.arg(sort)
    d <- x$dim
    maxnas <- x$maxnas
    cat('\nData frame:',x$dfname,'\t',d[1],' observations and ',d[2],
        ' variables    Maximum # NAs:',maxnas,'\n\n',sep='')
    cont <- x$contents
    nam <- row.names(cont)

    switch(sort,
           names={cont <- cont[order(nam),]},
           labels={if(length(cont$Labels)) 
                     cont <-  cont[order(cont$Labels, nam),]},
           NAs={if(maxnas>0) cont <- cont[order(cont$NAs,nam),]})

    if(length(cont$Levels))
      cont$Levels <- ifelse(cont$Levels==0,'',format(cont$Levels))
    print(cont)

    if(prlevels && length(L <- x$Levels)) {
      cat('\n')
      nam <- lin <- names(L)
      w <- .Options$width-max(nchar(nam))-5
      ## separate multiple lines per var with \n for print.char.matrix
      for(i in 1:length(L))
        lin[i] <- paste(pasteFit(L[[i]], width=w), collapse='\n')
      if(.R.)
        {
          z <- cbind(Variable=nam,Levels=lin)
          print.char.matrix(z, col.txt.align='left', col.name.align='left',
                            row.names=TRUE, col.names=TRUE)
        } else print.char.matrix(matrix(lin,ncol=1,
                                        dimnames=list(nam,'Levels')))
    }
    invisible()
  }


html.contents.data.frame <-
  function(object, sort=c('none','names','labels','NAs'), prlevels=TRUE,
           file=paste('contents',object$dfname,'html',sep='.'),
           append=FALSE, ...) {
    sort <- match.arg(sort)
    d <- object$dim
    maxnas <- object$maxnas
    cat('<hr><h2>Data frame:',object$dfname,
        '</h2>',d[1],
        ' observations and ',d[2],
        ' variables, maximum # NAs:',maxnas,'<hr>\n',sep='',
        file=file, append=append)
    cont <- object$contents
    nam <- row.names(cont)

    switch(sort,
           names={cont <- cont[order(nam),]},
           labels={if(length(cont$Labels)) 
                     cont <-  cont[order(cont$Labels, nam),]},
           NAs={if(maxnas>0) cont <- cont[order(cont$NAs,nam),]})

    if(length(cont$Levels)) {
      cont$Levels <- ifelse(cont$Levels==0,'',format(cont$Levels))
      adj <- rep('l', length(cont))
      adj[names(cont) %in% c('NAs','Levels')] <- 'r'
      out <- html(cont, file=file, append=TRUE,
                  link=ifelse(cont$Levels=='','',paste('#',nam,sep='')),
                  linkCol='Levels', col.just=adj, ...)
    } else out <- html(cont, file=file, append=TRUE, ...)
    cat('<hr>\n', file=file, append=TRUE)

    if(prlevels && length(L <- object$Levels)) {
      nam <- names(L)
      lab <- lev <- character(0)
      for(i in 1:length(L)) {
        l <- L[[i]]
        lab <- c(lab, nam[i], rep('',length(l)-1))
        lev <- c(lev, l)
      }
      z <- cbind(Variable=lab, Levels=lev)
      out <- html(z, file=file, append=TRUE,
                  link=lab, linkCol='Variable', linkType='name', ...)
      cat('<hr>\n',file=file,append=TRUE)
    }
    out
  }

contents.list <- function(object, dslabels=NULL, ...) {
  nam <- names(object)
  if(length(dslabels)) {
    dslabels <- dslabels[nam]
    names(dslabels) <- NULL
  }
  g <- function(w) {
    c(Obs=length(w[[1]]), Var=length(w),
      Var.NA=sum(sapply(w, function(x) sum(is.present(x))==0)))
  }
  v <- t(sapply(object, g))
  structure(list(contents=if(length(dslabels))
                 data.frame(Label=dslabels,Obs=v[,'Obs'],
                            Var=v[,'Var'],Var.NA=v[,'Var.NA'],
                            row.names=nam) else
                 data.frame(Obs=v[,'Obs'],Var=v[,'Var'],
                            Var.NA=v[,'Var.NA'], row.names=nam)),
            class='contents.list')
}

print.contents.list <-
  function(x, sort=c('none','names','labels','NAs','vars'), ...) {
    sort <- match.arg(sort)
    cont <- x$contents
    nam <- row.names(cont)

    cont <- cont[
      switch(sort,
             none=1:length(nam),
             names=order(nam),
             vars=order(cont$Var),
             labels=order(cont$Label, nam),
             NAs=order(cont$Var.NA,nam)),]

    print(cont)
    invisible()
  }

do <- function(condition, expressions, device=NULL, file, append=FALSE,
               multiplot=FALSE, ...) {

if(!condition) return(invisible())

# The following function is courtesy of Bill Dunlap, StatSci

strip.comments <- function(expr) {
  if (mode(expr) == "comment.expression") {
	not.comment <- sapply(expr, function(ei)mode(ei)!="comment")
	if (sum(not.comment)!=1)
	  stop("unexpected result: no non-comment in expression")
	  else {
		Recall(expr[not.comment][[1]])
	  }
  } else expr
}


condition <- as.character(substitute(condition))
scondition <- if(under.unix) condition else
  substring(sedit(condition, '.', ''), 1,8)
pcondition <- if(multiplot) substring(scondition,1,7) else scondition

do.file <- if(missing(file)) {
  if(length(ds <- .Options$do.file)==0) '' else ds
} else file

do.prefix <- .Options$do.prefix

if(do.file!='') {
  if(do.file=='condition') 
	sink(sink.file <- paste(if(length(do.prefix))
          paste(do.prefix,if(under.unix)'.' else '/',sep=''), 
          paste(scondition, 'lst',sep='.'), 
            sep=''), append=append) else
    sink(sink.file <- paste(do.file, '.lst',sep=''), append=append)
}

if(missing(device)) device <- .Options$do.device

if(length(device)) {
  suffix <- if(device %in% c('postscript','ps','ps.slide')) 'ps' else 
    if(device %in% c('win.slide','win.printer')) 'wmf' else 'gr'
  file   <- paste(if(length(do.prefix))
                    paste(do.prefix,if(under.unix) '.' else '/',sep=''),
                if(device!='ps.slide' && device!='win.slide')
                  paste(pcondition, suffix, sep='.') else
                   pcondition, sep='')
  if(multiplot) {
    if(under.unix) stop('multiplot=T not meaningful under UNIX')
    if(!(device %in% c('win.slide','win.printer')))
      stop('multiplot only meaningful for device=win.slide,win.printer')
    file <- paste(file,'#',sep='')
  }
  get(device)(file, ...)
}

do.echo <- .Options$do.echo
if(length(do.echo)==0) do.echo <- TRUE

do.comments <- .Options$do.comments
if(length(do.comments)==0) do.comments <- FALSE

invis.fctns <- c('plot','lines','points','abline','text','mtext','title',
                 'impute', 'survplot')
## generic functions whose body ends in UseMethod but are invisible
## this list should grow

for(ex in substitute(expressions)) {

  lv <- eval(ex, local=1)
  exs <- strip.comments(ex)
  m <- mode(exs)
  if(m == 'name' || (m=='call' && (length(exs$pl)==0 || 
	(is.logical(exs$pl) && !exs$pl)))) {
	## some functions called to plot (pl=T) - don't auto print results
	inv <- if(m != 'call') FALSE else  { # see if expression is call to function
									 # with body ending in invisible()
	  ex1 <- as.character(exs[1])
	  inv <- if(any(ex1==invis.fctns)) TRUE else
	  if(exists(ex1, mode='function')) {
		f <- get(ex1, mode='function')
		f <- f[[length(f)]]
		f1 <- as.character(f)[1]
		if(f1=='invisible' || f1=='.Cur.pic') TRUE else {
		  m <- mode(f)
		  if(m=='{') {f <- f[[length(f)]]; f1 <- as.character(f)[1]}
		  f1=='invisible' || f1=='.Cur.pic'
		}
	  } else FALSE
	}
	if(!inv) {
	  if(do.echo) { cat('\n'); dput(if(do.comments) ex else exs); cat('\n') }
	  print(lv)
	}
  }
}

if(length(device)) dev.off()

if(do.file!='') {
  sink()
  cat('Print output ',if(append)'appended' else 'written',' to file "',
	  sink.file,'".\n',sep='')
  all.files <- unique(c(.Options$.all.do.files, sink.file))
  options(.all.do.files=all.files, TEMPORARY=FALSE)
  if(under.unix) {
    pwd.home <- unix('pwd;echo $HOME')
    cat('$1', paste(paste(pwd.home[1],all.files,sep='/'), collapse=' '),' &\n',
        file=paste(pwd.home[2],'/.lst',sep=''))
    unix('chmod +x $HOME/.lst')
  }
}

invisible()
}


dot.chart<-function(z, major, minor, fun = mean, subset, pch=18, mkh=.035,
	cex=.5, xlab = label(z), prt=TRUE, ...)
{
count <- function(ww) sum(!is.na(ww))

xl<-xlab
#Note: dotchart does not pass the following parameters to points and mtext
oldpar<-par(mkh=mkh, cex=cex)
if(!missing(subset))	{
	z <- z[subset]
	major <- major[subset]
	if(!missing(minor))minor <- minor[subset]	}
major<-as.category(major)
if(missing(minor)){
	tabl <- tapply(z, list(major), fun)
	tabln <- tapply(z, list(major), count)
	names(tabl) <- levels(major)
	names(tabln) <- levels(major)
	cmajor <- category(row(tabl), label=levels(major))
	dotchart(tabl, labels=levels(cmajor)[cmajor], xlab="", pch=pch,
	 ...)
	}
else {
	minor<-as.category(minor)
	tabl <- tapply(z, list(major, minor), fun)
	tabln <- tapply(z, list(major, minor), count)
	dimnames(tabl) <- list(levels(major),levels(minor))
	dimnames(tabln) <- list(levels(major),levels(minor))
	cminor <- category(col(tabl), label = levels(minor))
	cmajor <- category(row(tabl), label = levels(major))
	dotchart(tabl, labels = levels(cminor)[cminor], groups = cmajor, 
		xlab = "", pch=pch,  ...)
	}
par(oldpar)
if(xl!="" & xl!=" ") title(xlab=xl)

if(prt)	{
	print(xl,quote=FALSE)
	print(tabl,digits=4)
	print("------- n -------",quote=FALSE)
	print(tabln)	}


invisible()
}
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
						 dens.opts=NULL, lwd=1, ...)  {

  datadensity <- match.arg(datadensity)
  colspec <- FALSE
  if(datadensity != 'none') {
	if(side %in% c(2,4))
	  stop('side must be 1 or 3 when datadensity is specified')
	if('frac' %nin% names(dens.opts)) dens.opts$frac <- frac
	if('side' %nin% names(dens.opts)) dens.opts$side <- side
	if('col' %in%   names(dens.opts)) colspec <- TRUE
  }

  if(missing(xlab))	{
#	xlab <- attr(x,"label")  26sep02
#	if(is.null(xlab) || xlab=="")xlab <- deparse(substitute(x))
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

  if(missing(xlim)) xlim <- range(X)

  n <- if(normwt) length(X) else sum(weights[nna])
  m <- (if(normwt) length(nna) else sum(weights, na.rm=TRUE)) - n
  weights <- weights[nna]

  for(i in 1:nlev) {
	s <- group == lev[i]
	x <- X[s]
	wt <- weights[s]

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
		  quant <- if(what=='1-F')min(x[y<=w]) else min(x[y>=w])
		  lines(c(a[1],quant),c(w,w),lty=2,col=1)
		  lines(c(quant,quant),c(w,a[3]),lty=2,col=col[i])
		}
	  }
	}
	curves[[i]] <- list(x=x, y=y)
    if(datadensity!='none') {
	  if(!colspec) dens.opts$col <- col[i]
	  do.call(
			  switch(datadensity, 
					 rug    ='scat1d', hist='histSpike',
					 density='histSpike'),
	  c(list(x=x,add=TRUE),if(datadensity=='density')list(type='density'), dens.opts))
	}
}

  if(nlev > 1 && (is.list(label.curves) || label.curves))
	labcurve(curves, type='s', lty=lty, col=col, opts=label.curves)

  invisible(structure(if(nlev==1) list(x = x, y = y) else curves, 
					  N=list(n=n, m=m)))
}

ecdf.data.frame <- function(x, group=rep(1,nrows), 
							weights=rep(1,nrows), normwt=FALSE,
							label.curves=TRUE, n.unique=10, na.big=FALSE, 
							subtitles=TRUE,  vnames=c("labels","names"),
                            ...) {
  vnames <- match.arg(vnames)
  mf <- par('mfrow')
  if(length(mf)==0) mf <- c(1,1)

  g <- function(v, n.unique) {   ## 7sep02
    if(is.character(v) || is.category(v)) return(FALSE)
    length(unique(v[!is.na(v)])) >= n.unique
  }
  use <- sapply(x, g, n.unique=n.unique)
  automf <- FALSE  ## 22sep02
  if((la <- sum(use)) > 1 & max(mf)==1) {
    mf <- if(la<=4)c(2,2) else if(la<=6)c(2,3) else if(la<=9)c(3,3) else
    if(la<=12)c(3,4) else if(la<=16) c(4,4) else c(4,5)
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
#    lab <- attr(v,"label") 26sep02
    lab <- if(vnames=='names') nam[j]
    else label(v, units=TRUE, plot=TRUE, default=nam[j]) 
    z <- ecdf(v, group=group, weights=weights, normwt=normwt, 
              xlab=lab, label.curves=label.curves, 
              subtitles=subtitles, ...)
    if(na.big) {
      m <- attr(z,'N')$m
      if(m > 0) mtext(paste(m,"NAs"),line=-2,cex=1)
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


prepanel.ecdf <- function(x, y, fun, ...) {
  xlim <- range(x,na.rm=TRUE)
  ylim <- fun(c(0,1))
  if(any(is.infinite(ylim))) ylim <- fun(c(.001,.999))   # was inf 18Mar02
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
					   col = NULL, ...) {
  ## y duplicates x in S-Plus
method <- match.arg(method)
if(length(groups)) groups <- as.factor(groups)

if(!.R.) llines <- lines
if(.R.) type <- 's'   # lattice histogram sets to 'percent'

##g <- if(length(groups)) oldUnclass(groups[subscripts]) else NULL
g <- oldUnclass(groups)[subscripts]
ng <- if(length(groups)) max(g, na.rm=TRUE) else 1  ## na.rm 8Aug00

plot.symbol <- trellis.par.get(if(ng>1)"superpose.symbol" else "plot.symbol")
plot.line   <- trellis.par.get(if(ng>1)"superpose.line"   else "plot.line")

qrefs <- function(x, q, col, fun, llines, grid) {
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
				   frac=switch(datadensity,none=NA,rug=.03,hist=.1,density=.1),
				   dens.opts=NULL, llines, ...) {
  ## y ignored
  z <- wtd.ecdf(x, type=ecdf.type, na.rm=FALSE)
  ## For some reason S-Plus will not plot anything the following way
  ## when lwd is a variable
##  llines(z$x, fun(z$ecdf), lwd = lwd, lty = lty, col = col,
##         type = type, ...)
  do.call('llines', list(z$x, fun(z$ecdf), lwd = lwd, lty = lty, col = col,
                         type = type, ...))
  if(length(q)) qrefs(x, q, col, fun=fun, llines=llines, grid=.R.)
  datadensity <- match.arg(datadensity)
  if(datadensity != 'none') {
	if(side %in% c(2,4))
	  stop('side must be 1 or 3 when datadensity is specified')
	if('frac' %nin% names(dens.opts)) dens.opts$frac <- frac
	if('side' %nin% names(dens.opts)) dens.opts$side <- side
	if('col'  %nin% names(dens.opts)) dens.opts$col  <- col
	if('lwd'  %nin% names(dens.opts)) dens.opts$lwd  <- lwd
	do.call(
			switch(datadensity, 
				   rug    ='scat1d', hist='histSpike',
				   density='histSpike'),
	  c(list(x=x,add=TRUE,grid=.R.),
        if(datadensity=='density')list(type='density'),
        dens.opts))
  }
}

pspanel <- function(x, subscripts, groups, type, lwd, lty,
					pch, cex, font, col, q, qrefs, 
					ecdf.type, fun, llines, ...) {
  ## y ignored
  lev <- levels(groups)
  groups <- as.numeric(groups)[subscripts]
  N <- seq(along = groups)
  ##    curves <- vector('list', length(lev))             ## 19Mar02
  curves <- list()  ## 31aug02
  ##    names(curves) <- lev                              ## 19Mar02 31aug02
    
##	for(i in sort(unique(groups))) {                  ## 19Mar02
    for(i in 1:length(lev)) {
##      if(is.na(i)) next   ## 8Aug00                 ## 19Mar02
	  which <- N[groups == i]	# j <- which[order(x[which])]	
										# sort in x
	  j <- which	# no sorting
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
if(!length(col)) col <- plot.line$col
col <- rep(col, length = ng)

if(ng > 1) {
  levnum <- sort(unique(g))
  curves <- pspanel(x, subscripts, groups,    ## rm y 19Mar02
                    lwd=lwd, lty=lty, pch=pch, cex=cex, 
                    font=font, col=col, type=type, q=q, qrefs=qrefs, 
                    ecdf.type=method, fun=fun, llines=llines)
  if(!(is.logical(label.curves) && !label.curves)) {
    lc <- if(is.logical(label.curves)) list(lwd=lwd, cex=cex[1]) else
	      c(list(lwd=lwd, cex=cex[1]), label.curves)
##    curves <- vector('list',length(levnum)); names(curves) <- levels(groups
## 19Mar02
##	i <- 0
##	for(gg in levnum) {
##	  i <- i+1
##	  s <- g==gg
##	  curves[[i]] <- list(x[s], y[s])
##	}
	labcurve(curves, lty=lty[levnum], lwd=lwd[levnum], col=col[levnum], 
			 opts=lc, grid=.R., ...)
  }
} else ppanel(x,
			  lwd=lwd, lty=lty, pch=pch, cex=cex, 
			  font=font, col=col, type=type, q=q, qrefs=qrefs, 
			  ecdf.type=method, fun=fun, llines=llines, ...) ## rm y 19Mar02 

if(ng>1) { ##set up for key() if points plotted
    if(.R.) {
      Key <- function(x=0, y=1, lev, col, lty, lwd, ...) {
        oldpar <- par(usr=c(0,1,0,1),xpd=NA)
        ## Even though par('usr') shows 0,1,0,1 after lattice draws
        ## its plot, it still needs resetting
        on.exit(par(oldpar))
        if(is.list(x)) { y <- x[[2]]; x <- x[[1]] }
        if(!length(x)) x <- 0
        if(!length(y)) y <- 1  ## because of formals()
        rlegend(x, y, legend=lev, lty=lty, lwd=lwd, col=col)
        invisible()
      }
 } else {
   Key <- function(x=NULL, y=NULL, lev, col, lty, lwd, ...) {
     if(length(x)) {
       if(is.list(x)) {y <- x$y; x <- x$x}
       key(x=x, y=y, text=list(lev, col=col), 
           lines=list(col=col,lty=lty,lwd=lwd),
           transparent=TRUE, ...) } else
     key(text=list(lev, col=col), 
         lines=list(col=col,lty=lty,lwd=lwd),transparent=TRUE, ...)
     invisible()
   }
 }
    formals(Key) <- list(x=NULL, y=NULL, lev=levels(groups), col=col,
                         lty=lty, lwd=lwd,...=NULL)
    storeTemp(Key)
  }
}

ecdf.formula <- function(x,
                         data = sys.frame(sys.parent()), 
						 groups = NULL, 
						 prepanel=prepanel.ecdf, panel=panel.ecdf, ..., 
						 xlab, ylab, fun=function(x)x, subset=TRUE) {
  if(.R.) {
    require('grid')
    require('lattice')
    vars <- var.inner(x)
    xname <- vars[1]
    if(missing(xlab))
      xlab <- label(eval(parse(text=vars[1]), data),
                    units=TRUE, plot=TRUE, default=xname, grid=TRUE)
#      xlab <- attr(eval(parse(text=vars[1]), data),'label') 26sep02
  } else {
  vars <- attr(terms.inner(x),'variables')
  xname <- as.character(vars[1])
  if(missing(xlab))
    xlab <- label(eval(vars[1], data), units=TRUE, plot=TRUE,
                  default=xname)
#    xlab <- attr(eval(vars[1], data),'label') 26sep02
}
  if(missing(ylab)) 
	ylab <- if(missing(fun))paste('Proportion <=',xname) else ''
  subset <- eval(substitute(subset), data)

  if(.R.)
    do.call("histogram",
            c(list(formula=x, data=data,
                   prepanel=prepanel, panel=panel,
                   ylab=ylab, xlab=xlab, fun=fun),
              ## was jyst groups=groups 31aug02
              if(!missing(groups))list(groups=eval(substitute(groups),data)),
              if(!missing(subset))list(subset=subset),
              list(...))) else  {
    prepanel$fun <- fun
    ## argument not transmitted for some reason
    setup.2d.trellis(x, data = data,
                     prepanel=prepanel, panel=panel,
                     xlab=xlab, ylab=ylab, fun=fun,
                     groups = eval(substitute(groups),  data),
                     ..., subset = subset)
  }
}


eip <- function(name) {
name <- as.character(substitute(name))
f <- find(name)
if(length(f)!=1) stop('object must exist in exactly one place')
## g <- if(under.unix) jove(get(name)) else edit(get(name))  16Apr02
g <- edit(get(name))
if(.R.) assign(name, g, pos=match(f,search())) else assign(name, g, where=f)
cat('Object', name, 'stored in', f, '\n')
invisible()
}
#From: geyer@galton.uchicago.edu
#Modified 11May91 FEH - added na.rm to range()
#Modified 12Jul91 FEH - added add=T and lty=1 parameters
#Modified 12Aug91 FEH - added explicit ylim parameter
#Modified 26Aug94 FEH - added explicit lwd parameter for segments()
#FEH 2Jul02 added horizontal charts with differences on 2nd axis

errbar <- function(x, y, yplus, yminus, cap=.015,
                   xlab=as.character(substitute(x)),
                   ylab=if(is.factor(x) || is.character(x))'' else
                        as.character(substitute(y)), add=FALSE, lty=1, ylim, 
                   lwd=1, Type=rep(1,length(y)), ... ) {
  if(missing(ylim))
    ylim <- range(y[Type==1],yplus[Type==1],yminus[Type==1],na.rm=TRUE)
  if(is.factor(x) || is.character(x)) {
    x <- as.character(x)
    n <- length(x)
    t1 <- Type==1
    t2 <- Type==2
    n1 <- sum(t1)
    n2 <- sum(t2)

    omai <- par('mai')
    mai <- omai
    mai[2] <- max(strwidth(x, 'inches'))+.25*.R.
    par(mai=mai)
    on.exit(par(mai=omai))
    plot(0,0,xlab=ylab,ylab='',xlim=ylim,ylim=c(1,n+1),axes=FALSE)
    axis(1)
    w <- if(any(t2))n1+(1:n2)+1 else numeric(0)
    axis(2, at=c(1:n1,w), labels=c(x[t1],x[t2]),
         las=1,adj=1)
    points(y[t1], 1:n1, pch=16)
    segments(yplus[t1], 1:n1, yminus[t1], 1:n1)

    if(any(Type==2)) {
      abline(h=n1+1, lty=2)
      offset <- mean(y[t1]) - mean(y[t2])
      if(min(yminus[t2]) < 0 & max(yplus[t2]) > 0)
        lines(c(0,0)+offset, c(n1+1,par('usr')[4]), lty=2)
      points(y[t2] + offset, w, pch=16)
      segments(yminus[t2]+offset, w, yplus[t2]+offset, w)
      at <- pretty(range(y[t2],yplus[t2],yminus[t2]))
      axis(3, at=at+offset, label=format(round(at,6)))
    }
    return(invisible())
  }
  if(!add) plot( x, y, ylim=ylim, xlab=xlab, ylab=ylab, ... )
  xcoord <- par()$usr[1:2]
  segments( x, yminus, x, yplus , lty=lty, lwd=lwd)
  smidge <- cap * ( xcoord[2] - xcoord[1] ) / 2
  segments( x - smidge, yminus, x + smidge, yminus, lwd=lwd)
  segments( x - smidge, yplus, x + smidge, yplus, lwd=lwd)
  invisible()
}
### event.chart.q: eventchart program 1.0 (creates function event.chart)
###
### last edited: 9-27-97
### last edited: 10-20-98, add pty='m' for the default plotting;
###      one may change to pty='s' to get the 'square plot' for the Goldman's Event Chart                      
### FEH changes 9may02 for R

event.chart <-  function(
        data, subset.r = 1:dim(data)[1], subset.c = 1:dim(data)[2], 

        sort.by = NA, sort.ascending = TRUE, 
        sort.na.last = TRUE, sort.after.subset = TRUE,
        y.var = NA, y.var.type = 'n', 
        y.jitter = FALSE, y.jitter.factor = 1,
        y.renum = FALSE, NA.rm = FALSE, x.reference = NA, 
        now = max(data[,subset.c], na.rm = TRUE),
        now.line = FALSE, now.line.lty = 2,
        now.line.lwd = 1, now.line.col = 1, pty='m',
        date.orig = c(1,1,1960), titl = 'Event Chart',

        y.idlabels = NA, y.axis = 'auto', 
        y.axis.custom.at = NA, y.axis.custom.labels = NA, 
        y.julian = FALSE, y.lim.extend = c(0,0),
        y.lab = ifelse(is.na(y.idlabels), '' , as.character(y.idlabels)), 

        x.axis.all = TRUE, x.axis = 'auto', 
        x.axis.custom.at = NA, x.axis.custom.labels = NA, 
        x.julian = FALSE, x.lim.extend = c(0,0), x.scale = 1,
        x.lab = ifelse(x.julian, 'Follow-up Time', 'Study Date'),

        line.by = NA, line.lty = 1, line.lwd = 1, line.col = 1,
        line.add = NA, line.add.lty = NA, 
        line.add.lwd = NA, line.add.col = NA, 
        point.pch = 1:length(subset.c),
        point.cex = rep(0.6,length(subset.c)),
        point.col = rep(1,length(subset.c)),

        legend.plot = FALSE, legend.location = 'o', legend.titl = titl, 
        legend.titl.cex = 3.0, legend.titl.line = 1.0, 
        legend.point.at = list(x = c(5,95), y = c(95,30)),
        legend.point.pch = point.pch,
        legend.point.text = ifelse(rep(is.data.frame(data),
              length(subset.c)), names(data[,subset.c]), subset.c), 
        legend.cex = 2.5, legend.bty = 'n',
        legend.line.at = list(x = c(5,95), y = c(20,5)),
        legend.line.text = names(table(as.character(data[,line.by]),
              exclude = c('','NA'))), 
        legend.line.lwd = line.lwd, legend.loc.num = 1, 

        ...)

{

  legnd <- function(..., pch) {
    if(missing(pch)) legend(...) else
    if(.R.) legend(..., pch=pch) else
    legend(..., marks=pch)
  }
  if(.R.) {
  month.day.year <- function(jul, origin.) {
    if(missing(origin.) || is.null(origin.))
      if(is.null(origin. <- .Options$chron.origin))
        origin. <- c(month = 1, day = 1, year = 1960)
    if(all(origin. == 0))
      shift <- 0
    else shift <- julian(origin = origin.)
    ## relative origin
    ## "absolute" origin
    j <- jul + shift
    j <- j - 1721119
    y <- (4 * j - 1) %/% 146097
    j <- 4 * j - 1 - 146097 * y
    d <- j %/% 4
    j <- (4 * d + 3) %/% 1461
    d <- 4 * d + 3 - 1461 * j
    d <- (d + 4) %/% 4
    m <- (5 * d - 3) %/% 153
    d <- 5 * d - 3 - 153 * m
    d <- (d + 5) %/% 5
    y <- 100 * y + j
    y <- y + ifelse(m < 10, 0, 1)
    m <- m + ifelse(m < 10, 3, -9)
    list(month = m, day = d, year = y)
  }

  ## julian.r
  ## Convert between Julian and Calendar Dates
  
  julian <- function(m, d, y, origin.) {
    only.origin <- all(missing(m), missing(d), missing(y))
    if(only.origin)
      m <- d <- y <- NULL
    ## return days since origin
    if(missing(origin.)) if(is.null(origin. <- .Options$chron.origin))
      origin. <- c(month = 1, day = 1, year = 1960)
    nms <- names(d)
    max.len <- max(length(m), length(d), length(y))
    ##
    ## prepend new origin value and rep out to common max. length:
    m <- c(origin.[1], rep(m, length = max.len))
    d <- c(origin.[2], rep(d, length = max.len))
    y <- c(origin.[3], rep(y, length = max.len))
    ##
    ## code from julian date in the S book (p.269)
    ##
    y <- y + ifelse(m > 2, 0, -1)
    m <- m + ifelse(m > 2, -3, 9)
    c <- y %/% 100
    ya <- y - 100 * c
    out <- (146097 * c) %/% 4 + (1461 * ya) %/% 4 + (153 * m + 2) %/% 5 +
      d + 1721119
    ##
    ## now subtract the new origin from all dates
    ##
    if(!only.origin) {
      if(all(origin. == 0))
        out <- out[-1]
      else out <- out[-1] - out[1]
    }
    names(out) <- nms
    out
  }
}

### stop function if unacceptable violations occur 
###   (other stops may occur later)
   if(!is.matrix(data) && !is.data.frame(data))
     stop("argument data must be a matrix or a data frame\n")
   ## added is.data.frame 9may02 FEH

### section 1:	do necessary subsetting and sorting of data
   targodata <- apply(data[, subset.c, drop = FALSE], 2, as.numeric)
     ### targodata for target 'overall' data
   if(!is.na(x.reference))
     targodata <- apply(targodata - data[, x.reference], 2, as.numeric)

### start of sort routine
   if(!is.na(sort.by[1]))
    {	
     if(sort.after.subset == TRUE)
       data <- data[subset.r, ]

     m <- dim(data)[1]
     keys <- 1:m
     rotate <- m:1
     length.sort.by <- length(sort.by)
     asc <- rep(sort.ascending, length.sort.by)
     for (i in length.sort.by:1)
      {
       if(asc[i])
         keys[] <- keys[sort.list(data[, sort.by[[i]]][keys], 
                   na.last = sort.na.last)]
         else keys[] <- keys[order(data[, sort.by[[i]]][keys],
                        rotate, na.last = sort.na.last)[rotate]]
      }
     data <- data[keys, ]

     if(sort.after.subset == FALSE)
      {
       subset.r <- (1:dim(data)[1])[subset.r]
       targdata <- apply(data[subset.r, subset.c, drop = FALSE], 2, as.numeric)
      }
     else if(sort.after.subset == TRUE)
      {
       targdata <- apply(data[, subset.c, drop = FALSE], 2, as.numeric)
       subset.ro <- (1:dim(data)[1])[subset.r]	
       subset.r <- seq(length(subset.ro))
      }
    }
   else if(is.na(sort.by[1]))
    {
     subset.r <- (1:dim(data)[1])[subset.r]
     targdata <- apply(data[subset.r, subset.c, drop = FALSE], 2, as.numeric)
    }
### end of sort routine

### start to deal with missing values and renumbering y-axis
   if(NA.rm == TRUE)
    {
     whotoplot <- subset.r[!(apply(is.na(targdata),1,all))]
	### whotoplot is for use for data matrix(dataframe); 
	###  essentially who will be plotted from data
     t.whotoplot <- seq(dim(targdata)[1])[!(apply(is.na(targdata),1,all))]
	### t.whotoplot is for use for targdata matrix(dataframe);
	###  essentially, who will be plotted from targdata 
     if(y.renum == TRUE)
       {
        whattoplot <- seq(subset.r[!(apply(is.na(targdata),1,all))])
	   ### whattoplot is what will be plotted on y-axis of event chart
       }
     else if(y.renum == FALSE)
      {
       if((!is.na(sort.by[1]) & sort.after.subset == FALSE) | (is.na(sort.by[1])))
         whattoplot <- subset.r[!(apply(is.na(targdata),1,all))]
       else if(!is.na(sort.by[1]) & sort.after.subset == TRUE)
	 whattoplot <- subset.ro[!(apply(is.na(targdata),1,all))]
      }
    }
   else if(NA.rm == FALSE)
    {
     whotoplot <- subset.r
     t.whotoplot <- seq(dim(targdata)[1])
     if(y.renum == TRUE)
       whattoplot <- seq(subset.r)
     else if(y.renum == FALSE)
      {
       if((!is.na(sort.by[1]) & sort.after.subset == FALSE)  |  (is.na(sort.by[1])))
	 whattoplot <- subset.r
       else if(!is.na(sort.by[1]) & sort.after.subset == TRUE)
	 whattoplot <- subset.ro
      }
    }
### end of dealing with missing values and renumbering of y-axis


### section 2:	  perform necessary manipulations using x.reference and y.var

### deal with re-referencing x-axis with x.reference
   if(!is.na(x.reference))
    {
     targdata <- apply(targdata - data[subset.r, x.reference], 2, as.numeric)
     if(NA.rm == TRUE)
      {
       x.referencew <- data[whotoplot, x.reference]
       whotoplot <- whotoplot[!is.na(x.referencew)]
       t.whotoplot <- t.whotoplot[!is.na(x.referencew)]
       whattoplot.ref <- whattoplot[!is.na(x.referencew)]
       if(y.renum == FALSE)
        {
         if((!is.na(sort.by[1]) & sort.after.subset == FALSE) | (is.na(sort.by[1])))
          whattoplot <- seq(subset.r[1],
                           subset.r[1] + length(whattoplot.ref) - 1)
         else if(!is.na(sort.by[1]) & sort.after.subset == TRUE)
          whattoplot <- seq(subset.ro[1],
                           subset.ro[1] + length(whattoplot.ref) - 1)     
        }
       else if(y.renum == TRUE)
        whattoplot <- seq(length(whattoplot.ref))
      }
    }     	

### deal with using a data frame variable to place lines on y-axis
   if(!is.na(y.var))
    {
     if(!is.na(sort.by[1]))
      stop("cannot use sort.by and y.var simultaneously\n")
     y.varw <- as.numeric(data[whotoplot, y.var])
     whotoplot <- whotoplot[!is.na(y.varw)]
     t.whotoplot <- t.whotoplot[!is.na(y.varw)]
     whattoplot <- y.varw[!is.na(y.varw)]
     if(y.jitter == TRUE)
       {
        range.data <- diff(range(whattoplot))
        range.unif <- y.jitter.factor * 
                      (range.data / (2 * (length(whattoplot) - 1)))
        whattoplot <- whattoplot + 
                      runif(length(whattoplot), -(range.unif), range.unif)
       }
    }

   sort.what <- sort(whattoplot)
   length.what <- length(whattoplot)	


### section 3:	perform all plotting	

### first, make sure length of point.pch, cex, col is same as subset.c

  len.c <- length(subset.c)

  if(length(point.pch) < len.c)
   {
    warning("length(point.pch) < length(subset.c)") 
    point.pch <- rep(point.pch, len.c)[1:len.c]
   }
  if(length(point.cex) < len.c)
   {
    warning("length(point.cex) < length(subset.c)") 
    point.cex <- rep(point.cex, len.c)[1:len.c]
   }
  if(length(point.col) < len.c)
   {
    warning("length(point.col) < length(subset.c)") 
    point.col <- rep(point.col, len.c)[1:len.c]
   }

### set default of par(new=F)
   par(new = FALSE)

### plot external legend (if requested)
   if(legend.plot == TRUE  && legend.location == 'o')
    {
     plot(1, 1, type = 'n', xlim = c(0,100), ylim = c(0,100),
	  axes = FALSE, xlab = '', ylab = '')
     mtext(legend.titl, line = legend.titl.line, 
           outer = FALSE, cex = legend.titl.cex)
     legnd(legend.point.at[[1]], legend.point.at[[2]],
           leg = legend.point.text,
           pch = legend.point.pch, cex = legend.cex, # was marks= 9may02
           col = point.col, bty = legend.bty)
     if(!is.na(line.by))
      {
       par(new = TRUE)
       legnd(legend.line.at[[1]], legend.line.at[[2]],
             leg = legend.line.text, cex = legend.cex,
             lty = line.lty, lwd = legend.line.lwd,
             col = line.col, bty = legend.bty)
      }
     invisible(if(.R.)par(ask=TRUE) else dev.ask(TRUE))  ## FEH 9may02
    }
				
### start creating objects to be used in determining plot region

   targdata <- targdata /  x.scale
   targodata <- targodata / x.scale 

   minvec <- apply(targdata[t.whotoplot,, drop = FALSE], 1, min, na.rm = TRUE)
   minotime <- ifelse(x.axis.all,
                     min(apply(targodata,1,min,na.rm = TRUE), na.rm = TRUE),
                     min(minvec, na.rm = TRUE))
   maxvec <- apply(targdata[t.whotoplot,, drop = FALSE], 1, max, na.rm = TRUE)
   maxotime <- ifelse(x.axis.all,
                     max(apply(targodata,1,max,na.rm = TRUE), na.rm = TRUE),
                     max(maxvec, na.rm = TRUE))

### determine par parameters and plot graphical region based 
###  on request of y.var and, subsequently, y.var.type and now.line

   y.axis.top <- sort.what[length.what] + y.lim.extend[2]
   y.axis.bottom <- sort.what[1] - y.lim.extend[1]
   x.axis.right <- maxotime + x.lim.extend[2]
   x.axis.left <- minotime - x.lim.extend[1]

   if(!is.na(y.var) & y.var.type == 'd')
    {
     oldpar <- par(omi = rep(0,4), lwd = .6, 
                  mgp = c(3.05,1.1,0), tck = -0.006, ...)
### set pty
     par(pty=pty)
     plot(whattoplot, type = 'n',
	  xlim = c(x.axis.left, 
                   ifelse(now.line, (now - (min(data[, subset.c], na.rm=TRUE))) / 
                   x.scale, x.axis.right)),ylim = c(y.axis.bottom,
                   ifelse(pty=='s', now, y.axis.top)),
	  	   xlab = x.lab, ylab = y.lab, axes = FALSE)
     if(now.line == TRUE)
       abline(now, ((sort.what[1] - now) / 
              (((now - min(data[, subset.c], na.rm=TRUE)) / x.scale) - minotime)),
	      lty = now.line.lty, lwd = now.line.lwd, col = now.line.col)
    }
   else if(is.na(y.var)  |  (!is.na(y.var) & y.var.type == 'n'))
    {
     if(now.line == TRUE)
      stop("with now.line==T, y.var & y.var.type=='d' must be specified\n")
     oldpar <- par(omi = rep(0, 4), lwd = .6, 
                  mgp = c(2.8,1.1,0), tck = -0.006, ...)
     plot(whattoplot, type = 'n',
	  xlim = c(x.axis.left, x.axis.right),
	  ylim = c(y.axis.bottom - 1, y.axis.top + 1),
	  xlab = x.lab, ylab = y.lab, axes = FALSE)
    }	
	
### plot y-axis labels

   if(!is.na(y.idlabels))
    {
     if(!is.na(y.var))
      {
       warning("y.idlabels not used when y.var has been specified\n")
       axis(side = 2)
      }
     else if(is.na(y.var))
       axis(side = 2, at = whattoplot,
            labels = as.vector(data[whotoplot, y.idlabels]))
    }
   else if(is.na(y.idlabels))
    {
     if(y.axis == 'auto')
      {
       if(is.na(y.var)  |  (!is.na(y.var) & y.var.type == 'n'))
         axis(side = 2) 
       else if(!is.na(y.var) & y.var.type == 'd')
        {
         if(y.julian == FALSE)
          {
           y.axis.auto.now.bottom <- ifelse(now.line, sort.what[1],
                                     y.axis.bottom)        
### marked by JJL, disable square plot ###                       y.axis.auto.now.top <- ifelse(now.line, now, y.axis.top)
           y.axis.auto.now.top <- ifelse(now.line, y.axis.top, y.axis.top)
           y.axis.auto.at <- round(seq(y.axis.auto.now.bottom,
                            y.axis.auto.now.top, length = 5))
           y.axis.auto.labels <- paste(month.day.year(
                    y.axis.auto.at, origin=date.orig)$month,'/',
                    month.day.year(y.axis.auto.at, origin=date.orig)$day,'/',
                    substring(month.day.year(y.axis.auto.at,
                    origin=date.orig)$year,3,4), sep='')
           axis(side = 2, at = y.axis.auto.at, labels = y.axis.auto.labels)
          }
         else if(y.julian == TRUE)
           axis(side = 2)
        }
      }
     else if(y.axis == 'custom')
      {
       if(is.na(y.axis.custom.at[1]) || is.na(y.axis.custom.labels[1]))
         stop("with y.axis == 'custom', must specify y.axis.custom.at and y.axis.custom.labels\n")
       axis(side = 2, at = y.axis.custom.at, labels = y.axis.custom.labels)
      }
    }
	
### plot x-axis labels

   if(x.axis == 'auto')
    {
     if(x.julian == FALSE)      
      {
       x.axis.auto.at <- round(seq(x.axis.left, x.axis.right,
                         length = 5))
       x.axis.auto.labels <- paste(month.day.year(x.axis.auto.at, 
                   origin=date.orig)$month,'/',
	           month.day.year(x.axis.auto.at, origin=date.orig)$day,'/',
	           substring(month.day.year(x.axis.auto.at,
	           origin=date.orig)$year,3,4), sep='')
       axis(side = 1, at = x.axis.auto.at, labels = x.axis.auto.labels)
      }
     else if(x.julian == TRUE)
       axis(side = 1)
    }
   else if(x.axis == 'custom')
    {
     if(is.na(x.axis.custom.at[1]) || is.na(x.axis.custom.labels[1]))
       stop("with x.axis = 'custom', user must specify x.axis.custom.at and x.axis.custom.labels\n")
     axis(side = 1, at = x.axis.custom.at, labels = x.axis.custom.labels)
    }
	
   if(!is.na(titl)) {title(titl)}


### plot lines and points
	
   if(!is.na(line.by))
    {
     line.byw <- data[whotoplot, line.by]
     table.by <- table(as.character(line.byw), exclude = c('','NA'))
     names.by <- names(table.by)
     len.by <- length(table.by)
     if(length(line.lty) < len.by)
       warning("user provided length(line.lty) < num. of line.by categories")
     if(length(line.lwd) < len.by)
       warning("user provided length(line.lwd) < num. of line.by categories")
     if(length(line.col) < len.by)
       warning("user provided length(line.col) < num. of line.by categories")
     line.lty <- rep(line.lty, len=len.by)
     line.lwd <- rep(line.lwd, len=len.by)
     line.col <- rep(line.col, len=len.by)
     lbt.whotoplot <- (1:(length(t.whotoplot)))[
	      as.character(line.byw) != '' & as.character(line.byw) != 'NA']
     for(i in lbt.whotoplot)
      {
       lines(c(minvec[i], maxvec[i]), rep(whattoplot[i],2),
             lty=as.vector(line.lty[names.by==line.byw[i]]),
             lwd=as.vector(line.lwd[names.by==line.byw[i]]),
             col=as.vector(line.col[names.by==line.byw[i]]))
      }
    }
   else if(is.na(line.by))
    {
     for(i in 1:length(t.whotoplot))
       lines(c(minvec[i], maxvec[i]), rep(whattoplot[i],2),
             lty=line.lty[1], lwd=line.lwd[1], col=line.col[1])
    }

   for(j in 1:dim(targdata)[2])
       points(as.vector(unlist(targdata[t.whotoplot,j])), whattoplot, 
	      pch=point.pch[j], cex=point.cex[j],
       col=point.col[j])
   ## removed mkh=0 FEH 9may02

### add line.add segments (if requested)

   if(!is.na(as.vector(line.add)[1]))
    {
     if(any(is.na(line.add.lty))) 
        stop("line.add.lty can not have missing value(s) with non-missing line.add\n")
     if(any(is.na(line.add.lwd)))
        stop("line.add.lwd can not have missing value(s) with non-missing line.add\n")
     if(any(is.na(line.add.col)))
        stop("line.add.col can not have missing value(s) with non-missing line.add\n")
  
     line.add.m <- as.matrix(line.add)
     dim.m <- dim(line.add.m)

     if(dim.m[1] != 2)
       stop('line.add must be a matrix with two rows\n')
     if(length(line.add.lty)!=dim.m[2]) 
       stop("length of line.add.lty must be the same as number of columns in line.add\n")
     if(length(line.add.lwd)!=dim.m[2])
       stop("length of line.add.lwd must be the same as number of columns in line.add\n")
     if(length(line.add.col)!=dim.m[2])
       stop("length of line.add.col must be the same as number of columns in line.add\n")

     for(j in (1:dim.m[2]))
      {
       for(i in (1:length(t.whotoplot)))
        {
         add.var1 <- subset.c == line.add.m[1,j]
         if (any(add.var1)==FALSE) stop("variables chosen in line.add must also be in subset.c\n")
         add.var2 <- subset.c == line.add.m[2,j]
         if (any(add.var2)==FALSE) stop("variables chosen in line.add must also be in subset.c\n")
         segments(targdata[i, (1:len.c)[add.var1]], whattoplot[i], 
          targdata[i, (1:len.c)[add.var2]], whattoplot[i],
          lty = line.add.lty[j], lwd = line.add.lwd[j],
          col = line.add.col[j])
        }
      }
     }

### plot internal legend (if requested)

   if(legend.plot == TRUE  &  legend.location != 'o') 
    {
     if(legend.location == 'i')
      {
       legnd(legend.point.at[[1]], legend.point.at[[2]], 
             leg = legend.point.text,
             pch = legend.point.pch, cex = legend.cex,  # marks 9may02
             col = point.col, bty = legend.bty)
       if(!is.na(line.by))
         legnd(legend.line.at[[1]], legend.line.at[[2]],
               leg = legend.line.text, cex = legend.cex,
               lty = line.lty, lwd = legend.line.lwd,
               col = line.col, bty = legend.bty)
      }
     else if(legend.location == 'l')
      {
      cat('Please click at desired location to place legend for points.\n') 
       legnd(locator(legend.loc.num), leg = legend.point.text,
             pch = legend.point.pch, cex = legend.cex,  # marks 9may02
             col = point.col, bty = legend.bty)
       if(!is.na(line.by))
        {
        cat('Please click at desired location to place legend for lines.\n')
         legnd(locator(legend.loc.num), leg = legend.line.text, 
               cex = legend.cex, lty = line.lty, 
               lwd = legend.line.lwd, col = line.col, bty = legend.bty)
        }
      }
    }

### add box to main plot and clean up
	
   invisible(box())
   invisible(if(.R.)par(ask=FALSE) else dev.ask(FALSE))  ## FEH 9may02
   par(oldpar)

}

# event.convert.s
# convert 2-column coded events to multiple event time for event.chart()
# input: a matrix or dataframe with at least 2 columns
#        by default, the first column contains the event time and
#                    the second column contains the k event codes (e.g. 1=dead, 0=censord)
# ouput: a matrix of k columns, each column contains the time of kth coded event
#        
event.convert <- function(data2, event.time = 1, event.code = 2)
{
        dim.d <- dim(data2)
        len.t <- length(event.time)
        if(len.t != length(event.code))
                stop("length of event.time and event.code must be the same")
        if(any(event.time > dim.d[2]))
                stop(paste("Column(s) in event.time cannot be greater than ", 
                        dim.d[2]))
        if(any(event.code > dim.d[2]))
                stop(paste("Column(s) in event.code cannot be greater than ", 
                        dim.d[2]))
        name.data <- names(data2)[event.time]
        if(is.null(name.data)) {
                name.data <- paste("V", event.time, sep = "")
        }
        n.level <- rep(NA, len.t)
        for(i in (1:len.t)) {
                n.level[i] <- length(table(data2[, event.code[i]]))
        }
        tot.col <- sum(n.level)
        data.out <- matrix(NA, dim.d[1], tot.col)
        name.col <- rep(NA, tot.col)
        n.col <- 1
        for(i in (1:len.t)) {
                tab.d <- table(data2[, event.code[i]])
                if(is.null(oldClass(data2[, event.code[i]])))
                        level.value <- as.numeric(names(tab.d))
                else level.value <- names(tab.d)
                for(j in (1:length(tab.d))) {
                        data.out[, n.col] <- rep(NA, dim.d[1])
                        check <- data2[, event.code[i]] == level.value[j]
                        check[is.na(check)] <- FALSE
                        data.out[, n.col][data2[, event.code[i]] == level.value[
                                j]] <- data2[, event.time[i]][check]
                        name.col[n.col] <- paste(name.data[i], ".", names(tab.d
                                )[j], sep = "")
                        n.col <- n.col + 1
                }
        }
        dimnames(data.out) <- list(1:dim.d[1], name.col)
        return(as.matrix(data.out))
}
### event.history-sim-request.txt: s-plus code to make event history graphs
###	(for distribution, including SIM readers)
###	 last edited: 09-28-01

### start event.history function 
###	--> assume data is approporately pre-processed (e.g., smoothed) 
###		prior to function call
 

event.history <- function(data, survtime.col, surv.col, 
	surv.ind = c(1,0), 
	subset.rows = NULL, 
	covtime.cols = NULL, cov.cols = NULL, 
	num.colors = 1, cut.cov = NULL, colors = 1, 
	cens.density = 10, mult.end.cens = 1.05,
	cens.mark.right = FALSE, cens.mark = '-', 
	cens.mark.ahead = .5, cens.mark.cutoff = -1e-8, cens.mark.cex = 1.0, 
	x.lab = 'time under observation', 
	y.lab = 'estimated survival probability', 
	title = 'event history graph', 
	...)

{


## if covtime.cols was assigned a single zero, then
##  make it a one-column matrix of zeroes:
if(is.null(covtime.cols))
  covtime.cols <- as.matrix(rep(0, dim(data)[1]))

## do necessary subsetting
if(!is.null(subset.rows))
 {
  data <- data[subset.rows,]
  surv.col  <- surv.col[subset.rows]
  survtime.col  <- survtime.col[subset.rows]
  covtime.cols <- covtime.cols[subset.rows,]
  if(!is.null(cov.cols))
	cov.cols  <- cov.cols[subset.rows,]
 }



## put in stops signifying 'illegal' data
if(any(is.na(surv.col)))
  stop('cannot have NA entries in surv.col column \n')

if(any(is.na(survtime.col)))
  stop('cannot have NA entries in survtime.col column \n')

if(min(survtime.col) < 0)
  stop('survtime.col observations cannot be < 0 \n')

if(min(covtime.cols, na.rm = TRUE) < 0)
  stop('covtime.cols observations cannot be < 0 \n')



## create color-covariate cutting based on subset data, as desired
if(is.null(cov.cols))
  colors.cat <- matrix(1, nrow=dim(data)[1])
else
 {
  if(is.null(cut.cov))
    colors.cat <- matrix(as.numeric(cut(cov.cols, breaks = num.colors)), 
			ncol=dim(cov.cols)[2])
  else colors.cat <- matrix(as.numeric(cut(cov.cols, breaks = cut.cov)), 
			ncol=dim(cov.cols)[2])
 }


## order the entire dataframe such that
##  time is in descending order and, when tied, then, 
##  survival comes before censoring 

if(surv.ind[1] > surv.ind[2])
  data <- data[order(unlist(survtime.col), unlist(-surv.col)),]
else if(surv.ind[1] < surv.ind[2])
  data <- data[order(unlist(survtime.col), unlist(surv.col)),]



## determine vector of upcoming consecutive censored objects if current is censored
cens.consec.vec <- rep(NA, dim(data)[1])
cnt <- 0
for(i in dim(data)[1]:1)
 {
  if(surv.col[i] == surv.ind[1])
   {
	cnt <- 0
	cens.consec.vec[i] <- 0
	next
   }
  else if(surv.col[i] == surv.ind[2])	
   {
	cnt <- cnt + 1
	cens.consec.vec[i] <- cnt - 1
   }
 }


## some pre-processing here before plotting:
## determine vector of upcoming events (possibly tied events) following
##  any censored time or string of consecutive censored times;
##  also, determine upcoming event times (or, by default,
##  5% beyond final censored time if no event times
##  eventually follow a censored time)
##  --> also, determine string size of censored obs followed by event(s)

n <- dim(data)[1]
cnt <- 0
seq.events <- (1:n)[surv.col == surv.ind[1]]
upcoming.events <- time.ahead <- string <- split <- rep(NA, dim(data)[1])
table.temp <- table(survtime.col[surv.col == surv.ind[1]]) 

for(i in 1:n)
 {
  if(surv.col[i] == surv.ind[2])
   {
    if((n - cens.consec.vec[i]) > i)
     {
      cnt <- cnt + 1
      upcoming.events[i] <- table.temp[as.numeric(names(table.temp)) > survtime.col[i]][1]
      time.ahead[i] <- as.numeric(names(table.temp[as.numeric(names(table.temp)) > survtime.col[i]])[1])
    
      seq.event.after <- seq.events[seq.events > i][1]
      if(i == 1  | (cnt == i))
	{
        string[i] <- table.temp[as.numeric(names(table.temp)) > survtime.col[i]][1] + 
						(seq.event.after - 1)
	}			   
	 else
	  {
 		seq.event.before <- rev(seq.events[seq.events < i])[1]
		string[i] <- table.temp[as.numeric(names(table.temp)) > survtime.col[i]][1] +
				(seq.event.after  - seq.event.before - 1)
	  }
	
      split[i] <- cnt
      if(surv.col[i+1] == surv.ind[1])
        cnt <- 0
     }

    else if((n - cens.consec.vec[i]) <= i)
     {
	cnt <- cnt + 1
	time.ahead[i] <- survtime.col[n] * mult.end.cens
	split[i] <- cnt
	seq.event.before <- rev(seq.events[seq.events < i])[1]
	string[i] <- n - seq.event.before
     }
   }	## end censored if statement

  else if(surv.col[i] == surv.ind[1])
   {
	if(i > 1)
	 {
		if(surv.col[i-1] == surv.ind[2])
		 {
		  split[i] <- split[i-1] + 1
		  string[i] <- string[i-1]
		 }
		else if((surv.col[i-1] == surv.ind[1]) & (survtime.col[i-1] == survtime.col[i]) & 
			!is.na(split[i-1]))
		 {
		  split[i] <- split[i-1] + 1
		  string[i] <- string[i-1]
		 }
	  }
   }   ## end event if statement
 }		## end pre-processing for loop



## set up plotting region, axis labels, title, etc.
plot(x=c(0, max(survtime.col, na.rm=TRUE) * mult.end.cens), y=c(0,1), type='n', 
	  xlab=x.lab, ylab=y.lab, main=title, ...)


## definitions needed in below for loop
temp.prob.c <- temp.prob.e <- NA
temp.prob.old <- 1
temp.prob.e.old <- 1
cens.cnt <- 0
cumsum.e <- cumsum(surv.col)



## main function for loop to create plotting lines for each patient

for(i in 1:n)
 {
  len.cov <- sum(!is.na(covtime.cols[i,])) 	## number of intervals to draw for patient i

  if(len.cov < 1)
    stop('can have only non-NA covariate observations in iteration', i, '\n')

  if(surv.col[i] == surv.ind[1]) 	## event
	{
	 temp.prob.e <- temp.prob.e.old * (n - i) / (n - i + 1)
	 if(!is.na(split[i])) 
	  {
	   upcoming.prob.e <- (n - (i + (string[i] - split[i]))) / 
				(n + upcoming.event.old - (i + (string[i] - split[i]))) *
				temp.prob.e.old
	   temp.prob.plot <- temp.prob.e.old - 
			((temp.prob.e.old - upcoming.prob.e) * split[i]/string[i])
	  } 
	 else
	   temp.prob.plot <- temp.prob.e


	 ## perform plotting for uncensored obs i 	
	 if(len.cov > 1) 
	  {
	   for(j in (1:(len.cov - 1)))
	    {
		color <- switch(colors.cat[i, j], colors[1], colors[2], colors[3], colors[4], colors[5],
											    colors[6], colors[7], colors[8], colors[9], colors[10],
											    colors[11], colors[12], colors[13], colors[14], colors[15],
											    colors[16], colors[17], colors[18], colors[19], colors[20])
		polygon(x=c(covtime.cols[i,j], covtime.cols[i,j+1], covtime.cols[i,j+1], covtime.cols[i,j]), 
		  y=c(temp.prob.plot, temp.prob.plot, temp.prob.old, temp.prob.old), col=color)
	    }
	  }
		
	 color <- switch(colors.cat[i, len.cov], colors[1], colors[2], colors[3], colors[4], colors[5],
						colors[6], colors[7], colors[8], colors[9], colors[10],
						colors[11], colors[12], colors[13], colors[14], colors[15],
						colors[16], colors[17], colors[18], colors[19], colors[20])
										
	 polygon(x=c(covtime.cols[i,len.cov], survtime.col[i], survtime.col[i], covtime.cols[i,len.cov]), 
			  y=c(temp.prob.plot, temp.prob.plot, temp.prob.old, temp.prob.old), col=color)

	 if(!is.na(string[i]) & (split[i] < string[i])) 
	   temp.prob.old <- temp.prob.plot
	 else 
	   temp.prob.e.old <- temp.prob.old <- temp.prob.plot	   	
	
	} 	## end event if statement for plotting



  else if(surv.col[i] == surv.ind[2]) 	## censored
   {
	 if((n - cens.consec.vec[i]) > i)	
	  {
		upcoming.prob.c <- (n - (i + (string[i] - split[i]))) / 
				(n + upcoming.events[i] - (i + (string[i] - split[i]))) *
				temp.prob.e.old
		temp.prob.plot <- temp.prob.e.old - 
				((temp.prob.e.old - upcoming.prob.c) * split[i]/string[i]) 
	    upcoming.event.old <- upcoming.events[i]
	  }
	
	 else if((n - cens.consec.vec[i]) <= i) 
	  {
	   temp.prob.plot <- temp.prob.e.old - (temp.prob.e.old * split[i]/string[i])	
	  }
	
	## perform plotting for censored obs i 	
	 if(len.cov > 1)
	  {
	   for(j in (1:(len.cov - 1)))
	    {
		  color <- switch(colors.cat[i, j], colors[1], colors[2], colors[3], colors[4], colors[5],
						colors[6], colors[7], colors[8], colors[9], colors[10],
						colors[11], colors[12], colors[13], colors[14], colors[15],
						colors[16], colors[17], colors[18], colors[19], colors[20])
		  polygon(x=c(covtime.cols[i,j], covtime.cols[i,j+1], covtime.cols[i,j+1], covtime.cols[i,j]), 
		  		   y=c(temp.prob.plot, temp.prob.plot, temp.prob.old, temp.prob.old), col=color)
		 }
	  }
	
	 color <- switch(colors.cat[i, len.cov], colors[1], colors[2], colors[3], colors[4], colors[5],
						colors[6], colors[7], colors[8], colors[9], colors[10],
						colors[11], colors[12], colors[13], colors[14], colors[15],
						colors[16], colors[17], colors[18], colors[19], colors[20])
	 polygon(x=c(covtime.cols[i,len.cov], survtime.col[i], survtime.col[i], covtime.cols[i,len.cov]), 
			  y=c(temp.prob.plot, temp.prob.plot, temp.prob.old, temp.prob.old), col=color)
	 polygon(x=c(survtime.col[i], time.ahead[i], time.ahead[i], survtime.col[i]), 
		 y=c(temp.prob.plot, temp.prob.plot, temp.prob.old, temp.prob.old), 
	         density=cens.density, border=TRUE)	 

     ## Following was if(cens.mark.right == TRUE)  FEH 31jan03
	 if(cens.mark.right & temp.prob.plot >= cens.mark.cutoff)
	   text(x = time.ahead[i] + cens.mark.ahead, 
	        y = temp.prob.old,  	
	        labels = cens.mark, cex = cens.mark.cex) 
	
	 temp.prob.c <- temp.prob.old <- temp.prob.plot
	    
   }	## end censored if statement for plotting

 } ## end of function's major for loop

} ## end of function itself



find.matches <- function(x, y, tol=rep(0,ncol(y)), scale=tol, maxmatch=10) {

  if(.R.) rep.int <- rep
  
#if(length(dim(x))==0) x <- matrix(x, nrow=1)  10may02
  if(!is.matrix(x)) x <- as.matrix(x)
  n <- nrow(x)
  p <- ncol(x)
  if(!is.matrix(y)) y <- as.matrix(y)  ## 10may02
  if(p != ncol(y)) stop("number of columns of x and y must match")
  ny <- nrow(y)
  rown <- dimnames(x)[[1]]
  ry <- dimnames(y)[[1]]
  matches <- matrix(if(length(ry))"" else 0, n, maxmatch,
                    dimnames=list(rown, paste("Match #",1:maxmatch,sep="")))
  distance <- matrix(NA, n, maxmatch,
                     dimnames=list(rown,
                       paste("Distance #",1:maxmatch,sep="")))
  if(length(ry)==0) ry <- 1:ny
  scale <- ifelse(scale==0,1,tol)
  ones <- rep(1,p)
  mx <- 0
  for(i in 1:n) {
    dif <- abs(y - rep(x[i,], rep.int(ny,p)))
    toll <- rep(tol, rep.int(nrow(dif),p))
    which <- (1:ny)[((dif > toll) %*% ones)==0]
    lw <- length(which)
    if(lw) {
      scaled <- dif[which,,drop=FALSE]/rep(scale, rep.int(lw,p))
      dist <- (scaled^2) %*% ones
      lw <- min(lw,maxmatch)
      mx <- max(mx,lw)
      d <- order(dist)[1:lw]
      matches[i,1:lw] <- ry[which[d]]
      distance[i,1:lw] <- dist[d]
    }
  }
  structure(list(matches=matches[,1:mx], distance=distance[,1:mx]), 
            class="find.matches")
}

print.find.matches <- function(x, digits=.Options$digits, ...) {
  cat("\nMatches:\n\n")
  print(x$matches, quote=FALSE)
  cat("\nDistances:\n\n")
  print(x$distance, digits=digits)
  invisible()
}


summary.find.matches <- function(object, ...) {
mat <- object$matches
dist <- object$distance
cat("Frequency table of number of matches found per observation\n\n")
m <- (!is.na(dist)) %*% rep(1,ncol(mat))
print(table(m))
cat("\nMedian minimum distance by number of matches\n\n")
print(tapply(dist[m>0,1], m[m>0], median))
ta <- table(mat[m>0,1])
ta <- ta[ta>1]
if(length(ta)) {
 cat("\nObservations selected first more than once (with frequencies)\n\n")
 print(ta)
}
else cat("\nNo observations selected first more than once\n\n")
invisible()
}

matchCases <- function(xcase,    ycase,    idcase=names(ycase),
                       xcontrol, ycontrol, idcontrol=names(ycontrol),
                       tol=NULL,
                       maxobs=max(length(ycase),length(ycontrol))*10,
                       maxmatch=20, which=c('closest','random')) {

  if(!length(tol)) stop('must specify tol')
  if((length(xcase)!=length(ycase)) || (length(xcontrol)!=length(ycontrol)))
    stop('lengths of xcase, ycase and of xcontrol, ycontrol must be same')
  which <- match.arg(which)
  
  ycase    <- as.matrix(ycase)
  ycontrol <- as.matrix(ycontrol)
  if(!length(idcase))    idcase    <- 1:length(ycase)
  if(!length(idcontrol)) idcontrol <- 1:length(ycontrol)
  idcase    <- as.character(idcase)
  idcontrol <- as.character(idcontrol)
  
  j <- is.na(ycase %*% rep(1,ncol(ycase))) | is.na(xcase)
  if(any(j)) {
    warning(paste(sum(j),'cases removed due to NAs'))
    ycase <- ycase[!j,,drop=FALSE]
    xcase <- xcase[!j]
    idcase <- idcase[!j]
  }
  j <- is.na(ycontrol %*% rep(1,ncol(ycontrol))) | is.na(xcontrol)
  if(any(j)) {
    warning(paste(sum(j),'controls removed due to NAs'))
    ycontrol <- ycontrol[!j,,drop=FALSE]
    xcontrol <- xcontrol[!j]
    idcontrol <- idcontrol[!j]
  }

  idCase <- id <- character(maxobs)
  type   <- factor(rep(NA,maxobs), c('case','control'))
  x      <- numeric(maxobs)
  y      <- matrix(NA, ncol=ncol(ycase), nrow=maxobs)

  last <- 0
  ncase <- length(ycase)
  ncontrol <- length(ycontrol)
  matches  <- integer(ncase)
  for(i in 1:ncase) {
    s <- abs(xcontrol-xcase[i]) <= tol
    nmatch <- sum(s)
    if(nmatch > maxmatch) {
      s <- (1:ncontrol)[s]  ## next line was sample(j,...) 4jun02
      if(which=="random") s <- sample(s, maxmatch, replace=FALSE) else {
        errors <- abs(xcontrol[s]-xcase[i])
        serrors <- order(errors)
        s <- (s[serrors])[1:maxmatch]
      }
      nmatch <- maxmatch
    }
    matches[i] <- nmatch
    if(!nmatch) next
    end <- last + nmatch + 1
    if(end > maxobs) stop(paste('needed maxobs >',maxobs))
    start <- last+1
    last <- end
    idCase[start:end] <- rep(idcase[i], nmatch+1)
    type[start:end]   <- c('case',rep('control',nmatch))
    id[start:end]     <- c(idcase[i], idcontrol[s])
    x[start:end]      <- c(xcase[i], xcontrol[s])
    y[start:end,]     <- rbind(ycase[i,,drop=FALSE], ycontrol[s,,drop=FALSE])
  }

  cat('\nFrequencies of Number of Matched Controls per Case:\n\n')
  print(table(matches))
  cat('\n')
  structure(list(idcase=idCase[1:end], type=type[1:end],
                 id=id[1:end], x=x[1:end], y=drop(y[1:end,])),
            row.names=as.character(1:end),
            class='data.frame')
}





#Dan Heitjan  dheitjan@biostats.hmc.psu.edu
#
ftupwr <- function(p1,p2,bign,r,alpha) {
## Compute the power of a two-sided level alpha test of the
## hypothesis that pi1=pi2, when pi1=p1, pi2=p2, and there are
## bign observations, bign/(1+r) in group 1 and r*bign/(1+r) in
## group 2.  This is based on the two-tailed test version of
## formula (6) in Fleiss, Tytun and Ury (1980 Bcs 36, 343--346).
## This may be used for del not too small (del>=0.1) and r not
## too big or small (0.33<=r<=3).
##   Daniel F. Heitjan, 30 April 1991
 mstar <- bign/(r+1)
 del <- abs(p2-p1)
 rp1 <- r+1
 zalp <- qnorm(1-alpha/2)
 pbar <- (p1+r*p2)/(1+r)
 qbar <- 1-pbar
 num <- (r*del^2*mstar-rp1*del)^0.5-zalp*(rp1*pbar*qbar)^0.5
 den <- (r*p1*(1-p1)+p2*(1-p2))^0.5
 zbet <- num/den
 pnorm(zbet) }
##
ftuss <- function(p1,p2,r,alpha,beta) {
## Compute the approximate sample size needed to have power 1-beta
## for detecting significance in a two-tailed level alpha test of
## the hypothesis that pi1=pi2, when pi1=p1, pi2=p2, and there
## are to be m in group 1 and rm in group 2.  The calculation is
## based on equations (3) and (4) of Fleiss, Tytun and Ury (1980
## Bcs 36, 343--346).  This is accurate to within 1% for
## moderately large values of del(p2-p1) (del>=0.1) and sample
## sizes that are not too disproportionate (0.5<=r<=2).
##   Daniel F. Heitjan, 30 April 1991
 zalp <- qnorm(1-alpha/2)
 zbet <- qnorm(1-beta)
 rp1 <- (r+1)
 pbar <- (p1+r*p2)/rp1
 qbar <- 1-pbar
 q1 <- 1-p1
 q2 <- 1-p2
 del <- abs(p2-p1)
 num <- (zalp*(rp1*pbar*qbar)^0.5+zbet*(r*p1*q1+p2*q2)^0.5)^2
 den <- r*del^2
 mp <- num/den
 m <- 0.25*mp*(1+(1+2*rp1/(r*mp*del))^0.5)^2
 list(n1=floor(m+1),n2=floor(m*r+1)) }

gbayes <- function(mean.prior, var.prior, m1, m2, stat, var.stat,
                   n1, n2, cut.prior, cut.prob.prior=.025) {

if(!missing(cut.prior)) 
  var.prior <- ((cut.prior - mean.prior)/qnorm(1 - cut.prob.prior))^2

if(!is.function(var.stat)) {
  vs <- var.stat
  if(!missing(n1))
    stop('may not specify n1,n2 when var.stat is not a function')
} else vs <- var.stat(m1,m2)

var.post <- 1/(1/var.prior + 1/vs)
mean.post <- (mean.prior/var.prior + stat/vs)*var.post
result <- list(mean.prior=mean.prior, var.prior=var.prior, 
               mean.post=mean.post,   var.post=var.post)

if(!missing(n1)) {
  mean.pred <- mean.post
  var.pred <- var.post + var.stat(n1,n2)
  result$mean.pred <- mean.pred
  result$var.pred  <- var.pred
}
structure(result, class='gbayes')
}

plot.gbayes <- function(x, xlim, ylim, name.stat='z', ...) {
  obj <- x
  pred <- length(obj$mean.pred)>0
  if(missing(xlim)) xlim <- obj$mean.post + c(-6,6)*sqrt(obj$var.post)
  x <- seq(xlim[1], xlim[2], length=200)
  y1 <- dnorm(x,obj$mean.prior,sqrt(obj$var.prior))
  y2 <- dnorm(x,obj$mean.post, sqrt(obj$var.post))
  plot(x, y1, xlab=name.stat, ylab='Density',type='l',lty=1,
	   ylim=if(missing(ylim)) range(c(y1,y2)) else ylim)
  curves <- vector('list',2+pred)
  names(curves) <- c('Prior','Posterior',if(pred)'Predictive')
  curves[[1]] <- list(x=x,y=y1)
  lines(x, y2, lty=2)
  curves[[2]] <- list(x=x,y=y2)
  if(pred) {
    y <- dnorm(x,obj$mean.pred,sqrt(obj$var.pred))
    lines(x, y, lty=3)
    curves[[3]] <- list(x=x,y=y)
  }
  labcurve(curves, ...)
  invisible()
}

gbayes2 <- function(sd, prior, delta.w=0, alpha=0.05,
                    upper=Inf, prior.aux=NULL) {
  if(!is.function(prior)) stop('prior must be a function')
  z <- qnorm(1-alpha/2)
  prod <- function(delta, prior, delta.w, sd, z, prior.aux) {
    (1 - pnorm((delta.w - delta)/sd + z)) *
      if(length(prior.aux)) prior(delta, prior.aux) else prior(delta)
  }
  ww <- if(.R.)'value' else 'integral'
  ip <- if(length(prior.aux))
    integrate(prior, -Inf, upper, prior.aux=prior.aux)[[ww]] else
    integrate(prior, -Inf, upper)[[ww]]
  if(abs(ip-1) > .01)
    warning(paste('integrate failed to obtain 1.0 for integral of prior.\nDivided posterior probability by the integral it did obtain (',
                  format(ip),').\nTry specifying upper=.',sep=''))
  integrate(prod, delta.w, upper,
            prior=prior, delta.w=delta.w, sd=sd, z=z,
            prior.aux=prior.aux)[[ww]]
}


# v = variance of Xn after future obs.
gbayesMixPredNoData <-
  function(mix=NA, d0=NA, v0=NA, d1=NA, v1=NA,
           what=c('density','cdf')) {
    what <- match.arg(what)
    g <- function(delta, v, mix, d0, v0, d1, v1, dist) {
      if(mix==1) {
        pv <- 1/(1/v0 + 1/v)
        dist(delta, d0, sqrt(pv))
      } else if(mix==0) {
        pv <- 1/(1/v1 + 1/v)
        dist(delta, d1, sqrt(pv))
      } else {
        pv0 <- 1/(1/v0 + 1/v)
        pv1 <- 1/(1/v1 + 1/v)
        mix*dist(delta, d0, sqrt(pv0)) +
          (1-mix)*dist(delta, d1, sqrt(pv1))
      }
    }
#    g$mix <- mix; g$d0 <- d0; g$v0 <- v0; g$d1 <- d1; g$v1 <- v1 10may02
#    g$dist <- switch(what, density=dnorm, cdf=pnorm)
    formals(g) <- list(delta=numeric(0), v=NA, mix=mix, d0=d0, v0=v0,
                       d1=d1, v1=v1, dist=NA)
    g
  }

#mp <- function(d,mix,d0,v0,d1,v1,what=c('density','cdf')) {
#  what <- match.arg(what)
#  f <- switch(what, density=dnorm, cdf=pnorm)
#  plot(d,mix*f(d,d0,sqrt(v0))+(1-mix)*f(d,d1,sqrt(v1)),
#       type='l', lwd=3)
#  invisible()
#}

gbayesMixPost <- function(x=NA, v=NA, mix=1, d0=NA, v0=NA, d1=NA,
                          v1=NA, what=c('density','cdf')) {
  what <- match.arg(what)
  g <- function(delta, x, v, mix=1, 
                d0, v0, d1, v1, dist) {
    if(mix==1) {
      pv <- 1/(1/v0 + 1/v)
      dist(delta, (d0/v0 + x/v)*pv, sqrt(pv))
    } else if(mix==0) {
      pv <- 1/(1/v1 + 1/v)
      dist(delta, (d1/v1 + x/v)*pv, sqrt(pv))
    } else {
      prior.odds <- mix/(1-mix)
      pv0 <- 1/(1/v0 + 1/v); pv1 <- 1/(1/v1 + 1/v)
      likelihood.ratio <- dnorm(x, d0, sqrt(v0))/
        dnorm(x, d1, sqrt(v1))
      post.odds <- prior.odds * likelihood.ratio
      mixp <- post.odds/(1+post.odds)
      mixp*dist(delta, (d0/v0 + x/v)*pv0, sqrt(pv0)) +
        (1-mixp)*dist(delta, (d1/v1 + x/v)*pv1, sqrt(pv1))
    }
  }
#  g$x <- x; g$v <- v; g$mix <- mix; g$d0 <- d0; g$v0 <- v0;
#  g$d1 <- d1; g$v1 <- v1
#  g$dist <- switch(what, density=dnorm, cdf=pnorm)  10may02
  formals(g) <- list(delta=numeric(0), x=x, v=v, mix=mix, d0=d0, v0=v0,
                     d1=d1, v1=v1,
                     dist=switch(what, density=dnorm, cdf=pnorm))
  g
}


gbayesMixPowerNP <- function(pcdf, delta, v, delta.w=0, mix, interval,
                          nsim=0, alpha=0.05) {

  if(nsim==0) {
    ## Solve for statistic x such that the posterior cdf at
    ## (delta.w,x)=alpha/2
    g <- function(x, delta.w, v, alpha, pcdf, mix) {
      pcdf(delta.w, x, v, mix) - alpha/2 }
#    g$delta.w <- delta.w; g$v <- v; g$alpha <- alpha; g$pcdf <- pcdf
#    g$mix <- if(missing(mix)) pcdf$mix else mix  10may02
    formals(g) <- list(x=numeric(0), delta.w=delta.w, v=v,
                       alpha=alpha, pcdf=pcdf,
                       mix=if(missing(mix)) (if(.R.)as.list(pcdf)$mix
                          else pcdf$mix) else mix)

#    s <- seq(interval[1],interval[2],length=100)
#    gs <- g(s)
#    plot(s, gs, type='l')
##    interval[2] <- min(s[sign(gs)!=sign(gs[1])])
##    interval[1] <- max(s[s < interval[2] & sign(gs)==sign(gs[1])])
#    interval[1] <- max(s[sign(gs)!=sign(gs[100])])
#    interval[2] <- min(s[s > interval[1] & sign(gs)==sign(gs[100])])
#    prn(interval)

    x <- uniroot(g, interval=interval)$root
    c('Critical value'=x, Power=1 - pnorm(x, delta, sqrt(v)))
  } else {
    x <- rnorm(nsim, delta, sqrt(v))
    probs <- if(missing(mix)) pcdf(delta.w, x, v) else
      pcdf(delta.w, x, v, mix=mix)
    pow <- mean(probs <= alpha/2)
    se <- sqrt(pow*(1-pow)/nsim)
    c(Power=pow, 'Lower 0.95'=pow-1.96*se, 'Upper 0.95'=pow+1.96*se)
  }
}

gbayes1PowerNP <- function(d0, v0, delta, v, delta.w=0, alpha=0.05) {
  pv <- 1/(1/v0 + 1/v)
  z <- qnorm(alpha/2)
  1 - pnorm(v*( (delta.w - sqrt(pv)*z)/pv - d0/v0 ), delta, sqrt(v))
}








groupn<-function(x,y,m=150){
s <- !is.na(x+y)
x<-x[s]
y<-y[s]
i<-order(x)
x<-x[i]
y<-y[i]
n<-length(x)
if(n<m)stop("m<number of observations in groupn")
start<-1
end<-m
meanx<-NULL
meany<-NULL
while(end<=n){
	meanx<-c(meanx,mean(x[start:end]))
	meany<-c(meany,mean(y[start:end]))
	start<-start+m
	end<-end+m}
if(end>n){
	meanx<-c(meanx,mean(x[n-m+1:n]))
	meany<-c(meany,mean(y[n-m+1:n]))	}
return(list(x=meanx,y=meany))}

hist.data.frame <- function(x, n.unique=3, nclass="compute", na.big=FALSE,
							rugs=FALSE, mtitl=FALSE, ...) {
  oldmf  <- par('mfrow')
  oldoma <- par('oma')
  on.exit(par(mfrow=oldmf, oma=oldoma))
  mf <- oldmf
  if(length(mf)==0) mf <- c(1,1)
  automf <- FALSE  ## 22sep02
  if((la <- length(x))>1 & max(mf)==1) {
    mf <- if(la<=4)c(2,2) else if(la<=6)c(2,3) else if(la<=9)c(3,3) else
	if(la<=12)c(3,4) else if(la<=16) c(4,4) else c(4,5)
    automf <- TRUE
    par(mfrow=mf)
  }
  if(is.character(mtitl)) par(oma=c(0,0,3,0))
  nam <- names(x)
  i <- 0
  j <- 0
  for(v in x) {
	j <- j+1
	if(!is.character(v)) {
      type <- if(inherits(v,'factor'))'factor' else
      if(inherits(v,'dates'))'date' else 'none'
      if(type!='none') v <- oldUnclass(v)
      w <- v[!is.na(v)]
      n <- length(w)
      if(length(unique(w)) >= n.unique) {
        i <- i+1
        if(is.numeric(nclass)) nc <- nclass else
        if(nclass=="compute") nc <- max(2,trunc(min(n/10,25*logb(n,10))/2))
        lab <- attr(v,"label")
        lab <- if(length(lab) && nchar(lab) > 35) nam[j] else
        label(v, units=TRUE, plot=TRUE, default=nam[j])
        ##	nl <- if(is.null(lab)) 0 else nchar(lab)  26sep02
        ##	if(nl==0 | nl>20)lab <- nam[j]
        if(.R.) {
          if(nclass!="default")hist(v,nclass=nc,xlab=lab,
               axes=type!='date',main='')
          else hist(v,xlab=lab, axes=type!='date',main='')
        } else {
          if(nclass!="default")hist(v,nclass=nc,xlab=lab,style.bar='old',
               axes=type!='date')
          else hist(v,xlab=lab,style.bar='old', axes=type!='date')
        }
        if(type=='date') {
          axis(2)
          r <- range(v, na.rm=TRUE)
          by <- round((r[2]-r[1])/(par('lab')[2] - 1))
          at <- seq(r[1], r[2], by=by)
          axis(1, at=at, labels=format(chron(at)))
        }
      
        m <- sum(is.na(v))
        pm <- paste("n:",n," m:",m,sep="")
        title(sub=pm,adj=0,cex=.5)
        if(na.big && m>0) mtext(paste(m,"NAs"),line=-2,cex=1)
        if(rugs) scat1d(v, ...)
        if(automf && interactive() && names(dev.list())!='postscript' &&
           (i %% prod(mf)==0))	{
          if(is.character(mtitl)) mtitle(mtitl)
          cat("click left mouse button to proceed\n")
          locator(1)
        } else if(is.character(mtitl) && i %% prod(mf)==1) mtitle(mtitl)
      }
    }
  }
  invisible(ceiling(i / prod(mf)))
}
"histbackback"<-
function(x, y, brks = NULL, xlab = NULL, axes = TRUE, probability = FALSE, 
		 xlim = NULL, ylab='',...)
{
	if(length(xlab))
		xlab <- rep(xlab, length = 2)
	if(is.list(x)) {
	  namx <- names(x)  # FEH 5Jan99
		y <- x[[2]]   # was x$y  FEH
		if(!length(xlab)) {
		  if(length(namx)) xlab <- namx[1:2] else {   #FEH
			xlab <- deparse(substitute(x))
			xlab <- paste(xlab, c("x", "y"), sep = "$")
		  }
		}
		x <- x[[1]]   # was x$x FEJ
	}
	else if(!length(xlab))
		xlab <- c(deparse(substitute(x)), deparse(substitute(y)))
	if(!length(brks))
		brks <- hist(c(x, y), plot = FALSE)$breaks
	ll <- hist(x, breaks = brks, plot = FALSE, probability = probability)
	rr <- hist(y, breaks = brks, plot = FALSE, probability = probability)
    if(.R. && probability) {  ## FEH 12may02
      ll$counts <- ll$density
      rr$counts <- rr$density
    }
	if(length(xlim) == 2)
		xl <- xlim
	else {
      xl <- pretty(range(c( - ll$counts, rr$counts)))  ## 1Dec01
      xl <- c(xl[1],xl[length(xl)])
    }
      
	if(length(ll$counts) > 0) {
      if(.R.) barplot(-ll$counts, xlim=xl, space=0,
                      horiz=TRUE, axes=FALSE, col=0, ...) else
		barplot( - ll$counts, brks, xlim = xl, histo = TRUE, horiz = TRUE, 
			axes = FALSE, ...)
		par(new = TRUE)
	}
	if(length(rr$counts) > 0) {
      if(.R.) barplot(rr$counts, xlim=xl, space=0,
                      horiz=TRUE, axes=FALSE, col=0, ...) else
		barplot(rr$counts, brks, xlim = xl, histo = TRUE, horiz = TRUE, axes
			 = FALSE, ...)
    }
	if(axes) {
		mgp.axis(1, at=pretty(xl), labels=format(abs(pretty(xl))))  ##FEH
        if(.R.) {
          del <- (brks[2]-brks[1] - (brks[3]-brks[2]))/2
          brks[1] <- brks[1] + del
          brks[-1] <- brks[-1] - del
          mgp.axis(2, at=0:(length(brks)-1),
                   labels=formatC(brks, format='f', digits=.Options$digits))
        }
        else  mgp.axis(2)
		title(xlab = xlab[1], adj = (-0.5 * xl[1])/( - xl[1] + xl[2]))
		title(xlab = xlab[2], adj = ( - xl[1] + 0.5 * xl[2])/( - xl[1] + 
			xl[2]))
		if(ylab!='') title(ylab=ylab)   # FEH
	}
	abline(v = 0)
	box()
	invisible(list(left = ll$counts, right = rr$counts, breaks = brks))
}
#Changes since sent to statlib: improved printing N matrix in print.hoeffd
hoeffd <- function(x, y) {

  phoeffd <- function(d, n) {
    d <- as.matrix(d); n <- as.matrix(n)
    b <- d + 1/36/n
    z <- .5*(pi^4)*n*b
    zz <- as.vector(z)
    zz[is.na(zz)] <- 1e30   # so approx won't bark
 
    tabvals <- c(5297,4918,4565,4236,3930,
	3648,3387,3146,2924,2719,2530,2355,2194,2045,1908,1781,1663,1554,1453,
	1359,1273,1192,1117,1047,0982,0921,
	0864,0812,0762,0716,0673,0633,0595,0560,0527,0496,0467,0440,0414,0390,
	0368,0347,0327,0308,0291,0274,0259,
	0244,0230,0217,0205,0194,0183,0173,0163,0154,0145,0137,0130,0123,0116,
	0110,0104,0098,0093,0087,0083,0078,
	0074,0070,0066,0063,0059,0056,0053,0050,0047,0045,0042,0025,0014,0008,
	0005,0003,0002,0001)/10000

    P <- ifelse(z<1.1 | z>8.5, pmax(1e-8,pmin(1,exp(.3885037-1.164879*z))),
                matrix(approx(c(seq(1.1, 5,by=.05),
                                seq(5.5,8.5,by=.5)),
                              tabvals, zz)$y,
                       ncol=ncol(d)))
    dimnames(P) <- dimnames(d)
    P
  }

if(!missing(y)) x <- cbind(x, y)
x[is.na(x)] <- 1e30
storage.mode(x) <- if(.R.) "double" else "single"
p <- as.integer(ncol(x))
if(p<1) stop("must have >1 column")
n <- as.integer(nrow(x))
if(n<5) stop("must have >4 observations")

h <- if(.R.)
  .Fortran("hoeffd", x, n, p, hmatrix=double(p*p), npair=integer(p*p),
	double(n), double(n),  double(n), double(n), double(n), 
	double(n), integer(n), PACKAGE="Hmisc") else
  .Fortran("hoeffd", x, n, p, hmatrix=single(p*p), npair=integer(p*p),
	single(n), single(n),  single(n), single(n), single(n), 
	single(n), integer(n))
npair <- matrix(h$npair, ncol=p)
h <- matrix(h$hmatrix, ncol=p)
h[h>1e29] <- NA
nam <- dimnames(x)[[2]]
dimnames(h) <- list(nam, nam)
dimnames(npair) <- list(nam, nam)
P <- phoeffd(h, npair)
diag(P) <- NA
structure(list(D=30*h, n=npair, P=P), class="hoeffd")

}



print.hoeffd <- function(x, ...) {
cat("D\n")
print(round(x$D,2))
n <- x$n
if(all(n==n[1,1])) cat("\nn=",n[1,1],"\n") else {
  cat("\nn\n")
  print(x$n)
}
cat("\nP\n")
P <- x$P
P <- ifelse(P<.0001,0,P)
p <- format(round(P,4))
p[is.na(P)] <- ""
print(p, quote=FALSE)
invisible()
}
impute <- function(x, ...) UseMethod("impute")


impute.default <- function(x, fun=median, ...)
{
 m <- is.na(x)
 k <- sum(m)
 if(k==0) return(x)

 nam <- names(x)
 if(!length(nam)) {nam <- as.character(1:length(x)); names(x) <- nam}

 if(!is.function(fun)) 
 {
   fill <- fun
   if(is.character(fill) && length(fill)==1 && fill=="random")
	fill <- sample(x[!is.na(x)], sum(is.na(x)), replace=TRUE)
 }

 else				{
 if(is.factor(x))
 {
   freq <- table(x)
   fill <- names(freq)[freq==max(freq)][1]   #take first if not unique
 }
 else  fill <- if(missing(fun) && is.logical(x))
					  (if(sum(x[!m]) >= sum(!m)/2) TRUE else FALSE) else
					  fun(x[!m])
# median(logical vector) doesn't work - know trying to get median
# if fun is omitted.  Get mode.
}

 if(length(fill)>1 && length(fill)!=k)
	stop("length of vector of imputed values != no. NAs in x")

# lab <- label(x)
# if(is.null(lab) || lab=="") lab <- name
# lab <- paste(lab,"with",sum(m),"NAs imputed to",format(fill))
# attr(x, "label") <- lab
 if(is.factor(x))
 {
   newlev <- sort(unique(fill))
   if(any(!(z <- newlev %in% levels(x))))
   {
	xc <- as.character(x)
	xc[m] <- fill
	x <- factor(xc, c(levels(x), newlev[!z]))
   }
   else x[m] <- fill
 }
 else x[m] <- fill
 ## .SV4. x 2 5may03
 if(.SV4.) warning('impute class not added to object because of S-Plus 6 restrictions; will not print or subset imputation information')
 structure(x, imputed=(1:length(x))[m],
           class=c(if(!.SV4.)'impute',attr(x,'class')))
}

print.impute <- function(x, ...)
{
 i <- attr(x,"imputed")
 if(!length(i)) {print.default(x); return(invisible())}
 if(is.factor(x)) w <- as.character(x)
 else w <- format(x)
 names(w) <- names(x)
 w[i] <- paste(w[i], "*", sep="")
 attr(w, "label") <- attr(w,"imputed") <- attr(w, "class") <- NULL
 print.default(w, quote=FALSE)
 invisible()
}

summary.impute <- function(object, ...)
{
 i <- attr(object, "imputed")
 oi <- object
 attr(oi,'class') <- attr(oi,'class')[attr(oi,'class')!="impute"]
 oi <- oi[i]
 if(all(oi==oi[1])) cat("\n",length(i),"values imputed to",
   if(is.numeric(oi)) format(oi[1]) else as.character(oi[1]),"\n\n")
 else 
 {
   cat("\nImputed Values:\n\n")
   if(length(i)<20) print(oi)
   else print(describe(oi, descript=as.character(sys.call())[2]))
   cat("\n")
 }
NextMethod("summary")
}

"[.impute" <- function(x, ..., drop=FALSE) {
  ats <- attributes(x)
  ats$dimnames <- NULL
  ats$dim <- NULL
  ats$names <- NULL
  attr(x,'class') <- NULL
  y <- x[..., drop = drop]
  if(length(y)==0) return(y)
  k <- 1:length(x); names(k) <- names(x)
  k <- k[...]
  attributes(y) <- c(attributes(y), ats)
  imp <- attr(y, "imputed")
  attr(y, "imputed") <- j <- (1:length(k))[k %in% imp]
  if(length(j)==0) {
    cy <- attr(y,'class')[attr(y,'class')!='impute']
    y <- structure(y, imputed=NULL, class=if(length(cy))cy else NULL)
  }
  y
}

is.imputed <- function(x)
{
 w <- rep(FALSE, length(x))
 if(length(z <- attr(x,"imputed"))) w[z] <- TRUE
 w
}

as.data.frame.impute <- function(x, row.names = NULL, optional = FALSE, ...)
{
        nrows <- length(x)
        if(!length(row.names)) {
# the next line is not needed for the 1993 version of data.class and is
# included for compatibility with 1992 version
                if(length(row.names <- names(x)) == nrows && !any(duplicated(
                        row.names))) {
                }
                else if(optional)
                        row.names <- character(nrows)
                else row.names <- as.character(1:nrows)
        }
        value <- list(x)
        if(!optional)
                names(value) <- deparse(substitute(x))[[1]]
        structure(value, row.names=row.names, class='data.frame')
}
"%in%" <- function(a,b)				{

if(is.factor(a) & is.numeric(b))	{
   warning("a is factor, b is numeric.  Assuming b is coded factor values")
   a <- oldUnclass(a)			}

else if(is.numeric(a) && is.factor(b))	{
   warning("a is numeric, b is factor.  Assuming a is coded factor values")
   b <- oldUnclass(b)			}

match(a, b, nomatch=0) > 0
						}


"%nin%" <- function(a, b) ! (a %in% b)
#Modification of S-supplied interaction function to keep levels in
#correct order if drop=T. Also, sep argument added.

interaction <- function(..., drop = FALSE, sep=".", left=FALSE)
{
	g <- if(left) function(x) format(x) else function(x) x
	allf <- list(...)
	if(length(allf) == 1 && is.list(ttt <- oldUnclass(allf[[1]])))
		allf <- ttt
	nterms <- length(allf)
	what <- allf[[nterms]]

	if(!length(levels(what)))
		what <- factor(what)
	levs <- oldUnclass(what) - 1
	labs <- g(levels(what))
#	for(what in rev(allf[ - nterms])) {Thanks Rich Calaway <rich@insightful.com>
#		if(is.null(levels(what)))      24Jul01
#			what <- factor(what)
    rev.allf <- rev(allf[ - nterms])
    for(k in seq(along = rev.allf)) {
      what <- as.factor(rev.allf[[k]])
      wlab <- g(levels(what))
      i <- oldUnclass(what) - 1
      levs <- levs * length(wlab) + i
      labs <- as.vector(outer(wlab, labs, paste, sep = sep))
	}
	levs <- levs + 1
	if(drop) {
      ulevs <- sort(unique(levs[!is.na(levs)]))   #sort() added FH
      levs <- match(levs, ulevs)
      labs <- labs[ulevs]
    }
	levels(levs) <- labs
#    structure(levs, class='factor')  17Jul01
    oldClass(levs) <- 'factor'
    levs
}
is.present <- function(x)
{
	if(is.character(x))
		return(x!="")
	else return(!is.na(x))
}
james.stein <- function(y, group) {

  s <- !(is.na(y)|is.na(group))
  y <- y[s]; group <- as.character(group[s])   # as.char -> unused levels OK
  k <- length(unique(group))
  if(k<3) stop("must have >=3 groups")
  
  stats <- function(w) {
    bar <- mean(w)
    ss  <- sum((w-bar)^2)
    n <- length(w)
#   if(n<2) stop("a group has n<2")
    c(n=length(w), mean=bar, ss=ss, var=ss/n/(n-1))
  }

  Z <- stats(y)
  st <- tapply(y, group, FUN=stats)
  nams <- names(st)
  z <- matrix(unlist(st),ncol=4,byrow=TRUE)
  ssb <- stats(z[,2])["ss"]
  shrink <- 1 - (k-3)*z[,4]/ssb
  shrink[z[,1]==1] <- 0
  shrink <- pmin(pmax(shrink,0),1)
  list(n=z[,1], mean=z[,2], 
       shrunk.mean=structure(Z["mean"]*(1-shrink)+shrink*z[,2], names=nams),
       shrink=shrink)
}

labcurve <- function(curves, labels=names(curves), 
					 method=NULL, keys=NULL, keyloc=c('auto','none'),
                     type='l', step.type=c('left','right'),
                     xmethod=if(any(type=='s')) 'unique' else 'grid', 
					 offset=NULL,
                     xlim=NULL, tilt=FALSE, window=NULL,
                     npts=100, cex=NULL, 
					 adj='auto', angle.adj.auto=30, 
					 lty=pr$lty, lwd=pr$lwd, col.=pr$col,
					 transparent=TRUE, arrow.factor=1, 
					 point.inc=NULL, opts=NULL, key.opts=NULL, 
					 empty.method=c('area','maxdim'), 
					 numbins=25, 
					 pl=!missing(add), add=FALSE, 
					 ylim=NULL, xlab="", ylab="",
                     whichLabel=1:length(curves),
                     grid=FALSE, xrestrict=NULL, ...) {

  if(grid && !.R.) {
##    warning('specified grid=T under S-Plus, ignored')
    grid <- FALSE
  }

  if(.R. && pl && !add) {plot.new(); par(new=TRUE)}  # enables strwidth etc.
  ## added !add 11dec02

  if(.R.) {
    oxpd <- par('xpd')
    par(xpd=NA)
    on.exit(par(xpd=oxpd))
  }
  
  gfun <- ordGridFun(.R. && grid)    ## see Misc.s
  gun  <- gfun$unit

  diffu <- function(v) diff(oldUnclass(v))  # mainly for POSIXt 17jun02
  ## also look at difftime
  
  mcurves <- missing(curves)

  pr <- par(c('cex','col','lwd','lty'))

  if(!mcurves) {
	nc <- length(curves)
	type <- rep(type, length=nc)
	lty  <- rep(lty,  length=nc)
	lwd  <- rep(lwd,  length=nc)
	col. <- rep(col., length=nc)
	for(i in 1:nc) {
	  z <- curves[[i]]
	  if(pl && !add) {
		if(i==1) {
		  xlm <- range(z[[1]],na.rm=TRUE)
		  ylm <- range(z[[2]],na.rm=TRUE)
		} else {
		  xlm <- range(xlm,z[[1]],na.rm=TRUE)
		  ylm <- range(ylm,z[[2]],na.rm=TRUE)
		}
	  }
	  if(length(a <- z$type)) type[i] <- a
	  if(length(a <- z$lty))  lty[i]  <- a
	  if(length(a <- z$lwd))  lwd[i]  <- a
	  if(length(a <- z$col))  col.[i] <- a
	}
  }

  ## Optionally bring arguments from opts as if they were listed outside opts
  ## This is used when opts is passed through to a function calling labcurve
  if(length(opts) && is.list(opts)) {
	names.opts <- names(opts)
	full.names <- c('labels','method','keys','keyloc','type','step.type',
					'xmethod','offset','xlim','tilt','window','npts','cex',
					'adj','angle.adj.auto','lty','lwd','col.','n.auto.keyloc',
					'transparent','arrow.factor','point.inc','key.opts',
					'empty.method','numbins','ylim','xlab','ylab')
	i <- charmatch(names.opts, full.names, -1)
	if(any(i < 1)) stop(paste('Illegal elements in opts:',
							  paste(names.opts[i < 1], collapse=' ')))
	for(j in 1:length(opts)) assign(full.names[i[j]],opts[[j]],immediate=TRUE)
  }

  if(mcurves) nc <- length(labels) else
    if(!is.logical(labels) && nc != length(labels))
      stop('length of labels is not equal to # curves')  #28Nov99

  type <- rep(type, length=nc)
  lty  <- rep(lty,  length=nc)
  lwd  <- rep(lwd,  length=nc)
  col. <- rep(col., length=nc)

  if(pl) {
	if(mcurves) stop('curves must be given if pl=T')
	if(!add) {
	  if(!length(xlim)) xlim <- xlm
	  if(!length(ylim)) ylim <- ylm
	  namcur <- names(curves[[1]])   #13Jul97
	  if(xlab=='' && length(namcur)) xlab <- namcur[1]
	  if(ylab=='' && length(namcur)) ylab <- namcur[2]
      if(grid) {
        stop("grid=TRUE when pl=TRUE is not yet implemented")
      } else
      plot(0, 0, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab,
           type='n', xaxt='n')  ## xaxt 15jun02
      if(inherits(xlim,'POSIXt') || inherits(xlim,'POSIXct'))
        axis.POSIXct(1) else axis(1)   ## 15jun02 18sep02
            
	  pr <- par(c('cex','col','lwd','lty'))
	}
	for(i in 1:nc) {
	  z <- curves[[i]]
	  gfun$lines(z[[1]], z[[2]],
                 type=type[i], lty=lty[i], lwd=lwd[i], col=col.[i])
	}
  }

  if(length(method) && method=='none') return(invisible()) # 29sep02

  pr <- parGrid(grid)  ## 20Mar02
  usr <- pr$usr; uin <- pr$uin
  
  is.keys    <- length(keys) > 0
  lines.keys <- length(keys)==1 && is.character(keys) && keys=='lines'

  if(!length(method)) {
	if(is.keys) method <- if(is.numeric(keys) || lines.keys)
	  'on top' else 'offset' else
	method <- 'offset'
  }

  ## Expand abbreviations for method - couldn't use match.arg
  possible.methods <- c('offset','on top','arrow','mouse','locator')
  i <- charmatch(method, possible.methods, -1)
  if(i < 1) stop(paste('method must be one of ',
					   paste(possible.methods,collapse=' ')))
  method <- possible.methods[i]
  
  if(!length(cex)) cex <- pr$cex

  if(mcurves && method %nin% c('mouse','locator')) 
	  stop('must specify curves unless method="mouse" or "locator"')

  if(!lines.keys && is.keys && length(keys) != nc) 
	stop('number of keys must = number of curves')

  if(method %in% c('mouse','locator')) {
	if(adj=='auto') adj <- .5
	xt <- yt <- numeric(nc)
	for(i in 1:nc) {
      if(i %in% whichLabel) {  ## 17sep02
        cat('\nPosition pointer to desired center of curve label and click for',
            labels[i],'\n')
        lab.pos <- locator(1)
        xt[i] <- lab.pos$x
        yt[i] <- lab.pos$y
        gfun$text(lab.pos, labels[i], cex=cex, adj=adj, col=col.[i],
                  ...)
      }
	}
	return(invisible(list(x=xt, y=yt, offset=0,
						  adj=adj, cex=cex, angle=0, col=col., lwd=lwd,
						  key.opts=key.opts, ...)))
  }

  if(is.character(keyloc)) keyloc <- match.arg(keyloc)

  empty.method <- match.arg(empty.method)

  if(!length(offset)) offset <-
    if(grid)unit(.75,"strheight","m") else strheight('m','user', cex)*.75

  if(!length(xlim)) xlim <- usr[1:2]
  if(!length(ylim)) ylim <- usr[3:4]

  ##  if(!length(point.inc)) point.inc <- diff(xlim)/5
  ## moved to be used only when needed 15jun02

  if(nc==1) {
	ci <- curves[[1]]
	xx <- ci[[1]]; yy <- ci[[2]]
	s <- is.finite(xx+yy)
	xx <- xx[s];  yy <- yy[s]
	imid <- trunc((length(xx)+1)/2)
	adj <- if(is.character(adj))0.5 else adj
	if(any(whichLabel==1))
      gfun$text(xt <- gun(xx[imid]),
                yt <- gun(yy[imid])+offset,
                labels, 
                cex=cex, adj=adj, col=col., ...)
	return(invisible(list(x=xt, y=yt, offset=offset,
						  adj=adj, cex=cex, col=col., lwd=lwd, angle=0, 
						  key.opts=key.opts, ...)))
  }

  if(xmethod %nin% c('grid','unique')) 
	stop('xmethod must be "grid" or "unique"')
  step.type <- match.arg(step.type)


  if(is.character(adj)) {
	adj.does.vary     <- TRUE
	adj.needs.to.vary <- TRUE
	adj <- rep(.5, nc)
  } else {
	adj.does.vary     <- length(adj) > 1
	adj.needs.to.vary <- FALSE
	adj <- rep(adj, length=nc)
  }

  if(xmethod=='grid') xs <- seq(xlim[1],xlim[2],length=npts) else {
	xs <- unlist(sapply(curves, function(z)z[[1]]))
	xs <- sort(unique(xs[!is.na(xs)]))
	xs <- xs[xs>=xlim[1] & xs<=xlim[2]]
  }

  ys <- matrix(NA, nrow=length(xs), ncol=nc)
  rng <- matrix(NA, nrow=2, ncol=nc)

  for(i in 1:nc) {
	ci <- curves[[i]]
	xx <- ci[[1]]; yy <- ci[[2]]
	s <- is.finite(xx+yy)
	xx <- xx[s]
	y <- approx(xx, yy[s], xout=xs, f=if(step.type=='left')0 else 1,
				method=if(type[i]=='l')"linear" else "constant")$y
	y <- pmax(pmin(y,usr[4]),usr[3])
	## Where one curve is not defined, consider this gap to have an ordinate
	## that is far from the other curves so labels where be placed where
	## the other curves haven't started or after they've ended
	y[is.na(y)] <- 1e10
	ys[,i] <- y
    rxx <- range(xx) ## 12feb03 and next 5 lines
    if(length(xrestrict)) {
      rxx[1] <- max(rxx[1],xrestrict[1])
      rxx[2] <- min(rxx[2],xrestrict[2])
    }
	rng[,i] <- rxx
	## Save real range of each x-vector so candidates for labeling
	## will be where the curve really exists
  }

  if(method=='on top' && is.keys && is.numeric(keys)) {
	## Draw periodic symbols
	sym <- function(curve, pch, inc, offset, type, step.type, col.,
                    grid, gfun) {
	  x <- curve[[1]]; y <- curve[[2]]
	  s <- is.finite(x+y)
	  x <- x[s]; y <- y[s]
	  if(length(x)<2)
        stop("when specifying numeric keys (pch) you must have >=2 data points")
	  lim <- range(x)
      ## Next line was gun(seq())+offset,'x'  7apr03
	  xx <- if(grid)convertNative(gun(seq(lim[1],lim[2],by=inc) +
                                  offset),'x') else
	   seq(lim[1], lim[2], by=inc) + offset
	  if(length(xx)>1) xx <- xx[-1]
	  xx <- xx[xx<=lim[2]]
	  if(length(xx)==0) 
		warning('curve was too short to mark with a symbol.\nMay want to change point.inc or xmethod for labcurve')
	  else {
		yy <- approx(x, y, xout=xx,
                     method=if(type=='l')'linear' else 'constant', 
					 f=if(step.type=='left')0 else 1)$y
	  gfun$points(xx, yy, pch=pch, col=col.)
	  }
	}
    if(!length(point.inc)) point.inc <- diffu(xlim)/5
	for(i in 1:nc) sym(curves[[i]], keys[i], point.inc, (i-1)*point.inc/nc,
					   type[i], step.type, col.=col.[i],
                       grid, gfun)
	xt <- yt <- NULL
  } else {
	xt <- yt <- direction <- numeric(nc)
	angle <- rep(0,nc)

	g <- function(x) {  # finds min(abs(x)) but keeps original sign
	  ax <- abs(x)
      if(all(is.na(ax))) return(NA)   ## 29Jan02
	  w <- min(ax, na.rm=TRUE)
	  (x[ax==w])[1]   #use first occurrence
	}
	for(i in 1:nc) {
	  yi <- ys[,i]
	  yi[xs<rng[1,i] | xs>rng[2,i]] <- NA
	  diffmat <- ys[,-i,drop=FALSE] - yi
	  mindiff <- apply(diffmat, 1, g)
	  z <- abs(mindiff)==max(abs(mindiff),na.rm=TRUE)
	  maxid   <- min(c(1:length(mindiff))[z], na.rm=TRUE)
	  xt[i] <- xs[maxid]
	  yt[i] <- ys[maxid,i]
      if(!is.na(mindiff[maxid])) 
        direction[i] <- 1-2*(mindiff[maxid]>0)  ## if 16may03 + next if
	  yto <- yt[i] +
        direction[i]*(if(grid)convertNative(offset,'y') else offset)
      if(!is.na(yto)) 
      if(yto >= usr[4] || yto <= usr[3]) direction[i] <- -direction[i]

	  ## Find slope of curve i at xt[i]
	  if(tilt || adj.needs.to.vary) {
		angle[i] <- if(type[i]=='s') 0 else {
		  ci <- curves[[i]]
		  xx <- ci[[1]]; yy <- ci[[2]]
		  s <- is.finite(xx+yy)
		  w <- if(length(window)) window else {
			nch <- if(lines.keys) nchar(labels[i]) else 
			if(is.keys) 1*is.numeric(keys) + 
			  nchar(keys[i])*is.character(keys) else nchar(labels[i])
			w <- if(grid)
              nch*convertNative(unit(.75,"strwidth","m"),'x') else
              nch*strwidth('m','user',cex)
		  }
		  yy <- approx(xx[s], yy[s], xout=c(xt[i]-w/2,xt[i]+w/2),
					   rule=2)$y
		  slope <- diff(yy)/w
		  180*atan(slope*uin[2]/uin[1])/pi
		}
	  }
	  if(adj.needs.to.vary) {
		adj[i] <- if(type[i]=='s') 1*(direction[i]<0) else {
          ## is.na(angle[i]) 16may03
		  if(is.na(angle[i]) || abs(angle[i])<=angle.adj.auto).5 else
		  if((direction[i]<0 && slope>0) || 
			 (direction[i]>0 && slope<0)) 0 else 1
		}
	  }
	}

	if(!tilt) angle[] <- 0
	if(!lines.keys && method=='offset' && (!is.logical(labels) || labels)) {
	  if(is.keys) {
		if(is.numeric(keys)) for(i in 1:nc)
		  gfun$points(xt[i], (gun(yt) + direction*offset)[i], 
                      pch=keys[i], col=col.[i])
		  else if(i %in% whichLabel)    ## 17sep02
            gfun$text(xt, gun(yt) + direction*offset,
                      keys, cex=cex,  
                      adj=adj[1], col=col., ...)
	  }
		else {
		  if(tilt || adj.does.vary) for(i in whichLabel)   ## 17sep02
			gfun$text(xt[i], gun(yt[i])+direction[i]*offset, 
                      labels[i], cex=cex, srt=angle[i], 
                      adj=adj[i], col=col.[i],...) else
		  gfun$text(xt, gun(yt)+direction*offset, labels, 
                    cex=cex, adj=adj[1], col=col., ...)
		}
	}
	retlist <- list(x=xt, y=yt, offset=direction*offset,
					adj=adj, cex=cex, col=col., lwd=lwd, angle=if(tilt) angle, 
					key.opts=key.opts, ...)
  }

  if(method %in% c('on top','arrow') && (!is.logical(labels) || labels)) {

	retlist <- list(x=xt, y=yt, offset=0, 
					adj=.5, cex=cex, col=col., lwd=lwd, angle=0, 
					key.opts=key.opts, ...)

	if(method == 'on top' && !lines.keys) {
	  if(is.keys) {
		if(is.character(keys))
		  gfun$text(xt, yt, keys, cex=cex, col=col., adj=.5, ...)
		## numeric keys (periodic plotting symbols) already handled above
	  } else gfun$text(xt, yt, labels, cex=cex, col=col., adj=.5, ...)
	} else if(method=='arrow') {
	  ydelta <- if(grid)unit(1/17,'npc') else diffu(ylim)/17
	  xdelta <- if(grid)unit(1/26,'npc') else diffu(xlim)/26
	  lab.pos <- list(x=gun(xt) + xdelta*arrow.factor,
					  y=gun(yt) + ydelta*arrow.factor)

	  gfun$arrows(gun(xt)+xdelta*.6*arrow.factor,
                  gun(yt)+ydelta*.6*arrow.factor,
                  xt,yt,open=TRUE,size=.06,col=col.)
	  gfun$text(lab.pos, labels, cex=cex, col=col., ...)
	}
  }


  if(is.keys && (!is.character(keyloc) || keyloc!='none')) {
	## Make legend

    s <- whichLabel   ## 17sep02
	if(is.character(keyloc) && keyloc=='auto') {
	  ## Find emptiest spot for drawing legend by finding
	  ## center of largest empty rectangle large enough to hold 
	  ## this rectangle
	  Xs <- rep(xs, nc)
	  Ys <- as.vector(ys)
      putKeyEmpty(Xs, Ys,
                  labels=if(lines.keys || is.numeric(keys))labels[s] else
                   paste(keys,'    ',labels, sep='')[s],  # 27may02
                  pch=if(is.numeric(keys)) keys[s],
                  lty=lty[s], lwd=lwd[s], cex=cex, col=col.[s],
                  transparent=transparent, plot=TRUE,
                  key.opts=key.opts, xlim=xlim, ylim=ylim, grid=grid)
      ## added xlim 16Mar02
	} else putKey(keyloc,
                  labels=if(lines.keys || is.numeric(keys))labels[s] else
                   paste(keys,'    ',labels, sep='')[s],  # 27may02
                  pch=if(is.numeric(keys)) keys[s],
                  lty=lty[s], lwd=lwd[s], cex=cex, col=col.[s],
                  transparent=transparent, plot=TRUE,
                  key.opts=key.opts, grid=grid) # remove ylim 1Mar01
  }

  invisible(retlist)
}

# Version of legend for R that implements plot=FALSE, adds grid=TRUE
# Also defaults lty, lwd, pch to NULL and checks for length>0 rather
# than missing(), so it's easier to deal with non-applicable parameters
if(.R.) {
  rlegend <- function (x, y, legend, fill, col = "black", lty=NULL, lwd=NULL,
                       pch=NULL, angle = NULL,  
                       density = NULL, bty = "o", bg = par("bg"),
                       pt.bg = NA, cex = 1, 
                       xjust = 0, yjust = 1, x.intersp = 1, y.intersp= 1,
                       adj = 0, text.width = NULL,
                       merge = do.lines && has.pch, trace = FALSE, 
                       ncol = 1, horiz = FALSE, plot=TRUE, grid=FALSE,
                       ...) {

    gfun <- ordGridFun(grid)   ## see Misc.s

    if (is.list(x)) {
        if (!missing(y)) {
            if (!missing(legend)) 
                stop("`y' and `legend' when `x' is list (need no `y')")
            legend <- y
        }
        y <- x$y
        x <- x$x
    }
    else if (missing(y)) 
        stop("missing y")
    if (!is.numeric(x) || !is.numeric(y)) 
        stop("non-numeric coordinates")
    if ((nx <- length(x)) <= 0 || nx != length(y) || nx > 2) 
        stop("invalid coordinate lengths")
    xlog <- par("xlog")
    ylog <- par("ylog")
    rect2 <- function(left, top, dx, dy, ...) {
        r <- left + dx
        if (xlog) {
            left <- 10^left
            r <- 10^r
        }
        b <- top - dy
        if (ylog) {
            top <- 10^top
            b <- 10^b
        }
        gfun$rect(left, top, r, b, angle = angle, density = density, 
                  ...)
    }
    segments2 <- function(x1, y1, dx, dy, ...) {
        x2 <- x1 + dx
        if (xlog) {
            x1 <- 10^x1
            x2 <- 10^x2
        }
        y2 <- y1 + dy
        if (ylog) {
            y1 <- 10^y1
            y2 <- 10^y2
        }
        gfun$segments(x1, y1, x2, y2, ...)
    }
    points2 <- function(x, y, ...) {
        if (xlog) 
            x <- 10^x
        if (ylog) 
            y <- 10^y
        gfun$points(x, y, ...)
    }
    text2 <- function(x, y, ...) {
        if (xlog) 
            x <- 10^x
        if (ylog) 
            y <- 10^y
        gfun$text(x, y, ...)
    }
    if (trace) 
        catn <- function(...) do.call("cat", c(lapply(list(...), 
            formatC), list("\n")))
    pr  <- parGrid(grid)  ## 20Mar02 FEH
    cin <- pr$cin         ## FEH
    Cex <- cex * pr$cex   ## FEH
    if (!length(text.width)) ## FEH
        text.width <- max(strwidth(legend, u = "user", cex = cex))
    else if (!is.numeric(text.width) || text.width < 0) 
        stop("text.width must be numeric, >= 0")
    xc <- Cex * xInch(cin[1], warn.log = FALSE, grid=grid)  ## FEH in Misc.s
    yc <- Cex * yInch(cin[2], warn.log = FALSE, grid=grid)  ## FEH
    xchar <- xc
    yextra <- yc * (y.intersp - 1)
    ymax <- max(yc, strheight(legend, u = "user", cex = cex))
    ychar <- yextra + ymax
    if (trace) 
        catn("  xchar=", xchar, "; (yextra,ychar)=", c(yextra, 
            ychar))
    if (!missing(fill)) {
        xbox <- xc * 0.8
        ybox <- yc * 0.5
        dx.fill <- xbox
    }
    do.lines <- (length(lty) && any(lty > 0)) || length(lwd)
    n.leg <- length(legend)
    n.legpercol <- if (horiz) {
        if (ncol != 1) 
            warning(paste("horizontal specification overrides: Number of columns :=", 
                n.leg))
        ncol <- n.leg
        1
    }
    else ceiling(n.leg/ncol)
    if (has.pch <- length(pch)) {
        if (is.character(pch) && nchar(pch[1]) > 1) {
            if (length(pch) > 1) 
                warning("Not using pch[2..] since pch[1] has multiple chars")
            np <- nchar(pch[1])
            pch <- substr(rep(pch[1], np), 1:np, 1:np)
        }
        if (!merge) 
            dx.pch <- x.intersp/2 * xchar
    }
    x.off <- if (merge) 
        -0.7
    else 0
    if (xlog) 
        x <- log10(x)
    if (ylog) 
        y <- log10(y)
    if (nx == 2) {
        x <- sort(x)
        y <- sort(y)
        left <- x[1]
        top <- y[2]
        w <- diff(x)
        h <- diff(y)
        w0 <- w/ncol
        x <- mean(x)
        y <- mean(y)
        if (missing(xjust)) 
            xjust <- 0.5
        if (missing(yjust)) 
            yjust <- 0.5
    }
    else {
        h <- n.legpercol * ychar + yc
        w0 <- text.width + (x.intersp + 1) * xchar
        if (!missing(fill)) 
            w0 <- w0 + dx.fill
        if (has.pch && !merge) 
            w0 <- w0 + dx.pch
        if (do.lines) 
            w0 <- w0 + (2 + x.off) * xchar
        w <- ncol * w0 + 0.5 * xchar
        left <- x - xjust * w
        top <- y + (1 - yjust) * h
    }
    if (bty != "n") {
        if (trace) 
            catn("  rect2(", left, ",", top, ", w=", w, ", h=", 
                h, "...)", sep = "")
        if(plot) rect2(left, top, dx = w, dy = h, col = bg)  ## FEH
    }
    xt <- left + xchar + (w0 * rep(0:(ncol - 1), rep(n.legpercol, 
        ncol)))[1:n.leg]
    yt <- top - (rep(1:n.legpercol, ncol)[1:n.leg] - 1) * ychar - 
        0.5 * yextra - ymax
    if (!missing(fill)) {
        fill <- rep(fill, length.out = n.leg)
        if(plot) rect2(left = xt, top = yt + ybox/2, dx = xbox, dy = ybox, 
            col = fill)   ## FEH
        xt <- xt + dx.fill
    }
    if (has.pch || do.lines) 
        col <- rep(col, length.out = n.leg)
    if (do.lines) {
        seg.len <- 2
        ok.l <- if (!length(lty)) {
            lty <- 1
            TRUE
        }
        else lty > 0
        if (!length(lwd)) 
            lwd <- pr$lwd   ## FEH
        lty <- rep(lty, length.out = n.leg)
        lwd <- rep(lwd, length.out = n.leg)
        if (trace) 
            catn("  segments2(", xt[ok.l] + x.off * xchar, ",", 
                yt[ok.l], ", dx=", seg.len * xchar, ", dy=0, ...)", 
                sep = "")
        if(plot)segments2(xt[ok.l] + x.off * xchar, yt[ok.l], dx = seg.len * 
            xchar, dy = 0, lty = lty[ok.l], lwd = lwd[ok.l], 
            col = col[ok.l])   ## FEH
        xt <- xt + (seg.len + x.off) * xchar
    }
    if (has.pch) {
        pch <- rep(pch, length.out = n.leg)
        pt.bg <- rep(pt.bg, length.out = n.leg)
        ok <- is.character(pch) | pch >= 0
        x1 <- (if (merge) 
            xt - (seg.len/2) * xchar
        else xt)[ok]
        y1 <- yt[ok]
        if (trace) 
            catn("  points2(", x1, ",", y1, ", pch=", pch[ok], 
                "...)")
        if(plot)points2(x1, y1, pch = pch[ok], col = col[ok], cex = cex, 
            bg = pt.bg[ok])  ## FEH
        if (!merge) 
            xt <- xt + dx.pch
    }
    xt <- xt + x.intersp * xchar
    if(plot)text2(xt, yt, labels = legend, adj = adj, cex = cex) ## FEH
    invisible(list(rect = list(w = w, h = h, left = left, top = top), 
                   text = list(x = xt, y = yt)))
  }
  NULL
}

putKey <- function(z, labels, type=NULL,
                   pch=NULL, lty=NULL, lwd=NULL,
                   cex=par('cex'), col=rep(par('col'),nc),
                   transparent=TRUE, plot=TRUE, key.opts=NULL, grid=FALSE) {

  if(grid) {
    require('grid')
    require('lattice')  # use draw.key in lattice    29Jan02
  }
  
  if(!.R. && !existsFunction('key')) 
    stop('must do library(trellis) to access key() function')

  nc <- length(labels)
  if(!length(pch)) pch <- rep(NA, nc)
  if(!length(lty)) lty <- rep(NA, nc)
  if(!length(lwd)) lwd <- rep(NA, nc)
  
  pp <- !is.na(pch)
  lp <- !is.na(lty) | !is.na(lwd)
  lwd <- ifelse(is.na(lwd), par('lwd'), lwd)
  
  if(!length(type)) type <- ifelse(!(pp | lp), 'n',
                                   ifelse(pp & lp, 'b',
                                          ifelse(pp, 'p', 'l')))
  
  pch <- ifelse(is.na(pch) & type!='p' & type!='b',
                if(.R.)NA else 0, pch)  ## NA was 0 12dec02
  lty <- ifelse(is.na(lty) & type=='p',
                if(.R.)NA else 1, lty)  ## NA was 1 12dec02
  lwd <- ifelse(is.na(lwd) & type=='p', 1, lwd)
  cex <- ifelse(is.na(cex) & type!='p' & type!='b', 1, cex)

  if(!.R. && any(is.na(pch))) stop("pch can not be NA for type='p' or 'b'") #12dec02
  if(!.R. && any(is.na(lty))) stop("lty can not be NA for type='l' or 'b'") #12dec02
  if(any(is.na(lwd))) stop("lwd can not be NA for type='l' or 'b'")
  if(any(is.na(cex))) stop("cex can not be NA for type='p' or 'b'")
  
  m <- list()
  m[[1]] <- as.name(if(grid)'draw.key' else if(.R.)'rlegend' else 'key')
  if(!grid) {m$x <- z[[1]]; m$y <- z[[2]]}

  if(.R.) {
    if(grid) {
      w <- list(text=list(labels, col=col))
      ##    m$xjust <- m$yjust <- 0.5
      if(!(all(is.na(lty)) & all(is.na(lwd)))) {
        lns <- list()
        if(!all(is.na(lty))) lns$lty <- lty
        if(!all(is.na(lwd))) lns$lwd <- lwd
        lns$col <- col
        w$lines <- lns
      }
      if(!all(is.na(pch))) w$points <- list(pch=pch, col=col)
      ## was if(!all(is.na(pch)) && !all(pch==0)) w$points <- list(pch=pch, col=col) 12dec02
      ##    if(length(key.opts)) m[names(key.opts)] <- key.opts
      m$key <- w
      m$draw <- plot
      if(plot) m$vp <-
        viewport(x=unit(z[[1]],'native'),y=unit(z[[2]],'native'))
      z <- eval(as.call(m))
      size <- if(plot) c(NA,NA) else {
        if(version$major=='1' && as.numeric(version$minor) > 7.1) {
          width <- getFromNamespace('width','grid')
          height <- getFromNamespace('height','grid')
        }
        c(convertNative(width(z), 'x', 'dimension')[1],
          convertNative(height(z),'y', 'dimension')[1])
      }
      return(invisible(size))
    } else {
      m$legend <- labels
      m$xjust <- m$yjust <- .5
      m$plot <- plot
      m$col <- col
      m$cex <- cex
      if(!all(is.na(lty))) m$lty <- lty
      if(!all(is.na(lwd))) m$lwd <- lwd
      if(!all(is.na(pch))) m$pch <- pch
      ## was if(!all(is.na(pch)) && !all(pch==0)) m$pch <- pch 12dec02
      if(length(key.opts)) m[names(key.opts)] <- key.opts
      w <- eval(as.call(m))$rect
      return(invisible(c(w$w[1], w$h[1])))
    }
  }
  
  m$transparent <- transparent
  m$corner <- c(.5,.5)
  m$plot   <- plot
  m$type   <- type

  if(!plot) labels <- substring(labels, 1, 10)
  ## key gets length wrong for long labels
  m$text <- list(labels, col=col)
  if(all(type=='p')) m$points <- list(pch=pch, cex=cex, col=col)
  else m$lines <- if(any(type!='l'))
    list(lty=lty, col=col, lwd=lwd, pch=pch, cex=cex) else
  list(lty=lty, col=col, lwd=lwd)
                                                 
  if(length(key.opts)) m[names(key.opts)] <- key.opts
  invisible(eval(as.call(m)))  ## execute key(....)
}

putKeyEmpty <- function(x, y, labels, type=NULL,
                        pch=NULL, lty=NULL, lwd=NULL,
                        cex=par('cex'), col=rep(par('col'),nc),
                        transparent=TRUE, plot=TRUE, key.opts=NULL,
                        empty.method=c('area','maxdim'), 
                        numbins=25, 
                        xlim=pr$usr[1:2], ylim=pr$usr[3:4],
                        grid=FALSE) { 
  nc <- length(labels)
  empty.method <- match.arg(empty.method)

  pr <- parGrid(grid)
  uin <- pr$uin

  if(.R.) uin <- 1  ## already in x,y units
  z <- putKey(list(0, 0), labels, type, pch, lty, lwd, cex, col,
              transparent=transparent, plot=FALSE,
              key.opts=key.opts, grid=grid)/uin
  ## /uin converts to x,y units

  ## Find center of largest empty rectangle large enough to hold 
  ## this rectangle
  s  <- is.finite(x + y)
  if(length(xlim)) s <- s & (x >= xlim[1] & x <= xlim[2])
  if(length(ylim)) s <- s & (y >= ylim[1] & y <= ylim[2])
  x <- x[s]
  y <- y[s]
  keyloc <- largest.empty(x, y, xlim=xlim, ylim=ylim,
                          width=z[1], height=z[2],
                          method=empty.method, numbins=numbins, grid=grid)
  if(is.na(keyloc$x)) {
    cat('No empty area large enough for automatic key positioning.  Specify keyloc or cex.\n')
    cat('Width and height of key as computed by key(), in data units:',
        format(z),'\n')
    return(keyloc)
  } else if(plot) putKey(keyloc, labels, type,
                         pch, lty, lwd, cex, col, transparent, plot=TRUE,
                         key.opts=key.opts, grid=grid)
  invisible(keyloc)
}

largest.empty <- function(x, y, 
						  width, height, 
						  numbins=25,
						  method=c('area','maxdim'),
						  xlim=pr$usr[1:2], ylim=pr$usr[3:4],
						  pl=FALSE, grid=FALSE) {
  method <- match.arg(method)
  pr <- parGrid(grid)
  
  itype  <- 1*(method=='area')+2*(method=='maxdim')
  storage.mode(x) <- storage.mode(y) <- storage.mode(xlim) <-
	storage.mode(ylim) <- storage.mode(width) <-
      storage.mode(height) <- 'double'
  storage.mode(numbins) <- storage.mode(itype) <- 'integer'

  a <- if(.R.)
    .Fortran('largrec', x, y, length(x), 
             xlim, ylim, 
             width, height, numbins, itype,
             rx=double(2), ry=double(2), PACKAGE="Hmisc") else
    .Fortran('largrec', x, y, length(x), 
				xlim, ylim, 
				width, height, numbins, itype,
				rx=double(2), ry=double(2))
  x <- a$rx
  if(any(x > 1e29)) {
	warning('no empty rectangle was large enough')
	return(list(x=NA, y=NA))
  }
  y <- a$ry
  if(pl) ordGridFun(grid)$polygon(x[c(1,2,2,1)],y[c(1,1,2,2)], col=1+itype)
  list(x=mean(x), y=mean(y))
}


drawPlot <- function(..., xlim=c(0,1), ylim=c(0,1), xlab='', ylab='',
                     ticks=c('none','x','y','xy'),
                     key=FALSE, opts=NULL) {

  Points <- function(label=' ', type=c('p','r'), n, pch=pch.to.use[1],
                     cex=par('cex'), rug=c('none','x','y','xy'),
                     ymean=NULL) {
    type <- match.arg(type)
    rug <- match.arg(rug)
    cat('\nClick mouse for each point',
        if(label!='')paste(' for group ',label),'.',
        if(missing(n))' Right click when finished.', '\n',sep='')
    pts <- if(missing(n)) locator(type='p',pch=pch,cex=cex) else
      locator(n, type='p', pch=pch, cex=cex)
    if(length(ymean)) pts$y <- pts$y - mean(pts$y) + ymean  ## 26Jan01
    if(type=='p') 
      storeTemp(pch.to.use[pch.to.use != pch],'pch.to.use')
    else {
      scat1d(pts$x, side=1)
      pch <- NA
    }
    switch(rug,
           x = scat1d(pts$x, side=1),
           y = scat1d(pts$y, side=2),
           xy = {scat1d(pts$x, side=1); scat1d(pts$y, side=2)},
           none = )
           
    structure(list(points=pts, label=label, type=type,
                   pch=pch, cex=cex, rug=rug), class='Points')
  }

  Curve <- function(label=' ',
                    type=c('bezier','polygon','linear','pol','step','gauss'),
                    n=NULL, lty=1, lwd=par('lwd'), degree=2,
                    evaluation=100, ask=FALSE) {
    isfun <- is.function(type)
    if(!isfun) type <- match.arg(type)
    if(!isfun && !length(n) && type=='linear') n <- 2
    if(!isfun && type=='gauss') n <- 3
    xlim <- par('usr')[1:2]
    redraw <- TRUE
    
    if(isfun) {
      x <- seq(xlim[1], xlim[2], length=evaluation)
      pts <- list(x=as.single(x), y=as.single(type(x)))
      lines(pts, lty=lty, lwd=lwd)
    } else repeat {
      cat('\nClick mouse for each point',
          if(label!='')paste(' for group ',label),'.',
          if(!length(n))' Right click when finished.', '\n', sep='')
      pts <- if(!length(n)) locator(type='l', lty=lty, lwd=lwd) else
      locator(n, type='l', lty=lty, lwd=lwd)
      n <- length(pts$x)
      if(n < 2) stop('must click at least 2 points')
      if(n==2) type <- 'linear'

      if(type=='pol') {
        x <- matrix(NA, nrow=n, ncol=degree)
        for(i in 1:degree) x[,i] <- pts$x^i
        f <- lm.fit.qr.bare(x, pts$y)
        x <- matrix(NA, nrow=evaluation, ncol=degree)
        x[,1] <- seq(min(pts$x),max(pts$x), length=evaluation)
        if(degree > 1) for(i in 1:degree) x[,i] <- x[,1]^i
        cof <- f$coefficients
        y <- cof[1] + x %*% cof[-1]
        pts <- list(x=as.single(x[,1]), y=as.single(y))
        if(redraw) lines(pts, lty=lty, lwd=lwd)
      }

      if(type=='bezier') {
        pts <- bezier(pts, xlim=range(pts$x), evaluation=evaluation)
        if(redraw) lines(pts, lty=lty, lwd=lwd)
      }
      if(type=='gauss') {
        mu <- pts$x[2]
        delta <- diff(pts$x[-2])/2
        htavg <- sum(pts$y[-2])/2
        htmax <- pts$y[2]
        x <- seq(xlim[1], xlim[2], length=evaluation)
        b2 <- delta^2 / log(htmax/htavg)
        y <- htmax * exp(-(x-mu)^2/b2)
        i <- y > 1e-4
        pts <- list(x=as.single(x[i]), y=as.single(y[i]))
        lines(pts, lty=lty, lwd=lwd)
      }
      if(type=='step' && redraw)
        lines(pts, type='s', lty=lty, lwd=lwd)

      if(!ask) break
      if(readline('\nType y to accept, n to re-draw:')=='y') break
    }
    
    structure(list(points=pts, label=label, type=type, lty=lty,
                   lwd=lwd),  class='Curve')
  }

  Abline <- function(...) {
    abline(...)
    structure(list(...), class='Abline')
  }
  
  storeTemp(Points)
  storeTemp(Curve)
  storeTemp(Abline)
  
  storeTemp(c(1,2,3,4,16,17,5,6,15,18,19),'pch.to.use')

  ticks <- match.arg(ticks)
  if(missing(ticks)) {
    if(!missing(xlim)) ticks <- 'x'
    if(!missing(ylim)) ticks <- 'y'
    if(!missing(xlim) && !missing(ylim)) ticks <- 'xy'
  }
  plot(xlim, ylim, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab,
       type='n', axes=ticks=='xy')

  switch(ticks,
         none = {axis(1, at=xlim, labels=FALSE)
                 axis(2, at=ylim, labels=FALSE)},
         x    = {axis(1)
                 axis(2, at=ylim, labels=FALSE)},
         y    = {axis(1, at=xlim, labels=FALSE)
                 axis(2)},
         xy   = )

  W <- list(...)
  m <- length(W)
  type <- label <- rep('', m)
  lty <- lwd <- pch <- cex <- rep(NA, m)
  curves <- vector('list', m)
  i <- 0
  for(j in 1:m) {
    w <- W[[j]]
    if(attr(w,'class')=='Abline') next
    i <- i + 1
    isfun <- is.function(w$type)
    curves[[i]] <- if(!key || isfun) w$points else
    switch(w$type,
           step = approx(w$points,
             xout=seq(min(w$points$x),max(w$points$x),length=50),
             method='constant', f=0),
           linear = approx(w$points,
             xout=seq(min(w$points$x),max(w$points$x),length=50)),
           w$points)
    label[i] <- w$label
    type[i] <- if(isfun) 'l' else switch(w$type, p='p', r='r', step='s', 'l')
    if(type[i]=='p') {
      pch[i] <- w$pch
      cex[i] <- w$cex
    } else if(type[i] != 'r') {  ## if( ) 12dec02
      lty[i] <- w$lty
      lwd[i] <- w$lwd
    }
  }
  if(i < m) {
    curves <- curves[1:i]
    label  <- label[1:i]
    type   <- type[1:i]
    lty    <- lty[1:i]
    lwd    <- lwd[1:i]
    pch    <- pch[1:i]
    cex    <- cex[1:i]
  }
  keyloc <- NULL
  j <- type!='r'
  if(any(j)) {   ## 12dec02
    if(!key)
      labcurve(curves[j], labels=label[j], type=type[j],
               lty=lty[j], lwd=lwd[j], opts=opts) else {
                 x <- unlist(lapply(curves, function(z)z$x))
                 y <- unlist(lapply(curves, function(z)z$y))
                 keyloc <- putKeyEmpty(x, y, labels=label[j], type=type[j],
                                       pch=pch[j], lty=lty[j],
                                       lwd=lwd[j], cex=cex[j])
               }
  }

  structure(list(W, xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim,
                 ticks=ticks, key=key, keyloc=keyloc, opts=opts),
            class='drawPlot')
}

bezier <- function(x, y, xlim, evaluation=100) {
  if(missing(y)) {
    y <- x[[2]]
    x <- x[[1]]
  }
  n <- length(x)
  X <- Y <- single(evaluation)
  Z <- seq(0, 1, length=evaluation)
  X[1] <- x[1]; X[evaluation] <- x[n]
  Y[1] <- y[1]; Y[evaluation] <- y[n]
  for(i in 2:(evaluation-1)) {
    z <- Z[i]
    xz <- yz <- 0
    const <- (1 - z)^(n-1)
    for(j in 0:(n-1)) {
      xz <- xz + const*x[j+1]
      yz <- yz + const*y[j+1]
      const <- const* (n-1-j)/(j+1) * z/(1-z)
      if(is.na(const))prn(c(i,j,z))
    }
    X[i] <- xz; Y[i] <- yz
  }
  list(x=as.single(X), y=as.single(Y))
}

plot.drawPlot <- function(x, file, xlab, ylab, ticks,
                          key=x$key, keyloc=x$keyloc, ...) {
  if(missing(xlab)) xlab <- x$xlab
  if(missing(ylab)) ylab <- x$ylab
  xlim <- x$xlim
  ylim <- x$ylim
  if(missing(ticks)) ticks <- x$ticks

  if(!missing(file)) setps(file, type='char', ...)

  plot(xlim, ylim, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab,
       type='n', axes=ticks=='xy')
  switch(ticks,
         none = {axis(1, at=xlim, labels=FALSE)
                 axis(2, at=ylim, labels=FALSE)},
         x    = {axis(1)
                 axis(2, at=ylim, labels=FALSE)},
         y    = {axis(1, at=xlim, labels=FALSE)
                 axis(2)},
         xy   = )

  data <- x[[1]]
  m <- length(data)
  type <- label <- rep('', m)
  lty <- lwd <- pch <- cex <- rep(NA, m)
  curves <- vector('list', m)
  i <- 0
  for(j in 1:m) {
    w <- data[[j]]
    if(attr(w, 'class') == 'Abline') {
      do.call("abline", oldUnclass(w))
      next
    }
    i <- i + 1
    if(is.function(w$type)) w$type <- 'l'
    curves[[i]] <- if(!key) w$points else
    switch(w$type,
           step = approx(w$points,
             xout=seq(min(w$points$x),max(w$points$x),length=50),
             method='constant', f=0),
           linear = approx(w$points,
             xout=seq(min(w$points$x),max(w$points$x),length=50)),
           w$points)
    label[i] <- w$label
    switch(attr(w, 'class'),
           Points = {
             type[i] <- w$type
             pch[i] <- w$pch
             cex[i] <- w$cex
             switch(w$type,
                    p = points(w$points, cex=w$cex, pch=w$pch),
                    r = scat1d(w$points$x, side=1))
             switch(w$rug,
                    x = scat1d(w$points$x, side=1),
                    y = scat1d(w$points$y, side=2),
                    xy = {scat1d(w$points$x, side=1)
                          scat1d(w$points$y, side=2)},
                    none = )
           },
           Curve = {
             type[i] <- if(w$type=='step')'s' else 'l'
             lty[i] <- w$lty
             lwd[i] <- w$lwd
             lines(w$points, lty=w$lty, lwd=w$lwd, type=type[i])
           })
  }

  if(i < m) {
    curves <- curves[1:i]
    label  <- label[1:i]
    type   <- type[1:i]
    pch    <- pch[1:i]
    lty    <- lty[1:i]
    lwd    <- lwd[1:i]
    cex    <- cex[1:i]
  }
    
  if(key && !length(keyloc))
    stop('you may not specify key=T unless key=T was specified to drawPlot or keyloc is specified to plot')

  if(any(label!='')) {
    j <- type!='r'
    if(any(j)) {  ## 12dec02
      if(key) putKey(keyloc, labels=label[j],
                     type=type[j], pch=pch[j],
                     lty=lty[j], lwd=lwd[j], cex=cex[j]) else
      labcurve(curves[j], type=type[j],
               lty=lty[j], lwd=lwd[j], labels=label[j], opts=x$opts)
    }
  }                
  if(!missing(file)) {
    dev.off()
    cat('\nCreated file ',file,'.ps\n',sep='')
  }
  invisible()
}


#"label<-"  <- function(x, value) {
#    attr(x, "label") <- value
#    x
#    }

label <- function(x, units=FALSE, plot=FALSE, default=NULL, grid=FALSE)  {
  at <- attributes(x)
  lab <- at$label
  if(length(default) && !length(lab)) lab <- default
  un  <- at$units
  labelPlotmath(lab, if(units)un else NULL, plotmath=plot, grid=grid)
}

labelPlotmath <- function(label, units=NULL, plotmath=.R., grid=FALSE)
{
  ## For now, Lattice/Grid in R do not support expressions
  if(grid) plotmath <- FALSE

  if(!length(label)) label <- ''
  if(!length(units)) units <- ''
  g <- if(plotmath && .R.)
    function(x,y=NULL, xstyle=NULL, ystyle=NULL) {
      h <- function(w, style=NULL)
        if(length(style)) paste(style,'(',w,')',sep='') else w
      if(!length(y)) return(parse(text=h(plotmathTranslate(x),xstyle)))
      x <- paste('list(',h(plotmathTranslate(x),xstyle),',',
                 h(plotmathTranslate(y),ystyle),')',sep='')
      parse(text=x)
    } else
  function(x, y=NULL, ...) if(length(y)) paste(x,y) else x

  if(units=='') g(label)
#    return(if(label=='' || !.R. || !plotmath) label
#    else g(paste('paste("',label,'")',sep='')))
  ## paste('foo') allows foo to have blanks in plotmath
  
  else if(label=='') g(units)
  else if(plotmath && .R.) g(label, units, ystyle='scriptstyle')
#  label <- if(plotmath && .R.)
#    paste('list(paste("',label, '"), scriptstyle(~~',units,'))',sep='')
  else paste(label,' [',units,']',sep='')
}

plotmathTranslate <- function(x) {
  if(length(grep('paste', x))) return(x)
  specials <- c(' ','%','_')
  spec <- FALSE
  for(s in specials) if(length(grep(s,x))) spec <- TRUE
  if(spec) x <- paste('paste("',x,'")',sep='')
  x
}
  
##From Bill Dunlap, StatSci  15Mar95:

"label<-" <- if(!.SV4.) function(x, value)
  structure(x, label=value,
            class=c('labelled',
              attr(x,'class')[attr(x,'class')!='labelled'])) else
function(x, value) {    # 1Nov00 for Splus 5.x, 6.x
##  oldClass(x) <- unique(c('labelled', oldClass(x),
##                          if(is.matrix(x))'matrix'))
  attr(x,'label') <- value
  x
}

if(!.SV4.) "[.labelled"<- function(x, ...) {
  atr <- attributes(x)
##  lab <- attr(x, "label")  19sep02
  x <- NextMethod("[")
  attr(x, "label") <- atr$label
  if(length(atr$units)) attr(x,'units') <- atr$units
  if(!inherits(x,'labelled'))
    attr(x,'class') <- c("labelled", attr(x,'class'))
  x
}

if(FALSE) {
  y <- matrix(1:12, nrow=4)
  class(y)
  oldClass(y) <- 'labelled'
  class(y)
  oldClass(y)
  attr(y,'label') <- 'Y'

  ##y <- structure(matrix(1:12, nrow=4), class=c('labelled','matrix'), label='Y')

  a <- structure(list(x=1:4, y=y),
                 class='data.frame',row.names=c('a','b','c','d'))
  a[1:4,1]
  a[1:4,2]
}

if(!.SV4.) "print.labelled"<- function(x, ...)
{
	x.orig <- x
    u <- attr(x,'units')   ## 19sep02
    if(length(u)) attr(x,'units') <- NULL   # so won't print twice
	cat(attr(x, "label"),if(length(u))paste('[',u,']',sep=''), "\n")
	attr(x, "label") <- NULL
#	attr(x,'class') <- setdiff(attr(x,'class'), "labelled")
    # The above didn't work under R 20Mar01
    class(x) <- if(length(class(x))==1 && class(x)=='labelled') NULL else
                class(x)[class(x) != 'labelled']
    ## 3mar03 - added NULL part above, to work for R 1.7
	# next line works around print bug
	if(!length(attr(x,'class')))
		attr(x,'class') <- NULL
	NextMethod("print")
	invisible(x.orig)
}
if(.R.) as.data.frame.labelled <- as.data.frame.vector

if(!.R. && version$major < 5) as.data.frame.labelled <- function(x, ...) {
  y <- x
  cy <- attr(y,'class')
  cy <- if(length(cy)>1) cy[cy!='labelled'] else NULL
  if(length(cy)==0) cy <- NULL  # handles wierd case e.g. class=rep('lab..',2)
  attr(y,'class') <- cy
  # data.class(character(0) class) returns ''
  d <- data.class(y)
  methodname <- paste("as.data.frame", d, sep = '.')
  if(exists(methodname, mode = "function"))
            (get(methodname, mode = "function"))(x, ...)
#  else stop(paste("no method for coercing", d, "to data.frame")) 26May97
  else {
	if(options()$check)
		warning(paste("no method for coercing",d,"to data.frame"))
	as.data.frame.AsIs(y, ...)
  }
}


Label <- function(object, ...)
   UseMethod("Label")
 
Label.data.frame <- function(object, file='', append=FALSE, ...) {
  nn <- names(object)
  for(i in 1:length(nn)) {
    lab <- attr(object[[nn[i]]],'label')
    lab <- if(length(lab)==0) '' else lab
    cat("label(",nn[i],")\t<- '",lab,"'\n", 
        append=if(i==1)append else TRUE, file=file, sep='')
  }
invisible()
}

reLabelled <- function(object) {
  for(i in 1:length(object)) {
    x <- object[[i]]
    lab <- attr(x, 'label')
    cl  <- oldClass(x)
    if(length(lab) && !any(cl=='labelled')) {
      oldClass(x) <- c('labelled',cl)
      object[[i]] <- x
    }
  }
  object
}


llist <- function(..., labels=TRUE) {
  dotlist <- list(...)
  lname <- names(dotlist)
  name <- vname <- as.character(sys.call())[-1]
  for(i in 1:length(dotlist)) {
    vname[i] <- if(length(lname) && lname[i]!='') lname[i] else name[i]
    ## Was changed 21Mar01 - R barked at setting vname[i] to NULL
	lab <- vname[i]
	if(labels) {
	  lab <- attr(dotlist[[i]],'label')
	  if(length(lab) == 0) lab <- vname[i]
	} 
	label(dotlist[[i]]) <- lab
  }
  names(dotlist) <- vname[1:length(dotlist)]
  dotlist
}
##!!WRONG ARG x in !.SV4. def latex generic!
#Changed x to object inside latex() for !.SV4. (Thanks David Lovell)

#Thanks to David R. Lovell <David.Lovell@cmis.csiro.au> CSIRO
#for scientific=    8Feb2000

first.word <- function(x, i=1, expr=substitute(x)) {
  words <- if(!missing(x)) as.character(x)[1] else
    as.character(unlist(expr))[1]
  ## Added !missing(x) as.char(x) 25May01
#	first.letters <- substring(words, 1, 1)
#	word.selector <- (match(first.letters, c(letters,LETTERS,"."), 0) > 0)
#	words <- words[word.selector][i]
#	if(!under.unix) {
#	  words <- sedit(words,'.','')
#	  words <- substring(words,1,8)
#	}
    ##18Nov00 FEH:
    if(i > 1) stop('i > 1 not implemented')
    chars <- substring(words, 1:nchar(words), 1:nchar(words))
    legal.chars <- c(letters,LETTERS,'.',
                     '0','1','2','3','4','5','6','7','8','9')
    non.legal.chars <- (1:length(chars))[chars %nin% legal.chars]
    if(!any(non.legal.chars)) return(words)
    if(non.legal.chars[1]==1) return(character(0))
    substring(words, 1, non.legal.chars[1]-1)
}

#1. if x is a data.frame, then do each component separately.
#2. if x is a matrix, but not a data.frame, make it a data.frame
#   with individual components for the columns.
#3. if a component x$x is a matrix, then do all columns the same.
#4. Use right justify by default for numeric columns.
#5. Use left justify for non-numeric columns.

# The following are made complicated by matrix components of data.frames:
#6. vector cdec must have number of items equal to number of columns
#   of input x.
#7. matrix dec must have number of columns equal to number of columns
#   of input x.
#8. scalar dec is expanded to a vector cdec with number of items equal
#   to number of columns of input x.
#9. vector rdec must have number of items equal to number of rows of input x.
#   rdec is expanded to matrix dec.
#10. col.just must have number of columns equal to number of columns
#    of output cx.

# Value:
# character matrix with character images of properly rounded x.
# matrix components of input x are now just sets of columns of character matrix.
# attr(,col.just) repeats input col.just when provided.
#	Otherwise, recommended justification for columns of output.
#	Default is "l" for characters and factors, "r" for numeric.
#	When dcolumn==T, numerics will have ".".


#FEH 21May96 - changed default for numeric.dollar to cdot
#FEH  5Jun96 - re-written to not rely on as.data.frame,
#              converted data frames to matrices the slow way
#              added matrix.sep 
#    12Aug99 - allowed # decimal places=NA (no rounding, just use format())
#    27May02 - added booktabs FEH
##   13Dec02 - added ctable   FEH
## arguments included check.names=TRUE 23jan03
format.df <- function(x,
	digits, dec=NULL, rdec=NULL, cdec=NULL, numeric.dollar=cdot,
	na.blank=FALSE, na.dot=FALSE, blank.dot=FALSE,
	col.just=NULL,
    cdot=FALSE, dcolumn=FALSE, matrix.sep=' ', scientific=c(-4,4), ...)
{

if(cdot && dcolumn) stop('cannot have both cdot=T and dcolumn=T')

if(missing(digits)) digits <- NULL
if((!length(digits))+(!length(dec))+(!length(rdec))+(!length(cdec)) < 3)
  stop('only one of digits, dec, rdec, cdec may be given')
# if(length(digits)) .Options$digits    6Aug00 what was that?
if(length(digits)) {
  oldopt <- options(digits=digits)
  on.exit(options(oldopt))
}

## For now nsmall and scientific are ignored in R  25May01
formt <- if(!.R.) format.default else
 function(x, decimal.mark='.', nsmall=0, scientific=c(-4,4)) {
   x <- format(x)
   if(decimal.mark!='.') x <- gsub('\\.',decimal.mark,x)
   x
 }
  
dot <- if(cdot) (if(.R.)'\\\\cdotp\\\\!' else '\\cdotp\\!') else '.'

if(is.data.frame(x)) x <- unclass(x)
xtype <- if(is.list(x)) 1 else if(length(dim(x))) 2 else 3  
#Following changed as above 10Mar01
#atx <- attributes(x)
#cl <- atx$class
#if(length(cl) && (idf <- any(cl=='data.frame'))) 
#  attr(x,'class') <- cl[cl!='data.frame']
#xtype <- if(is.list(x))1 else if(length(atx$dim))2 else 3
ncx <- if(xtype==1) length(x) else if(xtype==2)ncol(x) else 1
nams <- if(xtype==1) names(x) else if(xtype==2)dimnames(x)[[2]] else ''
if(!length(nams)) nams <- rep('', ncx)  ## 19apr03
nrx <- if(xtype==1) {
  if(length(d <- dim(x[[1]]))) d[1] else length(x[[1]])
} else if(xtype==2) nrow(x) else length(x)

rnam <- if(xtype==1) attr(x,'row.names') else
  if(xtype==2)dimnames(x)[[1]] else names(x)

if(length(dec)+length(rdec)+length(cdec)==0) rtype <- 1
if(length(rdec)) {
  rtype <- 2
  dec <- matrix(rdec, nrow=nrx, ncol=ncx)
}
if(length(dec)) {
  rtype <- 3
  if(length(dec)==1) cdec <- rep(dec, ncx)
}
if(length(cdec)) rtype <- 4

cx <- NULL
nam <- NULL
cjust <- NULL

if(blank.dot) sas.char <- function(x) {
	n.x <- nchar(x)
	blanks.x <-
	sapply(n.x, function(n.x.i) paste(rep(" ", n.x.i), collapse=""))
	ifelse(x == blanks.x, ".", x)
}

for(j in 1:ncx) {
  xj <- if(xtype==1) x[[j]] else if(xtype==2) x[,j] else x
  namj <- nams[j]
  num <- is.numeric(xj) || all(is.na(xj)) ## 16sep03
  if(isChron(xj)) num <- FALSE            ## 16sep03
 #using xtype avoids things like as.matrix changing special characters 
  ncxj <- max(1,dim(xj)[2], na.rm=TRUE)
  ## Added na.rm=T 5Jan01: SV4 makes dim(xj)=single number if x is data.frame

  for(k in 1:ncxj) {
    xk <- if(ld <- length(dim(xj))==2)xj[,k] else xj
    ## Added ==2 5Jan01
    names(xk) <- NULL   # gets around bug in format.default when 
                        # nsmall is given and there are NAs
    namk <- if(ld) {
      dn <- dimnames(xj)[[2]][k]
      if(length(dn)==0) dn <- as.character(k)
      dn
    } else ''
    namk <- paste(namj, if(namj!='' && namk!='')matrix.sep else '', namk,
                  sep='')
    if(num) {
      cj <- if(length(col.just)) col.just[j] else 'r'
      if(rtype==1) cxk <- formt(xk, decimal.mark=dot, scientific=scientific)
      else if(rtype==3) {
		cxk <- character(nrx)  ## corrected 4Nov97 Eric Bissonette
		for(i in 1:nrx) cxk[i] <-
          if(is.na(dec[i,j])) formt(xk[i], decimal.mark=dot,
                                     scientific=scientific) else
        formt(round(xk[i], dec[i,j]), decimal.mark=dot,
               nsmall=dec[i,j], scientific=scientific)
        ## 12Aug99
	  }
      else if(rtype==4)  # 12Aug99
        cxk <- if(is.na(cdec[j])) formt(xk, decimal.mark=dot,
                                         scientific=scientific) else 
        formt(round(xk, cdec[j]), decimal.mark=dot, nsmall=cdec[j],
               scientific=scientific)
      if(na.blank) cxk[is.na(xk)] <- ''
      if(na.dot) cxk[is.na(xk)] <- '.'  # SAS-specific
      if(blank.dot) cxk <- sas.char(cxk)
      if(numeric.dollar) cxk <- paste("$",cxk,"$",sep="")
# These columns get real minus signs in LaTeX, not hyphens,
# but lose alignment unless their col.just="r"
      if(dcolumn | (length(col.just) && col.just[j]=='c')) {
        cxk <- sedit(cxk, " ", "~")
        if(dcolumn) cj <- "."
      } 
    } else {   #ended if(num)
      cj <- if(length(col.just)) col.just[j] else 'l'
      cxk <- as.character(xk)
    }         
  cx <- cbind(cx, cxk)
  nam <- c(nam, namk)
  cjust <- c(cjust, cj)
  }    #end for k
}              #end for j

dimnames(cx) <- list(rnam, nam)
attr(cx,"col.just") <- cjust
cx
}

#first.hline.double added FEH 11Jun95
#Usage:
#	latex(x) # for x any S object

#Value is a file object of class=c("latex","file") which is
#automatically printed by print.latex(), which constructs a file object
#of class=c("dvi","file"), and automatically prints it using
#print.dvi().  print.latex() returns an invisible file object.


# dcolumn numeric.dollar cdot
#
# dc cd nd  format.df latex.default  # comment
# F  F  T	   $		     # LaTeX usage
# F  T  T   \cdot! $		     # LaTeX usage
# T  F  F   . ~	      .	    dcolumn  # LaTeX usage
# T  T  F   . ~	      \cdot dcolumn  # LaTeX usage
#        
# F  F  F    			     # non-TeX (hyphens in TeX)
#        
# F  T  F   \cdot!		     # TeX errors, hyphens
# T  F  T   . ~	   $  .	    dcolumn  # TeX errors
# T  T  T   . ~	   $  \cdot dcolumn  # TeX errors



latex.default <-
  function(object,
           title=first.word(deparse(substitute(object))),
           file=paste(title, ".tex", sep=""),
           append=FALSE, label=title,
           rowlabel=title, rowlabel.just="l", cgroup=NULL, n.cgroup=NULL,
           rgroup=NULL, n.rgroup=NULL,
           rowname, cgroup.just=rep("c",length(n.cgroup)),
           colheads=dimnames(cx)[[2]],
           extracolheads=NULL, extracolsize='scriptsize',
           dcolumn=FALSE, numeric.dollar=!dcolumn, cdot=FALSE,
           longtable=FALSE, draft.longtable=TRUE, ctable=FALSE, booktabs=FALSE,
           table.env=TRUE, here=FALSE, lines.page=40,
           caption=NULL, caption.lot=NULL, caption.loc=c('top','bottom'),
           double.slash=FALSE,
           vbar=FALSE, collabel.just=rep("c",nc), na.blank=TRUE,
           insert.bottom=NULL, first.hline.double=!(booktabs | ctable),
           where='!tbp', size=NULL,
           center=c('center','centering','none'),
           landscape=FALSE,
           ...)      ## center MJ 08sep03
{
  center <- match.arg(center)
  caption.loc <- match.arg(caption.loc)
  cx <- format.df(object, dcolumn=dcolumn, na.blank=na.blank,
                  numeric.dollar=numeric.dollar, cdot=cdot, ...)
  # removed check.names=FALSE from above 23jan03
  if (missing(rowname)) rowname <- dimnames(cx)[[1]]
  col.just <- attr(cx,"col.just")
  nc <- ncol(cx)
  nr <- nrow(cx)

if (length(cgroup)) {
  k <- length(cgroup)
  if(!length(n.cgroup)) n.cgroup <- rep(nc/k, k)
  if(sum(n.cgroup)!=nc) stop("sum of n.cgroup must equal number of columns")
  if(length(n.cgroup)!=length(cgroup))
    stop("cgroup and n.cgroup must have same lengths")
}

  if(!length(rowname)) rgroup <- NULL

  if(!length(n.rgroup) && length(rgroup))
	n.rgroup <- rep(nr/length(rgroup), length(rgroup))
  if(length(n.rgroup) && sum(n.rgroup)!=nr)
	stop("sum of n.rgroup must equal number of rows in object")
  if(length(rgroup) && length(n.rgroup) && (length(rgroup)!=length(n.rgroup)))
	stop("lengths of rgroup and n.rgroup must match")
  if (length(rgroup) && rowlabel.just=="l")
	rowname <- paste("~~",rowname,sep="")

  sl <- ifelse(double.slash, "\\\\", "\\")
  eol <- if(ctable) paste(sl, 'NN', sep='') else paste(sl,sl,sep='')
  if(booktabs) {  # 27may02
    toprule    <- paste(sl,"toprule",sep="")
    midrule    <- paste(sl,"midrule",sep="")
    bottomrule <- paste(sl,"bottomrule",sep="")
  } else if(ctable) {   ## 13dec02
    toprule    <- paste(sl, 'FL', sep='')
    midrule    <- paste(sl, 'ML', sep='')
    bottomrule <- paste(sl, 'LL', sep='')
    } else {
    toprule <- if(first.hline.double)
      paste(sl,"hline",sl,"hline",sep="") else paste(sl,"hline",sep="")
    midrule <- bottomrule <- paste(sl,"hline",sep="")
  }

#if (!vbar && length(cgroup)) {
  if (length(cgroup)) {
	last.col <- cumsum(n.cgroup)
	first.col <- c(1, 1+last.col[-length(last.col)])
	cgroup.cols <- cbind(first.col,last.col)
	col.subs <- list()	
    for (i in seq(along=first.col))
      col.subs[[i]] <- first.col[i]:last.col[i]
    
    cxi <- list()
    for (i in seq(along=col.subs)) cxi[[i]] <- cx[,col.subs[[i]],drop=FALSE]
    
    cxx <- cxi[[1]]
    col.justxx <- col.just[col.subs[[1]]]
    collabel.justxx <- collabel.just[col.subs[[1]]]

	cgroupxx <- cgroup[1]
	n.cgroupxx <- n.cgroup[1]
	for (i in seq(along=col.subs)[-1]) {
      cxx <- cbind(cxx, "", cxi[[i]])  # was ""="" 23Feb01 "=" 2Apr02
      col.justxx <- c(col.justxx, "c", col.just[col.subs[[i]]])
      collabel.justxx <-
        c(collabel.justxx, "c", collabel.just[col.subs[[i]]])
      cgroupxx <- c(cgroupxx, "", cgroup[i])
      n.cgroupxx <- c(n.cgroupxx, 1, n.cgroup[i])
	}
	cgroup.colsxx <- cgroup.cols + 0:(nrow(cgroup.cols)-1)
    
	cx <- cxx
	col.just <- col.justxx
	collabel.just <- collabel.justxx
	n.cgroup <- n.cgroupxx
	cgroup.cols <- cgroup.colsxx[cgroup!="",,drop=FALSE]
	cgroup <- cgroupxx
	nc <- ncol(cx)
  }

  if (length(rowname)) {
    cx <- cbind(rowname, cx)
    dimnames(cx)[[2]][1] <- rowlabel
    col.just <- c(rowlabel.just, col.just)
    if(length(extracolheads))
      extracolheads <- c('', extracolheads)  ## 16jun03
    
    collabel.just <- c(rowlabel.just, collabel.just)
    if (!length(cgroup)) {
      n.cgroup <- c(1, nc)
      cline <- NULL
    }
    else {
      cgroup <- c(rowlabel, cgroup)
      dimnames(cx)[[2]][1] <- ""
      rlj <- ifelse(rowlabel.just=="l", "l", "c")
      cgroup.just <- c(rlj, cgroup.just)
      n.cgroup <- c(1, n.cgroup)
      cgroup.cols <- 1+cgroup.cols
      cline <- paste(sl, "cline{", cgroup.cols[,1],"-", cgroup.cols[,2], "}",
                     sep="", collapse=" ")
    }
    nc <- 1 + nc
  }

  vbar <- ifelse(vbar, "|", "")

  if(!append) cat("", file=file)	#start new file
  cat("%",deparse(sys.call()), "\n%\n", file=file, append=file!='')
  ## append= 19apr03 and other places
  ## Was as.character(as.name(match.call()))  15Sep00

  if(dcolumn) {
    decimal.point <- ifelse(cdot, paste(sl,"cdot",sep=""), ".")
    cat(sl,"newcolumntype{.}{D{.}{",decimal.point,"}{-1}}\n",
        sep="", file=file, append=file!='')  # was newcolumn 26Feb02
  }

  { # tabular.cols
    tabular.cols <- paste(vbar, col.just, sep="")
    if (!length(n.cgroup)) tabular.cols <- c(tabular.cols, vbar)
    else {
      vv2 <- cumsum(n.cgroup)
      tabular.cols[vv2] <- paste(tabular.cols[vv2],vbar,sep="")
    }
    tabular.cols <- paste(tabular.cols, collapse="")
  }

  if(length(caption) && !ctable) {
    caption <- paste(
                     sl,"caption",
                     if(length(caption.lot)) paste("[",caption.lot,"]",sep=""),
                     "{", caption,
                     if(!longtable) paste(sl,"label{", label, "}",sep=""),
                     "}", sep="")
    table.env <- TRUE
  }

  if(ctable) {  ## 13dec02
    latex.begin <- c(if(length(size)) paste('{',sl,size,sep=''),
                     paste(sl, "ctable[", sep=''),
                     if(length(caption) && caption.loc=='bottom') 'botcap,',
                     if(length(caption))
                     paste('caption={',caption,'},',sep=''),
                     if(length(caption.lot))
                     paste('cap={',caption.lot,'},',sep=''),
                     paste('label=',label,',',sep=''),
                     if(!landscape) paste('pos=',where,',',sep=''),
                     if(landscape) 'rotate',
                     paste(']{',tabular.cols, '}',sep=''),
                     if(length(insert.bottom))
                     paste('{',sl,'tnote[]{',sedit(insert.bottom,'\\\\',' '),
                           '}}',
                           sep='') else '{}',
                     ## tnote does not allow \\ in its argument
                     paste('{', toprule, sep='')
    )
    latex.end <- c('}',if(length(size)) '}')
  } else if(!longtable) {
    latex.begin <- c(if(landscape) paste(sl, "begin{landscape}",sep=""),
                     if(table.env) paste(
                                         sl, "begin{table}",
                                         if(here)"[H]" else
                                         paste('[',where,']',sep=''),
                                         "\n", sep=""),
                     if(length(size)) paste(sl,size,'\n',sep=''),
                     if(caption.loc=='top' && !missing(caption))
                     paste(caption, "\n"),              ## 3oct03
                     if(center == 'center')             ## MJ: 08sep03
                     paste(sl,"begin{center}\n", sep="")## MJ: 08sep03
                     else {if (center == 'centering')  ## MJ: 08sep03
                     paste(sl,"centering\n", sep="")}, ## MJ: 08sep03
                     paste(sl,"begin{tabular}{", tabular.cols, "}",
                           toprule, "\n", sep="")
                                        #11Jun95   12jan03 "}" was "}{" WHY!
                     )
    latex.end <- c(
                   paste(sl,"end{tabular}\n", sep = ""),
                   if(center == 'center')  ## MJ: 08sep03
                   paste(sl,"end{center}\n", sep=""), ## MJ: 08sep03
                   if(caption.loc=='bottom' && !missing(caption))
                     paste(caption,'\n'),   # 3oct03
                   if(length(insert.bottom)) insert.bottom,
                   if(table.env) paste(sl, "end{table}\n", sep=""),
                   if(landscape) paste(sl, "end{landscape}\n", sep="")
                   )
  }
  else {
    latex.begin <- c(
                     paste(
                           if (!draft.longtable)
                           paste(sl,"let",sl,"LTmulticolumn=",sl,"multicolumn", sep=""),
                           paste(sl,"setlongtables",sep=""),
                           if(landscape) paste(sl, "begin{landscape}",sep=""),
                           if(length(size)) paste(sl,size,'\n',sep=''),
                           paste(sl,"begin{longtable}{", tabular.cols, "}",sep=""),
                           sep="\n"),
                     if(caption.loc=='top' && !missing(caption))
                      paste(caption, sl,sl,"\n", sep=""),
                     paste(toprule, "\n", sep="")    #11Jun95
                     )
    latex.end <- paste(if(caption.loc=='bottom' && !missing(caption))
                        paste(caption, sl,sl,"\n",sep=""),  ## 3oct03
                       if(length(insert.bottom)) insert.bottom,
                       paste(sl,"end{longtable}\n", sep=""),
                       if(landscape) paste(sl,"end{landscape}\n",sep=""))
  }
  
  cat(latex.begin, file=file, append=file!='')

  if(length(cgroup)) {  # was !missing 5Oct00
    cvbar <- paste(cgroup.just, vbar, sep="")
    cvbar[1] <- paste(vbar, cvbar[1], sep="")
    cvbar[-length(cvbar)] <- paste(cvbar[-length(cvbar)], vbar, sep="")
    slmc <- paste(sl,"multicolumn{",sep="")
    labs <- paste(sl, "bf ", cgroup, sep="")
    labs <- paste(slmc, n.cgroup, "}{", cvbar, "}{", labs, "}", sep="")

    cat(labs, file=file, sep="&\n", append=file!='')
    
    if (!length(cline)) {   # was is.length 2Apr02
      inr <- as.numeric(length(rowname))
      cline <- paste(sl,"cline{",1+inr,"-",nc,"}",sep="")
    } 
    cat(eol, " ",cline,"\n", sep="",file=file, append=file!='')
    ## eol was sl, sl  13dec02
  }


  { # column labels
    cvbar <- paste(collabel.just, vbar, sep="")
    cvbar[1] <- paste(vbar, cvbar[1], sep="")
    if (length(n.cgroup)) {
      vv2 <- cumsum(n.cgroup[-length(n.cgroup)])
      cvbar[vv2] <- paste(cvbar[vv2],vbar,sep="")
    }
    slmc1 <- paste(sl, "multicolumn{1}{", sep="")
#    labs <- dimnames(cx)[[2]]   ## 28apr03 and next 5  15jul03 next 2
    labs <- colheads
    if(length(labs)) {
      if(!length(extracolheads)) {
        heads <- get2rowHeads(labs)
        labs <- heads[[1]]
        if(any(heads[[2]] != '')) extracolheads <- heads[[2]]
      }
      labs <- paste(slmc1, cvbar, "}{", labs, "}", sep="")
      
      cat(labs, file=file, sep="&\n", append=file!='')

      if(length(extracolheads)) {
        extracolheads <- ifelse(extracolheads=='',extracolheads,
                                paste('{',sl,extracolsize,' ',
                                      extracolheads,'}',sep=''))
        extracolheads <- ifelse(extracolheads=='',extracolheads,
                                paste(slmc1,cvbar,'}{',
                                      extracolheads,'}',sep=''))
#      cat(eol," ", paste(c(if(length(rowname))'',extracolheads),collapse='&'),
#          file=file, append=file!='') # 21jan03
        cat(eol," ", paste(extracolheads,collapse='&'),
            file=file, append=file!='') # 28apr03
      }
      if(ctable) cat(midrule, '\n', sep='', file=file, append=file!='') else
      cat(eol," ",midrule, "\n",sep="",file=file, append=file!='')
      ## eol was sl, sl  13dec02
    }
  }


  if(longtable) {
    if(missing(caption))
      cat(sl,"endhead\n",midrule,sl,"endfoot\n",sep="",
          file=file,append=file!='')
    else {
      cat(sl,"endfirsthead\n", sep="",file=file, append=file!='')
      cat(sl,"caption[]{\\em (continued)} ",sl,sl,"\n",
          sep="",file=file, append=file!='')
      cat(midrule, "\n", sep="",file=file, append=file!='')
      cat(labs, file=file, sep="&", append=file!='')
      cat(sl,sl," ",midrule, "\n",sl,"endhead",midrule,sl,"endfoot\n",
          sep="",file=file, append=file!='')
      cat(sl,"label{", label, "}\n", sep="", file=file, append=file!='')
    }
  }

  { # individual lines, grouped if appropriate, longtable if appropriate
    if (length(n.rgroup)) {
      rg.end   <- cumsum(n.rgroup)
      rg.start <- rg.end-n.rgroup+1
      if(!length(rgroup)) rgroup <- rep("",length(n.rgroup))
      else rgroup <- paste("{",sl,"bf ",rgroup,"}",sep="")
      seq.rgroup <- seq(along=n.rgroup)
    }
    else {
      seq.rgroup <- 1
      rg.end <- nr
      rg.start <- 1
    }

    linecnt <- 0
    for (j in seq.rgroup) {
      if (length(n.rgroup)) {
        if(longtable && linecnt>0 &&
           (linecnt+n.rgroup[j]+(n.rgroup[j]>1)) > lines.page) {
          cat(sl,"newpage\n", sep="",file=file, append=file!='')
          linecnt <- 0
        }
        cat(rgroup[j], rep("",nc-1), sep="&", file=file, append=file!='')
        cat(eol,"\n", sep="",file=file, append=file!='')
        ## eol was sl,sl 13dec02
        linecnt <- linecnt+1
      }
      for(i in rg.start[j]:rg.end[j]) {
        if (!length(n.rgroup)) {
          if(longtable && linecnt>0 && (linecnt+1 > lines.page)) {
            cat(sl,"newpage\n",sep="",file=file, append=file!='')
            linecnt <- 0						
          }
        }
        cat(cx[i,], file=file, sep="&", append=file!='')
        cat(if(!ctable || i < rg.end[j]) eol,
            "\n", sep="",file=file, append=file!='')
        ## eol was sl,sl  added if( ) 13dec02
        linecnt <- linecnt+1
      }
      cat(bottomrule, "\n", sep="",file=file, append=file!='')
    }
  }

  cat(latex.end, file=file, sep="\n", append=file!='')
  sty <- c("longtable"[longtable], "here"[here], "dcolumn"[dcolumn],
           "ctable"[ctable], "booktabs"[booktabs],
           if(landscape && !ctable) "lscape")
  
  structure(list(file=file, style=sty), class='latex')
}


latex.function <- function(
	object,
	title=first.word(deparse(substitute(object))),
	file=paste(title, ".tex", sep=""),
	append=FALSE,
	assignment=TRUE,  type=c('example','verbatim'), ...)
{
  type <- match.arg(type)
  tmpfile <- tempfile()
  sink(tmpfile)
  if(assignment) cat(title , '<- ')
  print(object)
  sink()
  environment <- ifelse(type=='example', "Example", "verbatim")
  preamble <- paste("\\begin{",environment,"}\n",sep="")
  cat(preamble, file=file, append=append)
  cmd <- ifelse (type=='example',
                 "sed -e 's/\t/    /g' -e 's/\\\\/&(&backslash&)/g' -e 's/[{}]/\\\\&/g' -e 's/~/{\\\\Twiddle}/g' -e 's/\\^/{\\\\Hat}/g'\ -e 's/\\$/\\\\$/g' -e 's/@/{@}/g' -e 's/_/\\\\_/g' -e 's/<-/\\\\Gets/g' -e 's/#\\(.*$\\)/{\\\\rm\\\\scriptsize \\\\#\\1}/'",
                 "sed 's/\t/\ \ \ \ /g'"  #tab to blanks
                 )
  ## Someday need to fetch updated S.sty which defines \At
  cmd <- paste(cmd, "<", tmpfile, ">>", file)
  sys(cmd)
  unlink(tmpfile)
  postamble <- paste("\\end{",environment,"}\n", sep="")
  cat(postamble, file=file, append=file!='')

  structure(list(file=file, style=if(type=='example')'S'), class='latex')
}

latexVerbatim <- function(x,
                          title=first.word(deparse(substitute(x))),
                          file=paste(title, ".tex", sep=""),
                          append=FALSE, size=NULL, hspace=NULL,
                          width=.Options$width, length=.Options$length, ...) {

  if(!missing(width) || !missing(length)) {
    old <- options(width=width, length=length)
    on.exit(options(old))
  }
  sink(file, append=append)
  cat('\\setbox0=\\vbox{\n',if(length(size))c('\\',size,'\n'),
      '\\begin{verbatim}\n', sep='')
  print(x, ...)
  cat('\\end{verbatim}\n}\n',if(length(hspace))c('\\hspace{',hspace,'}'),
      '{\\makebox[\\textwidth]{\\box0}}\n', sep='')
  sink()
 
  structure(list(file=file, style=NULL), class='latex')
}

latex.list <- function( object,
	title=first.word(deparse(substitute(object))),
                       file=paste(title, ".tex", sep=""), append=FALSE,
                       label,
                       caption, caption.lot,
                       caption.loc=c('top','bottom'),
                       ...) {
  caption.loc <- match.arg(caption.loc)
  nx <-	names(object)
  if (!length(nx)) nx <- paste(title, "[[",
	seq(along=object), "]]", sep="")
  tmp <- latex(object=object[[1]],
	caption=nx[1], label=nx[1], append=append, title=title, file=file,
	caption.lot=NULL, caption.loc=caption.loc, ...)
  tmp.sty <- tmp$style
  for (i in	seq(along=object)[-1]) {
    tmp <- latex(object=object[[i]],
                 caption=nx[i], label=nx[i], append=file!='', title=title, file=file,
                 caption.lot=NULL, caption.loc=caption.loc, ...)
    tmp.sty <- c(tmp.sty, tmp$style)
  }
  sty <- if(length(tmp.sty)) unique(tmp.sty) else NULL
  structure(list(file=file, style=sty), class='latex')
}

## Function to translate several expressions to LaTeX form, many of
## which require to be put in math mode.
## Arguments inn and out specify additional input and translated
## strings over the usual defaults.
## If pb=T, also translates [()] to math mode using \left, \right
## Assumes that input text always has matches, e.g. [) [] (] (), and
## that surrounding  by $$ is OK
## latexTranslate is used primarily by summary.formula

latexTranslate <- function(object, inn=NULL, out=NULL, pb=FALSE, ...) {

  text <- object
  
  inn <- c("|",  "%",  "<=",     "<",  ">=",     ">",  "_",
         inn, 
         if(pb) c("[","(","]",")"))

out <- c("$|$","\\%","$\\leq$","$<$","$\\geq$","$>$","\\_",
         out, 
         if(pb) c("$\\left[","$\\left(","\\right]$","\\right)$"))

text <- sedit(text, '$', 'DOLLARS', wild.literal=TRUE)   ##17Nov00
text <- sedit(text, inn, out)

  ##See if string contains an ^ - superscript followed by a number
  ## (number condition added 31aug02)

  dig <- c('0','1','2','3','4','5','6','7','8','9')

for(i in 1:length(text)) {
  lt <- nchar(text[i])
  x <- substring(text[i],1:lt,1:lt)
  j <- x=='^'
  if(any(j)) {
    is <- ((1:lt)[j])[1]  #get first ^
    remain <- x[-(1:is)]
    k <- remain %in% c(' ',',',')',']','\\','$')
    ## Following 3 lines 31aug02
    if(remain[1] %in% dig ||
       (length(remain) > 1 && remain[1]=='-' && remain[2] %in% dig))
       k[-1] <- k[-1] | remain[-1] %nin% dig
    ie <- if(any(k)) is + ((1:length(remain))[k])[1] else length(x)+1
#See if math mode already turned on (odd number of $ to left of ^)
    dol <- if(sum(x[1:is]=='$') %% 2) '' else '$'
    substring2(text[i],is,ie-1) <- paste(dol,'^{',
                                        substring(text[i],is+1,ie-1),'}',
       dol,sep='')  # 25May01
  }
}
sedit(text, 'DOLLARS', '\\$', wild.literal=TRUE)  ## 17Nov00
}


latex <- function(object,
                  title=first.word(deparse(substitute(object))),...)
  {
    ## added title= 25May01
    if (!length(oldClass(object))) oldClass(object) <- data.class(object)
    UseMethod("latex")
  }


optionsCmds <- function(pgm) {
  optionName <- paste(pgm,'cmd',sep='')
  v <- .Options[[optionName]]
  if(pgm=='xdvi' && !under.unix && !length(v))
    v <- 'yap'  # MikTeX  7Feb03
  if(length(v) && v!='') pgm <- v
  pgm
}

dvi.latex <- function(object, prlog=FALSE,
                      nomargins=TRUE, width=5.5, height=7, ...) {
  fi <- object$file; sty <- object$style

  if(length(sty))sty <- paste('\\usepackage{',sty,'}',sep='')
  if(nomargins) sty <-  c(sty,
                          paste('\\usepackage[paperwidth=',width,
                                'in,paperheight=', height,
                                'in,noheadfoot,margin=0in]{geometry}',sep=''))
  pre <- tempfile(); post <- tempfile(); tmp <- tempfile()
  cat('\\documentclass{report}',sty,'\\begin{document}',file=pre,sep='\n')
  cat('\\end{document}',file=post)
  if(under.unix)
    sys(paste('cat',pre,fi,post,'>',paste(tmp,'tex',sep='.')))
  else sys(paste('copy',pre,'+',fi,'+',post,paste(tmp,'tex',sep='.')))
  ## 17dec02
  unlink(c(pre,post))
  sc <- if(under.unix)';' else '&'   # DOS command separator #  7feb03
  sys(paste('cd',tempdir(),sc,optionsCmds('latex'),tmp))
  if(prlog) cat(scan(paste(tmp,'log',sep='.'),list(''),sep='\n')[[1]],
                sep='\n')
  fi <- paste(tmp,'dvi',sep='.')
  structure(list(file=fi), class='dvi')
}

if(.R. && FALSE) show <- function(object) UseMethod('show')

show.dvi <- function(object, width=5.5, height=7) {
  viewer <- optionsCmds('xdvi')
  cmd <- if(viewer=='yap') paste(viewer,object$file) else
  paste(viewer, ' -paper ',
        width,'x',height,'in -s 0 ',
        object$file,' &',sep='')
  sys(cmd)
  invisible()
}

## enhanced show.latex 22dec02 - special treatment of file==''
show.latex <- function(object) {
  if(object$file=='') {
    if(length(object$style)) {
      latexStyles <- if(exists('latexStyles'))
        unique(c(latexStyles, object$style)) else object$style
      storeTemp(latexStyles,'latexStyles')
    }
    return(invisible())
  }
  show.dvi(dvi.latex(object))
}

print.dvi <- function(x, ...) show.dvi(x)
print.latex <- function(x, ...) show.latex(x)
  
dvi         <- function(object, ...) UseMethod('dvi')
dvips       <- function(object, ...) UseMethod('dvips')
dvigv       <- function(object, ...) UseMethod('dvigv')
dvips.dvi   <- function(object, file, ...) {
  cmd <- if(missing(file))
  paste(optionsCmds('dvips'),'-f', object$file,' | lpr') else
  paste(optionsCmds('dvips'),'-o', file, object$file)
  invisible(sys(cmd))
}
dvigv.dvi   <- function(object, ...)
  invisible(sys(paste(optionsCmds('dvips'),'-f',object$file,
                      '| gv - &')))
dvips.latex <- function(object, ...) invisible(dvips.dvi(dvi.latex(object)))
dvigv.latex <- function(object, ...) invisible(dvigv.dvi(dvi.latex(object)))

html <- function(object, ...) UseMethod('html')
                 
html.latex <- function(object, ...) {
  fi  <- object$file
  sty <- object$style
  
  if(length(sty))sty <- paste('\\usepackage{',sty,'}',sep='')
  pre <- tempfile(); post <- tempfile(); tmp <- tempfile()
  cat('\\documentclass{report}',sty,'\\begin{document}',file=pre,sep='\n')
  cat('\\end{document}',file=post)
  if(under.unix)
    sys(paste('cat',pre,fi,post,'>',paste(tmp,'tex',sep='.')))
  else sys(paste('copy',pre,'+',fi,'+',post,paste(tmp,'tex',sep='.')))
  ## 17dec02
  unlink(c(pre,post))
  sc <- if(under.unix)';' else '&'  # 7feb03
  sys(paste('cd ',tempdir(),sc,' hevea ',tmp,'.tex',sep=''))
  fi <- paste(tmp,'html',sep='.')
  structure(list(file=fi), class='html')
}

html.data.frame <- function(object,
                            file=paste(first.word(deparse(substitute(object))),
                                       'html',sep='.'),
                            append=FALSE,
                            link=NULL, linkCol=1, linkType=c('href','name'),
                            ...) {

  linkType <- match.arg(linkType)
  
  x   <- format.df(object, ...)
  adj <- attr(x,'col.just')

  if(any(adj=='r')) for(i in seq(along=adj)[adj=='r'])
    x[,i] <- paste('<div align=right>',x[,i],'</div>',sep='')

  if(length(r <- dimnames(x)[[1]])) x <- cbind('Name'=r, x)
  cat('<TABLE BORDER>\n', file=file, append=append)
  cat('<tr>', paste('<td>', dimnames(x)[[2]], '</td>',sep=''), '</tr>\n',
      sep='', file=file, append=file!='')
  if(length(link)) x[,linkCol] <-
    ifelse(link=='',x[,linkCol],
           paste('<a ',linkType,'="',link,'">',x[,linkCol],'</a>',sep=''))

  for(i in 1:nrow(x))
    cat('<tr>',paste('<td>',x[i,],'</td>',sep=''),'</tr>\n',
        sep='', file=file, append=file!='')

  cat('</TABLE>\n', file=file, append=file!='')
  structure(list(file=file), class='html')
}

html.default <- function(object,
                         file=paste(first.word(deparse(substitute(object))),
                           'html',sep='.'),
                         append=FALSE,
                         link=NULL, linkCol=1, linkType=c('href','name'),
                         ...)
html.data.frame(object, file=file, append=append, link=link,
                linkCol=linkCol, linkType=linkType, ...)

show.html <- function(object) {
  browser <- .Options$help.browser
  if(!length(browser)) browser <- .Options$browser
  if(!length(browser)) browser <- 'netscape'
  sys(paste(browser, object, if(under.unix) '&'))
  invisible()
}

print.html <- function(x, ...) show.html(x)

latexSN <- function(x) {
  x <- format(x)
  x <- sedit(x, c('e+00','e-0*',                'e-*',
                    'e+0*',               'e+*'),
                  c('',    '\\\!\\times\\\!10^{-*}','\\\!\\times\\\!10^{-*}',
                    '\\\!\\times\\\!10^{*}','\\\!\\times\\\!10^{*}'))
  x
}

ldBands <- function(n=length(times), times=NULL,  alpha=.05,
                    sided=2, alphaLower=alpha/2, alphaUpper=alpha/2,
                    information=NULL,
                    spending=c('OBrien-Fleming','Pocock','alpha*t^phi',
                      'Hwang-Shih-DeCani'),
                    phi=1,
                    spending2=c('OBrien-Fleming','Pocock','alpha*t^phi',
                      'Hwang-Shih-DeCani'),
                    phi2=phi,
                    truncate=Inf, power=NULL, pr=TRUE) {

  if(missing(n) && missing(times))
    stop('must specify n or times')
  if(!length(times)) times <- seq(0,1,length=n+1)[-1]
  spending  <- match.arg(spending)
  spending2 <- if(missing(spending2)) spending else match.arg(spending2)
  alpha <- alphaLower+alphaUpper
  if(length(power) && length(information))
    stop('information may not be specified when power is')
  sp <- c('OBrien-Fleming'=1,'Pocock'=2,'alpha*t^phi'=3,
          'Hwang-Shih-DeCani'=4)[spending]
  if(sided != 3) {spending2 <- spending; sp2 <- sp} else
  sp2 <- c('OBrien-Fleming'=1,'Pocock'=2,'alpha*t^phi'=3,
           'Hwang-Shih-DeCani'=4)[spending2]

  if(phi==0) {
    warning('phi may not be zero.  Set to 1')
    phi <- 1
  }
  if(length(times))       times       <- sort(times)
  if(length(information)) information <- sort(information)

  fi <- tempfile()
  ## Note: times always has length>0 below
  ## When power is given, assumes spending function always determines
  ## bounds
  p <- if(under.unix) function(x) paste(x,'\\n',sep='',collapse='') else
                      function(x) paste(x,'\n', sep='',collapse='')
  ## If running Linux/Unix can avoid creating an input file, just pipe
  ## echo output as stdin.  echo needs embedded '\n' hence output \\n
    
  w <- paste(if(under.unix)'echo -e "' else '',
             p(0),
             p(if(length(power)) 2 else 1),
             p(n),
             p(if(length(times)) c(0,paste(times,collapse=' ')) else 1),
             p(if(length(power))1 else
               if(length(information))
                 c(1,paste(information,collapse=' ')) else 0),
             p(alpha), p(sided),
             if(sided==3)p(alphaLower) else '',
             p(sp),
             if(sp %in% 3:4) p(phi) else '',
             if(sided==3) p(c(sp2,if(sp2 %in% 3:4)phi2 else NULL)) else '',
             p(if(is.infinite(truncate)) 0 else c(1,truncate)),
             if(length(power)) p(power) else '',
             p(0),p(0),
             if(under.unix)'"' else '',
             sep='')

  if(under.unix) sys(paste(w,'| ld98 >',fi)) else {
    fin <- tempfile()
    cat(w, file=fin)
    sys(paste('ld98 <',fin,'>',fi))
    unlink(fin)
  }
  w <- if(.R.) scan(fi, what=list(z=''),sep='\n',quiet=TRUE)$z else
               scan(fi, what=list(z=''),sep='\n')$z
  if(pr) cat(w,sep='\n')
  unlink(fi)
  if(length(power)) {
    i <- grep('drift =',w)
    j <- substring.location(w[i], 'drift =')$last
    drift <- as.numeric(substring(w[i],j+1))
  } else drift <- NULL
  
  head <- grep(if(length(power))'cum exit pr' else 'cum alpha',w)
  w <- w[(head+1):length(w)]
  tail <- grep(if(length(power))'Would you like to start again'
                           else 'Do you want to see a graph',w)
  w <- w[1:(tail-1)]
  z <- if(.R.) unPaste(w, ' +', extended=TRUE) else
               unPaste(sedit(w,'  ',' '),' ')

  if(length(power)) {
    i <- 1   ## 19dec02
    tim        <- as.numeric(z[[i+2]])
    if(max(abs(tim-times)) > .01) stop('program logic error')
    low       <- as.numeric(z[[i+3]])
    hi        <- as.numeric(z[[i+4]])
    exit.prob <- as.numeric(z[[i+5]])
    cum.exit.prob <- as.numeric(z[[i+6]])
    data <- data.frame(time=times, lower=low,upper=hi,
                       exit.prob=exit.prob,cum.exit.prob=cum.exit.prob)
  } else {
    tim       <- as.numeric(z[[2]])
    if(max(abs(tim-times)) > .01) stop('program logic error')
    
    i <- if(length(information))1 else 0
    low       <- as.numeric(z[[3+i]])
    hi        <- as.numeric(z[[4+i]])
    alpha.inc <- as.numeric(z[[5+i]])
    cum.alpha <- as.numeric(z[[6+i]])
    data <- data.frame(time=times, lower=low,upper=hi,
                       alpha.inc=alpha.inc,cum.alpha=cum.alpha)
  }
  if(length(information)) data$information <- information
  
  res <- structure(list(data=data, power=power, drift=drift,
                        type=if(length(power))'power' else 'boundaries',
                        n=n, alpha=alpha, alphaLower=alphaLower,
                        alphaUpper=alphaUpper, sided=sided,
                        spending=spending, phi=phi,
                        spending2=spending2, phi2=phi2,
                        truncate=truncate),
                   class='ldBands')
  res
}

print.ldBands <- function(x, ...) {
  if(x$sided < 3) {
    cat('alpha=',format(x$alpha),'\t',x$sided,
        '-sided  \tSpending function:',x$spending,sep='')
    if(x$spending=='alpha*t^phi') cat('\tExponent:',x$phi,sep='')
    if(x$spending=='Hwang-Shih-DeCani') cat('\tPhi:',x$phi,sep='')
  } else {
    cat('Lower bounds:\n\n')
     cat('alpha=',format(x$alphaLower),
         '\tSpending function:',x$spending,sep='')
    if(x$spending=='alpha*t^phi') cat('\tExponent:',x$phi,sep='')
    if(x$spending=='Hwang-Shih-DeCani') cat('\tPhi:',x$phi,sep='')
    cat('\n\nUpper bounds:\n\n')
    cat('alpha=',format(x$alphaUpper),
        '\tSpending function:',x$spending2,sep='')
    if(x$spending2=='alpha*t^phi') cat('\tExponent:',x$phi2,sep='')
    if(x$spending2=='Hwang-Shih-DeCani') cat('\tPhi:',x$phi2,sep='')
  }
  cat('\n\n')
  if(length(x$power)) cat('Power:',x$power,'\tDrift:',x$drift,'\n\n')
  print(x$data)
  invisible()
}

plot.ldBands <- function(x, xlab='Time', ylab='Z', actual=NULL,
                         type='b', labels=NULL, ...) {
  d <- x$data
  mfr <- par('mfrow')
  if(prod(mfr) != 1) {
    on.exit(par(mfrow=mfr))
    par(mfrow=c(2,1))
  }
  plot(d$time, d$lower, type=type, ylim=range(d$lower,d$upper),
       xlab=xlab, ylab=ylab, axes=length(labels)==0)
  if(length(labels)) {
    axis(2)
    if(length(labels) != length(d$time))
      stop('length of labels not equal to length of times generated by ldBands')
    axis(1, at=d$time, labels=labels)
  }
  lines(d$time, d$upper, type=type)
  if(length(actual)) points(actual[[1]],actual[[2]], pch=16)
  if(x$type=='power')
    labcurve(list(Instant   =list(d$time,d$exit.prob),
                  Cumulative=list(d$time,d$cum.exit.prob)),
             lty=2:1, pl=TRUE, type=type,
             xlab=xlab, ylab='Exit Probability')
  invisible()
}

summary.ldBands <- function(object, stdiff=NULL, n=NULL,
                            p1=NULL, p2=NULL,
                            hr=NULL, events=NULL,
                            pbar=NULL, sd=NULL, ...) {
  
  if(length(pbar) + length(sd) == 0) {
    drift <- object$drift
    if(!length(drift)) stop('did not specify power= to ldBands')

    if(length(p1)) stdiff <- (p1-p2)/sqrt(p1*(1-p1)+p2*(1-p2))
    if(length(events)) hr <- exp(2*drift/sqrt(events))
    if(length(hr)) events <- 4*((drift/log(hr))^2)
  
    if(length(stdiff)+length(n)+length(events)==0)
      stop('must specify stdiff, n, hr, or events')

    if(length(stdiff)) n <- (drift/stdiff)^2 else
    if(length(n)) stdiff <- drift/sqrt(n)
    structure(list(stdiff=stdiff, n=n, p1=p1, p2=p2, hr=hr, events=events,
                   drift=drift, power=object$power),
              class='summary.ldBands')
  } else {
    if(length(n) != nrow(object$data))
      stop('length of n must equal number of looks')
    d <- object$data
    d$n <- n
    if(length(pbar)) {
      sepdiff      <- sqrt(2*pbar*(1-pbar)/n)
      d$diff.lower <- d$lower*sepdiff
      d$diff.upper <- d$upper*sepdiff
      selogOR      <- sqrt(2/(pbar*(1-pbar)*n))
      d$or.lower   <- exp(d$lower*selogOR)
      d$or.upper   <- exp(d$upper*selogOR)
      object$data     <- d
      object
    } else {
      semeandiff   <- sd*sqrt(2/n)
      d$diff.lower <- d$lower*semeandiff
      d$diff.upper <- d$upper*semeandiff
      object$data     <- d
      object
    }
  }
}

print.summary.ldBands <- function(x, ...) {
  cat('Drift:',x$drift,'\tPower:',x$power,sep='')
  if(length(x$p1))     cat('\tp1:',x$p1,'\tp2:',x$p2,sep='')
  cat('\n\n')
  if(length(x$n))      cat('Maximum sample size per treatment:',
                           x$n,'\n',sep='')
  if(length(x$events)) cat('Maximum number of events per treatment:',
                           x$events,'\n',sep='')
  if(length(x$stdiff)) cat('Detectible standardized effect:\t',
                           x$stdiff,'\n',sep='')
  if(length(x$hr))     cat('Hazard ratio:\t',x$hr,'\n',sep='')
  invisible()
}

list.tree <- function(struct,depth=-1,numbers=FALSE,maxlen=22,
	maxcomp=12,attr.print=TRUE,front="",fill=". ",name.of,size=TRUE)
{ 
if(depth==0) return()
opts <- options(digits=5)
on.exit(options(opts))
if (missing(name.of)) name.of <- deparse(substitute(struct))
len <- length(struct)
cat(front,name.of,"=",storage.mode(struct),len)
if(size) cat(" (",object.size(struct)," bytes)",sep="")
if(is.array(struct)) cat("=",
		if(length(dimnames(struct)))"named", 
		"array",paste(dim(struct),collapse=" X ")) 
if(is.ts(struct)) cat("= time series",tsp(struct)) 
if(is.category(struct)) 
	cat("= category (",length(levels(struct))," levels)",sep="")
if(length(attr(struct,'class'))>0) cat("(",attr(struct,'class'),")")
if(is.atomic(struct) && !is.character(struct)&& len>0 && maxlen>0) {
	field <- "="
	for(i in 1:length(struct)) {
	    field <- paste(field,format(as.vector(struct[i])))
	    if(nchar(field)>maxlen-6) {field <- paste(field,"..."); break}
	    }
	cat(field,"\n",sep="")
	} else
	if(is.character(struct) && len>0 && maxlen>0) 
	    cat("=",substring(struct[1:(last <- max(1,(1:len)
	    [cumsum(nchar(struct)+1)<maxlen]))],1,maxlen),
	    if(last<len)" ...","\n") else cat("\n")
if (mode(struct)=="list" && len>0) {
	structnames <- names(struct)
	if(!length(structnames)) structnames <- rep("",len)
	noname <- structnames==""
	structnames[noname] <- 
		paste("[[",(1:length(structnames))[noname],"]]",sep="")
	for (i in 1:min(length(structnames),maxcomp)) 
	if (mode(struct[[i]])=="argument" | mode(struct[[i]])=="unknown") 
	   cat(front,fill," ",structnames[i]," = ",
		as.character(struct[[i]])[1],"\n",sep="") else 
	   list.tree(struct[[i]],depth=depth-1,numbers,maxlen,maxcomp,
		attr.print,
		if(numbers)paste(front,i,sep=".") else paste(front,fill,sep=""),
		fill,structnames[i],size=FALSE)
	if(length(structnames)>maxcomp) 
		cat(front,fill," ...   and ",length(structnames)-maxcomp,
		" more\n",sep="")
	} 
attribs <- attributes(struct)
attribnames <- names(attribs)
if(length(attribnames)>0 && attr.print) for (i in (1:length(attribnames))
		[attribnames!="dim" & attribnames!="dimnames" & 
		attribnames!="levels" & attribnames!="class" &
		attribnames!="tsp" & 
		(attribnames!="names" | mode(struct)!="list")])
	list.tree(attribs[[i]],depth-1,numbers,maxlen,maxcomp,attr.print,
		if(numbers)paste(front,i,sep="A") else paste(front,"A ",sep=""),
		fill,attribnames[i],size=FALSE)
invisible()
}

##############################################################################
expr.tree <- function(struct,front="",fill=". ",name.of,numbers=FALSE,depth=-1,
	show.comment=FALSE)
{ 
if (missing(name.of)) name.of <- deparse(substitute(struct)) else
	 if(is.atomic(struct) | is.name(struct))
		name.of <- paste(name.of,deparse(struct)) 
cat(front,"",name.of,"=",mode(struct),length(struct),"\n")
if(depth!=0 && is.recursive(struct) ) {
	structlength <- length(struct)
	structnames <- names(struct)
	if(length(structnames)==0) structnames <- rep("",structlength)
	if(structlength>0)
	for (i in 1:length(structnames)) {
	  if((mode(struct[[i]])!="missing" || is.function(struct)) &&
	    (mode(struct[[i]])!="comment" || show.comment))
	      expr.tree(struct[[i]],
		if(numbers)paste(front,i,sep=".") else paste(front,fill,sep=""),
		fill,structnames[i],numbers,"depth"=depth-1)
          }
	}
invisible(character(0))
} 
mask<- function(a) {
  ##determine which bits are on in a vector of status bytes
  if(a>=.Machine$integer.max)stop("Value > integer.max")
  a <- as.integer(a) 
  as.logical((rep(a, 8)%/%rep(2^(0:7), rep(length(a),8)))%%2)
}

#  Rick Becker
#  Improved by Peter Melewski 14Apr02

#Multiply matrix by a vector
#vector can be same length as # columns in a, or can be longer,
#in which case b[kint] is added to a * b[s:length(b)], s=length(b)-ncol(a)+1
#F. Harrell 17 Oct90
#Mod         5 Jul91 - is.vector -> !is.matrix
#           16 Oct91 - as.matrix -> matrix(,nrow=1)
#	    29 Oct91 - allow b to be arbitrarily longer than ncol(a), use b(1)
#	    13 Nov91 - matrix(,nrow=1) -> matrix(,ncol=1)
#	    14 Nov91 - changed to nrow=1 if length(b)>1, ncol=1 otherwise
#	    25 Mar93 - changed to use %*%
#           13 Sep93 - added kint parameter

matxv <- function(a,b,kint=1) {

if(!is.matrix(a)) {
   if(length(b)==1) a <- matrix(a, ncol=1)
   else a <- matrix(a, nrow=1)	
}

nc <- dim(a)[2]
lb <- length(b)
if(lb<nc)
	stop(paste("columns in a (",nc,") must be <= length of b (",
		length(b),")",sep=""))

if(nc==lb) drop(a %*% b) else
drop(b[kint] + (a %*% b[(lb-nc+1):lb]))
}

#storage.mode(a) <- "single"
#storage.mode(b) <- "double"
#
#library.dynam(section="local", file="matxv.o")
#
#.Fortran("matxv",a,b,d[1],d[2],length(b),c=single(d[1]), NAOK=T,
#	specialsok=T)$c


if(!.R.) mem <- function() {
  cat("Memory used:  Current=",memory.size(),
      " Maximum=",memory.size(TRUE),"\n")
  invisible()
}
minor.tick <- function(nx=2, ny=2, tick.ratio=.5) {

ax <- function(w, n, tick.ratio) {

range <- par("usr")[if(w=="x") 1:2 else 3:4]
tick.pos <- if(w=="x") par("xaxp") else par("yaxp")

#Solve for first and last minor tick mark positions that are on the graph

distance.between.minor <- (tick.pos[2]-tick.pos[1])/tick.pos[3]/n
possible.minors <- tick.pos[1]-(0:100)*distance.between.minor  #1:100 13may02
low.minor <- min(possible.minors[possible.minors>=range[1]])
if(is.na(low.minor)) low.minor <- tick.pos[1]
possible.minors <- tick.pos[2]+(0:100)*distance.between.minor  #1:100 13may02
hi.minor <- max(possible.minors[possible.minors<=range[2]])
if(is.na(hi.minor)) hi.minor <- tick.pos[2]

if(.R.) axis(if(w=="x") 1 else 2,
             seq(low.minor,hi.minor,by=distance.between.minor),
             labels=FALSE, tcl=par('tcl')*tick.ratio) else
axis(if(w=="x") 1 else 2, seq(low.minor,hi.minor,by=distance.between.minor),
	labels=FALSE, tck=par('tck')*tick.ratio)
}

if(nx>1) ax("x", nx, tick.ratio=tick.ratio)
if(ny>1) ax("y", ny, tick.ratio=tick.ratio)

invisible()
}

#Thanks for Rick Becker for suggestions
mtitle <-
  function(main,ll,lc,
           lr=if(.R.) format(Sys.time(),'%d%b%y') else
           if(under.unix)unix("date '+%d%h%y'") else date(), 
           cex.m=1.75, cex.l=.5, ...) {
    out <- any(par()$oma!=0)
    g <- if(out) function(...) mtext(..., outer=TRUE) else 
    function(z, adj, cex, side, ...) 
      if(missing(side)) title(z, adj=adj, cex=cex) else
	title(sub=z, adj=adj, cex=cex)
    if(!missing(main))g(main,cex=cex.m,adj=.5)
    if(!missing(lc)) g(lc,side=1,adj=.5,cex=cex.l,...)
    if(!missing(ll)) g(ll,side=1,adj=0,cex=cex.l,...)
    if(lr!="") g(lr,side=1,adj=1,cex=cex.l,...)
    invisible()
  }

if(!.R.) {
  mulbar.chart<-function(z, x, y, fun = mean, marginals=TRUE, subset, prt=TRUE,
	zlab = label(z), xlab=label(x), ylab=if(!missing(y))label(y), 
	varwidth=TRUE, overall, ...)
{
xl<-xlab
yl<-ylab
zl<-zlab
if(!missing(subset))	{
	x <- x[subset]
	if(!missing(y)) y <- y[subset]
	z <- z[subset]	}
x<-as.category(x)
count <- function(ww) sum(!is.na(ww))
oldpar <- par(mar=c(7,4,3,2)+.1)
if(marginals)ntext <- "n="
else ntext <- "Maximum n="
if(missing(y)){
	tabln <- tapply(z, list(x), count)
	tabl <- tapply(z, list(x), fun)
	nmin <- min(tabln)
	nmax <- max(tabln)
	cx <- category(row(tabl), label=levels(x))
	if(marginals) {
		tabln <- c(tabln, 1)
		tabl  <- c(tabl, if(missing(overall)) fun(z) else overall)
		levels(cx) <- c(levels(cx),"All")	}
	names(tabl) <- levels(cx)
	names(tabln) <- levels(cx)
	if(varwidth) barplot(tabl, tabln, names=levels(cx), xlab=xl, main=zl)
	else barplot(tabl, names=levels(cx), xlab=xl, main=zl)
	mtext(paste("n=",count(z)," (",nmin,"-",nmax,")",sep=""),
		side=1,line=5,adj=0)
	if(varwidth)mtext("Width proportional to sample size",side=1,line=6,adj=0)
			}
else {
	y<-as.category(y)
	tabl <- tapply(z, list(y,x), fun)
	tabln <- tapply(z, list(y,x), count)
	nmin <- min(tabln)
	cy <- category(row(tabl), label = levels(y))
	cx <- category(col(tabl), label = levels(x))
	if(marginals) {
		tabl <- cbind(tabl, tapply(z, list(y), fun))
		tabl <- rbind(tabl, c(tapply(z, list(x), fun), 
			if(missing(overall)) fun(z) else overall))
		tabln <- cbind(tabln, tapply(z, list(y), count))
		tabln <- rbind(tabln,c(tapply(z, list(x), count), 1))
		levels(cx) <- c(levels(cx),"All")
		levels(cy) <- c(levels(cy),"All")	}
		dimnames(tabl) <- list(levels(cy),levels(cx))
		dimnames(tabln) <- list(levels(cy),levels(cx))
	if(varwidth)
	mulbar(tabln, tabl, collab=levels(cx), rowlab = levels(cy), 
		main=zl, ylab=yl, ...)	else
	mulbar(1+0*tabl, tabl, collab=levels(cx), rowlab=levels(cy), main=zl,
		ylab=yl, ...)
	mtext(xl,side=1,line=3)
	if(varwidth)
	mtext("Width proportional to sample size",side=1,line=6,adj=0)
	mtext(paste("n=",count(z)," (",nmin,"-",max(tabln),")",
		"   Height=",signif(as.single(min(tabl)),5),
		"-",signif(as.single(max(tabl)),5),sep=""),
		side=1,line=5,adj=0)
				}
par(oldpar)
if(prt)	{
	print(zl,quote=FALSE)
	print(tabl,digits=4)
	print("------- n -------",quote=FALSE)
	print(tabln)	}
invisible()
}
NULL
}

#Enhancement of na.omit  F. Harrell 20 Oct 91
#Allows an element of the data frame to be another data frame
#Note: S does not invoke na.action if only a data frame variable is missing!

na.delete <- function(frame)						{

	y.detail <- na.detail.response(frame)
	n <- length(frame)
	omit <- FALSE
	vars <- seq(length = n)
	nmiss <- rep(0,n)
	storage.mode(nmiss) <- "integer"
	for(j in vars)                                  {
		x <- frame[[j]]
		if(is.data.frame(x)) x <- as.matrix(x)
		oldClass(x) <- NULL	#so Surv object is.na ignored
		if(!is.atomic(x)) 
		   stop("non-atomic, non-data frame variables not allowed")
	# variables are assumed to be either some sort of matrix, numeric or cat'y
		isna <- is.na(x)	#Change from T. Therneau
		d <- dim(x)
		if(is.null(d) || length(d) != 2)	{
	#		isna <- is.na(x)
			nmiss[j] <- sum(isna)
			omit <- omit | isna		}
		else {
	#		isna <-is.na(x %*% rep(0,d[2]))
			isna <- (isna %*% rep(1,d[2])) > 0
			nmiss[j] <- sum(isna)
			omit <- omit | isna
		     }
	                                                }
	if(any(omit))				{
		rn <- row.names(frame)

		frame <- frame[!omit,,drop=FALSE]
		names(nmiss) <- names(frame)
	# a %ia% b terms are included - delete them since main effects
	# already counted  (next 2 stmts reinstated 27Oct93)

		i <- grep("%ia%", names(nmiss))
		if(length(i)>0) nmiss <- nmiss[-i]
		attr(frame,"nmiss") <- nmiss    # for backward compatibility
		temp <- seq(omit)[omit]
		names(temp) <- rn[omit]
		na.info <- list(nmiss=nmiss, omit=temp, 
				na.detail.response=y.detail)
		oldClass(na.info) <- "delete"
		attr(frame, "na.action") <- na.info
						}
	frame
									}

naprint.delete <- function(x, ...) {
  if(length(g <- x$nmiss))	{
    cat("Frequencies of Missing Values Due to Each Variable\n")
    print(g)
    cat("\n")            	}
   if(length(g <- x$na.detail.response))	{
     cat("\nStatistics on Response by Missing/Non-Missing Status of Predictors\n\n")
     print(oldUnclass(g))
     cat("\n")		
   }
  invisible()
}
   

naresid.delete <- function(omit, x, ...) {
   omit <- omit$omit
   ## 28Oct99:
   if(exists('naresid.omit')) naresid.omit(omit, x) else
   naresid.exclude(omit, x)
 }


nafitted.delete <- function(obj, x) {
  omit <- obj$omit
  if(exists('naresid.omit')) naresid.omit(omit, x) else
  naresid.exclude(omit, x)
}

  
  
na.detail.response <- function(mf)					{
     if(is.null(z <- .Options$na.detail.response) || !z) return(NULL)
     response <- model.extract(mf, response)
     if(is.null(response)) return(NULL)
     if(!is.matrix(response)) response <- as.matrix(response)
     GFUN <- options()$na.fun.response
     if(is.null(GFUN)) GFUN <-  function(x, ...)	{
        if(is.matrix(x)) x <- x[,ncol(x)]
	x <- x[!is.na(x)]
	c(N=length(x),Mean=mean(x))
							}
     else GFUN <- eval(as.name(GFUN), local=FALSE)
     w <- NULL; nam <- names(mf); wnam <- NULL
     N <- nrow(mf)
     p <- ncol(mf)
     omit <- rep(FALSE, N)
     for(i in 2:p)		{
        x <- mf[,i]
        if(is.matrix(x)) x <- x[,1]
        isna <- is.na(x)
	omit <- omit | isna
        nmiss <- sum(isna)
        if(nmiss)	{
          w <- cbind(w, GFUN(response[isna,]))
	  wnam <- c(wnam, paste(nam[i],"=NA",sep=""))
			}
        n <- N-nmiss
        if(n)		{
	  w <- cbind(w, GFUN(response[!isna,]))
	  wnam <- c(wnam, paste(nam[i],"!=NA",sep=""))
			}
				}
     if(p>2)	{   # summarize response for ANY x missing
	nmiss <- sum(omit)
	if(nmiss)	{
	  w <- cbind(w, GFUN(response[omit,]))
	  wnam <- c(wnam, "Any NA")
			}
	if(N-nmiss)	{
	  w <- cbind(w, GFUN(response[!omit,]))
	  wnam <- c(wnam, "No NA")
			}
		}

     dimnames(w)[[2]] <- wnam

     w
						}
na.keep <- function(mf) {
  w <- na.detail.response(mf)
  if(length(w)) oldClass(w) <- 'keep'  ## 9Apr02
  attr(mf, "na.action") <- w
  mf
}




naprint.keep <- function(x, ...) {
  if(length(x)) {
    cat("\nStatistics on Response by Missing/Non-Missing Status of Predictors\n\n")
    print(oldUnclass(x))
    cat("\n")
  }
  invisible()
}


naresid.keep <- function(omit, x, ...) x
na.pattern<-function(x)
{
        if(is.list(x)) {
                k <- length(x)
                n <- length(x[[1]])
                x <- matrix(unlist(x), n, k)
        }
        n <- dim(x)[1]
        k <- dim(x)[2]
        y <- matrix(as.integer(is.na(x)), n, k)
        pattern <- y[, 1]
        for(i in 2:k) {
                pattern <- paste(pattern, y[, i], sep = "")
        }
        table(pattern)
}



#Werner, Martin and Tim have added several useful
#things. At the end of this e-mail there is our final result.
#
#As an example we reproduced a similar figure as Fig. 4.23 of Chambers et
#al. (1983) "Graphical Methods For Data Analysis":
#
#ii_3:4
#x <- matrix(aperm(iris[,ii,], perm =c(1,3,2)), ncol=2,
#	    dimnames=list(dimnames(iris)[[1]],dimnames(iris)[[2]][ii]))
#xr <- round(2*x,1)/2
#nam <- dimnames(xr)[[2]]
#p.sunflowers(xr[,1],xr[,2], xlab=nam[1], ylab=nam[2], size= 1/16,
#	     main="Iris data")
#
#
#Andreas Ruckstuhl <ruckstuhl@stat.math.ethz.ch>			
#Seminar fuer Statistik, SOL G5, ETH (Federal Institute of Technology)
#8092 Zurich	SWITZERLAND  	phone: x-41-1-256-5319  fax: x-41-1-252-3410
#
#
#================================ S function ========================
#
if(!.R.) {
  p.sunflowers <- function(x, y, number, size = 0.125, add = FALSE,
                           pch = 16, ...)
{
## Purpose: Produce a 'sunflower'-Plot
## -------------------------------------------------------------------------
## Arguments: x,y: coordinates;
##    number[i] = number of times for (x[i],y[i])  [may be 0]
##    size: in inches;  1 in := 2.54 cm
##    add : (logical) Should I add to a previous plot ?
##    further args: as for plot(..)
## -------------------------------------------------------------------------
## Authors: Andreas Ruckstuhl, Werner Stahel, Martin Maechler, Tim Hesterberg
## Date   : Aug 89 / Jan 93,   March 92,      Jan 93,          Jan 93
## Examples: p.sunflowers(x=sort(round(rnorm(100))), y= round(2*rnorm(100),0))
## ~~~~~~~~  p.sunflowers(rnorm(100),rnorm(100), number=rpois(n=100,lambda=2), 
##                        main="Sunflower plot")
	n <- length(x)
	if(length(y) != n)
		stop("x & y must have same length !")
	if(missing(number)) {
		orderxy <- order(x, y)
		x <- x[orderxy]
		y <- y[orderxy]
		first <- c(TRUE, (x[-1] != x[ - n]) | (y[-1] != y[ - n]))
		x <- x[first]
		y <- y[first]
		number <- diff(c((1:n)[first], n + 1))
	}
	else {
		if(length(number) != n)
			stop("number must have same length as x & y !")
		x <- x[number > 0]
		y <- y[number > 0]
		number <- number[number > 0]
	}
	n <- length(x)
	if(!add) {
		axislabels <- match(c("xlab", "ylab"), names(list(...)))
		if(!is.na(axislabels[1]))
			xlab <- list(...)[[axislabels[1]]]
		else xlab <- deparse(substitute(x))
		if(!is.na(axislabels[2]))
			ylab <- list(...)[[axislabels[2]]]
		else ylab <- deparse(substitute(y))
		plot(x, y, xlab = xlab, ylab = ylab, type = "n", ...)
	}
	nequ1 <- number == 1
	if(any(nequ1))
		points(x[nequ1], y[nequ1], pch = pch, csi = size * 1.25)
	if(any(!nequ1))
		points(x[!nequ1], y[!nequ1], pch = pch, csi = size * 0.8)
	i.multi <- (1:n)[number > 1]
	if(length(i.multi)) {
		ppin <- par()$pin
		pusr <- par()$usr
		xr <- (size * abs(pusr[2] - pusr[1]))/ppin[1]
		yr <- (size * abs(pusr[4] - pusr[3]))/ppin[2]
		i.rep <- rep(i.multi, number[number > 1])
		z <- NULL
		for(i in i.multi)
			z <- c(z, 1:number[i])
		deg <- (2 * pi * z)/number[i.rep]
		segments(x[i.rep], y[i.rep], x[i.rep] + xr * sin(deg), y[i.rep] +
			yr * cos(deg))
	}
invisible()
}
NULL
}


if(FALSE) {
panel.abwplot <- function(x, y, box.ratio = 1, means=TRUE,
						  font = box.dot$font, pch = box.dot$pch, 
						  cex = box.dot$cex, 
						  col = box.dot$col, ...) {
  ok <- !is.na(x) & !is.na(y)
  x <- x[ok]
  y <- y[ok]
  y.unique <- sort(unique(y))
  width <- box.ratio/(1 + box.ratio)
  w <- width/2
  lineopts <- trellis.par.get("box.rectangle")
  for(Y in y.unique) {
	X <- x[y == Y]
	q <- quantile(X, c(.01,.05,.1,.25,.75,.9,.95,.99,.5))
	median.value <- list(x = q[9], y = Y)
	z <- c(1, .01,
		   2, .01,
		   2, .05,
		   3, .05,
		   3, .10,
		   4, .10,
		   4, .25,
		   5, .25,
		   5, .10,
		   6, .10,
		   6, .05,
		   7, .05,
		   7, .01,
		   8, .01,
		   8,-.01,
		   7,-.01,
		   7,-.05,
		   6,-.05,
		   6,-.10,
		   5,-.10,
		   5,-.25,
		   4,-.25,
		   4,-.10,
		   3,-.10,
		   3,-.05,
		   2,-.05,
		   2,-.01,
		   1,-.01,
		   1, .01)
	box.dot <- trellis.par.get("box.dot")
	box.dot.par <- c(list(pch = pch, cex = cex, col = col, font = font), ...)
	do.call('lines',c(list(x=q[z[seq(1,length(z),by=2)]],
						   y=Y + 4*w*z[seq(2,length(z),by=2)]),lineopts))
##	do.call('segments',c(list(x1=q[c(2:7)],y1=Y+rep(-w,6),
##							  x2=q[c(2:7)],y2=Y+rep(w,6)),
##						 lineopts))
	do.call("points", c(median.value, box.dot.par))
	if(means) do.call('lines',c(list(x=rep(mean(X),2),y=Y+c(-w,w)),
							   lineopts, lty=2))
  }
}
NULL
}
panel.bpplot <- function(x, y, box.ratio = 1, means=TRUE, qref=c(.5,.25,.75),
						 probs= c(.05,.125,.25,.375), nout=0,
						 datadensity=FALSE, scat1d.opts=NULL,
						 font = box.dot$font, pch = box.dot$pch, 
						 cex  = box.dot$cex, col = box.dot$col, ...) {

  grid <- .R.
  if(grid) {lines <- llines; points <- lpoints; segments <- lsegments}

  y <- as.numeric(y)   ## 25nov02
  ok <- !is.na(x) & !is.na(y)
  x <- x[ok]
  y <- y[ok]
  y.unique <-  sort(unique(y))
  width <- box.ratio/(1 + box.ratio)
  w <- width/2
  probs2 <- sort(c(probs,1-probs))

  box.dot  <- trellis.par.get("box.dot")
  lineopts <- trellis.par.get("box.rectangle")
  box.dot.par <- c(list(pch = pch, cex = cex, col = col, font = font), ...)

  m  <- length(probs)
  m2 <- length(probs2)
  j <- c(1,sort(rep(2:m2,2)),-sort(-rep(1:(m2-1),2)))
  z <- c(sort(rep(probs,2)),-sort(-rep(probs[1:(m-1)],2)))
  z <- c(z, -z, probs[1])
  k <- max(z)
  k <- if(k > .48) .5 else k
  if(length(qref)) {
	size.qref <- pmin(qref, 1-qref)
	size.qref[qref==.5] <- k
  }
  
  for(Y in y.unique) {
	X <- x[y == Y]
    if(!length(X)) next   ## 25nov02
	q <- quantile(X, c(probs2,qref))
	if(length(qref)) 
	  do.call('segments',c(list(q[-(1:m2)],      Y-w*size.qref/k,
                                q[-(1:m2)], 	 Y+w*size.qref/k),
						   lineopts))
	do.call('lines',c(list(x=q[j], y=Y + w*z/k), lineopts))
	if(means) {
	  mean.value <- list(x=mean(X), y=Y)
	  do.call('points', c(mean.value, box.dot.par))
	}
	if(datadensity) do.call('scat1d',c(list(x=X,y=Y,grid=grid), scat1d.opts))
	if(nout>0) {
	  ii <- if(nout < 1) {
		## Note - bug in quantile - endless loop if probs=c(.5,.5)
		if(nout==.5) stop('instead of nout=.5 use datadensity=T')
		cuts <- quantile(X, c(nout,1-nout))
		X < cuts[1] | X > cuts[2]
	  } else {
		X <- sort(X)
		nx <- length(X)
		ll <- 1:nx
		(ll <= min(nout,nx/2)) | (ll >= max(nx-nout+1,nx/2))
	  }
	  if(sum(ii)) do.call('scat1d',c(list(x=X[ii],y=Y,grid=grid), scat1d.opts))
	}
  }
}

# Given a matrix where rows are groups and columns have all the
# quantiles already computed, plus the Mean, draw a panel containing
# horizontal box-percentile plots like the default in panel.bpplot.  This is
# primarily for plot.summary.formula.reverse's continuous variable
# plots

bpplt <- function(stats, xlim, xlab='', box.ratio = 1, means=TRUE,
                  qref=c(.5,.25,.75), qomit=c(.025,.975),
                  pch=16, cex.labels=par('cex'),
                  cex.points=if(prototype)1 else .5,
                  grid=FALSE) {

  prototype <- missing(stats)
  if(prototype) {
    x <- c(.025,.05,.125,.25,.375,.5,.625,.75,.875,.95,.975)
    stats <- matrix(x, nrow=1, dimnames=list('',format(x)))
    Means <- .56
  } else {
    Means <- stats[,'Mean']
    stats <- stats[,dimnames(stats)[[2]] %nin% c('Mean','SD'),drop=FALSE]
  }
  groups <- dimnames(stats)[[1]]
  qq <- as.numeric(dimnames(stats)[[2]])
  probs2 <- qq
  if(missing(xlim)) xlim <- range(stats)
  i <- integer(0)
  for(a in c(.5,qomit))
      i <- c(i, (1:length(probs2))[abs(probs2-a)<.001])
  probs2 <- probs2[-i]
  probs  <- probs2[1:(floor(length(probs2)/2))]

  if(grid) {lines <- llines; points <- lpoints; segments <- lsegments}

  width <- box.ratio/(1 + box.ratio)
  w <- width/2

  m  <- length(probs)
  m2 <- length(probs2)
  j <- c(1,sort(rep(2:m2,2)),-sort(-rep(1:(m2-1),2)))
  z <- c(sort(rep(probs,2)),-sort(-rep(probs[1:(m-1)],2)))
  z <- c(z, -z, probs[1])
  k <- max(z)
  k <- if(k > .48) .5 else k
  if(length(qref)) {
	size.qref <- pmin(qref, 1-qref)
	size.qref[qref==.5] <- k
  }

  if(.R.) plot.new()
  mai <- omai <- par('mai')
  on.exit(par(mai=omai))
  mxlab <- .3+max(strwidth(groups, units='inches',cex=cex.labels))
  ## was .2+max  31jan03
  mai[2] <- mxlab
  par(mai=mai, new=TRUE)
  
  plot(xlim, c(.5,length(groups)+.5), xlim=xlim, xlab='', ylab='',
       axes=FALSE, type='n')
  if(!prototype) {
    box()
    mgp.axis(1, axistitle=xlab)  ## 28jan03
  }
  if(.R.) mtext(paste(groups,''), 2, 0, at=length(groups):1,
                adj=1, las=1, cex=cex.labels) else
  mtext(paste(groups,''), 2, 0, at=length(groups):1,
        adj=1, srt=0, cex=cex.labels)

  y <- 0
  for(Y in length(groups):1) {
    y <- y + 1
    q <- stats[Y,match(c(probs2,qref),qq)]
	if(length(qref)) 
	  do.call('segments',c(list(q[-(1:m2)],      y-w*size.qref/k,
                                q[-(1:m2)], 	 y+w*size.qref/k)))
	lines(q[j], y + w*z/k)
	if(means) points(Means[Y], y, pch=pch, cex=cex.points)
  }
  if(prototype) {
    mar <- par('mar')
    on.exit(par(mar=mar))
    par(mar=rep(.5,4))
    text(Means, 1.025+.02, 'Mean')
    for(a in c(.5,probs2)) {
      if(.R.) arrows(a, .6, a, .725, length=.1) else
      arrows(a, .6, a, .725, size=.1)
      f <- format(a)
      text(a, .575, format(a))
    }
    text(.5, .52, 'Quantiles')
    xd <- .004
    text(.485-xd, 1,
         if(.R.) expression(Median==Q[2]) else 'Median = Q2', srt=90)
    text(.235-xd, 1,
         if(.R.) expression(Q[1]) else 'Q1', srt=90)
    text(.735-xd, 1,
         if(.R.) expression(Q[3]) else 'Q3', srt=90)
    lines(c(.375,.625), rep(1.3,2)); text(.635, 1.3,  '1/4', adj=0, cex=.9)
    lines(c(.25, .75 ), rep(1.35,2));text(.76,  1.35, '1/2', adj=0, cex=.9)
    lines(c(.125,.875), rep(1.4,2)); text(.885, 1.4,  '3/4', adj=0, cex=.9)
    lines(c(.05, .95),  rep(1.45,2));text(.96,  1.45, '9/10',adj=0, cex=.9)
    text(.68, 1.24, 'Fraction of Sample Covered', adj=0, srt=13, cex=.7)
  }
}



pc1 <- function(x, hi) {
  if(.R.) require('mva')
  p <- ncol(x)
  x <-  x[!is.na(x %*% rep(1,p)),]
  xo <- x
  for(i in 1:p) {
    y <- x[,i]
    x[,i] <- (y-mean(y))/sqrt(var(y))
  }
  g <- prcomp(x)
  cat("Fraction variance explained by PC1:",format(g$sdev[1]^2/sum(g$sdev^2)),
      "\n\n")
  pc1 <- g$x[,1]
  
  f <- lsfit(xo, pc1)
  
  if(!missing(hi)) {
    if(sum(f$coef[-1]<0) >= p/2) pc1 <- -pc1
    r <- range(pc1)
    pc1 <- hi*(pc1-r[1])/diff(r)
    f <- lsfit(xo, pc1)
  }
  
  cat("Coefficients to obtain PC1:\n\n")
  print(f$coef)
  attr(pc1,"coef") <- f$coef
  invisible(pc1)
}

plsmo <- function(x,y,method=c("lowess","supsmu","raw"),
		xlab,ylab,add=FALSE,lty=1:nlev,col=par('col'),lwd=par('lwd'),
        iter=if(length(unique(y))>2) 3 else 0, bass=0,
		trim, fun, group=rep(1,length(x)), prefix, xlim, ylim, 
		label.curves=TRUE,	datadensity=FALSE, lines.=TRUE,
		subset=TRUE, grid=FALSE, ...) {

  if(.R.) library(modreg)
  
  gfun <- ordGridFun(grid)
  nam <- as.character(sys.call())[2:3]
  method <- match.arg(method)
  if(!missing(subset)) {  ## 20jul02
    x <- x[subset]
    y <- y[subset]
    group <- group[subset]
  }
    
  group <- as.factor(group)
  if(!missing(prefix)) levels(group) <- paste(prefix,levels(group))
  group <- as.factor(group)
  nna <- !(is.na(x+y)|is.na(group))
  x <- x[nna]
  y <- y[nna]
  group <- group[nna]

  lev <- levels(group)
  nlev <- length(lev)
  curves <- vector('list',nlev)
  names(curves) <- lev

  xmin <- ymin <- 1e30; xmax <- ymax <- -1e30
  for(g in lev) {
	s <- group==g
	z <- switch(method, 
                lowess=lowess(x[s],y[s],iter=iter),
                supsmu=supsmu(x[s],y[s], bass=bass),
                raw=approx(x[s],y[s],xout=sort(unique(x[s]))))
    
	if(missing(trim))trim <- if(sum(s)>200) 10/sum(s) else 0
	if(trim>0 && trim<1) {
      xq <- quantile(x[s],c(trim,1-trim))
      s <- z$x>=xq[1] & z$x<=xq[2]
      z <- list(x=z$x[s],y=z$y[s])
    }
	if(!missing(fun)) {
	  yy <- fun(z$y)
	  s <- !is.infinite(yy) & !is.na(yy)   ## was is.inf 11Apr02
	  z <- list(x=z$x[s],y=yy[s])
	}
	curves[[g]] <- z
	xmin <- min(xmin, z$x); xmax <- max(xmax, z$x)
	ymin <- min(ymin, z$y); ymax <- max(ymax, z$y)
	}

	if(!add) {
      if(grid) stop('add=T not implemented under grid/lattice in R')
#	  if(missing(xlab)) xlab <- if(label(x)!='') label(x) else nam[1] 26sep02
#	  if(missing(ylab)) ylab <- if(label(y)!='') label(y) else nam[2]
      if(missing(xlab))
        xlab <- label(x, units=TRUE, plot=TRUE, default=nam[1])
      if(missing(ylab))
        ylab <- label(y, units=TRUE, plot=TRUE, default=nam[2])
	  plot(xmin,ymin,xlim=if(missing(xlim))c(xmin,xmax) else xlim,
		   ylim=if(missing(ylim))c(ymin,ymax) else ylim, type='n',
		   xlab=xlab, ylab=ylab)
	}
	lty <- rep(lty, length=nlev)
	col <- rep(col, length=nlev)
    if(missing(lwd) && is.list(label.curves) &&
       length(label.curves$lwd)) lwd <- label.curves$lwd  # 20Feb00
    lwd <- rep(lwd, length=nlev)

	if(lines.) for(i in 1:nlev)
      gfun$lines(curves[[i]], lty=lty[i], col=col[i], lwd=lwd[i])  # 20Feb00

	if(datadensity) {
	  for(i in 1:nlev) {
		s <- group==lev[i]
		x1 <- x[s]
		y.x1 <- approx(curves[[i]], xout=x1)$y
		scat1d(x1, y=y.x1, col=col[i], grid=grid, ...)
	  }
	}

	if((is.list(label.curves) || label.curves) && 
	   nlev>1 && (!missing(prefix) | !add | !missing(label.curves))) 
	  labcurve(curves, lty=lty, col=col, opts=label.curves, grid=grid)
	invisible(curves)
}



panel.plsmo <- function(x, y, subscripts, groups=NULL, type='b', 
						label.curves=TRUE,
						lwd = superpose.line$lwd, 
						lty = superpose.line$lty, 
						pch = superpose.symbol$pch, 
						cex = superpose.symbol$cex, 
						font = superpose.symbol$font, 
						col = NULL,...) {

  superpose.symbol <- trellis.par.get("superpose.symbol")
  superpose.line <- trellis.par.get("superpose.line")
  if(length(groups)) groups <- as.factor(groups)
  g <- oldUnclass(groups)[subscripts]
  ng <- if(length(groups)) max(g) else 1
  lty  <- rep(lty, length = ng)
  lwd  <- rep(lwd, length = ng)
  pch  <- rep(pch, length = ng)
  cex  <- rep(cex, length = ng)
  font <- rep(font, length = ng)
  if(!length(col)) col <- if(type=='p') superpose.symbol$col else
    superpose.line$col
  col <- rep(col, length = ng)
  lc <- if(is.logical(label.curves)) {
	if(label.curves) list(lwd=lwd, cex=cex[1]) else FALSE
  } else c(list(lwd=lwd, cex=cex[1]), label.curves)
  if(type!='p') if(ng > 1)
	plsmo(x, y, group=groups[subscripts,drop=FALSE], 
          add=TRUE, lty=lty, col=col, label.curves=lc, grid=.R., ...) else
    plsmo(x, y, add=TRUE, lty=lty, col=col, label.curves=lc, grid=.R.,
          ...)

  if(type!='l') {
	if(ng > 1) panel.superpose(x, y, subscripts,
                               if(.R.)as.integer(groups) else groups, 
							   lwd=lwd, lty=lty, pch=pch, cex=cex, 
							   font=font, col=col) else
	  panel.xyplot(x, y, 
				   lwd=lwd, lty=lty, pch=pch, cex=cex, 
				   font=font, col=col)
	if(ng > 1) {
      Key <- if(.R.) function(x=NULL, y=NULL, lev, cex, col, font, pch) {
        oldpar <- par(usr=c(0,1,0,1),xpd=NA)
        on.exit(par(oldpar))
        if(is.list(x)) { y <- x[[2]]; x <- x[[1]] }
        ## Even though par('usr') shows 0,1,0,1 after lattice draws
        ## its plot, it still needs resetting
        if(!length(x)) x <- 0
        if(!length(y)) y <- 1  ## because of formals()
        rlegend(x, y, legend=lev, cex=cex, col=col, pch=pch)
        invisible()
      } else function(x=NULL, y=NULL, lev, cex, col, font, pch, ...) {
		if(length(x)) {
		  if(is.list(x)) {y <- x$y; x <- x$x}
		  key(x=x, y=y, text=list(lev, col=col), 
			  points=list(cex=cex,col=col,font=font,pch=pch),
			  transparent=TRUE, ...) } else
		key(text=list(lev, col=col), 
			points=list(cex=cex,col=col,font=font,pch=pch),
            transparent=TRUE, ...)
        invisible()
      }
      formals(Key) <- list(x=NULL,y=NULL,lev=levels(groups), cex=cex,
                           col=col, font=font, pch=pch)
	  storeTemp(Key)
    }
  }
}
popower <- function(p, odds.ratio, n, n1, n2, alpha=.05) {
if(missing(n)) n <- n1+n2 else {n1 <- n2 <- n/2}
p <- p[!is.na(p)]
if(abs(sum(p)-1)>.0001) stop('probabilities in p do not add up to 1')
z <- qnorm(1-alpha/2)
A <- n2/n1
ps <- 1 - sum(p^3)
V <- n1*n2*n/3/((n+1)^2)*ps
power <- pnorm(abs(logb(odds.ratio))*sqrt(V) - z)
eff <- ps/(1-1/n/n)
structure(list(power=power, eff=eff), class='popower')
}

print.popower <- function(x, ...) {
  cat('Power:',round(x$power,3),
      '\nEfficiency of design compared with continuous response:',
         round(x$eff,3),'\n\n')
  invisible()
}
      
posamsize <- function(p, odds.ratio, fraction=.5, 
                     alpha=.05, power=.8) {

p <- p[!is.na(p)]
if(abs(sum(p)-1)>.0001) stop('probabilities in p do not add up to 1')

A <- (1-fraction)/fraction
log.or <- logb(odds.ratio)
z.alpha <- qnorm(1-alpha/2)
z.beta <- qnorm(power)
ps <- 1 - sum(p^3)
n <- 3*((A+1)^2)*(z.alpha+z.beta)^2/A/(log.or^2)/ps
eff <- ps/(1-1/n/n)
structure(list(n=n,eff=eff), class='posamsize')
}

print.posamsize <- function(x, ...) {
  cat('Total sample size:',round(x$n,1),
      '\nEfficiency of design compared with continuous response:',
         round(x$eff,3),'\n\n')
  invisible()
}
ps.slide <- function(file, background=if(type!=2)"white" else "navy blue", 
	foreground=if(type==2)'yellow' else (if(background=="white")"black" else "white"),
	font='Helvetica',
	pointsize=c(24,28,14,14)[type], hor=type!=4, 
	lwd=c(2,5,2,4)[type],
	mgp=if(under.unix) list(c(1.8,.4,0),c(1.5,.2,0),c(2,.4,0),c(1.5,.2,0))[[type]] else
					 list(c(1.8,.5,0),c(1.5,.4,0),c(2,.5,0),c(1.5,.4,0))[[type]],
	mar=list(c(4,3,2,1)+.1,c(5,4,2.25,2)+.1,c(3,3,1,1)+.1,
		c(5,4,2.25,2)+.1)[[type]],
	pch=202, view=FALSE, pcx=FALSE, tiff=FALSE, close=view|pcx|tiff, bty="l", type=2,
	height=switch(type,NULL,NULL,5,8), width=switch(type,NULL,NULL,7,7),
					 tck=if(type==3 || !under.unix)-.013 else par('tck'), 
					 las=if(type==3)1 else 0, 
					 eps=FALSE, ...) {

if(close) {
  graphics.off()
  file <- .Options$ps.slide.file
  if(view) unix(paste("ghostview ", file, ".ps &", sep=""), output=FALSE)
  if(pcx) {
    unix(paste("(gs -sDEVICE=pbm -sOutputFile=- -r75 -q - quit.ps < ",
	file, ".ps | pnmflip -cw | ppmtopcx > ", file, ".pcx) &", sep=""),
	output=FALSE)
    cat("\nFile ", file, ".pcx being created \n", sep="")
#    if(view) unix(paste("xli ", file, ".pcx &", sep=""), output=FALSE)
  }
  if(tiff) {
    unix(paste("(gs -sDEVICE=pbmraw -sOutputFile=- -r300 -q - quit.ps < ",
         file, ".ps | pnmflip -cw | pnmtotiff > ", file, ".tiff) &",sep=""),
         output=FALSE)
    cat("\nFile ", file, ".tiff being created \n", sep="")
  }
  return(invisible())
}

if(is.logical(background) && background) background <- "navy blue"
options(ps.slide.file=file, TEMPORARY=FALSE)
if(!.R.) {
  cols <- ps.colors.rgb[c(foreground,background),]
  fonts <- if(under.unix)ps.options()$fonts else ps.fonts
  fonts[1] <- font
  if(font=='Times-Roman') fonts[5] <- 'Times-Bold'
  if(under.unix) {
    ps.options(colors=cols, background=2, fonts=fonts, pointsize=pointsize, ...)
    cat('\nIf using legend() be sure to add the arguments background=2, bty="n"\n')
  
  if(length(height) && length(width)) 
	postscript(paste(file,'.ps',sep=''),
			   hor=hor, height=height, width=width,
			   pointsize=.6*pointsize*max(width/(30*12/72.27),
				 height/(30*12/72.27/((1+sqrt(5))/2))),
			   print.it=FALSE, onefile=!eps) else
  postscript(paste(file,".ps",sep=""), hor=hor, print.it=FALSE, onefile=!eps)
} else {
  if(length(height) && length(width)) 
	postscript(paste(file,'.ps',sep=''),
			   hor=hor, height=height, width=width,
			   pointsize=.6*pointsize*max(width/(30*12/72.27),
				 height/(30*12/72.27/((1+sqrt(5))/2))),
			   colors=cols, fonts=fonts, ...) else
	postscript(paste(file,'.ps',sep=''),
			   hor=hor, colors=cols, fonts=fonts, ...)
	  }
} else { # 10Apr01
  if(length(height) && length(width)) 
	postscript(paste(file,'.ps',sep=''),
			   hor=hor, height=height, width=width,
			   pointsize=.6*pointsize*max(width/(30*12/72.27),
				 height/(30*12/72.27/((1+sqrt(5))/2))),
			   fg=foreground, bg=background, family=font, ...) else
	postscript(paste(file,'.ps',sep=''),
			   fg=foreground, bg=background, family=font, ...)
}

  par(lwd=lwd, mgp=mgp, mar=mar, pch=pch, bty=bty, smo=0, tck=tck, las=las)
#  mgp.axis.labels(c(mgp[2], if(las==1) 1.3 else mgp[2]))
  invisible()
}

if(!.R. && !under.unix) ps.colors.rgb <-
structure(.Data = c(1, 0.972549019607843, 0.972549019607843, 0.96078431372549, 
	0.96078431372549, 0.862745098039216, 1, 1, 0.992156862745098, 
	0.992156862745098, 0.980392156862745, 0.980392156862745, 
	0.980392156862745, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
	0.941176470588235, 0.96078431372549, 0.96078431372549, 
	0.941176470588235, 0.941176470588235, 0.941176470588235, 
	0.901960784313726, 1, 1, 1, 1, 1, 0, 0.184313725490196, 
	0.184313725490196, 0.184313725490196, 0.184313725490196, 
	0.411764705882353, 0.411764705882353, 0.411764705882353, 
	0.411764705882353, 0.43921568627451, 0.43921568627451, 0.43921568627451,
	0.43921568627451, 0.466666666666667, 0.466666666666667, 
	0.466666666666667, 0.466666666666667, 0.752941176470588, 
	0.752941176470588, 0.827450980392157, 0.827450980392157, 
	0.827450980392157, 0.827450980392157, 0.0980392156862745, 
	0.0980392156862745, 0, 0, 0, 0.392156862745098, 0.392156862745098, 
	0.282352941176471, 0.282352941176471, 0.415686274509804, 
	0.415686274509804, 0.482352941176471, 0.482352941176471, 
	0.517647058823529, 0.517647058823529, 0, 0, 0.254901960784314, 
	0.254901960784314, 0, 0.117647058823529, 0.117647058823529, 0, 0, 
	0.529411764705882, 0.529411764705882, 0.529411764705882, 
	0.529411764705882, 0.274509803921569, 0.274509803921569, 
	0.690196078431373, 0.690196078431373, 0.67843137254902, 
	0.67843137254902, 0.690196078431373, 0.690196078431373, 
	0.686274509803922, 0.686274509803922, 0, 0, 0.282352941176471, 
	0.282352941176471, 0.250980392156863, 0, 0.87843137254902, 
	0.87843137254902, 0.372549019607843, 0.372549019607843, 0.4, 0.4, 
	0.498039215686275, 0, 0, 0.333333333333333, 0.333333333333333, 
	0.56078431372549, 0.56078431372549, 0.180392156862745, 
	0.180392156862745, 0.235294117647059, 0.235294117647059, 
	0.125490196078431, 0.125490196078431, 0.596078431372549, 
	0.596078431372549, 0, 0, 0.486274509803922, 0.486274509803922, 0, 
	0.498039215686275, 0, 0, 0.67843137254902, 0.67843137254902, 
	0.196078431372549, 0.196078431372549, 0.603921568627451, 
	0.603921568627451, 0.133333333333333, 0.133333333333333, 
	0.419607843137255, 0.419607843137255, 0.741176470588235, 
	0.741176470588235, 0.941176470588235, 0.933333333333333, 
	0.933333333333333, 0.980392156862745, 0.980392156862745, 1, 1, 1, 1, 
	0.933333333333333, 0.933333333333333, 0.854901960784314, 
	0.72156862745098, 0.72156862745098, 0.737254901960784, 
	0.737254901960784, 0.803921568627451, 0.803921568627451, 
	0.545098039215686, 0.545098039215686, 0.627450980392157, 
	0.803921568627451, 0.870588235294118, 0.96078431372549, 
	0.96078431372549, 0.956862745098039, 0.956862745098039, 
	0.823529411764706, 0.823529411764706, 0.698039215686274, 
	0.647058823529412, 0.913725490196078, 0.913725490196078, 
	0.980392156862745, 1, 1, 1, 1, 1, 1, 0.941176470588235, 
	0.941176470588235, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.858823529411765, 
	0.858823529411765, 0.690196078431373, 0.780392156862745, 
	0.780392156862745, 0.815686274509804, 0.815686274509804, 1, 
	0.933333333333333, 0.866666666666667, 0.854901960784314, 
	0.729411764705882, 0.729411764705882, 0.6, 0.6, 0.580392156862745, 
	0.580392156862745, 0.541176470588235, 0.541176470588235, 
	0.627450980392157, 0.576470588235294, 0.576470588235294, 
	0.847058823529412, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 0.941176470588235, 0.87843137254902, 
	0.756862745098039, 0.513725490196078, 1, 0.933333333333333, 
	0.803921568627451, 0.545098039215686, 1, 0.933333333333333, 
	0.803921568627451, 0.545098039215686, 0.941176470588235, 
	0.87843137254902, 0.756862745098039, 0.513725490196078, 
	0.513725490196078, 0.47843137254902, 0.411764705882353, 
	0.27843137254902, 0.282352941176471, 0.262745098039216, 
	0.227450980392157, 0.152941176470588, 0, 0, 0, 0, 0.117647058823529, 
	0.109803921568627, 0.0941176470588235, 0.0627450980392157, 
	0.388235294117647, 0.36078431372549, 0.309803921568627, 
	0.211764705882353, 0, 0, 0, 0, 0.529411764705882, 0.494117647058824, 
	0.423529411764706, 0.290196078431373, 0.690196078431373, 
	0.643137254901961, 0.552941176470588, 0.376470588235294, 
	0.776470588235294, 0.725490196078431, 0.623529411764706, 
	0.423529411764706, 0.792156862745098, 0.737254901960784, 
	0.635294117647059, 0.431372549019608, 0.749019607843137, 
	0.698039215686274, 0.603921568627451, 0.407843137254902, 
	0.87843137254902, 0.819607843137255, 0.705882352941177, 
	0.47843137254902, 0.733333333333333, 0.682352941176471, 
	0.588235294117647, 0.4, 0.596078431372549, 0.556862745098039, 
	0.47843137254902, 0.325490196078431, 0, 0, 0, 0, 0, 0, 0, 0, 
	0.592156862745098, 0.552941176470588, 0.474509803921569, 
	0.32156862745098, 0.498039215686275, 0.462745098039216, 0.4, 
	0.270588235294118, 0.756862745098039, 0.705882352941177, 
	0.607843137254902, 0.411764705882353, 0.329411764705882, 
	0.305882352941176, 0.262745098039216, 0.180392156862745, 
	0.603921568627451, 0.564705882352941, 0.486274509803922, 
	0.329411764705882, 0, 0, 0, 0, 0, 0, 0, 0, 0.498039215686275, 
	0.462745098039216, 0.4, 0.270588235294118, 0.752941176470588, 
	0.701960784313725, 0.603921568627451, 0.411764705882353, 
	0.792156862745098, 0.737254901960784, 0.635294117647059, 
	0.431372549019608, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 0.87843137254902, 0.819607843137255, 
	0.705882352941177, 0.47843137254902, 0.749019607843137, 
	0.698039215686274, 0.603921568627451, 0.407843137254902, 
	0.607843137254902, 0.568627450980392, 0.490196078431373, 
	0.333333333333333, 0.670588235294118, 0.623529411764706, 
	0.537254901960784, 0.364705882352941, 1, 0.933333333333333, 
	0.803921568627451, 0.545098039215686, 0, 0, 0.0117647058823529, 
	0.0117647058823529, 0.0196078431372549, 0.0196078431372549, 
	0.0313725490196078, 0.0313725490196078, 0.0392156862745098, 
	0.0392156862745098, 0.0509803921568627, 0.0509803921568627, 
	0.0588235294117647, 0.0588235294117647, 0.0705882352941176, 
	0.0705882352941176, 0.0784313725490196, 0.0784313725490196, 
	0.0901960784313725, 0.0901960784313725, 0.101960784313725, 
	0.101960784313725, 0.109803921568627, 0.109803921568627, 
	0.12156862745098, 0.12156862745098, 0.129411764705882, 
	0.129411764705882, 0.141176470588235, 0.141176470588235, 
	0.149019607843137, 0.149019607843137, 0.16078431372549, 
	0.16078431372549, 0.168627450980392, 0.168627450980392, 
	0.180392156862745, 0.180392156862745, 0.188235294117647, 
	0.188235294117647, 0.2, 0.2, 0.211764705882353, 0.211764705882353, 
	0.219607843137255, 0.219607843137255, 0.231372549019608, 
	0.231372549019608, 0.23921568627451, 0.23921568627451, 
	0.250980392156863, 0.250980392156863, 0.258823529411765, 
	0.258823529411765, 0.270588235294118, 0.270588235294118, 
	0.27843137254902, 0.27843137254902, 0.290196078431373, 
	0.290196078431373, 0.301960784313725, 0.301960784313725, 
	0.309803921568627, 0.309803921568627, 0.32156862745098, 
	0.32156862745098, 0.329411764705882, 0.329411764705882, 
	0.341176470588235, 0.341176470588235, 0.349019607843137, 
	0.349019607843137, 0.36078431372549, 0.36078431372549, 
	0.368627450980392, 0.368627450980392, 0.380392156862745, 
	0.380392156862745, 0.388235294117647, 0.388235294117647, 0.4, 0.4, 
	0.411764705882353, 0.411764705882353, 0.419607843137255, 
	0.419607843137255, 0.431372549019608, 0.431372549019608, 
	0.43921568627451, 0.43921568627451, 0.450980392156863, 
	0.450980392156863, 0.458823529411765, 0.458823529411765, 
	0.470588235294118, 0.470588235294118, 0.47843137254902, 
	0.47843137254902, 0.490196078431373, 0.490196078431373, 
	0.498039215686275, 0.498039215686275, 0.509803921568627, 
	0.509803921568627, 0.52156862745098, 0.52156862745098, 
	0.529411764705882, 0.529411764705882, 0.541176470588235, 
	0.541176470588235, 0.549019607843137, 0.549019607843137, 
	0.56078431372549, 0.56078431372549, 0.568627450980392, 
	0.568627450980392, 0.580392156862745, 0.580392156862745, 
	0.588235294117647, 0.588235294117647, 0.6, 0.6, 0.611764705882353, 
	0.611764705882353, 0.619607843137255, 0.619607843137255, 
	0.631372549019608, 0.631372549019608, 0.63921568627451, 
	0.63921568627451, 0.650980392156863, 0.650980392156863, 
	0.658823529411765, 0.658823529411765, 0.670588235294118, 
	0.670588235294118, 0.67843137254902, 0.67843137254902, 
	0.690196078431373, 0.690196078431373, 0.701960784313725, 
	0.701960784313725, 0.709803921568627, 0.709803921568627, 
	0.72156862745098, 0.72156862745098, 0.729411764705882, 
	0.729411764705882, 0.741176470588235, 0.741176470588235, 
	0.749019607843137, 0.749019607843137, 0.76078431372549, 
	0.76078431372549, 0.768627450980392, 0.768627450980392, 
	0.780392156862745, 0.780392156862745, 0.788235294117647, 
	0.788235294117647, 0.8, 0.8, 0.811764705882353, 0.811764705882353, 
	0.819607843137255, 0.819607843137255, 0.831372549019608, 
	0.831372549019608, 0.83921568627451, 0.83921568627451, 
	0.850980392156863, 0.850980392156863, 0.858823529411765, 
	0.858823529411765, 0.870588235294118, 0.870588235294118, 
	0.87843137254902, 0.87843137254902, 0.890196078431372, 
	0.890196078431372, 0.898039215686275, 0.898039215686275, 
	0.909803921568627, 0.909803921568627, 0.92156862745098, 
	0.92156862745098, 0.929411764705882, 0.929411764705882, 
	0.941176470588235, 0.941176470588235, 0.949019607843137, 
	0.949019607843137, 0.96078431372549, 0.96078431372549, 
	0.968627450980392, 0.968627450980392, 0.980392156862745, 
	0.980392156862745, 0.988235294117647, 0.988235294117647, 1, 1, 
	0.980392156862745, 0.972549019607843, 0.972549019607843, 
	0.96078431372549, 0.96078431372549, 0.862745098039216, 
	0.980392156862745, 0.980392156862745, 0.96078431372549, 
	0.96078431372549, 0.941176470588235, 0.92156862745098, 0.92156862745098,
	0.937254901960784, 0.937254901960784, 0.92156862745098, 
	0.92156862745098, 0.894117647058824, 0.854901960784314, 
	0.854901960784314, 0.870588235294118, 0.870588235294118, 
	0.894117647058824, 0.972549019607843, 1, 0.980392156862745, 
	0.980392156862745, 0.96078431372549, 1, 1, 1, 1, 0.972549019607843, 
	0.972549019607843, 0.901960784313726, 0.941176470588235, 
	0.941176470588235, 0.894117647058824, 0.894117647058824, 1, 0, 
	0.309803921568627, 0.309803921568627, 0.309803921568627, 
	0.309803921568627, 0.411764705882353, 0.411764705882353, 
	0.411764705882353, 0.411764705882353, 0.501960784313725, 
	0.501960784313725, 0.501960784313725, 0.501960784313725, 
	0.533333333333333, 0.533333333333333, 0.533333333333333, 
	0.533333333333333, 0.752941176470588, 0.752941176470588, 
	0.827450980392157, 0.827450980392157, 0.827450980392157, 
	0.827450980392157, 0.0980392156862745, 0.0980392156862745, 0, 0, 0, 
	0.584313725490196, 0.584313725490196, 0.23921568627451, 
	0.23921568627451, 0.352941176470588, 0.352941176470588, 
	0.407843137254902, 0.407843137254902, 0.43921568627451, 
	0.43921568627451, 0, 0, 0.411764705882353, 0.411764705882353, 0, 
	0.564705882352941, 0.564705882352941, 0.749019607843137, 
	0.749019607843137, 0.807843137254902, 0.807843137254902, 
	0.807843137254902, 0.807843137254902, 0.509803921568627, 
	0.509803921568627, 0.768627450980392, 0.768627450980392, 
	0.847058823529412, 0.847058823529412, 0.87843137254902, 
	0.87843137254902, 0.933333333333333, 0.933333333333333, 
	0.807843137254902, 0.807843137254902, 0.819607843137255, 
	0.819607843137255, 0.87843137254902, 1, 1, 1, 0.619607843137255, 
	0.619607843137255, 0.803921568627451, 0.803921568627451, 1, 
	0.392156862745098, 0.392156862745098, 0.419607843137255, 
	0.419607843137255, 0.737254901960784, 0.737254901960784, 
	0.545098039215686, 0.545098039215686, 0.701960784313725, 
	0.701960784313725, 0.698039215686274, 0.698039215686274, 
	0.984313725490196, 0.984313725490196, 1, 1, 0.988235294117647, 
	0.988235294117647, 1, 1, 0.980392156862745, 0.980392156862745, 1, 1, 
	0.803921568627451, 0.803921568627451, 0.803921568627451, 
	0.803921568627451, 0.545098039215686, 0.545098039215686, 
	0.556862745098039, 0.556862745098039, 0.717647058823529, 
	0.717647058823529, 0.901960784313726, 0.909803921568627, 
	0.909803921568627, 0.980392156862745, 0.980392156862745, 1, 1, 1, 
	0.843137254901961, 0.866666666666667, 0.866666666666667, 
	0.647058823529412, 0.525490196078431, 0.525490196078431, 
	0.56078431372549, 0.56078431372549, 0.36078431372549, 0.36078431372549, 
	0.270588235294118, 0.270588235294118, 0.32156862745098, 
	0.52156862745098, 0.72156862745098, 0.96078431372549, 0.870588235294118,
	0.643137254901961, 0.643137254901961, 0.705882352941177, 
	0.411764705882353, 0.133333333333333, 0.164705882352941, 
	0.588235294117647, 0.588235294117647, 0.501960784313725, 
	0.627450980392157, 0.627450980392157, 0.647058823529412, 
	0.549019607843137, 0.549019607843137, 0.498039215686275, 
	0.501960784313725, 0.501960784313725, 0.388235294117647, 
	0.270588235294118, 0.270588235294118, 0, 0.411764705882353, 
	0.411764705882353, 0.0784313725490196, 0.0784313725490196, 
	0.752941176470588, 0.713725490196078, 0.713725490196078, 
	0.43921568627451, 0.43921568627451, 0.188235294117647, 
	0.0823529411764706, 0.0823529411764706, 0.125490196078431, 
	0.125490196078431, 0, 0.509803921568627, 0.627450980392157, 
	0.43921568627451, 0.333333333333333, 0.333333333333333, 
	0.196078431372549, 0.196078431372549, 0, 0, 0.168627450980392, 
	0.168627450980392, 0.125490196078431, 0.43921568627451, 
	0.43921568627451, 0.749019607843137, 0.980392156862745, 
	0.913725490196078, 0.788235294117647, 0.537254901960784, 
	0.96078431372549, 0.898039215686275, 0.772549019607843, 
	0.525490196078431, 0.937254901960784, 0.874509803921569, 
	0.752941176470588, 0.513725490196078, 0.894117647058824, 
	0.835294117647059, 0.717647058823529, 0.490196078431373, 
	0.854901960784314, 0.796078431372549, 0.686274509803922, 
	0.466666666666667, 0.870588235294118, 0.811764705882353, 
	0.701960784313725, 0.474509803921569, 0.980392156862745, 
	0.913725490196078, 0.788235294117647, 0.537254901960784, 
	0.972549019607843, 0.909803921568627, 0.784313725490196, 
	0.533333333333333, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 0.941176470588235, 0.87843137254902, 
	0.756862745098039, 0.513725490196078, 0.894117647058824, 
	0.835294117647059, 0.717647058823529, 0.490196078431373, 1, 
	0.933333333333333, 0.803921568627451, 0.545098039215686, 
	0.435294117647059, 0.403921568627451, 0.349019607843137, 
	0.235294117647059, 0.462745098039216, 0.431372549019608, 
	0.372549019607843, 0.250980392156863, 0, 0, 0, 0, 0.564705882352941, 
	0.525490196078431, 0.454901960784314, 0.305882352941176, 
	0.72156862745098, 0.674509803921569, 0.580392156862745, 
	0.392156862745098, 0.749019607843137, 0.698039215686274, 
	0.603921568627451, 0.407843137254902, 0.807843137254902, 
	0.752941176470588, 0.650980392156863, 0.43921568627451, 
	0.886274509803922, 0.827450980392157, 0.713725490196078, 
	0.482352941176471, 0.886274509803922, 0.827450980392157, 
	0.713725490196078, 0.482352941176471, 0.882352941176471, 
	0.823529411764706, 0.709803921568627, 0.482352941176471, 
	0.937254901960784, 0.874509803921569, 0.752941176470588, 
	0.513725490196078, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 0.96078431372549, 0.898039215686275, 
	0.772549019607843, 0.525490196078431, 0.96078431372549, 
	0.898039215686275, 0.772549019607843, 0.525490196078431, 1, 
	0.933333333333333, 0.803921568627451, 0.545098039215686, 1, 
	0.933333333333333, 0.803921568627451, 0.545098039215686, 1, 
	0.933333333333333, 0.803921568627451, 0.545098039215686, 1, 
	0.933333333333333, 0.803921568627451, 0.545098039215686, 1, 
	0.933333333333333, 0.803921568627451, 0.545098039215686, 1, 
	0.933333333333333, 0.803921568627451, 0.545098039215686, 1, 
	0.933333333333333, 0.803921568627451, 0.545098039215686, 1, 
	0.933333333333333, 0.803921568627451, 0.545098039215686, 1, 
	0.933333333333333, 0.803921568627451, 0.545098039215686, 1, 
	0.933333333333333, 0.803921568627451, 0.545098039215686, 1, 
	0.933333333333333, 0.803921568627451, 0.545098039215686, 
	0.964705882352941, 0.901960784313726, 0.776470588235294, 
	0.525490196078431, 0.925490196078431, 0.862745098039216, 
	0.745098039215686, 0.505882352941176, 1, 0.933333333333333, 
	0.803921568627451, 0.545098039215686, 1, 0.933333333333333, 
	0.803921568627451, 0.545098039215686, 0.843137254901961, 
	0.788235294117647, 0.67843137254902, 0.458823529411765, 
	0.756862745098039, 0.705882352941177, 0.607843137254902, 
	0.411764705882353, 0.725490196078431, 0.67843137254902, 
	0.584313725490196, 0.396078431372549, 0.756862745098039, 
	0.705882352941177, 0.607843137254902, 0.411764705882353, 
	0.415686274509804, 0.388235294117647, 0.333333333333333, 
	0.227450980392157, 0.509803921568627, 0.474509803921569, 
	0.407843137254902, 0.27843137254902, 0.827450980392157, 
	0.772549019607843, 0.666666666666667, 0.450980392156863, 
	0.905882352941176, 0.847058823529412, 0.729411764705882, 
	0.494117647058824, 0.647058823529412, 0.603921568627451, 
	0.52156862745098, 0.352941176470588, 0.498039215686275, 
	0.462745098039216, 0.4, 0.270588235294118, 0.188235294117647, 
	0.172549019607843, 0.149019607843137, 0.101960784313725, 
	0.250980392156863, 0.231372549019608, 0.2, 0.137254901960784, 
	0.549019607843137, 0.509803921568627, 0.43921568627451, 
	0.298039215686275, 0.627450980392157, 0.584313725490196, 
	0.505882352941176, 0.341176470588235, 0.647058823529412, 
	0.603921568627451, 0.52156862745098, 0.352941176470588, 
	0.498039215686275, 0.462745098039216, 0.4, 0.270588235294118, 
	0.447058823529412, 0.415686274509804, 0.356862745098039, 
	0.243137254901961, 0.388235294117647, 0.36078431372549, 
	0.309803921568627, 0.211764705882353, 0.270588235294118, 
	0.250980392156863, 0.215686274509804, 0.145098039215686, 0, 0, 0, 0, 
	0.0784313725490196, 0.0705882352941176, 0.0627450980392157, 
	0.0392156862745098, 0.431372549019608, 0.415686274509804, 
	0.376470588235294, 0.227450980392157, 0.709803921568627, 
	0.662745098039216, 0.568627450980392, 0.388235294117647, 
	0.682352941176471, 0.635294117647059, 0.549019607843137, 
	0.372549019607843, 0.509803921568627, 0.474509803921569, 
	0.407843137254902, 0.27843137254902, 0.203921568627451, 
	0.188235294117647, 0.16078431372549, 0.109803921568627, 
	0.243137254901961, 0.227450980392157, 0.196078431372549, 
	0.133333333333333, 0, 0, 0, 0, 0.513725490196078, 0.47843137254902, 
	0.411764705882353, 0.27843137254902, 0.733333333333333, 
	0.682352941176471, 0.588235294117647, 0.4, 0.4, 0.372549019607843, 
	0.32156862745098, 0.215686274509804, 0.243137254901961, 
	0.227450980392157, 0.196078431372549, 0.133333333333333, 
	0.188235294117647, 0.172549019607843, 0.149019607843137, 
	0.101960784313725, 0.509803921568627, 0.474509803921569, 
	0.407843137254902, 0.27843137254902, 0.882352941176471, 
	0.823529411764706, 0.709803921568627, 0.482352941176471, 0, 0, 
	0.0117647058823529, 0.0117647058823529, 0.0196078431372549, 
	0.0196078431372549, 0.0313725490196078, 0.0313725490196078, 
	0.0392156862745098, 0.0392156862745098, 0.0509803921568627, 
	0.0509803921568627, 0.0588235294117647, 0.0588235294117647, 
	0.0705882352941176, 0.0705882352941176, 0.0784313725490196, 
	0.0784313725490196, 0.0901960784313725, 0.0901960784313725, 
	0.101960784313725, 0.101960784313725, 0.109803921568627, 
	0.109803921568627, 0.12156862745098, 0.12156862745098, 
	0.129411764705882, 0.129411764705882, 0.141176470588235, 
	0.141176470588235, 0.149019607843137, 0.149019607843137, 
	0.16078431372549, 0.16078431372549, 0.168627450980392, 
	0.168627450980392, 0.180392156862745, 0.180392156862745, 
	0.188235294117647, 0.188235294117647, 0.2, 0.2, 0.211764705882353, 
	0.211764705882353, 0.219607843137255, 0.219607843137255, 
	0.231372549019608, 0.231372549019608, 0.23921568627451, 
	0.23921568627451, 0.250980392156863, 0.250980392156863, 
	0.258823529411765, 0.258823529411765, 0.270588235294118, 
	0.270588235294118, 0.27843137254902, 0.27843137254902, 
	0.290196078431373, 0.290196078431373, 0.301960784313725, 
	0.301960784313725, 0.309803921568627, 0.309803921568627, 
	0.32156862745098, 0.32156862745098, 0.329411764705882, 
	0.329411764705882, 0.341176470588235, 0.341176470588235, 
	0.349019607843137, 0.349019607843137, 0.36078431372549, 
	0.36078431372549, 0.368627450980392, 0.368627450980392, 
	0.380392156862745, 0.380392156862745, 0.388235294117647, 
	0.388235294117647, 0.4, 0.4, 0.411764705882353, 0.411764705882353, 
	0.419607843137255, 0.419607843137255, 0.431372549019608, 
	0.431372549019608, 0.43921568627451, 0.43921568627451, 
	0.450980392156863, 0.450980392156863, 0.458823529411765, 
	0.458823529411765, 0.470588235294118, 0.470588235294118, 
	0.47843137254902, 0.47843137254902, 0.490196078431373, 
	0.490196078431373, 0.498039215686275, 0.498039215686275, 
	0.509803921568627, 0.509803921568627, 0.52156862745098, 
	0.52156862745098, 0.529411764705882, 0.529411764705882, 
	0.541176470588235, 0.541176470588235, 0.549019607843137, 
	0.549019607843137, 0.56078431372549, 0.56078431372549, 
	0.568627450980392, 0.568627450980392, 0.580392156862745, 
	0.580392156862745, 0.588235294117647, 0.588235294117647, 0.6, 0.6, 
	0.611764705882353, 0.611764705882353, 0.619607843137255, 
	0.619607843137255, 0.631372549019608, 0.631372549019608, 
	0.63921568627451, 0.63921568627451, 0.650980392156863, 
	0.650980392156863, 0.658823529411765, 0.658823529411765, 
	0.670588235294118, 0.670588235294118, 0.67843137254902, 
	0.67843137254902, 0.690196078431373, 0.690196078431373, 
	0.701960784313725, 0.701960784313725, 0.709803921568627, 
	0.709803921568627, 0.72156862745098, 0.72156862745098, 
	0.729411764705882, 0.729411764705882, 0.741176470588235, 
	0.741176470588235, 0.749019607843137, 0.749019607843137, 
	0.76078431372549, 0.76078431372549, 0.768627450980392, 
	0.768627450980392, 0.780392156862745, 0.780392156862745, 
	0.788235294117647, 0.788235294117647, 0.8, 0.8, 0.811764705882353, 
	0.811764705882353, 0.819607843137255, 0.819607843137255, 
	0.831372549019608, 0.831372549019608, 0.83921568627451, 
	0.83921568627451, 0.850980392156863, 0.850980392156863, 
	0.858823529411765, 0.858823529411765, 0.870588235294118, 
	0.870588235294118, 0.87843137254902, 0.87843137254902, 
	0.890196078431372, 0.890196078431372, 0.898039215686275, 
	0.898039215686275, 0.909803921568627, 0.909803921568627, 
	0.92156862745098, 0.92156862745098, 0.929411764705882, 
	0.929411764705882, 0.941176470588235, 0.941176470588235, 
	0.949019607843137, 0.949019607843137, 0.96078431372549, 
	0.96078431372549, 0.968627450980392, 0.968627450980392, 
	0.980392156862745, 0.980392156862745, 0.988235294117647, 
	0.988235294117647, 1, 1, 0.980392156862745, 1, 1, 0.96078431372549, 
	0.96078431372549, 0.862745098039216, 0.941176470588235, 
	0.941176470588235, 0.901960784313726, 0.901960784313726, 
	0.901960784313726, 0.843137254901961, 0.843137254901961, 
	0.835294117647059, 0.835294117647059, 0.803921568627451, 
	0.803921568627451, 0.768627450980392, 0.725490196078431, 
	0.725490196078431, 0.67843137254902, 0.67843137254902, 
	0.709803921568627, 0.862745098039216, 0.941176470588235, 
	0.803921568627451, 0.803921568627451, 0.933333333333333, 
	0.941176470588235, 0.980392156862745, 0.980392156862745, 1, 1, 1, 
	0.980392156862745, 0.96078431372549, 0.96078431372549, 
	0.882352941176471, 0.882352941176471, 1, 0, 0.309803921568627, 
	0.309803921568627, 0.309803921568627, 0.309803921568627, 
	0.411764705882353, 0.411764705882353, 0.411764705882353, 
	0.411764705882353, 0.564705882352941, 0.564705882352941, 
	0.564705882352941, 0.564705882352941, 0.6, 0.6, 0.6, 0.6, 
	0.752941176470588, 0.752941176470588, 0.827450980392157, 
	0.827450980392157, 0.827450980392157, 0.827450980392157, 
	0.43921568627451, 0.43921568627451, 0.501960784313725, 
	0.501960784313725, 0.501960784313725, 0.929411764705882, 
	0.929411764705882, 0.545098039215686, 0.545098039215686, 
	0.803921568627451, 0.803921568627451, 0.933333333333333, 
	0.933333333333333, 1, 1, 0.803921568627451, 0.803921568627451, 
	0.882352941176471, 0.882352941176471, 1, 1, 1, 1, 1, 0.92156862745098, 
	0.92156862745098, 0.980392156862745, 0.980392156862745, 
	0.705882352941177, 0.705882352941177, 0.870588235294118, 
	0.870588235294118, 0.901960784313726, 0.901960784313726, 
	0.901960784313726, 0.901960784313726, 0.933333333333333, 
	0.933333333333333, 0.819607843137255, 0.819607843137255, 0.8, 0.8, 
	0.815686274509804, 1, 1, 1, 0.627450980392157, 0.627450980392157, 
	0.666666666666667, 0.666666666666667, 0.831372549019608, 0, 0, 
	0.184313725490196, 0.184313725490196, 0.56078431372549, 
	0.56078431372549, 0.341176470588235, 0.341176470588235, 
	0.443137254901961, 0.443137254901961, 0.666666666666667, 
	0.666666666666667, 0.596078431372549, 0.596078431372549, 
	0.498039215686275, 0.498039215686275, 0, 0, 0, 0, 0.603921568627451, 
	0.603921568627451, 0.184313725490196, 0.184313725490196, 
	0.196078431372549, 0.196078431372549, 0.196078431372549, 
	0.196078431372549, 0.133333333333333, 0.133333333333333, 
	0.137254901960784, 0.137254901960784, 0.419607843137255, 
	0.419607843137255, 0.549019607843137, 0.666666666666667, 
	0.666666666666667, 0.823529411764706, 0.823529411764706, 
	0.87843137254902, 0.87843137254902, 0, 0, 0.509803921568627, 
	0.509803921568627, 0.125490196078431, 0.0431372549019608, 
	0.0431372549019608, 0.56078431372549, 0.56078431372549, 
	0.36078431372549, 0.36078431372549, 0.0745098039215686, 
	0.0745098039215686, 0.176470588235294, 0.247058823529412, 
	0.529411764705882, 0.862745098039216, 0.701960784313725, 
	0.376470588235294, 0.376470588235294, 0.549019607843137, 
	0.117647058823529, 0.133333333333333, 0.164705882352941, 
	0.47843137254902, 0.47843137254902, 0.447058823529412, 0.47843137254902,
	0.47843137254902, 0, 0, 0, 0.313725490196078, 0.501960784313725, 
	0.501960784313725, 0.27843137254902, 0, 0, 0, 0.705882352941177, 
	0.705882352941177, 0.576470588235294, 0.576470588235294, 
	0.796078431372549, 0.756862745098039, 0.756862745098039, 
	0.576470588235294, 0.576470588235294, 0.376470588235294, 
	0.52156862745098, 0.52156862745098, 0.564705882352941, 
	0.564705882352941, 1, 0.933333333333333, 0.866666666666667, 
	0.83921568627451, 0.827450980392157, 0.827450980392157, 0.8, 0.8, 
	0.827450980392157, 0.827450980392157, 0.886274509803922, 
	0.886274509803922, 0.941176470588235, 0.858823529411765, 
	0.858823529411765, 0.847058823529412, 0.980392156862745, 
	0.913725490196078, 0.788235294117647, 0.537254901960784, 
	0.933333333333333, 0.870588235294118, 0.749019607843137, 
	0.509803921568627, 0.858823529411765, 0.8, 0.690196078431373, 
	0.470588235294118, 0.768627450980392, 0.717647058823529, 
	0.619607843137255, 0.419607843137255, 0.725490196078431, 
	0.67843137254902, 0.584313725490196, 0.396078431372549, 
	0.67843137254902, 0.631372549019608, 0.545098039215686, 
	0.368627450980392, 0.803921568627451, 0.749019607843137, 
	0.647058823529412, 0.43921568627451, 0.862745098039216, 
	0.803921568627451, 0.694117647058824, 0.470588235294118, 
	0.941176470588235, 0.87843137254902, 0.756862745098039, 
	0.513725490196078, 0.941176470588235, 0.87843137254902, 
	0.756862745098039, 0.513725490196078, 0.96078431372549, 
	0.898039215686275, 0.772549019607843, 0.525490196078431, 
	0.882352941176471, 0.823529411764706, 0.709803921568627, 
	0.482352941176471, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 0.831372549019608, 0.776470588235294, 
	0.666666666666667, 0.454901960784314, 0.756862745098039, 
	0.705882352941177, 0.607843137254902, 0.411764705882353, 
	0.623529411764706, 0.580392156862745, 0.501960784313725, 
	0.341176470588235, 0.603921568627451, 0.564705882352941, 
	0.486274509803922, 0.329411764705882, 0.498039215686275, 
	0.462745098039216, 0.4, 0.270588235294118, 0, 0, 0, 0, 0, 0, 0, 0, 
	0.243137254901961, 0.227450980392157, 0.196078431372549, 
	0.133333333333333, 0.43921568627451, 0.407843137254902, 
	0.352941176470588, 0.23921568627451, 0.56078431372549, 0.52156862745098,
	0.450980392156863, 0.305882352941176, 0.545098039215686, 
	0.509803921568627, 0.43921568627451, 0.298039215686275, 
	0.87843137254902, 0.819607843137255, 0.705882352941177, 
	0.47843137254902, 0, 0, 0, 0, 0, 0, 0, 0, 0.145098039215686, 
	0.133333333333333, 0.113725490196078, 0.0784313725490196, 
	0.0588235294117647, 0.0549019607843137, 0.0470588235294118, 
	0.0313725490196078, 0.756862745098039, 0.705882352941177, 
	0.607843137254902, 0.411764705882353, 0.415686274509804, 
	0.388235294117647, 0.333333333333333, 0.227450980392157, 
	0.27843137254902, 0.258823529411765, 0.223529411764706, 
	0.149019607843137, 0.607843137254902, 0.568627450980392, 
	0.490196078431373, 0.333333333333333, 0.729411764705882, 
	0.682352941176471, 0.588235294117647, 0.4, 0.309803921568627, 
	0.286274509803922, 0.247058823529412, 0.168627450980392, 
	0.141176470588235, 0.129411764705882, 0.113725490196078, 
	0.0745098039215686, 0.188235294117647, 0.172549019607843, 
	0.149019607843137, 0.101960784313725, 0.250980392156863, 
	0.231372549019608, 0.2, 0.137254901960784, 0.411764705882353, 
	0.384313725490196, 0.329411764705882, 0.223529411764706, 
	0.47843137254902, 0.447058823529412, 0.384313725490196, 
	0.258823529411765, 0, 0, 0, 0, 0, 0, 0, 0, 0.337254901960784, 
	0.313725490196078, 0.270588235294118, 0.184313725490196, 
	0.27843137254902, 0.258823529411765, 0.223529411764706, 
	0.149019607843137, 0, 0, 0, 0, 0, 0, 0, 0, 0.576470588235294, 
	0.537254901960784, 0.462745098039216, 0.313725490196078, 
	0.705882352941177, 0.654901960784314, 0.564705882352941, 
	0.384313725490196, 0.772549019607843, 0.72156862745098, 
	0.619607843137255, 0.423529411764706, 0.725490196078431, 
	0.67843137254902, 0.584313725490196, 0.396078431372549, 
	0.670588235294118, 0.623529411764706, 0.537254901960784, 
	0.364705882352941, 0.701960784313725, 0.654901960784314, 
	0.564705882352941, 0.384313725490196, 0.588235294117647, 
	0.549019607843137, 0.470588235294118, 0.32156862745098, 1, 
	0.933333333333333, 0.803921568627451, 0.545098039215686, 
	0.980392156862745, 0.913725490196078, 0.788235294117647, 
	0.537254901960784, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 1, 0.933333333333333, 0.803921568627451, 
	0.545098039215686, 0, 0, 0.0117647058823529, 0.0117647058823529, 
	0.0196078431372549, 0.0196078431372549, 0.0313725490196078, 
	0.0313725490196078, 0.0392156862745098, 0.0392156862745098, 
	0.0509803921568627, 0.0509803921568627, 0.0588235294117647, 
	0.0588235294117647, 0.0705882352941176, 0.0705882352941176, 
	0.0784313725490196, 0.0784313725490196, 0.0901960784313725, 
	0.0901960784313725, 0.101960784313725, 0.101960784313725, 
	0.109803921568627, 0.109803921568627, 0.12156862745098, 
	0.12156862745098, 0.129411764705882, 0.129411764705882, 
	0.141176470588235, 0.141176470588235, 0.149019607843137, 
	0.149019607843137, 0.16078431372549, 0.16078431372549, 
	0.168627450980392, 0.168627450980392, 0.180392156862745, 
	0.180392156862745, 0.188235294117647, 0.188235294117647, 0.2, 0.2, 
	0.211764705882353, 0.211764705882353, 0.219607843137255, 
	0.219607843137255, 0.231372549019608, 0.231372549019608, 
	0.23921568627451, 0.23921568627451, 0.250980392156863, 
	0.250980392156863, 0.258823529411765, 0.258823529411765, 
	0.270588235294118, 0.270588235294118, 0.27843137254902, 
	0.27843137254902, 0.290196078431373, 0.290196078431373, 
	0.301960784313725, 0.301960784313725, 0.309803921568627, 
	0.309803921568627, 0.32156862745098, 0.32156862745098, 
	0.329411764705882, 0.329411764705882, 0.341176470588235, 
	0.341176470588235, 0.349019607843137, 0.349019607843137, 
	0.36078431372549, 0.36078431372549, 0.368627450980392, 
	0.368627450980392, 0.380392156862745, 0.380392156862745, 
	0.388235294117647, 0.388235294117647, 0.4, 0.4, 0.411764705882353, 
	0.411764705882353, 0.419607843137255, 0.419607843137255, 
	0.431372549019608, 0.431372549019608, 0.43921568627451, 
	0.43921568627451, 0.450980392156863, 0.450980392156863, 
	0.458823529411765, 0.458823529411765, 0.470588235294118, 
	0.470588235294118, 0.47843137254902, 0.47843137254902, 
	0.490196078431373, 0.490196078431373, 0.498039215686275, 
	0.498039215686275, 0.509803921568627, 0.509803921568627, 
	0.52156862745098, 0.52156862745098, 0.529411764705882, 
	0.529411764705882, 0.541176470588235, 0.541176470588235, 
	0.549019607843137, 0.549019607843137, 0.56078431372549, 
	0.56078431372549, 0.568627450980392, 0.568627450980392, 
	0.580392156862745, 0.580392156862745, 0.588235294117647, 
	0.588235294117647, 0.6, 0.6, 0.611764705882353, 0.611764705882353, 
	0.619607843137255, 0.619607843137255, 0.631372549019608, 
	0.631372549019608, 0.63921568627451, 0.63921568627451, 
	0.650980392156863, 0.650980392156863, 0.658823529411765, 
	0.658823529411765, 0.670588235294118, 0.670588235294118, 
	0.67843137254902, 0.67843137254902, 0.690196078431373, 
	0.690196078431373, 0.701960784313725, 0.701960784313725, 
	0.709803921568627, 0.709803921568627, 0.72156862745098, 
	0.72156862745098, 0.729411764705882, 0.729411764705882, 
	0.741176470588235, 0.741176470588235, 0.749019607843137, 
	0.749019607843137, 0.76078431372549, 0.76078431372549, 
	0.768627450980392, 0.768627450980392, 0.780392156862745, 
	0.780392156862745, 0.788235294117647, 0.788235294117647, 0.8, 0.8, 
	0.811764705882353, 0.811764705882353, 0.819607843137255, 
	0.819607843137255, 0.831372549019608, 0.831372549019608, 
	0.83921568627451, 0.83921568627451, 0.850980392156863, 
	0.850980392156863, 0.858823529411765, 0.858823529411765, 
	0.870588235294118, 0.870588235294118, 0.87843137254902, 
	0.87843137254902, 0.890196078431372, 0.890196078431372, 
	0.898039215686275, 0.898039215686275, 0.909803921568627, 
	0.909803921568627, 0.92156862745098, 0.92156862745098, 
	0.929411764705882, 0.929411764705882, 0.941176470588235, 
	0.941176470588235, 0.949019607843137, 0.949019607843137, 
	0.96078431372549, 0.96078431372549, 0.968627450980392, 
	0.968627450980392, 0.980392156862745, 0.980392156862745, 
	0.988235294117647, 0.988235294117647, 1, 1), .Dim = c(738, 3), 
	.Dimnames = list(c("snow", "ghost white", "GhostWhite", "white smoke", 
	"WhiteSmoke", "gainsboro", "floral white", "FloralWhite", "old lace", 
	"OldLace", "linen", "antique white", "AntiqueWhite", "papaya whip", 
	"PapayaWhip", "blanched almond", "BlanchedAlmond", "bisque", 
	"peach puff", "PeachPuff", "navajo white", "NavajoWhite", "moccasin", 
	"cornsilk", "ivory", "lemon chiffon", "LemonChiffon", "seashell", 
	"honeydew", "mint cream", "MintCream", "azure", "alice blue", 
	"AliceBlue", "lavender", "lavender blush", "LavenderBlush", 
	"misty rose", "MistyRose", "white", "black", "dark slate gray", 
	"DarkSlateGray", "dark slate grey", "DarkSlateGrey", "dim gray", 
	"DimGray", "dim grey", "DimGrey", "slate gray", "SlateGray", 
	"slate grey", "SlateGrey", "light slate gray", "LightSlateGray", 
	"light slate grey", "LightSlateGrey", "gray", "grey", "light grey", 
	"LightGrey", "light gray", "LightGray", "midnight blue", "MidnightBlue",
	"navy", "navy blue", "NavyBlue", "cornflower blue", "CornflowerBlue", 
	"dark slate blue", "DarkSlateBlue", "slate blue", "SlateBlue", 
	"medium slate blue", "MediumSlateBlue", "light slate blue", 
	"LightSlateBlue", "medium blue", "MediumBlue", "royal blue", 
	"RoyalBlue", "blue", "dodger blue", "DodgerBlue", "deep sky blue", 
	"DeepSkyBlue", "sky blue", "SkyBlue", "light sky blue", "LightSkyBlue", 
	"steel blue", "SteelBlue", "light steel blue", "LightSteelBlue", 
	"light blue", "LightBlue", "powder blue", "PowderBlue", 
	"pale turquoise", "PaleTurquoise", "dark turquoise", "DarkTurquoise", 
	"medium turquoise", "MediumTurquoise", "turquoise", "cyan", 
	"light cyan", "LightCyan", "cadet blue", "CadetBlue", 
	"medium aquamarine", "MediumAquamarine", "aquamarine", "dark green", 
	"DarkGreen", "dark olive green", "DarkOliveGreen", "dark sea green", 
	"DarkSeaGreen", "sea green", "SeaGreen", "medium sea green", 
	"MediumSeaGreen", "light sea green", "LightSeaGreen", "pale green", 
	"PaleGreen", "spring green", "SpringGreen", "lawn green", "LawnGreen", 
	"green", "chartreuse", "medium spring green", "MediumSpringGreen", 
	"green yellow", "GreenYellow", "lime green", "LimeGreen", 
	"yellow green", "YellowGreen", "forest green", "ForestGreen", 
	"olive drab", "OliveDrab", "dark khaki", "DarkKhaki", "khaki", 
	"pale goldenrod", "PaleGoldenrod", "light goldenrod yellow", 
	"LightGoldenrodYellow", "light yellow", "LightYellow", "yellow", "gold",
	"light goldenrod", "LightGoldenrod", "goldenrod", "dark goldenrod", 
	"DarkGoldenrod", "rosy brown", "RosyBrown", "indian red", "IndianRed", 
	"saddle brown", "SaddleBrown", "sienna", "peru", "burlywood", "beige", 
	"wheat", "sandy brown", "SandyBrown", "tan", "chocolate", "firebrick", 
	"brown", "dark salmon", "DarkSalmon", "salmon", "light salmon", 
	"LightSalmon", "orange", "dark orange", "DarkOrange", "coral", 
	"light coral", "LightCoral", "tomato", "orange red", "OrangeRed", "red",
	"hot pink", "HotPink", "deep pink", "DeepPink", "pink", "light pink", 
	"LightPink", "pale violet red", "PaleVioletRed", "maroon", 
	"medium violet red", "MediumVioletRed", "violet red", "VioletRed", 
	"magenta", "violet", "plum", "orchid", "medium orchid", "MediumOrchid", 
	"dark orchid", "DarkOrchid", "dark violet", "DarkViolet", "blue violet",
	"BlueViolet", "purple", "medium purple", "MediumPurple", "thistle", 
	"snow1", "snow2", "snow3", "snow4", "seashell1", "seashell2", 
	"seashell3", "seashell4", "AntiqueWhite1", "AntiqueWhite2", 
	"AntiqueWhite3", "AntiqueWhite4", "bisque1", "bisque2", "bisque3", 
	"bisque4", "PeachPuff1", "PeachPuff2", "PeachPuff3", "PeachPuff4", 
	"NavajoWhite1", "NavajoWhite2", "NavajoWhite3", "NavajoWhite4", 
	"LemonChiffon1", "LemonChiffon2", "LemonChiffon3", "LemonChiffon4", 
	"cornsilk1", "cornsilk2", "cornsilk3", "cornsilk4", "ivory1", "ivory2", 
	"ivory3", "ivory4", "honeydew1", "honeydew2", "honeydew3", "honeydew4", 
	"LavenderBlush1", "LavenderBlush2", "LavenderBlush3", "LavenderBlush4", 
	"MistyRose1", "MistyRose2", "MistyRose3", "MistyRose4", "azure1", 
	"azure2", "azure3", "azure4", "SlateBlue1", "SlateBlue2", "SlateBlue3", 
	"SlateBlue4", "RoyalBlue1", "RoyalBlue2", "RoyalBlue3", "RoyalBlue4", 
	"blue1", "blue2", "blue3", "blue4", "DodgerBlue1", "DodgerBlue2", 
	"DodgerBlue3", "DodgerBlue4", "SteelBlue1", "SteelBlue2", "SteelBlue3", 
	"SteelBlue4", "DeepSkyBlue1", "DeepSkyBlue2", "DeepSkyBlue3", 
	"DeepSkyBlue4", "SkyBlue1", "SkyBlue2", "SkyBlue3", "SkyBlue4", 
	"LightSkyBlue1", "LightSkyBlue2", "LightSkyBlue3", "LightSkyBlue4", 
	"SlateGray1", "SlateGray2", "SlateGray3", "SlateGray4", 
	"LightSteelBlue1", "LightSteelBlue2", "LightSteelBlue3", 
	"LightSteelBlue4", "LightBlue1", "LightBlue2", "LightBlue3", 
	"LightBlue4", "LightCyan1", "LightCyan2", "LightCyan3", "LightCyan4", 
	"PaleTurquoise1", "PaleTurquoise2", "PaleTurquoise3", "PaleTurquoise4", 
	"CadetBlue1", "CadetBlue2", "CadetBlue3", "CadetBlue4", "turquoise1", 
	"turquoise2", "turquoise3", "turquoise4", "cyan1", "cyan2", "cyan3", 
	"cyan4", "DarkSlateGray1", "DarkSlateGray2", "DarkSlateGray3", 
	"DarkSlateGray4", "aquamarine1", "aquamarine2", "aquamarine3", 
	"aquamarine4", "DarkSeaGreen1", "DarkSeaGreen2", "DarkSeaGreen3", 
	"DarkSeaGreen4", "SeaGreen1", "SeaGreen2", "SeaGreen3", "SeaGreen4", 
	"PaleGreen1", "PaleGreen2", "PaleGreen3", "PaleGreen4", "SpringGreen1", 
	"SpringGreen2", "SpringGreen3", "SpringGreen4", "green1", "green2", 
	"green3", "green4", "chartreuse1", "chartreuse2", "chartreuse3", 
	"chartreuse4", "OliveDrab1", "OliveDrab2", "OliveDrab3", "OliveDrab4", 
	"DarkOliveGreen1", "DarkOliveGreen2", "DarkOliveGreen3", 
	"DarkOliveGreen4", "khaki1", "khaki2", "khaki3", "khaki4", 
	"LightGoldenrod1", "LightGoldenrod2", "LightGoldenrod3", 
	"LightGoldenrod4", "LightYellow1", "LightYellow2", "LightYellow3", 
	"LightYellow4", "yellow1", "yellow2", "yellow3", "yellow4", "gold1", 
	"gold2", "gold3", "gold4", "goldenrod1", "goldenrod2", "goldenrod3", 
	"goldenrod4", "DarkGoldenrod1", "DarkGoldenrod2", "DarkGoldenrod3", 
	"DarkGoldenrod4", "RosyBrown1", "RosyBrown2", "RosyBrown3", 
	"RosyBrown4", "IndianRed1", "IndianRed2", "IndianRed3", "IndianRed4", 
	"sienna1", "sienna2", "sienna3", "sienna4", "burlywood1", "burlywood2", 
	"burlywood3", "burlywood4", "wheat1", "wheat2", "wheat3", "wheat4", 
	"tan1", "tan2", "tan3", "tan4", "chocolate1", "chocolate2", 
	"chocolate3", "chocolate4", "firebrick1", "firebrick2", "firebrick3", 
	"firebrick4", "brown1", "brown2", "brown3", "brown4", "salmon1", 
	"salmon2", "salmon3", "salmon4", "LightSalmon1", "LightSalmon2", 
	"LightSalmon3", "LightSalmon4", "orange1", "orange2", "orange3", 
	"orange4", "DarkOrange1", "DarkOrange2", "DarkOrange3", "DarkOrange4", 
	"coral1", "coral2", "coral3", "coral4", "tomato1", "tomato2", "tomato3",
	"tomato4", "OrangeRed1", "OrangeRed2", "OrangeRed3", "OrangeRed4", 
	"red1", "red2", "red3", "red4", "DeepPink1", "DeepPink2", "DeepPink3", 
	"DeepPink4", "HotPink1", "HotPink2", "HotPink3", "HotPink4", "pink1", 
	"pink2", "pink3", "pink4", "LightPink1", "LightPink2", "LightPink3", 
	"LightPink4", "PaleVioletRed1", "PaleVioletRed2", "PaleVioletRed3", 
	"PaleVioletRed4", "maroon1", "maroon2", "maroon3", "maroon4", 
	"VioletRed1", "VioletRed2", "VioletRed3", "VioletRed4", "magenta1", 
	"magenta2", "magenta3", "magenta4", "orchid1", "orchid2", "orchid3", 
	"orchid4", "plum1", "plum2", "plum3", "plum4", "MediumOrchid1", 
	"MediumOrchid2", "MediumOrchid3", "MediumOrchid4", "DarkOrchid1", 
	"DarkOrchid2", "DarkOrchid3", "DarkOrchid4", "purple1", "purple2", 
	"purple3", "purple4", "MediumPurple1", "MediumPurple2", "MediumPurple3",
	"MediumPurple4", "thistle1", "thistle2", "thistle3", "thistle4", 
	"gray0", "grey0", "gray1", "grey1", "gray2", "grey2", "gray3", "grey3", 
	"gray4", "grey4", "gray5", "grey5", "gray6", "grey6", "gray7", "grey7", 
	"gray8", "grey8", "gray9", "grey9", "gray10", "grey10", "gray11", 
	"grey11", "gray12", "grey12", "gray13", "grey13", "gray14", "grey14", 
	"gray15", "grey15", "gray16", "grey16", "gray17", "grey17", "gray18", 
	"grey18", "gray19", "grey19", "gray20", "grey20", "gray21", "grey21", 
	"gray22", "grey22", "gray23", "grey23", "gray24", "grey24", "gray25", 
	"grey25", "gray26", "grey26", "gray27", "grey27", "gray28", "grey28", 
	"gray29", "grey29", "gray30", "grey30", "gray31", "grey31", "gray32", 
	"grey32", "gray33", "grey33", "gray34", "grey34", "gray35", "grey35", 
	"gray36", "grey36", "gray37", "grey37", "gray38", "grey38", "gray39", 
	"grey39", "gray40", "grey40", "gray41", "grey41", "gray42", "grey42", 
	"gray43", "grey43", "gray44", "grey44", "gray45", "grey45", "gray46", 
	"grey46", "gray47", "grey47", "gray48", "grey48", "gray49", "grey49", 
	"gray50", "grey50", "gray51", "grey51", "gray52", "grey52", "gray53", 
	"grey53", "gray54", "grey54", "gray55", "grey55", "gray56", "grey56", 
	"gray57", "grey57", "gray58", "grey58", "gray59", "grey59", "gray60", 
	"grey60", "gray61", "grey61", "gray62", "grey62", "gray63", "grey63", 
	"gray64", "grey64", "gray65", "grey65", "gray66", "grey66", "gray67", 
	"grey67", "gray68", "grey68", "gray69", "grey69", "gray70", "grey70", 
	"gray71", "grey71", "gray72", "grey72", "gray73", "grey73", "gray74", 
	"grey74", "gray75", "grey75", "gray76", "grey76", "gray77", "grey77", 
	"gray78", "grey78", "gray79", "grey79", "gray80", "grey80", "gray81", 
	"grey81", "gray82", "grey82", "gray83", "grey83", "gray84", "grey84", 
	"gray85", "grey85", "gray86", "grey86", "gray87", "grey87", "gray88", 
	"grey88", "gray89", "grey89", "gray90", "grey90", "gray91", "grey91", 
	"gray92", "grey92", "gray93", "grey93", "gray94", "grey94", "gray95", 
	"grey95", "gray96", "grey96", "gray97", "grey97", "gray98", "grey98", 
	"gray99", "grey99", "gray100", "grey100"), c("Red", "Green", "Blue")))

setps <- function(filename, w=0, h=3, pointsize=10, sublines=0, toplines=0,
		type="symbol", lwd=2, font='Helvetica',
		leftlines=0, las=1, 
		trellis=!(missing(setTrellis.) & missing(strip.blank) &
				  missing(lty.dot.line) & missing(lwd.dot.line)), 
		setTrellis.=TRUE, 
		strip.blank = TRUE, lty.dot.line = 1, lwd.dot.line = 1,
		seqno=NULL, color=FALSE) {

  filebase <- if(type=='char') filename else as.character(substitute(filename))
  if(length(seqno)) filebase <- paste(filebase,seqno,sep='')
  filename <- paste(filebase,'.ps',sep='')
  if(length(.Options$setpsPrefix))
    filename <- paste(.Options$setpsPrefix, filename, sep='')

#Changed after submission to s-news: pointsize=NULL
#Antonio likes the default
#ratio of width/height to be the "golden ratio", which is the default.
#I often prefer a smaller ratio of 1.4. If exactly one of (width, height)
#is zero, the "ratio" is used to replace it based on the one specified.
#For a single figure in the plot I usually use psfig(filename,height=3).
#For a single figure in the plot I usually use psfig(filename,height=3).
#The logic in psfig assumes that one figure is being drawn, i.e., that
#par(mfrow=c(1,1)) is in effect. It will work for multiple plots if you
#set pointsize to something like 9.
#sublines specifies the number of extra lines to leave at the bottom of
#the plot for subtitles.
#
#   I include an S function that sets the stage for EPS graphics
#generation that will be incorporated by TeX (LaTeX, etc.), and that
#does a little of what you want, by hand, not in the smart way you
#envision.
#  Note that this function intentionally disallows main titles, with
#the understanding that they will be part of the figure's caption,
#which TeX itself generates. You may like to use it as starting point
#to get something that suits your needs.
#
#  - Antonio Possolo
#
#       Applied Mathematics & Statistics
#       The Boeing Company
#                          antonio@atc.boeing.com
#
#
#Added else scale <-   FEH 8Sep92, also added arg "ratio",
#commented out warning message for omitting main title,
#added arg sublines, pointsize
#may want to specify pointsize=9 if multiple plots used
#added lwd FEH 27Oct92
#added toplines FEH 18Oct93
#override fonts spec to ps.options because of bug - FEH 21Apr94
#added bty="l" FEH 24Aug94
#added leftlines FEH 26Aug94
#added onefile 27Feb95
#maden font default to Helvetica 25Mar00
#Doug Bates just does this:
#a) use postscript(filename, height=xx, width=yy, pointsize=10)
#b) change the figure's region on the page by using
#   par (mar=c(3.5, 3.5, 1.5, 0.5))  ## for example and perhaps also
#   par (mgp=c(2.5, 0.5, 0))
#
  # added color=FALSE 7feb03
psfig <-
function(file = "", width = 0, height = 0, ratio= (1 + sqrt(5))/2,
		 font = 'Helvetica', 
		 pointsize=NULL, sublines=0, 
		 toplines=0, leftlines=0, lwd=0.5, bty="l", onefile=FALSE, 
		 las=NULL, trellis=FALSE, color=FALSE) {

#	POSTSCRIPT FIGURE MAKER
#	for incorporation into TeX using PSFIG or BoxedEPSF.
#	The strategy is to create a pleasant aspect ratio, 
#	while minimizing white space around the figure.
#
# Aspect ratio is Golden Ratio
# Standard width is 30 picas = 30*12/(72.27) inches
	StandardWidth <- (30 * 12)/(72.27)
	StandardHeight <- StandardWidth/ratio
	StandardPointSize <- 9
	if ( width == 0 & height == 0 ) 
		{ 
		width <- StandardWidth
		height <- StandardHeight
		scale <- 1
		}
	if ( width > 0 & height == 0 ) 
		{ 
		height <- width/ratio
		scale <- width/StandardWidth
		}
	if ( width == 0 & height > 0 ) 
		{ 
		width <- height*ratio
		scale <- width/StandardWidth
		}
	else scale <- max(width/StandardWidth,height/StandardHeight)

	if(!length(pointsize)) pointsize <- round(scale * StandardPointSize)
#
#	FONTS & FONT SELECTION
#
#  1 Helvetica               19 Bookman-DemiItalic
#  2 Courier                 20 Bookman-Light
#  3 Times-Roman             21 Bookman-LightItalic
#  4 Helvetica-Oblique       22 Helvetica-Narrow
#  5 Helvetica-Bold          23 Helvetica-Narrow-Bold
#  6 Helvetica-BoldOblique   24 Helvetica-Narrow-BoldOblique
#  7 Courier-Oblique         25 Helvetica-Narrow-Oblique
#  8 Courier-Bold            26 NewCenturySchlbk-Roman
#  9 Courier-BoldOblique     27 NewCenturySchlbk-Bold
# 10 Times-Italic            28 NewCenturySchlbk-Italic
# 11 Times-Bold              29 NewCenturySchlbk-BoldItalic
# 12 Times-BoldItalic        30 Palatino-Roman
# 13 Symbol                  31 Palatino-Bold
# 14 AvantGarde-Book         32 Palatino-Italic
# 15 AvantGarde-BookOblique  33 Palatino-BoldItalic
# 16 AvantGarde-Demi         34 ZapfChancery-MediumItalic
# 17 AvantGarde-DemiOblique  35 ZapfDingbats
# 18 Bookman-Demi           
#
    if(!.R.) {
      ps.fonts <- if(under.unix)ps.options()$fonts else ps.fonts

      if(is.numeric(font)) {  # was is.number 10Apr01
		fontNumber <- font
		if(fontNumber < 1 | fontNumber > length(ps.fonts)) {
          fontNumber <- 1
          cat(paste(
                    "\tPSFIG WARNING: Font requested is not available\n",
                    "\t\tSubstituted by Helvetica\n"))
		}
      }
      else {
		fontName <- font
		fontNumber <- match(fontName, ps.fonts)
		if(is.na(fontNumber)) {
          fontNumber <- 1
          cat(paste(
                    "\tPSFIG WARNING: Font requested is not available\n",
                    "\t\tSubstituted by Helvetica\n"))
		}
      }
	if(under.unix) {
     ## do.call 21Oct99 - problem with lazy eval in unix
	  if(trellis)
        do.call('trellis.device',
                list(device='postscript', file=file,
                     horizontal = FALSE, width = width, height = height,
                     pointsize = pointsize, fonts=ps.fonts[fontNumber],
                     font = 1, maximize=TRUE, onefile=onefile,
                     print.it=FALSE, color=color)) else
	  postscript(file = file, horizontal = FALSE, width = width, height = height,
		pointsize = pointsize, fonts=ps.fonts[fontNumber], font = 1, 
		maximize=TRUE, onefile=onefile, print.it=FALSE)	
        # was font=fontNumber, fonts omitted - bug
	}
	else {
	  if(trellis)
        do.call('trellis.device',
                list(device='postscript',
                     file = file, horizontal = FALSE, width = width,
                     height = height,
                     pointsize = pointsize,
                     fonts=ps.fonts[fontNumber],
                     font = 1, color=color)) else
	  postscript(file = file, horizontal = FALSE, width = width, height = height,
		pointsize = pointsize, fonts=ps.fonts[fontNumber], font = 1)
	}

#
#	PLOT DESIGN
#	Lines are 1pt wide, which is half standard width
#	(LWD is interpreted in units of 1/36 inch
#	 LWD=0 yields the thinnest possible line on the device)
#	Axis labels closer to axes than default 
#	(MGP: margin line for the axis title, axis labels, 
#	 and axis line in units of MEX)
#	Margin widths narrower than default
#	(MAR: bottom, left, top, right)
	if(trellis) return(invisible())
    } else {   # 10Apr01
      if(trellis) do.call('trellis.device',
                          list(device='postscript',
                               file = file, horizontal = FALSE,
                               width = width, height = height,
                               pointsize = pointsize,
                               family=font, color=color,
                               bg=if(!color)'white' else NULL)) else
	  postscript(file = file, horizontal = FALSE, width = width, height = height,
                 pointsize = pointsize, family=font,
                 onefile=onefile, print.it=FALSE)	
    }
    if(.R.) par(lwd=lwd, mgp=c(2.2,.45,0), tcl=-0.4,
                mar=c(3+sublines+.25*(sublines>0),3.5+leftlines,
                      1+toplines,1)+.1, bty=bty) else
    par(lwd=lwd, mgp=c(2,.4,0),
        mar=c(3+sublines+.25*(sublines>0),3+leftlines,
              1+toplines, 1)+.1, bty=bty)
    ## was mgp=c(2, 0.5, 0) 11Jan01  c(2.5,.6,0) R c(2,.4,0) S+ 27jan03
	#	SMO is number of rasters that the piecewise linear
#	approximation to a curve is allowed to differ from the exact
#	position of the curve.
	par(smo = 0)	#	PLOTTING SYMBOL
#	PCH  selects plotting characters from Standard Encoding
#	(PostScript Language Reference Manual, p.252)
#	168 = currency
#	180 = centered period
#	183 = bullet (with a negative font parameter yields a circle)
	par(pch = 1)	# was 183 11Jan01
# MAIN TITLE not allowed: plot will be described in figure caption, 
#	handled by TeX itself.
#	cat(paste("\tPSFIG WARNING:", "Do not use high-level parameter MAIN\n",
#		"\t\tFigure caption should be created within LaTeX\n"))	#
	if(length(las)) par(las=las)
	invisible()
}

  psfig(filename, h=h, w=w, ratio=1.4, 
        pointsize=pointsize,sublines=sublines,toplines=toplines,
        lwd=lwd,font=font,leftlines=leftlines, las=las,
        trellis=trellis, color=color)  # color= 7feb03
          
  if(trellis && setTrellis.) setTrellis(strip.blank = strip.blank, 
     lty.dot.line = lty.dot.line, lwd.dot.line = lwd.dot.line)

	topdf <- function(filebase) {
	  cmd <- if(under.unix)'gs' else 'gswin32c'
	  cmd <- paste(cmd,' -q -dNOPAUSE -dBATCH -sDEVICE#pdfwrite -sOutputFile#',filebase,'.pdf -c save pop -f ',filebase,'.ps',sep='')
	  sys(cmd)
	  invisible()
	}
  formals(topdf) <- list(filebase=filebase)
  storeTemp(topdf)
  invisible()
}


setpdf <- function(filename, w=0, h=4, pointsize=10, sublines=0, toplines=0,
				   type="symbol", lwd=1.5,
                   font=if(.R.)'Helvetica' else 1,
                   ratio= if(.R.) 4/3 else (1 + sqrt(5))/2,
				   leftlines=0, las=1, bty='l', hor=FALSE,
                   trellis=!(missing(setTrellis.) & missing(strip.blank) &
                             missing(lty.dot.line) & missing(lwd.dot.line)), 
                   setTrellis.=TRUE, 
                   strip.blank = TRUE, lty.dot.line = 1, lwd.dot.line =1,
                   region=c(0, 0, h, w), color=FALSE, seqno=NULL, ...) {

  if(type=="char") filename <- paste(filename,seqno,".pdf",sep="") else
  filename <- paste(substitute(filename),seqno,".pdf",sep="")
  if(length(.Options$setpdfPrefix))
    filename <- paste(.Options$setpdfPrefix, filename, sep='')

  if (w > 0 & h == 0) h <- w/ratio
  if (w == 0 & h > 0) w <- h*ratio
  if(.R.) {
    if(trellis) trellis.device('pdf', file=filename, width=w, height=h,
                               pointsize=pointsize, family=font,
                               color=color,onefile=FALSE,
                               bg=ifelse(color,NULL,'white')) else
                pdf(filename, width=w, height=h, pointsize=pointsize,
                    family=font,onefile=FALSE)
  } else {
    if(trellis) trellis.device('pdf.graph', file=filename,
                               horizontal=hor, width=w, height=h,
                               pointsize=pointsize, font=font,
                               region=region,
                               color=color) else
    pdf.graph(filename, horizontal=hor, width=w, height=h,
              pointsize=pointsize, font=font, region=region,
              color=color)
  }
  if(!trellis) {
    if(.R.) par(lwd=lwd, mgp=c(2.2,.45,0), tcl=-0.4,
                mar=c(3+sublines+.25*(sublines>0),3.5+leftlines,
                      1+toplines,1)+.1, bty=bty) else
    par(lwd=lwd, mgp=c(2,.4,0),
        mar=c(3+sublines+.25*(sublines>0),3+leftlines,
              1+toplines, 1)+.1, bty=bty)

    ## was mgp=c(2.5,.6,0) R c(2,.6,0) S+ 27jan03
    par(smo = 0)
  }
  if(length(las)) par(las=las)
  if(trellis && setTrellis.)
    setTrellis(strip.blank = strip.blank, 
               lty.dot.line = lty.dot.line, lwd.dot.line = lwd.dot.line)
  invisible()
}

tex  <-  function(string, lref='c', psref='c', scale=1, srt=0) 
  paste('\\tex[',lref,'][',psref,'][',
        format(scale),'][',format(srt),']{',string,'}',sep='')

showPsfrag <- function(filename) {
  file <- paste(as.character(substitute(filename)),'ps',sep='.')
  out <- "TEMPltx"
  cat('\\documentclass{article}',
      '\\usepackage{graphics}',
      '\\usepackage[scanall]{psfrag}',
      '\\begin{document}',
      paste('\\includegraphics{',file,'}',sep=''),
      '\\end{document}',sep='\n', file=paste(out,'tex',sep='.'))
  sys(paste('latex "\\scrollmode\\input" ',out,';dvips -o ',out,'.ps ',out,
            '; gv ',out,'.ps  &',
            sep=''))
  unlink(paste(out,c('tex','log','dvi','ps','aux','pfg'),sep='.'))
  invisible()
}
pstamp <- if(.R.) function(txt, pwd=FALSE, time.=TRUE) {

    stamp <- function(string, ...) {
      opar <- par(yaxt='s',xaxt='s',xpd=NA)
      on.exit(par(opar))
      plt <- par('plt')
      usr <- par('usr')
      text(usr[2] + diff(usr[1:2])/diff(plt[1:2])*
           (1-plt[2]) - .6*strwidth('m'),
           usr[3] - diff(usr[3:4])/diff(plt[3:4])*plt[3] +
           .6*strheight('m'),
           string, adj=1)
      invisible()
    }

    date.txt <-
      if(time.) format(Sys.time()) else format(Sys.time(), '%Y-%m-%d')
    if(pwd) date.txt <- paste(getwd(), date.txt)

    old <- par(c('mfrow','cex'))
    par(mfrow=c(1,1))
    par(cex=.5)
    if(!missing(txt)) date.txt <- paste(txt,'   ',date.txt, sep='')
    stamp(string=date.txt,print=FALSE,plot=TRUE)
    par(old)
    invisible()

  } else function(txt, pwd=FALSE, time.=under.unix) {

    date.txt <- if(time.) date() else {
      if(.SV4.)
        format(timeDate(date(), in.format='%w %m %d %H:%M:%S %Z %Y',
               format='%Y-%m-%d')) else
      if(under.unix) unix('date +%Y-%m-%d') else
      stop('time.=T not supported')
    }
                 
  if(pwd) {
	if(!under.unix) stop('pwd not supported except with Linux/UNIX')
    pwd <- unix('pwd')
    date.txt <- paste(pwd, date.txt)
  } 
  old <- par(c('mfrow','cex'))
  par(mfrow=c(1,1))
  par(cex=.5)
  if(!missing(txt)) date.txt <- paste(txt,'   ',date.txt, sep='')
  stamp(string=date.txt,print=FALSE,plot=TRUE)
  par(old)
  invisible()
}  
#Computes rank correlation measures between a variable X and a possibly
#censored variable Y, with event/censoring indicator EVENT
#Rank correlation is extension of Somers' Dxy = 2(Concordance Prob-.5)
#See Harrell et al JAMA 1984(?)
#Set outx=T to exclude ties in X from computations (-> Goodman-Kruskal
# gamma-type rank correlation)

rcorr.cens <- function(x, S, outx=FALSE)					{

if(!length(dim(S))) S <- cbind(S, rep(1, length(S)))
y <- S[,1]
event <- S[,2]
if(length(y)!=length(x))stop("y must have same length as x")

miss <- is.na(x) | is.na(y) | is.na(event)
nmiss <- sum(miss)
if(nmiss>0)	{
	miss <- !miss
	x <- x[miss]
	y <- y[miss]
	event <- event[miss]	}
n <- length(x)
ne <- sum(event)
storage.mode(x) <- if(.R.) "double" else "single"
storage.mode(y) <- if(.R.) "double" else "single"
storage.mode(event) <- "logical"

z <- if(.R.)
    .Fortran("cidxcn",x,y,event,length(x),nrel=double(1),nconc=double(1),
	nuncert=double(1),
	c.index=double(1),gamma=double(1),sd=double(1),as.logical(outx),
             PACKAGE="Hmisc") else
  .Fortran("cidxcn",x,y,event,length(x),nrel=double(1),nconc=double(1),
	nuncert=double(1),
	c.index=double(1),gamma=double(1),sd=double(1),as.logical(outx))
r <- c(z$c.index,z$gamma,z$sd,n,nmiss,ne,z$nrel,z$nconc,z$nuncert)
names(r) <- c("C Index","Dxy","S.D.","n","missing","uncensored",
	"Relevant Pairs",
	"Concordant","Uncertain")
r
									}




rcorr <- function(x, y, type=c("pearson","spearman")) {

type <- match.arg(type)

if(!missing(y)) x <- cbind(x, y)
x[is.na(x)] <- 1e30
storage.mode(x) <- if(.R.)"double" else "single"
p <- as.integer(ncol(x))
if(p<1) stop("must have >1 column")
n <- as.integer(nrow(x))
if(n<5) stop("must have >4 observations")

h <- if(.R.)
  .Fortran("rcorr", x, n, p, itype=as.integer(1+(type=="spearman")),
	hmatrix=double(p*p), npair=integer(p*p),
	double(n), double(n),  double(n), double(n),
	double(n), integer(n), PACKAGE="Hmisc") else
  .Fortran("rcorr", x, n, p, itype=as.integer(1+(type=="spearman")),
	hmatrix=single(p*p), npair=integer(p*p),
	single(n), single(n),  single(n), single(n),
	single(n), integer(n))
npair <- matrix(h$npair, ncol=p)
h <- matrix(h$hmatrix, ncol=p)
h[h>1e29] <- NA
nam <- dimnames(x)[[2]]
dimnames(h) <- list(nam, nam)
dimnames(npair) <- list(nam, nam)
P <- matrix(2*(1-pt(abs(h)*sqrt(npair-2)/sqrt(1-h*h), npair-2)),ncol=p)
P[abs(h)==1] <- 0
diag(P) <- NA
dimnames(P) <- list(nam,nam)
structure(list(r=h, n=npair, P=P), class="rcorr")

}


print.rcorr <- function(x, ...) {
print(round(x$r,2))
n <- x$n
if(all(n==n[1,1])) cat("\nn=",n[1,1],"\n\n") else {
  cat("\nn\n")
  print(n)
}
cat("\nP\n")
P <- x$P
P <- ifelse(P<.0001,0,P)
p <- format(round(P,4))
p[is.na(P)] <- ""
print(p, quote=FALSE)
invisible()
}

spearman2 <- function(x, ...) UseMethod("spearman2") 

spearman2.default <- function(x, y, p=1, minlev=0,
                              exclude.imputed=TRUE, ...) {
  if(p > 2) stop('p must be 1 or 2')
  if(exclude.imputed) im <- is.imputed(x) | is.imputed(y)
  y <- as.numeric(y)
  if(is.character(x)) x <- factor(x)
  s <- !(is.na(x) | is.na(y))
  if(exclude.imputed) s <- s & !im
  n <- sum(s)
  ## 28Apr99:
  if(n < 3) return(c(rho2=NA,F=NA,df1=0,df2=n,P=NA,n=n,'Adjusted rho2'=NA))
  x <- x[s]; y <- y[s]
  u <- length(unique(x))
  if(is.category(x) && u > 2) {
    if(minlev > 0) {
  		x <- combine.levels(x, minlev)
		if(length(levels(x))<2) {
          warning(paste('x did not have >= 2 categories with >=',
                        mlev,'of the observations'))
          return(c(rho2=NA,F=NA,df1=0,df2=n,P=NA,n=n,'Adjusted rho2'=NA))
        }
      }
    x <- model.matrix(~x, data=data.frame(x))
    p <- ncol(x)-1
    rsquare <- lm.fit.qr.bare(x, rank(y), intercept=FALSE)$rsquared
  } else {
    x <- as.numeric(x)
    if(u < 3) p <- 1
    x <- rank(x)
    rsquare <- if(p==1) cor(x, rank(y))^2 else {
      x <- cbind(x, x^2)
      lm.fit.qr.bare(x, rank(y), intercept=TRUE)$rsquared
    }
  }
  df2 <- n-p-1
  fstat <- rsquare/p/((1-rsquare)/df2)
  pvalue <- 1-pf(fstat,p,df2)
  rsqa <- 1 - (1 - rsquare)*(n-1)/df2
  
  x <- c(rsquare,fstat,p,df2,pvalue,n,rsqa)
  names(x) <- c("rho2","F","df1","df2","P","n","Adjusted rho2")
  x
}

spearman2.formula <- function(x, p=1, data, subset, na.action,
                              minlev=0, exclude.imputed=TRUE, ...) {
  call <- match.call()
  nact <- NULL
  y <- match.call(expand=FALSE)
  y$formula <- x
  y$x <- y$p <- y$minlev <- y$exclude.imputed <- y$... <- NULL
  if(missing(na.action)) y$na.action <- na.retain
  y[[1]] <- as.name("model.frame")
  ##See if Des argument exists in current model.frame.default 3aug02
  ##if(length(model.frame.default$Des)) y$Des  <- FALSE   #turn off Design
  x <- eval(y, sys.parent())
  nam <- names(x)
  y <- x[[1]]
  w <- t(sapply(x[-1], spearman2, y=y, minlev=minlev, p=p,
                exclude.imputed=exclude.imputed))
  dimnames(w)[[2]] <- c("rho2","F","df1","df2","P","n","Adjusted rho2")
  structure(w, class='spearman2.formula', yname=names(x)[1])
}

print.spearman2.formula <- function(x, ...) {
  cat('\nSpearman rho^2    Response variable:',attr(x,'yname'),'\n\n')
  dig <- c(3,2,0,0,4,0,3)
  for(i in 1:7) x[,i] <- round(x[,i],dig[i])
  attr(x,'yname') <- oldClass(x) <- NULL
  print(x)
  invisible()
}

plot.spearman2.formula <- function(x,
                                   what=c('Adjusted rho2','rho2','P'),
                                   sort.=TRUE, main, xlab, ...) {
  what <- match.arg(what)
  if(missing(xlab)) xlab <- switch(what,
                                   'Adjusted rho2'=
                                     if(.R.)expression(Adjusted~rho^2)
                                         else 'Adjusted rho^2',
                                   'rho2'=if(.R.)expression(rho^2)
                                          else 'rho^2',
                                   'P'='P-value')
  if(missing(main))
    main <- if(.R.) parse(text=paste('paste(Spearman,~rho^2,~~~~Response:',
                            attr(x,'yname'),')',sep='')) else
      paste('Spearman rho^2    Response variable:',attr(x,'yname'))
  if(.SV4.) x <- matrix(oldUnclass(x), nrow=nrow(x),
                          dimnames=dimnames(x)) ## 19Nov00
  ## SV4 doesn't consider a matrix with extra attributes as a matrix
  aux <- paste(x[,'n'],x[,'df1'])
  stat <- x[,what]
  if(sort.) {
    i <- order(stat)
    stat <- stat[i]
    aux <- aux[i]
  }
  dotchart2(stat, auxdata=aux, reset.par=TRUE,
            xlab=xlab, auxtitle=c('N  df'),
            main=main, ...)
  invisible()
}

#Computes rank correlation measures between a variable X and a possibly
#censored Surv variable Y
#Rank correlation is extension of Somers' Dxy = 2(Concordance Prob-.5)
#See Harrell et al JAMA 1984(?)
#Set outx=T to exclude ties in X from computations (-> Goodman-Kruskal
# gamma-type rank correlation)
#No. This is the version extended to paired predictions
#method=1: concordance=delta x1 < delta x2
#method=2: concordance=x1 concordant and x2 discordant

rcorrp.cens <- function(x1, x2, S, outx=FALSE, method=1) {

if(!length(dim(S))) S <- cbind(S, rep(1, length(S)))
y <- S[,1]
event <- S[,2]

if(length(x1)!=length(x2))stop("x1 and x3 must have same length")
if(length(y)!=length(x1))stop("y must have same length as x")
if(method!=1 & method!=2)stop("method must be 1 or 2")

miss <- is.na(x1+x2+y+event)
nmiss <- sum(miss)
if(nmiss>0)			{
	miss <- !miss
	x1 <- x1[miss]
	x2 <- x2[miss]
	y <- y[miss]
	event <- event[miss]	}
n <- length(x1)
if(n<2)stop("<2 non-missing observations")
ne <- sum(event)
storage.mode(x1) <- if(.R.)"double" else "single"
storage.mode(x2) <- if(.R.)"double" else "single"
storage.mode(y) <- if(.R.)"double" else "single"
storage.mode(event) <- "logical"
storage.mode(method) <- "integer"
storage.mode(outx) <- "logical"

z <- if(.R.)
  .Fortran("cidxcp",x1,x2,y,event,length(x1),method,outx,
	nrel=double(1),nuncert=double(1),
	c1=double(1),c2=double(1),gamma1=double(1),gamma2=double(1),
	gamma=double(1),sd=double(1),c12=double(1),c21=double(1),
           PACKAGE="Hmisc") else
  .Fortran("cidxcp",x1,x2,y,event,length(x1),method,outx,
	nrel=double(1),nuncert=double(1),
	c1=double(1),c2=double(1),gamma1=double(1),gamma2=double(1),
	gamma=double(1),sd=double(1),c12=double(1),c21=double(1))

r <- c(z$gamma,z$sd,z$c12,z$c21,n,nmiss,ne,z$nrel,z$nuncert,z$c1,z$c2,
	z$gamma1,z$gamma2)
names(r) <- c("Dxy","S.D.","x1 more concordant","x2 more concordant",
	"n","missing","uncensored",
	"Relevant Pairs","Uncertain","C X1","C X2","Dxy X1","Dxy X2")
r
}


#rcspline.eval - function to create design matrix for restricted cubic
#	spline function of Stone & Koo, given an input vector and optionally
#	a vector of knots.  If knots are not given, knots are set using
#	default algorithm.  If the number of knots is not given, 5 are used.
#	Terms are normalized by (outer-inner knot)^2.
#	Can optionally return antiderivative of spline functions if
#	type="integral".
#	norm=0 : no normalization of constructed variables
#	norm=1 : divide by cube of difference in last 2 knots
#		 makes all variables unitless
#	norm=2 : (default) divide by square of difference in outer knots
#		 makes all variables in original units of x
#
#	Returns:
#		x - design matrix for derived spline variables
#		(includes original x in first column if inclx=T or 
#		 type="integral")
#		attribute knots - input or derived vector of knots
#	If knots.only=T, returns instead the vector of estimated or given
#	knots.
#	If rpm is not null, replaces missing x with rpm before evaluating
#	but after estimating knots.
#
#	F. Harrell 13 Feb 90
#       Modified   28 Mar 90 - improved default knot computation
#		   22 Aug 90 - put knots as attribute, return matrix
#		   20 Sep 90 - added knots.only argument
#		   16 Oct 90 - added rpm argument
#		   11 Dec 91 - added type argument
#		   27 Dec 91 - added norm argument
#		   26 Jun 93 - added evasive action if <3 knots
#        
rcspline.eval <- function(x,knots,nk=5,inclx=FALSE,knots.only=FALSE,
	type="ordinary",norm=2, rpm=NULL) 				{

if(missing(knots)) {
	xx <- x[!is.na(x)]
	n <- length(xx)
	if(n<6)stop('fewer than 6 non-missing observations with knots omitted')
	if(nk<3) stop('nk must be >= 3')
	outer <- .1
	if(nk>3) outer <- .05
	if(nk>6) outer <- .025
	knots <- quantile(xx,seq(outer,1.0-outer,length=nk))
	if(length(unique(knots))<3)		{
	   knots <- quantile(xx,seq(outer,1.0-outer,length=2*nk))
	   if((nu <- length(unique(knots)))<3)	{
		cat("Fewer than 3 unique knots.  Frequency table of variable:\n")
		print(table(xx))
		stop()			}
	   warning(paste("could not obtain",nk,"knots with default algorithm.\n",
		"Used alternate algorithm to obtain",
		nu,"knots"))
						}
	if(n<100) {
		xx <- sort(xx)
		knots[1]<-xx[5]
		knots[nk]<-xx[n-4] }
					}
knots <- sort(unique(knots))
nk <- length(knots)
if(nk<3)	{
   cat("fewer than 3 unique knots.  Frequency table of variable:\n")
   print(table(x))
   stop()	}

if(knots.only) return(knots)

#x <- as.matrix(x)     10Mar01
#storage.mode(x) <- "single"
if(!is.null(rpm)) x[is.na(x)] <- rpm
xx <- matrix(1.1,length(x),nk-2)  # 10Mar01
knot1 <- knots[1]
knotnk <- knots[nk]
knotnk1 <- knots[nk-1]
if(norm==0) kd <- 1
else if(norm==1) kd <- knotnk-knotnk1
else kd <- (knotnk-knot1)^.66666666666666666666666

if(type=="integral") power <- 4 else power <- 3

for(j in 1:(nk-2))	{
	xx[,j]<-pmax((x-knots[j])/kd,0)^power + 
		((knotnk1-knots[j])*pmax((x-knotnk)/kd,0)^power -
		(knotnk-knots[j])*(pmax((x-knotnk1)/kd,0)^power))/
		(knotnk-knotnk1)
		 	}

if(power==4) xx <- cbind(x, x*x/2, xx*kd/4)
else if(inclx) xx <- cbind(x, xx)
if(!.R.) storage.mode(xx) <- 'single'  # 10Mar01
attr(xx,"knots") <- knots

xx
						}

		
#Mod rep(1,n)-> rep(1,length(xe)) 1 Jul 91
rcspline.plot <- function(x,y,model="logistic",xrange,
	event,nk=5,knots=NULL,show="xbeta",adj=NULL,xlab,ylim,
	plim=c(0,1),plotcl=TRUE,showknots=TRUE,add=FALSE,subset,lty=1,noprint=FALSE,
	m,smooth=FALSE,bass=1,main="auto",statloc) {
if(!(model=="logistic"|model=="cox"|model=="ols"))
	stop('model must be "logistic", "cox", or "ols"')
if(!(show=="xbeta"|show=="prob"))stop('show must be "xbeta" or "prob"')
if(!missing(event))model<-"cox"
if(model=="cox" & missing(event))stop('event must be given for model="cox"')
if(show=="prob" & !missing(adj))stop('show="prob" cannot be used with adj')
if(show=="prob" & model!="logistic")
	stop('show="prob" can only be used with model="logistic"')
if(length(x)!=length(y))stop('x and y must have the same length')
if(!missing(event) && length(event)!=length(y))
	stop('y and event must have the same length')
if(!missing(adj))	{
	if(!is.matrix(adj)) adj <- as.matrix(adj)
	if(dim(adj)[1]!=length(x))stop('x and adj must have the same length') }
if(missing(xlab))xlab <- label(x)
if(missing(ylab))ylab <- label(y)
isna <- is.na(x) | is.na(y) 
if(!missing(event)) isna <- isna | is.na(event)
nadj <- 0
if(!missing(adj))  {
	nadj <- ncol(adj)
	isna <- isna | apply(is.na(adj),1,sum)>0 }
if(!missing(subset))isna <- isna | (!subset)
x <- x[!isna]
y <- y[!isna]
if(!missing(event)) event <- event[!isna]
if(!missing(adj)) adj <- adj[!isna,]
n <- length(x)
if(n<6)stop('fewer than 6 non-missing observations')
if(missing(xrange)) {
	frac<-10./max(n,200)
	xrange<-quantile(x,c(frac,1.-frac))	}
if(missing(knots)) xx <- rcspline.eval(x,nk=nk)
else xx <- rcspline.eval(x,knots)
knots <- attr(xx,"knots")
nk <- length(knots)

df1 <- nk-2
if(model=="logistic") {
	b <- lrm.fit(cbind(x,xx,adj),y)
#	b <- glim(cbind(x,xx,adj),y,rep(1,n),error="binomial",
#		link="logit")
#	if(!noprint)glim.print(b)
	beta <- b$coef
	cov <- b$var
#	model.lr <- b$deviance[1] - b$deviance[2]
	model.lr <- b$stats["Model L.R."]
	offset <- 1 	#to skip over intercept parameter
	ylabl <- if(show=="prob") "Probability" else "log Odds"
	sampled <- paste("Logistic Regression Model, n=",n," d=",sum(y),sep="")
									}
if(model=="cox") {
	lllin <- coxph.fit(cbind(x,adj),cbind(y,event),strata=NULL,
		offset=NULL, init=NULL, iter.max=10, eps=.0001, 
		method="efron", rownames=NULL)$loglik[2]
	b <- coxph.fit(cbind(x,xx,adj),cbind(y,event),strata=NULL,
		offset=NULL, init=NULL, iter.max=10, eps=.0001, 
		method="efron", rownames=NULL)
	beta <- b$coef
	if(!noprint) {print(beta); print(b$loglik)}
	beta <- b$coef
	cov <- b$var
	model.lr<-2*(b$loglik[2]-b$loglik[1])
	offset <- 0
	ylabl <- "log Relative Hazard"
	sampled <- paste("Cox Regression Model, n=",n," events=",sum(event),
		sep="")
									}
if(model=="logistic"|model=="cox")	{
	model.df <- nk-1+nadj
	model.aic <- model.lr-2.*model.df
	v <- solve(cov[(1+offset):(nk+offset-1),(1+offset):(nk+offset-1)])
	assoc.chi <- beta[(1+offset):(nk+offset-1)] %*% v %*%
		beta[(1+offset):(nk+offset-1)]
	assoc.df <- nk-1   #attr(v,"rank")
	assoc.p <- 1.-pchisq(assoc.chi,nk-1)
	v <- solve(cov[(2+offset):(nk+offset-1),(2+offset):(nk+offset-1)])
	linear.chi <- beta[(2+offset):(nk+offset-1)] %*% v %*%
		beta[(2+offset):(nk+offset-1)]
	linear.df <- nk-2   #attr(v,"rank")
	linear.p <- 1.-pchisq(linear.chi,linear.df)
	if(nadj>0) {
		ntot <- offset+nk-1+nadj
		v <- solve(cov[(nk+offset):ntot,(nk+offset):ntot])
	 	adj.chi <- beta[(nk+offset):ntot] %*% v %*%
			beta[(nk+offset):ntot]
	 	adj.df <- attr(v,"rank")
	 	adj.p <- 1.-pchisq(adj.chi,adj.df)	}
	 else	{
		adj.chi <- 0
		adj.p <- 0	}
							}

#Evaluate xbeta for expanded x at desired range
xe <- seq(xrange[1],xrange[2],length=600)
if(model=="cox")xx <- rcspline.eval(xe,knots,inclx=TRUE)
else xx<- cbind(rep(1,length(xe)),rcspline.eval(xe,knots,inclx=TRUE))
xbeta <- xx %*% beta[1:(nk-1+offset)]
var <- drop(((xx %*% cov[1:(nk-1+offset),1:(nk-1+offset)])*xx) %*% 
	rep(1,ncol(xx)))
lower <- xbeta-1.96*sqrt(var)
upper <- xbeta+1.96*sqrt(var)
if(show=="prob") {
	xbeta <- 1./(1.+exp(-xbeta))
	lower <- 1./(1.+exp(-lower))
	upper <- 1./(1.+exp(-upper))
						}
xlim <- range(pretty(xe))
if(missing(ylim))ylim <- range(pretty(xbeta))
if(main=="auto")	{
	if(show=="xbeta")main <- "Estimated Spline Transformation"
	else main <- "Spline Estimate of Prob{Y=1}"	}
if(!interactive() & missing(statloc))statloc<-"ll"
if(!add)	{
oldmar<-par("mar")
if(!missing(statloc) && statloc[1]=="ll")oldmar[1]<-11
oldpar <- par(err=-1,mar=oldmar)
plot(xe,xbeta,type="n",main=main,xlab=xlab,ylab=ylabl,
	main=main,xlim=xlim,ylim=ylim)
lines(xe,xbeta,lty=lty)
ltext<-function(z,line,label,cex=.8,adj=0){
	zz<-z
	zz$y<-z$y-(line-1)*1.2*cex*par("csi")*(par("usr")[4]-par("usr")[3])/
		(par("fin")[2])   #was 1.85
	text(zz,label,cex=cex,adj=adj)}
sl<-0
if(missing(statloc)){
	cat("Click left mouse button at upper left corner for statistics\n")
	z<-locator(1)
	statloc<-"l"	}
else if(statloc[1]!="none")	{
	if(statloc[1]=="ll")	{
		z<-list(x=par("usr")[1],y=par("usr")[3])
		sl<-3		}
	else z<-list(x=statloc[1],y=statloc[2]) 		  }	
if(statloc[1]!="none" & (model=="logistic" | model=="cox"))	{
 	rnd <- function(x,r=2)as.single(round(x,r))
	ltext(z,1+sl,sampled)
	ltext(z,2+sl,"    Statistic        X2  df")
	chistats<-format(as.single(round(c(model.lr,model.aic,
		assoc.chi,linear.chi,adj.chi),2)))
	pvals<-format(as.single(round(c(assoc.p,linear.p,adj.p),4)))
	ltext(z,3+sl,paste("Model        L.R. ",chistats[1],model.df,
		" AIC=",chistats[2]))
	ltext(z,4+sl,paste("Association  Wald ",chistats[3],assoc.df,
		" p= ",pvals[1]))
	ltext(z,5+sl,paste("Linearity    Wald ",chistats[4],linear.df,
		" p= ",pvals[2]))
	if(nadj>0)ltext(z,6+sl,paste("Adjustment   Wald " ,chistats[5],
		adj.df," p= ",pvals[3]))}
						}
else lines(xe,xbeta,lty=lty)
if(plotcl)  {
	lines(xe,lower,lty=2)
	lines(xe,upper,lty=2)	
		}

if(showknots) {
	bot.arrow <- par("usr")[3]
	top.arrow <- bot.arrow+.05*(par("usr")[4]-par("usr")[3])
	for(i in 1:nk)
		arrows(knots[i],top.arrow,knots[i],bot.arrow,rel=TRUE,size=.5)
								}
if(model=="logistic" & nadj==0)			{
	if(smooth)		{
		z<-supsmu(x,y,bass=bass)
		if(show=="xbeta")z$y <- logb(z$y/(1.-z$y))
		points(z,cex=.4)	}
	if(!missing(m))		{
		z<-groupn(x,y,m=m)
		if(show=="xbeta")z$y <- logb(z$y/(1.-z$y))
		points(z,pch=2,mkh=.05)}
						}
if(!add)par(oldpar)
invisible(list(knots=knots,x=xe,xbeta=xbeta,lower=lower,upper=upper))
								}



rcspline.restate <- function(knots, coef, type=c("ordinary","integral"),
			x="X", lx=nchar(x),norm=2, 
			columns=65, before="& &", after="\\", 
			begin="", nbegin=0, digits=max(8,.Options$digits)) {


type <- match.arg(type)
k <- length(knots)
if(k<3) stop("must have >=3 knots in a restricted cubic spline")
p <- length(coef)
if(p == k)	{
   Intc <- coef[1]
   coef <- coef[-1]
   p <- p-1
   }
else Intc <- 0
if(k-1 != p) stop("coef must be of length # knots - 1")

knotnk <- knots[k]; knotnk1 <- knots[k-1]; knot1 <- knots[1]
if(norm==0) kd <- 1 else
if(norm==1) kd <- (knotnk-knotnk1)^3 else
kd <- (knotnk-knot1)^2
coef[-1] <- coef[-1]/kd

d <- c(0, knots-knotnk)[1:p]
coefk <- sum(coef*d)/(knotnk-knotnk1)

d <- c(0, knots-knotnk1)[1:p]
coefk1 <- sum(coef*d)/(knotnk1-knotnk)

if(is.null(names(coef)))names(coef) <- paste(x,1:length(coef),sep="")
coef <- c(coef, coefk, coefk1)
names(coef)[k] <- "1st restricted coef"
names(coef)[k+1] <- "2nd restricted coef"

if(type=="integral") coef <- c(.5*coef[1],.25*coef[-1])

cof <- format.sep(coef, digits)
kn <- format.sep(-knots, digits)
if(Intc!=0)	{
  txt <- txt2 <- format.sep(Intc, digits)
  if(type=="integral") {txt <- paste(txt, "* x")
			txt2 <- paste(txt2, '*', x)}
  if(coef[1]>=0) {txt <- paste(txt, "+"); txt2 <- paste(txt2,'+')}
  }
else txt <- txt2 <- ""

if(cof[1]!=0) {txt <- paste(txt,cof[1],if(type=="ordinary")"* x" else
	"* x^2", sep="")
		txt2 <- paste(txt2,cof[1],if(type=="ordinary")paste("*",x) else
			paste("*",x,"^2"),sep="")
		}
for(i in 2:(p+2))	{
   nam <- paste("pmax(x", if(knots[i-1]<0) "+" else NULL, 
		if(knots[i-1]!=0) kn[i-1] else NULL,
		",0)^", if(type=="ordinary")"3" else "4", sep="")
   nam2 <- paste("pmax(",x,if(knots[i-1]<0) "+" else NULL,
		if(knots[i-1]!=0) kn[i-1] else NULL,
		",0)^", if(type=="ordinary")"3" else "4", sep="")
   z <- paste(if(coef[i]>0 & (i>2 | coef[1]!=0 | Intc!=0)) "+" else NULL,
		cof[i], "*", nam, sep="")
   z2 <- paste(if(coef[i]>0 & (i>2 | coef[1]!=0 | Intc!=0)) "+" else NULL,
		cof[i], "*", nam2, sep="")
   txt <- paste(txt , z,  sep="")
   txt2<- paste(txt2, z2, sep="")
   }

#func <- function(x) NULL
#func[[2]] <- parse(text=txt)[[1]]
func <- parse(text=paste('function(x)', txt))  ## 11Apr02

cof <- format.sep(coef, digits)
kn <- format.sep(-knots, digits)

lcof <- nchar(cof)
#cof <- sys('sed -e "s/e+00//" -e "s/e-0\\(.\\)/\\\\!\\\\times\\\\!10^{-\\1}/" 
#-e "s/e-\\(..\\)/\\\\!\\\\times\\\\!10^{-\\1}/" 
#-e "s/e+0\\(.\\)/\\\\!\\\\times\\\\!10^{\\1}/" 
#-e "s/e+\\(..\\)/\\\\!\\\\times\\\\!10^{\\1}/"', cof)
cof <- sedit(cof, c('e+00','e-0*',                'e-*',
                    'e+0*',               'e+*'),
                  c('',    '\\\!\\times\\\!10^{-*}','\\\!\\times\\\!10^{-*}',
                    '\\\!\\times\\\!10^{*}','\\\!\\times\\\!10^{*}'))
cur <- begin; colcnt <- nbegin; tex <- NULL
if(Intc!=0)	{
   fint <- format.sep(Intc, digits)
   if(type=="integral") { fint <- paste(fint, x); colcnt <- colcnt+2 }
   cur <- paste(cur, fint, sep="")
   colcnt <- colcnt + nchar(fint)
   if(coef[1]>0) { cur <- paste(cur, " + ", sep=""); colcnt <- colcnt+3 }
   }
if(coef[1]!=0)	{
#   sp <- if(length(grep("times",cof[1]))==0) "\\:" else NULL
   sp <- if(substring.location(cof[1],"times")$first > 0) "\\:" else NULL
   cur <- paste(cur, cof[1], sp, x, if(type=="integral") "^2",sep="")  
  #\:=medium space in LaTeX
   colcnt <- colcnt+lcof[1]+lx+(type=="integral")
		}

tex.names <- character(p+2)
size <- lx+lcof[-1]+nchar(kn)+3

for(i in 2:(p+2)) 			{
   nam <- paste("(", x, if(knots[i-1]<0) "+" else NULL,
	if(knots[i-1]!=0) kn[i-1] else NULL, 
	")_{+}^{", if(type=="ordinary")"3}" else "4}", sep="")
   q <- paste(if(coef[i]>0 & (i>2 | coef[1]!=0 | Intc!=0)) "+" else NULL,
	      cof[i], nam, sep="")
   n <- size[i-1]
   if(colcnt+n > columns)	{
      tex <- c(tex, cur)
      cur <- ""
      colcnt <- 0		}
   cur <- paste(cur, q, sep="")
   colcnt <- colcnt+n
					}

tex <- c(tex, cur)
tex <- paste(before, tex, after)

if(Intc!=0) coef <- c(Intercept=Intc, coef)

attr(coef, "knots") <- knots
attr(coef, "function") <- func
attr(coef, "function.text") <- txt2
#attr(tex, "class")  <- "TeX"
attr(coef, "latex")   <- tex
names(colcnt) <- NULL
attr(coef, "columns.used") <- colcnt


coef									}
reShape <- function(x, ..., id, colvar, base, reps,
                    times=1:reps, timevar='seqno') {
  if(!missing(base)) {
    if(!is.list(x))
      stop('x must be a list or data frame when base is given')
    repvars <- as.vector(outer(base,1:reps,paste,sep=''))
    nam <- names(x)
    nonrep <- nam[nam %nin% repvars]
    res <- vector('list', 1+length(nonrep)+length(base))
    names(res) <- c(timevar, nonrep, base)
    x1 <- x[[1]]
    n <- if(is.matrix(x1)) nrow(x1) else length(x1)
    res[[1]] <- rep(times[1:reps], n)

    for(i in nonrep) res[[i]] <- rep(x[[i]], rep(reps,n))

    ## Get indexes that will put unlist() in right order
    k <- as.vector(matrix(1:(reps*n), nrow=reps, byrow=TRUE))
    for(i in base) {
      bn <- paste(i, 1:reps, sep='')
      x1 <- x[[bn[1]]]
      at <- attributes(x1)
      at$names <- NULL
      x1 <- unlist(x[bn])[k]
      if(length(at)) attributes(x1) <- at
      res[[i]] <- x1
    }
    
    if(is.data.frame(x)) {
      rn <- attr(x,'row.names')
      ln <- length(rn)
      if(ln) {
        ## R calls data.frame even if specify structure, and R does
        ## not have dup.row.names argument to data.frame as does S+
        if(.R.) return(data.frame(res,
            row.names=paste(rep(rn,rep(reps,ln)), rep(1:reps,n)))) else
        return(structure(res, class='data.frame',
                         row.names=rep(rn,rep(reps,ln))))
      }
    }
    return(res)
  }
    
  if(is.matrix(x)) {
	y <- as.vector(x)
	v1 <- all.is.numeric(dimnames(x)[[1]][row(x)],'vector')
    v2 <- all.is.numeric(dimnames(x)[[2]][col(x)],'vector')
	w <- list(v1, v2, y)
	names(w) <- c('rowvar','colvar',as.character(substitute(x)))
	if(length(nd <- names(dimnames(x)))) names(w)[1:2] <- nd
	w

  } else {
	id <- as.factor(id)
	colvar <- as.factor(colvar)
	m <- matrix(NA, nrow=length(levels(id)), ncol=length(levels(colvar)),
				dimnames=list(levels(id), levels(colvar)))
	dotlist <- list(...)
	if(!length(dotlist)) {
	  m[cbind(id, colvar)] <- x
	  m
	} else {
	res <- vector('list',nx <- 1+length(dotlist))
	names(res) <- (as.character(sys.call())[-1])[1:nx]
	nam2 <- names(sys.call()[-1])[1:nx]
	if(length(nam2)) names(res) <- ifelse(nam2=='',names(res),nam2)
	w <- m; w[cbind(id, colvar)] <- x; res[[1]] <- w
	for(j in 2:nx) {
	  w <- m; w[cbind(id, colvar)] <- dotlist[[j-1]]
	  res[[j]] <- w
	}
	res
  }
}
}

recode <- function(..., ret=c('numeric','factor'),
                   none=if(ret=='numeric')0 else 'none',na) {

ret <- match.arg(ret)
w <- list(...)

#alternative form: recode(x, from, to), e.g. recode(x, c(1,3), c(0,1))
if(!is.logical(w[[1]]) && length(w)==3) {
  z <- w[[3]][match(w[[1]],w[[2]])]
  if(!missing(none)) z[if(is.numeric(none))is.na(z) else z==''] <- none
  return(z)
}

nam <- names(w)
#.Options$warn <- -1   6Aug00
#numnam <- as.numeric(nam)
#if(missing(ret)) ret <- if(any(is.na(numnam))) 'factor' else 'numeric'
if(missing(ret)) ret <- if(all.is.numeric(nam))'numeric' else 'factor'

result <- rep(none, length(w[[1]]))

for(i in 1:length(w)) result[w[[i]]] <- if(ret=='numeric') numnam[i] else nam[i]

if(ret=='factor') result <- as.factor(result)
if(!missing(na)) result[is.na(na)] <- NA
result
}

rm.boot <- function(time, y, id=seq(along=time), subset=TRUE,
					plot.individual=FALSE,
					bootstrap.type=c('x fixed','x random'),
					nk=6, knots, B=500, smoother=supsmu, 
					xlab, xlim, ylim=range(y), 
					times=seq(min(time),max(time),length=100),
					absorb.subject.effects=FALSE, rho=0,
					cor.pattern=c('independent','estimate'), ncor=10000,
					...) {

bootstrap.type <- match.arg(bootstrap.type)
absorb.subject.effects <- absorb.subject.effects & !missing(id)
if(!is.function(cor.pattern)) cor.pattern <- match.arg(cor.pattern)
if(!(is.character(cor.pattern) && cor.pattern=='independent') && 
   rho!=0) stop("can't specify both cor.pattern='estimate' and rho")
if(rho != 0) cor.pattern <- 'equal correlation'
dodep <- rho !=0 || !is.character(cor.pattern) || cor.pattern=='estimate'

## X fixed also implies that subjects are fixed

id <- as.character(id)
ylab <- label(y)
if(ylab=='') ylab <- 'y'
if(missing(xlab)) {
  xlab <- units(time)
  if(xlab=='') xlab <- 'Time'
}

if(length(subset) > 1) {
  id <- id[subset]; time <- time[subset]; y <- y[subset] }

s <- is.na(time + y)
if(any(s)) {
  s <- !s
  id <- id[s]
  time <- time[s]
  y <- y[s]
}
## Need to order data so that a subject's records stay together
## Otherwise, the mean residuals at each time will not vary over resamples
## when bootstrap.type='x fixed'

s <- order(id, time)
id <- id[s]; time <- time[s]; y <- y[s]

if(bootstrap.type=='x fixed' && diff(range(table(id))) != 0) 
  warning('To work properly with bootstrap.type="x fixed" all subjects must have the same # observations')

n <- length(y)

clusters <- unique(id)

if(plot.individual) {
  ploti <- function(time, y, id, clusters, xlim, ylim, xlab, ylab, 
					smoother, ...) {
	plot(0,0,xlim=range(pretty(range(time))),ylim=ylim,
		 xlab=xlab, ylab=ylab, type='n')
	j <- 0
	for(i in clusters) {
	  s <- id==i
	  j <- j+1
	  lines(smoother(time[s],y[s],...),lty=j)
	}
  }
ploti(time, y, id, clusters, xlim, ylim, xlab, ylab, smoother, ...)
}

if(nk==0) knots <- double(0)
if(missing(knots) && nk>0) {
  knots <- rcspline.eval(time,nk=nk,knots.only=TRUE)
  if(length(knots) != nk) {
	warning('could not obtain requested number of knots')
	nk <- length(knots) 
  }
}
else nk <- length(knots)
p <- if(nk==0) 1 else nk-1

X.times <- if(nk==0) as.matrix(times) else rcspline.eval(times, knots, inclx=TRUE)

X.Time <- if(nk==0) as.matrix(time) else rcspline.eval(time, knots, inclx=TRUE)
X <- if(missing(id)) cbind(X.Time,1) else 
  model.matrix(~ X.Time+id-1,
               data=list(X.Time=X.Time,id=as.factor(id)))
## was id=id 3Apr02   Thanks: Don MacQueen, for R

f <- lm.fit.qr.bare(X, y, intercept=FALSE)
res <- f$residuals
sigma2 <- sum(res^2)/n

if(absorb.subject.effects) {
  mean.intercept <- mean(c(0,f$coef[-(1:p)]))
  y <- y + mean.intercept - (f$coef[-(1:p)])[paste('id',id,sep='')]
  if(plot.individual) {
	ploti(time, y, id, clusters, xlim, ylim, xlab, ylab, smoother, ...)
	title('Raw Data Adjusted to Have a Common Intercept')
  }
}

if(is.character(cor.pattern) && cor.pattern=='estimate') {
  timediff <- product <- single(ncor)
  used <- 0
  i <- 0
  meanres <- tapply(res, time, mean)
  sdres   <- sqrt(tapply(res, time, var))
  if(any(is.na(sdres))) stop('one or more times occur in only one subject')

  for(wid in clusters) {
	s <- id==wid
	x <- time[s]
	cx <- as.character(x)
	r <- (res[s] - meanres[cx])/sdres[cx]
	if(any(is.na(r))) stop('program logic error')
	diffs <- outer(x, x, FUN=function(a,b)abs(a-b))
	prods <- outer(r, r, FUN='*')
	np <- length(prods)
	if(used + np > ncor) {
	  cat('\nUsed only',i,'subjects in estimating covariance pattern.\nMay want to increase ncor.\n')
	  break
	}
	i <- i+1
	timediff[(used+1):(used+np)] <- diffs
	product[(used+1):(used+np)]  <- prods
	used <- used+np
  }
timediff <- timediff[1:used]; product <- product[1:used]
product <- tapply(product, round(timediff,4), mean)
timediff <- as.numeric(names(product))
product[timediff==0] <- 1
plot(timediff, product, xlab='Absolute Difference in Time',
	 ylab='Correlation', type='b')

cor.pattern <- list(x=timediff, y=product)
}

##Subject effects are at the end, using cell means model
##Take intercept as average of all subject effects
cof <- function(fit,p) {
  ko <- fit$coef
  c(mean(ko[-(1:p)]), ko[1:p])
}

o.coef   <- cof(f,p)

if(bootstrap.type=='x random') {
  orig.obsno <- split(1:n, id)
} else {
  R    <- split(res, id)
  yhat <- if(!absorb.subject.effects) f$fitted.values else
                                   o.coef[1] + X.Time %*% o.coef[-1]
}

Coef <- matrix(NA, B+1, p+1)
sse  <- loglik <- single(B+1)
loglik.dep <- NULL

Coef[1,]  <- o.coef
sse[1]    <- sigma2*n
loglik[1] <- n*logb(2*pi*sigma2) + n

if(dodep) {
  loglik.dep <- loglik
  lldep <- function(time, id, sigma2, res, rho, cor.pattern) {
	ll <- 0
	for(subj in unique(id)) {
	  s  <- id==subj
	  x  <- time[s]
	  y  <- res[s]
	  p  <- sum(s)
	  if(is.character(cor.pattern) && cor.pattern=='equal correlation')
		cov <- sigma2*(diag(rep(1-rho,p))+rho) else {
		  cov <- if(is.function(cor.pattern)) 
			outer(x, x, cor.pattern)*sigma2 else {
			  timediff <- outer(x, x, function(a,b)abs(a-b))
			  matrix(approx(cor.pattern, xout=timediff)$y, nrow=p)*sigma2
			}
		}
	  ## Following code taken from dmvnorm()
	  eS <- eigen(cov, sym = TRUE)
	##  y <- y %*% (eS$vectors * rep(1/sqrt(eS$values), each = p)) 24Feb02
 	  y <- y %*% (eS$vectors * rep(1/sqrt(eS$values),
                                   rep(p,length(eS$values))))
	  logl <- sum(y^2) + p*logb(2*pi) + logb(prod(eS$values))
	  ll <- ll + logl
	}
	ll
  }
loglik.dep[1] <- lldep(time, id, sigma2, res, rho, cor.pattern)
}

uneven    <- 0

for(i in 1:B) {
  if(i %% 10 ==0) cat(i,'')
  pts <- sample(clusters, rep=TRUE)

  if(bootstrap.type=='x random') {
	obsn <- unlist(orig.obsno[pts])
	idb <- id[obsn]

	xt <- X.Time[obsn,,drop=FALSE]
	f.b <- lm.fit.qr.bare(if(absorb.subject.effects || missing(id)) 
						  cbind(xt,1) else 
					 model.matrix(~xt+idb-1,
                                  data=list(xt=xt,idb=as.factor(idb))),
                          y[obsn], intercept=FALSE)
    ## was idb=idb 3Apr02
  } else {

	rr <- unlist(R[pts])
	lrr <- length(rr)
	uneven <- max(uneven, abs(lrr-n))
	if(lrr > n) rr <- rr[1:n] else
	if(lrr < n) rr <- c(rr, sample(rr, n-lrr, rep=TRUE))
	yb.e <- yhat + rr
	f.b   <- if(absorb.subject.effects) 
	  lm.fit.qr.bare(cbind(X.Time,1), yb.e, intercept=FALSE) else
	  lm.fit.qr.bare(X, yb.e, intercept=FALSE)
  }

cofb <- cof(f.b, p)   #26Jun97

  pred <- if(bootstrap.type=='x fixed') {
	if(!absorb.subject.effects) X %*% f.b$coefficients else
	cofb[1] + X.Time %*% cofb[-1]
  } else cofb[1] + X.Time %*% cofb[-1]
## x random case may only work properly if absorb.subject.effects, as
## we have to ignore the original subject ids anyway (the bootstrap
## sample in general won't represent all subjects)
  Coef[i+1,]  <- cofb    #26Jun97
  sse[i+1]    <- sum((y-pred)^2)
  sigma2      <- sum(f.b$residuals^2)/length(f.b$residuals)
  loglik[i+1] <-  n*logb(2*pi*sigma2) + sse[i+1]/sigma2
  if(dodep) loglik.dep[i+1] <- 
	lldep(time, id, sigma2, y-pred, rho, cor.pattern)
}

if(uneven>0) warning(paste('Subjects had unequal number of records.\nMaximum discrepency between total number of bootstrap records sampled and original\nnumber of records (',n,') is ',uneven,'. Bootstrap estimates are approximate.',
						   sep=''))

if(dodep) {
  srho <- spearman(loglik, loglik.dep)
  cat('\n\nSpearman rank correlation between',B+1,'log likelihoods assuming independence and assuming dependence:',round(srho,3),'\n')
}

storage.mode(Coef) <- 'single'
storage.mode(sse)  <- 'single'
structure(list(Coef=Coef, sse=sse, loglik=loglik, loglik.dep=loglik.dep,
			   times=times, X.times=X.times,
			   xlab=xlab, ylab=ylab, ylim=ylim, 
			   bootstrap.type=bootstrap.type, fit=f, knots=knots, 
			   rho=rho, cor.pattern=cor.pattern), 
		  class='rm.boot')
}

plot.rm.boot <-
  function(x, obj2, conf.int=.95,
           xlab=obj$xlab, ylab=obj$ylab, xlim, ylim=obj$ylim,
           individual.boot=FALSE,
           pointwise.band=FALSE,
           curves.in.simultaneous.band=FALSE,
           col.pointwise.band=2,
           objective=c('-2 log L','sse','dep -2 log L'), 
           add=FALSE, ncurves,
           multi=FALSE, multi.method=c('color','density'),
           multi.conf=c(.05,.1,.2,.3,.4,.5,.6,.7,.8,.9,.95,.99),
           multi.density=c(-1,90,80,70,60,50,40,30,20,10, 7,4),
           multi.col =c( 1, 8,20, 5, 2, 7,15,13,10,11,9,14),
           subtitles=TRUE, ...) {
  ##	2 was between 5 and 7, 17 was between 8 and 20

obj <- x
objective <- match.arg(objective)
if(missing(objective)) objective <- 
  if(obj$rho==0 && is.character(obj$cor.pattern))'-2 log L' else 'dep -2 log L'

sse   <- switch(objective, 
				sse            = obj$sse,
				'-2 log L'     = obj$loglik,
				'dep -2 log L' = obj$loglik.dep)

B     <- length(sse)
Coef  <- obj$Coef
times <- obj$times

if(!missing(obj2)) {
  if((length(times) != length(obj2$times)) || 
	 (any(times != obj2$times, na.rm=TRUE)))
	stop('times vector must be identical for both rm.boot objects')
  times <- ifelse(is.na(times), NA, obj2$times)
  sse <- sse + obj2$sse
  if(missing(ylab)) ylab <- paste(obj$ylab,'-',obj2$ylab)
}

## order from best -2 log likelihood or sum of squared errors to worst
i <- order(sse)
## Select best confidence coefficient*B estimates
conf <- if(multi) max(multi.conf) else conf.int
i <- i[1:round(conf*B)]
if(i[1] != 1) warning(paste(
	  'design is imbalanced enough that best log likelihood or SSE was not\n',
	  'obtained from overall fit (objective=',format(sse[1]),') but from\n',
	  'a bootstrap fit (objective=',format(sse[i[1]]),')\nThis can also happen if the objective is not -2 log L',sep=''))

## Evaluate all fits on time grid and compute point by point max and min

curves <- cbind(1,obj$X.times) %*% t(Coef)
if(!missing(obj2)) {
  curves <- curves - cbind(1,obj2$X.times) %*% t(obj2$Coef)
  if(missing(ylim)) ylim <- range(curves[,i])
}			

if(multi) {
  multi.method <- match.arg(multi.method)
  if(missing(xlim)) plot(times, curves[,1], type='n',
						 xlab=xlab, ylab=ylab, ylim=ylim) else
  plot(times, curves[,1], type='n',
	   xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim)
  title(paste('Simultaneous',min(multi.conf),'-',max(multi.conf),
		'Confidence Regions'))
  high.prev <- low.prev <- curves[,1]
  for(j in 1:length(multi.conf)) {
	ii <- i[1:round(multi.conf[j]*B)]
	high <- apply(curves[,ii], 1, max)
	low  <- apply(curves[,ii], 1, min)
	if(multi.method=='density') {
	polygon(c(times,rev(times)), c(high.prev,rev(high)), 
			density=multi.density[j])
	polygon(c(times,rev(times)), c(low.prev, rev(low)),  
			density=multi.density[j])
  } else {
	polygon(c(times,rev(times)), c(high.prev,rev(high)), 
			col=multi.col[j])
	polygon(c(times,rev(times)), c(low.prev, rev(low)),  
			col=multi.col[j])
  }
	high.prev <- high; low.prev <- low
  }
  lines(times, curves[,1], lwd=2, col=0)  ## point estimates in white
} else {
  if(add) lines(times, curves[,1]) else {
	if(missing(xlim)) plot(times, curves[,1], type='l',
						   xlab=xlab, ylab=ylab, ylim=ylim) else
	plot(times, curves[,1], type='l',
		 xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim)
	title(paste('Simultaneous',conf.int,'Confidence Region'))
  }

  high <- apply(curves[,i], 1, max)
  low  <- apply(curves[,i], 1, min)
  lines(times, high, lty=2)
  lines(times, low,  lty=2)
  
}
result <- list(times=times, fitted=curves[,1], lower=low, upper=high)

if(individual.boot || curves.in.simultaneous.band) {
  subs <- if(individual.boot) 1:B else i
  if(!missing(ncurves)) subs <- sample(subs, ncurves)
  for(j in subs) lines(times, curves[,j], lty=2)
}

if(pointwise.band) {
  p <- apply(curves, 1, quantile, probs=c((1-conf.int)/2,1-(1-conf.int)/2))
  lines(times,p[1,],col=col.pointwise.band)
  lines(times,p[2,],col=col.pointwise.band)
  result <- c(result, list(pointwise.lower=p[1,], pointwise.upper=p[2,]))
}
if(!add && subtitles) {
  title(sub=obj$bootstrap.type,adj=1)
  title(sub=paste(B-1,'bootstrap repetitions'),adj=0)
}

invisible(result)
}





#Rick Chappell <> Asst. Professor, Depts. of Statistics and Human Oncology
#<> University of Wisconsin at Madison <> chappell@stat.wisc.edu
#(608) 263-5572 / 262-2733 <> take logs

samplesize.bin <- function(alpha, beta, pit, pic, rho=.5) {

# alpha is the scalar ONE-SIDED test size, or two-sided size/2
# beta is a scalar or vector of powers
# pit is the hypothesized treatment probability of success
# pic is the hypothesized control probability of success
# returns required TOTAL sample size, using arcsin transformation
# rho is the proportion of the sample devoted to treated group (0 <rho < 1) 

as <- function(x) { asin(sqrt(x))}
invas <- function(x) {(sin(x))**2}

Zalpha <- qnorm(1-alpha)
Zbeta  <- qnorm(beta)
n <- Zalpha + Zbeta
n <- n/(as(pit) - as(pic))
n <- (n**2)/(4*rho*(1-rho))
round(n+.5,0)                                       }

sas.get <- if(under.unix || .R.)
  function(library, member, variables = character(0), 
					ifs = character(0), 
					format.library = library, id, 
					dates. = c("sas","yymmdd","yearfrac","yearfrac2"), 
					keep.log = TRUE, log.file = "_temp_.log", 
					macro = sas.get.macro,
					data.frame.out = existsFunction("data.frame"), 
					clean.up = !.R.,
					quiet = FALSE, temp = tempfile("SaS"), 
					formats=TRUE, recode=formats, 
					special.miss=FALSE, sasprog="sas", as.is=.5, check.unique.id=TRUE,
					force.single=FALSE,
					where, uncompress=FALSE)
{
  if(.R. && force.single) stop('force.single does not work under R')
  dates. <- match.arg(dates.)

  file.exists <- if(.R.) function(name) {
    w <- file.access(name)==0
    attr(w, 'which') <- name[w]
    w
  } else function(name)
	{  # 22Oct00
	  n <- length(name)
	  w <- logical(n)
	  for(i in 1:n) 
		w[i] <- sys(paste("test -f", name[i], "-o -d", name[i])) == 0
	  which <- character(0)
	  if(any(w)) which <- name[w]
	  attr(w, 'which') <- which
	  w
	}
  file.is.dir <- if(.R.) function(name) !is.na(file.info(name)$isdir) else
  function(name) sys(paste("test -d", name)) == 0

  file.is.readable <- if(.R.) function(name) file.access(name,4)==0
  else function(name) sys(paste("test -r", name)) == 0

  fileShow <- if(.R.) function(x) file.show(x) else page(filename=x)

  if(recode) formats <- TRUE

  if(missing(formats) || formats) {
	## *****  Next line begins mod from Mike Kattan edits 11 Sep 97
	## Redone FEH 22Oct00
	no.format <- all(!file.exists(paste(format.library,
	 c('formats.sc2','formats.sct','formats.sct01','formats.sas7bcat'),
	 sep='/')))
	if(no.format) {
	  if((!missing(formats) && formats) || (!missing(recode) && recode))
		warning(paste(paste(format.library, 
	       "/formats.sc? or formats.sas7bcat",sep = ""), 
					  " not found. Formatting ignored. \n"))
	  formats <- recode <- FALSE
	}
	## ***** End Mike Kattan edits 11 Sep 97
  }
# 5 Changes here from Claudie Berger <claudie@osteo1.ri.mgh.mcgill.ca> 19feb00
# Allows work on sas v7.
  sasin   <- paste(temp, ".3.sas", sep = "")
  sasout1 <- paste(temp, ".1.sas", sep = "")
  sasout2 <- paste(temp, ".2.sas", sep = "")
  sasout3 <- paste(temp, ".4.sas", sep = "")
  sasout4 <- paste(temp, ".5.sas", sep = "")
  nvariables <- length(variables)
  if(nvariables>0)	{
	if(any(jdup <- duplicated(variables)))
	  stop(paste("duplicate variables requested: ", variables[jdup]))
  }
  varstring <- paste(variables, collapse = "\n ")
  ifs <- paste("'",paste(ifs, collapse = ";\n "),"'",sep="")
  if(length(sasin) != 1)
	stop("Illegal temporary file name")
  temp.files <- c(sasin, sasout1, sasout2, sasout3, sasout4)
  if(!keep.log)
	temp.files <- c(temp.files, log.file)
  if(clean.up) on.exit(unlink(temp.files))
##	on.exit(sys(paste("rm -f", paste(temp.files, collapse = " ")))) 4oct03
  if(missing(member))
	stop("SAS member name is required")
  if(missing(library))
	stop("SAS library name is required")
  cat(macro, sep="\n", file=sasin)

  sasds.suffix <- c('sd2','sd7','ssd01','ssd02','ssd03','ssd04','sas7bdat') 
  ## 22Oct00

  if(library == "") {
	if(uncompress) {  # 22Oct00
	  unix.file <- paste(member, sasds.suffix, sep=".")
	  if(any(fe <- file.exists(paste(unix.file,".gz",sep=""))))
		sys(paste("gunzip ",attr(fe,'which'),'.gz',sep='')) else
	  if(any(fe <- file.exists(paste(unix.file,".Z",sep=""))))
		sys(paste("uncompress ",attr(fe,'which'),'.Z',sep=''))
	}
	cat("%sas_get(", member, " ,", sasout1, " ,", sasout2,
		" ,", sasout3, " ,", sasout4,
		", dates=", dates., ", vars=", varstring, ", ifs=",
		ifs, ", formats=", as.integer(formats),
		", specmiss=", as.integer(special.miss),
		");\n", file = sasin, append = TRUE, sep = "")
  }
	else {
	  if(!file.is.dir(library))
		stop(paste(sep = "", "library, \"", library, 
				   "\", is not a Unix directory"))
	  unix.file <- paste(library, "/", member, ".", sasds.suffix,
                         sep='')
      ##23Nov00
	  if(uncompress) {  #22Oct00
		if(any(fe <- file.exists(paste(unix.file,".gz",sep=""))))
		  sys(paste("gunzip ", attr(fe,'which'),'.gz',sep='')) else
		if(any(fe <- file.exists(paste(unix.file,".Z",sep=""))))
		  sys(paste("uncompress ",attr(fe,'which'),'.Z',sep=''))
	  }
	  if(!any(fe <- file.exists(unix.file))) {
		stop(paste(sep = "", "Unix file, \"",
			paste(unix.file,collapse=' '), 
				   "\", does not exist"))
	  }
		else {
		 file.name <- attr(fe,'which')
		 if(!file.is.readable(file.name)) {
		  stop(paste(sep = "", 
					 "You do not have read permission for Unix file, \"",
					 file.name, "\""))   # 22Oct00
		 }
		}
	  cat("libname temp '", library, "';\n", file = sasin, append = TRUE,
		  sep = "")	#
										# format.library should contain formats.sct containing user defined
										# formats used by this dataset.  It must be present.
	  cat("libname library '", format.library, "';\n", file = sasin,
		  append = TRUE, sep = "")
	  cat("%sas_get(temp.", member, " ,", sasout1, " ,", sasout2,
		  " ,", sasout3,", ",sasout4,
		  ", dates=", dates., ", vars=", varstring, ", ifs=",
		  ifs, ", formats=",as.integer(formats),
		  ", specmiss=", as.integer(special.miss),
		  ");\n", file = sasin, append = TRUE, sep = "")
	}
  status <- sys(paste(sasprog, sasin, "-log", log.file))
  if(status != 0) {
	if(!quiet) fileShow(log.file)  ## 4oct03
	stop(paste("SAS job failed with status", status))
  }
										#
										# Read in the variable information
										#
  if(!(file.exists(sasout1) && file.exists(sasout2))) {
	if(!quiet) fileShow(log.file)  ## 4oct03
	stop("SAS output files not found")
  }
  vars <- if(.R.) scan(sasout1, list(name = "", type = 0, length = 0,
                                     format = "", label = "", n = 0),
                       multi.line = FALSE, sep = "\022",
                       flush=TRUE, comment.char='', quote='') else
  scan(sasout1, list(name = "", type = 0, length = 0, format = "",
                     label = "", n = 0),
       multi.line = FALSE, flush=TRUE, sep = "\022")
## Thanks Don MacQueen for scan fix for R
  nvar <- length(vars$name)
  if(nvar == 0) {
	if(!quiet) fileShow(log.file)  ## 4oct03
	stop("First SAS output is empty")
  }
  nrow <- vars$n[1]	#n is the same for each variable
  ##
  ## Read the data in
  ##  We try to be clever about the variable type.  If SAS is character
  ##  use char of course.  If is numeric and length >4, use double.  If
  ##  numeric and length <4, use single.  We could also use the format to
  ##  choose further, if it consists of a number followed by a "."
  ##  can we safely assume integer.
  ##
  type <- ifelse(vars$type == 2, "character(nrow)", 
				 ifelse(force.single | (vars$length < 5 & !.R.),  ##28Mar01
						"single(nrow)", "double(nrow)"))	#
  ##BILL: I corrected the macro so the following isn't needed:
  ## get rid of trailing blank on names
  ##	vars$name <- unix("sed 's/ $//'", vars$name)
  inlist <- paste("\"", vars$name, "\"=", type, sep = "", collapse = ", "
				  )
  inlist <- parse(text = paste("list(", inlist, ")"))	#
  ## Inlist would now be the size of the final data structure, if I had
  ## evaluated it.
  ##
  ## Read the data
  ds <- if(.R.) scan(sasout2, eval(inlist), sep = "\022", multi.line = FALSE,
             flush=TRUE, comment.char='', quote='') else
    scan(sasout2, eval(inlist), sep = "\022", multi.line = FALSE,
             flush=TRUE)
  if(length(ds) < nvariables) {
	m <- variables[is.na(match(variables, names(ds)))]
	if(length(m) > 0) {
	  warning(paste(length(m), 
					"requested variables did not exist:", 
					paste("\"", m, "\"", sep = "", collapse = " "), 
					"\n\t(use sas.contents())"))
	}
  }

  format <- vars$format
  format[format=='$'] <- ' '    # 1Mar00
  label <- vars$label
  name <- vars$name
  esasout3 <- formats && file.exists(sasout3)   #added formats && 1/20/93
  if(recode && !esasout3) recode <- FALSE
  FORMATS <- NULL

  if(formats && esasout3) 	{
	FORMATS <- dget(sasout3)
	if(length(FORMATS)==0) {FORMATS <- NULL; recode <- FALSE}	
  }
  smiss <- NULL
  if(special.miss && file.exists(sasout4))
	smiss <- if(.R.) scan(sasout4, list(name="", code="", obs=integer(1)),
                          multi.line=FALSE, flush=TRUE, sep="\022",
                          comment.char='', quote='') else
      scan(sasout4, list(name="", code="", obs=integer(1)),
				  multi.line=FALSE, flush=TRUE, sep="\022")
  sasdateform <- c("date","mmddyy","yymmdd","ddmmyy","yyq","monyy",
				   "julian","qtr","weekdate","weekdatx","weekday","month")
  dateform <- 	
	list(as.name("ddmmmyy"),"m/d/y","y/m/d","d/m/y",as.name("ddmmmyy"),
		 "mon year",as.name("ddmmmyy"),"mon",as.name("ddmmmyy"),
		 as.name("ddmmmyy"), as.name("ddmmmyy"),"m")
  sastimeform <- c("hhmm","hour","mmss","time")
  timeform <- c("h:m","h","m:s","h:m:s")
  sasdatetimeform <- c("datetime","tod")
  datetimeform <- list(list(as.name("ddmmmyy"),"h:m:s"), c("m/d/y"," "))
  z <- "%02d%b%Y"
  dateform4 <-
    c(z,"%02m/%02d/%Y","%Y/%02m/%02d","%02d/%02m/%Y", z,"%02m %Y",
      z,"%02m", z, z, z,"%02m")
  timeform4 <- c("%02H:%02M","%02H","%02M:%02S","%02H:%02M:%02S")
  datetimeform4 <- c("%02d%b%Y %02h:%02m:%02s","%02m/%02d/%Y")

  if(.R.) {   ## Don MacQueen
    days.to.adj <- as.numeric(difftime(ISOdate(1970,1,1,0,0,0) , 
                                       ISOdate(1960,1,1,0,0,0), 'days'))
    secs.to.adj <- days.to.adj*24*60*60
  }

  for(i in 1:nvar) {
	atr <- list()
	dsi <- ds[[i]]
	fname <- format[i]
	rec <- FALSE
	if(fname!=" ") {
	  ff <- fname
	  if(dates.=="sas" & (m <- match(fname,sasdateform,0)) >0) {
										#look for partial dates
		dd <- dsi-floor(dsi)
		ddn <- !is.na(dd)
		if(any(ddn) && any(dd[ddn]!=0))
		  {
			ll <- 1:length(dd)
			atr$partial.date <- 
			  list(month=ll[dd==.5],day=ll[dd==.25],both=ll[dd==.75])
			atr$imputed <- ll[dd!=0]
			dsi <- floor(dsi)
		  }
		dsi <- if(.R.) {
          ## Don MacQueen 3Apr02
          tmpd <- structure((dsi-days.to.adj)*24*60*60,
                            class=c('POSIXt','POSIXct'))
          as.POSIXct(format(tmpd,tz='GMT'),tz='') } else
        if(.SV4.) timeDate(julian=dsi,format=dateform4[m]) else
          dates(dsi, out.format=dateform[[m]])
		if(length(atr$imputed)) 
	     attr(dsi,'class') <- c("impute",attr(dsi,'class'))
		ff <- NULL
	  }
		else {
		  if((m <- match(fname,sastimeform,0)) >0) {
            dsi <- if(.R.) {
              ## Don MacQueen 3Apr02
              tmpd <- structure(dsi, class=c('POSIXt','POSIXct'))
              tmpd <- as.POSIXct(format(tmpd,tz='GMT'),tz='')
              structure(tmpd, class=c('timePOSIXt','POSIXt','POSIXct'))
            }  else
            if(.SV4.) timeDate(ms=dsi*1000, format=timeform4[m]) else
			 chron(times=dsi/86400, out.format=timeform[m])
			ff <- NULL					
		  }
			else if((m <- match(fname,sasdatetimeform,0))>0) {
			  dsi <- if(.R.) {  ## D.M. 3Apr02
                tmpd <-	structure(dsi-secs.to.adj,
                                  class=c('POSIXt','POSIXct'))
                as.POSIXct(format(tmpd,tz='GMT'),tz='')
              } else
                if(.SV4.) timeDate(julian=dsi/86400,
                                        format=datetimeform4[m]) else
                chron(dsi/86400, out.format=datetimeform[[m]])
			  ff <- NULL					
			}
		}
	  atr$format <- ff
	  if(recode & length(g <- FORMATS[[fname]])) {
		labs <- g$labels
		if(!is.logical(recode)) {
		  labs <- if(recode==1) paste(g$values,":",labs,sep="") else
		  paste(labs,"(",g$values,")",sep="")	}
		dsi <- factor(dsi, g$values, labs)
		atr$sas.codes <- g$values
		rec <- TRUE
	  }   
	}
	if(data.frame.out && !rec && vars$type[i]==2 &&
	   ((is.logical(as.is) && !as.is) || 
		(is.numeric(as.is) && length(unique(dsi)) < as.is*length(dsi))))
	  dsi <- factor(dsi, exclude="") #exclude added 5Mar93
	## For data frames, char. var usually factors
	if(label[i]!=" ") label(dsi) <- label[i]  #atr$label <- label[i]
	if(length(smiss$name))	{
	  j <- smiss$name==name[i]
	  if(any(j)) {
		atr$special.miss <- 
		  list(codes=smiss$code[j],obs=smiss$obs[j])
		attr(dsi,'class') <- c("special.miss",attr(dsi,'class'))
	  }
	}
	if(!is.null(atr)) attributes(dsi) <- c(attributes(dsi),atr)
	if(missing(where)) ds[[i]] <- dsi else
	assign(name[i], dsi, where=where)				
  }

  if(!missing(where)) return(structure(where, class="where"))

  atr <- list()

  if(missing(id))	{
	if(data.frame.out) atr$row.names <- as.character(1:nrow)
  }
	else {
	  idname <- id 
	  jj <- match(idname, names(ds), 0)
	  if(any(jj==0))stop(paste(
			   "id variable(s) not in dataset:",
			   paste(idname[jj==0],collapse=" ")))
	  if(length(idname)==1) {
		id <- ds[[idname]] #Need since not use data.frame
	  }   else {		 
		id <- as.character(ds[[idname[1]]])
		for(jj in 2:length(idname))
		  id <- paste(id, as.character(ds[[idname[jj]]]))
	  }
	  if(check.unique.id) {
		dup <- duplicated(id)
		if(any(dup)) warning(paste("duplicate IDs:",
								   paste(id[dup], collapse=" ")))
	  }
	  if(data.frame.out) atr$row.names <- as.character(id)
		else atr$id <- id	
	}
  if(!is.null(FORMATS)) atr$formats <- FORMATS
  if(data.frame.out) atr$class <- "data.frame"
  attributes(ds) <- c(attributes(ds),atr)
  ds
} else function(library=".", member, variables = character(0), 
	ifs = character(0), 
	format.library = library, id, sasout, 
	keep.log = TRUE, log.file = "_temp_.log", macro = sas.get.macro,
	clean.up = TRUE, formats=TRUE, recode=formats, 
	special.miss=FALSE, sasprog="sas", as.is=.5, check.unique.id=TRUE,
	force.single=FALSE, where, unzip=FALSE)
{
  if(force.single && .R.) stop('force.single does not work under R')
if(recode) formats <- TRUE

sasran <- !missing(sasout)

if(sasran) {
  if(missing(library)+missing(member)+missing(variables)+
	missing(ifs)+missing(format.library)+missing(keep.log)+
	missing(log.file)+missing(formats)+
	missing(special.miss)+missing(sasprog)+
	missing(unzip) != 11)
	  stop('when sasout is given you may not specify options telling SAS how to run')
  if(length(sasout)==1) {
    dos(paste('pkunzip', sasout), out=FALSE, translate=TRUE)
    sasout <- rep('', 4)
    filenames <- c('dict','data','formats','specmiss')
    for(i in 1:4) if(access(filenames[i],4)==0) sasout[i] <- filenames[i]
    if(any(sasout[1:2]==''))stop('no files named dict and data')
    on.exit(unlink(sasout[sasout!='']))
  }
  if(any(sasout[1:2]==''))stop('sasout[1] and sasout[2] must not be ""')
  j <- sasout[sasout!='']
  k <- access(j,4) < 0
  if(any(k)) stop(paste('these files do not exist or you do not have read access:\n',paste(j[k],collapse='\n')))
  formats <- sasout[3]!='' && access(sasout[3])==0
  if(missing(recode)) recode <- formats
  special.miss <- sasout[4]!='' && access(sasout[4])==0
} else {
	# *****  Next line begins mod from Mike Kattan edits 11 Sep 97
	# Added 2 phrases for sas7bcat 9Oct00.  Changed FEH 22Oct00
	no.format <- all(access(paste(format.library,
	 c('formats.sc2','formats.sct','formats.sct01','formats.sas7bcat'),
		sep='/'),4) < 0)
	if(no.format) {
		if((!missing(formats) && formats) || (!missing(recode) && recode))
		  warning(paste(paste(format.library, 
	        "/formats.sc? or formats.sas7bcat",sep = ""), 
						" not found. Formatting ignored. \n"))
		formats <- recode <- FALSE
	}
# ***** End Mike Kattan edits 11 Sep 97
# 5 Changes here from Claudie Berger <claudie@osteo1.ri.mgh.mcgill.ca>
# 19feb00 (changed from unix version). Allows work on sas v7.

  sasout <- paste(tempfile(c('a','b','c','d','in')),'sas',sep='.')
  sasin  <- sasout[5]
  if(clean.up) on.exit(unlink(c(sasout,if(!keep.log)log.file)))

  if(missing(member)) stop('must specify member')
  if(library != '.' && !is.dir(library)) stop('library is not a valid directory name')

  nvariables <- length(variables)
  if(nvariables>0)	{
   if(any(jdup <- duplicated(variables)))
      stop(paste("duplicate variables requested: ", variables[jdup]))
				}
  varstring <- paste(variables, collapse = "\n ")
  ifs <- paste("'",paste(ifs, collapse = ";\n "),"'",sep="")

  cat(macro, sep="\n", file=sasin)
  if(unzip) {
    file <- paste(member,".zip",sep="")
    if(library != '.') file <- paste(library,'/',file,sep='')
    if(access(file)==0) dos(
      if(library=='.') paste("pkunzip",file) else 
                       paste("pkunzip",file,library),
      out=FALSE, translate=TRUE) else
    cat(file,'does not exist.  No unzipping attempted.\n')
  }

  file <- paste(member, 
	c('sd2','sd7','ssd01','ssd02','ssd03','ssd04','sas7bdat'), sep='.')
  if(library != '.') file <- paste(library, '/', file, sep='')
  if(all(access(file,4) < 0)) 
	stop(paste('file',paste(file,collapse=' '),
		       'does not exist or you do not have read access'))	
  cat("libname temp '", library, "';\n", file = sasin, append = TRUE,
			sep = "")
  if(format.library != '.' && (!is.dir(format.library) || access(format.library,4)<0))
    stop('format.library does not exist or you do not have read access for it')

# format.library should contain formats.sct containing user defined
# formats used by this dataset.
  cat("libname library '", format.library, "';\n", file = sasin,
	append = TRUE, sep = "")
  cat("%sas_get(temp.", member, " ,", sasout[1], " ,", sasout[2],
			" ,", sasout[3],", ",sasout[4],
			", dates=sas, vars=", varstring, 
			", ifs=", ifs, 
			", formats=",as.integer(formats),
			", specmiss=", as.integer(special.miss),
			");\n", file = sasin, append = TRUE, sep = "")
  cat('Invoking SAS for Windows.  Click the SAS icon if you want to watch.\n')
  win3(paste(sasprog, sasin, "-log", log.file, "-icon"))
  if(access(log.file) < 0) 
	stop(paste('SAS did not create log file',log.file,
	           '\nCheck that sas.exe is in your path.'))

  if(any(access(sasout[1:2]) < 0)) {
    cat('\nSAS did not run correctly to produce at least two ASCII files\n')
    cat('Make sure that sas.exe is in your path.\nPutting SAS log file in a window.\n')
    win3(paste('notepad',log.file), multi=TRUE)
    stop()
  }
}

#
# Read in the variable information
#

vars <- if(.R.) scan(sasout[1], list(name = "", type = 0, length = 0,
                                     format = "", label = "", n = 0),
                     multi.line = FALSE, flush=TRUE, sep = "\022",
                     comment.char='', quote='') else
  scan(sasout[1], list(name = "", type = 0, length = 0, format = "",
		label = "", n = 0), multi.line = FALSE, flush=TRUE, sep = "\022")
nvar <- length(vars$name)
if(nvar == 0) {
  if(!sasran) {
    cat('\nError: first SAS output file is empty.  Putting log file in a window.\nMake sure that sas.exe is in the path')
    win3(paste('notepad',log.file), multi=TRUE)
    stop()
  }
  stop("First SAS output file is empty.  Make sure that sas.exe is in the path")
}

nrow <- vars$n[1]	#n is the same for each variable
#
# Read the data in
#  We try to be clever about the variable type.  If SAS is character
#  use char of course.  If is numeric and length >4, use double.  If
#  numeric and length <4, use single.  We could also use the format to
#  choose further, if it consists of a number followed by a "."
#  can we safely assume integer.
#
type <- ifelse(vars$type == 2, "character(nrow)", 
	ifelse(force.single | (vars$length < 5 & !.R.),   ## 28Mar01
		"single(nrow)", "double(nrow)"))	#
inlist <- paste("\"", vars$name, "\"=", type, sep = "", collapse = ", ")
inlist <- parse(text = paste("list(", inlist, ")"))	#
# Inlist would now be the size of the final data structure, if I had
# evaluated it.
#
# Read the data
ds <- scan(sasout[2], eval(inlist), sep = "\022", multi.line = FALSE,
           flush=TRUE)
if(!sasran && (length(ds) < nvariables)) {
  m <- variables[is.na(match(variables, names(ds)))]
  if(length(m) > 0)
    warning(paste(length(m), 
		"requested variables did not exist:", paste(
		"\"", m, "\"", sep = "", collapse = " ")))
}
format <- vars$format
format[format=='$'] <- ' '    # 1Mar00

label <- vars$label
name <- vars$name

FORMATS <- NULL
formats <- formats && access(sasout[3])==0
if(formats) {
  FORMATS <- dget(sasout[3])
  if(length(FORMATS)==0) formats <- FALSE
}
if(recode && !formats) recode <- FALSE

smiss <- NULL
if(special.miss && access(sasout[4])==0)
  smiss <- scan(sasout[4], 
                list(name="", code="", obs=integer(1)),
		    multi.line=FALSE, flush=TRUE, sep="\022")

sasdateform <- c("date","mmddyy","yymmdd","ddmmyy","yyq","monyy",
	"julian","qtr","weekdate","weekdatx","weekday","month")
dateform <- 	
	list(as.name("ddmmmyy"),"m/d/y","y/m/d","d/m/y",as.name("ddmmmyy"),
         "mon year",as.name("ddmmmyy"),"mon",as.name("ddmmmyy"),
         as.name("ddmmmyy"), as.name("ddmmmyy"),"m")
sastimeform <- c("hhmm","hour","mmss","time")
timeform <- c("h:m","h","m:s","h:m:s")
sasdatetimeform <- c("datetime","tod")
datetimeform <- list(list(as.name("ddmmmyy"),"h:m:s"), c("m/d/y"," "))

z <- "%02d%b%Y"
dateform4 <-
  c(z,"%02m/%02d/%Y","%Y/%02m/%02d","%02d/%02m/%Y", z,"%02m %Y",
    z,"%02m", z, z, z,"%02m")
timeform4 <- c("%02H:%02M","%02H","%02M:%02S","%02H:%02M:%02S")
datetimeform4 <- c("%02d%b%Y %02h:%02m:%02s","%02m/%02d/%Y")


for(i in 1:nvar) {
  atr <- list()
  dsi <- ds[[i]]
  fname <- format[i]
  rec <- FALSE
  if(fname!=" ") {
    ff <- fname
    if((m <- match(fname,sasdateform,0)) >0) {
      #look for partial dates
      dd <- dsi-floor(dsi)
      ddn <- !is.na(dd)
      if(any(ddn) && any(dd[ddn]!=0)) {
        ll <- 1:length(dd)
	  atr$partial.date <- 
		 list(month=ll[dd==.5],day=ll[dd==.25],both=ll[dd==.75])
	  atr$imputed <- ll[dd!=0]
	  dsi <- floor(dsi)
      }
      dsi <-  if(.SV4.) timeDate(julian=dsi,format=dateform4[m]) else
        dates(dsi, out.format=dateform[[m]])
      if(length(atr$imputed)) 
	    attr(dsi,'class') <- c("impute",attr(dsi,'class'))
      ff <- NULL
    } else if((m <- match(fname,sastimeform,0)) >0) {
      dsi <- if(.SV4.) timeDate(ms=dsi*1000, format=timeform4[m]) else
        chron(times=dsi/86400, out.format=timeform[m])
      ff <- NULL
    } else if((m <- match(fname,sasdatetimeform,0))>0) {
      dsi <- if(.SV4.) timeDate(julian=dsi/86400,
                                format=datetimeform4[m]) else
        chron(dsi/86400, out.format=datetimeform[[m]])
	ff <- NULL
    }
    atr$format <- ff
    if(recode & length(g <- FORMATS[[fname]])) {
      labs <- g$labels
      if(!is.logical(recode)) {
        labs <- if(recode==1) paste(g$values,":",labs,sep="") else
	            paste(labs,"(",g$values,")",sep="")	
    }
    dsi <- factor(dsi, g$values, labs)
    atr$sas.codes <- g$values
    rec <- TRUE
  }  # end if(fname!=' ')
 }
  if(!rec && vars$type[i]==2 && ((is.logical(as.is) && !as.is) || 
               (is.numeric(as.is) && length(unique(dsi)) < as.is*length(dsi))))
  dsi <- factor(dsi, exclude="")
	 # For data frames, char. var usually factors
  if(label[i]!=" ") label(dsi) <- label[i]
  if(length(smiss$name)) {
    j <- smiss$name==name[i]
    if(any(j)) {
      atr$special.miss <- 
			list(codes=smiss$code[j],obs=smiss$obs[j])
      attr(dsi,'class') <- c("special.miss",attr(dsi,'class'))
    }
  }
  if(!is.null(atr)) attributes(dsi) <- c(attributes(dsi),atr)
  if(missing(where)) ds[[i]] <- dsi else 
    assign(name[i], dsi, where=where)				
}

if(!missing(where)) return(structure(where, class="where"))

atr <- list()
if(missing(id)) atr$row.names <- as.character(1:nrow)	else 	{
  idname <- id 
  jj <- match(idname, names(ds), 0)
  if(any(jj==0))stop(paste(
    "id variable(s) not in dataset:",paste(idname[jj==0],collapse=" ")))
  if(length(idname)==1) id <- ds[[idname]] #Need since not use data.frame
  else {		 
    id <- as.character(ds[[idname[1]]])
    for(jj in 2:length(idname))
      id <- paste(id, as.character(ds[[idname[jj]]]))
  }
  if(check.unique.id) {
    dup <- duplicated(id)
    if(any(dup)) warning(paste("duplicate IDs:",
      paste(id[dup], collapse=" ")))
  }
atr$row.names <- as.character(id)
}
if(length(FORMATS)) atr$formats <- FORMATS
atr$class <- "data.frame"
attributes(ds) <- c(attributes(ds),atr)
ds
}

if(.R.) {  ## Don MacQueen 3Apr02
  ## slightly modified copy of format.POSIXct() from R base
  format.timePOSIXt <- function (x, format = "%H:%M:%S", tz = "",
                                 usetz = FALSE, ...)
    {
      if (!inherits(x, c("timePOSIXt","POSIXct"))) stop("wrong class")
      class(x) <- class(x)[-1]
      structure(format.POSIXlt(as.POSIXlt(x, tz), format, usetz, ...),
                names = names(x))
    }

  print.timePOSIXt <- function(x, ...) print(format(x, ...))
  NULL
}

if(!.R.) {
#Output format routine needed by chron for usual SAS date format

ddmmmyy <- function(x)
{
	y <- month.day.year(trunc(oldUnclass(x)), attr(x,"origin"))
	yr <- y$year
	m <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct",
		"Nov","Dec")[y$month]
	ifelse(yr<1900 | yr>=2000, paste(y$day,m,yr,sep=""),
		paste(y$day,m,yr-1900,sep=""))
}
}


# Functions to handle special.miss class

is.special.miss <- function(x, code) {
  sm <- attr(x, "special.miss")
  if(!length(sm)) return(rep(FALSE, length(x)))
  if(missing(code)) {
    z <- rep(FALSE, length(x))
    z[sm$obs] <- TRUE
  } else {
    z <- rep(FALSE, length(x))
    z[sm$obs[sm$codes==code]] <- TRUE
  }
z
}

"[.special.miss" <- function(x, ..., drop=FALSE) {
	ats <- attributes(x)
	ats$dimnames <- NULL
	ats$dim <- NULL
	ats$names <- NULL
	attr(x,'class') <- NULL
	y <- x[..., drop = drop]
	if(length(y) == 0)
		return(y)
	k <- seq(along=x)
	names(k) <- names(x)
	k <- k[...]
	attributes(y) <- c(attributes(y), ats)
	smiss <- attr(y, "special.miss")
	codes <- rep("ZZ",length(x))
	codes[smiss$obs] <- smiss$codes
	codes <- codes[...]
	which <- codes!="ZZ"
	if(sum(which)) attr(y,"special.miss") <- 
	  list(obs=seq(along=k)[codes!="ZZ"],codes=codes[codes!="ZZ"])
	else {
	  attr(y,"special.miss") <- NULL
	  attr(y,'class') <- attr(y,'class')[attr(y,'class') != "special.miss"]
	  if(length(attr(y,'class'))==0) attr(y,'class') <- NULL
	}
	y
}

format.special.miss <- function(x, ...) {
  w <- if(is.factor(x)) as.character(x) else {
    cl <- attr(x,'class'); cl <- cl[cl!="special.miss"]
    if(length(cl)) { attr(x,'class') <- cl; format(x, ...) } else
      format.default(x, ...)
    }
  sm <- attr(x, "special.miss")
  names(w) <- names(x)
  if(!length(sm)) return(w)
  w[sm$obs] <- sm$codes
  attr(w,"label") <- attr(w,"special.miss") <- attr(w,"class") <- NULL
  w
}

print.special.miss <- function(x, ...) {
  sm <- attr(x, "special.miss")
  if(!length(sm)) {
    print.default(x)
    return(invisible())
  }
  w <- format.special.miss(x)
  print.default(w, quote=FALSE)
  invisible()
}

sas.codes <- function(object) attr(object, "sas.codes")
code.levels <- function(object) {
	if(length(cod <- attr(object,"sas.codes"))) 
		levels(object) <- paste(cod,":",levels(object),sep="")
	object
}

as.data.frame.special.miss <- function(x, row.names = NULL, optional = FALSE)
{
        nrows <- length(x)
        if(is.null(row.names)) {
# the next line is not needed for the 1993 version of data.class and is
# included for compatibility with 1992 version
                if(length(row.names <- names(x)) == nrows && !any(duplicated(
                        row.names))) {
                }
                else if(optional)
                        row.names <- character(nrows)
                else row.names <- as.character(1:nrows)
        }
        value <- list(x)
        if(!optional)
                names(value) <- deparse(substitute(x))[[1]]
	    structure(value, row.names=row.names, class='data.frame')
}



sas.get.macro <-
c("/* Macro sas_get (modified by F. Harrell 30Jan90, Bill Dunlap Dec90, FH Mar92,",
	"\t\t\tFH Apr95 (extend LENGTH smiss))", 
	"    Sets up for conversion of SAS dataset to S dataset.", 
	"    Arguments:", "\tdataset - name of SAS dataset", 
	"\ttemp1\t- Name of temporary dataset to contain data dictionar (unquoted)",
	"\t\t  default=/tmp/file.1", 
	"\ttemp2\t- Name of temporary dataset to contain ASCII version of SAS", 
	"\t\t  dataset (unquoted)", "\t\t  default=/tmp/file.2", 
	"\ttemp3   - Name of temporary dataset to contain ASCII file with S", 
	"\t\t  program to store format values and labels", 
	"\ttemp4   - Name of temporary dataset to contain ASCII file with", 
	"\t\t  locations of special missing values", 
	"\tdates\t- SAS to store date variables in SAS format (# days from 1/1/60)",
	"\t\t  (default)", 
	"\t\t- YEARFRAC to store as days from 1/1/1900, divided by 365.25", 
	"\t\t- YEARFRAC2 to store as year + fraction of current year", 
	"\t\t- YYMMDD to store as numeric YYMMDD", 
	"\tvars    - list of variable in dataset that you want returned to Splus",
	"                  (unquoted, separate variable names with spaces)  If empty,",
	"                  then return all variables.", 
	"        ifs     - sequence of SAS subsetting if statements, (unquoted,",
	"                  separated by semicolons).", 
	"\tformats - 0 (default) - do not create file on temp3 containing S", 
	"\t\t  statements to store format values and labels, 1 do create", 
	"\tspecmiss- 0 (default).  Set to 1 to write a data file on temp4 with",
	"\t\t  the fields: variable name, special missing value code,", 
	"\t\t  observation number", 
	"                                                                              */",
	"%macro sas_get(dataset,  temp1, temp2, temp3, temp4, dates=SAS, vars=, ifs=, ",
	"\tformats=0, specmiss=0);", 
	"OPTIONS NOFMTERR;",
	"%IF %QUOTE(&temp1)=  %THEN %LET temp1=/tmp/file.1;", 
	"%IF %QUOTE(&temp2)=  %THEN %LET temp2=/tmp/file.2;", 
	"%IF %QUOTE(&temp3)=  %THEN %LET temp3=/tmp/file.3;", 
	"%IF %QUOTE(&temp4)=  %THEN %LET temp4=/tmp/file.4;",
    ## Next line had %QUOTE(&ifs),1,\"'\"  31oct02
	"%LET dates=%UPCASE(&dates);", "%LET ifs=%SCAN(%QUOTE(&ifs),1,'');", 
	"%LET _s_=_sav_;", 
	"/* BILL: Can these 2 subsets be combined into one pass of the data? -Frank*/",
	"/* Subset by observation first */", "%IF %QUOTE(&ifs)^= %THEN %DO;", 
	" data _osub_ ;", "  set &dataset ;", "  &ifs ;", 
	" %LET dataset=_osub_ ;", " %END;", "/* Then subset by variable */", 
	"%IF &vars^= %THEN %DO;", " data _vsub_ ;", "  set &dataset ;", 
	"  keep &vars ;", " %LET dataset=_vsub_ ;", " %END;", 
	"proc contents data=&dataset out=&_s_(KEEP=name type length label format nobs ",
	" varnum) noprint; ", "%IF &formats=1 %THEN %DO;", 
	"   PROC FORMAT LIBRARY=LIBRARY CNTLOUT=f(KEEP=fmtname type start end label);",
	"   DATA f; SET f; RETAIN n 0; n+1; IF type=\"C\" THEN fmtname=\"$\"||fmtname;",
	"   PROC SORT DATA=f OUT=f(DROP=n); BY fmtname n; ", 
	"  *Sort by n instead of start for numerics so 13 sorts after 2;", 
	"  *Dont consider formats containing ANY range of values;", 
	"  *Dont consider formats that dont have at least one non-missing (if", 
	"   numeric) starting value.  This gets rid of formats that are used", 
	"   only to label special missing values;", 
	"   DATA f2; SET f; BY fmtname; RETAIN anyrange 0 anynmiss 0;", 
	"      IF FIRST.fmtname THEN DO;anyrange=0;anynmiss=0;END;", 
	"      IF start^=end THEN anyrange=1;", 
	"      IF TYPE=\"C\" THEN anynmiss=1; ", 
	"      ELSE IF (start+0)>. THEN anynmiss=1;", 
	"      IF LAST.fmtname & anynmiss & ^anyrange THEN OUTPUT; KEEP fmtname;",
	"   DATA f; MERGE f f2(IN=in2); BY fmtname; IF in2;", 
	"      IF TYPE=\"N\" THEN DO; IF (start+0)>.;  *S cannot handle special missings;",
	"         END;", "      RENAME fmtname=format start=value; DROP end;", 
	"   PROC SORT DATA=&_s_(KEEP=format) OUT=sform; BY format;", 
	"   DATA sform; SET sform; BY format; IF LAST.format;", 
	"   DATA f; MERGE sform(IN=in1) f(IN=in2); BY format; ", 
	"      IF in1 & in2;", 
	"   *This keeps formats ever used by any variable;", 
	"   DATA _NULL_; SET f END=_eof_; BY format;", 
	"      ARRAY val{*} $ 16 val1-val500; ARRAY lab{*} $ 40 lab1-lab500; ", 
	"      RETAIN done 0 nform 0 nval 0 val1-val500 \" \" lab1-lab500 \" \" bk -1; ",
	"      FILE \"&temp3\" LRECL=4096;", "      IF FIRST.format THEN DO;", 
	"         IF ^done THEN PUT 'list(' @@;  done=1;", 
	"         nform=nform+1; nval=0;", 
	"         format=TRANSLATE(format,\".abcdefghijklmnopqrstuvwxyz\",", 
	"                                 \"_ABCDEFGHIJKLMNOPQRSTUVWXYZ\");", 
	"          IF nform=1 THEN PUT '\"' format +bk '\"=list(' @@;", 
	"         ELSE PUT ', \"' format +bk '\"=list(' @@;", "         END;", 
	"      nval=nval+1; ", 
	"      IF nval>500 THEN DO; ERROR \">500 format values not allowed\";ABORT ABEND;",
	"         END;", "      val{nval}=value; lab{nval}=label; ", 
	"      IF LAST.format THEN DO;", "         PUT \"values=c(\" @@; ", 
	"         DO i=1 TO nval; IF i>1 THEN PUT \",\" @@;", 
	"            IF type=\"N\" THEN PUT val{i} +bk @@;", 
	"            ELSE PUT '\"' val{i} +bk '\"'  @@;", "            END;", 
	"         PUT \"),labels=c(\" @@;", 
	"         DO i=1 TO nval; IF i>1 THEN PUT \",\" @@;", 
	"            PUT '\"' lab{i} +bk '\"' @@;", "            END;", 
	"         PUT \"))\";", "         END;", 
	"      IF _eof_ THEN PUT \")\";", "   %END;", 
	"PROC SORT DATA=&_s_;BY varnum;", "data _null_;", " set &_s_ end=eof;", 
	" FILE \"&temp1\";  RETAIN _bk_ -1;", " if _n_ = 1 then do;", 
	"%IF &specmiss=0 %THEN %LET ofile=_NULL_; ", 
	"%ELSE %LET ofile=smiss(KEEP=vname val obs);", 
	"  put \"data &ofile; set &dataset end=eof;\";", 
	"  put '  file \"&temp2\" RECFM=D LRECL=4096;';", 
	"  put \"  retain __delim 18 _bk_ -1 obs 0; LENGTH _xx_ $ 20 obs 5;obs+1; \";",
	"%IF &specmiss=1 %THEN %DO;", 
	"  put \"LENGTH vname $ 8 val $ 1;\"; %END;", "  end;", 
	" IF type=2 THEN DO;", "  PUT 'FORMAT ' name ';' @;", 
	"  PUT 'IF ' name '=\" \" THEN PUT __delim IB1. @;';", 
	"/* $char added F.H. 24Mar92, dropped  +_bk_ before __delim */", 
	"/* $CHAR. removed FEH 2Aug92, added null FORMAT above, added back +_bk_ */",
	"  PUT 'ELSE PUT ' name '+_bk_ __delim IB1. @;';", "  END;", 
	" ELSE DO; ", "  PUT 'IF ' name '<=.Z THEN _xx_=\"NA\";' @;", 
	"  PUT 'ELSE _xx_=LEFT(PUT(' @;", "  format=UPCASE(format);", 
	"  IF format=\"DATE\"|format=\"MMDDYY\"|format=\"YYMMDD\"|",
    "format=\"DDMMYY\"|format=\"YYQ\"|format=\"MONYY\"|format=\"JULIAN\" THEN DO;",
	"   %IF &dates=SAS %THEN", "    PUT name \",BEST18.)\";", 
	"   %ELSE %IF &dates=YYMMDD %THEN", "    PUT name \",YYMMDD6.)\";", 
	"   %ELSE %IF &dates=YEARFRAC %THEN", 
	"    PUT \"(\" name \"-MDY(1,1,1900))/365.25,7.3)\";", 
	"   %ELSE %IF &dates=YEARFRAC2 %THEN %DO;", 
	"    PUT \"YEAR(\" name \")-1900+(\" name \"-MDY(1,1,YEAR(\" name \")))/\" @;",
	"    PUT \"(MDY(12,31,YEAR(\" name \"))-MDY(1,1,YEAR(\" name \"))+1),7.3)\";",
	"    %END;", "   ;", "   END;\t", 
	"  ELSE DO;PUT name \",BEST18.)\" @;END;", 
	"  PUT ');  PUT _xx_ +_bk_ __delim IB1. @;';  *Added +_bk_ 2Aug92;", 
	"%IF &specmiss=1 %THEN %DO;", 
	"  put 'IF .A<=' name '<=.Z THEN DO;",
    "   vname=\"' name +_bk_ '\"; val=put(' name ',1.); OUTPUT; END;';",
	"  %END;", "  END;", "if eof then PUT 'PUT; RUN;';", "run;", 
	"%include \"&temp1\";", "data _null_; set &_s_;", 
	" retain __delim 18 _bk_ -1; ", " file \"&temp1\" LRECL=4096;", 
	" name=TRANSLATE(name,\".abcdefghijklmnopqrstuvwxyz\",", 
	"\t\t     \"_ABCDEFGHIJKLMNOPQRSTUVWXYZ\");", 
	" format=TRANSLATE(format,\".abcdefghijklmnopqrstuvwxyz\",", 
	"                         \"_ABCDEFGHIJKLMNOPQRSTUVWXYZ\");", 
	" put name +_bk_ __delim IB1. type +_bk_ __delim IB1. length +_bk_ __delim IB1.",
	"  format +_bk_ __delim IB1. label +_bk_ __delim IB1. nobs +_bk_ __delim IB1.;",
	"run;", "%IF &specmiss=1 %THEN %DO;", 
	" PROC SORT DATA=smiss OUT=smiss;BY vname val obs;", 
	" DATA _NULL_; SET smiss;FILE \"&temp4\" RECFM=D LRECL=30;", 
	" RETAIN _bk_ -1 __delim 18;", 
	" vname=TRANSLATE(vname,\".abcdefghijklmnopqrstuvwxyz\",", 
	"\t\t       \"_ABCDEFGHIJKLMNOPQRSTUVWXYZ\");", 
	" PUT vname +_bk_ __delim IB1. val +_bk_ __delim IB1. obs +_bk_ __delim IB1.;",
	" RUN;", " %END;", "%mend sas_get;")

cleanup.import <- function(obj, labels=NULL, lowernames=FALSE, 
						   force.single=TRUE, force.numeric=TRUE,
                           rmnames=TRUE,
						   big=1e20, sasdict, 
						   pr=prod(dimobj) > 5e5,
                           datevars=NULL,
                           dateformat='%d%b%Y') {
  nam <- names(obj)
  dimobj <- dim(obj)
  nv <- length(nam)

  if(!missing(sasdict)) {
	sasvname <- make.names(sasdict$NAME)
	if(any(w <- nam %nin% sasvname)) stop(paste(
	 'The following variables are not in sasdict:',
	 paste(nam[w],collapse=' ')))
	saslabel <- structure(as.character(sasdict$LABEL), 
						  names=as.character(sasvname))
	labels <- saslabel[nam]
	names(labels) <- NULL
  }
	
  if(length(labels) && length(labels) != dimobj[2])
    stop('length of labels does not match number of variables')

  if(lowernames) names(obj) <- casefold(nam)

  if(pr) cat(dimobj[2],'variables; Processing variable:')
  for(i in 1:dimobj[2]) {
	if(pr) cat(i,'')
	x <- obj[[i]]; modif <- FALSE
    if(length(dim(x))) next     # 6Jan03

    if(rmnames) {
      if(length(attr(x,'names'))) {
        attr(x,'names') <- NULL
        modif <- TRUE
      } else if(length(attr(x,'.Names'))) {
        attr(x,'.Names') <- NULL
        modif <- TRUE
      }
    }

    if(.R. && length(attr(x,'Csingle'))) {
      attr(x,'Csingle') <- NULL
      modif <- TRUE
    }
    
    ## The following is to fix imports of S+ transport format data
    ## that were created in SV3
    if(.SV4.) {
      cl <- oldClass(x)
      xlev <- length(attr(x, 'levels'))
      if(any(cl=='AsIs')) {
        modif <- TRUE
        cat('Removed AsIs class from variable\t\t', nam[i], '\n')
        oldClass(x) <- cl[cl != 'AsIs']
        cl <- cl[cl != 'AsIs']
      }
      if(any(cl=='labelled')) {
        modif <- TRUE
        ##For some strange reason if class=c('labelled','factor'),
        ##removing labelled class changes class to 'category'
        cl <- oldClass(x) <- if(length(cl)==1 ||
                                (length(cl)==2 && cl[2]=='factor' &&
                                 !xlev)) NULL else
         cl[cl != 'labelled']
        cat('Removed labelled class from variable\t', nam[i], '\n')
      }
      if(any(cl=='factor') && !xlev) {
        modif <- TRUE
        oldClass(x) <- cl[cl != 'factor']
        cat('Removed factor class from variable having no levels\t',
            nam[i], '\n')
      }
    }

    if(length(datevars) && nam[i] %in% datevars & !all(is.na(x))) {
      if(!is.factor(x) || is.character(x))
        stop(paste('variable',nam[i],
                   'must be a factor or character variable for date conversion'))
      x <- as.POSIXct(strptime(as.character(x), dateformat))
      modif <- TRUE
    }

	if(length(labels)) {
	  label(x) <- labels[i]
	  modif <- TRUE
	}

	if(force.numeric && length(lev <- levels(x))) {
#	  .Options$warn <- -1   6Aug00
#	  s <- lev != ''
#	  if(all(!is.na(as.numeric(lev[s])))) {
	if(all.is.numeric(lev)) {
		labx <- attr(x,'label')
		x <- as.numeric(as.character(x))
		label(x) <- labx
		modif <- TRUE
	  }
	}


	if(storage.mode(x) == 'double') {
	  xu <- oldUnclass(x)
	  j <- is.infinite(xu) | is.nan(xu) | abs(xu) > big
	  if(any(j,na.rm=TRUE)) {
		x[j] <- NA
		modif <- TRUE
		if(pr)cat('\n')
		cat(sum(j,na.rm=TRUE),'infinite values set to NA for variable',
			nam[i],'\n')
	  }
         
      isdate <- isChron(x)  ## 31aug02
	  if(force.single && !isdate) {
        allna <- all(is.na(x))
        if(allna) {
          storage.mode(x) <- 'integer'
          modif <- TRUE
        }
        if(!allna) {
          notfractional <- !any(floor(x) != x, na.rm=TRUE)  ## 28Mar01
          ## max(abs()) 22apr03
          if(max(abs(x),na.rm=TRUE) <= (2^31-1) && notfractional) {   ## 29may02
            storage.mode(x) <- 'integer'
            modif <- TRUE
          } else if(!.R.) {
            storage.mode(x) <- 'single'
            modif <- TRUE
          }
        }
      }
    }
    if(modif) obj[[i]] <- x
    NULL
  }

  if(pr) cat('\n')
  if(!missing(sasdict)) {
    sasat <- sasdict[1,]
    attributes(obj) <- c(attributes(obj),
                         sasds=as.character(sasat$MEMNAME),
                         sasdslabel=as.character(sasat$MEMLABEL))
  }
  obj
}

if(FALSE) {
  ## Here's some code I had to run once to clean up a data frame with
  ## S-Plus 6 on Windows:

  w <- card1
  for(i in 1:length(w)) {
    at <- attributes(w[[i]])
    if(any(at$class == 'Design')) {
      at$class <- at$class[at$class != 'Design']
      attributes(w[[i]]) <- at
    }
    lab <- attr(w[[i]],'label')
    if(length(lab)) {
      names(lab) <- NULL
      attr(w[[i]],'label') <- lab
    }
  }
}

  

upData <- function(object, ...,
                   rename=NULL, drop=NULL,
                   labels=NULL, units=NULL, levels=NULL,
                   force.single=TRUE, lowernames=FALSE, moveUnits=FALSE) {

no <- names(object)
n  <- nrow(object)
if(!length(n)) {
  x <- object[[1]]
  d <- dim(x)
  n <- if(length(d)) d[1] else length(x)
}
rnames <- row.names(object)

if(lowernames) names(obj) <- casefold(nam)
cat('Input object size:\t',object.size(object),'bytes;\t',
    length(no),'variables\n')

## The following keeps label(object[[n]]) <- 'label' from removing the
## 'labelled' class from objects with other classes
# if(.R.) object <- oldUnclass(object)

if(.SV4.) for(i in 1:length(no)) {
  z <- object[[i]]
  cl <- oldClass(z)
  modif <- FALSE
  zlev <- length(attr(z, 'levels'))
  if(any(cl=='AsIs')) {
    modif <- TRUE
    cat('Removed AsIs class from variable\t\t', no[i], '\n')
    cl <- cl[cl != 'AsIs']
    oldClass(z) <- cl
  }
  if(any(cl=='labelled')) {
    ##For some strange reason if class=c('labelled','factor'),
    ##removing labelled class changes class to 'category'
    modif <- TRUE
    cl <- oldClass(z) <- if(length(cl)==1 ||
       (length(cl)==2 && cl[2]=='factor' && !zlev)) NULL else
     cl[cl != 'labelled']
    oldClass(z) <- cl  # new
    cat('Removed labelled class from variable\t', no[i], '\n')
  }
    if(any(cl=='factor') && !zlev) {
      modif <- TRUE
      oldClass(z) <- cl[cl != 'factor']
      cat('Removed factor class from variable having no levels\t',
          no[i], '\n')
    }
    if(modif)  object[[i]] <- z
}

if(moveUnits) for(i in 1:length(no)) {
  z <- object[[i]]
  lab <- attr(z,'label')
  if(!length(lab) || length(attr(z,'units'))) next
  paren <- length(grep('\\(.*\\)',lab))
  brack <- length(grep('\\[.*\\]',lab))
  if(paren+brack == 0) next
  cat('Label for',no[i],'changed from',lab,'to ')
  u <- if(paren)regexpr('\\(.*\\)',lab) else regexpr('\\[.*\\]',lab)
  len <- attr(u,'match.length')
  un <- substring(lab, u+1, u+len-2)
  lab <- substring(lab, 1, u-1)
  if(substring(lab, nchar(lab), nchar(lab)) == ' ')
    lab <- substring(lab, 1, nchar(lab)-1) # added 2nd char above 8jun03
  cat(lab,'\n\tunits set to ',un,'\n',sep='')
  attr(z,'label') <- lab
  attr(z,'units') <- un
  object[[i]] <- z
}

if(length(rename)) {
  nr <- names(rename)
  if(length(nr)==0 || any(nr==''))
    stop('the list or vector specified in rename must specify variable names')
  for(i in 1:length(rename)) {
    if(nr[i] %nin% no) stop(paste('unknown variable name:',nr[i]))
    cat('Renamed variable\t', nr[i], '\tto', rename[[i]], '\n')
  }
  no[match(nr, no)] <- unlist(rename)
  names(object) <- no
}

z <- substitute(list(...))

if(length(z) > 1) {
  z <- z[-1]
  vn <- names(z)
  if(!length(vn) || any(vn=='')) stop('variables must all have names')
  for(i in 1:length(z)) {
    v <- vn[i]
    if(v %in% no)
      cat('Modified variable\t',v,'\n') else {
      cat('Added variable\t\t', v,'\n')
      no <- c(no, v)
    }
    x <- eval(z[[i]], object)
    d <- dim(x)
    lx <- if(length(d))d[1] else length(x)
    if(lx != n) {
      if(lx == 1) warning(paste('length of ',v,
             ' is 1; will replicate this value.',sep='')) else {
               f <- find(v)
               if(length(f))cat('Variable',v,'found in',
                                paste(f,collapse=' '),'\n')
               stop(paste('length of ',v,' (',lx, ')\n',
                          'does not match number of rows in object (',
                          n,')',sep=''))
    }
      }
    ## If x is factor and is all NA, user probably miscoded. Add
    ## msg.
    if(is.factor(x) && all(is.na(x)))
      warning(paste('Variable ',v,'is a factor with all values NA.\n',
  'Check that the second argument to factor() matched the original levels.\n',
                    sep=''))
                    object[[v]] <- x
  }
}

if(force.single) {
  sm <- sapply(object, storage.mode)
  if(any(sm=='double'))
    for(i in 1:length(sm)) {   # 28Mar01
      if(sm[i]=='double') {
        x <- object[[i]]
        if(isChron(x)) next   ## 31aug02
        if(all(is.na(x))) storage.mode(object[[i]]) <- 'integer' else
        {
          notfractional <- !any(floor(x) != x, na.rm=TRUE)  ## 28Mar01
          ## max(abs()) 22apr03
          if(notfractional && max(abs(x),na.rm=TRUE) <= (2^31-1))
            storage.mode(object[[i]]) <- 'integer' else
          if(!.R.) storage.mode(object[[i]]) <- 'single'
        }
      }
    }
}

if(length(drop)) {
  if(length(drop)==1) cat('Dropped variable\t',drop,'\n') else
  cat('Dropped variables\t',paste(drop,collapse=','),'\n')

  s <- drop %nin% no
  if(any(s)) warning(paste(
     'The following variables in drop= are not in object:',
                           paste(drop[s],collapse=' ')))
  no <- no[no %nin% drop]
  object <- object[no]
}

if(length(levels)) {
  if(!is.list(levels))stop('levels must be a list')
  nl <- names(levels)
  s <- nl %nin% no
  if(any(s)) {
    warning(paste(
                  'The following variables in levels= are not in object:',
                  paste(nl[s],collapse=' ')))
    nl <- nl[!s]
  }
  for(n in nl) {
    if(!is.factor(object[[n]])) object[[n]] <- as.factor(object[[n]])
    levels(object[[n]]) <- levels[[n]]
    ## levels[[nn]] will usually be a list; S+ invokes merge.levels
  }
}

if(length(labels)) {
  nl <- names(labels)
  if(!length(nl)) stop('elements of labels were unnamed')
  s <- nl %nin% no
  if(any(s)) {
    warning(paste(
                  'The following variables in labels= are not in object:',
                  paste(nl[s], collapse=' ')))
    nl <- nl[!s]
  }
  for(n in nl) {
    if(.SV4.) attr(object[[n]],'label') <- labels[[n]] else
    label(object[[n]]) <- labels[[n]]
  }
}

if(length(units)) {
#  if(!is.list(units))stop('units must be a list')
  nu <- names(units)
  s <- nu %nin% no
  if(any(s)) {
    warning(paste(
                  'The following variables in units= are not in object:',
                  paste(nu[s], collapse=' ')))
    nu <- nu[!s]
  }
  for(n in nu) attr(object[[n]],'units') <- units[[n]]
}

cat('New object size:\t',object.size(object),'bytes;\t',
    length(no),'variables\n')
# if(.R.) object <- structure(object, class='data.frame', row.names=rnames)
object
}

exportDataStripped <- if(.R.) function(data, ...)
  stop('function not available for R') else function(data, ...) {
    for(i in 1:length(data)) {
      atr <- attributes(data[[i]])
      if(any(names(atr) %in% c('label','imputed','format','units'))) {
        attr(data[[i]],'label') <- attr(data[[i]],'imputed') <-
          attr(data[[i]],'format') <- attr(data[[i]],'units') <-
            attr(data[[i]],'comment') <- NULL
      }
    }
    exportData(data, ...)
  }

if(.R.) {
  spss.get <- function(file, datevars=NULL,
                       use.value.labels=TRUE,
                       to.data.frame=TRUE,
                       max.value.labels=Inf,
                       typeDate=c('POSIX','chron'),
                       force.single=TRUE) {
    require('foreign')
    typeDate <- match.arg(typeDate)
    w <- read.spss(file, use.value.labels=use.value.labels,
                   to.data.frame=to.data.frame,
                   max.value.labels=max.value.labels)

    a   <- attributes(w)
    vl  <- a$variable.labels
    nam <- a$names
    lnam <- names(vl)
    if(length(vl)) for(i in 1:length(vl)) {
      n <- lnam[i]
      lab <- vl[i]
      if(lab != '' && lab != n) label(w[[i]]) <- lab
    }
    attr(w, 'variable.labels') <- NULL

    if(force.single || length(datevars)) for(v in nam) {
      x <- w[[v]]
      changed <- FALSE
      if(v %in% datevars) {
        x <- switch(typeDate,
                    POSIX = ISOdate(1584,10,14) + x,
                    chron = structure((unclass(ISOdate(1584,10,14)) +
                      x ) /24/60/60 - 1,
                      class=c('dates','times'), units=NULL,
                      format=c(dates='day mon year'),
                      origin=c(month=1,day=1,year=1970))   )
        changed <- TRUE
      } else if(all(is.na(x))) {
        storage.mode(x) <- 'integer'
        changed <- TRUE
      } else if(!(is.factor(x) || is.character(x))) {
        if(all(is.na(x))) {
          storage.mode(x) <- 'integer'
          changed <- TRUE
        } else if(max(abs(x),na.rm=TRUE) <= (2^31-1) &&
           all(floor(x) == x, na.rm=TRUE)) {
          storage.mode(x) <- 'integer'
          changed <- TRUE
        }
      }
      if(changed) w[[v]] <- x
    }
    w
  }
  NULL
}

  
if(.R.) {               
sasxport.get <- function(file, force.single=TRUE) {

  require('foreign') || stop('foreign package is not installed')

  sasdateform <-
    toupper(c("date","mmddyy","yymmdd","ddmmyy","yyq","monyy",
              "julian","qtr","weekdate","weekdatx","weekday","month"))
  sastimeform     <- toupper(c("hhmm","hour","mmss","time"))
  sasdatetimeform <- toupper(c("datetime","tod"))
  days.to.adj <- as.numeric(difftime(ISOdate(1970,1,1,0,0,0) , 
                                     ISOdate(1960,1,1,0,0,0), 'days'))
  secs.to.adj <- days.to.adj*24*60*60

  if(length(grep('http://', file))) {
    tf <- tempfile()
    download.file(file, tf, mode='wb', quiet=TRUE)
    file <- tf
  }
  dsinfo <- lookup.xport(file)
  ds     <- read.xport(file)
  
  ## PROC FORMAT CNTLOUT= dataset present?
  fds <- which(sapply(dsinfo, function(x)
                      all(c('FMTNAME','START','END','MIN','MAX','FUZZ')
                      %in% x$name)))
  if(length(fds) > 1) {
    warning('transport file contains more than one PROC FORMAT CNTLOUT= dataset; using only the first')
    fds <- fds[1]
  }
  
  finfo <- NULL
  if(length(fds)) {
    finfo <- ds[[fds]]
    ## Remove leading $ from char format names
#    fmtname <- sub('^\\$','',as.character(finfo$FMTNAME))
    fmtname <- as.character(finfo$FMTNAME)
    finfo <- split(finfo[c('START','END','LABEL')], fmtname)
    finfo <- lapply(finfo,
                    function(f) {
                      st <- as.character(f$START)
                      en <- as.character(f$END)
                      if(!all(st==en)) return(NULL)
                      list(value = all.is.numeric(st, 'vector'),
                           label = as.character(f$LABEL))
                    })
  }

  ## Number of non-format datasets
  nods <- length(dsinfo)
  nds  <- nods - (length(finfo) > 0)

  which.regular <- setdiff(1:nods,fds)
  dsn <- tolower(names(dsinfo)[which.regular])
  
  if(nds > 1) {
    res <- vector('list', nds)
    names(res) <- gsub('_','.',dsn)
  }

  j <- 0
  for(k in which.regular) {
    j   <- j + 1
    cat('Processing SAS dataset', dsn[j], '\n')
    w   <- if(nods==1) ds else ds[[k]]
    nam      <- names(w)
    names(w) <- tolower(nam)
    dinfo    <- dsinfo[[k]]
    fmt      <- sub('^\\$','',dinfo$format)
    lab      <- dinfo$label
    for(i in 1:length(w)) {
      changed <- FALSE
      x  <- w[[i]]
      fi <- fmt[i]
      if(fi != '' && length(finfo) && (fi %in% names(finfo))) {
        f <- finfo[[fi]]
        if(length(f)) {  ## may be NULL because had a range in format
          x <- factor(x, f$value, f$label)
          attr(x, 'format') <- fi
          changed <- TRUE
        }
      }
      if(is.numeric(x)) {
        if(fi %in% sasdateform) {
          tmp <- structure((x - days.to.adj)*24*60*60,
                           class=c('POSIXt','POSIXct'))
          x <- as.POSIXct(format(tmp,tz='GMT'),tz='')
          changed <- TRUE
        } else if(fi %in% sastimeform) {
          tmp <- structure(x, class=c('POSIXt','POSIXct'))
          tmp <- as.POSIXct(format(tmp,tz='GMT'),tz='')
          x <- structure(tmp, class=c('timePOSIXt','POSIXt','POSIXct'))
          changed <- TRUE
        } else if(fi %in% sasdatetimeform) {
          tmp <- structure(x - secs.to.adj,
                           class=c('POSIXt','POSIXct'))
          x <- as.POSIXct(format(tmp,tz='GMT'),tz='')
          changed <- TRUE
        } else if(force.single) {
          if(all(is.na(x))) {
            storage.mode(x) <- 'integer'
            changed <- TRUE
          } else if(max(abs(x),na.rm=TRUE) <= (2^31-1) &&
             all(floor(x) == x, na.rm=TRUE)) {
            storage.mode(x) <- 'integer'
            changed <- TRUE
          }
        }
      }
      if(lab[i] != '') {
        label(x) <- lab[i]
        changed <- TRUE
      }
      
      if(changed) w[[i]] <- x
    }
    if(nds > 1) res[[j]] <- w
  }
  if(nds > 1) res else w
}

NULL}

csv.get <- function(file, lowernames=FALSE, datevars=NULL,
                    dateformat='%d%b%Y', ...) {
  w <- read.csv(file, check.names=FALSE, ...)
  n <- names(w)
  m <- make.names(n, unique=TRUE)
  if(lowernames) m <- casefold(m)
  changed <- any(m != n)
  if(changed) names(w) <- m
  cleanup.import(w, labels=if(changed)n else NULL,
                 datevars=datevars, dateformat=dateformat)
}

sasdsLabels <- function(file) {
  w <- scan(file, sep='\n', what='', quiet=TRUE)
  i <- grep('Data Set Name:', w)
  if(!length(i)) return(NULL)
  n <- tolower(sub('.*\\.([A-Z0-9\\_]*)[[:space:]]+.*','\\1',w[i]))
  w <- gsub('\t','',w)
  labs <- ifelse(nchar(w[i-1])==0,w[i-2],w[i-1])
  names(labs) <- n
  labs
}

  
### -*-S-*- Improvements due to Martin Maechler <maechler@stat.math.ethz.ch>

scat1d <- function(x, side=3, frac=.02, jitfrac=.008, tfrac, 
				   eps=ifelse(preserve,0,.001),
				   lwd=0.1, col=par('col'), y=NULL, curve=NULL,
				   bottom.align=FALSE, preserve=FALSE, fill=1/3, limit=TRUE, 
				   nhistSpike=2000, nint=100, 
				   type=c('proportion','count','density'),
                   grid=FALSE,
				   ...) {
  
  type <- match.arg(type)
  if(length(x) >= nhistSpike)
    return(histSpike(x, side=side, type=type,
                     frac=2.5*frac, col=col, y=y, curve=curve,
                     bottom.align=if(type=='density') TRUE else bottom.align, 
                     add=TRUE, nint=nint, grid=grid, ...))

  gfun <- ordGridFun(grid)

  if(side==1 || side==3 || length(y) || length(curve)) {l <- 1:2; ax <- 1} else
                                      {l <- 3:4; ax <- 2}

  pr <- parGrid(grid)
  usr <- pr$usr; pin <- pr$pin; uin <- pr$uin
  ## Not using elegant unit() because of efficiency when n very large

  u <- usr[l]
  u.opp <- usr[-l]
  w <- u[2]-u[1]
  ## Start JOA 12.8.97 : handle xy missings parallel
  if (length(y)>1){ ## length=1 special case needed for datadensity
  	if (length(x)!=length(y))stop("y must have same length as x (or length(y)=1)")
  	selector <- !(is.na(x)|is.na(y))
  	x <- oldUnclass(x[selector])
  	y <- oldUnclass(y[selector])
  }
  else
  ## Stop JOA 12.8.97
  x <- oldUnclass(x[!is.na(x)])  ## unclass 29Jul97
  if(length(curve)) y <- approx(curve, xout=x, rule=2)$y   #31Dec98
  n <- length(x)
  if(missing(tfrac)) tfrac <- if(n<125) 1 else max(.1, 125/n)
  else if (tfrac < 0 || tfrac > 1) stop("must have  0 <= tfrac <= 1")

  ## Start JOA 19.8.97
  if(jitfrac>0 && any(duplicated( if(eps>0) round(x/w/eps) else x )))
  	if (preserve)
  		x <- jitter2(x, fill=fill, limit=limit, eps=w*eps)
  	else
  ## Stop JOA 19.8.97
    	x <- x + runif(n, -w*jitfrac, w*jitfrac)

##  h <- (u.opp[2]-u.opp[1])*frac*min(fin)/fin[-ax]
  h <- min(pin)*frac/uin[-ax]
  if(length(y)) { a <- y - h/2; b <- y + h/2 } else {
	a <- if(side<3) u.opp[1] else u.opp[2]-h
	b <- if(side<3) u.opp[1]+h else u.opp[2]
  }
  if(tfrac<1)
  {
     l <- tfrac*(b-a)
     a <- a + runif(n)*(b-l-a)   ##runif(n, a, b-l) if frac>0
     b <- a+l
  }
  if(ax==1 && bottom.align) {a <- a + h/2; b <- b + h/2}
  if(ax==1) gfun$segments(x, a, x, b, lwd=lwd, xpd=frac<0, col=col)
  else gfun$segments(a, x, b, x, lwd=lwd, xpd=frac<0, col=col)
  invisible()
}


jitter2 <- function(x,...)UseMethod("jitter2")

jitter2.default <- function(x, fill=1/3, limit=TRUE, eps=0,
                            presorted=FALSE, ...)
{ 
  x2 <- x[!is.na(x)]
  if (!presorted){ o <- order(x2); x2 <- x2[o] }
  if (eps>0)  r <- rle(round(x2/eps)*eps)
  else   r <- rle(x2)
  if ( length(r$length)<2 || max(r$length)<2 ) return(x)
  d <- abs(diff(r$values))
  d <- pmin( c(d[1],d), c(d,d[length(d)]) )
  who <- rep(r$lengths>1,r$lengths)
  d <- d[r$lengths>1]*fill/2
  if (is.logical(limit) && limit) limit <- min(d)
  if (limit) d <- pmin(d,limit)
  r$values <- r$values[r$lengths>1]-d
  r$lengths <- r$lengths[r$lengths>1]
  d <- d*2/(r$lengths-1)
  k <- length(r$lengths)
  n <- sum(who)
  val <- rep(r$values,r$lengths)
  add <- (0:(n-1))-rep(c(0,cumsum(r$lengths[-k])),r$lengths)
  add <- add[order(rep(1:k,r$lengths),runif(n))]
  add <- add * rep(d,r$lengths)
  val <- val + add
  x2[who] <- val
  if (!presorted)x2[o]<-x2
  x[!is.na(x)] <- x2
  x
}

jitter2.data.frame <- function(x, ...)
{
	as.data.frame(lapply(x,function(z,...){
		if (is.numeric(z)) jitter2.default(z,...)
		else z
	},...))
}


datadensity <- function(object, ...) {  ## 7Nov00
  if(!length(oldClass(object))) oldClass(object) <- data.class(object)
  UseMethod('datadensity')
}

datadensity.data.frame <-
  function(object, group,
           which=c('all','continuous','categorical'),
           method.cat=c('bar','freq'),
           col.group=1:10,
           n.unique=10, show.na=TRUE, nint=1, naxes,
           q, bottom.align=nint>1,
           cex.axis=sc(.5,.3), cex.var=sc(.8,.3),
           lmgp=if(.R.)if(version$minor=='5.1')sc(-.2,-.625) else sc(0,0) else sc(.3,0),
           tck=sc(-.009,-.002),
           ranges=NULL, labels=NULL, ...) {

which <- match.arg(which)
method.cat <- match.arg(method.cat)
maxna <- 0
mgroup <- missing(group)  # before R changes it

## Was 28aug02
##    if(nu < 2) 'nil' else
##    list(if(is.category(x) || is.character(x) ||
##            nu < n.unique)
##         'cat' else 'cont',
##         na=sum(is.na(x)))

z <-
  sapply(object, function(x, n.unique) {
    xp <- x[!is.na(x)] # 7jun03 and next;unique not handle empty factor var
    nu <- if(length(xp)) length(unique(xp)) else 0 # 18Oct01+next
    if(nu < 2) c(0,0) else
    c(type=if(is.category(x) || is.character(x) ||
            nu < n.unique) 1 else 2,
         na=sum(is.na(x)))
  }, n.unique=n.unique)
types <- c('nil','cat','cont')[z[1,]+1]  # was unlist(z[1,]) unlist(z[2,])
numna <- z[2,]
fnumna <- format(numna)
maxna <- max(numna)

w <- switch(which,
			all        = types != 'nil',   # 18Oct01
			continuous = types == 'cont',
			categorical= types == 'cat')

if(missing(naxes)) naxes <- sum(w)

## Function to scale values such that when naxes<=3 get hi, >=50 get
## lo, otherwise linearly interpolate between 3 and 50
sc <- function(hi,lo,naxes) approx(c(50,3),c(lo,hi),xout=naxes,rule=2)$y
formals(sc) <- list(hi=NA,lo=NA,naxes=naxes)
nams <- names(object)
max.length.name <- max(nchar(nams))

oldpar <- oPar()  # in Hmisc Misc.s
mgp  <- c(0,lmgp,0)   # 18Oct01, for axis

mai  <- oldpar$mai
if(.R.) { plot.new(); par(new=TRUE) } # enables strwidth
mxlb <-  .1 + max(strwidth(nams, units='inches', cex=cex.var))
mai[2] <- mxlb
if(!show.na) maxna <- 0
max.digits.na <- if(maxna==0) 0 else trunc(log10(maxna))+1
if(maxna > 0) mai[4] <- .1 + strwidth('Missing',units='inches',cex=cex.var)
par(mgp=mgp, mai=mai,tck=tck)
on.exit(setParNro(oldpar))

if(!mgroup) group <- as.factor(group)
  else {
  group <- factor(rep(1,length(object[[1]])))
  ngroup <- 0
}
ngroup <- length(levels(group))
col.group <- rep(col.group, length=ngroup)

y <- 0
for(i in (1:length(nams))[w]) {
  if(y < 1) {
	plot(c(0,1),c(1,naxes),xlim=c(.02,.98),ylim=c(1,naxes),
		 xlab='',ylab='',type='n',axes=FALSE)
    usr    <- par('usr')
	y <- naxes + 1
	if(maxna > 0) {
      outerText('Missing',
                y=naxes+strheight('Missing',units='user',cex=cex.var),
                cex=cex.var)
    }
	charheight <- strheight('X',units='user',cex=.6)  ## par('cxy')[2]
  }
  y <- y - 1
  x <- object[[i]]
  if(types[i] == 'cont' ) {  ## continuous variable
	x <- oldUnclass(x)          ## 29Jul97 - handles dates
	isna <- is.na(x)
	nna  <- sum(isna)
	N <- length(x) - nna
    r <- if(length(ranges) && length(ranges[[nams[i]]]))
      ranges[[nams[i]]] else range(x, na.rm=TRUE)  ## 7Nov00
    p <- pretty(r, if(nint==1)5 else nint)
	if(nint < 2) p <- c(p[1],p[length(p)]) ##bug in pretty for nint=1
	xmin <- p[1]
	xmax <- p[length(p)]
    if(.R.) cex <- par(cex=cex.axis)  # Bug in R: cex= ignored in
                                      # axis( )
	axis(side=1, at=(p-xmin)/(xmax-xmin), labels=format(p),
		 pos=y, cex=cex.axis)   # 18Oct01
    if(.R.) par(cex=cex)
	if(mgroup)
	  scat1d((x-xmin)/(xmax-xmin), y=y, bottom.align=bottom.align, 
			 minf=.075, frac=sc(.02,.005), ...)
	else for(g in 1:ngroup) {
		j <- group==levels(group)[g]
		scat1d((x[j]-xmin)/(xmax-xmin), y=y, bottom.align=bottom.align,
			   col=col.group[g], tfrac=if(N<125) 1 else max(.1, 125/N), 
			   minf=.075, frac=sc(.02,.005), ...)
	  }
	if(!missing(q)) {
	  quant <- quantile(x, probs=q, na.rm=nna>0)
	  points((quant-xmin)/(xmax-xmin),
			 rep(y-.5*charheight,length(q)),
			 pch=17, cex=.6)
	}
  } else {  ## character or categorical or discrete numeric
	if(is.character(x)) x <- as.factor(x)
	isna <- is.na(x)
	nna <- sum(isna)
    if(length(group) != length(x)) {  ## 7Nov00
      if(is.data.frame(object))
        stop('length of group must equal length of variables in data frame')
      group <- rep(1, length(x))
    }
	tab <- table(group,x)
	lev <- dimnames(tab)[[2]]
	nl  <- length(lev)
	if(is.numeric(x)) {
	  xx <- as.numeric(lev)
	  xx <- (xx-min(xx))/(max(xx)-min(xx))
	} else {
	  if(sum(nchar(lev)) > 200) 
		lev <- substring(lev, 1, max(1, round(200/length(lev))))
	  xx <- (0:(nl-1))/(nl-1)
	}
	if(.R.) {
      cex <- par(cex=cex.axis)
      axis(side=1, at=xx, labels=lev, pos=y, cex=cex.axis, tick=FALSE)
      par(cex=cex)
    } else axis(side=1, at=xx, labels=lev, pos=y, cex=cex.axis, ticks=FALSE)
	lines(c(0,1),c(y,y))
	maxfreq <- max(tab)
	for(g in if(ngroup==0) 1 else 1:ngroup) {
	  tabg <- tab[g,]
	  if(method.cat=='bar')
	  symbols(xx, y+.4*tabg/maxfreq/2, add=TRUE,
			  rectangles=cbind(.02, .4*tabg/maxfreq), inches=FALSE,
			  col=col.group[g])
	else text(xx, rep(y+.1,nl), format(tabg), cex=cex.axis*sqrt(tab/maxfreq),
			  adj=.5)
	}
  }
  mtext(if(length(labels))labels[i] else nams[i],   ## 14Dec01
        2, 0, at = y, srt = 0, cex = cex.var, adj = 1, las=1)
  ## las=1 for R 19Mar01 (also 3 lines down)
  if(show.na && nna > 0) {
#	mtext(format(nna), 4, line = max.digits.na/3,
#		  at = y, srt = 0, adj = 1, cex = cex.var*.66667, las=1)
    outerText(fnumna[i], y, setAside='Missing', cex=cex.var)
  }
}

invisible()
}

histSpike <- function(x, side=1, nint=100, frac=.05, minf=NULL,
					  mult.width=1,
					  type=c('proportion','count','density'),
					  xlim=range(x),
					  ylim=c(0,max(f)), xlab=deparse(substitute(x)), 
					  ylab=switch(type,proportion='Proportion',
						               count     ='Frequency',
						               density   ='Density'),
					  y=NULL, curve=NULL, add=FALSE, 
					  bottom.align=type=='density', 
					  col=par('col'), lwd=par('lwd'), grid=FALSE, ...) {
  type <- match.arg(type)
  if(!add && side!=1) stop('side must be 1 if add=F')
  if(add && type=='count') warning('type="count" is ignored if add=T')
  ## Following 4 lines deleted 12Sep00, 7 lines added after that
#  if(length(y) > 1) stop('y must be a constant')
#  if(length(y) && !add) stop('y may be not be given when add=F')
#  if(type=='density' && !bottom.align) 
#	stop('bottom.align must be T fo type="density"')
  if(length(y) > 1) {   ## 12Sep00
    if(length(y) != length(x))stop('lengths of x and y must match')
    if(length(curve))warning('curve ignored when y specified')
    i <- !is.na(x+y)
    curve <- list(x=x[i], y=y[i])
  }
  if(length(curve) && !missing(bottom.align) && bottom.align)
    warning('bottom.align=T specified with curve or y; ignoring bottom.align')

  gfun <- ordGridFun(grid)
  x <- x[!is.na(x)]
  x <- x[x >= xlim[1] & x <= xlim[2]]

  if(type != 'density') {
	if(is.character(nint)) {
	  f <- table(x)
	  x <- as.numeric(names(f))
	} else {
	  ncut <- nint+1
	  bins <- seq(xlim[1], xlim[2], length = ncut)
	  delta <- (bins[2]-bins[1]) / 2
##	  f <- if(version$major < 5) table(cut(x, c(bins[1]-delta,bins)))
##        else table(oldCut(x, c(bins[1]-delta,bins)))   18Mar02
      f <- if(.SV4.) table(oldCut(x, c(bins[1]-delta,bins))) else
       table(cut(x, c(bins[1]-delta,bins)))
	  x <- bins
	  j <- f > 0
	  x <- x[j]
	  f <- f[j]
	}
	if(type=='proportion') f <- f / sum(f)
  } else {
	nbar <- logb(length(x), base = 2) + 1
	width <- diff(range(x))/nbar*.75*mult.width
	den <- density(x,width=width,n=200,from=xlim[1],to=xlim[2])
	x <- den$x
	f <- den$y
  }

  if(!add) {
    if(grid) stop('add=T not implemented for lattice')
	plot(0, 0, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, type='n')
  }
#	if(type=='density') lines(x, f, col=col, lwd=lwd) else   12Sep00
#	   segments(x, 0, x, f, col=col, lwd=lwd)
#	return(invisible(xlim))
#  }

  if(side==1 || side==3) {l <- 1:2; ax <- 1} else {l <- 3:4; ax <- 2}
  f <- f / max(f)
  if(length(minf)) f <- pmax(f, minf)

  pr <- parGrid(grid)
  usr <- pr$usr; pin <- pr$pin; uin <- pr$uin

  u <- usr[l]
  u.opp <- usr[-l]

  h <- min(pin)*frac/uin[-ax] * f
  if(length(curve) || length(y)) {
    if(length(curve)) y <- approx(curve, xout=x, rule=2)$y
    a <- y - h/2; b <- y + h/2 } else {
      a <- if(side<3) u.opp[1] else u.opp[2]-h
      b <- if(side<3) u.opp[1]+h else u.opp[2]
    }
  if(ax==1 && bottom.align && type!='density') {a <- a + h/2; b <- b + h/2}
  if(type=='density') {
    lll <- gfun$lines
    ## Problem in S+ getting right value of lwd
    if(ax==1) do.call('lll',list(x, if(side==1)b else a,
         lwd=lwd,  col=col)) else
    do.call('lll',list(if(side==2)b else a, x, lwd=lwd, col=col))
  } else {
    lll <- gfun$segments
    if(ax==1) do.call('lll',list(x, a, x, b, lwd=lwd, xpd=frac<0, col=col))
    else do.call('lll',list(a, x, b, x, lwd=lwd, xpd=frac<0, col=col))
  }
  invisible(xlim)
}



score.binary <- function(..., fun=max, points=1:p, 
                         na.rm=funtext=='max', retfactor=TRUE) {

x <- list(...)
p <- length(x)
nam <- (as.character(sys.call())[-1])[1:p]
x <- matrix(unlist(x), ncol=p)
if(!missing(points)) {
  if(length(points)==1) points <- rep(points, p)
  if(length(points)!=p) stop('wrong length for points')
}
x <- x * rep(points, rep.int(nrow(x),p))
funtext <- as.character(substitute(fun))
if(funtext=='max' && !missing(points) && retfactor)
  warning('points do not matter for fun=max with retfactor=T\nas long as they are in ascending order')
if(!missing(retfactor) && retfactor && funtext!='max')
  stop('retfactor=T only applies to fun=max')
if(.R.) {
  funargs <- as.list(args(fun))
  funargs <- funargs[-length(funargs)]
  if(any(names(funargs)=='na.rm')) funargs$na.rm <- na.rm
  formals(fun) <- funargs
} else fun$na.rm <- na.rm
xna <- apply(x, 1, function(x) any(is.na(x)))
x <- apply(x, 1, fun)
if(!na.rm) x[x==0 & xna] <- NA

if(retfactor && funtext=='max') 
  factor(x, c(0,points), c("none",nam)) else x
}


sedit <- function(text, from, to, test=NULL, wild.literal=FALSE) {
to <- rep(to, length=length(from))
for(i in 1:length(text)) {
  s <- text[i]
  if(length(s)) for(j in 1:length(from)) {
    old <- from[j]
    front <- back <- FALSE
    if(!wild.literal) {
      if(substring(old,1,1)=='^') { front <- TRUE; old <- substring(old,2) }
      if(substring(old,nchar(old))=='$') { 
        back <- TRUE; old <- substring(old, 1, nchar(old)-1)
      }
    }
    new <- to[j]

    lold <- nchar(old)
    if(lold > nchar(s)) next

    ex.old <- substring(old, 1:lold, 1:lold)
    if(!wild.literal && any(ex.old=='*')) 
      s <- replace.substring.wild(s, old, new, test=test, front=front, back=back)
    else {
      l.s <- nchar(s)
      is <- 1:(l.s-lold+1)
      if(front) is <- 1
      ie <- is + lold - 1
      if(back) ie <- l.s
      ss <- substring(s, is, ie)
      k <- ss==old
      if(!any(k)) next
      k <- is[k]
      substring2(s, k, k+lold-1) <- new
    }
  }
  text[i] <- s
}
text
}

substring.location <- function(text, string, restrict) {
if(length(text)>1) stop('only works with a single character string')
l.text <- nchar(text)
l.string <- nchar(string)
if(l.string > l.text) return(list(first=0,last=0))
if(l.string==l.text)  return(if(text==string)list(first=1,last=l.text) else 
  list(first=0,last=0))

is <- 1:(l.text-l.string+1)
ss <- substring(text, is, is+l.string-1)
k <- ss==string
if(!any(k)) return(list(first=0,last=0))
k <- is[k]
if(!missing(restrict)) k <- k[k>=restrict[1] & k<=restrict[2]]
if(length(k)==0) return(list(first=0,last=0))
list(first=k, last=k+l.string-1)
}

#if(version$major < 5)  14Sep00
substring2 <- substring
'substring2<-' <- function(text, first, last=100000, value) {

if(is.character(first)) {
  if(!missing(last)) stop('wrong # arguments')
  return(sedit(text, first, value))  ## value was setto 25May01
}

lf <- length(first)

if(length(text)==1 && lf > 1) {
	if(missing(last)) last <- nchar(text)
	last <- rep(last, length=lf)
	for(i in 1:lf) {
	  text <- paste(if(first[i]>1) 
            substring(text, 1, first[i]-1), value,
	    substring(text, last[i]+1), sep='')
	  if(i < lf) {
	    j <- (i+1):lf
	    w <- nchar(value) - (last[i]-first[i]+1)
	    first[j] <- first[j] + w  
	    last[j] <- last[j] +  w
	  }
	}
	return(text)
}
text <- paste(ifelse(first>1,substring(text, 1, first-1),''), value,
              substring(text, last+1), sep='')
text
}

if(!.R. && !exists('substring<-')) assign('substring<-',substring2)
#!R 25May01

replace.substring.wild <- function(text, old, new, test=NULL, 
                                   front=FALSE, back=FALSE) {

if(length(text)>1) stop('only works with a single character string')

if(missing(front) && missing(back)) {
  if(substring(old,1,1)=='^') { front <- TRUE; old <- substring(old,2) }
  if(substring(old, nchar(old))=='$') {
    back <- TRUE
    old <- substring(old, 1, nchar(old)-1)
  }
}
if((front || back) && old!='*') 
  stop('front and back (^ and $) only work when the rest of old is *')

star.old <- substring.location(old,'*')
if(length(star.old$first)>1) stop('does not handle > 1 * in old')
if(sum(star.old$first)==0) stop('no * in old')
star.new <- substring.location(new,'*')
if(length(star.new$first)>1) stop('cannot have > 1 * in new')

if(old=='*' && (front | back)) {
  if(front && back) stop('may not specify both front and back (or ^ and $) with old=*')
  if(length(test)==0) stop('must specify test= with old=^* or *$')
  et <- nchar(text)
  if(front) { st <- rep(1, et); en <- et:1 } else { st <- 1:et; en <- rep(et,et) }
  qual <- test(substring(text, st, en))
  if(!any(qual)) return(text)
  st <- (st[qual])[1]
  en <- (en[qual])[1]
  text.before <- if(st==1)'' else substring(text, 1, st-1)
  text.after  <- if(en==et)'' else substring(text, en+1, et)
  text.star   <- substring(text, st, en)
  new.before.star <- if(star.new$first>1) 
    substring(new, 1, star.new$first-1) else ''
  new.after.star <- if(star.new$last==length(new))'' else
    substring(new, star.new$last+1)
  return(paste(text.before, new.before.star, text.star, new.after.star,
               text.after, sep=''))
}
old.before.star <- if(star.old$first==1)'' else substring(old, 1, star.old$first-1)
old.after.star  <- if(star.old$last==nchar(old))'' else
  substring(old, star.old$first+1)

if(old.before.star=='') loc.before <- list(first=0, last=0) else {
  loc.before <- substring.location(text, old.before.star)
  loc.before <- list(first=loc.before$first[1], last=loc.before$last[1])
}
if(sum(loc.before$first+loc.before$last)==0) return(text)

loc.after <- if(old.after.star=='') list(first=0, last=0) else {
  la <- substring.location(text, old.after.star, 
          restrict=c(loc.before$last+1,1e10))
  lastpos <- length(la$first)
  la <- list(first=la$first[lastpos], last=la$last[lastpos])
  if(la$first+la$last==0) return(text)
  la
  }

loc.star <- list(first=loc.before$last+1, 
                 last=if(loc.after$first==0) nchar(text) else loc.after$first-1)
star.text <- substring(text, loc.star$first, loc.star$last)
if(length(test) && !test(star.text)) return(text)

if(star.new$first==0)
  return(paste(if(loc.before$first>1)substring(text,1,loc.before$first-1),
               new, sep=''))

new.before.star <- if(star.new$first==1)'' else substring(new, 1, star.new$first-1)
new.after.star  <- if(star.new$last==nchar(new)) '' else substring(new, star.new$first+1)

paste(if(loc.before$first>1)substring(text,1,loc.before$first-1),
      new.before.star,
      substring(text,loc.star$first,loc.star$last),
      new.after.star,
      if(loc.after$last<nchar(text) && loc.after$last>0) 
        substring(text,loc.after$last+1),  sep='')
}

# Some functions useful as test= arguments to replace.substring.wild, sedit
numeric.string <- function(string) {
#.Options$warn <- -1  6Aug00
oldopt <- options(warn=-1)
on.exit(options(oldopt))
!is.na(as.numeric(string))
} 

all.digits <- function(string) {
k <- length(string)
result <- logical(k)
for(i in 1:k) {
  st <- string[i]
  ls <- nchar(st)
  ex <- substring(st, 1:ls, 1:ls)
  result[i] <- all(match(ex,c('0','1','2','3','4','5','6','7','8','9'),nomatch=0)>0)
}
result
}

show.pch <- function(object=par('font')) {
plot(0,0,xlim=c(-1,11),ylim=c(0,26),type='n',axes=FALSE,xlab='',ylab='')
j <- -1
for(i in 0:253) {
  if(i %% 25==0) {j <- j+1; k <- 26}
  k <- k-1
  points(j, k, pch=i, font=object)
  text(j+.45, k, i)
}
invisible()
}


character.table <- function(font=1) {
# Prints numeric equivalents to all latin characters
# Usage: graphsheet(orientation = "portrait")
#        character.table()
# Print the resulting graphsheet.  The printed version doesn't allways
# corresponds to the screen display.  The character on line "xy" and column "z"
# of the table has code "xyz".
# These codes can be used as any other characters. e.g.
#  title("\347\340 et \340")
# As the command line window of Splus can't print special characters
#  cat("\347\340 et \340")
# will not print the special characters, at least under 4.5 and under 2000.
#
# Author:
# Pierre Joyet / Aktuariat                  pierre.joyet@bluewin.ch
  v <- 40:377
  v <- v[v %% 100 < 80 & v %% 10 < 8]
  par(mar = c(5, 5, 4, 2) + 0.1)
  plot(0:7, seq(4, 31, length = 8), type = "n", axes = FALSE, xlab = "",
       ylab = "")
  k <- 1
  for(i in 4:31)
    for(j in 0:7) {
      text(j, 35 - i, eval(parse(text = paste("\"\\", v[k], "\"",
                                   sep = ""))), font = font)
      k <- k + 1
    }
  text(0:7, rep(33, 7), as.character(0:7), font = 3)
  text(rep(-1, 28), 31:4, as.character(c(4:7, 10:17, 20:27, 30:37)),
       font = 3)
  invisible()
}

show.col <- function(object=NULL) {
  plot(0,0,xlim=c(-1,10),ylim=c(0,10),type='n',axes=FALSE,xlab='',ylab='')
  j <- -1
  for(i in 0:99) {
    if(i %% 10==0) {j <- j+1; k <- 10}
    k <- k-1
    points(j, k, pch=15, col=i, cex=3)
    text(j+.45, k, i)
  }
  invisible()
}
#FEH version of solve with argument tol passed to qr
#8 Apr 91

solvet <- function(a, b, tol=1e-9)
{
	if(!is.list(a))
		a <- qr(a, tol=tol)
	if(a$rank < ncol(a$qr))
		stop("apparently singular matrix")
	if(missing(b)) {
		b <- a$qr
		db <- dim(b)
		if(diff(db))
			stop("matrix inverse only for square matrices")
		b[] <- rep(c(1, rep(0, db[1])), length = prod(db))
	}
	qr.coef(a, b)
}
##S function somers2
##
##    Calculates concordance probability and Somers'  Dxy  rank  correlation
##    between  a  variable  X  (for  which  ties are counted) and a binary
##    variable Y (having values 0 and 1, for which ties are not  counted).
##    Uses short cut method based on average ranks in two groups.
## 
##    Usage:
## 
##         somers2(X,Y)
##
##    Returns vector whose elements are C Index, Dxy, n and missing, where
##    C Index is the concordance probability and Dxy=2(C Index-.5).
##
##    F. Harrell 28 Nov 90     6 Apr 98: added weights

somers2 <- function(x, y, weights=NULL, normwt=FALSE, na.rm=TRUE) {
  if(length(y)!=length(x))stop("y must have same length as x")
  y <- as.integer(y)
  wtpres <- length(weights)
  if(wtpres && (wtpres != length(x)))
	stop('weights must have same length as x')

  if(na.rm) {
	miss <- if(wtpres) is.na(x + y + weights) else is.na(x + y)
	nmiss <- sum(miss)
	if(nmiss>0)	{
	  miss <- !miss
	  x <- x[miss]
	  y <- y[miss]
	  if(wtpres) weights <- weights[miss]
	}
  } else nmiss <- 0
		
  u <- sort(unique(y))
  if(any(y %nin% 0:1)) stop('y must be binary')  ## 7dec02
  if(wtpres) {
	if(normwt) weights <- length(x)*weights/sum(weights)
	n <- sum(weights)
  } else n <- length(x)

  if(n<2)stop("must have >=2 non-missing observations")

  n1 <- if(wtpres)sum(weights[y==1]) else sum(y==1)
  if(n1==0 || n1==n) return(c(C=NA,Dxy=NA,n=n,Missing=nmiss))  ## 7dec02
  ## added weights > 0 30Mar00
  mean.rank <- if(wtpres) mean(wtd.rank(x, weights, na.rm=FALSE)[weights >
                                                      0 & y==1]) else 
                 mean(rank(x)[y==1])
  c.index <- (mean.rank - (n1+1)/2)/(n-n1)
  dxy <- 2*(c.index-.5)
  r <- c(c.index, dxy, n, nmiss)
  names(r) <- c("C","Dxy","n","Missing")
  r
}

if(FALSE) rcorrs <- function(x, y, weights=rep(1,length(y)),
				   method=c('exact','bin'), nbin=1000, na.rm=TRUE) {
## Experimental function - probably don't need

  method <- match.arg(method)
  
  if(na.rm) {
	s <- !is.na(x + oldUnclass(y) + weights)
	x <- x[s]; y <- y[s]; weights <- weights[s]
  }
  
  n <- length(x)
  if(missing(method)) method <- if(n < 1000) 'exact' else 'bin'

  y <- as.category(y); nly <- length(levels(y))
  if(method=='bin') {
	r <- range(x); d <- r[2] - r[1]
	x <- 1 + trunc((nbin-1)*(x - r[1])/d)
 
	xy <- y*nbin + x

	## Code below is lifted from rowsum()
	storage.mode(weights) <- "double"
    temp <- if(.R.)
      .C('R_rowsum', dd=as.integer(dd),
         as.double(max(1,weights)*n),
         x=weights, as.double(xy), PACKAGE='base') else
    .C("S_rowsum",
       dd = as.integer(c(n,1)),
       as.double(max(1,weights)*n),
       x = weights,
       as.double(xy))   ## 3Jun01
	new.n <- temp$dd[1]
	weights <- temp$x[1:new.n]

	uxy <- unique(xy)
	x <- uxy %% nbin
	y <- (uxy - x)/nbin
	n <- length(x)
 }
list(x=x, y=y, weights=weights)

storage.mode(x) <- "single"
storage.mode(y) <- "single"
storage.mode(event) <- "logical"

## wcidxy doesn't exist yet
z <- .Fortran("wcidxy",as.single(x),as.single(y),as.integer(weights),as.integer(n),
			  nrel=double(1),nconc=double(1),nuncert=double(1),
			  c.index=double(1),gamma=double(1),sd=double(1),as.logical(outx))
r <- c(z$c.index,z$gamma,z$sd,n,z$nrel,z$nconc,z$nuncert)
names(r) <- c("C Index","Dxy","S.D.","n","missing","uncensored",
			  "Relevant Pairs",	"Concordant","Uncertain")
r
}









#Spearman correlation test (p=1) or Spearman test extended by adding
#rank(x)^2 to model (p=2)
#F Harrell  30Sep90

spearman.test <- function(x,y,p=1)	{

x <- as.numeric(x); y <- as.numeric(y)  ## 17Jul97
if(length(x)!=length(y))stop("length of x must = length of y")
nomiss <- !is.na(x+y)
n <- sum(nomiss)
if(n<3)stop("fewer than 3 non-missing x-y pairs")
if(!(p==1 | p==2))stop("p must be 1 or 2")
x <- x[nomiss]
x <- rank(x)
y <- y[nomiss]
y <- rank(y)
sst <- sum((y-mean(y))^2)
if(p==2)x <- cbind(x,x^2)
sse <- sum((lsfit(x,y)$residuals)^2)
rsquare <- 1-sse/sst
df2 <- n-p-1
fstat <- rsquare/p/((1-rsquare)/df2)
pvalue <- 1-pf(fstat,p,df2)
x <- c(rsquare,fstat,p,df2,pvalue,n)
names(x) <- c("Rsquare","F","df1","df2","pvalue","n")
x
									}


spower <- function(rcontrol, rinterv, rcens, nc, ni,
					test=logrank, nsim=500, alpha=.05, pr=TRUE) {

crit <- qchisq(1-alpha, 1)
group <- c(rep(1,nc), rep(2,ni))
nexceed <- 0

for(i in 1:nsim) {
  if(pr && i %% 10 == 0) cat(i,'')
  yc <- rcontrol(nc)
  yi <- rinterv(ni)
  cens <- rcens(nc+ni)
  y <- c(yc, yi)
  S <- cbind(pmin(y,cens), 1*(y <= cens))
  nexceed <- nexceed + (test(S, group) > crit)
}
nexceed/nsim
}
  

Quantile2 <- function(scontrol, hratio, 
					  dropin=function(times)0, 
					  dropout=function(times)0,
					  m=7500, tmax, qtmax=.001, mplot=200, pr=TRUE, ...) {

## Solve for tmax such that scontrol(t)=qtmax
dlist <- list(...)
k <- length(dlist) && !is.null(dlist)
f    <- if(k) function(x, scontrol, qt, ...) scontrol(x, ...) - qt
else function(x, scontrol, qt) scontrol(x) - qt

if(missing(tmax)) {
  if(k) tmax <- uniroot(f, c(0,1e9), scontrol=scontrol, qt=qtmax, ...)$root
	else tmax <- uniroot(f, c(0,1e9), scontrol=scontrol, qt=qtmax)$root
}
if(pr) cat('\nInterval of time for evaluating functions:[0,',
		   format(tmax),']\n\n')

## Generate sequence of times to use in all approximations and sequence
## to use for plot method

times <- seq(0, tmax, length=m)
tim   <- seq(0, tmax, length=mplot)
tinc  <- times[2]

## Approximate hazard function for control group
sc <- scontrol(times, ...)    
hc <- diff(-logb(sc))
hc <- c(hc, hc[m-1])/tinc  ## to make length=m

## hazard function for intervention group
hr <- rep(hratio(times), length=m)
hi <- hc*hr

## hazard for control group with dropin
di  <- rep(dropin(times),length=m)
hc2 <- (1-di)*hc + di*hi

## hazard for intervention group with dropout
do  <- rep(dropout(times),length=m)
hi2 <- (1-do)*hi + do*hc

## survival for intervention group
si  <- exp(-tinc*cumsum(hi))

## Compute contaminated survival function for control and intervention
sc2 <- if(any(di>0))exp(-tinc*cumsum(hc2)) else sc
si2 <- exp(-tinc*cumsum(hi2))


## Store all functions evaluated at shorter times vector (tim), for
## plotting
asing <- if(.R.)function(x)x else as.single
sc.p  <- asing(approx(times, sc,  xout=tim)$y)
hc.p  <- asing(approx(times, hc,  xout=tim)$y)
sc2.p <- asing(approx(times, sc2, xout=tim)$y)
hc2.p <- asing(approx(times, hc2, xout=tim)$y)

si.p  <- asing(approx(times, si,  xout=tim)$y)
hi.p  <- asing(approx(times, hi,  xout=tim)$y)
si2.p <- asing(approx(times, si2, xout=tim)$y)
hi2.p <- asing(approx(times, hi2, xout=tim)$y)

dropin.p  <- asing(approx(times, di, xout=tim)$y)
dropout.p <- asing(approx(times, do, xout=tim)$y)
hratio.p  <- asing(approx(times, hr, xout=tim)$y)
hratio2.p <- hi2.p/hc2.p

tim       <- asing(tim)

plot.info <- list(
  "C Survival"                   =list(Time=tim,Survival=sc.p),
  "I Survival"                   =list(Time=tim,Survival=si.p),
  "C Survival w/Dropin"          =list(Time=tim,Survival=sc2.p),
  "I Survival w/Dropout"         =list(Time=tim,Survival=si2.p),
  "C Hazard"                     =list(Time=tim,Hazard=hc.p),
  "I Hazard"                     =list(Time=tim,Hazard=hi.p),
  "C Hazard w/Dropin"            =list(Time=tim,Hazard=hc2.p),
  "I Hazard w/Dropout"           =list(Time=tim,Hazard=hi2.p),
  "Dropin"                       =list(Time=tim,Probability=dropin.p),
  "Dropout"                      =list(Time=tim,Probability=dropout.p),
  "Hazard Ratio"                 =list(Time=tim,Ratio=hratio.p),
  "Hazard Ratio w/Dropin+Dropout"=list(Time=tim,Ratio=hratio2.p))

## Create S-Plus functions for computing random failure times for
## control and intervention subject to dropin, dropout, and hratio

r <- function(n, what=c('control','intervention'), 
			  times, csurvival, isurvival) {
  what <- match.arg(what)
  approx(if(what=='control')csurvival else isurvival, 
		 times, xout=runif(n), rule=2)$y
}
asing <- if(.R.)function(x)x else as.single
formals(r) <- list(n=integer(0),
                   what=c('control','intervention'),
                   times=asing(times), csurvival=asing(sc2),
                   isurvival=asing(si2))

structure(r, plot.info=plot.info, 
		  dropin=any(di>0), dropout=any(do>0),
		  class='Quantile2')
}

print.Quantile2 <- function(x, ...) {
  attributes(x) <- NULL
  print(x)
  invisible()
}

plot.Quantile2 <- function(x, 
						   what=c('survival','hazard','both','drop','hratio',
							 'all'), dropsep=FALSE,
						   lty=1:4, col=1, xlim, ylim=NULL, 
						   label.curves=NULL, ...) {
  what <- match.arg(what)
  pi <- attr(x, 'plot.info')
  if(missing(xlim)) xlim <- c(0,max(pi[[1]][[1]]))
  dropin  <- attr(x, 'dropin')
  dropout <- attr(x, 'dropout')
  i <- c(1,2,if(dropin)3,if(dropout)4)
  if(what %in% c('survival','both','all')) {
	if(dropsep && (dropin|dropout)) {
	  labcurve(pi[1:2], pl=TRUE, lty=lty, col=col, xlim=xlim, ylim=ylim,
			   opts=label.curves)
	  labcurve(pi[i[-(1:2)]], pl=TRUE, lty=lty, col=col, xlim=xlim, ylim=ylim,
			   opts=label.curves)
	} else
	labcurve(pi[i], pl=TRUE, lty=lty, col=col, xlim=xlim, ylim=ylim,
			 opts=label.curves)
  }

  if(what %in% c('hazard','both','all')) {
	if(dropsep && (dropin|dropout)) {
	  labcurve(pi[5:6], pl=TRUE, lty=lty, col=col, xlim=xlim, ylim=ylim,
			   opts=label.curves)
	  labcurve(pi[4+i[-(1:2)]], pl=TRUE, lty=lty, col=col, xlim=xlim, ylim=ylim,
			   opts=label.curves)
	} else
	labcurve(pi[4+i], pl=TRUE, lty=lty, col=col, xlim=xlim, ylim=ylim,
			 opts=label.curves)
  }

  if(what=='drop' || (what=='all' && (dropin | dropout))) {
	i <- c(if(dropin)9, if(dropout)10)
	if(length(i)==0) i <- 10
    labcurve(pi[i], pl=TRUE, lty=lty, col=col, xlim=xlim, ylim=ylim,
			 opts=label.curves)
  }

  if(what %in% c('hratio','all')) {
    i <- c(11, if(dropin|dropout) 12)
    labcurve(pi[i], pl=TRUE, lty=lty, col=col, xlim=xlim, ylim=ylim,
			 opts=label.curves)
  }

  invisible()
}

logrank <- function(S, group) {
  y     <- S[,1]
  event <- S[,2]
  i     <- order(-y)
  y     <- y[i]
  event <- event[i]
  group <- group[i]
  x     <- cbind(group==1, group==2, (group==1)*event, (group==2)*event)
  s     <- rowsumFast(x, y, FALSE)
  nr1 <- cumsum(s[,1])
  nr2 <- cumsum(s[,2])
  d1  <- s[,3]
  d2  <- s[,4]
  rd  <- d1+d2
  rs  <- nr1+nr2-rd
  n   <- nr1+nr2
  oecum <- d1 - rd*nr1/n
  vcum  <- rd * rs * nr1 * nr2 / n / n / (n-1)
  sum(oecum)^2 / sum(vcum,na.rm=TRUE)
}

Weibull2 <- function(times, surv) {

z1 <- -logb(surv[1])
z2 <- -logb(surv[2])
t1 <- times[1]
t2 <- times[2]
gamma <- logb(z2/z1)/logb(t2/t1)
alpha <- z1/(t1^gamma)

g <- function(times, alpha, gamma) {exp(-alpha*(times^gamma))}
formals(g) <- list(times=NULL, alpha=alpha, gamma=gamma)
g
}

#Function to fit a Gompertz survival distribution to two points
#The function is S(t) = exp[-(1/b)exp(a+bt)]
#Returns a list with components a and b, and a function for
#generating S(t) for a vector of times

Gompertz2 <- function(times, surv) {

z1 <- logb(-logb(surv[1]))
z2 <- logb(-logb(surv[2]))
t1 <- times[1]
t2 <- times[2]
b  <- (z2-z1)/(t2-t1)
a  <- z1 + logb(b)-b*t1

g <- function(times, a, b) {exp(-exp(a+b*times)/b)}
formals(g) <- list(times=NULL, a=a, b=b)
g
}

Lognorm2 <- function(times, surv) {

z1 <- qnorm(1-surv[1])
z2 <- qnorm(1-surv[2])
sigma <- logb(times[2]/times[1])/(z2-z1)
mu    <- logb(times[1]) - sigma*z1

g <- function(times, mu, sigma) {1 - pnorm((logb(times)-mu)/sigma)}
formals(g) <- list(times=NULL, mu=mu, sigma=sigma)
g
}
#Function to source(x) if x is given, or source(last x given) otherwise
#Last x is stored in options() last.source.   x is unquoted with .s omitted.
#Author: Frank Harrell  19May91

src <- function(x)			{
if(!missing(x))		{
	y <- paste(as.character(substitute(x)),".s",sep="")
	options(last.source=y, TEMPORARY=FALSE)
			}
else y <- options()$last.source
if(is.null(y)) stop("src not called with file name earlier")

source(y)
cat(y, "loaded\n")
invisible()
					}
##This has code from Bill Dunlap's "set.work" function
if(.R.) {
  store <- function(object, name=as.character(substitute(object)), 
                    where=if(under.unix || .SV4.)".Data" else "_Data")
    stop('function not available for R')
  stores <- function(...) stop('function not available for R')
} else {
  store <- function(object, name=as.character(substitute(object)), 
                    where=if(under.unix || .SV4.)".Data" else "_Data"){

  if(missing(object)) {
#    if(.R.) attach(NULL, name='.GlobalTemp', pos=1)
	temp <- if(under.unix) paste(".Data.temp",
								 unix("echo $$"), sep="") else tempfile()
	sys(paste("mkdir",temp), minimized=FALSE)
    if(.SV4.) sys(paste('mkdir ',temp,
                        if(under.unix)'/' else '\\',
                        '__Meta',sep=''))  ## 20jun02
	attach(temp, 1)
	options(.store.temp=temp, TEMPORARY=FALSE)
	l <- function() { detach(1, FALSE); 
					  sys(paste(if(under.unix)"rm -r" else "deltree /Y",.Options$.store.temp),
						  minimized=TRUE) }
	assign(".Last", l, where=1)
	return(invisible())
  }

  assign(name,object,where=where,immediate=TRUE)
  invisible()
}

  stores <- function(...) {
    nams <- as.character(sys.call())[-1]
    dotlist <- list(...)
    for(i in 1:length(nams)) assign(nams[i], dotlist[[i]],
                                    where=if(under.unix || .SV4.)".Data" else "_Data",
                                    immediate=TRUE)
    invisible()
  }
  NULL
}

storeTemp <- if(.R.) function(object,
                              name=deparse(substitute(object))) {
    pos <- match('.GlobalTemp', search())
    if(is.na(pos)) {
      attach(NULL,name='.GlobalTemp')
      pos <- match('.GlobalTemp', search())
    }
    assign(name, object, pos)
    invisible()
  } else function(object, name=deparse(substitute(object))) {
    assign(name, object, frame=0)
    invisible()
  }


#Substitute y when element of x is missing
#also return an attribute "substi.source"=vector of var names and NAs
substi <- function(x,y,pr=TRUE)		{

if(length(x)!=length(y))stop("lengths of x and y are different")
nf <- is.category(x)+is.category(y)
if(nf==1)stop("both x and y must be category variables if either is")

isna <- is.na(x)
vnames <- sys.call()[c(2,3)]
if(pr)	{
cat("Variables:",vnames,"\n")
cat("Used first  variable:",sum(!is.na(x)),"\n")
cat("Used second variable:",sum(is.na(x) & !is.na(y)),"\n") }
if(nf)					{
	levs <- unique(c(levels(x),levels(y)))
	x <- as.character(x)
	y <- as.character(y)
	x[isna] <- y[isna]
	x <- factor(x,levs)
	y <- factor(y,levs)		}
else x[isna] <- y[isna]
ss <- ifelse(isna & is.na(y),NA,ifelse(isna,2,1))
attr(ss,"names") <- NULL
ss <- factor(ss,labels=vnames)
if(pr)cat("Obs:",sum(!is.na(x))," Obs missing:",sum(is.na(x)),"\n")
attr(x,"substi.source") <- ss
attr(x,'class') <- c("substi",attr(x,'class'))
x
				}

substi.source <- function(x) attr(x,"substi.source")

"[.substi" <- function(x, ...) {
  ss <- attr(x,"substi.source")
  ats <- attributes(x)
  ats$dimnames <- ats$dim <- ats$names <- ats$substi.source <-
    attr(x,'class') <- NULL
  x <- (x)[...]
  attributes(x) <- ats
  attr(x,"substi.source") <- ss[...]
  x
}

print.substi <- function(x, ...) {

        i <- oldUnclass(attr(x, "substi.source"))
        if(!length(i)) {
                print.default(x)
                return(invisible())
        }
        if(is.factor(x))
                w <- as.character(x)
        else w <- format(x)
        names(w) <- names(x)
        w[i==2] <- paste(w[i==2], "*", sep = "")
        attr(w, "label") <- attr(w, "substi.source") <- attr(w, "class") <- NULL
        print.default(w, quote = FALSE)
        invisible()
}

as.data.frame.substi <- function(x, row.names = NULL, optional = FALSE, ...)
{
        nrows <- length(x)
        if(!length(row.names)) {
# the next line is not needed for the 1993 version of data.class and is
# included for compatibility with 1992 version
                if(length(row.names <- names(x)) == nrows && !any(duplicated(
                        row.names))) {
                }
                else if(optional)
                        row.names <- character(nrows)
                else row.names <- as.character(1:nrows)
        }
        value <- list(x)
        if(!optional)
                names(value) <- deparse(substitute(x))[[1]]
        structure(value, row.names=row.names, class='data.frame')
}

#note: ars may always be T
#30Oct00: if(under.unix)183 -> if(F)
summary.formula <-
  function(formula, data, subset, na.action, 
           fun=NULL,
           method=c('response','reverse','cross'),
           overall=method=='response'|method=='cross', 
           continuous=10, na.rm=method=='reverse', g=4, 
           quant=c(.025,.05,.125,.25,.375,.5,.625,.75,.875,.95,.975),
           nmin=0, test=FALSE,
           conTest=function(group,x) {
             st <- spearman2(group,x)
             list(P=st['P'], stat=st['F'],
                  df=st[c('df1','df2')],
                  testname=if(st['df1']==1)'Wilcoxon' else
                  'Kruskal-Wallis',
                  statname='F', latexstat='F_{df}',
                  plotmathstat='F[df]')
           },
           catTest=function(tab) {
             st <- if(!is.matrix(tab) || nrow(tab) < 2)
               list(p.value=NA, statistic=NA, parameter=NA) else
             chisq.test(tab, correct=FALSE)
             list(P=st$p.value, stat=st$statistic,
                  df=st$parameter,
                  testname='Pearson', statname='Chi-square',
                  latexstat='\\chi^{2}_{df}',
                  plotmathstat='chi[df]^2')
           }, ...) {
    call <- match.call()
    missmethod <- missing(method)   ## needed for R  9jul02
    method <- match.arg(method)
    
    X <- match.call(expand=FALSE)
    X$fun <- X$method <- X$na.rm <- X$g <- 
      X$overall <- X$continuous <- X$quant <- X$nmin <- X$test <-
        X$conTest <- X$catTest <- X$... <- NULL
    if(missing(na.action)) X$na.action <- na.retain
    Terms <- if(missing(data)) terms(formula,'stratify') else
    terms(formula,'stratify',data=data)
    X$formula <- Terms
    X[[1]] <- as.name("model.frame")
    
    X <- eval(X, sys.parent())
  
    Terms <- attr(X,"terms")
    resp <- attr(Terms,"response")
    
    if(resp==0 && missmethod) method <- 'reverse'
  
    if(test && method!='reverse')
      stop('test=TRUE only allowed for method="reverse"')
    if(test && .R.) require('ctest')

    if(method!='reverse' && resp!=1) 
      stop("must have a variable on the left hand side of the formula")
    nact <- attr(X, "na.action")
    nvar <- ncol(X)-1
    strat <- attr(Terms,'specials')$stratify
    if(length(strat)) {
      if(method!='response') 
        stop('stratify only allowed for method="response"')
      temp <- untangle.specials(Terms,'stratify')
      strat.name <- if(.R.) var.inner(Terms)[temp$terms] else
      attr(terms.inner(Terms),'term.labels')[temp$terms]
      strat <- if(length(temp$vars)==1) as.factor(X[[temp$vars]]) else
      stratify(X[,temp$vars])
      strat.label <- if(length(l <- attr(X[,temp$vars[1]],'label'))) l else 
      strat.name
      X[[temp$vars]] <- NULL   # remove strata factors
    } else {
      strat <- factor(rep('',nrow(X)))
      strat.name <- strat.label <- ''
    }
    nstrat <- length(levels(strat))
    
  if(resp>0) {
    Y <- X[[resp]]
    yname <- if(.R.) as.character(attr(Terms,'variables'))[2] else
    as.character(attr(Terms, "variables"))[1]  ## 25May01
    ylabel <- if(length(laby <- attr(Y,'label'))) laby else yname
    if(!is.matrix(Y)) Y <- matrix(Y, dimnames=list(names(Y),yname))
  } else { yname <- ylabel <- NULL }
    
    if(method!='reverse') {
      if(!length(fun)) {   # was missing(fun) 25May01
        fun <- function(y) apply(y, 2, mean)
        uy <- unique(Y[!is.na(Y)])  # fixed 16Mar96
        r <- range(uy, na.rm=TRUE)
        funlab <- if(length(uy)==2 && r[1]==0 & r[2]==1) "Fraction" else "Mean"
        funlab <- paste(funlab, 'of', yname)
      } else if(is.character(fun) && fun=='%') {
        fun <- function(y) {
          stats <- 100*apply(y, 2, mean)
          names(stats) <- paste(dimnames(y)[[2]],'%')
          stats
        }
        funlab <- paste('% of', yname)
      }
      ## Compute number of descriptive statistics per cell
      s <- if(inherits(Y,'Surv'))
        as.vector((1 * is.na(unclass(Y))) %*% rep(1, ncol(Y)) > 0) else
      ((if(is.character(Y)) Y==''|Y=='NA' else is.na(Y)) %*%
       rep(1,ncol(Y))) > 0
      ## Was is.na.Surv, is.Surv 30May01
      stats <- if(length(dim(Y))) fun(Y[!s,,drop=FALSE]) else fun(Y[!s])
      nstats <- length(stats)
      name.stats <- if(length(dn <- dimnames(stats))==2)
        as.vector(outer(dn[[1]],dn[[2]],FUN=function(a,b)paste(b,a))) else
      names(stats)

      if(length(fun)) {  # was !missing(fun) 25May01
        if(length(de <- deparse(fun)) == 2) { ## 27oct02
          de <- as.list(fun)
          de <- as.character(de[[length(de)]])
          funlab <- if(de[1] == 'apply')
            de[length(de)] else de[1]
          ## 2nd case is for simple function(x)mean(x) function
        } else funlab <- as.character(substitute(fun))
        ## funlab <- if(.R.)deparse(fun) else as.character(substitute(fun)) #25May01
        ## funlab <- funlab[length(funlab)] #handles fun=function(x)mean(x)
        ## chf <- if(.R.) as.character(as.list(fun)[[2]]) else
        ## as.character(fun[[2]])
        ## if(length(chf) > 3 && chf[1]=="apply") funlab <- chf[4]
        ## The preceeding gets "median" from function(y) apply(y, 2, median)
        ###  if(length(fun)==2 && length(fun[[2]])>1) funlab <- 
        ###  if(length(name.stats)==1) name.stats else funname
      }

      if(funlab=='') funlab <- yname

      if(length(name.stats)==0) {
        name.stats <- if(nstats==1) yname  else paste(yname ,1:nstats,sep='')
      }

      ##	if(nstats==1) funlab <- name.stats
      ##	if(length(laby) && !missing(fun)) funlab <- laby
    }

    if(method=='response') {
      X[[resp]] <- NULL   # remove response var
      s <- if(inherits(Y,'Surv'))
        as.vector((1 * is.na(unclass(Y))) %*% rep(1, ncol(Y)) > 0) else
      ((if(is.character(Y)) Y==''|Y=='NA' else is.na(Y)) %*% 
       rep(1,ncol(Y))) > 0
      ## was is.Surv(Y) ... is.na.Surv(Y)   25May01
      nmissy <- sum(s)
      if(nmissy) { X <- X[!s,,drop=FALSE]; Y <- Y[!s,,drop=FALSE]; strat <- strat[!s] }
      ##Compute total number of columns, counting n
      nc <- nstrat*(1+nstats)
      colname <- rep(c('N',name.stats),nstrat)
      rowname <- vname <- vlabel <- vunits <- res <- NULL
      dm <- dim(X)
      nx <- dm[2]
      n  <- dm[1]
      nlevels <- integer(nx)
      labels <- character(nx)
      units  <- labels  ## 28jan03

      i <- 0
      nams <- c(names(X), if(overall)'Overall')
      for(v in nams) {
        i <- i+1
        x <- if(v=='Overall') factor(rep('',n)) else X[[v]]
        labels[i] <- if(length(l <- attr(x,'label')))l else nams[i]
        units[i]  <- if(length(l <- attr(x,'units'))) l else ''  ## 28jan03
        if(!(ismc <- is.matrix(x))) {   ## 17Jan99
          s <- is.na(x)
          if(!is.category(x)) {
            xu <- unique(x[!s]); lu <- length(xu)
            x <- if(lu < continuous) {
              r <- range(xu)
              if(lu==2 && r[1]==0 && r[2]==1) 
                factor(x,labels=c('No','Yes')) else
              factor(x)}  else cut2(x, g=g, ...)
          }
          if(!na.rm && any(s)) {
            x <- na.include(x)
            if(.R.) levels(x)[is.na(levels(x))] <- 'NA'  ## 08may02
            ## R 1.5 and later has NA as level not 'NA', satisfies is.na
          }
          xlev <- levels(x)
          if(nmin > 0) { nn <- table(x); xlev <- names(nn)[nn >= nmin] }
        } else { ## 17Jan99
          xlev <- dimnames(x)[[2]]
          if(!length(xlev)) stop('matrix variables must have column dimnames')
          if(!is.logical(x)) {
            if(is.numeric(x)) x <- x==1 else {
              x <- structure(casefold(x),dim=dim(x))
              x <- x=='present' | x=='yes'
            }
          }
          if(nmin > 0) {
            nn <- apply(x, 2, sum, na.rm=TRUE)
            xlev <- xlev[nn >= nmin]
          }
        }
        nlevels[i] <- length(xlev)
        for(lx in xlev) {
          r <- NULL
          for(js in levels(strat)) {
            j <- if(ismc) strat==js  & x[,lx] else strat==js & x==lx ##17Jan99
            if(na.rm) j[is.na(j)] <- FALSE ##6Apr99
            nj <- sum(j)
            f <- if(nj) {
              statz <- unlist(fun(Y[j,,drop=FALSE]))
              ## 23apr03; had just let matrix replicate to fill
              ## Thanks: Derek Eder <derek.eder@neuro.gu.se>
              if(length(statz) != nstats)
                stop(paste('fun for stratum',lx,js,'did not return',
                           nstats, 'statistics'))
              matrix(statz, ncol=nstats, byrow=TRUE)
            } else rep(NA,nstats)
#            if(nj) prn(fun(Y[j,,drop=FALSE]))
#            f <- if(nj) matrix(unlist(fun(Y[j,,drop=FALSE])),ncol=nstats,byrow=TRUE)
#            else rep(NA,nstats)
            r <- c(r, nj, f)
          }
          res <- rbind(res, r)
        }
        rowname <- c(rowname, xlev)
        bl <- rep('',length(xlev)-1)
        vname <- c(vname,v,bl)
        vlabel <- c(vlabel,labels[i],bl)
        vunits <- c(vunits,units[i],bl)
      }
      rowname[rowname=='NA'] <- 'Missing'
      dimnames(res) <- list(rowname,colname)
      at <- list(formula=formula, call=call, n=n, nmiss=nmissy, yname=yname, 
                 ylabel=ylabel,
                 ycolname=if(length(d<-dimnames(Y)[[2]]))d else yname,
                 funlab=funlab,
                 vname=vname, vlabel=vlabel, nlevels=nlevels,
                 labels=labels, units=units, vunits=vunits,
                 strat.name=strat.name, strat.label=strat.label,
                 strat.levels=levels(strat))
      attributes(res) <- c(attributes(res), at)
      attr(res,'class') <- 'summary.formula.response'
      return(res)
    }

    if(method=='reverse') {
      if(resp) {
        group <- as.factor(X[[resp]])
        group.freq <- table(group)
        group.freq <- group.freq[group.freq>0]
        if(overall) group.freq <- c(group.freq,
                                    Combined=sum(group.freq))
      } else {
        group <- rep(0,nrow(X))
        group.freq <- NULL
      }
      nv <- ncol(X)-resp
      
      n <- integer(nv)
      type <- n
      nams <- names(X)
      comp <- vector("list",nv)
      names(comp) <- if(resp)nams[-1] else nams
      labels <- Units <- vector("character",nv)   ## Units 17sep02
      if(test) {
        testresults <- vector('list', nv)
        names(testresults) <- names(comp)
      }
      
      for(i in 1:nv) {
        w <- X[[resp+i]]
        if(length(attr(w,"label"))) labels[i] <- attr(w,"label")
        if(length(attr(w,'units'))) Units[i]  <- attr(w,'units')
        ## length added 7Jun01
        if(!is.matrix(w)) {
          if(!is.factor(w) && length(unique(w[!is.na(w)])) < continuous) 
            w <- as.factor(w)
          s <- !is.na(w)
          if(!na.rm && !all(s) && length(levels(w))) {  ## 9jul02 + 3 lines
            w <- na.include(w)
            if(.R.) levels(w)[is.na(levels(w))] <- 'NA'  ## 08may02
            s <- rep(TRUE,length(s))
          }
          n[i] <- sum(s)
          w <- w[s]
          g <- group[s]
          if(is.factor(w)) {
            tab <- table(w, g)
            if(test) testresults[[i]] <- catTest(tab)

            if(nrow(tab)==1) {  # 7sep02
              b <- casefold(dimnames(tab)[[1]],upper=TRUE)
              pres <- c('1','Y','YES','PRESENT')
              abse <- c('0','N','NO', 'ABSENT')
              jj <- match(b, pres, nomatch=0)
              if(jj > 0) bc <- abse[jj] else {
                jj <- match(b, abse, nomatch=0)
                if(jj > 0) bc <- pres[jj]
              }
              if(jj) {
                tab <- rbind(tab, rep(0, ncol(tab)))
                dimnames(tab)[[1]][2] <- bc
              }
            }
            if(overall) tab <- cbind(tab, Combined=apply(tab,1,sum))
            comp[[i]] <- tab
            type[i] <- 1
          } else {
            sfn <- function(x, quant) {
              o <- options(digits=10)
              ## 2sep02 so won't lose precision in quantile names
              on.exit(options(o))
              c(quantile(x,quant), Mean=mean(x), SD=sqrt(var(x)))
            }
            qu <- tapply(w, g, sfn, simplify=TRUE, quant)
            ## Added simplify=TRUE to work with R 7Jun01
            if(test) testresults[[i]] <- conTest(g, w)
            if(overall) qu$Combined <- sfn(w, quant)
            comp[[i]] <- matrix(unlist(qu),ncol=length(quant)+2,byrow=TRUE,
                                dimnames=list(names(qu),
                                  c(format(quant),'Mean','SD')))
            type[i] <- 2
          }
        } else {  ## matrix: multiple choice variables
          if(!is.logical(w)) {
            if(is.numeric(w)) w <- w==1 else {
              w <- structure(casefold(w),dim=dim(w))
              w <- w=='present' | w=='yes'
            }
          }
          n[i] <- nrow(w)
          g    <- as.factor(group)
          ncat <- ncol(w)
          tab <- matrix(NA, nrow=ncat, ncol=length(levels(g)),
                        dimnames=list(dimnames(w)[[2]], levels(g)))
          if(test) {
            pval <- numeric(ncat)
            names(pval) <- dimnames(w)[[2]]
            d.f. <- stat <- pval
          }
          for(j in 1:ncat) {
            tab[j,] <- tapply(w[,j], g, sum, simplify=TRUE, na.rm=TRUE)
            if(test) {
              tabj <- rbind(table(g)-tab[j,],tab[j,])
              st <- catTest(tabj)
              pval[j] <- st$P
              stat[j] <- st$stat
              d.f.[j] <- st$df
            }
          }
          if(test)
            testresults[[i]] <- list(P=pval, stat=stat, df=d.f.,
                                     testname=st$testname,
                                     statname=st$statname,
                                     latexstat=st$latexstat,
                                     plotmathstat=st$plotmathstat)
                                   
          ## Added simplify=TRUE for R 7Jun01
          if(overall) tab <- cbind(tab, Combined=apply(tab,1,sum))
          comp[[i]] <- tab
          type[i]   <- 3
        }
      }
	  
      labels <- ifelse(nchar(labels), labels, names(comp))
      return(structure(list(stats=comp, type=type, 
                            group.name=if(resp)nams[1] else NULL,
                            group.label=ylabel,
                            group.freq=group.freq,
                            labels=labels, units=Units,
                            quant=quant, N=sum(!is.na(group)), n=n,
                            testresults=if(test)testresults else NULL,
                            call=call, formula=formula), 
                       class="summary.formula.reverse"))
    }
    

    if(method=='cross') {
      X[[resp]] <- NULL
      Levels <- vector("list",nvar)
      nams <- names(X)
      names(Levels) <- names(X)
      labels <- character(nvar)
      for(i in 1:nvar) {
        xi <- X[[i]]
        ## 15feb03:
        if(inherits(xi,'mChoice')) xi <- as.character(xi) else
        if(is.matrix(xi) && ncol(xi) > 1) 
          stop('matrix variables not allowed for method="cross"')
        labels[i] <- if(length(l <- attr(xi,'label')))l else nams[i]
        if(!is.factor(xi) && length(unique(xi[!is.na(xi)]))>=continuous)
          xi <- cut2(xi, g=g, ...)
        X[[i]] <- na.include(as.factor(xi))
        if(.R.) levels(X[[i]])[is.na(levels(X[[i]]))] <- 'NA'  ## 08may02
        
        Levels[[i]] <- c(levels(X[[i]]),if(overall)"ALL")
      }
    
      ##Make a data frame with all combinations of values (including those
      ##that don't exist in the data, since trellis needs them)

      df <- expand.grid(Levels)
      nl <- nrow(df)
      N  <- Missing <- integer(nl)
      na <- is.na(Y %*% rep(1,ncol(Y)))
      S  <- matrix(NA, nrow=nl, ncol=nstats, dimnames=list(NULL,name.stats))

      ## 23apr03
      chk <- function(z, nstats) {
        if(length(z) != nstats)
          stop(paste('fun did not return',nstats,
                     'statistics for a stratum'))
        z
      }

      if(nvar==1) {
        df1 <- as.character(df[[1]]); x1 <- X[[1]]
        for(i in 1:nl) {
          s <- df1[i]=='ALL' | x1==df1[i]
          w <- s & !na
          N[i] <- sum(w)
          Missing[i] <- sum(na[s])
          S[i,] <- if(any(w))chk(fun(Y[w,,drop=FALSE]),nstats) else
           rep(NA,nstats)
        }
      } else if(nvar==2) {
        df1 <- as.character(df[[1]]); df2 <- as.character(df[[2]])
        x1 <- X[[1]]; x2 <- X[[2]]
        for(i in 1:nl) {
          s <- (df1[i]=='ALL' | x1==df1[i]) & (df2[i]=='ALL' | x2==df2[i])
          w <- s & !na
          N[i] <- sum(w)
          Missing[i] <- sum(na[s])
          S[i,] <- if(any(w))chk(fun(Y[w,,drop=FALSE]),nstats) else
           rep(NA,nstats)
        }
      } else if(nvar==3) {
        df1 <- as.character(df[[1]]); df2 <- as.character(df[[2]])
        df3 <- as.character(df[[3]])
        x1 <- X[[1]];   x2 <- X[[2]];   x3 <- X[[3]]
        for(i in 1:nl) {
          s <- (df1[i]=='ALL' | x1==df1[i]) & (df2[i]=='ALL' | x2==df2[i]) &
          (df3[i]=='ALL' | x3==df3[i])
          w <- s & !na
          N[i] <- sum(w)
          Missing[i] <- sum(na[s])
          S[i,] <- if(any(w))chk(fun(Y[w,,drop=FALSE]),nstats) else
           rep(NA,nstats)
        }
        
      } else stop('no more than 3 independent variables allowed')
    
      lab <- names(df)
      lab2 <- if(length(lab)>1) paste(lab,collapse=", ") else lab
      heading <- paste(funlab,"by",lab2)
      ##if(length(name.stats)) yname <- funlab <- name.stats

      attr(S,"label") <- yname    #funlab
      df$S <- S
      df$N <- N
      df$Missing <- Missing
      
      a <- list(heading=heading,byvarnames=lab2,Levels=Levels,labels=labels,
                na.action=nact,formula=formula,call=call,yname=yname,ylab=laby,
                class=c("summary.formula.cross","data.frame"))
      attributes(df) <- c(attributes(df), a)
      df
    }
    
  }


##The following makes formula(object) work (using especially for update())

formula.summary.formula.cross <- function(x, ...) attr(x,'formula')


na.retain <- function(d) d

print.summary.formula.response <- function(x, 
  vnames=c('labels','names'), prUnits=TRUE,
  abbreviate.dimnames=FALSE,
  prefix.width, min.colwidth, formatArgs=NULL, ...) {

  stats <- x
  stats <- oldUnclass(stats)
  vnames <- match.arg(vnames)
  ul <- vnames=='labels'

  at <- attributes(stats)
  ns <- length(at$strat.levels)

  vlabels <- at$labels
  if(prUnits) {
    atu <- translate(at$units, '*',' ') ## 31jan03
    vlabels <- ifelse(atu=='',vlabels,   ## 28jan03
                      paste(vlabels,' [',atu,']',sep=''))
  }

  cat(at$ylabel,
      if(ns>1) paste(' by', if(ul)at$strat.label else at$strat.name),
      '    N=',at$n,	if(at$nmiss)paste(', ',at$nmiss,' Missing',sep=''),
      '\n\n',sep='')
  d <- dim(stats)
  
  if(exists('print.char.matrix')) {
    nr <- length(at$nlevels)
    vlab <- if(ul) vlabels[vlabels!=''] else at$vname[at$vname!='']
    z <- matrix('',nrow=nr,ncol=1+d[2],dimnames=list(vlab,NULL))
    dz <- dimnames(stats)[[1]]
    cstats <- matrix('',nrow=d[1],ncol=d[2])
    for(j in 1:d[2]) {
      ww <- c(list(stats[,j]), formatArgs)
      cstats[,j] <- do.call('format', ww)  # 10Feb00
      cstats[is.na(stats[,j]),j] <- ''
    }

    is <- 1
    for(i in 1:nr) {
      ie <- is+at$nlevels[i]-1
      z[i,1] <- paste(dz[is:ie],collapse='\n')
      for(j in 1:d[2]) z[i,j+1] <- paste(cstats[is:ie,j],collapse='\n')
      is <- ie+1
    }
    if(missing(prefix.width)) prefix.width <- max(nchar(dimnames(z)[[1]]))
    if(missing(min.colwidth)) min.colwidth <- 
      max(min(nchar(cstats)[nchar(cstats)>0]), min(nchar(dimnames(stats)[[2]])))
    z <- rbind(c('',dimnames(stats)[[2]]), z)
    if(.R.) print.char.matrix(z, col.names=FALSE, ...) else
    print.char.matrix(z,abbreviate.dimnames=abbreviate.dimnames,
                      prefix.width=prefix.width, 
                      min.colwidth=min.colwidth, ...)  
    return(invisible())
  } 

  dz <- if(length(at$strat.levels)==1) dimnames(stats)[[2]] else
  paste(rep(at$strat.levels,length=d[2]),dimnames(stats)[[2]],sep=":")
  z <- matrix('', ncol=d[2]+2, nrow=d[1],
              dimnames=list(rep('',d[1]),c('','',dz)))
  z[,1] <- if(ul) vlabels else at$vname
  z[,2] <- dimnames(stats)[[1]]
  for(i in 1:d[2]) {
    ww <- c(list(stats[,i]), formatArgs)  # 10Feb00
    z[,i+2] <- do.call('format', ww)
  }
  print(z, quote=FALSE)
  invisible()
}

latex.summary.formula.response <- function(object, 
  title=first.word(deparse(substitute(object))), caption,
  trios, vnames=c('labels','names'), prUnits=TRUE,
  rowlabel='', cdec=2,
  ncaption=TRUE, ...) {
  stats <- object

  title <- title   # otherwise problem with lazy evaluation 25May01
  stats <- oldUnclass(stats)
  vnames <- match.arg(vnames)
  ul <- vnames=='labels'
  at <- attributes(stats)
  ns <- length(at$strat.levels)
  nstat <- ncol(stats)/ns
  if(!missing(trios)) {
    if(is.logical(trios)) trios <- at$ycolname
    ntrio <- length(trios)
    if(ntrio*3!=(nstat-1))   #allow for N
      stop('length of trios must be 1/3 the number of statistics computed')
  }

  if(missing(caption)) caption <- at$ylabel
  
  if(ns>1) caption <- paste(caption,' by', if(ul)at$strat.label else 
                            at$strat.name)
  if(ncaption)
    caption <- paste(caption,
                     '~~~~~N=',at$n,
                     if(at$nmiss)paste(',~',at$nmiss,' Missing',sep=''),
                     sep='')

  dm <- dimnames(stats)
  dm[[1]] <- latexTranslate(dm[[1]])
  dm[[2]] <- latexTranslate(dm[[2]])
  dimnames(stats) <- dm
  caption <- sedit(caption, "cbind", "")
  vn <- if(ul)at$vlabel else at$vname
  if(prUnits) {
    atvu <- translate(at$vunits, '*', ' ')
    vn <- ifelse(atvu=='', vn,  ## 28jan03
                 paste(vn,'~\\hfill\\tiny{', atvu, '}',sep=''))
  }
  vn <- latexTranslate(vn)
  cdec <- rep(cdec, length=(if(missing(trios))nstat else 1+(nstat-1)/3)-1)
  cdec <- rep(c(0,cdec), ns)

  if(missing(trios)) cstats <- oldUnclass(stats) else {
    fmt <- function(z, cdec) ifelse(is.na(z), '', format(round(z,cdec)))
    cstats <- list()
    k <- m <- 0
    for(is in 1:ns) {
      k <- k+1;  m <- m+1
      cstats[[k]] <- stats[,m]   # N, numeric mode
      for(j in 1:ntrio) {
        m <- m+1; k <- k+1
        cstats[[k]] <- paste('{\\scriptsize ',fmt(stats[,m],cdec[k]),'~}',
                             fmt(stats[,m+1],cdec[k]), ' {\\scriptsize ',
                             fmt(stats[,m+2],cdec[k]), '}',sep='')
        m <- m+2
      }
    }
    names(cstats) <- rep(c('N', trios), ns)
    attr(cstats, 'row.names') <- dm[[1]]
    attr(cstats,'class') <- 'data.frame'
    nstat <- 2  # for n.cgroup below
  }
  
  insert.bottom <- if(missing(trios))'' else 
  '\\noindent {\\scriptsize $a$\\ } $b$ {\\scriptsize $c$\\ } represent the lower quartile $a$, the median $b$, and the upper quartile $c$.'

r <-
  if(ns>1) latex(cstats, title=title, caption=caption, rowlabel=rowlabel,
                 n.rgroup=at$nlevels, rgroup=vn[vn!=''],
                 n.cgroup=rep(nstat,ns), cgroup=at$strat.levels, cdec=cdec,
                 col.just=rep('c',ncol(cstats)),
                 rowname=dm[[1]], insert.bottom=insert.bottom, ...)
  else latex(cstats, title=title, caption=caption, rowlabel=rowlabel,
             n.rgroup=at$nlevels, rgroup=vn[vn!=''], cdec=cdec,
             col.just=rep('c',ncol(cstats)),
             rowname=dm[[1]], insert.bottom=insert.bottom, ...)
  r
}

plot.summary.formula.response <-
  function(x, which=1,
           vnames=c('labels','names'), xlim, xlab,
           pch=c(16,1,2,17,15,3,4,5,0), superposeStrata=TRUE,
           dotfont=1, add=FALSE, 
           main, subtitles=TRUE, ...) {

    stats <- x
  stats  <- oldUnclass(stats)
  vnames <- match.arg(vnames)
  ul <- vnames=='labels'
  at <- attributes(stats)
  ns <- length(at$strat.levels)
  if(ns>1 && length(which)>1) 
    stop('cannot have a vector for which if > 1 strata present')
  if(ns < 2) superposeStrata <- FALSE
  vn <- if(ul) at$vlabel else at$vname
  Units <- at$vunits  ## 28jan03
  vn <- ifelse(Units=='', vn, paste(vn, ' [', Units, ']', sep=''))
  ## dotchart2 groups argument may not be an R plotmath expression
  vn <- vn[vn!='']
  d  <- dim(stats)
  n  <- d[1]
  nstat <- d[2]/ns
  vnd <- factor(rep(vn, at$nlevels))  ## was as.category 26Mar02
  dn <- dimnames(stats)
  if(missing(xlim)) xlim <- range(stats[,nstat*((1:ns)-1)+1+which],na.rm=TRUE)

  if(missing(main)) main <- at$funlab

  nw      <- length(which)
  pch     <- rep(pch, length=if(superposeStrata)ns else nw)
  dotfont <- rep(dotfont, length=nw)
  opar <- if(.R.) par(no.readonly=TRUE) else par()
  on.exit(par(opar))  ## 8apr03

  if(superposeStrata) Ns <- apply(stats[,nstat*((1:ns)-1)+1],1,sum)
  
  for(is in 1:ns) {
    for(w in 1:nw) {
      js <- nstat*(is-1)+1+which[w]
      z <- stats[,js]
      if(missing(xlab))xlab <- if(nw>1) dn[[2]][js] else at$ylabel
      dotchart2(z, groups=vnd, xlab=xlab, xlim=xlim,
                auxdata=if(superposeStrata) Ns else stats[,js-which[w]],
                auxtitle='N', sort=FALSE,
                pch=pch[if(superposeStrata)is else w], 
                dotfont=dotfont[w], 
                add=add | w>1 | (is > 1 && superposeStrata),
                reset.par=FALSE, ...)
      ## reset.par=if(missing(reset.par)) w==nw else reset.par, ...) 29jan03
      if(ns>1 && !superposeStrata)
        title(paste(paste(main,if(main!='')'   '),at$strat.levels[is]))
	  else if(main!='') title(main)
      if(ns==1 && subtitles) {
        title(sub=paste('N=',at$n,sep=''),adj=0,cex=.6)
        if(at$nmiss>0) title(sub=paste('N missing=',at$nmiss,sep=''),cex=.6,adj=1)
      }
    }
  }

  if(superposeStrata) { ##set up for Key()
    Key <- if(.R.) function(x=NULL, y=NULL, lev, pch) {
      oldpar <- par(usr=c(0,1,0,1),xpd=NA)
      on.exit(par(oldpar))
      if(is.list(x)) { y <- x$y; x <- x$x }
      if(!length(x)) x <- 0
      if(!length(y)) y <- 1  ## because of formals()
      rlegend(x, y, legend=lev, pch=pch, ...)
      invisible()
    } else function(x=NULL, y=NULL, lev, pch, ...) {
      if(length(x)) {
        if(is.list(x)) {y <- x$y; x <- x$x}
        key(x=x, y=y, text=list(lev), 
            points=list(pch=pch),
            transparent=TRUE, ...) } else
      key(text=list(lev), 
          points=list(pch=pch),transparent=TRUE, ...)
      invisible()
    }
    formals(Key) <- list(x=NULL,y=NULL,lev=at$strat.levels,
                         pch=pch)
    storeTemp(Key)
  }
  invisible()
}

plot.summary.formula.reverse <-
  function(x, 
           vnames=c('labels','names'), what=c('proportion','%'),
           which=c('both','categorical','continuous'),
           xlim=if(what=='proportion') c(0,1) else c(0,100), 
           xlab=if(what=='proportion')'Proportion' else 'Percentage', 
           pch=c(if(FALSE)183 else 16,1,2,17,15,3,4,5,0), exclude1=TRUE,
           dotfont=1, main, subtitles=TRUE,
           prtest=c('P','stat','df','name'), pdig=3, eps=.001,
           conType=c('dot','bp'), cex.means=.5, ...) {

    obj <- x
  vnames <- match.arg(vnames)
  what   <- match.arg(what)
  which  <- match.arg(which)
  conType <- match.arg(conType)
  
  ul <- vnames=='labels'

  if(is.logical(prtest) && !prtest) prtest <- 'none'
  test   <- obj$testresults
  if(!length(test)) prtest <- 'none'

  varNames <- names(obj$stats)
  vn <- if(ul) obj$labels else varNames

  Units <- obj$units
  
  nw     <- if(lg <- length(obj$group.freq)) lg else 1
  gnames <- names(obj$group.freq) 

  if(missing(main)) main <- if(nw==1)'' else 
  paste(if(what=='proportion')'Proportions' else
	    'Percentages','Stratified by',obj$group.label)

  pch     <- rep(pch, length=nw)
  dotfont <- rep(dotfont, length=nw)

  lab <- vnd <- z <- nmiss <- vnamd <- NULL
  type  <- obj$type; n <- obj$n

  opar <- par()  ## 1sep01
  on.exit(setParNro(opar))

  npages <- 0
  
  if(which != 'continuous' && any(type %in% c(1,3))) {
  ftstats <- NULL  
    for(i in (1:length(type))[type==1 | type==3]) {  ## 17Jan99
      nam <- vn[i]
      tab <- obj$stats[[i]]
      if(nw==1) tab <- as.matrix(tab)
      nr <- nrow(tab)
      denom <- if(type[i]==1) apply(tab, 2, sum) else obj$group.freq ## 17Jan99
      y <- (if(what=='proportion')1 else 100)*sweep(tab, 2, denom, FUN='/')
      lev <- dimnames(y)[[1]]
      exc <- exclude1 && (nr==2)
      jstart <- if(exc) 2 else 1
      ##  nn <- c(nn, n[i], rep(NA, if(exc) nr-2 else nr-1))
      ##  k <- 0
      rl <- casefold(lev)
      binary <- type[i]==1 && exc &&     ## 17Jan99
	  (all(rl %in% c("0","1"))|all(rl %in% c("false","true"))|
	   all(rl %in% c("absent","present")))

      for(j in jstart:nrow(y)) {
        if(nw==1) z <- rbind(z, y[j,]) else {
          yj <- rep(NA, nw)
          names(yj) <- gnames
          yj[names(y[j,])] <- y[j,]
          z <- rbind(z, yj)
        }
        lab <- c(lab, if(binary) '' else lev[j])
        vnd <- c(vnd, nam)
        vnamd <- c(vnamd, varNames[i])
      }
      if(any(prtest != 'none')) {
        fts <- formatTestStats(test[[varNames[i]]], type[i]==3,
                               if(type[i]==1)1 else 1:nr,
                               prtest=prtest,
                               plotmath=.R.,
                               pdig=pdig, eps=eps)
        ftstats <- c(ftstats, fts, 
                     if(type[i]==1 && nr-exc-1 > 0)
                     rep(if(.R.)expression('') else '',nr-exc-1))
      }
    }
#  dimnames(z)[[1]] <- lab
  dimnames(z) <- list(lab, dimnames(z)[[2]])  ## 22sep02
  for(i in 1:nw) {
    zi <- z[,i]
    if(any(prtest == 'none') || i > 1)
      dotchart2(zi, groups=vnd, xlab=xlab, xlim=xlim, 
                sort=FALSE, pch=pch[i], 
                dotfont=dotfont[i], 
                add=i>1, ...) else
    dotchart2(zi, groups=vnd, auxdata=ftstats,
              xlab=xlab, xlim=xlim, sort=FALSE,
              pch=pch[i], dotfont=dotfont[i],
              add=i>1, ...)
  }
  if(main!='') title(main)
  npages <- npages + 1
  setParNro(opar)  ## 1sep01

    if(nw > 1) { ##set up for key() if > 1 column
      Key <- if(.R.) function(x=NULL, y=NULL, lev, pch) { ## 1sep02 22jan03
        oldpar <- par(usr=c(0,1,0,1),xpd=NA)
        on.exit(par(oldpar))
        if(is.list(x)) { y <- x$y; x <- x$x }
        ## Even though par('usr') shows 0,1,0,1 after lattice draws
        ## its plot, it still needs resetting
        if(!length(x)) x <- 0
        if(!length(y)) y <- 1  ## because of formals()
        rlegend(x, y, legend=lev, pch=pch, ...)
        invisible()
      } else function(x=NULL, y=NULL, lev, pch, ...) {
        if(length(x)) {
          if(is.list(x)) {y <- x$y; x <- x$x}
          key(x=x, y=y, text=list(lev), 
              points=list(pch=pch),
              transparent=TRUE, ...) } else
        key(text=list(lev), 
            points=list(pch=pch),transparent=TRUE, ...)
        invisible()
      }
      formals(Key) <- list(x=NULL,y=NULL,lev=names(obj$group.freq),
                           pch=pch)   ## ,...=NULL) 1sep02
      storeTemp(Key)
    }
  }

  ncont <- sum(type==2)
  if(which != 'categorical' && ncont) {
  	mf <- par('mfrow')
	if(length(mf)==0) mf <- c(1,1)
	if(ncont > 1 & max(mf)==1) {
	  mf <- if(ncont <= 4)c(2,2) else if(ncont <= 6)c(2,3) else 
      if(ncont <= 9)c(3,3) else c(4,3)
      ## if(ncont <= 12)c(4,3) else if(ncont <= 16) c(4,4) else c(5,4)
      nr <- mf[1]  ## 27jan03 and below
      m  <- par('mar')
#      m[1] <- m[1]/min(nr,1.75)
#      if(.R.) par(mfrow=mf, tcl=-0.4/nr, mgp=c(2.2,.45/nr,0),
#                  mar=m) else
#      par(mfrow=mf, mgp=c(2,.4,0)/nr, mar=m)
      par(mfrow=mf)
	}

    npages <- npages + ceiling(sum(type==2) / prod(mf))
    
    for(i in (1:length(type))[type==2]) {
      ##      nam <- vn[i]  26sep02
      nam <- labelPlotmath(vn[i], Units[i])
      st <- obj$stats[[i]]
      if(nw==1) st <- as.matrix(st)
      if(conType=='dot') {
        quantile.columns <- dimnames(st)[[2]] %nin% c('Mean','SD')  ## 1sep01
        st <- st[,quantile.columns,drop=FALSE]
        xlim <- range(st)
        ns <- as.numeric(dimnames(st)[[2]])
        l  <- 1:length(ns)
        q1  <- l[abs(ns-.25) < .001]
        med <- l[abs(ns-.5)  < .001]
        q3  <- l[abs(ns-.75) < .001]
        st <- st[,c(q1,med,q3),drop=FALSE]

        for(j in 1:3) {
          stj <- st[,j]
          if(nw==1) names(stj) <- ''
          dotchart2(stj, xlab=nam, xlim=xlim, sort=FALSE,
                    pch=c(91,if(FALSE)183 else 16,93)[j], 
                    dotfont=dotfont[1],
                    add=j > 1)   ## , reset.par=j==3, ...) 1sep02
        }
      } else bpplt(st, xlab=nam, cex.points=cex.means)
      if(all(prtest != 'none')) {
        fts <- formatTestStats(test[[varNames[i]]], prtest=prtest,
                               plotmath=.R.,
                               pdig=pdig, eps=eps)
        title(fts, line=.5)  ## .5 ignored in S-Plus
      }
    }

	Key2 <- function(x=NULL, y=NULL, quant, ...) {
	  quant <- format(quant)
	  txt <- paste('(',quant[2],',',quant[3],',',quant[4], 
				   ') quantiles shown\nx-axes scaled to (',quant[1],',',
				   quant[5],') quantiles', sep='')
	  if(length(x)) {
		if(is.list(x)) {y <- x$y; x <- x$x}
		text(x,y,txt, cex=.8, adj=0, ...) } else
	  mtitle(lr=txt, cex.l=.8, line=1, ...)
	  invisible()
	}
	formals(Key2) <- list(x=NULL,y=NULL,quant=obj$quant) #,...=NULL)
	storeTemp(Key2)
}
invisible(npages)
}


#This version of the stardard dotchart function allows a vector of values
#to be specified (typically cell sizes) that are written to the right
#or horizontal (only) dot charts.  New vectors and auxdata and auxgdata and
#a label for auxdata, auxtitle.
#Also added: sort. parameter, to allow suppression of rearrangements of data,
#and added the parameter `add'.  Reference lines are always drawn with lwd=1.
#There's also a new parameter, groupfont, which specifies a font number for
#group headings.  Default is 5 for UNIX (usually Helvetica Bold)
#and 4 for Windows (bold)
#cex.labels is a cex to be used only for category labels.  Default is cex.
#Added reset.par - set to T to reset par() after making plot.  You will
#need to set reset.par to T for the last call in a sequence.

dotchart2 <- 
function(data, labels, groups = NULL, gdata = NA, horizontal = TRUE, 
        pch = if(FALSE)183 else 16, 
	xlab = "", ylab="", auxdata, auxgdata=NULL, auxtitle,
	lty = if(.R.)1 else 2, lines = TRUE, dotsize = .8, cex = par("cex"), 
	cex.labels = cex, cex.group.labels = cex.labels*1.25, sort.=TRUE, 
	add=FALSE, dotfont=par('font'), groupfont=if(under.unix)5 else 1, 
	reset.par=add, xaxis=TRUE, width.factor=if(.R.)1.5 else 1,
    lcolor=if(.R.)'gray' else par('col'), ...) {

  if(.R. && !add) {
    plot.new()   # 18jul02 needed for strwidth
    par(new=TRUE)
  }
  ieaux <- if(missing(auxdata)) FALSE else is.expression(auxdata)
  
  mtextsrt <- function(..., srt=0)
    if(.R.) mtext(..., las=1) else mtext(..., srt=srt)

	ndata <- length(data)
	if(missing(labels)) {
		if(!is.null(names(data)))
			labels <- names(data)
		else labels <- paste("#", seq(along = ndata))
	}
	else labels <- rep(as.character(labels), length = ndata)
	if(missing(groups)) {
		glabels <- NULL
		gdata <- NULL
	}
	else {
		if(!sort.) {   #assume data sorted in groups, but re-number groups
                       #to be as if groups given in order 1,2,3,...
          ug <- unique(as.character(groups))
		  groups <- factor(as.character(groups),levels=ug)
          ## was category()  26Mar02
		}
		groups <- oldUnclass(groups)
		glabels <- levels(groups)
		gdata <- rep(gdata, length = length(glabels))	
 	    ord <- order(groups, seq(along = groups))
		groups <- groups[ord]
		data <- data[ord]
		labels <- labels[ord]
		if(!missing(auxdata)) auxdata <- auxdata[ord]  #FEH
		}
	alldat <- c(data, gdata)
	if(!missing(auxdata)) {
	  auxdata <- c(auxdata, auxgdata)
	  if(!ieaux) auxdata <- format(auxdata)  ## 1sep02
	}

#	alllab <- c(paste(labels, ""), paste(glabels, "    "))	#added 1 space FEH
	alllab <- paste(c(labels, glabels),'')   # 23Nov98
	# set up margins and user coordinates, draw box
#	mxlab <- max(c(5, nchar(alllab)))  # 23Nov98
	tcex <- par('cex')
#	mxlab <- max(max(c(5, nchar(labels)))*cex.labels/tcex,  18jul02
#				 max(c(5, nchar(glabels)))*cex.group.labels/tcex)*.9 18jul02
# .9 was .85  17Jan99
	tmai <- par("mai")
	oldplt <- par("plt")
	if(reset.par)on.exit(par(mai = tmai, cex = tcex, usr = tusr))

	par(cex = cex)    # width.factor 19apr00:
#	mxlab <- mxlab * par("cin")[1] * width.factor	# adjust by char width
# previous line and above replaced with:   18jul02
  mxlab <- .1+max(strwidth(labels, units='inches',cex=cex.labels),
                  if(length(glabels))
                   strwidth(glabels,units='inches',cex=cex.group.labels))*
                 width.factor
  if(horizontal) {
	    tmai2 <- tmai[3:4]
#	    if(!missing(auxdata)) tmai2[2] <- max(tmai2[2],
#											  (2+max(width.factor*nchar(format(auxdata))))*
#											  par('cin')[1])
        # 18jul02:
        if(!missing(auxdata))
          tmai2[2] <- .2+width.factor*
                          max(strwidth(if(ieaux) auxdata else format(auxdata),
                                       units='inches',cex=cex.labels))
		par(mai = c(tmai[1], mxlab, tmai2))
		if(!add)plot(alldat, seq(along = alldat), type = "n",
                     ylab = '', axes = FALSE, xlab = '', ...)
        ## ylab=ylab  16Apr02
		logax <- par("xaxt") == "l"
	}
	else {
		par(mai = c(mxlab, tmai[2:4]))
		if(!add)plot(seq(along = alldat), alldat, type = "n",
                     xlab = "", axes = FALSE, ylab = '', ...)
		logax <- par("yaxt") == "l"
	}
	tusr <- par("usr")
	if(!add && logax) {
		if(horizontal)
			abline(v = 10^tusr[1:2], h = tusr[3:4])
		else abline(v = tusr[1:2], h = 10^tusr[3:4])
	}
	else if(!add) abline(v = tusr[1:2], h = tusr[3:4])
	den <- ndata + 2 * length(glabels) + 1
	if(horizontal) {
		if(!add && xaxis)mgp.axis(1, axistitle=xlab)
		delt <- ( - (tusr[4] - tusr[3]))/den
		ypos <- seq(tusr[4], by = delt, length = ndata)
	}
	else {
		if(!add)mgp.axis(2, axistitle=xlab)
		delt <- (tusr[2] - tusr[1])/den
		ypos <- seq(tusr[1], by = delt, length = ndata)
	}
	if(!missing(groups)) {
		ypos1 <- ypos + 2 * delt * (if(length(groups)>1)
          cumsum(c(1, diff(groups) > 0)) else 1)   #6Oct99
		diff2 <- c(3 * delt, diff(ypos1))
		ypos2 <- ypos1[abs(diff2 - 3 * delt) < abs(0.001 * delt)] - 
			delt
		ypos <- c(ypos1, ypos2) - delt
	}
#put on labels and data
	ypos <- ypos + delt
	nongrp <- 1:ndata
	if(horizontal) {
	  xmin <- par('usr')[1]
		if(!add && lines)
			abline(h = ypos[nongrp], lty = lty, lwd=1, col=lcolor)
      ## was h=ypos[!is.na(alldat)] 31jan03
		points(alldat, ypos, pch = pch, cex = dotsize * cex, font=dotfont)
		if(!add && !missing(auxdata)) {
		  faux <- if(ieaux) auxdata else format(auxdata)
          ## Next 5 lines replaced 18jul02
		  ##mtextsrt(faux, 4, 
          ##	line=(mm <- .75+max(1,max(nchar(faux))/2)), 
          ##	at=ypos[nongrp], srt=0, adj=1, cex=cex.labels)
          ## if(!missing(auxtitle)) mtextsrt(auxtitle, 4, line=mm, srt=0, adj=1,
          ##							cex=cex.labels, at=par('usr')[4])
          upedge <- par('usr')[4]
          outerText(faux, ypos[nongrp], adj=1, cex=cex.labels)
          if(!missing(auxtitle))
            outerText(auxtitle, upedge+strheight(auxtitle,cex=cex.labels)/2,
                      adj=1, cex=cex.labels, setAside=faux[1])
          
#		  mtextsrt(faux, 4, at=ypos[nongrp], srt=0, adj=1, cex=cex.labels)
#		  if(!missing(auxtitle)) mtextsrt(auxtitle, 4, srt=0, adj=1,
#										cex=cex.labels, at=par('usr')[4])

		}
	  if(!add) {
		labng <- alllab[nongrp]
		## Bug in sending character strings to mtext or text containing
		## [ or ] - they don't right-justify in S+    23Nov98
		bracket <- substring(labng,1,1)=='[' |
		           substring(labng,nchar(labng),nchar(labng))==']'
		yposng <- ypos[nongrp]
		s <- !bracket
		if(any(s)) mtextsrt(paste(labng[s],''), 2, 0, at=yposng[s],
                             srt=0, adj=1, cex=cex.labels)
		s <- bracket
		if(any(s)) {
          if(.R.) text(rep(par('usr')[1],sum(s)),
                       yposng[s], labng[s], adj=1,
                       cex=cex.labels, srt=0,xpd=NA) else
          if(.SV4. && under.unix) text(rep(par('usr')[1],sum(s)), ## 20Jun02
                       yposng[s], labng[s], adj=1,
                       cex=cex.labels, srt=0) else {
           xmin <- par('usr')[1] -
             max(nchar(labng[s]))*0.5*cex.labels*par('1em')[1]
#          xmin <- par('usr')[1] - max(strwidth(labng[s],cex=cex.labels))/2
           text(rep(xmin,sum(s)), yposng[s], labng[s], adj=0,
			   cex=cex.labels, srt=0)
         }
		}

#		mtext(paste(labng,''), 2, 0, at = ypos[nongrp], srt = 0, 
#			  adj = 1, cex = cex.labels)
		if(!missing(groups))
		  mtextsrt(paste(alllab[ - nongrp],''), 2, 0, at = ypos[ - nongrp], 
				srt = 0, adj = 1, cex = cex.group.labels, font=groupfont)
	  }
	}
	else {
		if(!add && lines)
			abline(v = ypos[nongrp], lty = lty, lwd=1, col=lcolor)
        ## was v=ypos[!is.na(alldat)] 31jan03
		points(ypos, alldat, pch = pch, cex = dotsize * cex, font=dotfont)
		if(!add) mtextsrt(alllab[nongrp], 1, 0,
                          at = ypos[nongrp], srt = 90, adj = 1,
                          cex = cex.labels)
		if(!add && !missing(groups))
			mtextsrt(alllab[ - nongrp], 1, 0, at = ypos[ - nongrp], 
				srt = 90, adj = 1, cex = cex.group.labels, font=groupfont)
	}
	plt <- par("plt")
	if(horizontal) {
		frac <- (oldplt[2] - oldplt[1])/(oldplt[2] - plt[1])
		umin <- tusr[2] - (tusr[2] - tusr[1]) * frac
		tusr <- c(umin, tusr[2:4])
	}
	else {
		frac <- (oldplt[4] - oldplt[3])/(oldplt[4] - plt[3])
		umin <- tusr[4] - (tusr[4] - tusr[3]) * frac
		tusr <- c(tusr[1:2], umin, tusr[4])
	}
	invisible()
}

print.summary.formula.reverse <- 
  function(x, digits, prn=!all(n==N), pctdig=0, 
		   npct=c('numerator','both','denominator','none'),
		   exclude1=TRUE, vnames=c("labels","names"), prUnits=TRUE,
		   sep="/", abbreviate.dimnames=FALSE, 
		   prefix.width=max(nchar(lab)), 
		   min.colwidth, formatArgs=NULL,
           prtest=c('P','stat','df','name'), prmsd=FALSE, long=FALSE,
           pdig=3, eps=.001, ...) {

    npct   <- match.arg(npct)
    vnames <- match.arg(vnames)
    if(is.logical(prtest) && !prtest) prtest <- 'none'
    stats  <- x$stats
    nv     <- length(stats)
    cstats <- lab <- character(0)
    nn     <- integer(0)
    type   <- x$type
    n      <- x$n
    N      <- x$N
    nams   <- names(stats)
    labels <- x$labels
    Units  <- x$units
    test   <- x$testresults
    if(!length(test)) prtest <- 'none'

    nw     <- if(lg <- length(x$group.freq)) lg else 1  #23Nov98
    gnames <- names(x$group.freq)

    if(!missing(digits)) {    #.Options$digits <- digits 6Aug00
      oldopt <- options(digits=digits)
      on.exit(options(oldopt))
    }


    cstats <- NULL
    for(i in 1:nv) {
      nn <- c(nn, n[i])
      nam <- if(vnames=="names") nams[i] else labels[i]
      if(prUnits && nchar(Units[i]))
        nam <- paste(nam,' [',translate(Units[i],'*',' '),']',sep='')
      tr <- if(length(test) && all(prtest!='none')) test[[nams[i]]] else NULL
      if(type[i]==1 || type[i]==3) {
        cs <- formatCats(stats[[i]], nam, tr, type[i], x$group.freq,
                         npct, pctdig, exclude1, long, prtest,
                         pdig=pdig, eps=eps)
        nn <- c(nn, rep(NA, nrow(cs)-1))
      } else cs <- formatCons(stats[[i]], nam, tr, x$group.freq, prmsd,
                              sep, formatArgs, prtest,
                              pdig=pdig, eps=eps)
      cstats <- rbind(cstats, cs)
    }

    lab <- dimnames(cstats)[[1]]
    gl <- names(x$group.freq)
    gl <- if(length(gl)) paste(gl," \n(N=",x$group.freq,")",sep="") else ""
    if(length(test) && !all(prtest=='none'))
      gl <- c(gl, if(length(prtest)==1 && prtest!='stat')
              if(prtest=='P')'P-value' else prtest
      else '  Test\nStatistic')

    ##lab <- format(lab)   21Jan99
    nc <- nchar(cstats)
    spaces <- substring("                                                        ",
                        1, (max(nc)-nc+1)/2)   # center strings
    dc <- dim(cstats)
    cstats <- paste(spaces, cstats, sep="")
    dim(cstats) <- dc
    if(prn) {
      cnn <- format(nn)
      cnn[is.na(nn)] <- ''
      cstats <- cbind(cnn, cstats)
      gl <- c('N', gl)
    }
    cstats <- rbind(gl, cstats)
    dimnames(cstats) <- list(c('',lab), NULL)
    
    cat("\n\nDescriptive Statistics",
        if(length(x$group.label)) paste(" by",x$group.label) else
        paste("  (N=",x$N,")",sep=""),"\n\n", sep="")
    if(exists("print.char.matrix")) {
      if(missing(min.colwidth)) min.colwidth <- 
        max(min(nchar(gl)),min(nc[nc>0]))
      if(.R.) print.char.matrix(cstats, col.names=FALSE,
                                col.txt.align='left', ...) else
      print.char.matrix(cstats, abbreviate.dimnames=abbreviate.dimnames,
                        prefix.width=prefix.width,
                        min.colwidth=min.colwidth, ...)} else
    print(cstats, quote=FALSE)
    invisible(cstats)
  }

## Function to format subtable for categorical var, for method='reverse'
formatCats <- function(tab, nam, tr, type, group.freq,
                       npct, pctdig, exclude1, long, prtest,
                       latex=FALSE, testUsed=character(0),
                       npct.size='scriptsize', pdig=3, eps=.001,
                       footnoteTest=TRUE) {

  gnames <- names(group.freq)
  nr <- nrow(tab)

  ## If there was a missing column of tab because the variable was
  ## always NA for one (or more) of the groups, add columns of NAs
  if(ncol(tab) > 1) {  ## 23sep03
    tabfull <- matrix(NA,nrow=nr,ncol=length(group.freq),
                      dimnames=list(dimnames(tab)[[1]],gnames))
    tabfull[,dimnames(tab)[[2]]] <- tab
    tab <- tabfull
  }

  denom <- if(type==1) apply(tab, 2, sum) else group.freq ## 17Jan99
  pct <- 100*sweep(tab, 2, denom, FUN='/')
  cpct <- paste(format(round(pct,pctdig)),if(latex)"\\%" else
                "%",sep="")
  denom.rep <- matrix(rep(format(denom),nr),nrow=nr,byrow=TRUE)
  if(npct!='none') cpct <- paste(cpct,
       if(latex)
       switch(npct,
              numerator=paste('{\\',npct.size,' (',format(tab),')}',sep=''),
              denominator=paste('{\\',npct.size,' of',denom.rep,'}'),
              both=paste('{\\',npct.size,' $\\frac{',
                format(tab),'}{',denom.rep,
                '}$}',sep='')) else
       switch(npct,
              numerator=paste('(',format(tab),')',sep=''),
              denominator=paste('of',denom.rep),
              both=paste(format(tab),'/',denom.rep,sep='')))
  if(latex) cpct <- sedit(cpct,' ','~')
  dim(cpct) <- dim(pct)
  dimnames(cpct) <- dimnames(pct)
  cpct[is.na(pct)] <- ""
  lev <- dimnames(pct)[[1]]
  exc <- exclude1 && (nr==2) && (type==1) # type==1 10jul02
  rl <- casefold(dimnames(pct)[[1]])
  binary <- type==1 && exc &&    ## 17Jan99
  (all(rl %in% c("0","1"))|all(rl %in% c("false","true"))|
   all(rl %in% c("absent","present")))
  if(binary) long <- FALSE
  jstart <- if(exc) 2 else 1
  
  nw <- if(lg <- length(group.freq)) lg else 1
  lab <- if(binary) nam else if(long)
    c(nam, paste('   ',lev[jstart:nr])) else
  c(paste(nam,':',lev[jstart]),
    if(nr > jstart) paste('   ',lev[(jstart+1):nr]))
  cs <- matrix('', nrow=long+(if(exc)nr-1 else nr),
               ncol=nw + (length(tr) > 0),
               dimnames=list(lab, c(gnames,if(length(tr))'' else NULL)))
  if(nw==1) cs[(long+1):nrow(cs),1] <- cpct[jstart:nr,] else
  cs[(long+1):nrow(cs),1:nw] <- cpct[jstart:nrow(cpct),gnames]
  if(length(tr)) {
    ct <- formatTestStats(tr, type==3, if(type==1)1 else 1:nr,
                          prtest, latex=latex, testUsed=testUsed,
                          pdig=pdig, eps=eps, footnoteTest=footnoteTest)
    if(length(ct)==1) cs[1,ncol(cs)] <- ct else
    cs[(long+1):nrow(cs),ncol(cs)] <- ct
  }
  cs
}

## Function to format subtable for continuous var, for method='reverse'
formatCons <- function(stats, nam, tr, group.freq, prmsd, sep='/',
                       formatArgs=NULL, prtest,
                       latex=FALSE, testUsed=character(0),
                       middle.bold=FALSE, outer.size=NULL, msdsize=NULL,
                       pdig=3, eps=.001, footnoteTest=TRUE) {
  nw <- if(lg <- length(group.freq)) lg else 1
  ns <- dimnames(stats)[[2]]
  ns <- ifelse(ns %in% c('Mean','SD'), '-1', ns)
  ns <- as.numeric(ns)
  l  <- 1:length(ns)
  q1  <- l[abs(ns-.25) < .001]
  med <- l[abs(ns-.5) < .001]
  q3  <- l[abs(ns-.75) < .001]
  qu <- stats[,c(q1,med,q3),drop=FALSE]
  if(prmsd) qu <- cbind(qu,stats[,c('Mean','SD'),drop=FALSE])
  ww <- c(list(qu), formatArgs)
  cqu <- do.call('format', ww)
  cqu[is.na(qu)] <- ''
  if(latex) {
    st <- character(nrow(cqu))
    names(st) <- dimnames(qu)[[1]]   ## 31jul02
    bld <- if(middle.bold) '\\bf ' else ''
    for(j in 1:nrow(cqu)) {
      st[j] <- paste("{\\",outer.size," ",cqu[j,1],
                     "~}{",bld,cqu[j,2],
                     " }{\\",outer.size," ",cqu[j,3],"} ",sep="")
      if(prmsd) st[j] <- if(length(msdsize))
        paste(st[j], '~{\\',msdsize,'(',cqu[j,4], '$\\pm$',
              cqu[j,5],')}', sep='') else
        paste(st[j], '~(', cqu[j,4], '$\\pm$',
              cqu[j,5],')', sep='')
    }
  }
  else st <- if(prmsd)
    apply(cqu, 1,
          function(x,sep) paste(x[1],sep,x[2],sep,x[3],'  ',
                                x[4],'+/-',x[5],sep=''), sep=sep) else
  apply(cqu, 1, paste, collapse=sep)
  if(any(is.na(qu))) st <- ""
  if(nw==1) yj <- st else {
    yj <- rep('',nw)
    names(yj) <- names(group.freq)
    yj[names(st)] <- st
  }
  if(length(tr)) {
    ct <- formatTestStats(tr, prtest=prtest, latex=latex,
                          testUsed=testUsed, pdig=pdig, eps=eps,
                          footnoteTest=footnoteTest)
    yj <- c(yj, ct)
  }
  matrix(yj, nrow=1, dimnames=list(nam,names(yj)))
}


formatTestStats <- function(tr, multchoice=FALSE,
                            i=if(multchoice)NA else 1,
                            prtest, latex=FALSE,
                            testUsed=character(0),
                            pdig=3, eps=.001,
                            plotmath=FALSE, footnoteTest=TRUE) {

  ## tr=an element of testresults (created by summary.formula method='reverse')
  if(i > 1 && !multchoice) stop('logic error')
  pval     <- tr$P[i]
  teststat <- tr$stat[i]
  testname <- tr$testname

  if(any(is.na(pval)) || any(is.na(teststat))) {
    res <- rep('', length(pval))
    if(latex && length(testUsed))
      res <- if(footnoteTest)
        rep(paste('$^{',match(testname,testUsed),
                  '}$',sep=''), length(pval)) else rep('', length(pval))
    return(res)
  }
  ## Note: multchoice tests always have only one type of d.f.
  deg <- if(multchoice)tr$df[i] else tr$df
  dof      <- if(multchoice)as.character(deg) else
              paste(deg,collapse=',')
  statname <- if(latex)tr$latexstat else
  if(plotmath) tr$plotmathstat else tr$statname
  if(length(prtest)>1 && 'stat' %in% prtest && (latex || plotmath)) {
    ## replace "df" inside statname with actual d.f.
    if(length(grep('df',statname)))
       statname <- sedit(statname, 'df',
                         if(latex || length(deg)==1) dof else
                         paste('list(',dof,')',sep=''))
     }

  pval <- format.pval(pval,digits=pdig,eps=eps)
  plt <- substring(pval,1,1)=='<'

  if(latex) {
    if(length(prtest)==1) 
               paste('$',
                     switch(prtest,
                            P=pval,
                            stat=format(round(teststat,2)),
                            df=dof, name=statname),
                     if(footnoteTest &&
                        length(testUsed))paste('^{',match(testname,testUsed),
                                               '}',sep=''),'$',sep='')
    else paste('$',
               if('stat' %in% prtest)
                paste(statname,'=',format(round(teststat,2)),sep=''),
               if(all(c('stat','P') %in% prtest)) ',~',
               if('P' %in% prtest)paste('P',if(plt)'' else '=', pval,
                                        sep=''),
               if(footnoteTest &&
                  length(testUsed)) paste('^{',match(testname,testUsed),
                                          '}',sep=''),
               '$')
  } else if(plotmath) {
    if(length(prtest)==1) parse(text=
               switch(prtest,
                      P=if(plt)paste('~P',pval,sep='') else
                      paste('~P==',pval,sep=''),
                      stat=format(round(teststat,2)),
                      dof=format(dof),
                      name=statname)) else
    parse(text=paste(
            if('stat' %in% prtest)
            paste('~list(',statname,'==',
                  format(round(teststat,2)),sep=''),
            if(all(c('stat','P') %in% prtest)) ', ',
            if('P' %in% prtest)paste(if(plt)'~P' else '~P==',pval,')',sep='')))
  } else {
    if(length(prtest)==1) switch(prtest,
               P=pval,
               stat=format(round(teststat,2)),
               df=dof, name=statname)
    else paste(if('stat' %in% prtest)
               paste(statname,'=',format(round(teststat,2)),sep=''),
               if('df' %in% prtest) paste('d.f.=',dof,sep=''),
               if('P' %in%  prtest)paste('P', if(plt)'' else '=', pval,
                                         sep=''))
  }
}


latex.summary.formula.reverse <- 
  function(object, title=first.word(deparse(substitute(object))),
           digits, prn=!all(n==N), pctdig=0, 
		   npct=c('numerator','both','denominator','none'),
		   npct.size='scriptsize', Nsize='scriptsize',
		   exclude1=TRUE,  vnames=c("labels","names"), prUnits=TRUE,
		   middle.bold=FALSE, outer.size="scriptsize",
		   caption, rowlabel="",
           insert.bottom=TRUE, dcolumn=FALSE,
           prtest=c('P','stat','df','name'), prmsd=FALSE, msdsize=NULL,
           long=FALSE, pdig=3, eps=.001, ...) {
    x      <- object
    npct   <- match.arg(npct)
    vnames <- match.arg(vnames)
    if(is.logical(prtest) && !prtest) prtest <- 'none'
    stats  <- x$stats
    nv     <- length(stats)
    cstats <- lab <- character(0)
    nn     <- integer(0)
    type   <- x$type
    n      <- x$n
    N      <- x$N
    nams   <- names(stats)
    labels <- x$labels
    Units  <- x$units
    nw     <- if(lg <- length(x$group.freq)) lg else 1  #23Nov98
    gnames <- names(x$group.freq)
    test   <- x$testresults
    if(!length(test)) prtest <- 'none'
    gt1.test <- if(all(prtest=='none')) FALSE else
     length(unique(sapply(test,function(a)a$testname))) > 1

    if(!missing(digits)) {   #.Options$digits <- digits 6Aug00
      oldopt <- options(digits=digits)
      on.exit(options(oldopt))
    }

    if(missing(caption))
      caption <- paste("Descriptive Statistics",
                       if(length(x$group.label)) paste(" by",x$group.label) else
                       paste("  $(N=",x$N,")$",sep=""), sep="")
    
    bld <- if(middle.bold) '\\bf ' else ''
    cstats <- NULL
    testUsed <- character(0)

    for(i in 1:nv) {
      nn <- c(nn, n[i])   ## 12aug02
      nam <- if(vnames=="names") nams[i] else labels[i]
      if(prUnits && nchar(Units[i]) > 0)
        nam <- paste(nam, '~\\hfill\\tiny{',translate(Units[i],'*',' '),'}',sep='')
      tr  <- if(length(test) && all(prtest!='none')) test[[nams[i]]] else NULL
      if(length(test) && all(prtest!='none'))
        testUsed <- unique(c(testUsed, tr$testname))
      if(type[i]==1 || type[i]==3) {
        cs <- formatCats(stats[[i]], nam, tr, type[i], x$group.freq,
                         npct, pctdig, exclude1, long, prtest,
                         latex=TRUE, testUsed=testUsed,
                         npct.size=npct.size,
                         footnoteTest=gt1.test)
        nn <- c(nn, rep(NA, nrow(cs)-1))
      } else cs <- formatCons(stats[[i]], nam, tr, x$group.freq, prmsd,
                              prtest=prtest,
                              latex=TRUE, testUsed=testUsed,
                              middle.bold=middle.bold,
                              outer.size=outer.size, msdsize=msdsize,
                              pdig=pdig, eps=eps, footnoteTest=gt1.test)
                              
      cstats <- rbind(cstats, cs)
    }

        lab <- dimnames(cstats)[[1]]
    gl <- names(x$group.freq)
    ##gl <- if(length(gl)) paste(gl, " $(N=",x$group.freq,")$",sep="") else " "
    ## Thanks: Eran Bellin <ebellin@montefiore.org>   3Aug01
    if(!length(gl)) gl <- " "

    lab <- sedit(lab,c(" ","&"),c("~","\\&"))  #was format(lab) 21Jan99
    lab <- latexTranslate(lab)
    gl  <- latexTranslate(gl)
    ## if(any(gl != " ")) gl <- paste(gl, " $(N=",x$group.freq,")$",sep="") # 3Aug01
    ## Added any( ) 26Mar02  21jan03
    extracolheads <- if(any(gl != " "))
      c(if(prn)'', paste('$N=',x$group.freq,'$',sep='')) else NULL # 21jan03

    if(length(test) && !all(prtest=='none')) {
      gl <- c(gl, if(length(prtest)==1 && prtest!='stat')
              if(prtest=='P')'P-value' else prtest else
              'Test Statistic')
      if(length(extracolheads)) extracolheads <- c(extracolheads,'') # 21jan03
    }

    dimnames(cstats) <- list(NULL,gl) 
		## was dimnames(cstats) <- list(lab, gl) 12aug02
    cstats <- data.frame(cstats, check.names=FALSE)
    ## Added row.names=lab below 10jul02 - S+ was dropping dimnames[[1]]
    ##attr(cstats,'row.names') <- lab  12aug02
    col.just <- rep("c",length(gl))
    if(dcolumn && all(prtest!='none') &&
       gl[length(gl)] %in% c('P-value','Test Statistic'))
      col.just[length(col.just)] <- '.'
    if(prn) {
      cstats <- data.frame(N=nn, cstats, check.names=FALSE)
      col.just <- c("r",col.just)
    }
    if(!insert.bottom) legend <- NULL else {
      legend <- paste(if(any(type==2)) {
        paste("\\noindent {\\",outer.size," $a$\\ }{",bld,"$b$\\ }{\\",
              outer.size," $c$\\ } represent the lower quartile $a$, the median $b$, and the upper quartile $c$\\ for continuous variables.",
              if(prmsd) '~~$x\\pm s$ represents $\\bar{X}\\pm 1$ SD.' else '',
              '\\\\', sep="")
      },
                      if(prn)'$N$\\ is the number of non--missing values.\\\\',
                      if(any(type==1) && npct=='numerator')
                      'Numbers after percents are frequencies.',
                      sep="\n")
      if(length(testUsed))
        legend <-paste(legend,'\\\\\n','\n\n',
                       if(length(testUsed)==1)'Test used:' else 'Tests used:',
                       if(length(testUsed)==1) paste(testUsed,'test') else
                       paste(paste('$^{',1:length(testUsed),'}$',testUsed,
                                   ' test',sep=''),collapse='; '))
      ## added rowname=lab 12aug02  added '\n\n' 4mar03 for ctable=T
    }
    latex.default(cstats, title=title, caption=caption, rowlabel=rowlabel,
                  col.just=col.just, numeric.dollar=FALSE, 
                  insert.bottom=legend,  rowname=lab, dcolumn=dcolumn,
                  extracolheads=extracolheads, extracolsize=Nsize,
                  ...)
    
}


print.summary.formula.cross <- function(x, twoway=nvar==2, 
				  prnmiss=any(stats$Missing>0), prn=TRUE,
				  abbreviate.dimnames=FALSE, 
				  prefix.width=max(nchar(v)),
				  min.colwidth, formatArgs=NULL, ...) {

  stats <- x
  a <- attributes(stats)
  cat("\n",a$heading,"\n\n")
  attr(stats,'class') <- NULL
  ylab <- attr(stats$S,"label")
  nvar <- length(a$Levels)
  vnames <- names(a$Levels)
  nam <- c(vnames, if(prn)"N", if(prnmiss) "Missing", "S") #5Oct00
  stats <- stats[nam]
  S <- stats$S
  ars <- length(dim(S))
  attr(stats,"row.names") <- rep("",length(a$row.names))
  if(twoway && nvar==2 && exists("print.char.matrix")) {
    V <- stats[[vnames[1]]]
    H <- stats[[vnames[2]]]
    v <- levels(V)
    h <- levels(H)
    z <- dimnames(stats$S)[[2]]
    if(!length(z)) z <- ylab
    z <- c(if(prn)"N", if(prnmiss)"Missing", z)  # 5Oct00
    header <- matrix(paste(z,collapse="\n"),1,1)
    if(.R.) print.char.matrix(header, col.names=FALSE) else
    print.char.matrix(header)
    d <- c(length(v),length(h),length(z))
    st <- array(NA, dim=d, dimnames=list(v,h,z))
    cstats <- array("", dim=d, dimnames=list(v,h,z))
    for(i in 1:length(V)) {
      j <- V==V[i,drop=FALSE] & H==H[i,drop=FALSE]
      st[V[i,drop=FALSE],H[i,drop=FALSE],] <-
        c(if(prn)stats$N[j],if(prnmiss)stats$Missing[j],
          if(ars)S[j,] else S[j])  # 5Oct00
    }
    for(k in 1:d[3]) {
      ww <- c(list(st[,,k]), formatArgs)  #10Feb00
      cstats[,,k] <- ifelse(is.na(st[,,k]),"",do.call('format',ww))
    }
    dimn <- dimnames(cstats)[1:2]
    names(dimn) <- vnames
    cstats2 <- matrix("", nrow=d[1], ncol=d[2], dimnames=dimn)
    for(i in 1:d[1]) {
      for(j in 1:d[2]) {
        cstats2[i,j] <- paste(cstats[i,j,], collapse="\n")
      }
    }
    if(missing(min.colwidth)) min.colwidth <- 
      max(min(nchar(dimnames(cstats2)[[2]])), 
          min(nchar(cstats)[nchar(cstats)>0]))
    return(invisible(if(.R.)print.char.matrix(cstats2,
                                              col.names=TRUE, ...) else
                     print.char.matrix(cstats2,  prefix.width=prefix.width,
                                       abbreviate.dimnames=abbreviate.dimnames,
                                       min.colwidth=min.colwidth, ...)))
    ## was col.names=FALSE 26Mar02
  }
  ##print.char.matrix not present (old version of S-Plus)
  ##print.data.frame messes up matrix names (here prefixing by S)
  if(ars) {
    stats$S <- NULL
    snam <- dimnames(S)[[2]]
    for(i in 1:ncol(S)) stats[[snam[i]]] <- S[,i]
  } else names(stats)[length(stats)] <- ylab
  attr(stats,'class') <- "data.frame"
  invisible(print(stats, ...))
}

latex.summary.formula.cross <-
  function(object,
           title=first.word(deparse(substitute(object))),
           twoway=nvar==2,
           prnmiss=TRUE, prn=TRUE,
           caption=attr(object,"heading"), vnames=c('labels','names'),
           rowlabel="", ...) {
    
    stats <- object
    vnames <- match.arg(vnames)
    ul <- vnames=='labels'

    stats <- oldUnclass(stats)
    a <- attributes(stats)
    ylab <- attr(stats$S,"label")
    nvar <- length(a$Levels)
    nam <- c(names(a$Levels), if(prn)"N", if(prnmiss)"Missing","S")
    ##Force lazy evaluation since stats about to change
    caption <- caption; title <- title
    stats <- stats[nam]
    S <- stats$S
    ars <- length(dim(S))
    inn <- c('cbind','c(','ALL',  'NA')
    out <- c('',     '(' ,'Total','Missing')
    caption <- latexTranslate(caption, inn, out, pb=TRUE)

    if(twoway)rowlab <- if(ul) latexTranslate(a$labels[1],inn,out,pb=TRUE) else 
    names(stats)[1]

    rvar <- stats[[1]]
    cvar <- stats[[2]]
    lev1 <- levels(rvar)
    lev2 <- levels(cvar)
    if(!twoway) {
      for(i in 1:nvar) stats[[i]] <- latexTranslate(as.character(
                              stats[[i]]),inn,out,pb=TRUE)
      ##Used to do this translating unconditionally   6Jun96

      if(ars) {
        stats$S <- NULL
        snam <- latexTranslate(dimnames(S)[[2]],inn,out,pb=TRUE)
        for(i in 1:ncol(S)) stats[[snam[i]]] <- S[,i]
      } else names(stats)[length(stats)] <- ylab
      stats <- structure(stats, row.names=rep("",length(stats$N)),
                         class="data.frame")
      return(latex(stats, title=title, caption=caption, rowlabel=rowlabel, 
                   col.just=c("l","l",rep("r",length(stats)-2)), ...))
    }

    ##Two-way
    S <- cbind(N=if(prn)stats$N,
               Missing=if(prnmiss && any(stats$Missing))
                        stats$Missing,  #5Oct00
               stats$S)
    nr <- length(lev1)
    nc <- length(lev2)
    ns <- ncol(S)
    snam <- dimnames(S)[[2]]
    snam <- latexTranslate(snam, inn, out, pb=TRUE)
    dn <- if(ns > 1) rep(snam, nc) else
    latexTranslate(lev2,inn,out,pb=TRUE) # 5Oct00
    st <- matrix(NA, nrow=nr, ncol=nc*ns, dimnames=list(NULL,dn))
    for(i in 1:nr) {
      l <- 0
      for(j in 1:nc) {
        w <- rvar==lev1[i] & cvar==lev2[j]
        if(any(w)) for(k in 1:ns) {
          l <- l+1
          st[i,l] <- S[w,k]
        }
      }
    }
    latex(st, title=title, caption=caption, 
          rowlabel=if(rowlabel=='') rowlab else rowlabel,
          n.rgroup=c(nrow(st)-1,1),
          n.cgroup=if(ns>1) rep(ns,nc),  # ns>1 5Oct00
          cgroup  =if(ns>1) latexTranslate(lev2,inn,out,pb=TRUE),
          check.names=FALSE, rowname=latexTranslate(lev1,inn,out,pb=TRUE), ...)
  }


##stratify is a modification of Therneau's survival4 strata function
##Saves label attributute and defaults shortlabel to T

stratify <- function(..., na.group = FALSE, shortlabel = TRUE)
{
  words <- as.character((match.call())[-1])
  if(!missing(na.group))
    words <- words[-1]
  allf <- list(...)
  xlab <- attr(allf[[1]],'label')  #FEH 2Jun95
  if(length(allf) == 1 && is.list(ttt <- oldUnclass(allf[[1]]))) {
    allf <- ttt
    words <- names(ttt)
  }
  nterms <- length(allf)
  what <- allf[[1]]
  if(is.null(levels(what)))
    what <- factor(what)
  levs <- oldUnclass(what) - 1
  wlab <- levels(what)
  if(na.group && any(is.na(what))) {
    levs[is.na(levs)] <- length(wlab)
    wlab <- c(wlab, "NA")
  }
  if(shortlabel)
    labs <- wlab
  else labs <- paste(words[1], wlab, sep = "=")
  for(i in (1:nterms)[-1]) {
    what <- allf[[i]]
    if(is.null(levels(what)))
      what <- factor(what)
    wlab <- levels(what)
    wlev <- oldUnclass(what) - 1
    if(na.group && any(is.na(wlev))) {
      wlev[is.na(wlev)] <- length(wlab)
      wlab <- c(wlab, "NA")
    }
    if(!shortlabel)
      wlab <- format(paste(words[i], wlab, sep = "="))
    levs <- wlev + levs * (length(wlab))
    labs <- paste(rep(labs, rep(length(wlab), length(labs))), rep(
                                                                  wlab, length(labs)), sep = ", ")
  }
  levs <- levs + 1
  ulevs <- sort(unique(levs[!is.na(levs)]))
  levs <- match(levs, ulevs)
  labs <- labs[ulevs]
  levels(levs) <- labs
  attr(levs,'class') <- "factor"
  if(length(xlab)) label(levs) <- xlab   #FEH 2Jun95
  levs
}



'[.summary.formula.response' <- function(z,i,j,drop=FALSE) {
  at <- attributes(z)
  at$dim <- at$dimnames <- NULL

  if(!missing(j)) {
    z <- oldUnclass(z)[,j,drop=FALSE]
    at$ycolname <- at$ycolname[j]
    attributes(z) <- c(attributes(z), at)
  }
  if(missing(i)) return(z)

  if(is.character(i)) {
    vn <- at$vname[at$vname!='']
    k <- match(i, vn, nomatch=0)
    if(any(k==0)) stop(paste('requested variables not in object:',
             paste(i[k==0],collapse=' ')))
    i <- k
  }

  j <- integer(0)
  nl <- at$nlevels
  is <- 1
  for(m in 1:length(nl)) {
    ie <- is+nl[m]-1
    if(any(i==m)) j <- c(j,is:ie)
    is <- ie+1
  }
  at$vname   <- at$vname[j]
  at$vlabel  <- at$vlabel[j]
  at$nlevels <- at$nlevels[i]
  at$labels  <- at$labels[i]

  z <- oldUnclass(z)[j,,drop=FALSE]
  attributes(z) <- c(attributes(z), at)
  z
}


cumcategory <- function(y) {
  if(!is.category(y)) y <- factor(y)  ## was as.category 26Mar02
  lev <- levels(y)
  y <- oldUnclass(y)
  Y <- matrix(NA, nrow=length(y), ncol=length(lev)-1,
			  dimnames=list(NULL,paste('>=',lev[-1],sep='')))
  storage.mode(Y) <- 'integer'
  for(i in 2:length(lev)) Y[,i-1] <- 1*(y >= i)
  Y
}

mChoice <- function(..., label='', 
					sort.levels=c('original','alphabetic'),
					add.none=TRUE,	none.name='none',
                    na.result=FALSE, drop=TRUE) {

  sort.levels <- match.arg(sort.levels)
  dotlist <- list(...)
  lev <- unique(unlist(lapply(dotlist, function(x)levels(as.factor(x)))))
  if(sort.levels=='alphabetic') lev <- sort(lev)
  X <- as.matrix(as.data.frame(lapply(dotlist,as.character)))
  vcall <- as.character(sys.call())[-1]  ## 15feb03
  Y <- matrix(NA, ncol=length(lev), nrow=nrow(X),
              dimnames=list(names(dotlist[[1]]),lev))
  if(na.result) anyna <- apply(X=='', 1, any)
  unused <- integer(0)
  for(j in 1:length(lev)) {
    Y[,j] <- apply(X==lev[j],1,any)
    if(na.result) Y[,j] <- ifelse(!Y[,j] & anyna, NA, Y[,j])
    if(drop && sum(Y[,j],na.rm=TRUE)==0) unused <- c(unused,j)
  }
  if(length(unused)) Y <- Y[,-unused,drop=FALSE]
  if(add.none) {
    isnone <- apply(Y,1,sum,na.rm=TRUE) == 0
    if(any(isnone)) Y <- cbind(Y,none=isnone)
  }
  if(label == '') label <- attr(dotlist[[1]],'label')
  if(!length(label)) {
    label <- vcall[1]
    if(length(nn <- names(dotlist)[1]))label <- nn
  }
  structure(Y, label=label, class=c('mChoice','labelled',attr(Y,'class')))
}


summarize <- function(X, by, FUN, ..., 
					  stat.name=deparse(substitute(X)), 
					  type=c('variables','matrix'), subset=TRUE) {

  type <- match.arg(type)
  if(missing(stat.name) && length(stat.name)>1) stat.name <- 'X' # 2Mar00
  if(!is.list(by)) {
	nameby <- deparse(substitute(by))
    bylabel <- label(by)
	by <- list(by[subset])
	names(by) <- if(length(nameby)==1) nameby else 'by'   # 2Mar00
  } else {
    bylabel <- sapply(by, label)
    if(!missing(subset))
      by <- lapply(by, function(y, subset) y[subset],
                   subset=subset)
  }
  nby <- length(by)
  
#  bylabel[bylabel==''] <- names(by)  21Mar00
  bylabel <- ifelse(bylabel=='', names(by), bylabel)
  typical.computation <- FUN(X, ...)
  nc <- length(typical.computation)
  xlabel <- deparse(substitute(X))
  if(length(xlabel)!=1) xlabel <- 'X'  # 2Mar00
  if(length(xlab <- attr(X,'label'))) xlabel <- xlab

  if(!missing(subset))
    X <- if(is.matrix(X)) X[subset,,drop=FALSE] else X[subset]

  if(!.R.)  # 21Mar01: S-Plus converts factor to integer during paste
    for(i in 1:nby) if(is.category(by[[i]])) by[[i]] <- as.character(by[[i]])
  ## is.category added 9May01
  byc <- do.call('paste',c(by,sep='|'))

  ## split does not handle matrices
#  msplit <- function(x, group) {
#    if(is.matrix(x)) {
#      group <- as.factor(group)
#      l <- levels(group)
#      res <- vector('list', length(l))
#      names(res) <- l
#      for(j in l) res[[j]] <- x[group==j,,drop=FALSE]
#      res
#    } else split(x, group)
#  }
# Following was streamlined 10oct02 using the new mApply
#  if(nc==1) r <- sapply(msplit(X, byc), FUN, ..., simplify=TRUE) else {
#    r <- sapply(msplit(X, byc), FUN, ..., simplify=TRUE)
#    r <- matrix(unlist(r), nrow=nc, dimnames=dimnames(r))
  ## 2Mar00: added unlist because sapply was creating an array of
  ## lists in S+2000
#  }
  r <- mApply(X, byc, FUN, ...)
#  if(nc > 1) r <- matrix(unlist(r), nrow=nc, dimnames=dimnames(r))10oct02
  
  if(.R.) {   # someday can use unpaste defined in Misc.s
    ans <- strsplit(if(nc==1)names(r) else dimnames(r)[[1]],'\\|')
    ##was dimnames(r)[[2]] 10oct02
    ## strsplit returns list "transpose" of unpaste
    bb <- matrix(unlist(ans), nrow=nby)
    ans <- vector('list', nby)
    for(jj in 1:nby) ans[[jj]] <- bb[jj,]
  } else {
    ans <- if(nc==1)names(r) else dimnames(r)[[1]]  # was [[2]] 8jan03
    if(nby==1) ans <- list(ans) else   # nby==1 9May01
    ans <- unPaste(ans, sep='|')  # 21Mar01  nby>1 9May01
  }
  names(ans) <- names(by)
  if(nc>1 && (nc != ncol(r))) stop('program logic error')  # was nrow 10oct02
  snames <- names(typical.computation)
##  if(!missing(stat.name) | (missing(stat.name) & length(snames)==0))
##	  snames <- if(length(stat.name)==nc)stat.name else 
##        paste(stat.name[1],1:nc,sep='')
    if(!length(snames)) snames <- paste(stat.name,1:nc,sep='')
    if(length(stat.name)==1)snames[1] <- stat.name else snames <- stat.name
#  wrn <- .Options$warn
#  .Options$warn <- -1   6Aug00
  oldopt <- options(warn=-1)
  on.exit(options(oldopt))
  notna <- rep(TRUE, length(ans[[1]]))
  for(i in 1:length(by)) {
	byi <- by[[i]]
    ansi <- ans[[i]]
    if(is.category(byi)) {
      if(!is.character(ansi))
        stop('program logic error:ansi not character')
      ansi <- factor(ansi, levels(byi))  ## 23aug02
#	  ansi <- structure(as.numeric(ansi),   21Mar01
#						levels=levels(byi), class='factor')
    }
	else if(is.numeric(byi)) ansi <- as.numeric(ansi)
    names(ansi) <- NULL
	label(ansi) <- bylabel[i]
    ans[[i]] <- ansi
	notna <- notna & !is.na(ansi)
  }
  if(type=='matrix' || nc==1) {
	ans[[stat.name]] <- if(nc==1) structure(r,names=NULL) else 
	  structure(r, dimnames=list(NULL, snames), names=NULL)  #was t(r) 10oct02
	label(ans[[stat.name]]) <- xlabel
  } else {
	snames <- make.names(snames)
	for(i in 1:length(snames)) {
	  ans[[snames[i]]] <- structure(r[,i], names=NULL) ## was r[i,] 10oct02
	  label(ans[[snames[i]]]) <- xlabel
	}
  }
  notna <- notna & !is.na(if(nc==1) r else (r %*% rep(1,nc))) ## t(r) 10oct02
  ans <- structure(ans, class='data.frame', 
				   row.names=1:length(ans[[1]]))[notna,]
  iorder <- do.call('order', structure(oldUnclass(ans)[1:nby],names=NULL))
  ## order can bomb if data frame given (preserves names)
  ans[iorder,]
}

##Following code is based on tapply instead
if(FALSE) {
  r <- as.array(tapply(x, by, FUN, ...))
  dn <- dimnames(r)
  wrn <- .Options$warn
  .Options$warn <- -1
  for(i in 1:length(by)) {
	byi <- by[[i]]
	if(is.numeric(byi) && !is.category(byi)) dn[[i]] <- as.numeric(dn[[i]])
  }
  .Options$warn <- wrn
  names(dn) <- names(by)
  ans <- expand.grid(dn)

  typical.computation <- FUN(x, ...)
  nc <- length(typical.computation)
  snames <- names(typical.computation)
  if(length(snames)) snames <- paste(stat.name, snames) else
    snames <- if(nc==1) stat.name else paste(stat.name,1:nc)
  for(i in 1:length(r)) if(!length(r[[i]]))r[[i]] <- rep(NA,nc)
  ## unlist will skip positions where calculations not done (NULLs)
  S <- matrix(unlist(r), ncol=length(snames), 
			  dimnames=list(NULL,snames), byrow=TRUE)
  if(type=='matrix') {
	ans$S <- S
	if(stat.name != 'S') names(ans)[length(ans)] <- stat.name
  } else ans <- cbind(ans, S)
  ans
}

as.character.mChoice <- function(x) {
  lev <- dimnames(x)[[2]]
  d <- dim(x)
  w <- rep('',d[1])
  for(j in 1:d[2]) {
    w <- paste(w,ifelse(w!='' & x[,j],',',''),
               ifelse(x[,j],lev[j],''),sep='')
  }
w
}

smean.cl.normal <- function(x, mult=qt((1+conf.int)/2,n-1),
                            conf.int=.95, na.rm=TRUE) {
  if(na.rm) x <- x[!is.na(x)]
  n <- length(x)
  if(n < 2) return(c(Mean=mean(x),Lower=NA,Upper=NA))
  xbar <- sum(x)/n
  se <- sqrt(sum((x - xbar)^2) / n / (n-1))
  c(Mean=xbar, Lower=xbar - mult*se, Upper=xbar + mult*se)
}

smean.sd <- function(x, na.rm=TRUE) {
  if(na.rm) x <- x[!is.na(x)]
  n <- length(x)
  if(n == 0) return(c(Mean=NA, SD=NA))
  xbar <- sum(x)/n
  sd <- sqrt(sum((x - xbar)^2)/(n-1))
  c(Mean=xbar, SD=sd)
}

smean.sdl <- function(x, mult=2, na.rm=TRUE) {
  if(na.rm) x <- x[!is.na(x)]
  n <- length(x)
  if(n == 0) return(c(Mean=NA, Lower=NA, Upper=NA))
  xbar <- sum(x)/n
  sd <- sqrt(sum((x - xbar)^2)/(n-1))
  c(Mean=xbar, Lower=xbar - mult * sd, Upper=xbar + mult * sd)
}

#S-Plus gives a parse error for R's .Internal()
#Might try not using an else to see if S still parses
smean.cl.boot <- if(.R.) eval(parse(text=paste(c(
  'function(x, conf.int=.95, B=1000, na.rm=TRUE, reps=FALSE) {',
  'if(na.rm) x <- x[!is.na(x)]',
  'n <- length(x)',
  'xbar <- mean(x)',
  'if(n < 2) return(Mean=xbar, Lower=NA, Upper=NA)',
  'z <- unlist(lapply(1:B, function(i,x,N)',
  'sum(x[.Internal(sample(N, N, TRUE, NULL))]),',
  'x=x, N=n)) / n',
  'quant <- quantile(z, c((1-conf.int)/2,(1+conf.int)/2))',
  'names(quant) <- NULL',
  'res <- c(Mean=xbar, Lower=quant[1], Upper=quant[2])',
  'if(reps) attr(res,"reps") <- z',
  'res}'),collapse='\n'))) else
  function(x, conf.int=.95, B=1000, na.rm=TRUE, reps=FALSE) {
  if(na.rm) x <- x[!is.na(x)]
  n <- length(x)
  xbar <- mean(x)
  if(n < 2) return(Mean=xbar, Lower=NA, Upper=NA)
  z <- unlist(lapply(1:B, function(i,x,N)
					 sum(x[.Internal(sample.index(N, N, TRUE),
									 "S_sample",TRUE,0)]), x=x, N=n)) / n
  quant <- quantile(z, c((1-conf.int)/2,(1+conf.int)/2))
  names(quant) <- NULL
  res <- c(Mean=xbar, Lower=quant[1], Upper=quant[2])
  if(reps) attr(res, 'reps') <- z
  res
}

smedian.hilow <- function(x, conf.int=.95, na.rm=TRUE) {
  quant <- quantile(x, probs=c(.5,(1-conf.int)/2,(1+conf.int)/2), na.rm=na.rm)
  names(quant) <- c('Median','Lower','Upper')
  quant
}

						  
mApply <- function(X, INDEX, FUN=NULL, ..., simplify=TRUE) {
## Matrix tapply
## X: matrix with n rows; INDEX: vector or list of vectors of length n
## FUN: function to operate on submatrices of x by INDEX
## ...: arguments to FUN; simplify: see sapply
## Modification of code by Tony Plate <tplate@blackmesacapital.com> 10Oct02
## If FUN returns more than one number, mApply returns a matrix with
## rows corresponding to unique values of INDEX
nr <- nrow(X)
if(!length(nr)) {  ## X not a matrix
  r <- tapply(X, INDEX, FUN, ..., simplify=simplify)
  if(is.matrix(r)) r <- drop(t(r)) else
  if(simplify && is.list(r))
    r <- drop(matrix(unlist(r), nrow=length(r),
                     dimnames=list(names(r),names(r[[1]])), byrow=TRUE))
} else {
  idx.list <- tapply(1:nr, INDEX, c)
  r <- sapply(idx.list, function(idx,x,fun,...) fun(x[idx,,drop=FALSE],...),
              x=X, fun=FUN, ..., simplify=simplify)
  if(simplify) r <- drop(t(r))
}
dn <- dimnames(r)  ## 22mar03   length(dn) 29may03
if(length(dn) && !length(dn[[length(dn)]])) {
  dn[[length(dn)]] <- names(FUN(x,...))
  dimnames(r) <- dn
}

if(simplify && is.list(r) && is.array(r)) {

  ll <- sapply(r, length)
  maxl <- max(ll)
  empty <- (1:length(ll))[ll==0]
  for(i in empty) r[[i]] <- rep(NA, maxl)
  ## unlist not keep place for NULL entries for nonexistent categories
  first.not.empty <- ((1:length(ll))[ll > 0])[1]
  nam <- names(r[[first.not.empty]])
  dr <- dim(r)
  
  r <- aperm(array(unlist(r), dim=c(maxl,dr),
                   dimnames=c(list(nam),dimnames(r))),
             c(1+seq(length(dr)), 1))
}
r
}

subsAttr <- function(x) {
  g <- function(y) {
    a <- attributes(y)
    a$dim <- a$names <- a$dimnames <- NULL
    a$storage.mode <- storage.mode(y)
    a
  }
  if(is.list(x)) sapply(x, g) else g(x)
}

asNumericMatrix <- function(x) {
  a <- attributes(x)
  k <- length(a$names)
  y <- matrix(unlist(x), ncol=k, dimnames=list(a$row.names,a$names))
  if(storage.mode(y)=='character')
    warning('x had at least one character vector')
  y
}

matrix2dataFrame <- function(x, at, restoreAll=TRUE) {
  d <- dimnames(x)
  k <- length(d[[2]])
  w <- vector('list',k)
  nam <- names(w) <- d[[2]]
  sm <- storage.mode(x)
  
  for(i in 1:k) {
    a <- at[[nam[i]]]
    if(!length(a)) next
    xi <- x[,i]
    names(xi) <- NULL
    if(restoreAll) {
      if(a$storage.mode != sm) storage.mode(xi) <- a$storage.mode
      a$storage.mode <- NULL
      attributes(xi) <- a
    } else {
      if(length(l   <- a$label))  label(xi) <- l
      if(length(u   <- a$units))  units(xi) <- u
      if(length(lev <- a$levels)) xi <- factor(xi, 1:length(lev), lev)
    }
    w[[i]] <- xi
  }
  structure(w, class='data.frame', row.names=d[[1]])
}

#marginals applies only to symbol="therm", orig.scale to symbol="circle"

symbol.freq <- function(x, y, symbol=c("thermometer","circle"), 
						marginals=FALSE, orig.scale=FALSE,
						inches=.25, width=.15, subset, srtx=0, ...)
{
  symbol <- match.arg(symbol)
  if(missing(subset)) subset <- rep(TRUE, length(x))
  if(!is.logical(subset)) {
	s <- rep(FALSE,length(x))
	s[subset] <- FALSE
	subset <- s
  }
  xlab <- attr(x,'label')
  if(!length(xlab)) xlab <- as.character(substitute(x))
  ylab <- attr(y,'label')
  if(!length(ylab)) ylab <- as.character(substitute(y))
  
  s <- !(is.na(x) | is.na(y)) & subset
  x <- x[s]
  y <- y[s]
  f <- table(x, y)
  dx <- dimnames(f)[[1]]
  dy <- dimnames(f)[[2]]
  if(orig.scale) xp <- as.numeric(dimnames(f)[[1]])	else
  xp <- 1:length(dimnames(f)[[1]])
  xp1 <- length(xp)+1
  if(orig.scale) yp <- as.numeric(dimnames(f)[[2]])	else
  yp <- 1:length(dimnames(f)[[2]])
  yp1 <- length(yp)+1
  m <- nrow(f) * ncol(f)
  xx <- single(m)
  yy <- single(m)
  zz <- single(m)
  k <- 0
  for(i in 1:nrow(f)) {
	for(j in 1:ncol(f)) {
	  k <- k + 1
	  xx[k] <- xp[i]
	  yy[k] <- yp[j]
	  if(f[i, j] > 0)
		zz[k] <- f[i, j]
		else zz[k] <- NA
	}
  }
  maxn <- max(f)
  n <- 10^round(log10(maxn))
  if(marginals) {
	xx <- c(xx, rep(xp1, length(yp)))
	yy <- c(yy, yp)
	zz <- c(zz, table(y)/2)
	xx <- c(xx, xp)
	yy <- c(yy, rep(yp1, length(xp)))
	zz <- c(zz, table(x)/2)		
	xx <- c(xx, xp1)
	yy <- c(yy, yp1)
	zz <- c(zz, n)
  }
  if(symbol=="circle") {
	##		zz <- inches*sqrt(zz/maxn)
	zz <- sqrt(zz)
	if(orig.scale)symbols(xx,yy,circles=zz,inches=inches,
						  smo=.02,xlab=xlab,ylab=ylab,...) else
	symbols(xx,yy,circles=zz,inches=inches,smo=.02,
            xlab=xlab,ylab=ylab,axes=FALSE,...)
	title(sub=paste("n=",sum(s),sep=""),adj=0)
    if(marginals) {
      axis(1, at = 1:xp1, 
           label = c(dx, "All/2"), srt=srtx,adj=if(srtx>0)1 else .5)
      axis(2, at = 1:yp1, 
           label = c(dy, "All/2"),adj=1)
    } else { #	if(!orig.scale) {
	  axis(1, at=xp, label=dx, srt=srtx, adj=if(srtx>0)1 else .5)
	  axis(2, at=yp, label=dy)
    }
	return(invisible())
  }
  zz <- cbind(rep(width,length(zz)), inches*zz/maxn, rep(0,length(zz)))
  symbols(xx,yy,thermometers=zz,inches=FALSE,
          axes=FALSE,xlab=xlab,ylab=ylab,...) 
  title(sub=paste("n=",sum(s),sep=""),adj=0)
  if(marginals)	{
	text(xp1-width, yp1, n, adj=1, cex=.5)
	axis(1, at = 1:xp1, 
		 label = c(dx, "All/2"), srt=srtx,adj=if(srtx>0)1 else .5)
	axis(2, at = 1:yp1, 
		 label = c(dy, "All/2"),adj=1)
	abline(h=yp1-.5, lty=2)
	abline(v=xp1-.5, lty=2)
  }	else {
	axis(1, at=xp, label=dx, srt=srtx,adj=if(srtx>0)1 else .5)
	axis(2, at=yp, label=dy)
	cat("click left mouse button to position legend\n")
	xy <- locator(1)
	symbols(xy$x, xy$y, thermometers=cbind(width,inches*n/maxn,0), 
			inches=FALSE,add=TRUE,xlab=xlab,ylab=ylab)
	text(xy$x-width, xy$y, n,adj=1,cex=.5)
  }
  box()
  invisible()
}
# Improvements by Sebastian Weber <Sebastian.Weber@aventis.com> 26Aug03

sys <- if(.R.) function(command, text=NULL, output=TRUE) {
  cmd <- if(length(text))paste(command,text) else command
  if(under.unix) system(cmd, intern=output) else
  shell(cmd, wait=TRUE, intern=output)
} else if(under.unix) function(..., minimized) unix(...) else
  function(...,minimized=FALSE)  dos(..., minimized=minimized)
t.test.cluster <- function(y, cluster, group, conf.int=.95)    		{

## See:
## Donner A, Birkett N, Buck C, Am J Epi 114:906-914, 1981.
## Donner A, Klar N, J Clin Epi 49:435-439, 1996.
## Hsieh FY, Stat in Med 8:1195-1201, 1988.

group <- as.factor(group)
cluster <- as.factor(cluster)
s <- !(is.na(y)|is.na(cluster)|is.na(group))
y <- y[s];  cluster <- cluster[s];  group <- group[s]
n <- length(y)

if(n<2) stop("n<2")

gr <- levels(group)
if(length(gr)!=2)
  stop("must have exactly two treatment groups")

n <- table(group)
nc <- tapply(cluster, group, function(x)length(unique(x)))
bar <- tapply(y, group, mean)

u <- oldUnclass(group)
y1 <- y[u==1];               y2 <- y[u==2]
c1 <- factor(cluster[u==1]); c2 <- factor(cluster[u==2]) #factor rids unused lev
b1 <- tapply(y1, c1, mean);  b2 <- tapply(y2, c2, mean)
m1 <- table(c1);             m2 <- table(c2)
if(any(names(m1)!=names(b1)))stop("logic error 1")
if(any(names(m2)!=names(b2)))stop("logic error 2")
if(any(m2 < 2))
  stop(paste('The following clusters contain only one observation:',
             paste(names(m2[m2 < 2]), collapse=' ')))

M1 <- mean(y1);              M2 <- mean(y2)
ssc1 <- sum(m1*((b1-M1)^2)); ssc2 <- sum(m2*((b2-M2)^2))
if(nc[1]!=length(m1))stop("logic error 3")
if(nc[2]!=length(m2))stop("logic error 4")
df.msc <- sum(nc)-2
msc <- (ssc1+ssc2)/df.msc
v1 <- tapply(y1,c1,var);    v2 <- tapply(y2,c2,var)
ssw1 <- sum((m1-1)*v1);     ssw2 <- sum((m2-1)*v2)
df.mse <- sum(n)-sum(nc)
mse <- (ssw1+ssw2)/df.mse
na <- (sum(n)-(sum(m1^2)/n[1]+sum(m2^2)/n[2]))/(sum(nc)-1)
rho <- (msc-mse)/(msc+(na-1)*mse)
r <- max(rho, 0)
C1 <- sum(m1*(1+(m1-1)*r))/n[1]
C2 <- sum(m2*(1+(m2-1)*r))/n[2]
v <- mse*(C1/n[1]+C2/n[2])
v.unadj <- mse*(1/n[1]+1/n[2])
de <- v/v.unadj
dif <- diff(bar)
se <- sqrt(v)
zcrit <- qnorm((1+conf.int)/2)
cl <- c(dif-zcrit*se, dif+zcrit*se)
z <- dif/se
P <- 2*pnorm(-abs(z))


stats <- matrix(NA, nrow=20, ncol=2, dimnames=list(c("N","Clusters","Mean",
	"SS among clusters within groups","SS within clusters within groups",
	"MS among clusters within groups","d.f.",
	"MS within clusters within groups","d.f.",
	"Na","Intracluster correlation",
	"Variance Correction Factor","Variance of effect",
	"Variance without cluster adjustment","Design Effect",
	"Effect (Difference in Means)",
	"S.E. of Effect",paste(format(conf.int),"Confidence limits"),
	"Z Statistic","2-sided P Value"), gr))

stats[1,] <- n
stats[2,] <- nc
stats[3,] <- bar
stats[4,] <- c(ssc1, ssc2)
stats[5,] <- c(ssw1, ssw2)
stats[6,1] <- msc
stats[7,1] <- df.msc
stats[8,1] <- mse
stats[9,1] <- df.mse
stats[10,1] <- na
stats[11,1] <- rho
stats[12,] <- c(C1, C2)
stats[13,1] <- v
stats[14,1] <- v.unadj
stats[15,1] <- de
stats[16,1] <- dif
stats[17,1] <- se
stats[18,] <- cl
stats[19,1] <- z
stats[20,1] <- P

attr(stats,'class') <- "t.test.cluster"
stats


									}

print.t.test.cluster <- function(x, digits, ...) {
#   if(!missing(digits)).Options$digits <- digits      6Aug00
  if(!missing(digits)) {
    oldopt <- options(digits=digits)
    on.exit(options(oldopt))
  }
  cstats <- t(apply(x,1,format))
#   cstats <- format(x)
  attr(cstats,'class') <- NULL
  cstats[is.na(x)] <- ""
  invisible(print(cstats, quote=FALSE))
}

transace <- function(x, monotonic=NULL, categorical=NULL, binary=NULL, pl=TRUE) {
  if(.R.) require('acepack')  # provides ace, avas

nam <- dimnames(x)[[2]]
omit <- is.na(x %*% rep(1,ncol(x)))
omitted <- (1:nrow(x))[omit]
if(length(omitted)) x <- x[!omit,]
p <- ncol(x)
xt <- x  # binary variables retain original coding
if(!length(nam)) stop("x must have column names")
rsq <- rep(NA, p)
names(rsq) <- nam


for(i in (1:p)[!(nam %in% binary)])	{
  lab <- nam[-i]
  w <- 1:(p-1)
  im <- w[lab %in% monotonic]
  ic <- w[lab %in% categorical]
  if(nam[i] %in% monotonic) im <- c(0, im)
  if(nam[i] %in% categorical) ic <- c(0, ic)
  m <- 10*(length(im)>0)+(length(ic)>0)
  if(m==11) a <- ace(x[,-i], x[,i], monotone=im, categorical=ic)
  else if (m==10) a <- ace(x[,-i], x[,i], monotone=im)
  else if(m==1) a <- ace(x[,-i], x[,i], categorical=ic)
  else a <- ace(x[,-i], x[,i])
  xt[,i] <- a$ty
  rsq[i] <- a$rsq
  if(pl)plot(x[,i], xt[,i], xlab=nam[i], ylab=paste("Transformed",nam[i]))
}	

cat("R-squared achieved in predicting each variable:\n\n")

attr(xt, "rsq") <- rsq
attr(xt, "omitted") <- omitted
invisible(xt)
}


areg.boot <- function(x, y, data, weights, subset, na.action=na.delete,
					  B = 100, method=c('avas','ace'), evaluation=100, 
					  valrsq=TRUE, probs=c(.25,.5,.75),...) {
  acall   <- match.call()
  method  <- match.arg(method)
  if(.R.) require('acepack')  # provides ace, avas

## Temporarily fix bug in ace
  if(.R.) {
ace <- function (x, y, wt = rep(1, nrow(x)), cat = NULL, mon = NULL, 
    lin = NULL, circ = NULL, delrsq = 0.01) 
{
    x <- as.matrix(x)
    if (delrsq <= 0) {
        cat("delrsq must be positive")
        return()
    }
    iy <- ncol(x) + 1
    l <- matrix(1, ncol = iy)
    if (length(circ)) {
        for (i in 1:length(circ)) {
            if (circ[i] < 0 || circ[i] > ncol(x)) {
                cat("bad circ= specification")
                return()
            }
            if (circ[i] == 0) {
                cat("response spec can only be lin or ordered (default)")
                return()
            }
            else {
                nncol <- circ[i]
                if (l[nncol] != 2 & l[nncol] != 1) {
                  cat("conflicting transformation specifications")
                  return()
                }
                l[nncol] <- 2
            }
        }
    }
    if (length(mon)) {
        for (i in 1:length(mon)) {
            if (mon[i] < 0 || mon[i] > ncol(x)) {
                cat("bad mon= specification")
                return()
            }
            if (mon[i] == 0) {
                cat("response spec can only be lin or ordered (default)")
                return()
            }
            else {
                nncol <- mon[i]
                if (l[nncol] != 3 && l[nncol] != 1) {
                  cat("conflicting transformation specifications")
                  return()
                }
                l[nncol] <- 3
            }
        }
    }
    if (length(lin)) {
        for (i in 1:length(lin)) {
            if (lin[i] < 0 || lin[i] > ncol(x)) {
                cat("bad lin= specification")
                return()
            }
            if (lin[i] == 0) {
                nncol <- iy
            }
            else {
                nncol <- lin[i]
            }
            if (l[nncol] != 4 && l[nncol] != 1) {
                cat("conflicting transformation specifications")
                return()
            }
            l[nncol] <- 4
        }
    }
    if (length(cat)) {
        for (i in 1:length(cat)) {
            if (cat[i] < 0 || cat[i] > ncol(x)) {
                cat("bad cat= specification")
                return()
            }
            if (cat[i] == 0) {
#                cat("response spec can only be lin or ordered (default)")
#                return()
            }
            else {
                nncol <- cat[i]
                if (l[nncol] != 4 && l[nncol] != 1) {
                  cat("conflicting transformation specifications")
                  return()
                }
                l[nncol] <- 4
            }
        }
    }
    tx <- x
    ty <- y
    m <- matrix(0, nrow = nrow(x), ncol = iy)
    z <- matrix(0, nrow = nrow(x), ncol = 12)
    z <- as.matrix(z)
    ns <- 1
    mode(x) <- "double"
    mode(y) <- "double"
    mode(tx) <- "double"
    mode(ty) <- "double"
    mode(wt) <- "double"
    mode(delrsq) <- "double"
    mode(z) <- "double"
    junk <- .Fortran("mace", p = as.integer(ncol(x)), n = as.integer(nrow(x)), 
        x = t(x), y = y, w = as.double(wt), l = as.integer(l), 
        delrsq = delrsq, ns = as.integer(ns), tx = tx, ty = ty, 
        rsq = double(1), ierr = integer(1), m = as.integer(m), 
        z = z, PACKAGE = "acepack")
    return(junk)
}
}

  
  categorical <- NULL
  linear  <- NULL
  mono    <- NULL

  if(inherits(x,'formula')) {
##	nam <- attr(terms.inner(x),'term.labels')  2Apr01
	## terms.inner will cause I(), monotone() wrappers to be ignored
    nam <- var.inner(x)

	m <- match.call(expand = FALSE)
    Terms <- terms(x, specials=c('I','monotone'))  # 2Apr01
	m$formula <- x
	m$x <- m$y <- m$B <- m$method <- m$evaluation <- m$valrsq <- m$probs <- 
		m$... <- NULL
	m$na.action <- na.action

	m[[1]] <- as.name("model.frame")
	x <- eval(m, sys.parent())
	k <- length(x)
	p <- k - 1
	nact <- attr(x,"na.action")
#	Terms <- terms(x, specials=c('I','monotone'))  # 2Apr01

	linear <- attr(Terms,'specials')$I
	if(length(linear)) linear <- linear - 1  ## y is pos. 0 for avas,ace
	mono <- attr(Terms,'specials')$monotone
	if(length(mono)) {
	  mono <- mono - 1
	  if(method=='avas' && any(mono==0)) 
		stop('y is always monotone with method="avas"')
	}
#	attr(Terms, "formula") <- formula
	ylab <- as.character(attr(Terms,'variables')[if(.R.)2 else 1]) #2Apr01
	xbase <- 'x'
	weights <- model.extract(x, weights)
	cat.levels <- values <- vector('list',k)
	names(cat.levels) <- names(values) <- c(ylab,nam)

	for(j in 1:k) {
	  xj <- x[[j]]
	  if(is.character(xj)) {
		xj <- as.factor(xj)
		cat.levels[[j]] <- lev <- levels(xj)
		x[[j]] <- as.integer(xj)
		categorical <- c(categorical, j-1)
		values[[j]] <- 1:length(lev)
		if(method=='avas' && j==1)
		  stop('categorical y not allowed for method="avas"')
	  } else if(is.category(xj)) {
		cat.levels[[j]] <- lev <- levels(xj)
		x[[j]] <- as.integer(xj)
		categorical <- c(categorical, j-1)
		values[[j]] <- 1:length(lev)
		if(method=='avas' && j==1)
		  stop('categorical y not allowed for method="avas"')
	  } else {
        xj <- oldUnclass(xj) # 5Mar01
		xu <- sort(unique(xj))
		nu <- length(xu)
		if(nu < 3) linear <- unique(c(linear, j-1))
		values[[j]] <- if(nu <= length(probs)) xu else quantile(xj,probs)
	  }
	}
	y <- x[,1]
	x <- as.matrix(x[,-1])
  } else {
	nact <- values <- NULL
	if(missing(weights)) weights <- NULL
	ylab <- deparse(substitute(y))
	xbase <- deparse(substitute(x))
	x <- as.matrix(x)
	nam <- dimnames(x)[[2]]
	p <- ncol(x)
	if(!length(nam)) nam <- if(p==1)xbase else paste(xbase,1:p,sep='')
	omit <- is.na(y + (x %*% rep(1, ncol(x))))
	if(any(omit)) {
	  warning(paste(sum(omit),'observations with NAs deleted'))
	  x <- x[!omit,,drop=FALSE]
	  y <- y[!omit]
	}
  }
  n <- length(y)

  if(length(weights)==0) weights <- rep(1,n)
  fcall <- call(method, as.name('x'), as.name('y'), as.name('weights'))
  if(.R.) {  # 2Apr01
    if(length(mono))        fcall$mon <- mono
    if(length(linear))      fcall$lin <- linear
    if(length(categorical)) fcall$cat <- categorical
  } else {
    if(length(mono))        fcall$monotone <- mono
    if(length(linear))      fcall$linear <- linear
    if(length(categorical)) fcall$categorical <- categorical
}
  f <- eval(fcall)
  rsquared.app <- f$rsq

  k <- p + 1
  f.orig <- lm.fit.qr.bare(f$tx, f$ty)
  coef.orig <- f.orig$coefficients
  names(coef.orig) <- cnam <- c('Intercept',nam)
  lp <- f$ty - f.orig$residuals
##  bar <- rep(0, k)
##  cov <- matrix(0, nrow=k, ncol=k, dimnames=list(cnam, cnam))

  trans <- cbind(f$ty,f$tx)
  Xo <- cbind(y, x)
  xlim <- apply(Xo, 2, range)
  xlim[,1] <- range(trans[,1])   # 26Feb00
  nam <- c(ylab, nam)
  fit <- vector('list',k)
  names(fit) <- nam
  neval <- rep(evaluation, k)
  for(i in 1:k) {
    if(length(categorical) && ((i-1) %in% categorical)) neval[i] <- xlim[2,i]
    ## Note: approx will return NAs even when rule=3 if x coordinate
    ## contains duplicates, so sort by x and remove dups (fctn in Misc.s)
    fit[[i]] <- if(i==1)
       approxExtrap(trans[,1],y,
                    xout=seq(xlim[1,i],xlim[2,i],length=neval[i])) else
       approxExtrap(Xo[,i], trans[,i],
                    xout=seq(xlim[1,i],xlim[2,i],length=neval[i]))
  }

## 14may02  was:
#      fit[[i]] <- approx(if(i==1) xySortNoDupNoNA(trans[,1], y) else
#                         xySortNoDupNoNA(Xo[,i], trans[,i]), 
#                        xout=seq(xlim[1,i],xlim[2,i],length=neval[i]), rule=3)

  if(max(neval) > evaluation) 
	stop('evaluation must be >= # levels of categorical predictors')
  boot <- array(NA, c(evaluation,B,k), list(NULL,NULL,nam))
  coefs <- matrix(NA, nrow=B, ncol=k, dimnames=list(NULL,cnam))

  optimism <- 0

  fcall <- if(.R.)
    parse(text=paste(method,'(x[s,],y[s],weights[s]',
            if(length(mono))',mon=mono',
            if(length(linear))',lin=linear',
            if(length(categorical))',cat=categorical',')',sep=''))
  else parse(text=paste(method,'(x[s,],y[s],weights[s]',
               if(length(mono))',monotone=mono',
               if(length(linear))',linear=linear',
               if(length(categorical))',categorical=categorical',')',sep=''))
  ## could not figure out how to get eval like first one to 
  ## evaluate subscripted expressions

  nfail <- 0 # 2Apr01
  for(b in 1:B) {
	cat(b,'')
	s <- sample(n, n, rep = TRUE)
	g <- eval(fcall)
    if(!all(is.finite(g$tx))) {  # 2Apr01
      nfail <- nfail + 1
      next
    }
	f.ols <- lm.fit.qr.bare(g$tx, g$ty)
	cof <- f.ols$coefficients
	coefs[b,] <- cof
##	bar <- bar + cof
##	cov <- cov + cof %*% t(cof)

	X <- cbind(y,x)[s,]
	trans <- cbind(g$ty, g$tx)
	for(i in 1:k)
      boot[1:neval[i],b,i] <-
        if(i==1) approxExtrap(trans[,1],X[,1],
               xout=seq(xlim[1,i],xlim[2,i],length=neval[i]))$y else
          approxExtrap(X[,i], trans[,i],
                       xout=seq(xlim[1,i],xlim[2,i],
                         length=neval[i]))$y
#      boot[1:neval[i],b,i] <-    14may02
#        approx(if(i==1)xySortNoDupNoNA(trans[,1], X[,1]) else   # 26Feb00
#               xySortNoDupNoNA(X[,i], trans[,i]),
#               xout=seq(xlim[1,i],xlim[2,i],
#                 length=neval[i]), rule=3)$y
	if(valrsq) {
	  rsq.boot <- f.ols$rsquared
	  yxt.orig <- matrix(NA,nrow=n,ncol=k)
	  for(i in 1:k)yxt.orig[,i] <- approxExtrap(X[,i],trans[,i],xout=Xo[,i])$y
        #approx(xySortNoDupNoNA(X[,i], trans[,i]),  14may02
		#								  xout=Xo[,i], rule=3)$y

	  yt.hat <- cbind(1,yxt.orig[,-1]) %*% cof
	  yt <- yxt.orig[,1]
	  resid <- yt - yt.hat
	  yt <- yt[!is.na(resid)]
	  resid <- resid[!is.na(resid)]
	  m <- length(resid)
	  sst <- sum((yt - mean(yt))^2)
	  sse <- sum(resid^2)
	  rsquare <- 1 - sse/sst
	  optimism <- optimism + rsq.boot - rsquare
	}
  }
  if(nfail > 0) warning(paste(method,'failed to converge in',
                              nfail,'resamples'))  ## 2Apr01
## bar <- bar/B
## cov <- (cov - B * bar %*% t(bar)) / (B-1)
rsq.val <- if(valrsq) rsquared.app - optimism/(B-nfail)  ##nfail 2Apr01
structure(list(call=acall, method=method, 
			   coefficients=coef.orig, ##var=cov,
			   linear.predictors=if(.R.)lp else as.single(lp),
			   fitted.values=approxExtrap(fit[[1]],xout=lp)$y,  ##14may02
			   residuals=if(.R.)f.orig$residuals else as.single(f.orig$residuals),
			   na.action=nact, fit=fit, n=n,
			   linear=linear, categorical=categorical, monotone=mono,
			   cat.levels=cat.levels, values=values,
			   rsquared.app=rsquared.app,rsquared.val=rsq.val,
			   boot=boot,coef.boot=coefs,nfail=nfail), class='areg.boot')
}

print.areg.boot <- function(x, ...) {
	cat("\n")
	cat(x$method,"Additive Regression Model\n\n")
	dput(x$call)
	cat("\n")

	if(length(x$categorical)) cat('Categorical variables:',
									paste(names(x$fit)[x$categorical+1],
										  collapse=' '),'\n\n')
    if(length(x$nfail) && x$nfail > 0)   ## 2Apr01
      cat('\n',x$method,' failed to converge in ',
          x$nfail,' resamples\n\n',sep='')

	if(length(z <- x$na.action)) naprint(z)

  cat('n=',x$n,'  p=',length(x$fit)-1,
	  '\n\nApparent R2 on transformed Y scale:',round(x$rsquared.app,3))
  if(length(x$rsquared.val))
	cat('\nBootstrap validated R2            :',round(x$rsquared.val,3))
	cat('\n\nCoefficients of standardized transformations:\n\n')
	print(x$coefficients)
    res <- x$residuals
	rq <- c(quantile(res), mean(res), sqrt(var(res)))
	names(rq) <- c("Min", "1Q", "Median", "3Q", "Max", "Mean", "S.D.")
	cat("\n\nResiduals on transformed scale:\n\n")
	print(rq)
	cat('\n')
	invisible()
  }

summary.areg.boot <- function(object, conf.int=.95, values, adj.to,
                              statistic='median',q=NULL, ...) {
  scall   <- match.call()
  fit <- object$fit
  Boot <- object$boot
  Values <- object$values
  if(!missing(values)) Values[names(values)] <- values
  if(length(Values)==0) stop('summary does not work when first argument to areg.boot was not a formula')

  nfail <- object$nfail  # 2Apr01
  if(!length(nfail)) nfail <- 0
  
  res <- object$residuals
  
  Adj.to <- sapply(Values, function(y)median(1*y)) # 12May00 - handles logicals
  names(Adj.to) <- names(Values)   # median adds .50% in R
  if(!missing(adj.to)) Adj.to[names(adj.to)] <- adj.to

  zcrit <- qnorm((1+conf.int)/2)
  k <- length(fit)
  p <- k - 1
  B <- dim(Boot)[2]
  nam <- names(fit)
  coef.orig <- object$coefficients
  coefs <- object$coef.boot
  trans.orig.y <- fit[[1]]
  ytransseq <- trans.orig.y[[1]]

  ## The next 2 loops are required because it takes an extra step to compute 
  ## the linear predictor at all predictor adjust-to settings, not just jth
  ## Get predicted transformed y with all variables set to adj. values
  pred.ty.adj <- double(p)
  for(j in 2:k) {
	namj <- nam[j]
	trans.orig <- fit[[namj]]
	pred.ty.adj[j-1] <- coef.orig[j] *
      approxExtrap(trans.orig, xout=Adj.to[namj])$y
#	  approx(trans.orig, xout=Adj.to[namj], rule=3)$y 14may02
  }

  ## For each bootstrap rep compute term summarizing the contribution
  ## of the jth predictor, evaluated at the adj. value, to predicting
  ## the transformed y, using only transformations from that boot. rep.
  boot.adj <- matrix(NA, nrow=B, ncol=p)
  for(j in 2:k) {
	namj <- nam[j]
	adjj <- Adj.to[namj]
	bootj <- Boot[,,j]
	xt <- fit[[namj]]$x
	for(i in 1:B) {
	  bootji <- bootj[,i]
	  s <- !is.na(bootji)
      ## is.na added 3Apr01
      if(!is.na(coefs[i,j])) 
        boot.adj[i, j-1] <- coefs[i,j]*approxExtrap(xt[s], bootji[s],
                                              xout=adjj)$y  ## 14may02
	}
  }

  ## Now for each predictor compute differences in the chosen
  ## statistical parameter for the original scale of predicted y

  boot.y <- Boot[,,1]
  R <- vector('list',p)
  names(R) <- nam[-1]

  for(j in 2:k) {
	namj <- nam[j]
	xv <- Values[[namj]]
	trans.orig <- fit[[namj]]
	pred.term <- coef.orig[j]*approxExtrap(trans.orig, xout=xv)$y # 14may02
	pred.ty <- coef.orig[1] + sum(pred.ty.adj[-(j-1)]) + pred.term
##	pred.y <- approx(trans.orig.y$y, trans.orig.y$x, xout=pred.ty,rule=3)$y
    pred.y <- smearingEst(pred.ty, trans.orig.y, res,
                          statistic=statistic, q=q)
    lab <- attr(pred.y,'label')
	diff.pred <- pred.y[-1] - pred.y[1]

	## For the same variable (j) repeat this over bootstrap reps

	sumd <- sumd2 <- rep(0, length(xv)-1)
	bootj <- Boot[,,j]
	xt <- trans.orig$x
	b <- 0
    bmiss <- 0
	for(i in 1:B) {
      if(is.na(coefs[i,j])) next   ## From avas/ace failure
	  bootji <- bootj[,i]
	  s <- !is.na(bootji)
	  pred.term <- coefs[i,j]*approxExtrap(xt[s],bootji[s], xout=xv)$y #14may02
	  if(any(is.na(pred.term))) {
        bmiss <- bmiss+1
        next
      }
	  pred.ty <- coefs[i,1] + sum(boot.adj[i,-(j-1)]) + pred.term
	  s <- !is.na(boot.y[,i])
##	  pred.y <- approx(boot.y[s,i], trans.orig.y$x[s], xout=pred.ty,rule=3)$y
      pred.y <- smearingEst(pred.ty, list(x=ytransseq,y=boot.y[,i]), res,
                            statistic=statistic, q=q)
	  if(any(is.na(pred.y))) {
        bmiss <- bmiss+1
        next
      }
	  b <- b + 1
	  dp <- pred.y[-1] - pred.y[1]
	  sumd <- sumd + dp
	  sumd2 <- sumd2 + dp*dp
	}
	if(b < B) warning(paste('For',bmiss,'bootstrap samples a predicted value for one of the settings for',namj,'\ncould not be computed.  These bootstrap samples ignored.\nConsider using less extreme predictor settings.\n'))
	sediff <- sqrt((sumd2 - sumd*sumd/b)/(b-1))
	r <- cbind(c(0,  diff.pred), c(NA, sediff),
			   c(NA, diff.pred-zcrit*sediff),
			   c(NA, diff.pred+zcrit*sediff),
			   c(NA, diff.pred/sediff),
			   c(NA, 2*(1-pnorm(abs(diff.pred/sediff)))))
	cl <- object$cat.levels[[namj]]
	dimnames(r) <- list(x=if(length(cl))cl else format(xv),
						c('Differences','S.E',paste('Lower',conf.int),
						  paste('Upper',conf.int),"Z","Pr(|Z|)"))
	R[[j-1]] <- r
  }
  if(nchar(lab) > 10) lab <- substring(lab, 1, 10)
  structure(list(call=scall, results=R, adj.to=Adj.to, label=lab,
                 B=B, nfail=nfail, bmiss=bmiss),
            class='summary.areg.boot')
}

print.summary.areg.boot <- function(x, ...) {
  R <- x$results
  adj.to <- x$adj.to
  nam <- names(R)
  dput(x$call)

  cat('\nEstimates based on', x$B-x$nfail-x$bmiss, 'resamples\n\n')

  cat('\n\nValues to which predictors are set when estimating\neffects of other predictors:\n\n')
  print(adj.to)

  cat('\nEstimates of differences of effects on',x$label,'Y (from first X value),\nand bootstrap standard errors of these differences.\nSettings for X are shown as row headings.\n')
  for(j in 1:length(nam)) {
    cat('\n\nPredictor:',nam[j],'\n')
    print(R[[j]])
  }
  invisible()
}

plot.areg.boot <- function(x, ylim, boot=TRUE,
                           col.boot=2, lwd.boot=.15, conf.int=.95, ...) {
  fit <- x$fit
  Boot <- x$boot
  k <- length(fit)
  B <- dim(Boot)[2]
  nam <- names(fit)
  boot <- if(is.logical(boot)) (if(boot) B else 0) else min(boot, B)
  
  mfr <- par('mfrow')
  if(!length(mfr) || max(mfr) == 1) {
	mf <- if(k<=2)c(1,2) else if(k<=4)c(2,2) else if(k<=6)c(2,3) else 
    if(k<=9)c(3,3) else if(k<=12)c(3,4) else if(k<=16) c(4,4) else c(4,5)
	oldmfrow <- par(mfrow=mf,err=-1)
	on.exit(par(oldmfrow))
  }
  Levels <- x$cat.levels
  for(i in 1:k) {
    fiti <- fit[[i]]
	if(i==1) fiti <- list(x=fiti[[2]], y=fiti[[1]])
	xx <- fiti[[1]]
    y <- fiti[[2]]
	lx <- length(xx)
	booti <- Boot[,,i]
    yl <- if(!missing(ylim)) ylim else {
      rbi <- quantile(booti,c(.01,.99),na.rm=TRUE)
      if(i==1) range(approxExtrap(fiti, xout=rbi)$y) else range(rbi) #14may02
    }
	levi <- Levels[[i]]
	plot(xx, y, ylim=yl,
		 xlab=nam[i], ylab=paste('Transformed',nam[i]), type='n', lwd=3,
		 axes=length(levi)==0)
	if(ll <- length(levi)) {
	  mgp.axis(2, pretty(yl))
	  mgp.axis(1, at=1:ll, labels=levi)
	}
	if(boot>0) for(j in 1:boot) if(i==1)
      lines(xx, approxExtrap(fiti, xout=booti[1:lx,j])$y,   # 14may02
            col=col.boot, lwd=lwd.boot) else
      lines(xx, booti[1:lx,j], col=col.boot, lwd=lwd.boot)  # 5Mar01
	if(!(is.logical(conf.int) && !conf.int)) {
	  quant <- apply(booti[1:lx,],1,quantile,
					 na.rm=TRUE,probs=c((1-conf.int)/2, (1+conf.int)/2))
      if(i==1) {
        lines(xx, approxExtrap(fiti, xout=quant[1,])$y, lwd=2)  # 14may02
        lines(xx, approxExtrap(fiti, xout=quant[2,])$y, lwd=2)  # 14may02
      } else {
        lines(xx, quant[1,], lwd=2)
        lines(xx, quant[2,], lwd=2)
      }
	}
	lines(xx, fiti[[2]], lwd=3)
  }
invisible()
}

Function.areg.boot <- function(object, type=c('list','individual'),
                               ytype=c('transformed','inverse'),
                               prefix='.', suffix='', frame=0,
                               where=1, ...) {
  type <- match.arg(type)
  ytype <- match.arg(ytype)
  if(missing(type) && !(missing(prefix) & missing(suffix) &
                        missing(frame) & missing(where))) type <- 'individual'
  fit <- object$fit
  k <- length(fit)
  nam <- names(fit)
  g <- vector('list',k)
  catg <- object$categorical
  catl <- object$cat.levels
  names(g) <- nam
  for(i in 1:k) {
	if(length(catg) && ((i-1) %in% catg)) {
      if(i==1 && ytype=='inverse')
        stop('currently does not handle ytype="inverse" when y is categorical')
	  h <- function(x, trantab) {
		if(is.category(x)) x <- as.character(x)
		trantab[x]
	  }
	  w <- fit[[i]]$y
	  names(w) <- catl[[nam[i]]]
      formals(h) <- list(x=numeric(0), trantab=w)
	} else {
	  h <- function(x, trantab) {
		s <- !is.na(x)
		res <- rep(NA, length(x))
		res[s] <- approxExtrap(trantab, xout=x[s])$y  # 14may02
		res
	  }
      fiti <- fit[[i]]
      formals(h) <- list(x=numeric(0),
                         trantab=if(i==1 && ytype=='transformed')
                         list(x=fiti[[2]],y=fiti[[1]]) else fiti)
	}
	g[[i]] <- h
  }
  if(type=='list') return(g)
  fun.name <- paste(prefix, nam, suffix, sep='')
  for(i in 1:k) if(missing(where))
    assign(fun.name[i], g[[i]], frame=frame) else
    if(.R.) assign(fun.name[i], g[[i]], pos=where) else
    assign(fun.name[i], g[[i]], where=where)
  invisible(fun.name)
}

predict.areg.boot <- function(object, newdata, 
							  statistic=c('lp','median','quantile','mean',
                                'fitted','terms'), q=NULL, ...) {

  if(!is.function(statistic)) statistic <- match.arg(statistic)

  fit  <- object$fit
  fity <- fit[[1]]
  res  <- object$residuals
  if(missing(newdata)) {
	if(statistic=='terms')
      stop('statistic cannot be "terms" when newdata is omitted')
	lp <- object$linear.predictors
    y <- smearingEst(lp, fity, res, statistic=statistic, q=q)
    nac <- object$na.action
    return(if(length(nac)) nafitted(nac, y) else y)
  }
  cof <- object$coefficients
  Fun <- Function(object)
  nam <- names(fit)
  p <- length(nam)-1
  X <- matrix(NA, nrow=length(newdata[[1]]), ncol=p)
  for(i in 1:p) {
	nami <- nam[i+1]
	X[,i] <- Fun[[nami]](newdata[[nami]])
  }
  if(!is.function(statistic) && statistic=='terms') return(X)
  lp <- matxv(X, cof)
  smearingEst(lp, fity, res, statistic=statistic, q=q)
}

monotone <- if(!.SV4.)
  function(x) structure(x, class = unique(c("monotone",
  attr(x,'class')))) else
function(x) structure(x, class='monotone')
# SV4 can't handle multiple inheritance.  The above gets rid
# of e.g. "imputed" class

Mean <- function(object, ...) UseMethod("Mean")
Quantile <- function(object, ...) UseMethod("Quantile")

Mean.areg.boot <- function(object, evaluation=200, ...) {
  r <- range(object$linear.predictors)
  lp <- seq(r[1], r[2], length=evaluation)
  res <- object$residuals
  ytrans <- object$fit[[1]]
  asing <- if(.R.) function(x)x else as.single
  if(length(lp)*length(res) < 100000)
    means <- asing(smearingEst(lp, ytrans, res, statistic='mean')) else {
      means <- if(.R.)double(evaluation) else single(evaluation)
      for(i in 1:evaluation)
        means[i] <- mean(approxExtrap(ytrans, xout=lp[i]+res)$y) # 14may02
    }
  g <- function(lp, trantab) approxExtrap(trantab, xout=lp)$y  # 14may02
  formals(g) <- list(lp=numeric(0),
                     trantab=list(x=if(.R.)lp else asing(lp), y=means))
  g
}

Quantile.areg.boot <- function(object, q=.5, ...) {
  if(length(q) != 1 || is.na(q)) stop('q must be length 1 and not NA')
  g <- function(lp, trantab, residualQuantile)
    approxExtrap(trantab, xout=lp+residualQuantile)$y  # 14may02
  formals(g) <- list(lp=numeric(0), trantab=object$fit[[1]],
                     residualQuantile <- quantile(object$residuals, q))
  g
}

smearingEst <- function(transEst, inverseTrans, res,
                        statistic=c('median','quantile','mean','fitted','lp'),
                        q=NULL) {
  if(is.function(statistic)) label <- deparse(substitute(statistic)) else {
    statistic <- match.arg(statistic)
    switch(statistic,
           median = {statistic <- 'quantile'; q <- .5; label <- 'Median'},
           quantile = {if(!length(q))
                         stop('q must be given for statistic="quantile"');
                       label <- paste(format(q),'quantile')},
           mean = {statistic <- mean; label <- 'Mean'},
           fitted = {label <- 'Inverse Transformation'},
           lp = {label <- 'Transformed'})
  }
    y <- if(is.function(statistic)) {
      if(is.list(inverseTrans)) apply(outer(transEst, res,
        function(a, b, ytab) approxExtrap(ytab, xout=a+b)$y,  # 14may02
                        inverseTrans), 1, statistic) else
        apply(outer(transEst, res, function(a, b, invfun)invfun(a+b),
                    inverseTrans), 1, statistic)
    } else switch(statistic,
                  lp = transEst,
                  fitted = if(is.list(inverseTrans)) approxExtrap(   # 14may02
                    inverseTrans,
                    xout=transEst)$y else
                        inverseTrans(transEst),
                  quantile = if(is.list(inverseTrans)) approxExtrap( # 14may02
                    inverseTrans,
                    xout=transEst+quantile(res,q))$y else
                    inverseTrans(transEst+quantile(res,q)))
    structure(y, class='labelled', label=label)
  }

# 7Apr95 - predict.transcan: maxiter=1 if imp.con

transcan <- function(x, method=c("canonical","pc"),
					 categorical=NULL, asis=NULL, nk, 
					 imputed=FALSE, n.impute, 
					 boot.method=c('approximate bayesian', 'simple'),
					 trantab=FALSE, transformed=FALSE,
                     impcat=c("score","multinom","rpart","tree"),
					 mincut=40,
                     inverse=c('linearInterp','sample'), tolInverse=.05,
                     pr=TRUE, pl=TRUE, allpl=FALSE, show.na=TRUE,
                     imputed.actual=c('none','datadensity','hist','qq','ecdf'),
					 iter.max=50, eps=.1, curtail=TRUE, 
					 imp.con=FALSE, shrink=FALSE, init.cat="mode",
                     nres=if(boot.method=='simple')200 else 400,
					 data, subset, na.action, treeinfo=FALSE,
                     rhsImp=c('mean','random'),
                     details.impcat='', ...) {
#This is a non-.Internal version of the approx function.  The
#S-Plus version of approx sometimes bombs with a bus error.

  asing <- if(.R.) function(x)x else as.single
  
if(version$major < 4 && !.R.)
  approx <- function(x, y, xout, method = "linear", n = 50, rule = 1, f = 0) {
	nx <- length(x)
	if(any(is.na(x)) || any(is.na(y)))
		stop("Missing values not allowed")
	if(nx != length(y))
		stop("Lengths of x and y must match")
	if(nx < 2)
		stop("need at least 2 points")
	i <- order(x)
	x <- x[i]
	y <- y[i]
	if(missing(xout))
		xout <- seq(x[1], x[nx], length = n)
	else n <- length(xout)
	methods <- c("linear", "constant")
	if(!(imeth <- pmatch(method, methods, nomatch = 0)))
		stop("method must be \"linear\" or \"constant\"")
	method <- methods[imeth]
	if(method == "linear") {
		f <- -1
	}
	else if(method == "constant") {
		if(f < 0 || f > 1)
			stop("f must be in [0,1]")
	}
	val <-  .Fortran("approx",
		x = as.single(x),
		y = as.single(y),
		nx = as.integer(nx),
		xout = as.single(xout),
		m = as.integer(n),
		rule = as.integer(rule),
		f = as.single(f),
		yout = single(n),
		iscr = single(n))[c("xout", "yout")]
	names(val) <- c("x", "y")
	val
}

call        <- match.call()
method      <- match.arg(method)
impcat      <- match.arg(impcat)
boot.method <- match.arg(boot.method)
imputed.actual <- match.arg(imputed.actual)
inverse     <- match.arg(inverse)
rhsImp      <- match.arg(rhsImp)

if(missing(n.impute)) n.impute <- 0
if(n.impute > 0) {
  imputed <- TRUE
  if(impcat %in% c('rpart','tree'))
    stop('n.impute not supported for impcat="tree" or "rpart"')
  warning('transcan provides only an approximation to true multiple imputation.\nA better approximation is provided by the aregImpute function.\nThe MICE and other S libraries provide imputations from Bayesian posterior distributions.')
}
if(imputed.actual!='none') imputed <- TRUE

if(impcat=='multinom') {
  if(.R.) require('nnet') else if(!existsFunction('multinom')) library(nnet)
}
if(.R.) require('mva')
if(.R. & missing(data)) stop('Must specify data= when using R')  ## 11apr03

formula <- nact <- NULL

if(inherits(x,"formula")) {
  formula <- x    	# 25Ju95
  y <- match.call(expand=FALSE)
  y$x <- y$method <- y$categorical <- y$asis <- y$nk <- y$imputed <- 
	y$trantab <- y$impcat <- y$mincut <- y$pr <- y$pl <- y$allpl <- y$show.na <-
      y$iter.max <- y$eps <- y$curtail <- y$imp.con <- y$shrink <-
        y$init.cat <- y$n.impute <- y$... <- y$nres <- y$boot.method <-
          y$transformed <- y$treeinfo <- y$imputed.actual <-
            y$inverse <- y$tolInverse <- y$details.impcat <-
              y$rhsImp <- NULL
  y$formula <- x
  
  if(missing(na.action)) y$na.action <- na.retain
  y[[1]] <- as.name("model.frame")
  y <- eval(y, sys.parent())
  nact <- attr(y,"na.action")
  d <- dim(y)
##  nam <- names(y)
  nam <- if(.R.)var.inner(formula) else
   attr(terms.inner(formula),'term.labels')  # 2Apr01
  if(!length(asis)) {
	Terms <- terms(formula, specials='I')
	asis <- nam[attr(Terms,'specials')$I]
	## terms.inner will cause I() wrapper to be ignored
  }

#  if(attr(attr(y,"terms"),"response")==1) y <- y[,-1]
  x <- matrix(NA,nrow=d[1],ncol=d[2],
			  dimnames=list(attr(y,"row.names"),nam))
 for(i in 1:d[2]) {
   w <- y[[i]]
   if(is.character(w)) w <- factor(w)
   if(is.factor(w)) { 
     x[,i] <- oldUnclass(w)
     categorical <- c(categorical, nam[i])
   } else {
     x[,i] <- w
     nu <- length(unique(w[!is.na(w)]))
     if(nu<2) stop(paste("variable",nam[i],"has only one value"))
     if(nu==2) asis <- c(asis, nam[i])
     else if(nu==3) categorical <- c(categorical, nam[i])
   }
 }
}

nam <- dimnames(x)[[2]]
rnam <- dimnames(x)[[1]]
if(length(rnam)==0) rnam <- as.character(1:nrow(x))
p <- ncol(x)
if(is.null(nam)) stop("x must have column names")

n <- nrow(x)
if(missing(nk))	nk <- 3*(n<30)+4*(n>=30 & n<100)+5*(n>=100)

#Compute constant to multiply canonical variates by to get a variance of 1.0
varconst <- sqrt(n-1)

if(length(categorical))
  { 
    if(length(categorical)==1 && categorical=="*") categorical <- nam
    ## oldopts <- options(c('na.action','contrasts'))
    ## R does not allow multiple options to be spec.
    oldopts <- options()
    ##  names(oldopts) <- c('na.action','contrasts') #windows can mess this up
    if(impcat %in% c('rpart','tree'))
    {
      #define na.action that keeps all obs.
#       options(na.action="na.retain",     17Jan00
#	  contrasts=c("contr.treatment","contr.poly"))   17Jan00
#       on.exit(options(oldopts))   17Jan00
    }
    else	{
	options(contrasts=c("contr.treatment","contr.poly"))
	on.exit(options(oldopts))
	}
  }
if(length(asis)==1 && asis=="*") asis <- nam

R <- parms <- coef <- fill.con <- Imputed <- Trantab <- vector("list",p)
fillin <- rep(NA,p);  names(fillin) <- nam
scale <- rep(1,p);    names(scale) <- nam;   names(Trantab) <- nam #20Mar95
nparm <- shr <- fillin
if(n.impute > 0) {
  Resid <- vector("list",p)
  names(Resid) <- nam
} else Resid <- NULL

datad <- list(); datad.ranges <- list()

#For canonical-variate expansions (standardized), use scale of 1
xcoef <- matrix(NA, nrow=p, ncol=p+1, dimnames=list(nam,c("intercept",nam)))
usefill <- 1*(is.logical(imp.con) && imp.con)+2*(is.numeric(imp.con))
if(usefill==2 && length(imp.con)!=p)
	stop("length of imp.con != ncol(x)")

for(i in 1:p) {
  lab <- nam[i]
  y <- x[,i]
  na <- is.na(y)
  w <- y[!na]
  if(imputed && n.impute==0)
    Imputed[[i]] <- if(.R.)double(sum(na)) else single(sum(na))
  if(lab %in% asis) {
	fillin[i] <- if(usefill==2) imp.con[i] else median(w)
	scale[i] <- mean(abs(w-fillin[i]))
	if(is.na(fillin[i])) stop(paste("fillin value for",lab,"is NA"))
	coef[[i]] <- c(0,1)
	nparm[i] <- 1
  } else {
	if(lab %in% categorical) {
	  w <- table(y)
	  z <- as.numeric(names(w))
	  if(usefill==2) fillin[i] <- imp.con[i]
		else fillin[i] <- z[w==max(w)][1] #most freq. category
	  assign("Y", as.factor(y), 1)
	  opold <- options(na.action="na.retain")
	  w <- model.matrix(~Y) # uses contr.treatment (reference cell coding)
	  options(na.action=opold[[1]]) #for some reason Windows needs opt name
	  r <- attr(w,"contrasts")[[1]]
	  attr(r,"codes") <- z
	  parms[[i]] <- r
	  R[[i]] <- w[,-1,drop=FALSE]   #kill intercept column
	  nparm[i] <- length(z)-1
	  if(usefill>0) {
		fill.con[[i]] <- w[y==imp.con[i],-1,drop=FALSE][1,,drop=FALSE]
		##select first hit
		if(length(fill.con[[i]])==0)
		  stop("imp.con has a code not in the data for a categorical var")
	  }
	}
      else {
        fillin[i] <- if(usefill==2) imp.con[i] else median(y[!is.na(y)])
        R[[i]] <- rcspline.eval(y, nk=nk, inclx=TRUE)
        parms[[i]] <- attr(R[[i]], "knots")
		if(usefill>0) fill.con[[i]] <- 
		  rcspline.eval(fillin[i], parms[[i]], inclx=TRUE)
		nparm[i] <- length(parms[[i]])-1
      }
  }
}

xt <- x
if(init.cat %in% c("mode","random")) for(i in (1:p)[nam %in% categorical])
  xt[,i] <- if(init.cat=="mode") {
	if(is.na(fillin[i])) stop(paste("fillin value for",nam[i],"is NA"))
	xt[,i]==fillin[i]
  } else runif(n)

p1 <- p-1
R2 <- R2.adj <- if(.R.)double(p) else single(p);  r2 <- r2.adj <- NA
Details.impcat <- NULL  ## 21Feb02

last.iter <- FALSE
cat("Convergence criterion:")

milab <- as.character(1:n.impute)

predSamp <- function(res, yhat, rnam, allowed.range, n.impute, boot.method) {
  m <- length(yhat)
  yhat <- matrix(rep(yhat, n.impute), ncol=n.impute,
                 dimnames=list(rnam, as.character(1:n.impute)))
    errors <- if(boot.method=='simple')
      sample(res, m*n.impute, replace=TRUE) else {
        ## From Jeff Longmate (jlongmat@coh.org):
        n <- length(res)
        i <- ceiling(runif(n*n.impute, 0, n))
        j <- ceiling(runif(m*n.impute, 0, n)) +
          rep((0:(n.impute-1))*n, rep(m, n.impute))
        res[i[j]]
      }
    structure(pmax(pmin(yhat + errors, allowed.range[2]), allowed.range[1]),
              names=NULL)
  }

anyVarNA <- rep(FALSE, n)   ## 25Mar02

for(iter in 1:iter.max) {
  dmax <- 0
  if(last.iter) xtl <- xt
  for(i in 1:p) {
	lab <- nam[i]
	catg <- lab %in% categorical
	xx <- xt[,-i,drop=FALSE]
	k.other <- sum(pmax(nparm[-i]-1,0))/(p-1)+p-1  #effective d.f.
	if(iter==1) {
	  for(j in 1:p1) {
		if(any(z <- is.na(xx[,j])))	{
		  l <- (nam[-i])[j]
	      if(is.na(fillin[l]))stop(paste("variable",l,"has fillin value of NA"))
		  xx[z,j] <- fillin[l]
		}
	  }
	}
	if(method=="pc") {
	  z <- xx
	  for(k in 1:p1) { y <- z[,k]; z[,k] <- (y-mean(y))/sqrt(var(y)) }
	  P <- prcomp(z)$x[,1]   # 1st prin. comp.
	}
	j <- is.na(x[,i])
    anyVarNA[j] <- TRUE  ## 25Mar02
	if(lab %in% asis) {
	  y <- x[!j, i]
	  ##        warn <- .Options$warn
	  ##        .Options$warn <- -1   #cut out warning about NAs
	  f <- lm.fit.qr.bare(xx[!j,,drop=FALSE], y)
	  ##        .Options$warn <- warn
	  newy <- x[,i]
	  names(newy) <- NULL
	  xcof <- f$coef
	  r2 <- f$rsquared
	  nn <- length(y)
	  r2.adj <- max(0,1-(1-r2)*(nn-1)/(nn-k.other-1))
	  if(shrink) {
		ybar <- mean(y)
		shr[i] <- h <- (nn-k.other-1)*r2/(nn-1)/r2.adj
		xcof <- c(ybar*(1-h)+h*xcof[1],h*xcof[-1])
	  }
	  if(any(j)) newy[j] <- if(usefill>0) fillin[i] else
	    cbind(1,xx[j,,drop=FALSE]) %*% xcof

      res <- f$residuals ## 25Mar02
	  
	  if(last.iter) {
		ybar <- mean(y)
		if(imputed & any(j)) {
		  r <- range(newy[!j])
		  Imputed[[i]] <- if(n.impute==0)structure(
							   pmax(pmin(newy[j],r[2]),r[1]),
							   names=rnam[j]) else
          predSamp(res, newy[j], rnam[j], r, n.impute, boot.method)
          ## was f$residuals 25Mar02
#		  structure(pmax(pmin(matrix(rep(newy[j],n.impute),ncol=n.impute,
#									 dimnames=list(rnam[j],milab))+
#							  sample(f$residuals,sum(j)*n.impute,replace=T),
#							  r[2]),r[1]),names=NULL)
		  NULL
		}
		xcoef[i, c("intercept",nam[-i])] <- xcof
		if(trantab) { rr <- range(y); Trantab[[i]] <- list(x=rr, y=rr); NULL }
		if(n.impute > 0) Resid[[i]] <- 
		  if(length(res) <= nres) asing(res) else
		    asing(sample(res, nres))   # 7May99 was n instead of length
        ## was f$residuals 3 times 25Mar02
	  }
	}
	  else {
		f <- cancor(xx[!j,,drop=FALSE], R[[i]][!j,,drop=FALSE])
		r2 <- f$cor[1]^2
		xcof <- c(intercept=-sum(f$xcoef[,1] * f$xcenter),
				  f$xcoef[,1])*varconst
		cof <- if(method=="canonical") 
		  c(intercept=-sum(f$ycoef[,1] * f$ycenter), 
			f$ycoef[,1])*varconst else {
			  g <- lm.fit.qr.bare(R[[i]][!j,,drop=FALSE], P[!j])
			  g$coef }
		newy <- drop(cbind(1,R[[i]]) %*% cof)
        if((n.impute > 0 && last.iter) || rhsImp=='random') 
		  res <- if(method=='canonical') 
			newy[!j] - cbind(1,xx[!j,,drop=FALSE]) %*% xcof else g$residuals
        ## 25Mar02
        
		if(n.impute > 0 && last.iter) {
		  Resid[[i]] <- if(length(res) <= nres) asing(res) else
		    asing(sample(res, nres))  # 7May99
		}

		nn <- n - sum(j)
		k <- nparm[i]-1+k.other
		r2.adj <- max(0,1-(1-r2)*(nn-1)/(nn-k-1))
		if(shrink) {
		  shr[i] <- h <- (nn-k-1)*r2/(nn-1)/r2.adj
		  xcof <- h*xcof   #mean of can var=0
		}
		if(any(j)) newy[j] <- 
		  if(usefill>0) drop(cbind(1,fill.con[[i]]) %*% cof)
			else drop(cbind(1,xx[j,,drop=FALSE]) %*% xcof)

		if(last.iter) {
		  coef[[i]] <- cof
		  xcoef[i,c("intercept",nam[-i])] <- xcof
		  if(trantab || (any(j) && catg &&
                         impcat %in% c("score","multinom"))) {
			xa <- x[!j, i]
            ya <- newy[!j]
            tab <- table(paste(as.character(xa),
                               as.character(ya),sep=';'))  # 1Nov01
            vals <- names(tab)
            uvals <- unPaste(vals, ';')
            names(tab) <- NULL
            Trantab[[i]] <- 
              list(x=uvals[[1]], y=uvals[[2]], frequency=tab)
#			s <- !duplicated(xa)
#			xa <- xa[s]
#			ya <- newy[!j][s]
#			s <- order(xa)
#			Trantab[[i]] <- list(x=xa[s], y=ya[s])
			NULL
		  }

		  if(imputed & any(j)) {
			if(catg) {
			  if(usefill>0) pred <- rep(fillin[i], sum(j))
				else {
                 
				  if(impcat %in% c('rpart','tree')) {
#					y <- na.include(as.factor(x[,i]))  17Jan00
                    y <- as.factor(x[,i])
                    zdf <- list(xx=xx, y=y)
					f <- if(impcat=='tree')
                      tree(y ~ xx, control=tree.control(nobs=sum(!is.na(y)),
                                     mincut=mincut), data=zdf, subset=!is.na(y)) #16Dec97
                    else rpart(y ~ xx, control=rpart.control(
                                         minsplit=mincut), data=zdf)
			#17Jan00; won't work because rpart will not allow matrix x
					pred <-
			(t(apply(-predict(f,zdf)[j,,drop=FALSE],1,order)))[,1]
                    if(treeinfo) {
                      cat('\nProbabilities of Category Membership and Category Selected for',lab,'\n\n')
                      print(cbind(round(predict(f,zdf)[j,,drop=FALSE],3),
                                  Mode=pred))
                    }
					## Gets level number of most probable category
#					if(max(pred)==length(levels(y))) #na.include adds NA at end
#					  warning("imputed value for categorical variable is NA\nuses code 1 higher than real codes in imputed value list")
				  } else if(impcat=='score') {
                    ##Get category code with score closest to pred. score
					ti <- Trantab[[i]]
					if(n.impute==0) {
#					  ww <- apply(outer(newy[j], ti$y,
#                                        function(x,y)abs(x-y)),1,order)[1,]
                      ww <- order(ti$y)[round(approx(sort(ti$y),
                                                  1:length(ti$y),
                                                  xout=newy[j], rule=2)$y)]
# Thanks from Alan Zaslavsky <zaslavsk@hcp.med.harvard.edu>:
# "The idea is to interpolate (after arranging in order) and then round the
# index, since the fractional part of the index represents the relative 
# distance from the two adjacent values."
                      
##					  pred <- round(approx(ti$y, ti$x, xout=newy[j], rule=2)$y)
					  pred <- ti$x[ww]
					} else {
#					  sval <- rep(newy[j],n.impute) +
#						sample(res, sum(j)*n.impute, replace=T)
                      sval <- predSamp(0*res, newy[j], rnam[j], c(-Inf,Inf),
                                       n.impute, boot.method)
                      ww <- order(ti$y)[round(approx(sort(ti$y),
                                                     1:length(ti$y),
                                                     xout=sval,
                                                     rule=2)$y)]
                      pred <- matrix(ti$x[ww], ncol=n.impute,
                                     dimnames=list(rnam[j],milab))
#					  pred <- matrix(round(approx(ti$y, ti$x, xout=sval, 
#												  rule=2)$y),
#									 ncol=n.impute,
#									 dimnames=list(rnam[j],milab))
					  names(pred) <- NULL
                      if(lab==details.impcat)
                        Details.impcat <-
                          list(pred.trans.na=sval,imputed=pred,
                               pred.trans.nona=cbind(1,xx[!j,]) %*%	xcof,
                               obs=x[!j,i],trantab=ti)
					}
				  } else {
                    ## Multinomial logit   23Feb02
                    zdf <- list(y=as.factor(x[!j,i]),
                                xx=xx[!j,,drop=FALSE])
                    f <- multinom(y ~ xx, data=zdf,
                                  trace=FALSE, maxit=200)
##                    pred <- predict(f, list(xx=xx[j,,drop=FALSE]),
##                                    type='probs')
                    ncat <- length(levels(zdf$y))
                    ## bug in predict.multinom when predictor is a matrix
                    cf <- coef(f)
                    zdf <- cbind(1,xx[j,,drop=FALSE]) %*%
                      (if(is.matrix(cf)) t(cf) else as.matrix(cf))
                    pred <- exp(cbind(0,zdf))/
                      (1 + apply(exp(zdf),1,sum))
                    dimnames(pred)[[2]] <- as.character(1:ncat)
                    pred <-
                      if(n.impute==0) (t(apply(-pred,1,order)))[,1] 
                     else rMultinom(pred, n.impute)
                  }
				}
			  if(n.impute==0) names(pred) <- rnam[j]
			  Imputed[[i]] <- pred
			  NULL
			} else {
			  if(n.impute==0) {
				if(usefill>0) Im <- rep(fillin[i], sum(j)) else
#                Im <- invertTabulated(Trantab[[i]], aty=newy[j], #1Nov01
#                                      name=nam[i],  inverse=inverse,
#                                      tolInverse=tolInverse)
                Im <- invertTabulated(x[!j,i], newy[!j], aty=newy[j],
                                      name=nam[i], inverse=inverse,
                                      tolInverse=tolInverse)

##				  else Im <- approx(newy[!j], x[!j,i], newy[j], rule=2)$y
				names(Im) <- rnam[j]
				Imputed[[i]] <- Im
				NULL
			  } else {
#				sval <- rep(newy[j],n.impute) + 
#				  sample(res, sum(j)*n.impute, replace=T)

                sval <- predSamp(res, newy[j], rnam[j], c(-Inf,Inf),
                                 n.impute, boot.method)

                sval.orig <- matrix(invertTabulated(x[!j,i], newy[!j],
                                                    aty=sval,
                                                    name=nam[i],
                                                    inverse=inverse,
                                                    tolInverse=tolInverse),
                                    ncol=n.impute,
                                    dimnames=list(rnam[j],milab))

				names(sval.orig) <- NULL
				Imputed[[i]] <- sval.orig
				NULL
			  }
			}
		  }   ##end imputed
		}       ##end last.iter
	  }         ##end non-asis
	if(curtail && any(j)) {
	  r <- range(newy[!j])
	  newy[j] <- pmax(pmin(newy[j],r[2]),r[1])
	}
	if(iter>1) {
      jj <- if(rhsImp=='mean')TRUE else TRUE ## !anyVarNA  ## 25Mar02
	  dmax <- max(dmax, min(max(abs(xt[jj,i]-newy[jj]),na.rm=TRUE),
							max(abs(-xt[jj,i]-newy[jj]),na.rm=TRUE))/scale[i])
	  ##Allows for arbitrary flips (negation) of current transformation
	}
    if(rhsImp=='random')
      newy[j] <- newy[j] + sample(res, sum(j), replace=TRUE) ## 25Mar02
    
	if(last.iter) xtl[,i] <- newy else xt[,i] <- newy
	##don't update working transformations
	##during last iteration since recomputing x-coefficients
	##on the basis of current transformations, which may flip rapidly
	
	if((pl & last.iter) | allpl) {
	  xti <- if(last.iter) xtl[,i] else xt[,i]
	  plot(x[,i], xti, xlab=lab,ylab=paste("Transformed",lab))
	  title(sub=paste("R2=",format(round(r2,2)),sep=""),cex=.4,adj=0)
	  if(any(j)) title(sub=paste(sum(j),"missing"),cex=.4,adj=1)
	  if(show.na && any(j)) {
		scat1d(xti[j], 4, ...)
        ## added as.numeric 22Feb02
		if(imputed && last.iter) scat1d(as.numeric(Imputed[[i]]), 3, ...)
	  }
	}

    if(last.iter && imputed.actual!='none' && any(j)) {
      v1n <- nam[i]; v2n <- paste('Imputed',v1n)
      datad[[v1n]] <- x[!j,i]
      datad[[v2n]] <- Imputed[[i]]
      datad.ranges[[v1n]] <- datad.ranges[[v2n]] <-
        range(c(x[!j,i], Imputed[[i]]), na.rm=TRUE)
    }
    
	R2[i] <- r2; R2.adj[i] <- r2.adj
	
  }   #end i
  if(iter>1)cat(format(round(dmax,3)),"")
  if(iter %% 10 == 0) cat("\n")
  niter <- iter
  if(last.iter) break
  last.iter <- (iter==(iter.max-1)) || (iter>1 && dmax<eps) ||
   (rhsImp=='random' && iter==5)
}        #end iter
cat("\n")

if(iter.max>3 & niter==iter.max & dmax>=eps)
  stop(paste("no convergence in",iter.max,"iterations"))
										#last & was (!last.iter)

#Use xtl instead of xt, otherwise transformed variables will not
#match ones from predict() or Function() since coefficients have
#been updated

if(rhsImp=='mean') cat("Convergence in",niter,"iterations\n") ## 25Mar02

if(imputed.actual=='datadensity') {
  lab <- names(datad)
  datadensity.data.frame(datad, ranges=datad.ranges,
                         labels=ifelse((1:length(lab)) %% 2, lab,'Imputed'))
} else if(imputed.actual !='none') {
  namdd <- names(datad)
  for(i in seq(1,length(datad),by=2)) {
    if(imputed.actual=='hist') histbackback(datad[i:(i+1)]) else {
      v1 <- datad[[i]]; v2 <- datad[[i+1]]
      n1 <- namdd[i]; n2 <- namdd[i+1]
      if(imputed.actual=='ecdf' && is.numeric(datad[[i]]))
        ecdf(c(v1,v2), xlab=n1,
             group=c(rep('actual',length(v1)),
               rep('imputed',length(v2)))) else {
                 qqplot(v1, v2, xlab=n1, ylab=n2)
                 abline(a=0, b=1, lty=2)
               }
    }
  }
}

names(R2) <- nam
if(pr) {
  cat("R-squared achieved in predicting each variable:\n\n")
  print(round(R2, 3))
}
names(R2.adj) <- nam
if(pr) {
  cat("\nAdjusted R-squared:\n\n")
  print(round(R2.adj, 3))
}
if(shrink) {
  names(shr) <- nam
#  attr(xtl, "shrinkage") <- shr  7Nov00
  if(pr) {
    cat("\nShrinkage factors:\n\n")
    print(round(shr,3))
  }
} else shr <- NULL

names(parms) <- names(coef) <- nam
r <- apply(xtl, 2, range)
dimnames(r) <- list(c("low","high"), nam)

if(imputed) {
  names(Imputed) <- nam
} else Imputed <- NULL

#at <- c(attributes(xtl), at)  7Nov00
#if(!transformed) {
#  xtl <- NULL
#  at$dim <- at$dimnames <- NULL
#}
#attributes(xtl) <- at
#invisible(xtl)

structure(list(call=call, formula=formula, niter=niter, imp.con=usefill>0,
               n.impute=n.impute, residuals=Resid, rsq=R2, rsq.adj=R2.adj, 
               shrinkage=shr,
               inverse=inverse, tolInverse=tolInverse,
               categorical=categorical, asis=asis, parms=parms, coef=coef,
               xcoef=xcoef, fillin=fillin, scale=scale, ranges=r,
               transformed=if(transformed)xtl,
               trantab=if(trantab)Trantab, imputed=Imputed, na.action=nact,
               rhsImp=rhsImp, details.impcat=Details.impcat),
          class='transcan')

}


summary.transcan <- function(object, long=FALSE, ...) {
  ## Check for old style object  7Nov00
  if(!is.list(object)) object <- attributes(object)
dput(object$call); cat("\n")
if(length(nact <- object$na.action)) naprint(nact)
cat("Iterations:",object$niter,"\n\n")
cat("R-squared achieved in predicting each variable:\n\n")
print(round(object$rsq,3))
cat("\nAdjusted R-squared:\n\n")
print(round(object$rsq.adj,3))
if(length(shr <- object$shrink)) {
  cat("\nShrinkage factors:\n\n")
  print(round(shr,3))
}
cat("\nCoefficients of canonical variates for predicting each (row) variable\n\n")
xcoef <- object$xcoef[,-1]
g <- format(round(xcoef,2))
g[is.na(xcoef)] <- ""
print(g, quote=FALSE)

imp <- object$imputed
if(length(imp)) {
  nimp <- TRUE
  for(nn in names(imp)) {
	if(length(z <- imp[[nn]])) {
	  if(nimp & !long) cat("\nSummary of imputed values\n\n"); nimp <- FALSE
	  if(long) {cat("\nImputed values for",nn,"\n\n");print(z)}
	  print(describe(as.vector(z), nn))
	}
  }	
}
if(object$imp.con) 
  cat("\nImputed values set to these constants:\n\n")
  else cat("\nStarting estimates for imputed values:\n\n")
print(object$fillin)

invisible()
}

print.transcan <- function(x, long=FALSE, ...) {
  ## Check for old style 7Nov00
  if(!is.list(x)) {
    trans <- x
    cal   <- attr(x, 'call')
  } else {
    trans <- x$transformed
    cal   <- x$call
  }
  dput(cal); cat("\n")
  if(length(trans)) {
    if(long) print(oldUnclass(x)) else print.matrix(trans)
  }
  invisible()
}

impute.transcan <- function(x, var, imputation,
							name=as.character(substitute(var)),
							where.in, data, where.out=1, frame.out,
                            list.out=FALSE,
                            pr=TRUE, check=TRUE, ...) {
  if(!missing(imputation) && length(imputation)>1)
	stop('imputation must be a single number')
  ## Check for old style yNov00
  imp <- if(is.list(x)) x$imputed else attr(x, 'imputed')
  
  if(is.null(imp)) { 
	if(missing(var) && missing(name)) 
	  stop('imputed=TRUE was not specified to transcan')
	warning("imputed was not specified to transcan")
    return(if(!missing(var))var)
  }

  if(missing(var) && missing(name)) {
	nams   <- names(imp)
    if(list.out) {  ## 10Mar01
      outlist <- vector('list', length(nams))
      names(outlist) <- nams
    }
    if(missing(data)) { # 18Sep01
      if(missing(where.in)) where.in <- find(nams[1])[1]
      var1   <- get(nams[1],where.in)
    } else var1 <- data[[nams[1]]]  # 18Sep01
	namvar <- names(var1)
    if(!length(namvar) && !missing(data)) namvar <- row.names(data)  # 5Feb02
	if(check && length(namvar)==0)
	  warning(paste('variable',nams[1],
       'does not have a names() attribute\nand data does not have row.names. Assuming row names are integers.'))
	nimp <- integer(length(nams)); names(nimp) <- nams
	for(nam in nams) {
	  i <- imp[[nam]]
      if(!length(i)) {  # 10Mar01
        if(list.out) outlist[[nam]] <-
          if(missing(data)) get(nam, where.in) else data[[nam]] # 18Sep01
        next
      }
	  d <- dim(i)
	  if(!missing(imputation)) {
		if(!length(d)) 
		  stop('imputation can only be given when transcan used n.impute')
		if(imputation < 1 || imputation > d[2])
		  stop(paste('imputation must be between 1 and',d[2]))
		i <- i[,imputation]
	  } else if(length(d)) 
		stop('imputation must be specified when transcan used n.impute')

	  v <- if(missing(data)) get(nam, where.in) else data[[nam]] # 18Sep01
      ## Below was names(i) instead of match(...)  5Feb02
      if(length(namvar)) {
        sub <- match(names(i), namvar, nomatch=0)
        i <- i[sub > 0]
        sub <- sub[sub > 0]
      } else {
        if(!all.is.numeric(names(i)))
          stop(paste('names attribute of ',nam,
                     ' is not all numeric\n',
                     'and original observations did not have names',sep=''))
        sub <- as.integer(names(i))
      }
	  if(check)
		if((missing(imputation) || imputation==1) &&
			 !all(is.na(v[sub]))) stop(paste('variable',nam,
    'does not have same missing values as were present when transcan was run'))
      ## Added as.integer(i) below 5Feb02 (no particular reason but safety)
	  v[sub] <- if(is.factor(v)) levels(v)[as.integer(i)] else i
	  attr(v,'imputed') <- sub
	##	if(length(namvar))(1:length(v))[namvar %in% sub] else sub 5Feb02
	  if(!.SV4.) attr(v,'class') <- c('impute', attr(v,'class'))
      ## added !.SV4. 2may03
	  nimp[nam] <- length(i)
      if(list.out) outlist[[nam]] <- v else {
        if(missing(frame.out)) assign(nam, v, where=where.out) else
		assign(nam, v, frame=frame.out)
      }
	}
	if(pr) {
	  cat('\n\nImputed missing values with the following frequencies\n',
		  'and stored them in variables with their original names:\n\n')
	  print(nimp[nimp>0])
	}
    if(list.out) {  ## 5Feb02
      z <- sapply(outlist,length)
      if(diff(range(z)) > 0)
        stop('inconsistant naming of observations led to differing length vectors')
      return(outlist)
    }
	return(invisible(nimp))
  }

  impval <- imp[[name]]
  if(name %nin% names(imp))
    warning(paste('Variable',name,
                  'was not specified to transcan or had no NAs')) #13Aug01
  if(!length(impval)) return(var)
  d <- dim(impval)

  if(!missing(imputation)) {
	if(!length(d)) 
	  stop('imputation can only be given when transcan used n.impute')
	if(imputation < 1 || imputation > d[2])
	  stop(paste('imputation must be between 1 and',d[2]))
	impval <- impval[,imputation]
  } else if(length(d)) 
	stop('imputation must be specified when transcan used n.impute')

  namvar <- names(var)

  ## Begin 11apr03
  if(!length(namvar)) {
    if(missing(data))
	  stop(paste('variable',name,
                    'does not have a names() attribute\nand data= was not given.\nAssuming identifiers stored by transcan are integer subscripts')) else
    namvar <- row.names(data)
    if(!length(namvar))
    stop(paste('variable',name,
                  'does not have a names() attribute\nand data has no row.names'))
}
  ## End 11apr03
  
#  if(!length(namvar)) names(var) <- namvar <- as.character(1:length(var))
  if(length(namvar)) {
    sub <- match(names(impval), namvar, nomatch=0)
    impval <- impval[sub > 0]
    sub <- sub[sub > 0]
  } else {
    if(!all.is.numeric(names(impval)))
      stop(paste('names attribute of ',name,
                 ' is not all numeric\n',
                 'and original observations did not have names',sep=''))
    sub <- as.integer(names(impval))
  }

##Now take into account fact that transcan may have been
##run on a superset of current data frame

##  nam <- names(impval)[names(impval) %in% namvar] 5Feb02
##  m <- length(nam)
m <- length(sub)
if(check)
	if(missing(imputation) || imputation==1)if(m!=sum(is.na(var)))
     warning("number of NAs in var != number of imputed values from transcan.")
# Impute only works on same vectors originally given to transcan") 11apr03
  if(m==0) return(var)
#  if(is.factor(var)) var[nam] <- levels(var)[impval[nam]]  5Feb02
#  else var[nam] <- impval[nam]
  var[sub] <- if(is.factor(var)) levels(var)[as.integer(impval)] else impval
## was as.integer(imval) 1may03
  ##  lab <- label(var)
#  if(is.null(lab) || lab=="") lab <- name
#  lab <- paste(lab,"with",m,"NAs imputed")
#  attr(var, "label") <- lab
#  attr(var, "imputed") <- (1:length(var))[namvar %in% names(impval)] 5Feb02
  attr(var,'imputed') <- sub
  attr(var,'class') <- c("impute", attr(var,'class'))
  var
}

## "[.transcan" <- function(x, ..., drop=TRUE)  11apr03
"[.transcan" <- function(x, rows=1:d[1], cols=1:d[2], drop=TRUE)
{
  ## Check for old style object 7Nov00
  if(is.list(x)) {
    ## Begin 11apr03
    if(length(x$imputed) && sum(sapply(x$imputed,length))) {
      d <- dim(x$transformed)
      original.rownames <- dimnames(x$transformed)[[1]]
      subset.rownames   <- original.rownames[rows]
      for(v in names(x$imputed)) {
        z <- x$imputed[[v]]
        if(length(z)) {
          use <- names(z) %in% subset.rownames
          x$imputed[[v]] <- z[use]
        }
      }
    }
##    if(!length(x$transformed)) return(x)  End 11apr03
    x$transformed <- x$transformed[rows,cols, drop=drop]  ## was ... 11apr03
    return(x)
  }
  ats <- attributes(x)
  ats$dimnames <- ats$dim <- ats$names <- NULL
  attr(x, 'class') <- NULL
  y <- x[..., drop = drop]
  attributes(y) <- c(attributes(y), ats)
  if(is.null(dim(y)))
	{
	  aty <- attributes(y)
	  aty$call <- aty$iter <- aty$rsq <- aty$parms <- aty$coef <- 
		aty$xcoef <- aty$rsq.adj <- aty$shrink <-
          aty$fillin <- aty$imputed <- aty$class <- aty$ranges <- 
            aty$imp.con <- aty$scale <- aty$categorical <- aty$asis <-
              aty$trantab <- NULL
	  attributes(y) <- aty
	  if(is.character(z <- list(...)[[1]]))
		attr(y,"label") <- paste("Transformed",z)
      ##May someday have to use label(y) <- for this ?
	}
  y
}


predict.transcan <- function(object, newdata=NULL, iter.max=50, eps=.01, 
                             curtail=TRUE, type=c("transformed","original"),
                             inverse, tolInverse, ...) {
 type <- match.arg(type)

 if(!is.list(object)) object <- attributes(object) ## 7Nov00
 parms  <- object$parms
 coef   <- object$coef
 xcoef  <- object$xcoef
 fillin <- object$fillin
 ranges <- object$ranges
 scale  <- object$scale
 imp.con<- object$imp.con
 trantab<- object$trantab
 categorical <- object$categorical
 formula <- object$formula

 inverse <- if(missing(inverse)) object$inverse  # 1Nov01
 if(!length(inverse)) inverse <- 'linearInterp'
 tolInverse <- if(missing(tolInverse)) object$tolInverse
 if(!length(tolInverse)) tolInverse <- 0.05

 if(type=="original" & is.null(trantab))
	stop('type="trantab" and trantab=TRUE not specified to transcan')

 if(length(formula)) {
   oldop <- options(na.action="na.retain")
   y <- model.frame(formula, data=newdata)  #3Jun99 rm Des=F 10Aug01

   options(oldop)
#  if(attr(y,"terms"),"response")==1) y <- y[,-1,drop=TRUE]
   d <- dim(y)
   p <- d[2]
   newdata <- matrix(NA, nrow=d[1], ncol=p,
	dimnames=list(attr(y,"row.names"), names(y)))
   for(i in 1:p) {
      w <- y[[i]]
      if(is.character(w)) {
         warning("character predictor present.  Depending on levels being same as in original fit,\nthat all levels are present in the data, and that levels were in alphabetical order")
         w <- factor(w)
      }
      newdata[,i] <- oldUnclass(w)
   }
 } else {
   if(is.null(newdata)) stop("newdata must be given (unless formula was given to transcan)")
   p <- ncol(newdata)
 }
 if(!is.matrix(newdata)) {
   if(is.null(names(newdata))) names(newdata) <- dimnames(object)[[2]]
   newdata <- t(as.matrix(newdata))
 }

if(imp.con || !any(is.na(newdata))) iter.max <- 1  
#only 1 iteration needed if no NAs   (imp.con 7Apr95)
 xt <- newdata
 nam <- dimnames(object)[[2]]
 if(ncol(object)!=p) stop("wrong number of columns in newdata")
 if(is.null(dimnames(xt)[[2]]))dimnames(xt) <- list(dimnames(xt)[[1]],nam)
 else if(check && any(dimnames(newdata)[[2]]!=nam))
	warning("column names in newdata do not match column names in object")
 if(length(dimnames(xt)[[1]])==0) dimnames(xt) <- list(as.character(1:nrow(xt)),
	dimnames(xt)[[2]])

 for(iter in 1:iter.max)
 {
   dmax <- 0
   for(i in 1:p)
   {
      lab <- nam[i]
      j <- is.na(newdata[,i])
      prm <- parms[[lab]]
      if(length(prm)==0)  #asis
      {
	   newy <- newdata[,i]
	   if(any(j))newy[j] <- if(iter==1) fillin[i] else
	      drop(cbind(1,xt[j,-i,drop=FALSE]) %*% xcoef[i,-i-1])
      }
      else
      {
	   if(is.matrix(prm))  	#factor
	   {
	      lev <- attr(prm, "codes")
	      consec.lev <- match(newdata[,i], lev)  #may give NAs - OK for next line
	      R <- prm[consec.lev,, drop=FALSE]
	      if(iter==1 && any(match(newdata[!j,i], lev, 0)==0))
		stop("codes for categorical variable not in original list")
	   }
	   else R <- rcspline.eval(newdata[,i], prm, inclx=TRUE)
	   newy <- drop(cbind(1,R) %*% coef[[i]])
	   if(any(j)) newy[j] <- if(iter==1) 0 else
		drop(cbind(1, xt[j,-i,drop=FALSE]) %*%xcoef[i, -i-1])
     }
	if(curtail) newy <- pmax(pmin(newy,ranges[2,i]),ranges[1,i])
        if(iter>1) dmax <- max(dmax, min(
				max(abs(xt[,i]-newy),na.rm=TRUE),
				max(abs(-xt[,i]-newy),na.rm=TRUE))/scale[i])
	xt[,i] <- newy
   }	#end i
 niter <- iter
 if(niter>1 && dmax<eps) break
 if(rhsImp=='random' && niter>4) break  ## 25Mar02
 }	#end iter
 if(rhsImp=='mean') {
   if(iter.max>3 & niter==iter.max)
     stop(paste("no convergence in",iter.max,"iterations"))
   cat("Convergence in",niter,"iterations\n")
 }
 if(type=="transformed") return(xt)

for(i in 1:p) {
  ft <- trantab[[i]]
  j <- is.na(newdata[,i])
  if(any(j)) {
	newdata[j,i] <- if(imp.con) fillin[i]
	  else {
		## if(nam[i] %in% categorical) {
		## find category with scored value closest to given one
		## ww <- apply(outer(xt[j,i],ft$y,function(x,y)abs(x-y)),1,order)[1,]
		## newdata[j,i] <- ft$x[ww]
##		ww <- approx(ft$y, ft$x, xout=xt[j,i], rule=2)$y
        ww <- invertTabulated(ft, aty=xt[j,i], name=nam[i],
                              inverse=inverse, tolInverse=tolInverse)
		if(nam[i] %in% categorical) ww <- round(ww)
		ww
	  }
  }
}
newdata
}


Function <- function(object, ...)
   UseMethod("Function")

Function.transcan <- function(object, prefix=".", suffix="", where=1, ...)
{
at <- if(is.list(object)) object else attributes(object)  ## 7Nov00
Nam <- names(at$coef)  ## dimnames(x)[[2]] 7Nov00
p <- length(Nam)
categorical <- at$categorical
asis        <- at$asis
coef        <- at$coef
parms       <- at$parms
fnames      <- character(p)

for(i in 1:p)	{
  nam <- Nam[i]
  cof <- coef[[nam]]
  if(nam %in% asis) f <- function(x) x
  else if(nam %in% categorical)	{
     codes <- attr(parms[[nam]], "codes")
     g <- "{x <- oldUnclass(x);"
     cof[-1] <- cof[-1] + cof[1]  #convert from ref cell to cell means model
     for(j in 1:length(codes))	{
	if(j>1 && cof[j]>0) g <- paste(g,"+")
	g <- paste(g, format(cof[j]), "*(x==",format(codes[j]),")",sep="")
	}
     g <- paste(g, "}", sep="")
     f <- function(x) NULL
     f[[2]] <- parse(text=g)[[1]]
     }
  else f <- attr(rcspline.restate(parms[[nam]], cof), "function")
 
  fun.name <- paste(prefix,nam,suffix,sep="")
  cat("Function for transforming",nam,"stored as",fun.name,"\n")
  assign(fun.name, f, where=where)
  fnames[i] <- fun.name
  }
invisible(fnames)
}

na.retain <- function(mf) mf


plot.transcan <- function(x, ...) {
  ## check for old style object
  if(!is.list(x)) x <- attributes(x)   ## 7Nov00

trantab <- x$trantab
imputed <- x$imputed
if(length(trantab)==0) stop('you did not specify trantab=TRUE to transcan()')
p   <- length(trantab)
nam <- names(trantab)
for(w in nam) {
  z <- trantab[[w]]
  plot(z, xlab=w, ylab=paste('Transformed',w))
  title(sub=paste('R2=',format(round(at$rsq[w],2)),sep=''),cex=.4,adj=0)
  if(length(imputed)) {
    m <- imputed[[w]]
    if(L <- length(m)) {
      title(sub=paste(L,'missing'),cex=.4,adj=1)
      m.trans <- approx(z, xout=m, rule=2)$y
      scat1d(m, 3, ...)
      scat1d(m.trans, 4, ...)
    }
  }
}
}

#n.impute was at$n.impute  10Mar01
fit.mult.impute <- function(formula, fitter, xtrans, data,
							n.impute=xtrans$n.impute, fit.reps=FALSE, derived,
							pr=TRUE, subset, ...) {

#  at <- attributes(xtrans)  7Nov00
#  added data= 18Sep01
  using.Design <- FALSE
  fits <- if(fit.reps)vector('list',n.impute)
  used.mice <- any(oldClass(xtrans)=='mids')
  if(used.mice && missing(n.impute)) n.impute <- xtrans$m
  
  for(i in 1:n.impute) {
#	impute(xtrans, imputation=i, frame.out=1, pr=FALSE, check=FALSE)  10Mar01
    if(used.mice) completed.data <- complete(xtrans, i) else {
      completed.data <- data  # 18Sep01
      imputed.data <-
        impute.transcan(xtrans, imputation=i, data=data,
                        list.out=TRUE, pr=FALSE, check=FALSE)
      ## impute.transcan works for aregImpute
      completed.data[names(imputed.data)] <- imputed.data  # 18Sep01
    }
    if(!missing(derived)) {
      stop('derived variables in fit.mult.imputed not yet implemented')
      eval(derived, completed.data)
    }

	if(using.Design) options(Design.attr=da)
    f <- if(missing(subset)) fitter(formula, data=completed.data) else
    fitter(formula, data=completed.data[subset,])  # 10Mar01 16jul02
    # For some reason passing subset= causes model.frame bomb in R
	if(fit.reps) fits[[i]] <- f
	cof <- f$coef
	v <- Varcov(f)
	if(i==1) {
	  vavg <- 0*v
	  p <- length(f$coef)
	  bar <- rep(0, p)
	  vname <- names(f$coef)
	  cov <- matrix(0, nrow=p, ncol=p, dimnames=list(vname,vname))

	  if(inherits(f,'Design')) {
		using.Design <- TRUE
        da <- f$Design                         # 10Aug01
        if(!length(da)) da <- getOldDesign(f)  # 10Aug01
	  } else warning('Not using a Design fitting function; summary(fit) will use\nstandard errors, t, P from last imputation only.  Use Varcov(fit) to get the\ncorrect covariance matrix, sqrt(diag(Varcov(fit))) to get s.e.\n\n')
	}
	vavg <- vavg + v
	bar <- bar + cof
	cof <- as.matrix(cof)
	cov <- cov + cof %*% t(cof)
  }
  vavg <- vavg / n.impute    ## matrix \bar{U} in Rubin's notation
  bar <- bar/n.impute
  bar <- as.matrix(bar)
  ## Matrix B in Rubin's notation:
  cov <- (cov - n.impute * bar %*% t(bar))/(n.impute-1)
  U <- diag(vavg); B <- diag(cov)  ## save the diagonals of U and B

  cov <- vavg + (n.impute+1)/n.impute * cov  ## final covariance matrix

  r <- diag(cov) / diag(vavg)
  names(r) <- vname
  tau  <- (1 + 1/n.impute)*B/U
  missingInfo <- tau/(1+tau)
  dfmi <- (n.impute-1)*((1 + 1/tau)^2)
  if(pr) {
    cat('\nVariance Inflation Factors Due to Imputation:\n\n')
    print(round(r,2))
    cat('\nRate of Missing Information:\n\n')
    print(round(missingInfo,2))
    cat('\nd.f. for t-distribution for Tests of Single Coefficients:\n\n')
    print(round(dfmi,2))
  }
  
  f$coefficients <- drop(bar)
  f$var <- cov
  f$variance.inflation.impute <- r
  f$missingInfo <- missingInfo
  f$dfmi <- dfmi
  f$fits <- fits
  f$formula <- formula   ## 14jul02
#  attr(f,'class') <- c('fit.mult.impute', attr(f,'class')) 10Mar01
  if(using.Design) {
#    oldClass(f) <- 'Design'  18Apr02
    options(Design.attr=NULL)
  }
  f
}

Varcov.fit.mult.impute <- function(object, ...) object$var

##The following needed if Design is not in effect, to make anova work
Varcov <- function(object, ...) UseMethod("Varcov")

Varcov.default <- function(object, regcoef.only=FALSE, ...) {
  vc <- object$Varcov
  if(length(vc)) {
	if(regcoef.only) return(object$var) else
	return(vc(object,which='var'))
  }
   cov <- object$var
   if(is.null(cov)) stop("object does not have variance-covariance matrix")
   if(regcoef.only) {
	 p <- length(object$coef)
	 cov <- cov[1:p, 1:p, drop=FALSE]
   }
  cov
}

Varcov.lm <- function(object, ...) {
  cof <- object$coefficients
  if(.R.) {
    Qr <- object$qr
    cov <- chol2inv(Qr$qr)
  } else {
    rinv <- solve(object$R, diag(length(cof)))
    cov <- rinv %*% t(rinv)
  }
  cov <- sum(object$residuals^2)*cov/object$df.residual
  nm  <- names(cof)
  dimnames(cov) <- list(nm, nm)
  cov
}

Varcov.glm <- function(object, ...) {
  if(length(object$var)) return(object$var)  ## 24nov02, for glmD
  s <- summary.glm(object)
  s$cov.unscaled * s$dispersion
}

Varcov.multinom <- function(object, ...) vcov(object)

invertTabulated <- function(x, y, freq=rep(1,length(x)),
                            aty, name='value',
                            inverse=c('linearInterp','sample'),
                            tolInverse=0.05, rule=2) {
  inverse <- match.arg(inverse)
  if(is.list(x)) {
    freq <- x[[3]]
    y <- x[[2]]
    x <- x[[1]]
  }
#  if(name=='totcst') {  #TEMP
#    plot(x, y) #, xlim=c(0,6), ylim=c(-5,6.5))
#    scat1d(aty, side=2)
#    scat1d(y,   side=4)
#    title('Left: requested  Right:Tabulated')
#    title(sub=format(max(freq)),adj=0)
#  }
  if(inverse=='linearInterp')
    return(approx(y, x, xout=aty, rule=rule)$y)

  ##  del <- diff(wtd.quantile(y, freq, probs=c(.01,.99)))
  del <- diff(range(y, na.rm=TRUE))
  m <- length(aty)
  yinv <- if(.R.)double(m) else single(m)
  cant <- if(.R.)double(0) else single(0)
  for(i in 1:m) {
    a <- aty[i]
    s <- abs(y-a) < (tolInverse * del)
    nclose <- sum(s)
    if(nclose < 2) {
      if(nclose==0) cant <- c(cant, a)
      xest <- approx(y, x, xout=a, rule=rule)$y
      ## If a outside range of y, approx(rule=2) will return min or max
      ## x.  There may be many x's with y values near this extreme x.
      ## Take a random draw from them.
      a <- approx(x, y, xout=xest, rule=rule)$y
      s <- abs(y - a) < (tolInverse * del)
      nclose <- sum(s)
      if(nclose > 1) {
        maxdist <- max((y[s] - a)^2)
        wt <- if(maxdist==0) freq[s] else
         (1 - ((y[s] - a)^2) / maxdist) * freq[s]
        if(all(wt==0)) wt <- freq[s]  # y[s] all the same
        if(any(wt==0)) wt[wt==0] <- min(wt[wt>0])/2
        xest <- x[s][sample(nclose, 1, replace=FALSE,  prob=wt/sum(wt))]
      }
    } else {
      maxdist <- max((y[s] - a)^2)
      wt <- if(maxdist==0) freq[s] else
       (1 - ((y[s] - a)^2) / maxdist) * freq[s]
      if(all(wt==0)) wt <- freq[s]  # y[s] all the same
      if(any(wt==0)) wt[wt==0] <- min(wt[wt>0])/2
      
      xest <- x[s][sample(nclose, 1, replace=FALSE,  prob=wt/sum(wt))]
      ## sample(x[s],...) fails if x[s] is scalar; thanks: Bill Dunlap
    }
    yinv[i] <- xest
  }
#  if(name=='totcst') {
#    scat1d(yinv, side=1)
#    plot(aty, yinv)
#    histSpike(aty, side=3, add=T)
#    histSpike(yinv, side=4, add=T)
#    prn(table(yinv))
#    browser()
#  }
  if(length(cant))
    warning(paste('No actual ',name, ' has y value within ',
                  format(tolInverse),
                  '* range(y) (',format(del),
                  ') of the following y values:',
                  paste(format(sort(unique(cant))),collapse=' '),
                  '.\nConsider increasing tolInverse. ',
                  'Used linear interpolation instead.',sep=''))
    
  yinv
}

if(FALSE) {
par(mfrow=c(2,3))
w <- transcan(~totcst+age+dzgroup+scoma+meanbp+pafi+alb+crea+
  urine,imputed=TRUE,pl=FALSE,imputed.actual='datadensity',
  n.impute=2, inverse=c('linearInterp','sample')[2],data=support,tolInverse=1)

par(mfrow=c(3,3))
#par(mfrow=c(1,1))
w <- transcan(~totcst+alb+meanbp+pafi, imputed=TRUE, pl=TRUE,
              imputed.actual='ecdf', n.impute=2,
              inverse=c('linearInterp','sample')[2],
              data=support, tolInverse=.05)
}

# Trick taken from MICE impute.polyreg
rMultinom <- function(probs, m) {
  d <- dim(probs)
  n <- d[1]
  k <- d[2]
  lev <- dimnames(probs)[[2]]
  if(!length(lev)) lev <- 1:k
  ran <- matrix(lev[1], ncol=m, nrow=n)
  z <- apply(probs, 1, sum)
  if(any(abs(z-1) > .00001))
     stop('error in multinom: probabilities do not sum to 1')
  for(i in 1:m) {
    un <- rep(runif(n), rep(k,n))
    ran[,i] <- lev[1 + apply(un > apply(probs,1,cumsum),2,sum)]
  }
  ran
}

    
translate <- if(!.R. && !under.unix) 
  function(text, old, new, multichar) {
    if(!missing(multichar) && !multichar)
      stop('multichar=F not implemented for this operating system')
    sedit(text, old, new)
  } else if(FALSE && .R.) function(text, old, new, multichar=FALSE) {
    if(multichar) stop('multichar=T not implemented under R')
    k <- chartr(old, new, text)
    if(is.matrix(text)) k <- matrix(k, nrow=nrow(text))
    k
  } else
  function(text, old, new, multichar=FALSE) {
    if(length(old)>1 || (nchar(old)!=nchar(new))) multichar <- TRUE
    if(length(old)>1 && (length(new)>1 & length(new)!=length(old)))
      stop("old and new must have same lengths or new must have 1 element")

    if(.R. && !multichar) k <- chartr(old, new, text)  ## 27aug03
    else {
      if(multichar) command <- paste("sed",paste('-e "s/',old,"/",new,'/g"',
                                                 sep="", collapse=" "))
      else command <- paste("tr \"", old, "\" \"", new, "\"", sep="")
      ##    k <- sys(command, text)  replace with next 2 27aug03
      ## Thanks:   <Sebastian.Weber@aventis.com>  
      k <- unlist(lapply(text, function(x) {
        sys(paste("echo \"", x, "\" | ", command, sep="")) }))
    }
    if(is.matrix(text)) k <- matrix(k, nrow=nrow(text))
    k
  }
if(.R.) units <- function(x,...)  UseMethod("units")

"units<-"  <- function(x, value) {
    attr(x, "units") <- value
    x
    }

units.default <- function(x, none='', ...)  {
	lab <- attr(x, "units")
	if(is.null(lab)) lab <- attr(attr(x,'tspar'),'units')
	if(is.null(lab)) lab <- none
	lab
	}
#Added since sent to statlib:
# Fixed match.arg(type)
varclus <-
  function(x,
           similarity=c("spearman","pearson","hoeffding",
             "bothpos","ccbothpos"), 
           type=c("data.matrix","similarity.matrix"),
           method=if(.R.)"complete" else "compact",
           data, subset, na.action, minlev=.05) {


  call <- match.call()
  type <- match.arg(type)  #moved from 2 lines down 2Apr95
  if(type!="similarity.matrix") similarity <- match.arg(similarity)
  nact <- NULL

  if(.R.) require('mva')

  if(inherits(x,"formula")) {
    form <- x   # 20Mar01
	oldops <- options(contrasts=c("contr.treatment","contr.poly"))
	y <- match.call(expand=FALSE)
	y$x <- y$similarity <- y$type <- y$method <- y$minlev <- NULL
	y$formula <- x
	if(missing(na.action)) y$na.action <- na.retain
	y[[1]] <- as.name("model.frame")
    #See if Des argument exists in current model.frame.default
    #    if(length(model.frame.default$Des)) y$Des  <- F   #turn off Design
	x <- eval(y, sys.parent())
	drop <- NULL; nam <- names(x)
    nv <- length(x)  ## 8may02 - R redefines length if change list below
	if(minlev > 0) for(i in 1:length(x)) {
	  if(is.character(x[[i]])) x[[i]] <- as.factor(x[[i]])
	  if(is.factor(x[[i]])) {
		x[[i]] <- combine.levels(x[[i]],minlev)
		if(length(levels(x[[i]]))<2) {
##		  x[[i]] <- NULL  8may02  model.matrix will drop these
		  drop <- c(drop,i)
		  warning(paste('variable',nam[i],
'ignored since it has no level with relative frequency of\nat least',
						format(minlev)))
		}
	  }
	}
	Terms <- attr(x,'terms')
	if(length(drop)) Terms <- Terms[-drop]
#    if(length(drop)) Terms <- termsDrop(Terms, drop, data=x)
	nact <- attr(x,"na.action")
	x <- model.matrix(Terms, x)
#	if(attr(x,"term.labels")[1]=="(Intercept)") x <- x[,-1]  #20Mar01 for R
    if(dimnames(x)[[2]][1]=='(Intercept)') x <- x[,-1] # was [[1]] 3May01
	form <- TRUE
	options(oldops)
	type <- "data.matrix"
  }
  else form <- FALSE
  n <- NULL
  if(mode(x)!="numeric") stop("x matrix must be numeric")
  if(type=="data.matrix") {       # assume not a correlation matrix
    if(similarity %in% c("bothpos","ccbothpos")) {
      isthere <- 1*(!is.na(x))
      x[is.na(x)] <- 0
      x[x > 0] <- 1
      n <- crossprod(isthere)
      x <- crossprod(x)/n
      if(similarity=='ccbothpos') {
        cc <- diag(x) %*% t(diag(x))
        cc[row(cc)==col(cc)] <- 0
        x <- x - cc
      }
    } else if(similarity=="hoeffding") {
	D <- hoeffd(x); x <- D$D; n <- D$n 
    } else {
      D <- rcorr(x, type=similarity)
      x <- (D$r)^2
      n <- D$n
    }
  } else if(diff(dim(x))!=0) 
	stop("x must be square to be a similarity matrix")
  if(any(is.na(x))) {
      cat("Part of the similarity matrix could not be computed:\n")
      x[x<.01] <- 0
      print(x, digits=2)
      stop()
    }
  if(similarity=='ccbothpos') w <- NULL else
  w <- if(.R.) hclust(as.dist(1-x), method=method) else
  hclust(sim=x, method=method)
  structure(list(call=call, sim=x, n=n, hclust=w, similarity=similarity,
                 method=method, na.action=nact),class="varclus")
}

print.varclus <- function(x, ...) {
dput(x$call); cat("\n")
if(length(x$na.action)) naprint(x$na.action)
s <- c(hoeffding="30 * Hoeffding D",spearman="Spearman rho^2",
		pearson="Pearson r^2",bothpos="Proportion",
        ccbothpos="Chance-Corrected Proportion")[x$similarity]
cat("\nSimilarity matrix (",s,")\n\n",sep="")
k <- x$sim
lab <- abbreviate(dimnames(k)[[2]])
dimnames(k) <- list(lab,lab)
print.matrix(round(k, 2))
n <- x$n
if(length(n)) {
  if(length(n)==1) cat("\nNo. of observations used=", n,"\n\n") else {
    cat("\nNo. of observations used for each pair:\n\n")
    dimnames(n) <- list(lab,lab)
    print(n)
  }
}
cat("\nhclust results (method=",x$method,")\n\n",sep="")
print(x$hclust)
invisible()
}

plot.varclus <- function(x, ylab, abbrev=FALSE, legend.=FALSE, loc, maxlen=20,
                         labels=NULL, ...) {
  if(missing(ylab)) {
    s <- c(hoeffding="30 * Hoeffding D",
           spearman=if(.R.)expression(paste(Spearman,~rho^2)) else
           "Spearman rho^2",
		pearson=if(.R.)expression(paste(Pearson,~r^2)) else "Pearson r^2",
           bothpos="Proportion",
           ccbothpos="Chance-Corrected Proportion")[x$similarity]
#    if(s=="") s <- x$similarity  1Apr02
#    if(is.na(s)) s <- x$similarity
    if((is.expression(s) && s=='NULL') ||
       (!is.expression(s) && (is.na(s) || s=='')))
      s <- x$similarity  ## 8may02 9sep02
    ylab <- if(.R.) s else paste("Similarity (",s,")",sep="")
  }
  if(legend.) abbrev <- TRUE
  if(!length(labels)) labels <- dimnames(x$sim)[[2]]
  olabels <- labels
  if(abbrev) labels <- abbreviate(labels)
  if(!length(x$hclust))
    stop('clustering was not done on similarity="ccbothpos"')
  p <- if(.R.) {
    ## if(T & existsFunction('plclust'))  - didn't help with similarities
    #  plclust(x$hclust, labels=labels, ylab=ylab, ...) else {
        plot(x$hclust, labels=labels, ann=FALSE, axes=FALSE, ...)
        ya <- pretty(range(1-x$hclust$height))
        axis(2, at=1-ya, labels=format(ya))
        title(ylab=ylab)
  } else plclust(x$hclust, labels=labels, ylab=ylab, ...)
  s <- labels != olabels
  if(legend. && any(s)) {
      if(missing(loc)) {
        cat("Click mouse at upper left corner of legend\n")
        loc <- locator(1)
      }
      olabels <- ifelse(nchar(olabels)>maxlen, substring(olabels,1,maxlen),
	olabels)
      text(loc, paste(paste(labels[s],":",olabels[s],"\n"),
	collapse=""), adj=0)
  }
  invisible(p)
}

na.retain <- function(mf) mf


naclus <- function(df, method=if(.R.)"complete" else "compact") {
  ismiss <- function(x) if(is.character(x))x=='' else is.na(x) 
  na <- sapply(df, ismiss)*1

#  y <- apply(na, 2, sum)
  n <- nrow(na)
#  i <- y==0 | y==n
#  if(any(i)) warning(paste("The following variables are always or\nnever missing and were excluded from consideration:\n",
#	paste(dimnames(na)[[2]][i],collapse=" ")))
#  if(all(i)) NULL else   was varclus(na[,!i]...
  sim <- crossprod(na)/n
  res <- varclus(sim, type="similarity.matrix", similarity="Fraction Missing",
				 method=method)
  na.per.obs <- apply(na, 1, sum)
  nc <- ncol(na)
  mean.na <- rep(NA, nc)
  names(mean.na) <- dimnames(na)[[2]]
  for(i in 1:nc) {
	y <- na[,i]==1
	if(any(y)) mean.na[i] <- mean(na.per.obs[y]) - 1
	NULL
  }
  res$na.per.obs <- na.per.obs
  res$mean.na    <- mean.na
  res
}

naplot <- function(obj, which=c('all','na per var','na per obs','mean na',
						  'na per var vs mean na'),
				   ...) {
  which <- match.arg(which)
  tab <- table(obj$na.per.obs)
  na.per.var <- diag(obj$sim)
  names(na.per.var) <- dimnames(obj$sim)[[2]]
  mean.na <- obj$mean.na

  if(which %in% c('all','na per var'))
	dotchart(sort(na.per.var), xlab='Fraction of NAs', 
			 main='Fraction of NAs in each Variable', ...)

  if(which %in% c('all','na per obs'))
	dotchart2(tab, auxdata=tab, reset.par=TRUE,
	   xlab='Frequency', 
	   main='Number of Missing Variables Per Observation', ...)

  if(which %in% c('all','mean na'))
	dotchart(sort(mean.na), 
			 xlab='Mean Number of NAs',
	 main='Mean Number of Other Variables Missing for\nObservations where Indicated Variable is NA',
			 ...)

  if(which %in% c('all','na per var vs mean na')) {
    if(.R.) { # 31jul02
      xpd <- par('xpd')
      par(xpd=NA)
      on.exit(par(xpd=xpd))
    }
	plot(na.per.var, mean.na, xlab='Fraction of NAs for Single Variable',
		 ylab='Mean # Other Variables Missing', type='p')
	usr <- par('usr')
	eps <- .015*diff(usr[1:2]); epsy <- .015*diff(usr[3:4])
	s <- (1:length(na.per.var))[!is.na(mean.na)]
	taken.care.of <- NULL
	for(i in s) {
	  if(i %in% taken.care.of) next
	  w <- s[s > i & abs(na.per.var[s]-na.per.var[i]) < eps &
	    abs(mean.na[s]-mean.na[i]) < epsy]
	  if(any(w)) {
		taken.care.of <- c(taken.care.of, w)
		text(na.per.var[i]+eps, mean.na[i],
			 paste(names(na.per.var[c(i,w)]),collapse='\n'),adj=0)
	  } else text(na.per.var[i]+eps, mean.na[i], names(na.per.var)[i], adj=0)
	}
  }

  invisible(tab)
}

combine.levels <- function(x, minlev=.05) {
  x <- as.factor(x)
  lev <- levels(x)
  f <- table(x)/sum(!is.na(x))
  i <- f < minlev
  si <- sum(i)
  if(si==0) return(x)
  if(.R.) {
    comb <- if(si==1) names(sort(f))[1:2] else names(f)[i]
    keepsep <- setdiff(names(f), comb)
    names(keepsep) <- keepsep
    w <- c(list(OTHER=comb), keepsep)
    levels(x) <- w
  } else levels(x) <- if(si==1) list(OTHER=names(sort(f))[1:2]) else
  list(OTHER=names(f)[i])
  ## added OTHER in first list() 16Apr02.  Thanks: Peter Malewski
  x
}

plotMultSim <- function(s, x=1:dim(s)[3],
                        slim=range(pretty(c(0,max(s,na.rm=TRUE)))),
                        slimds=FALSE,
                        add=FALSE, lty=par('lty'), col=par('col'),
                        lwd=par('lwd'), vname=NULL, h=.5, w=.75,
                        u=.05, labelx=TRUE, xspace=.35) {
if(!length(vname)) vname <- dimnames(s)[[1]]
p <- dim(s)[1]
if(length(vname) != p) stop('wrong length for vname')
if(p != dim(s)[2]) stop('similarity matrix not square')
if(length(x) != dim(s)[3])
  stop('length of x differs from extent of 3rd dimension of s')

if(!add) {
#  omar <- par('mar')
#  mar <- omar
#  mar[c(1,2,4)] <- 0
#  par(mar=mar)
#  on.exit(par(mar=omar))   first arg to plot was .25 1oct02
  plot(c(-xspace,p+.5),c(.5,p+.25), type='n', axes=FALSE, xlab='',ylab='')
  if(labelx) text(1:p, rep(.6,p), vname, adj=.5)
  text(rep(.5,p), 1:p, vname, adj=1)
}

scaleit <- function(x, xlim, lim) lim[1] +
  (x-xlim[1])/diff(xlim) * diff(lim)

if(slimds) {
  slim.diag <- -1e10
  for(k in 1:length(x)) {
    sk <- s[,,k]
    r <- max(diag(sk))
    slim.diag <- max(slim.diag, r)
  }
  slim.diag <- range(pretty(c(0,slim.diag)))
  slim.offdiag <- slim.diag - diff(slim.diag)/2
}

rx  <- range(x)
rxe <- c(rx[1]-u*diff(rx), rx[2]+u*diff(rx))

for(i in 1:p) {
  for(j in 1:p) {
    if((i==j) && all(s[i,j,]==1)) next
    sl <- if(slimds) if(i==j) slim.diag else slim.offdiag else slim
    sle <- c(sl[1]-u*diff(sl), sl[2]+u*diff(sl))

    if(!add) {
      lines(c(i-w/2,i+w/2,i+w/2,
              i-w/2,i-w/2),
            c(j-h/2,j-h/2,j+h/2,
              j+h/2,j-h/2), col=if(.R.)gray(.5) else .5, lwd=.65)
      xc <- rep(i-w/2-u/3,2)
      yc <- scaleit(sl, sle, c(j-h/2,j+h/2))
#      if((!slimds && i==1 && j==1) || (slimds && (i==1 & j<=2))) {
      if(i==1 && j<=2) {
        text(xc, yc,
             format(sl,digits=2), adj=1, cex=.7)
        segments(rep(xc+u/8,2),yc,
                 rep(xc+u/3,2),yc)
      }
    }
    lines(scaleit(x, rxe, c(i-w/2,i+w/2)),
          scaleit(s[i,j,], sle, c(j-h/2,j+h/2)),
          lty=lty, lwd=lwd, col=col)
    if(!add && slimds && (i!=j))
      lines(c(i-w/2,i+w/2),
            rep(scaleit(0, sle, c(j-h/2,j+h/2)),2),
            col=if(.R.)gray(.5) else .5)
  }
}
invisible(slim)
}

wtd.mean <- function(x, weights=NULL, normwt='ignored', na.rm=TRUE) {
  if(!length(weights)) return(mean(x, na.rm=na.rm))
  if(na.rm) {
	s <- !is.na(x + weights)
	x <- x[s]
	weights <- weights[s]
  }
  sum(weights*x)/sum(weights)
}

wtd.var <- function(x, weights=NULL, normwt=FALSE, na.rm=TRUE) {
  if(!length(weights)) {
	if(na.rm) x <- x[!is.na(x)]
	return(var(x))
  }
  if(na.rm) {
	s <- !is.na(x + weights)
	x <- x[s]
	weights <- weights[s]
  }
  if(normwt) weights <- weights*length(x)/sum(weights)
  xbar <- sum(weights*x)/sum(weights)
  sum(weights*((x - xbar)^2)) / (sum(weights) - 1)
}

wtd.quantile <- function(x, weights=NULL, probs=c(0, .25, .5, .75, 1), 
						 type=c('quantile','(i-1)/(n-1)','i/(n+1)','i/n'), 
						 normwt=FALSE, na.rm=TRUE) {
  if(!length(weights)) return(quantile(x, probs=probs, na.rm=na.rm))
  type <- match.arg(type)
  if(any(probs < 0 | probs > 1))
	stop("Probabilities must be between 0 and 1 inclusive")
  nams <- paste(format(round(probs * 100, if(length(probs) > 1) 
							 2 - log10(diff(range(probs))) else 2)), 
				"%", sep = "")

  if(type=='quantile') {
	w <- wtd.table(x, weights, na.rm=na.rm, normwt=normwt, type='list')
	x     <- w$x
	wts   <- w$sum.of.weights
	n     <- sum(wts)
	order <- 1 + (n - 1) * probs
	low   <- pmax(floor(order), 1)
	high  <- pmin(low + 1, n)
	order <- order %% 1
	## Find low and high order statistics
	## These are minimum values of x such that the cum. freqs >= c(low,high)
	allq <- approx(cumsum(wts), x, xout=c(low,high), 
				   method='constant', f=1, rule=2)$y
	k <- length(probs)
	quantiles <- (1 - order)*allq[1:k] + order*allq[-(1:k)]
	names(quantiles) <- nams
	return(quantiles)
  } 
  w <- wtd.ecdf(x, weights, na.rm=na.rm, type=type, normwt=normwt)
  structure(approx(w$ecdf, w$x, xout=probs, rule=2)$y, 
			names=nams)
}

wtd.ecdf <- function(x, weights=NULL, 
					 type=c('i/n','(i-1)/(n-1)','i/(n+1)'), 
					 normwt=FALSE, na.rm=TRUE) {
  type <- match.arg(type)
  switch(type,
		 '(i-1)/(n-1)'={a <- b <- -1},
		 'i/(n+1)'    ={a <- 0; b <- 1},
		 'i/n'        ={a <- b <- 0})
  if(!length(weights)) {
#	.Options$digits <- 7  ## to get good resolution for names(table(x))6Aug00
    oldopt <- options(digits=7)
	on.exit(options(oldopt))
    cumu <- table(x)    ## R does not give names for cumsum
    isdate <- isChron(x)  ## 31aug02
    ax <- attributes(x)
    ax$names <- NULL
    x <- as.numeric(names(cumu))
    if(isdate) attributes(x) <- c(attributes(x),ax)
    cumu <- cumsum(cumu)
	cdf <- (cumu + a)/(cumu[length(cumu)] + b)
	if(cdf[1]>0) {x <- c(x[1], x); cdf <- c(0,cdf)}
	return(list(x = x, ecdf=cdf))
  }
  w <- wtd.table(x, weights, normwt=normwt, na.rm=na.rm)
  cumu <- cumsum(w$sum.of.weights)
  cdf <- (cumu + a)/(cumu[length(cumu)] + b)
  list(x = c(if(cdf[1]>0) w$x[1], w$x), ecdf=c(if(cdf[1]>0)0, cdf))
}

wtd.table <- function(x, weights=NULL, type=c('list','table'), 
					  normwt=FALSE, na.rm=TRUE) {
  type <- match.arg(type)
  if(!length(weights)) weights <- rep(1, length(x))
  isdate <- isChron(x)  ## 31aug02 + next 2
  ax <- attributes(x)
  ax$names <- NULL
  x <- if(is.character(x)) as.category(x) else oldUnclass(x)
  lev <- levels(x)
  if(na.rm) {
    s <- !is.na(x + weights)
    x <- x[s,drop=FALSE]    ## drop is for factor class
    weights <- weights[s]
  }
  n <- length(x)
  if(normwt) weights <- weights*length(x)/sum(weights)
  i <- order(x)  # R does not preserve levels here
  x <- x[i]; weights <- weights[i]

  if(any(diff(x)==0)) {  ## slightly faster than any(duplicated(xo))
	weights <- tapply(weights, x, sum)
    if(length(lev)) {    ## 3apr03
      levused <- lev[sort(unique(x))]  ## 7sep02
      ## Next 3 lines 21apr03
      if((length(weights) > length(levused)) &&
         any(is.na(weights))) weights <- weights[!is.na(weights)]
      if(length(weights) != length(levused)) stop('program logic error')
      names(weights) <- levused   # 10Apr01  length 16May01
    }
    if(!length(names(weights))) stop('program logic error')  # 16May01
    if(type=='table') return(weights)
    x <- all.is.numeric(names(weights),'vector')
    if(isdate) attributes(x) <- c(attributes(x),ax)   ## 31aug02
    names(weights) <- NULL
    return(list(x=x, sum.of.weights=weights))
}

  xx <- x  ## 31aug02
  if(isdate) attributes(xx) <- c(attributes(xx),ax)
  if(type=='list') list(x=if(length(lev))lev[x] else xx, 
       sum.of.weights=weights) else {
         names(weights) <- if(length(lev)) lev[x] else xx
         weights
       }
}

wtd.rank <- function(x, weights=NULL, normwt=FALSE, na.rm=TRUE) {
  if(!length(weights)) return(rank(x),na.last=if(na.rm)NA else TRUE)

  tab <- wtd.table(x, weights, normwt=normwt, na.rm=na.rm)

  freqs <- tab$sum.of.weights
  ## rank of x = # <= x - .5 (# = x, minus 1)
  r <- cumsum(freqs) - .5*(freqs-1)
  ## Now r gives ranks for all unique x values.  Do table look-up
  ## to spread these ranks around for all x values.  r is in order of x
  approx(tab$x, r, xout=x)$y
}

wtd.loess.noiter <- function(x, y, weights=rep(1,n), robust=rep(1,n), 
							 span=2/3, degree=1, cell=.13333, 
							 type=c('all','ordered all','evaluate'), 
							 evaluation=100, na.rm=TRUE) {
  type <- match.arg(type)
  if(.R.) require('modreg')
  n <- length(y)
  if(na.rm) {
	s <- !is.na(x+y+weights)
	x <- x[s]; y <- y[s]; weights <- weights[s]; n <- length(y)
  }
  robust <- weights * robust
  max.kd <- max(200, n)
  y <- if(.R.) .C("loess_raw",
		  as.double(y),
		  as.double(x),
		  as.double(weights),
		  as.double(robust),
		  as.integer(1),
		  as.integer(n),
		  as.double(span),
		  as.integer(degree),
		  as.integer(1),
		  as.integer(2),
		  as.integer(0),
		  as.double(cell),
		  as.character('interpolate/none'),
		  fitted.values = double(n),
		  parameter = integer(7),
		  a = integer(max.kd),
		  xi = double(max.kd),
		  vert = double(2),
		  vval = double(2 * max.kd),
		  diagonal = double(n),
		  trace.hat = double(1),
		  one.delta = double(1),
		  two.delta = double(1),
		  as.integer(FALSE), PACKAGE="modreg")$fitted.values else
    .C("loess_raw",
		  specialsok = TRUE,
		  as.double(y),
		  as.double(x),
		  as.double(weights),
		  as.double(robust),
		  as.integer(1),
		  as.integer(n),
		  as.double(span),
		  as.integer(degree),
		  as.integer(1),
		  as.integer(2),
		  as.integer(0),
		  as.double(cell),
		  as.character('interpolate/none'),
		  fitted.values = double(n),
		  parameter = integer(7),
		  a = integer(max.kd),
		  xi = double(max.kd),
		  vert = double(2),
		  vval = double(2 * max.kd),
		  diagonal = double(n),
		  trace.hat = double(1),
		  one.delta = double(1),
		  two.delta = double(1),
		  as.integer(FALSE))$fitted.values
switch(type,
	   all=list(x=x, y=y),
	   'ordered all'={i <- order(x); list(x=x[i],y=y[i])},
	   evaluate={
		 r <- range(x, na.rm=na.rm)
		 approx(x, y, xout=seq(r[1], r[2], length=evaluation))
	   })
}

num.denom.setup <- function(num, denom) {
  n <- length(num)
  if(length(denom) != n) stop('lengths of num and denom must match')
  s <- (1:n)[!is.na(num + denom) & denom != 0]
  num <- num[s]; denom <- denom[s]
  subs <- s[num > 0]
  y <- rep(1, length(subs))
  wt <- num[num > 0]
  other <- denom - num
  subs <- c(subs, s[other > 0])
  wt <- c(wt, other[other > 0])
  y <- c(y, rep(0, sum(other>0)))
  list(subs=subs, weights=wt, y=y)
}
Cbind <- function(...) {    # See llist function with Hmisc label function
  dotlist <- list(...)
  if(is.matrix(dotlist[[1]])) {
	y <- dotlist[[1]]
	ynam <- dimnames(y)[[2]]
	if(!length(ynam))
	  stop('when first argument is a matrix it must have column dimnames')
	other <- y[,-1,drop= FALSE]
	return(structure(y[,1], class='Cbind', label=ynam[1], other=other))
  }
  lname <- names(dotlist)
  name <- vname <- as.character(sys.call())[-1]
  for(i in 1:length(dotlist)) {
        vname[i] <- if(length(lname)) lname[i] else ''
        ## Added length() and '' 12Jun01, remove length(vname[i])==0 below
        if(vname[i]=='') vname[i] <- name[i]
	  }

  lab <- attr(y <- dotlist[[1]],'label')
  if(!length(lab)) lab <- vname[1]
  if(!is.matrix(other <- dotlist[[2]]) || ncol(other)<2) {  #9Jan98
	other <- as.matrix(as.data.frame(dotlist))[,-1,drop= FALSE]
	dimnames(other)[[2]] <- vname[-1]
  }
  structure(y, class='Cbind', label=lab, other=other)
}

if(.R.) as.numeric.Cbind <- as.double.Cbind <- function(x, ...) x
# Keeps xyplot from stripping off "other" attribute in as.numeric

#c.Cbind <- function(...) {
#  res <- oth <- numeric(0)
#  for(a in list(...)) {
#    lab <- attr(a,'label')
#    res <- c(res, oldUnclass(a))
#    oth <- rbind(oth, attr(a,'other'))
#  }
#  structure(res, class='Cbind', label=lab, other=oth)
#}

'[.Cbind' <- function(x, ...) {
  structure(oldUnclass(x)[...], class='Cbind',
			label=attr(x,'label'),
			other=attr(x,'other')[...,,drop= FALSE])
}

prepanel.xYplot <- function(x, y, ...) {
  xlim <- range(x, na.rm=TRUE)
  ylim <- range(y, attr(y,'other'), na.rm=TRUE)
  list(xlim=xlim, ylim=ylim, dx=diff(xlim), dy=diff(ylim))
}

## MB add method="filled bands" 
## MB use col.fill to specify colors for filling bands
panel.xYplot <-
  function(x, y, subscripts, groups = NULL, 
           type = if(is.function(method) || method == "quantiles")
           "b" else "p", 
           method = c("bars", "bands", "upper bars", "lower bars", 
             "alt bars", "quantiles", "filled bands"), 
           methodArgs = NULL, label.curves = TRUE, abline, 
           probs = c(0.5, 0.25, 0.75), nx, cap = 0.015, lty.bar = 1, 
           lwd = plot.line$lwd, lty = plot.line$lty, 
           pch = plot.symbol$pch, cex = plot.symbol$cex, 
           font = plot.symbol$font, col = NULL, 
           lwd.bands = NULL, lty.bands = NULL, col.bands = NULL, 
           minor.ticks = NULL, col.fill = NULL, ...)
{
  if(missing(method) || !is.function(method))
    method <- match.arg(method)   # was just missing() 26Nov01
  type <- type   # evaluate type before method changes 9May01
  if(length(groups)) groups <- as.factor(groups)
  other <- attr(y, "other")
  if(length(other)) {
    nother <- ncol(other)
    if(nother == 1) {
      lower <- y - other
      upper <- y + other
    }
    else {
      lower <- other[, 1]
      upper <- other[, 2]
    }
  }
  else nother <- 0
  y <- oldUnclass(y)
  g <- as.integer(groups)[subscripts]
  ng <- if(length(groups)) max(g) else 1
  levnum <- if(length(groups)) sort(unique(g)) else 1
  if(is.function(method) || method == "quantiles") {
    ## 2Mar00
    if(!is.function(method)) {
      method <- quantile  # above: methodArgs=NULL
      if(!length(methodArgs))
        methodArgs <- list(probs = probs)
    }
    if(length(methodArgs))
      methodArgs$na.rm <- TRUE
    else methodArgs <- list(na.rm = TRUE)
    if(ng == 1) {
      if(missing(nx))
        nx <- min(length(x)/4, 40)    
      ## Next 2 lines 2Mar00
      xg <- if(nx)
        as.numeric(as.character(cut2(x, 
                                     m = nx, levels.mean = TRUE))) else x
      dsum <- do.call("summarize",
                      c(list(y, llist(xg = xg), method, type = "matrix", 
                             stat.name = "Z"), methodArgs))
    }
    else {
      xg <- x
      if(missing(nx) || nx)
        for(gg in levnum) {
          ## 2Mar00
          w <- g == gg
          if(missing(nx))
            nx <- min(sum(w)/4, 40)
          xg[w] <-
            as.numeric(as.character(cut2(xg[w], m = nx,
                                         levels.mean = TRUE)))
        }
      dsum <- do.call("summarize",
                      c(list(y, by = llist(g, xg),
                             method, type = "matrix", stat.name = "Z"), 
                        methodArgs))
      g <- dsum$g
      groups <- factor(g, 1:length(levels(groups)),
                       levels(groups))
      subscripts <- TRUE     ## 6Dec00
    }
    x <- dsum$xg
    y <- dsum$Z[, 1, drop = TRUE]
    other <- dsum$Z[, -1]
    nother <- 2
    method <- "bands"
  }
  plot.symbol <- trellis.par.get(if(ng > 1) "superpose.symbol"
   else "plot.symbol")
  plot.line <- trellis.par.get(if(ng > 1) "superpose.line"
   else "plot.line")
  ## MB 04/17/01 default colors for filled bands
  ## 'pastel' colors matching superpose.line$col
  plot.fill <- c(9, 10, 11, 12, 13, 15, 7) 
  ##The following is a fix of panel.xyplot to work for type='b'
  ppanel <- function(x, y, type, cex, pch, font, lwd, lty, col, ...) {
    ##      if(type == "l")   9May01
    gfun <- ordGridFun(.R.)
    if(type != 'p') gfun$lines(x, y, lwd = lwd, lty = lty, col = col, ...)
    ##rm type=type 9May01

    if(type !='l') gfun$points(x=x, y=y,
         ## size=if(.R.)unit(cex*2.5,"mm") else NULL,
         pch = pch, font = font, cex = cex, col = col, 
         type = type, lwd=lwd, lty=lty, ...)
  }

  ##The following is a fix for panel.superpose for type='b' 
  pspanel <- function(x, y, subscripts, groups, type, lwd, lty, 
                      pch, cex, font, col, ...) {
    gfun <- ordGridFun(.R.)
    
	groups <- as.numeric(groups)[subscripts]
	N <- seq(along = groups)
	for(i in sort(unique(groups))) {
	  which <- N[groups == i]	# j <- which[order(x[which])]	
										# sort in x
	  j <- which	# no sorting
	  if(type != "p") gfun$lines(x[j], y[j],
           col = col[i], lwd = lwd[i], lty = lty[i], 
           ...)  # remove type=type[i] 9May01

      if(type !='l') gfun$points(x[j], y[j],
           ## size=if(.R.) unit(cex[i]*2.5, 'mm') else NULL,
           col = col[i], pch = pch[i], cex = cex[i],
           font = font[i], lty=lty[i], lwd=lwd[i], ...)
	  ## S-Plus version used type=type[i]; was type=type for points()
	}
  }
  
  lty <- rep(lty, length = ng)
  lwd <- rep(lwd, length = ng)
  pch <- rep(pch, length = ng)
  cex <- rep(cex, length = ng)
  font <- rep(font, length = ng)
  if(!length(col))
    col <- if(type == "p") plot.symbol$col else 
   plot.line$col
  col <- rep(col, length = ng)
  ## 14Apr2001 MB changes: set colors for method = "filled bands"
  if(!length(col.fill))
    col.fill <- plot.fill
  col.fill <- rep(col.fill, length = ng)       
  ## end MB

  if(ng > 1) {
    ## MB 14Apr2001: if method == "filled bands"
    ## have to plot filled bands first, otherwise lines/symbols
    ## would be hidden by the filled band
    if(method == "filled bands") {
      gfun <- ordGridFun(.R.)
      for(gg in levnum) {
        s <- g == gg
        gfun$polygon(x = c(x[s], rev(x[s])),
                     y = c(lower[s], rev(upper[s])), col =  col.fill[gg])
      }
    }  ## end MB
    pspanel(x, y, subscripts, groups, lwd = lwd, lty = 
            lty, pch = pch, cex = cex, font = font, col
            = col, type = type)
    if(type != "p" && !(is.logical(label.curves) && !
         label.curves)) {
      lc <- if(is.logical(label.curves))
        list(lwd  = lwd, cex = cex[1]) else
      c(list(lwd = lwd, cex = cex[1]), label.curves)
      curves <- vector("list", length(levnum))
      names(curves) <- levels(groups)[levnum]  # added levnum 24Oct01
      i <- 0
      for(gg in levnum) {
        i <- i + 1
        s <- g == gg
        curves[[i]] <- list(x[s], y[s])
      }
      labcurve(curves, lty = lty[levnum], lwd = lwd[levnum],
               col = col[levnum], opts = lc, grid=TRUE, ...)
    }
  }
  ## MB 14Apr2001: if method == "filled bands"
  ## plot filled bands first, otherwise lines/symbols
  ## would be hidden by the filled band
  else {
    if(method == "filled bands") {
      if(.R.) grid.polygon(x = c(x, rev(x)), y = c(lower, rev(upper)),
                           gp=gpar(fill = col.fill),
                           default.units='native') else
      polygon(x = c(x, rev(x)), y = c(lower, rev(upper)), col = col.fill)
    } ## end MB
    ppanel(x, y, lwd = lwd, lty = lty, pch = pch, cex = cex,
           font = font, col = col, type = type)
  } 
  ## 14Apr2001 MB
  ## final change for filled bands: just skip the rest
  ## if method = filled bands, remaining columns of other are ignored

  if(nother && method != "filled bands") {
    if(method == "bands") {
      dob <- function(a, def, ng, j)
        {
          if(!length(a))
            return(def)
          if(!is.list(a))
            a <- list(a)
          a <- rep(a, length = ng)
          sapply(a, function(b, j)
                 b[j], j = j)
        }
      for(j in 1:ncol(other)) {
        if(ng == 1)
          ppanel(x, other[, j], 
                 lwd = dob(lwd.bands, lwd, ng, j),
                 lty = dob(lty.bands, lty, ng, j), 
                 col = dob(col.bands, col, ng, j), 
                 pch = pch, cex = cex, font = 
                 font, type = "l")
        else pspanel(x, other[, j], 
                     subscripts, groups, 
                     lwd = dob(lwd.bands, lwd, ng, j),
                     lty = dob(lty.bands, lty, ng, j), 
                     col = dob(col.bands, col, ng, j), 
                     pch = pch, cex = cex, font = 
                     font, type = "l")
      }
    }
    else {
      errbr <- function(x, y, lower, upper, cap, 
                        lty, lwd, col, connect)
        {
          gfun    <- ordGridFun(.R.) ## see Misc.s
          segmnts <- gfun$segments
          gun     <- gfun$unit
          
          smidge <- 0.5 * cap *
            (if(.R.)unit(1,'npc') else diff(par("usr" )[1:2]))
          switch(connect,
                 all = {
                   segmnts(x, lower, x, upper,
                           lty = lty, lwd = lwd, col = col)
                   segmnts(gun(x)-smidge, lower,
                           gun(x)+smidge, lower,
                           lwd = lwd, lty = 1, col = col)
                   segmnts(gun(x)-smidge, upper,
                           gun(x)+smidge, upper,
                           lwd = lwd, lty = 1, col = col)
                 }
                 ,
                 upper = {
                   segmnts(x, y, x, upper, lty = lty, lwd = lwd, col = col)
                   segmnts(gun(x)-smidge,  upper,
                           gun(x)+smidge,  upper,
                           lwd = lwd, lty = 1, col = col)
                 }
                 ,
                 lower = {
                   segmnts(x, y, x, lower, lty = lty, lwd = lwd, col = col)
                   segmnts(gun(x)-smidge,  lower,
                           gun(x)+smidge,  lower,
                           lwd = lwd, lty = 1, col = col)
                 }
                 )
        }
      if(ng == 1)
        errbr(x, y, lower, upper, cap, 
              lty.bar, lwd, col, switch(method,
                                        bars = "all",
                                        "upper bars" = "upper",
                                        "lower bars" = "lower",
                                        "alt bars" = "lower"))
      else {
        if(method == "alt bars")
          medy <- median(y, na.rm = TRUE)
        for(gg in levnum) {
          s <- g == gg
          connect <- switch(method,
                            bars = "all",
                            "upper bars" = "upper",
                            "lower bars" = "lower",
                            "alt bars" = if(median(y[s], 
                              na.rm = TRUE) > medy) "upper"
                            else "lower")
          errbr(x[s], y[s], lower = lower[s],
                upper = upper[s], cap, lty.bar, 
                lwd[gg], col[gg], connect)
        }
      }
    }
  }
  if(length(minor.ticks)) {
    minor.at <- if(is.list(minor.ticks)) minor.ticks$at
    else minor.ticks
    minor.labs <- if(is.list(minor.ticks) &&
                     length(minor.ticks$labels)) minor.ticks$labels
    else FALSE
    gfun$axis(side = 1, at = minor.at, labels = FALSE,
              tck = par("tck") * 0.5, outer = TRUE, cex = par("cex") * 
         0.5)
    if(!is.logical(minor.labs))
      gfun$axis(side = 1, at = minor.at, labels = 
                minor.labs, tck = 0, cex = par("cex") * 0.5, line = 1.25)
  }
  if(type != "l" && ng > 1) {
	##set up for key() if points plotted
    if(.R.) {
      Key <- function(x=0, y=1, lev, cex, col, font, pch, ...) {
        oldpar <- par(usr=c(0,1,0,1),xpd=NA)
        on.exit(par(oldpar))
        if(is.list(x)) { y <- x[[2]]; x <- x[[1]] }
        ## Even though par('usr') shows 0,1,0,1 after lattice draws
        ## its plot, it still needs resetting
        if(!length(x)) x <- 0
        if(!length(y)) y <- 1  ## because of formals()
        rlegend(x, y, legend=lev, cex=cex, col=col, pch=pch)
        invisible()
      }
 } else {
   Key <- function(x=NULL, y=NULL, lev, cex, col, font, pch, ... ) {
     if(length(x)) {
       if(is.list(x)) {
         y <- x$y
         x <- x$x
       }
       key(x = x, y = y, text = list(lev, col = col),
           points = list(cex = cex, col = col, font = font,
             pch = pch), transparent = TRUE, ...)
     }
     else key(text = list(lev, col = col),
              points  = list(cex = cex, col = col,
                font = font, pch = pch), transparent =
              TRUE, ...)
     invisible()
   }
 }
    formals(Key) <- list(x=NULL,y=NULL,lev=levels(groups), cex=cex,
                         col=col, font=font, pch=pch)  #, ...=NULL)
    storeTemp(Key)
  }
  if(!missing(abline))
    do.call("panel.abline", abline)
  if(type == "l" && ng > 1) {
    ## Set up for legend (key() or rlegend()) if lines drawn
    if(.R.) {
      Key <- function(x=0, y=1, lev, cex, col, lty, lwd, ...) {
        oldpar <- par(usr=c(0,1,0,1),xpd=NA)
        on.exit(par(oldpar))
        if(is.list(x)) { y <- x[[2]]; x <- x[[1]] }
        ## Even though par('usr') shows 0,1,0,1 after lattice draws
        ## its plot, it still needs resetting
        if(!length(x)) x <- 0
        if(!length(y)) y <- 1  ## because of formals()
        rlegend(x, y, legend=lev, cex=cex, col=col, lty=lty, lwd=lwd)
        invisible()
    }
 } else {
   Key <- function(x=NULL, y=NULL, lev, col, lty, lwd, ...)
     {
       if(length(x)) {
         if(is.list(x)) {
           y <- x$y
           x <- x$x
         }
         key(x = x, y = y,
             text = list(lev, col = col),
             lines = list(col = col, lty = lty, lwd = lwd),
             transparent  = TRUE, ...)
       }
       else key(text = list(lev, col = col),
                lines = list(col = col, lty = lty, lwd = lwd),
                transparent = TRUE, ...)
       invisible()
     }
 }
    formals(Key) <- list(x=NULL,y=NULL,lev=levels(groups), col=col,
                         lty=lty, lwd=lwd)  #, ...=NULL)
    storeTemp(Key)
  }
}

xYplot <- if(.R.)
  function (formula, data=sys.frame(sys.parent()),
            groups, subset,
            xlab=NULL, ylab=NULL, ylim=NULL,
            panel=panel.xYplot, prepanel=prepanel.xYplot,
            scales=NULL, minor.ticks=NULL, ...) {

    require('grid')
  require('lattice')
  yvname <- as.character(formula[2])  # tried deparse
  y <- eval(parse(text=yvname), data)
  if(!length(ylab)) ylab <- label(y, units=TRUE, plot=TRUE,
                                  default=yvname, grid=TRUE)
#    ylab <- attr(y, 'label')  26sep02
#    if(!length(ylab)) ylab <- yvname
#  }
  if(!length(ylim)) {
    yother <- attr(y,'other')
    if(length(yother)) ylim <- range(y, yother, na.rm=TRUE)
  }

  xvname <- formula[[3]]
  if(length(xvname)>1 && as.character(xvname[[1]])=='|') 
    xvname <- xvname[[2]]  # ignore conditioning var
  xv <- eval(xvname, data)
  if(!length(xlab)) xlab <- label(xv, units=TRUE, plot=TRUE,
                                  default=as.character(xvname),
                                  grid=TRUE)
#    xlab <- attr(xv, 'label') 26sep02
#    if(!length(xlab)) xlab <- as.character(xvname)
#  }

  if(!length(scales$x)) {
    if(length(maj <- attr(xv,'scales.major'))) scales$x <- maj
  }
  if(!length(minor.ticks)) {
    if(length(minor <- attr(xv,'scales.minor'))) minor.ticks <- minor
  }

  if(!missing(groups)) groups <- eval(substitute(groups),data)
  if(!missing(subset)) subset <- eval(substitute(subset),data)

  ## Note: c(list(something), NULL) = list(something)
  ## The following was c(list(formula=formula,...,panel=panel),if()c(),...)
  ## 28aug02
  do.call("xyplot", c(list(formula=formula, data=data,
                           prepanel=prepanel, panel=panel),
                      if(length(ylab))list(ylab=ylab),
                      if(length(ylim))list(ylim=ylim),
                      if(length(xlab))list(xlab=xlab),
                      if(length(scales))list(scales=scales),
                      if(length(minor.ticks))list(minor.ticks=minor.ticks),
                      if(!missing(groups))list(groups=groups),
                      if(!missing(subset))list(subset=subset),
                      list(...)))
} else function(formula, data = sys.parent(1), 
                groups = NULL, 
                prepanel=prepanel.xYplot, panel='panel.xYplot',
                scales=NULL, ...,
                xlab=NULL, ylab=NULL,
                subset=TRUE, minor.ticks=NULL) {

  subset <- eval(substitute(subset), data)
  yvname <- deparse(formula[[2]])
  if(!length(ylab)) ylab <- label(eval(formula[[2]],data),
                                  units=TRUE, plot=TRUE, default=yvname)
#    ylab <- attr(eval(formula[[2]], data), 'label')  26sep02
#    if(!length(ylab)) ylab <- yvname
#  }
                
  xv <- formula[[3]]  ## 8Dec00
  if(length(xv)>1 && as.character(xv[[1]])=='|') 
    xv <- xv[[2]]  # ignore conditioning var
  xvname <- deparse(xv)
  xv <- eval(xv, data)
  if(!length(xlab)) xlab <- label(xv, units=TRUE, plot=TRUE, default=xvname)
#    xlab <- attr(xv, 'label') 26sep02
#    if(!length(xlab)) xlab <- xvname
#  }

  if(!length(scales$x)) {
    if(length(maj <- attr(xv,'scales.major'))) scales$x <- maj
  }
  if(!length(minor.ticks)) {
    if(length(minor <- attr(xv,'scales.minor'))) minor.ticks <- minor
  }
  
  setup.2d.trellis(formula, data = data,
				   prepanel=prepanel, panel=panel,
				   groups = eval(substitute(groups),  data), ...,
				   xlab=xlab, ylab=ylab,
                   subset = subset, scales=scales, minor.ticks=minor.ticks)
}

## Only change from default is replacement of x with oldUnclass(x)
if(!.R.)
  shingle <- function(x, intervals = sort(unique(oldUnclass(x)))) {
    if(is.vector(intervals))
      intervals <- cbind(intervals, intervals)
    dimnames(intervals) <- NULL
    attr(x, 'intervals') <- intervals
    class(x) <- 'shingle'   ## 6Aug00 to be like 5.x shingle
    x
  }

prepanel.Dotplot <- function(x, y, ...) {
  xlim <- range(x, attr(x,'other'), na.rm=TRUE)
  ylim <- range(as.numeric(y), na.rm=TRUE)  ## as.numeric 25nov02
  list(xlim=xlim, ylim=ylim) #, dx=diff(xlim), dy=diff(ylim))
}

 panel.Dotplot <- function(x, y, groups = NULL,
                           pch  = dot.symbol$pch, 
                           col  = dot.symbol$col, cex = dot.symbol$cex, 
                           font = dot.symbol$font, abline, ...){
   gfun <- ordGridFun(.R.) ## see Misc.s
   segmnts <- gfun$segments
   y <- as.numeric(y)      ## 7dec02

   gp <- length(groups)
   dot.symbol <- trellis.par.get(if(gp)'superpose.symbol' else 'dot.symbol')
   dot.line   <- trellis.par.get('dot.line')
   plot.line  <- trellis.par.get(if(gp)'superpose.line' else 'plot.line')

   gfun$abline(h = unique(y), lwd=dot.line$lwd, lty=dot.line$lty, 
               col=dot.line$col)
   if(!missing(abline))
     do.call("panel.abline", abline)

   other <- attr(x,'other')
   x <- oldUnclass(x)
   attr(x,'other') <- NULL
   if(length(other)) {
     nc <- ncol(other)
     segmnts(other[,1], y, other[,nc], y, lwd=plot.line$lwd[1],
              lty=plot.line$lty[1], col=plot.line$col[1])
     if(nc==4) {
       segmnts(other[,2], y, other[,3], y, lwd=2*plot.line$lwd[1],
                lty=plot.line$lty[1], col=plot.line$col[1])
       gfun$points(other[,2], y, pch=3, cex=cex, col=col, font=font)
       gfun$points(other[,3], y, pch=3, cex=cex, col=col, font=font)
     }
     ## as.numeric( ) 1 and 6 lines below 23Apr02
     if(gp) panel.superpose(x, y, groups=as.numeric(groups), pch=pch,
                            col=col, cex=cex, font=font, ...) else
     gfun$points(x, y, pch=pch[1], cex=cex, col=col, font=font)
   } else {
     if(gp) 
       panel.superpose(x, y, groups=as.numeric(groups),
                       pch=pch, col=col, cex=cex,
                       font=font, ...) else
     panel.dotplot(x, y, pch=pch, col=col, cex=cex, font=font, ...)
   }
 if(gp) {
     if(.R.) Key <- function(x=0, y=1, lev, cex, col, font, pch, ...) {
       oldpar <- par(usr=c(0,1,0,1),xpd=NA)
       on.exit(par(oldpar))
       if(is.list(x)) { y <- x[[2]]; x <- x[[1]] }
       ## Even though par('usr') shows 0,1,0,1 after lattice draws
       ## its plot, it still needs resetting
       if(!length(x)) x <- 0
       if(!length(y)) y <- 1  ## because of formals()
       rlegend(x, y, legend=lev, cex=cex, col=col, pch=pch)
       invisible()
     } else Key <- function(x=NULL, y=NULL, lev, cex, col, font, pch) { #, ...)
       if(length(x)) {
         if(is.list(x)) {y <- x$y; x <- x$x}
         key(x=x, y=y, text=list(lev, col=col), 
             points=list(cex=cex,col=col,font=font,pch=pch),
             transparent=TRUE)  #, ...)
       } else key(text=list(lev, col=col), 
                  points=list(cex=cex,col=col,font=font,pch=pch),
                  transparent=TRUE)  #, ...)
       invisible()
     }
     lev <- levels(as.factor(groups))
     ng <- length(lev)
     formals(Key) <- list(x=NULL,y=NULL,lev=lev,
                          cex=cex[1:ng], col=col[1:ng],
                          font=font[1:ng], pch=pch[1:ng])   #,...=NULL)
     storeTemp(Key)
   }
 }

 Dotplot <-
  if(.R.) function (formula, data=sys.frame(sys.parent()),
                    groups, subset,
                    xlab=NULL, ylab=NULL, ylim=NULL,
                    panel=panel.Dotplot, prepanel=prepanel.Dotplot,
                    scales=NULL, ...) {

    require('grid')
  require('lattice')
  yvname <- as.character(formula[2])  # tried deparse
  yv <- eval(parse(text=yvname), data)
  if(!length(ylab)) ylab <- label(yv, units=TRUE, plot=TRUE,
                                  default=yvname, grid=TRUE)
#    ylab <- attr(yv, 'label') 26sep02
#    if(!length(ylab)) ylab <- yvname
#  }
  if(!length(ylim)) {
    yother <- attr(yv,'other')
    if(length(yother)) ylim <- range(yv, yother, na.rm=TRUE)
  }
  if(is.character(yv)) yv <- factor(yv)
  if(!length(scales) && is.factor(yv))
    scales <- list(y=list(at=1:length(levels(yv)),labels=levels(yv)))
  
  xvname <- formula[[3]]
  if(length(xvname)>1 && as.character(xvname[[1]])=='|') 
    xvname <- xvname[[2]]  # ignore conditioning var
  xv <- eval(xvname, data)
  if(!length(xlab)) xlab <- label(xv, units=TRUE, plot=TRUE,
                                  default=as.character(xvname), grid=TRUE)
#    xlab <- attr(xv, 'label')  26sep02
#    if(!length(xlab)) xlab <- as.character(xvname)
#  }

  if(!missing(groups)) groups <- eval(substitute(groups),data)
  if(!missing(subset)) subset <- eval(substitute(subset),data)

  dul <- options(drop.unused.levels=FALSE)   ## 25nov02, for empty cells
  on.exit(options(dul))                      ## across some panels
  
  do.call("xyplot", c(list(formula=formula, data=data,
                           prepanel=prepanel, panel=panel),
                      if(length(ylab))list(ylab=ylab),  ## was c(ylab=)
                      if(length(ylim))list(ylim=ylim),  ## 28aug02
                      if(length(xlab))list(xlab=xlab),
                      if(!missing(groups))list(groups=groups),
                      if(!missing(subset))list(subset=subset),
                      if(length(scales))list(scales=scales),
                       list(...)))
} else function(formula, data = sys.parent(1), 
                prepanel=prepanel.Dotplot, panel = 'panel.Dotplot', 
                xlab = NULL, scales = NULL, ylim = NULL, groups = NULL, 
                ..., subset = TRUE) {
	sub.formula <- substitute(formula)
	formula <- eval(sub.formula, data)
	if(missing(xlab)) {
	  xv <- formula[[3]]
	  if(length(xv)>1 && as.character(xv[[1]])=='|') 
		xv <- xv[[2]]  # ignore conditioning var
#	  xlab <- attr(eval(xv, data), 'label') 26sep02
      xlab <- label(eval(xv,data), units=TRUE, plot=TRUE,
                    default=if(is.numeric(formula))
                     deparse(sub.formula) else '') 
	}
#	if(is.null(xlab) && is.numeric(formula))  26sep02
#		xlab <- deparse(sub.formula)
	subset <- eval(substitute(subset), data)
	groups <- eval(substitute(groups), data)

    dul <- options(drop.unused.levels=FALSE)   ## 25nov02, for empty cells
    on.exit(options(dul))

	data <- setup.1d.trellis(formula, data = data, panel=panel,
							 prepanel = prepanel, 
							 xlab = xlab, 
							 groups = groups, ..., subset = subset)
	if(!is.null(scales))
		data$scales <- add.scale.trellis(scales, data$scales)
	if(is.null(scale$y$limits) && is.null(ylim))
		data$scales$y$limits <- data$ylim + c(-0.75, 0.75)
	data
}


setTrellis <- function(strip.blank=TRUE, lty.dot.line=2, lwd.dot.line=1) {
  if(strip.blank) trellis.strip.blank()   # in Hmisc Misc.s
  dot.line <- trellis.par.get('dot.line')
  dot.line$lwd <- lwd.dot.line
  dot.line$lty <- lty.dot.line
  trellis.par.set('dot.line',dot.line)
  invisible()
}

numericScale <- function(x, label=NULL, skip.weekends= FALSE, ...) {
  td <- inherits(x,'timeDate')
  if(td) {
    u <- axis.time(range(x,na.rm=TRUE),
                   skip.weekends=skip.weekends, ...)$grid
    major  <- list(at=as.numeric(u$major.grid$x),
                   labels=format(u$major.grid$x))
    minor  <- list(at=as.numeric(u$minor$x),
                   labels=format(u$minor$x))
  }
  xn <- as.numeric(x)

  attr(xn,'label') <- if(length(label)) label else
    deparse(substitute(x))

  if(td) {
    attr(xn,'scales.major') <- major
    attr(xn,'scales.minor') <- minor
  }
  xn
}

## See proc.scale.trellis, render.trellis, axis.trellis for details of
## how scale is used
# Author: Frank Harrell 24 Jun 91
xy.group <- function(x,y,m=150,g,fun=mean,result="list")	{

k <- !is.na(x+y)
if(sum(k)<2)stop("fewer than 2 non-missing x and y")
x <- x[k]
y <- y[k]
if(missing(m)) q <- cut2(x,g=g,levels.mean=TRUE,digits=7) else
	q <- cut2(x,m=m,levels.mean=TRUE,digits=7)
n <- table(q)
x.mean <- as.single(levels(q))
y.fun <- as.vector(tapply(y, q, fun))
if(result=="matrix")	{
	z <- cbind(table(q),x.mean,y.fun)
	dimnames(z) <- list(levels(q), c("n","x","y"))
			}	else
	z <- list(x=x.mean,y=y.fun)
z								}
#Function to use the mouse to zoom in on plots.
#Author: Bill Dunlap <bill@STAT.WASHINGTON.EDU>
zoom<-function(fun=usa,...)	{
	on.exit(par(oldpar))
	oldpar<-par(err=-1)
	fun(...)
	while(TRUE) {
		cat("Click mouse over corners of zoom area: ")
		p<-locator(n=2)
		if(is.null(p$x) || length(p$x)!=2) break
		xlim<-range(p$x)
		ylim<-range(p$y)
		cat("xlim=",xlim,"ylim=",ylim,"\n")
		fun(...,xlim=xlim,ylim=ylim)
		}
	cat("Bye!\n")
}
