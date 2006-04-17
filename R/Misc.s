## $Id: Misc.s,v 1.18 2006/04/14 16:33:17 dupontct Exp $
		
if(!exists("NROW", mode='function')) {
  NROW <- function(x)
    if (is.array(x) || is.data.frame(x)) nrow(x) else length(x)
}

if(!exists("NCOL", mode='function')) {
  NCOL <- function(x)
    if (is.array(x) && length(dim(x)) > 1 || is.data.frame(x)) ncol(x) else as.integer(1)
}

prn <- function(x, txt)
{
  calltext <- as.character(sys.call())[2]

  if(!missing(txt)) {
    if(nchar(txt) + nchar(calltext) +3 > .Options$width)
      calltext <- paste('\n\n  ',calltext,sep='')
    else
      txt <- paste(txt, '   ', sep='')
    cat('\n', txt, calltext, '\n\n', sep='') 
  }
  else cat('\n',calltext,'\n\n',sep='')
  invisible(print(x))
}

format.sep <- function(x, digits, ...)
{
  y <- character(length(x))
  for(i in 1:length(x))
    y[i] <- if(missing(digits)) format(x[i], ...)
            else format(x[i],digits=digits, ...)  ## 17Apr02

  names(y) <- names(x)  ## 17Apr02
  y
}

nomiss <- function(x)
{
  if(is.data.frame(x)) na.exclude(x)
  else if(is.matrix(x))
    x[!is.na(x %*% rep(1,ncol(x))),]
  else x[!is.na(x)]
}

fillin <- function(v, p)
{
  v.f <- ifelse(is.na(v),p,v)
  if(length(p)==1)
    label(v.f) <- paste(label(v),"with",sum(is.na(v)),
                        "NAs replaced with",format(p))
  else
    label(v.f) <- paste(label(v),"with",sum(is.na(v)),"NAs replaced")
  v.f
}

spearman <- function(x, y)
{
  x <- as.numeric(x)
  y <- as.numeric(y)  ## 17Jul97
  
  notna <- !is.na(x+y)	##exclude NAs
  if(sum(notna) < 3)
    c(rho=NA)
  else
    c(rho=cor(rank(x[notna]), rank(y[notna])))
}

plotCorrPrecision <- function(rho=c(0,0.5), n=seq(10,400,length=100),
                              conf.int=0.95)
{
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
    if(.R.)
      assign(paste(prefix,nn[i],suffix,sep=""), x[,i], pos=1)
    else
      assign(paste(prefix,nn[i],suffix,sep=""), x[,i], where=1)
  invisible()
}

## Function to pick off ordinates of a step-function at user-chosen abscissas

stepfun.eval <- function(x, y, xout, type=c("left","right"))
{
  s <- !is.na(x+y)
  type <- match.arg(type)
  approx(x[s], y[s], xout=xout, method="constant", f=if(type=="left")0 else 1)$y
}

km.quick <- function(S, times, q)
{
  if(.R. && !existsFunction('survfit.km'))
    survfit.km <- getFromNamespace('survfit.km','survival')

  S <- S[!is.na(S),]
  n <- nrow(S)
  stratvar <- factor(rep(1,nrow(S)))
  f <- survfit.km(stratvar, S, se.fit=FALSE, conf.type='none')
  tt <- c(0, f$time)
  ss <- c(1, f$surv)
  if(missing(times))
    min(tt[ss <= q])
  else
    approx(tt, ss, xout=times, method='constant', f=0)$y
}

oPar <- function()
{
  ## Saves existing state of par() and makes changes suitable
  ## for restoring at the end of a high-level graphics functions
  oldpar <- par()
  oldpar$fin <- NULL
  oldpar$new <- FALSE
  invisible(oldpar)
}

setParNro <- function(pars)
{
  ## Sets non-read-only par parameters from the input list
  i <- names(pars) %nin%
    c('cin','cra','csi','cxy','din','xlog','ylog','gamma')
  invisible(par(pars[i]))
}

mgp.axis.labels <- function(value,type=c('xy','x','y','x and y'))
{
  type <- match.arg(type)
  if(missing(value)) {
    value <- .Options$mgp.axis.labels
    pr <- par(c('mgp','las'))
    mgp <- pr$mgp
    if(!length(value))
      value <- c(.7, .7)
    ##value <- c(mgp[2], if(pr$las==1) max(mgp[2],1.3) else mgp[2])
    return(switch(type, 
                  xy = value, 
                  x = c(mgp[1], value[1], mgp[3]),
                  y = c(mgp[1], value[2], mgp[3]),
                  'x and y' = list(x = c(mgp[1], value[1], mgp[3]),
                                   y = c(mgp[1], value[2], mgp[3]))))
  }
  
  if(value[1]=='default')
    value <- c(.7,.7)
  
  ##c(.6, if(par('las')==1) 1.3 else .6)
  options(mgp.axis.labels=value, TEMPORARY=FALSE)
  invisible()
}

mgp.axis <-
  function(side, at=NULL, ...,
           mgp=mgp.axis.labels(type=if(side==1 | side==3)'x' else 'y'),
           axistitle=NULL)
{
  ## Version of axis() that uses appropriate mgp from mgp.axis.labels and
  ## gets around bug in axis(2, ...) that causes it to assume las=1
  mfrow <- par('mfrow')          ## mfrow, tcl logic 28jan03
  nr <- mfrow[1]; nc <- mfrow[2]
  w <- list(side=side)
  w <- c(w, list(...))   ## 21apr03
  if(length(at))
    w$at <- at
  if(side==1 || side==3) {
    w$mgp <- mgp/nr
    if(.R.)
      w$tcl <- -0.4/nr
    if(side==1 && length(axistitle))
      title(xlab=axistitle, mgp=mgp/min(2.25,nr))
  } else {
    w$mgp <- mgp/nc
    if(.R.)
      w$tcl <- -0.4/nc
    las <- par('las')
    w$srt <- 90*(las==0)
    w$adj <- if(las==0)0.5
             else 1
    if(side==2 && length(axistitle))
      title(ylab=axistitle, mgp=mgp/min(2.25,nc))
  }
  do.call('axis', w)
  invisible()
}

trellis.strip.blank <- function()
{
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
                           intercept=TRUE, xpxi=FALSE)
{
  if(intercept)
    x <- cbind(1,x)
  if(storage.mode(x) != "double")
    storage.mode(x) <- "double"
  if(storage.mode(y) != "double")
    storage.mode(y) <- "double"
  
  dx <- dim(x)
  dn <- dimnames(x)
  qty <- y
  n <- dx[1]
  n1 <- 1:n
  p <- dx[2]
  p1 <- 1:p
  dy <- c(n, 1)
  z <- if(!.R.)
         .Fortran("dqrls",
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
                  rank = as.integer(p))
       else
         .Fortran("dqrls", qr = x, n = as.integer(n), p = as.integer(p),
                  y = y, ny = as.integer(1),
                  tol = as.double(tolerance), coef = double(p),
                  residuals = y, effects = y, rank = integer(1),
                  pivot = as.integer(p1),
                  qraux = double(p), work = double(2 * p), PACKAGE = "base")

  coef <- z$coef
  if(length(dn[[2]]))
    names(coef) <- dn[[2]]
  
  res <- z$residuals
  sse <- sum(res^2)
  sst <- sum((y-mean(y))^2)

  res <- list(coefficients=coef, residuals=res, 
              rsquared=1-sse/sst, fitted.values=y-res)
  if(xpxi) {
    if(.R.)
      xpxi <- chol2inv(z$qr)
    else {
      R <- (z$qr)[p1, , drop = FALSE]
      R[lower.tri(R)] <- 0
      rinv <- solve(R, diag(length(coef)))
      xpxi <- rinv %*% t(rinv)
    }
    res$xpxi <- xpxi
  }
  res
}

all.is.numeric <- function(x, what=c('test','vector'),
                           extras=c('.','NA'))
{
  what <- match.arg(what)
  old <- options(warn=-1)
  on.exit(options(old))
  ##.Options$warn <- -1  6Aug00
  x <- sub('[[:space:]]+$', '', x)
  x <- sub('^[[:space:]]+', '', x)
  xs <- x[x %nin% c('',extras)]
  isnum <- !any(is.na(as.numeric(xs)))
  if(what=='test')
    isnum
  else if(isnum)
    as.numeric(x)
  else x
}

Lag <- function(x, shift=1)
{
  ## Lags vector x shift observations, padding with NAs or blank strings
  ## on the left, preserving attributes of x

  # check to see if shift == 0
  if(shift == 0)
    return(x)

  # Create base vector use character to generate "" for mode "character"
  # Coerce base vector to be type of x
  xLen <- length(x)
  ret <- as.vector(character(xLen), mode=storage.mode(x))
  
  # set resp attributes equal to x attributes
  attrib <- attributes(x)

  if(!is.null(attrib$label))
    atr$label <- paste(attrib$label, 'lagged', shift, 'observations')

  if(xLen > shift){
    retrange = 1:shift
    ret[-retrange] <- x[1:(xLen - shift)]
  }
  
  attributes(ret) <- attrib
  return(ret)
}

xySortNoDupNoNA <- function(x, y)
{
  if(is.list(x)) {
    y <- x[[2]]; x <- x[[1]]
  }
  
  s <- !is.na(x + y)
  if(any(s)) {
    x <- x[s]; y <- y[s]
  }
  
  i <- order(x)
  x <- x[i]
  y <- y[i]
  i <- !duplicated(x)
  list(x=x[i], y=y[i])
}

## Lifted from rowsum in 4.5
rowsumFast <- function(x, group, reorder=FALSE)
{
  ## assumes x is a matrix
  ## by default, results are in order that unique group values
  ## encountered
  ## is fast and solves error that reorder= omitted from S+ 2000
  
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
                     x=x, as.double(group), PACKAGE='base')
          else
            .C(
               if(under.unix || version$major < 4 ||
                  (version$major == 4 && version$minor < 7))
                 "rowsum"
               else "S_rowsum",
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
                      adj=1, cex=par('cex'))
{
  ## Use text() to put test strings in left or right margins
  ## Temporarily sets par(xpd=NA) if using R
  ## For adj=1 side=4, setAside is a character string used to determine
  ## the space to set aside for all strings
  ## space is the number of extra characters to leave to the left of
  ## the string(s) (adj=0) or to the right (adj=1)
  
  usr <- par('usr')
  xpd <- par('xpd')
  if(.R. && !is.na(xpd)) {
    on.exit(par(xpd=xpd))
    par(xpd=NA)
  }
  
  ie <- is.expression(string)  ## 1sep02
  if(ie)
    adj <- 0  ## adj=1 not work well for expressions in R
  
  if(side!=4)
    stop('only side=4 implemented')
  space <- substring('                    ',1,space)
  if(adj==0)
    text(usr[2], y,
         if(ie)
           string
         else
           paste(space,string,sep=''),
         adj=0)
  else {
    usr.space.needed <- strwidth(setAside, units='user', cex=cex)
    text(usr[2]+0.5*strwidth(space, units='user', cex=cex)+usr.space.needed,
         y, string, adj=1, cex=cex) # was usr[2]- 18jul02;added 0* 25jul02
    ## was 0*strwidth(space,...) 31jan03
  }
  invisible()
}

if(FALSE) {
  expandUsrCoord <- function()
  {
    ## Expands usr coordinates of current plot to entire figure region
    ## so that out of range plots may be plotted
    pr <- par()
    usr <- pr$usr
    p <- pr$plt
    invisible(pr)
  }
}

if(!.R.)
  strwidth <- function(string, units=c('user','figure','inches'),
                       cex=pr$cex)
{
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
                        cex=pr$cex)
{
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
  if(length(ndimn))
    cnam[1] <- ndimn[1]  ## FEH
  ##dimnames(x)[[1]] <- seq(nrow(x))  25Mar02 for R  FEH
  dimnames(x) <- list(as.character(seq(nrow(x))), cnam)
  names(dimnames(x)) <- ndimn  ## 26Mar02 FEH
###  Set up some padding functions:
###
  pad.left <- function(z, pads)
  {
    ## Pads spaces to left of text
    padding <- paste(rep(" ", pads), collapse = "")
    paste(padding, z, sep = "")
  }
  
  pad.mid <- function(z, pads)
  {
    ## Centres text in available space
    padding.right <- paste(rep(" ", pads%/%2), collapse = "")
    padding.left <- paste(rep(" ", pads - pads%/%2), collapse = "")
    paste(padding.left, z, padding.right, sep = "")
  }
  
  pad.right <- function(z, pads) {
    ## Pads spaces to right of text
    padding <- paste(rep(" ", pads), collapse = "")
    paste(z, padding, sep = "")
  }
  
  ##  (Padding happens on the opposite side to alignment)
  pad.types <- c("left", "mid", "right")
  names(pad.types) <- c("right", "cen", "left")
  pad.name <- pad.types[col.name.align]
  pad.txt <- pad.types[col.txt.align]
  pad.cell <- pad.types[cell.align]
  
  ## Padding character columns
  ##    Need columns with uniform number of characters
  pad.char.col.right <- function(y)
  {
    ## For aligning text to LHS of column
    col.width <- nchar(y)
    biggest <- max(col.width)
    smallest <- min(col.width)
    padding <- biggest - col.width
    out <- NULL
    for (i in seq(y))
      out[i] <- pad.right(y[i], pads = padding[i])
    out
  }
  
  pad.char.col.left <- function(y)
  {
    ## For aligning text to RHS of column
    col.width <- nchar(y)
    biggest <- max(col.width)
    smallest <- min(col.width)
    padding <- biggest - col.width
    out <- NULL
    for (i in seq(y))
      out[i] <- pad.left(y[i], pads = padding[i])
    out
  }
  
  pad.char.col.mid <- function(y) {
    ## For aligning text to centre of column
    col.width <- nchar(y)
    biggest <- max(col.width)
    smallest <- min(col.width)
    padding <- biggest - col.width
    out <- NULL
    for (i in seq(y))
      out[i] <- pad.mid(y[i], pads = padding[i])
    out
  }
  
  ## which functions to use this time.
  pad.name.fn <- get(paste("pad.", pad.name, sep = ""))
  pad.txt.fn <- get(paste("pad.char.col.", pad.txt, sep = ""))
  pad.cell.fn <- get(paste("pad.", pad.cell, sep = ""))
  
  ## Remove troublesome factors
  x <- as.data.frame(x)
  fac.col <- names(x)[sapply(x, is.factor)]
  for (i in fac.col)
    x[, i] <- I(as.character(x[, i]))
  ## ARE ANY LINE BREAKS IN ANY COLUMNS?
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
    ## add in extra row/s
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
                                                    max)),
                           unlist(reprow), sep = ".")
    
    ## Make an index for the rows to be printed
    rn <- row.names(xx)
    rnb <- strsplit(rn, "\\.")
    rpref <- as.numeric(factor(sapply(rnb, function(z) z[1])))
    ## was codes( ) 10oct03
  }
  else
    rpref <- seq(nrow(x))
  x <- as.data.frame(xx)
  
  ## Character columns need different treatment from numeric columns
  char.cols <- sapply(x, is.character)
  if (any(char.cols)) 
    x[char.cols] <- sapply(x[char.cols], pad.txt.fn)
  
  ## Change numeric columns into character
  if (any(!char.cols)) 
    x[!char.cols] <- sapply(x[!char.cols], format)
  
  ## now all character columns each of which is uniform element width
  ##
  ## Lining up names with their columns
  ## Sometimes the names of columns are wider than the columns they name, 
  ##  sometimes vice versa.

  names.width <- nchar(names(x))
  if (!col.names) 
    names.width <- rep(0, length(names.width))
  cell.width <- sapply(x, function(y) max(nchar(as.character(y))))

  ## (the width of the characters in the cells as distinct
  ##  from their names)  
  name.pads <- cell.width - names.width
  cell.pads <- -name.pads
  name.pads[name.pads < 0] <- 0
  cell.pads[cell.pads < 0] <- 0
  pad.names <- name.pads > 0
  pad.cells <- cell.pads > 0
  
  ## Pad out the column names if necessary:
  if (any(pad.names)) {
    stretch.names <- names(x)[pad.names]
    for (i in stretch.names) {
      names(x)[names(x) == i] <- pad.name.fn(i, name.pads[i])
    }
  }
  
  ## likewise for the cells and columns
  if (any(pad.cells)) {
    stretch.cells <- names(x)[pad.cells]
    for (j in stretch.cells) x[, j] <- pad.cell.fn(x[, j], 
                                                   cell.pads[j])
  }
  
  ## Remove row names if not required
  if (!row.names) 
    x <- x[-1]
  ## Put the column names on top of matrix
  if (col.names) 
    mat2 <- rbind(names(x), as.matrix(x))
  else
    mat2 <- as.matrix(x)
  
  mat.names.width <- nchar(mat2[1, ])
  ## character string to separate rows
  space.h <- ""
  for (k in seq(along=mat.names.width)) {  ## added along= FEH 26Mar02
    space.h <- c(space.h, rep(vsep, mat.names.width[k]), csep)
  }
  
  line.sep <- paste(c(ifelse(left.border, csep, ""), space.h), 
                    collapse = "")
  if (col.names) 
    rpref <- c(0, rpref, 0)
  else
    rpref <- c(rpref, 0)
  
  ## print to screen or file
  if (top.border) {
    write(line.sep, file = file, append = append)
    append <- TRUE
  }
  for (i in 1:nrow(mat2)) {
    if (left.border) 
      write(paste(paste(c("", mat2[i, ]), collapse = hsep), 
                  hsep, sep = ""), file = file, append = append)
    else
      write(paste(paste(mat2[i, ], collapse = hsep), hsep, 
                  sep = ""), file = file, append = append)
    append <- TRUE

    ## print separator if row prefix is not same as next one
    if (rpref[i] != rpref[i + 1]) 
      write(line.sep, file = file, append = TRUE)
  }
}

unPaste <- if(.R.) function(str, sep='/', extended=FALSE)
{
  w <- strsplit(str, sep, extended=extended)
  w <- matrix(unlist(w), ncol=length(str))
  nr <- nrow(w)
  ans <- vector('list', nr)
  for(j in 1:nr)
    ans[[j]] <- w[j,]
  ans
} else function(...) unpaste(...)

get2rowHeads <- if(.R.) function(str)
{
  w <- strsplit(str, '\n')
  ## strsplit returns character(0) when element=""  23may03
  list(sapply(w, function(x)if(length(x))    x[[1]] else ''),
       sapply(w, function(x)if(length(x) > 1)x[[2]] else ''))
} else function(str)
{
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
                   if(.R.)parent.frame()
                   else sys.parent())
    }
    x[r, vars, drop = FALSE]
  }
  NULL
}

## Note: can't say f[vector of names] <- list(...) to update args
## In R you have to put ALL arguments in list(...) so sometimes we set
## unneeded ones to NULL.  Ignore this assignment in S
if(!.R.) {
  'formals<-' <- function(f, value)
  {
    nv <- names(value)
    if(any(nv %nin% names(f)))
      stop(paste('function does not have arguments',
                 paste(nv[nv %nin% names(f)],collapse=' '),
                 'to update'))
    
    for(a in nv) {
      v <- value[[a]]
      if(length(v))
        f[[a]] <- v
    }
    
    f
  }
  NULL
}

## Two lists of functions, one for primitives for S+ or R (either Trellis
## or low-level), one for R grid
## Note: rect is only defined in R, not S+
ordGridFun <- function(grid)
{
  if(!grid)
    list(lines    = function(...) lines(...),
         points   = function(..., size=NULL)
                    {
                      if(length(size))
                        warning('size not implemented yet')
                      points(...)
                    },
         text     = function(...) text(...),
         segments = function(...) segments(...),
         arrows   = if(.R.)
                      function(..., open, size)
                        arrows(..., length=size*.8)
                    else
                      function(...) arrows(...),
         rect     = function(...) rect(...),
         polygon  = function(...) polygon(...),
         abline   = function(...) abline(...),
         unit     = function(x, units='native')
                    {
                      if(units!='native')
                        stop('units="native" is only units implemented outside of grid')
                      x
                    },
         axis     = function(...) axis(...))
  else {
    require('grid') || stop('grid package not available')
    list(lines = function(x, y, ...)
         {
           if(is.list(x)) {
             y <- x[[2]]; x <- x[[1]]
           }
           llines(if(is.unit(x))
                    convertX(x, 'native', valueOnly=TRUE)
                  else x,
                  if(is.unit(y))
                    convertY(y, 'native', valueOnly=TRUE)
                  else y,
                  ...)
         },

         points = function(x, y, ...)
         {
           if(is.list(x)) {
             y <- x[[2]]; x <- x[[1]]
           }
           lpoints(if(is.unit(x))
                     convertX(x, 'native', valueOnly=TRUE)
                   else x,
                   if(is.unit(y))
                   convertY(y, 'native', valueOnly=TRUE)
                   else y,
                   ...)
         },

         text = function(x, y, ...)
         {
           if(is.list(x)) {
             y <- x[[2]]; x <- x[[1]]
           }
           ltext(if(is.unit(x))
                   convertX(x, 'native', valueOnly=TRUE)
                 else x,
                 if(is.unit(y))
                   convertY(y, 'native', valueOnly=TRUE)
                 else y,
                 ...)
         },

         segments = function(x0, y0, x1, y1, ...)
         {
           grid.segments(x0, y0, x1, y1, default.units='native',
                         gp=gpar(...))
         },
       
         arrows = function(...) larrows(...),

         rect = function(xleft, ybottom, xright, ytop, density, angle,
                         border, xpd, ...)
         {
           grid.rect(xleft, ybottom, width=xright-xleft,
                     height=ytop-ybottom, just='left',
                     default.units='native', gp=gpar(...))
         },
         polygon = function(x, y, col=par('col'), ...)
         grid.polygon(x, y, default.units='native', gp=gpar(fill=col,...)),
         abline=function(...) panel.abline(...),
         unit = function(x, units='native', ...) unit(x, units=units, ...),
       
         axis = function(side=1, at=NULL, labels, ticks=TRUE,
                         distn, line, pos, outer, ...)
         {
           if(!length(at))stop('not implemented for at= unspecified')
           if(side > 2) stop('not implemented for side=3 or 4')
           if(side==1) grid.xaxis(at=at, label=labels, ticks=ticks, gp=gpar(...))
           if(side==2) grid.yaxis(at=at, label=labels, ticks=ticks, gp=gpar(...))
         })
  }
}

parGrid <- function(grid=FALSE)
{
  pr <- par()
  cin <- pr$cin
  cex <- pr$cex
  lwd <- pr$lwd
  if(grid) {
    require('grid') || stop('grid package not available')
    ## cvp <- current.viewport()
    ## usr <- c(cvp$xscale, cvp$yscale)
    usr <- c(convertX(unit(0:1, "npc"), "native", valueOnly=TRUE),
             convertY(unit(0:1, "npc"), "native", valueOnly=TRUE))

    pin <- 
      c(convertWidth(unit(1, "npc"), "inches", valueOnly=TRUE),
        convertHeight(unit(1, "npc"), "inches", valueOnly=TRUE))

    uin <- 
      c(convertWidth(unit(1, "native"), "inches", valueOnly=TRUE),
        convertHeight(unit(1, "native"), "inches", valueOnly=TRUE))
    
  }
  else {
    usr <- pr$usr
    pin <- pr$pin
    uin <- c(pin[1]/(usr[2]-usr[1]), pin[2]/(usr[4]-usr[3]))
    ## 22Mar01 - R does not have par(uin)
  }
  list(usr=usr, pin=pin, uin=uin, cin=cin, cex=cex, lwd=lwd)
}

## Replaces R's xinch, yinch, extending them to grid
## Defines these for S-Plus
## These convert inches to data units
xInch <- function(x=1, warn.log=!grid, grid=FALSE)
{
  if (warn.log && par("xlog"))
    warning("x log scale:  xInch() is nonsense")
  pr <- parGrid(grid)
  x * diff(pr$usr[1:2])/pr$pin[1]
}

yInch <- function (y = 1, warn.log=!grid, grid=FALSE)
{
  if (warn.log && par("ylog"))
    warning("y log scale:  yInch is nonsense")
  pr <- parGrid(grid)
  y * diff(pr$usr[3:4])/pr$pin[2]
}

if(.R.) {
  na.include <- function(obj)
  {
    if(inherits(obj,'data.frame'))
      for(i in seq(along=obj))
        obj[[i]] <- na.include(obj[[i]])
    else {
      if(length(levels(obj)) && any(is.na(obj)))
        obj <- factor(obj,exclude=NULL)
    }
    obj
  }
  NULL
}

if(FALSE) {
  whichClosest <- function(x, w)
  {
    ## x: vector of reference values
    ## w: vector of values to find closest matches in x
    ## Returns: subscripts in x corresponding to w
    i <- order(x)
    x <- x[i]
    n <- length(x)
    br <- c(-1e30, x[-n]+diff(x)/2,1e30)
    m <- length(w)
    if(.R.)
      i[.C("bincode", as.double(w), m, as.double(br),
           length(br), code = integer(m), right = TRUE, 
           include = FALSE, NAOK = TRUE, DUP = FALSE, 
           PACKAGE = "base")$code]
    else
      if(.SV4.)
        i[.C("S_binning3", x=as.double(w), m, as.double(br),
             length(br), 0, 0, TRUE, TRUE)$x]
      else
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
whichClosest <- function(x, w)
{
  ## x: vector of reference values
  ## w: vector of values for which to lookup closest matches in x
  ## Returns: subscripts in x corresponding to w
  ## Assumes no NAs in x or w
  if(.R.)
    .Fortran("wclosest",as.double(w),as.double(x),
             length(w),length(x),
             j=integer(length(w)),PACKAGE="Hmisc")$j
  else
    .Fortran("wclosest",as.double(w),as.double(x),length(w),length(x),
             j=integer(length(w)))$j
}

whichClosePW <- function(x, w, f=0.2) {
  lx <- length(x)
  lw <- length(w)
  if(.R.)
    .Fortran("wclosepw",as.double(w),as.double(x),
             as.double(runif(lw)),as.double(f),
             lw, lx, double(lx), j=integer(lw),
             PACKAGE="Hmisc")$j
  else
    .Fortran("wclosepw",as.double(w),as.double(x),
             as.double(runif(lw)),as.double(f),
             lw, lx, double(lx), j=integer(lw))$j
}              

if(FALSE) {
  sampWtdDist <- function(x, w)
  {
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
                         f=0, ties='ordered', na.rm=FALSE)
{
  ## Linear interpolation using approx, with linear extrapolation
  ## beyond the data
  if(is.list(x)) {
    y <- x[[2]]; x <- x[[1]]
  }

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
  
  w <- if(.R.)
         approx(x, y, xout=xout, method=method, n=n,
                rule=2, f=f, ties=ties)$y
       else
         approx(x, y, xout=xout, method=method, n=n, rule=2, f=f)$y
  
  r <- range(x)
  d <- xout < r[1]
  if(any(is.na(d)))
    stop('NAs not allowed in xout')
  
  if(any(d))
    w[d] <- (y[2]-y[1])/(x[2]-x[1])*(xout[d]-x[1])+y[1]
  
  d <- xout > r[2]
  n <- length(y)
  if(any(d))
    w[d] <- (y[n]-y[n-1])/(x[n]-x[n-1])*(xout[d]-x[n-1])+y[n-1]
  
  list(x=xout, y=w)
}

if(!existsFunction('reorder.factor'))
  reorder.factor <- function(x, v, FUN = mean, ...)
    ordered(x, levels(x)[order(tapply(v, x, FUN, ...))])

Names2names <- function(x)
{
  if(is.list(x)) {
  }
  else {
    n <- names(attributes(x))
    if(any(n=='.Names'))
      names(attributes(x)) <- ifelse(n=='.Names','names',n)
  }
  x
}

## Use R function for S-Plus, just changed to .Options
if(!.R.) {
  format.pval <- function (pv, digits = max(1, .Options$digits - 2),
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
      else sep <- if(digits == 1) 
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
  tempdir <- function()
  {
    if(.R.) {
      if(under.unix)
        tmp <- sub("/[^/]*$","", tempfile())
      else
        tmp <- sub("\\[^\\]*$","", tempfile())
    }
    else {
      if(under.unix) {
        tmp <- getenv("S_TMPDIR")
        if(identical(tmp, "")) {
          warning("S_TMPDIR not set, using old Splus startup script?  Will use unsafe S_TMPDIR=/tmp.")
          tmp <- "/tmp"
        }
      }
      else
        tmp <- "/windows/temp" 
    }
    tmp
  }
}

##xedit <- function(file, header, title, delete.file=FALSE) {
## In R, use e.g. options(pager=xedit); page(x,'p')
##  sys(paste('xedit -title "', title, '" ', file, ' &',
##            sep=''))
##  invisible()
##}

if(FALSE) {
  gless <- function(x, ...)
  {
    ## Usage: gless(x) - uses print method for x, puts in window with
    ## gless using name of x as file name prefixed by ~, leaves window open
    nam <- substring(deparse(substitute(x)), 1, 40)
    file <- paste('/tmp/',nam,sep='~')  #tempfile('Rpage.')
    sink(file)
    ##  cat(nam,'\n' )
    ##  if(length(attr(x,'label')) && !inherits(x,'labelled'))
    ##    cat(attr(x,'label'),'\n')
    ##  cat('\n')
    print(x, ...)
    sink()
    sys(paste('gless --geometry=600x400 "',file,'" &',sep=''))
    ## gless does not have a title option
    invisible()
  }
  NULL
}

xless <-
  function(x, ..., title=substring(deparse(substitute(x)),1,40))
{
  ## Usage: xless(x) - uses print method for x, puts in persistent window with
  ## xless using name of x as title (unless title= is specified)
  file <- tempfile()
  sink(file)
  print(x, ...)
  sink()
  cmd <- paste('xless -title "',title,'" -geometry "90x40" "',
               file,'" &',sep='')
  if(.R.)
    system(cmd)
  else
    sys(cmd)
  
  invisible()
}

gView <- function(x, ...,
                  title=substring(deparse(substitute(x)),1,40),
                  nup=1, fancy=TRUE, fontsize=if(nup==1)9 else 8)
{
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
  cmd <- if(fancy) 'enscript -G'
         else 'enscript'
  
  cmd <- if(nup==1)
           paste(cmd, '-B -p')
         else
           paste(cmd, ' -',nup,' -r -j -p',sep='')
  
  font <- paste('Courier', fontsize, sep='')
  sys(paste(cmd, file2, '-f', font, '-t', title, '-b', title, file))
  sys(paste('gv', file2, '&'))
  invisible()
}

pasteFit <- function(x, sep=',', width=.Options$width)
{
  ## pastes as many elements of character vector x as will fit in a line
  ## of width 'width', starting new lines when needed
  ## result is the lines of pasted text
  m <- nchar(x)
  out <- character(0)
  cur <- ''
  n   <- 0
  for(i in 1:length(x)) {
    if(cur=='' | (m[i] + nchar(cur) <= width))
      cur <- paste(cur, x[i],
                   sep=if(cur=='')''
                       else ',')
    else {
      out <- c(out, cur)
      cur <- x[i]
    }
  }
  if(cur != '') out <- c(out, cur)
  out
}

## Determine if variable is a date, time, or date/time variable in R
## or S-Plus.  The following 2 functions are used by describe.vector
## timeUsed assumes is date/time combination variable and has no NAs
testDateTime <- function(x, what=c('either','both','timeVaries'))
{
  what <- match.arg(what)
  cl <- class(x)  # was oldClass 22jun03
  if(!length(cl))
    return(FALSE)

  dc <- if(.R.)
          c('Date', 'POSIXt','POSIXct','dates','times','chron')
        else
          c('timeDate','date','dates','times','chron')
  
  dtc <- if(.R.)
           c('POSIXt','POSIXct','chron')
         else
           c('timeDate','chron')
  
  switch(what,
         either = any(cl %in% dc),
         both   = any(cl %in% dtc),
         timeVaries = {
           if('chron' %in% cl || 'Date' %in% cl || !.R.) { 
             ## chron or S+ timeDate
             y <- as.numeric(x)
             length(unique(round(y - floor(y),13))) > 1
           }
           else if(.R.)
             length(unique(format(x,'%H%M%S'))) > 1
           else
             FALSE
         })
}

## Format date/time variable from either R or S+
## x = a numeric summary of the original variable (e.g., mean)
## at = attributes of original variable
formatDateTime <- function(x, at, roundDay=FALSE)
{
  cl <- at$class
  w <- if(any(cl %in% c('chron','dates','times'))){
         attributes(x) <- at
         fmt <- at$format
         if(roundDay) {
           if(length(fmt)==2 && is.character(fmt))
             format.dates(x, fmt[1])
           else
             format.dates(x)
         }
         else x
       } else if(.R.) {
         attributes(x) <- at
         if(roundDay && 'Date' %nin% at$class) 
           as.POSIXct(round(x, 'days'))
         else x
       } else
         timeDate(julian=if(roundDay)round(x)
                         else x)
  format(w)
}

## Note that expr may contain multiple expressions in { } but you
## cannot do assignments to objects this way
if(!.R.)
  evalq <- function(expr, envir, enclos)
             eval(substitute(expr), envir)

if(!.R.) {
  download.file <- function(url, destfile, quiet=FALSE, cacheOK=TRUE,
                            ...)
  {
    extra <- if (quiet) " --quiet"
             else ""
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
             where='http://biostat.mc.vanderbilt.edu/twiki/pub/Main/DataSets')
  {
    what <- match.arg(what)
    fn <- as.character(substitute(file))
    ads <-
      scan(paste(where,'Rcontents.txt',sep='/'),list(''),quiet=TRUE)[[1]]
    a <- unlist(strsplit(ads,'.sav'))
    if(missing(file))
      return(a)

    wds <- paste(substitute(file),'sav',sep='.')
    if(wds %nin% ads)
      stop(paste(wds,'is not on the web site.\nAvailable datasets:\n',
                 paste(a, collapse=' ')))
    if(what %in% c('contents','all')) {
      w <- paste('C',fn,'.html',sep='')
      browseURL(paste(where,w,sep='/'))
    }
    
    if(what %in% c('description','all')) {
      ades <- scan(paste(where,'Dcontents.txt',sep='/'),list(''),
                   quiet=TRUE)[[1]]
      i <- grep(paste(fn,'\\.',sep=''),ades)
      if(!length(i))
        warning(paste('No description file available for',fn))
      else {
        w <- ades[i[1]]
        browseURL(paste(where,w,sep='/'))
      }
    }
    
    if(what %nin% c('data','all'))
      return(invisible())
    
    f <- paste(where,wds,sep='/')
    tf <- tempfile()
    download.file(f, tf, mode='wb', quiet=TRUE)
    load(tf, .GlobalEnv)
    invisible()
  }
} else {
  getHdata <-
    function(file,
             where='http://biostat.mc.vanderbilt.edu/twiki/pub/Main/DataSets')
  {
    tf <- tempfile()
    download.file(paste(where,'Scontents.txt',sep='/'), tf, quiet=TRUE)
    ads <- scan(tf,list(''))[[1]]
    a <- sedit(ads,'.sdd','')
    if(missing(file))
      return(a)
    
    file <- as.character(substitute(file))
    wds <- paste(file,'sdd',sep='.')
    if(wds %nin% ads)
      stop(paste(wds,'is not on the web site.\nAvailable datasets:\n',
                 paste(a, collapse=' ')))

    f <- paste(where,wds,sep='/')
    tf <- tempfile()
    download.file(f, tf, quiet=TRUE)
    data.restore(tf)  # puts in search position 1
    if(.SV4.)
      assign(file, cleanup.import(get(file,where=1)), where=1)
    unlink(tf)
    invisible()
  }
}

hdquantile <- function(x, probs=seq(0, 1, 0.25), se=FALSE,
                       na.rm=FALSE, names=TRUE, weights=FALSE)
{
  if(na.rm) {
    na <- is.na(x)
    if(any(na))
      x <- x[!na]
  }
  
  x <- sort(x, na.last=TRUE)
  n <- length(x)
  if(n < 2)
    return(rep(NA, length(probs)))
  
  m  <- n + 1

  ps <- probs[probs > 0 & probs < 1]
  qs <- 1 - ps

  a <- outer((0:n)/n, ps,
             function(x,p,m) pbeta(x, p*m, (1-p)*m), m=m)
  w <- a[-1,] - a[-m,]

  r <- drop(x %*% w)
  rp <- range(probs)
  pp <- ps
  if(rp[1]==0) {
    r <- c(x[1], r); pp <- c(0,pp)
  }

  if(rp[2]==1) {
    r <- c(r, x[n]); pp <- c(pp,1)
  }
  
  r <- r[match(pp, probs)]

  if(names) names(r) <- format(probs)

if(weights)
  attr(r,'weights') <- structure(w, dimnames=list(NULL,format(ps)))

  if(!se)
    return(r)
  if(n < 3)
    stop('must have n >= 3 to get standard errors')

  l <- n - 1
  a <- outer((0:l)/l, ps,
             function(x,p,m) pbeta(x, p*m, (1-p)*m), m=m)
  w <- a[-1,] - a[-n,]

  storage.mode(x) <- 'double'
  storage.mode(w) <- 'double'

  nq <- length(ps)
  ## Get all n leave-out-one quantile estimates
  S <- matrix(.Fortran("jacklins", x, w, as.integer(n), as.integer(nq),
                       res=double(n*nq), PACKAGE='Hmisc')$res, ncol=nq)

  se <- l * sqrt(diag(var(S))/n)

  if(rp[1]==0)
    se <- c(NA, se)
  
  if(rp[2]==1)
    se <- c(se, NA)
  
  se <- se[match(pp,probs)]
  if(names)
    names(se) <- names(r)
  
  attr(r, 'se') <- se
  r
}

sepUnitsTrans <- function(x, 
                          conversion=c(day=1, month=365.25/12, year=365.25, week=7),
                          round=FALSE, digits=0)
{
  if(!any(is.present(x)))
    return(x)
  
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
  if(round)
    x <- round(x, digits)
  
  units(x) <- target
  if(length(lab))
    label(x) <- lab
  x
}

if(!.R.) dQuote <- function (x)
{
  if (length(x) == 0) 
    return(character())
  paste("\"", x, "\"", sep = "")
}

makeNames <- function(names, unique=FALSE, allow=NULL)
{
  ## Runs make.names with exceptions in vector allow
  ## By default, R 1.9 make.names is overridden to convert _ to . as
  ## with S-Plus and previous versions of R.  Specify allow='_' otherwise.
  if(!.R. & length(allow))
    stop('does not apply for S-Plus')
  n <- make.names(names, unique)
  if(!length(allow))
    n <- gsub('_', '.', n)
  n
}

Load <- function(object)
{
  nam <- deparse(substitute(object))
  path <- .Options$LoadPath
  if(length(path))
    path <- paste(path,'/',sep='')
  file <- paste(path, nam, '.rda', sep='')
  load(file, .GlobalEnv)
}

Save <- function(object)
{
  .ObjectName <- deparse(substitute(object))
  path <- .Options$LoadPath
  if(length(path))
    path <- paste(path, '/', sep='')
  
  .FileName <- paste(path, .ObjectName, '.rda', sep='')
  assign(.ObjectName, object)
  eval(parse(text=paste('save(', .ObjectName, ', file="',
                        .FileName, '", compress=TRUE)', sep='')))
}

getZip <- function(url, password=NULL) {
  ## Allows downloading and reading a .zip file containing one file
  ## File may be password protected.  Password will be requested unless given.
  ## Example: read.csv(getZip('http://biostat.mc.vanderbilt.edu/twiki/pub/Sandbox/WebHome/z.zip'))
  ## Password is 'foo'
  ## url may also be a local file
  ## Note: to make password-protected zip file z.zip, do zip -e z myfile
  if(toupper(substring(url, 1, 7)) == 'HTTP://') {
    f <- tempfile()
    download.file(url, f)
  } else f <- url
  cmd <- if(length(password))
    paste('unzip -p -P', password) else 'unzip -p'
  pipe(paste(cmd, f))
}
