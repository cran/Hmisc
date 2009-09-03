## $Id: summary.formula.s 684 2009-08-30 15:52:24Z harrelfe $
##note: ars may always be T

summary.formula <-
  function(formula, data, subset, na.action, 
           fun=NULL,
           method=c('response','reverse','cross'),
           overall=method=='response'|method=='cross', 
           continuous=10, na.rm=TRUE, na.include=method!='reverse',
           g=4, quant = c(0.025, 0.05, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 0.95, 0.975),
           nmin=if(method=='reverse') 100
                else 0,
           test=FALSE,
           conTest=function(group,x) {
             st <- spearman2(group,x)
             list(P=st['P'], stat=st['F'],
                  df=st[c('df1','df2')],
                  testname=if(st['df1']==1)'Wilcoxon'
                           else 'Kruskal-Wallis',
                  statname='F', latexstat='F_{df}',
                  plotmathstat='F[df]')
           },
           catTest=function(tab) {
             st <-
               if(!is.matrix(tab) || nrow(tab) < 2 | ncol(tab) < 2)
                 list(p.value=NA, statistic=NA, parameter=NA)
               else
                 {
                   rowcounts <- tab %*% rep(1, ncol(tab))
                   tab <- tab[rowcounts > 0,]
                   if(!is.matrix(tab)) 
                     list(p.value=NA, statistic=NA, parameter=NA)
                   else chisq.test(tab, correct=FALSE)
                 }

             list(P=st$p.value, stat=st$statistic,
                  df=st$parameter,
                  testname='Pearson', statname='Chi-square',
                  latexstat='\\chi^{2}_{df}',
                  plotmathstat='chi[df]^2')
           },
           ordTest=function(group, x) {
             require(Design)

             f <- lrm(x ~ group)$stats
             list(P=f['P'], stat=f['Model L.R.'], df=f['d.f.'],
                  testname='Proportional odds likelihood ratio',
                  statname='Chi-square',latexstat='\\chi^{2}_{df}',
                  plotmathstat='chi[df]^2')
           },
           ...)
{
  call <- match.call()
  missmethod <- missing(method)   ## needed for R  9jul02
  method <- match.arg(method)
    
  X <- match.call(expand=FALSE)
  X$fun <- X$method <- X$na.rm <- X$na.include <- X$g <-
    X$overall <- X$continuous <- X$quant <- X$nmin <- X$test <-
      X$conTest <- X$catTest <- X$... <- NULL
  if(missing(na.action))
    X$na.action <- na.retain

  Terms <- if(missing(data)) terms(formula,'stratify')
  else terms(formula,'stratify',data=data)

  X$formula <- Terms
  X[[1]] <- as.name("model.frame")

  X <- eval(X, sys.parent())

  Terms <- attr(X,"terms")
  resp <- attr(Terms,"response")
    
  if(resp==0 && missmethod)
    method <- 'reverse'
  
  if(test && method!='reverse')
    stop('test=TRUE only allowed for method="reverse"')

  if(method!='reverse' && resp!=1) 
    stop("must have a variable on the left hand side of the formula")

  nact <- attr(X, "na.action")
  nvar <- ncol(X)-1
  strat <- attr(Terms,'specials')$stratify

  getlab <- function(x, default)
    {
      lab <- attr(x, 'label')
      if(!length(lab) || lab=='') default else lab
    }
  
  if(length(strat)) {
    if(method!='response') 
      stop('stratify only allowed for method="response"')

    temp <- untangle.specials(Terms,'stratify')
    strat.name <- if(.R.) var.inner(Terms)[temp$terms]
                  else attr(terms.inner(Terms),'term.labels')[temp$terms]
    strat <- if(length(temp$vars)==1) as.factor(X[[temp$vars]])
             else stratify(X[,temp$vars])

    strat.label <- getlab(X[,temp$vars[1]], strat.name)

    X[[temp$vars]] <- NULL   # remove strata factors
  } else {
    strat <- factor(rep('',nrow(X)))
    strat.name <- strat.label <- ''
  }

  nstrat <- length(levels(strat))
    
  if(resp>0) {
    Y <- X[[resp]]
    yname <- if(.R.) as.character(attr(Terms,'variables'))[2]
             else as.character(attr(Terms, "variables"))[1]  ## 25May01

    ylabel <- getlab(Y, yname)

    if(!is.matrix(Y))
      Y <- matrix(Y, dimnames=list(names(Y),yname))
  } else {
    yname <- ylabel <- NULL
  }
    
  if(method!='reverse') {
    if(!length(fun)) {   # was missing(fun) 25May01
      fun <- function(y) apply(y, 2, mean)

      uy <- unique(Y[!is.na(Y)])  # fixed 16Mar96
      r <- range(uy, na.rm=TRUE)
      funlab <- if(length(uy)==2 && r[1]==0 & r[2]==1) "Fraction"
                else "Mean"

      funlab <- paste(funlab, 'of', yname)
    } else if(is.character(fun) && fun=='%') {
      fun <- function(y)
      {
        stats <- 100*apply(y, 2, mean)
        names(stats) <- paste(dimnames(y)[[2]],'%')
        stats
      }

      funlab <- paste('% of', yname)
    }

    ## Compute number of descriptive statistics per cell
    s <-
      if(inherits(Y,'Surv'))
        as.vector((1 * is.na(unclass(Y))) %*% rep(1, ncol(Y)) > 0)
      else
        ((if(is.character(Y)) Y==''|Y=='NA'
          else is.na(Y)) %*%
         rep(1,ncol(Y))) > 0
    
    ## Was is.na.Surv, is.Surv 30May01
    stats <- if(length(dim(Y))) fun(Y[!s,,drop=FALSE])
             else fun(Y[!s])

    nstats <- length(stats)
    name.stats <-
      if(length(dn <- dimnames(stats))==2)
        as.vector(outer(dn[[1]],dn[[2]],FUN=function(a,b)paste(b,a)))
      else
        names(stats)

    if(length(fun)) {
      if(length(de <- deparse(fun)) == 2) {
        de <- as.list(fun)
        de <- as.character(de[[length(de)]])
        funlab <- if(de[1] == 'apply') de[length(de)]
                  else de[1]

        ## 2nd case is for simple function(x)mean(x) function
      } else funlab <- as.character(substitute(fun))
    }

    if(funlab[1]=='')
      funlab <- yname

    if(length(name.stats)==0) {
      name.stats <- if(nstats==1) yname
                    else paste(yname ,1:nstats,sep='')
    }
  }

  if(method=='response') {
    X[[resp]] <- NULL   # remove response var
    s <-
      if(!na.rm) FALSE
      else if(inherits(Y,'Surv'))
        as.vector((1 * is.na(unclass(Y))) %*% rep(1, ncol(Y)) > 0)
      else
        ((if(is.character(Y)) Y==''|Y=='NA'
          else is.na(Y)) %*% 
         rep(1,ncol(Y))) > 0

    ## was is.Surv(Y) ... is.na.Surv(Y)   25May01
    nmissy <- sum(s)
    if(nmissy) {
      X <- X[!s,,drop=FALSE]
      Y <- Y[!s,,drop=FALSE]
      strat <- strat[!s]
    }

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
      x <- if(v=='Overall') factor(rep('',n))
           else X[[v]]
      if(inherits(x,'mChoice')) x <- as.numeric(x)

      labels[i] <- getlab(x, nams[i])
      
      units[i]  <- if(length(l <- attr(x,'units'))) l
                   else ''

      if(!(ismc <- is.matrix(x))) {
        s <- is.na(x)
        if(!is.category(x)) {
          xu <- unique(x[!s]);
          lu <- length(xu)
          x <- if(lu < continuous) {
            r <- range(xu)
            if(lu==2 && r[1]==0 && r[2]==1) 
              factor(x,labels=c('No','Yes'))
            else
              factor(x)
          } else cut2(x, g=g, ...)
        }

        if(na.include && any(s)) {
          x <- na.include(x)
          if(.R.)
            levels(x)[is.na(levels(x))] <- 'NA'

          ## R 1.5 and later has NA as level not 'NA', satisfies is.na
        }

        xlev <- levels(x)
        if(nmin > 0) {
          nn <- table(x);
          xlev <- names(nn)[nn >= nmin]
        }
      } else {
        xlev <- dimnames(x)[[2]]
        if(!length(xlev))
          stop('matrix variables must have column dimnames')

        if(!is.logical(x)) {
          if(is.numeric(x))
            x <- x==1
          else {
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
          j <- if(ismc) strat==js  & x[,lx]
               else strat==js & x==lx

          if(!na.include)
            j[is.na(j)] <- FALSE

          nj <- sum(j)
          f <-
            if(nj) {
              statz <- unlist(fun(Y[j,,drop=FALSE]))
              ## 23apr03; had just let matrix replicate to fill
              ## Thanks: Derek Eder <derek.eder@neuro.gu.se>
              if(length(statz) != nstats)
                stop(paste('fun for stratum',lx,js,'did not return',
                           nstats, 'statistics'))

              matrix(statz, ncol=nstats, byrow=TRUE)
            } else rep(NA,nstats)

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
    quants <- unique(c(quant, 0.025, 0.05, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 0.95, 0.975))
    if(resp) {
      group <- as.factor(X[[resp]])[,drop=TRUE]
      group.freq <- table(group)
      group.freq <- group.freq[group.freq>0]
      if(overall)
        group.freq <- c(group.freq, Combined=sum(group.freq))
    } else {
      group <- rep(0,nrow(X))
      group.freq <- NULL
    }

    nv <- ncol(X)-resp
      
    n <- integer(nv)
    type <- n
    nams <- names(X)
    comp <- dat <- vector("list",nv)
    names(comp) <- names(dat) <- if(resp)nams[-1]
                                 else nams

    labels <- Units <- vector("character",nv)
    if(test) {
      testresults <- vector('list', nv)
      names(testresults) <- names(comp)
    }
      
    for(i in 1:nv) {
      w <- X[[resp+i]]
      if(length(attr(w,"label")))
        labels[i] <- attr(w,"label")

      if(length(attr(w,'units')))
        Units[i]  <- attr(w,'units')

      if(!inherits(w,'mChoice')) {
          if(!is.factor(w) && length(unique(w[!is.na(w)])) < continuous) 
            w <- as.factor(w)
          s <- !is.na(w)
          if(na.include && !all(s) && length(levels(w))) {
            w <- na.include(w)
            if(.R.)
              levels(w)[is.na(levels(w))] <- 'NA'

            s <- rep(TRUE,length(s))
          }

          n[i] <- sum(s)
          w <- w[s]
          g <- group[s, drop=TRUE]
          if(is.factor(w)) {
            tab <- table(w, g)
            if(test) {
              if(is.ordered(w))
                testresults[[i]] <- ordTest(g, w)
              else
                testresults[[i]] <- catTest(tab)
            }

            if(nrow(tab)==1) {  # 7sep02
              b <- casefold(dimnames(tab)[[1]],upper=TRUE)
              pres <- c('1','Y','YES','PRESENT')
              abse <- c('0','N','NO', 'ABSENT')
              jj <- match(b, pres, nomatch=0)
              if(jj > 0)
                bc <- abse[jj]
              else {
                jj <- match(b, abse, nomatch=0)
                if(jj > 0) bc <- pres[jj]
              }

              if(jj) {
                tab <- rbind(tab, rep(0, ncol(tab)))
                dimnames(tab)[[1]][2] <- bc
              }
            }

            if(overall)
              tab <- cbind(tab, Combined=apply(tab,1,sum))

            comp[[i]] <- tab
            type[i] <- 1
          } else {
            sfn <- function(x, quant)
            {
              o <- options(digits=10)
              ## 2sep02 so won't lose precision in quantile names
              on.exit(options(o))
              c(quantile(x,quant), Mean=mean(x), SD=sqrt(var(x)))
            }

            qu <- tapply(w, g, sfn, simplify=TRUE, quants)
            ## Added simplify=TRUE to work with R 7Jun01
            if(test)
              testresults[[i]] <- conTest(g, w)

            if(overall)
              qu$Combined <- sfn(w, quants)

            comp[[i]] <- matrix(unlist(qu),ncol=length(quants)+2,byrow=TRUE,
                                dimnames=list(names(qu),
                                              c(format(quants),'Mean','SD')))
            if(any(group.freq <= nmin))
              dat[[i]] <-
                lapply(split(w,g),nmin=nmin,
                       function(x,nmin)
                         if(length(x) <= nmin)x
                         else NULL)

            type[i] <- 2
          }
        } else {
          w <- as.numeric(w)==1 ## multiple choice variables
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
                                   
          if(overall)
            tab <- cbind(tab, Combined=apply(tab,1,sum))

          comp[[i]] <- tab
          type[i]   <- 3
        }
      }
	  
    labels <- ifelse(nchar(labels), labels, names(comp))
    return(structure(list(stats=comp, type=type, 
                          group.name=if(resp)nams[1]
                                     else NULL,
                          group.label=ylabel,
                          group.freq=group.freq,
                          labels=labels, units=Units,
                          quant=quant, data=dat,
                          N=sum(!is.na(group)), n=n,
                          testresults=if(test)testresults
                                      else NULL,
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
      if(inherits(xi,'mChoice'))
        xi <- factor(format(xi))
      else if(is.matrix(xi) && ncol(xi) > 1) 
        stop('matrix variables not allowed for method="cross"')

      labels[i] <- getlab(xi, nams[i])

      if(is.factor(xi))
        xi <- xi[,drop=TRUE]

      if(!is.factor(xi) && length(unique(xi[!is.na(xi)]))>=continuous)
        xi <- cut2(xi, g=g, ...)
      X[[i]] <- na.include(as.factor(xi))
      if(.R.)
        levels(X[[i]])[is.na(levels(X[[i]]))] <- 'NA'
        
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
    chk <- function(z, nstats)
    {
      if(length(z) != nstats)
        stop(paste('fun did not return',nstats,
                   'statistics for a stratum'))

      z
    }

    if(nvar==1) {
      df1 <- as.character(df[[1]]); x1 <- X[[1]]
      for(i in 1:nl) {
        s <- df1[i]=='ALL' | x1==df1[i]
        w <- if(na.rm) s & !na else s
        N[i] <- sum(w)
        Missing[i] <- sum(na[s])
        S[i,] <- if(any(w))chk(fun(Y[w,,drop=FALSE]),nstats)
                 else rep(NA,nstats)
      }
    } else if(nvar==2) {
      df1 <- as.character(df[[1]]);
      df2 <- as.character(df[[2]])
      x1 <- X[[1]];
      x2 <- X[[2]]
      for(i in 1:nl) {
        s <- (df1[i]=='ALL' | x1==df1[i]) & (df2[i]=='ALL' | x2==df2[i])
        w <- if(na.rm) s & !na
             else s

        N[i] <- sum(w)
        Missing[i] <- sum(na[s])
        S[i,] <- if(any(w))chk(fun(Y[w,,drop=FALSE]),nstats)
                 else rep(NA,nstats)
      }
    } else if(nvar==3) {
      df1 <- as.character(df[[1]]);
      df2 <- as.character(df[[2]])
      df3 <- as.character(df[[3]])
      
      x1 <- X[[1]];
      x2 <- X[[2]];
      x3 <- X[[3]]

      for(i in 1:nl) {
        s <- (df1[i]=='ALL' | x1==df1[i]) & (df2[i]=='ALL' | x2==df2[i]) &
             (df3[i]=='ALL' | x3==df3[i])
        w <- if(na.rm) s & !na
             else s

        N[i] <- sum(w)
        Missing[i] <- sum(na[s])
        S[i,] <- if(any(w))chk(fun(Y[w,,drop=FALSE]),nstats)
                 else rep(NA,nstats)
      }  
    } else stop('no more than 3 independent variables allowed')
    
    lab <- names(df)
    lab2 <- if(length(lab)>1) paste(lab,collapse=", ")
            else lab

    heading <- paste(funlab,"by",lab2)
    ##if(length(name.stats)) yname <- funlab <- name.stats

    S <- S[,,drop=TRUE]
    attr(S,"label") <- yname    #funlab
    df$S <- S
    df$N <- N
    df$Missing <- Missing
      
    a <- list(heading=heading,byvarnames=lab2,Levels=Levels,labels=labels,
              na.action=nact,formula=formula,call=call,yname=yname,ylabel=ylabel,
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
                                           prefix.width, min.colwidth,
                                           formatArgs=NULL, ...)
{
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
      if(ns>1)
        paste(' by',
              if(ul) at$strat.label
              else at$strat.name),
      '    N=',at$n,
      if(at$nmiss) paste(', ',at$nmiss,' Missing',sep=''),
      '\n\n',sep='')

  d <- dim(stats)
  
  if(exists('print.char.matrix')) {
    nr <- length(at$nlevels)
    vlab <- if(ul) vlabels[vlabels!='']
            else at$vname[at$vname!='']

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
    if(missing(prefix.width))
      prefix.width <- max(nchar(dimnames(z)[[1]]))
    
    if(missing(min.colwidth))
      min.colwidth <- 
        max(min(nchar(cstats)[nchar(cstats)>0]), min(nchar(dimnames(stats)[[2]])))

    z <- rbind(c('',dimnames(stats)[[2]]), z)
    if(.R.)
      print.char.matrix(z, col.names=FALSE, ...)
    else
      print.char.matrix(z,abbreviate.dimnames=abbreviate.dimnames,
                        prefix.width=prefix.width, 
                        min.colwidth=min.colwidth, ...)  

    return(invisible())
  } 

  dz <-
    if(length(at$strat.levels)==1)
      dimnames(stats)[[2]]
    else
      paste(rep(at$strat.levels,length=d[2]),dimnames(stats)[[2]],sep=":")
  
  z <- matrix('', ncol=d[2]+2, nrow=d[1],
              dimnames=list(rep('',d[1]),c('','',dz)))

  z[,1] <- if(ul) vlabels
           else at$vname

  z[,2] <- dimnames(stats)[[1]]
  for(i in 1:d[2]) {
    ww <- c(list(stats[,i]), formatArgs)  # 10Feb00
    z[,i+2] <- do.call('format', ww)
  }

  print(z, quote=FALSE)
  invisible()
}

latex.summary.formula.response <-
  function(object, 
           title=first.word(deparse(substitute(object))), caption,
           trios, vnames=c('labels','names'), prn=TRUE, prUnits=TRUE,
           rowlabel='', cdec=2,
           ncaption=TRUE, ...)
{
  stats <- object

  title <- title   # otherwise problem with lazy evaluation 25May01
  stats <- oldUnclass(stats)
  at <- attributes(stats)
  if(!prn)
    stats <- stats[,dimnames(stats)[[2]]!='N',drop=FALSE]

  vnames <- match.arg(vnames)
  ul <- vnames=='labels'
  ns <- length(at$strat.levels)
  nstat <- ncol(stats)/ns
  if(!missing(trios)) {
    if(is.logical(trios))
      trios <- at$ycolname

    ntrio <- length(trios)
    if(ntrio*3!=(nstat-1))   #allow for N
      stop('length of trios must be 1/3 the number of statistics computed')
  }

  if(missing(caption)) caption <- latexTranslate(at$ylabel)
  
  if(ns>1) caption <- paste(caption,' by', if(ul)at$strat.label else 
                            at$strat.name)
  if(ncaption)
    caption <- paste(caption,
                     '~~~~~N=',at$n,
                     if(at$nmiss) paste(',~',at$nmiss,' Missing',sep=''),
                     sep='')

  dm <- dimnames(stats)
  dm[[1]] <- latexTranslate(dm[[1]], greek=.R.)
  dm[[2]] <- latexTranslate(dm[[2]], greek=.R.)
  dimnames(stats) <- dm
  caption <- sedit(caption, "cbind", "")
  vn <- if(ul)at$vlabel
        else at$vname

  if(prUnits) {
    atvu <- translate(at$vunits, '*', ' ')
    vn <- ifelse(atvu=='', vn,
                 paste(vn,'~\\hfill\\tiny{', atvu, '}',sep=''))
  }

  vn <- latexTranslate(vn, greek=.R.)
  cdec <- rep(cdec,
              length=(if(missing(trios))nstat
                      else 1+(nstat-1)/3)-1)

  cdec <- rep(c(if(prn)0 else NULL,cdec), ns)

  if(missing(trios))
    cstats <- oldUnclass(stats)
  else {
    fmt <- function(z, cdec) ifelse(is.na(z), '', format(round(z,cdec)))
    cstats <- list()
    k <- m <- 0
    for(is in 1:ns) {
      k <- k+1;  m <- m+1
      cstats[[k]] <- stats[,m]   ## N, numeric mode
      for(j in 1:ntrio) {
        m <- m+1; k <- k+1
        cstats[[k]] <- paste('{\\scriptsize ',fmt(stats[,m],cdec[k]),'~}',
                             fmt(stats[,m+1],cdec[k]), ' {\\scriptsize ',
                             fmt(stats[,m+2],cdec[k]), '}',sep='')
        m <- m+2
      }
    }

    names(cstats) <- rep(c(if(prn)'N'
                           else NULL, trios), ns)
    
    attr(cstats, 'row.names') <- dm[[1]]
    attr(cstats,'class') <- 'data.frame'
    nstat <- 2  # for n.cgroup below
  }
  
  insert.bottom <-
    if(missing(trios))
      ''
    else 
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
  function(x, which = 1,
           vnames = c('labels', 'names'), xlim, xlab,
           pch = c(16, 1, 2, 17, 15, 3, 4, 5, 0), superposeStrata = TRUE,
           dotfont=1, add=FALSE, reset.par=TRUE,
           main, subtitles=TRUE, ...)
{
  stats <- x
  stats  <- oldUnclass(stats)
  vnames <- match.arg(vnames)
  ul <- vnames=='labels'
  at <- attributes(stats)
  ns <- length(at$strat.levels)
  if(ns>1 && length(which)>1) 
    stop('cannot have a vector for which if > 1 strata present')

  if(ns < 2)
    superposeStrata <- FALSE

  vn <- if(ul) at$vlabel
        else at$vname

  Units <- at$vunits
  vn <- ifelse(Units=='', vn, paste(vn, ' [', Units, ']', sep=''))
  ## dotchart2 groups argument may not be an R plotmath expression
  vn <- vn[vn!='']
  d  <- dim(stats)
  n  <- d[1]
  nstat <- d[2]/ns
  vnd <- factor(rep(vn, at$nlevels))
  dn <- dimnames(stats)
  if(missing(xlim))
    xlim <- range(stats[,nstat*((1:ns)-1)+1+which],na.rm=TRUE)

  if(missing(main))
    main <- at$funlab

  nw      <- length(which)
  pch     <- rep(pch, length=if(superposeStrata)ns else nw)
  dotfont <- rep(dotfont, length=nw)
  opar <- if(.R.) par(no.readonly=TRUE)
          else par()

  if(reset.par)
    on.exit(par(opar))

  if(superposeStrata) Ns <- apply(stats[,nstat*((1:ns)-1)+1],1,sum)
  
  for(is in 1:ns) {
    for(w in 1:nw) {
      js <- nstat*(is-1)+1+which[w]
      z <- stats[,js]
      if(missing(xlab))
        xlab <- if(nw>1) dn[[2]][js]
                else at$ylabel

      dotchart2(z, groups=vnd, xlab=xlab, xlim=xlim,
                auxdata=if(superposeStrata) Ns
                        else stats[,js-which[w]],
                auxtitle='N', sort=FALSE,
                pch=pch[if(superposeStrata)is
                        else w], 
                dotfont=dotfont[w], 
                add=add | w>1 | (is > 1 && superposeStrata),
                reset.par=FALSE, ...)

      if(ns>1 && !superposeStrata)
        title(paste(paste(main,if(main!='')'   '),at$strat.levels[is]))
      else if(main!='') title(main)

      if(ns==1 && subtitles) {
        title(sub=paste('N=',at$n,sep=''),adj=0,cex=.6)
        if(at$nmiss>0)
          title(sub=paste('N missing=',at$nmiss,sep=''),cex=.6,adj=1)
      }
    }
  }

  if(superposeStrata) { ##set up for Key()
    Key <- if(.R.) function(x=NULL, y=NULL, lev, pch)
    {
      oldpar <- par(usr=c(0,1,0,1),xpd=NA)
      on.exit(par(oldpar))
      if(is.list(x)) {
        y <- x$y;
        x <- x$x
      }

      if(!length(x))
        x <- 0

      if(!length(y))
        y <- 1  ## because of formals()

      rlegend(x, y, legend=lev, pch=pch, ...)
      invisible()
    }
    else function(x=NULL, y=NULL, lev, pch, ...) {
      if(length(x)) {
        if(is.list(x)) {
          y <- x$y;
          x <- x$x
        }

        key(x=x, y=y, text=list(lev), 
            points=list(pch=pch),
            transparent=TRUE, ...)
      } else key(text=list(lev), 
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
  function(x, vnames = c('labels', 'names'), what = c('proportion','%'),
           which = c('both', 'categorical', 'continuous'),
           xlim = if(what == 'proportion') c(0,1)
                  else c(0,100), 
           xlab = if(what == 'proportion') 'Proportion'
                  else 'Percentage', 
           pch = c(16, 1, 2, 17, 15, 3, 4, 5, 0), exclude1 = TRUE,
           dotfont = 1, main, subtitles = TRUE,
           prtest = c('P', 'stat', 'df', 'name'), pdig = 3, eps = 0.001,
           conType = c('dot', 'bp', 'raw'), cex.means = 0.5, ...)
{
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
  vn <- if(ul) obj$labels
        else varNames
    
  Units <- obj$units
  
  nw     <- if(lg <- length(obj$group.freq)) lg
            else 1

  gnames <- names(obj$group.freq) 

  if(missing(main))
    main <- if(nw==1)''
  else 
    paste(if(what=='proportion')'Proportions'
          else 'Percentages','Stratified by',
          obj$group.label)

  pch     <- rep(pch, length=nw)
  dotfont <- rep(dotfont, length=nw)
  
  lab <- vnd <- z <- nmiss <- vnamd <- NULL
  type  <- obj$type; n <- obj$n

  opar <- par()
  on.exit(setParNro(opar))

  npages <- 0
  
  if(which != 'continuous' && any(type %in% c(1,3))) {
    ftstats <- NULL  
    for(i in (1:length(type))[type==1 | type==3]) {
      nam <- vn[i]
      tab <- obj$stats[[i]]
      if(nw==1)
        tab <- as.matrix(tab)

      nr <- nrow(tab)
      denom <- if(type[i]==1) apply(tab, 2, sum)
               else obj$group.freq

      y <- (if(what=='proportion') 1
            else 100) * sweep(tab, 2, denom, FUN='/')

      lev <- dimnames(y)[[1]]
      exc <- exclude1 && (nr==2)
      jstart <- if(exc) 2
                else 1

      ##  nn <- c(nn, n[i], rep(NA, if(exc) nr-2 else nr-1))
      ##  k <- 0

      rl <- casefold(lev)
      binary <- type[i]==1 && exc &&
                (all(rl %in% c("0","1"))|all(rl %in% c("false","true"))|
                 all(rl %in% c("absent","present")))

      for(j in jstart:nrow(y)) {
        if(nw==1)
          z <- rbind(z, y[j,])
        else {
          yj <- rep(NA, nw)
          names(yj) <- gnames
          yj[names(y[j,])] <- y[j,]
          z <- rbind(z, yj)
        }

        lab <- c(lab, if(binary) ''
                      else lev[j])

        vnd <- c(vnd, nam)
        vnamd <- c(vnamd, varNames[i])
      }

      if(any(prtest != 'none')) {
        fts <- formatTestStats(test[[varNames[i]]], type[i]==3,
                               if(type[i]==1)1
                               else 1:nr,
                               prtest=prtest,
                               plotmath=.R.,
                               pdig=pdig, eps=eps)

        ftstats <- c(ftstats, fts, 
                     if(type[i]==1 && nr-exc-1 > 0)
                       rep(if(.R.) expression('')
                           else '',
                           nr-exc-1))
      }
    }

    dimnames(z) <- list(lab, dimnames(z)[[2]])
    for(i in 1:nw) {
      zi <- z[,i]
      if(any(prtest == 'none') || i > 1)
        dotchart2(zi, groups=vnd, xlab=xlab, xlim=xlim, 
                  sort=FALSE, pch=pch[i],
                  dotfont=dotfont[i],
                  add=i>1, ...)
      else
        dotchart2(zi, groups=vnd, auxdata=ftstats,
                  xlab=xlab, xlim=xlim, sort=FALSE,
                  pch=pch[i], dotfont=dotfont[i],
                  add=i>1, ...)
    }

    if(main!='')
      title(main)

    npages <- npages + 1
    setParNro(opar)
    ## Dummy key if only one column, so won't use another Key from an
    ## earlier run
    if(nw < 2) {
      Key <- function(...)invisible(NULL)
      storeTemp(Key)
    } else { ##set up for key() if > 1 column
      Key <- if(.R.) function(x=NULL, y=NULL, lev, pch)
      {
        oldpar <- par(usr=c(0,1,0,1),xpd=NA)
        on.exit(par(oldpar))
        if(is.list(x)) {
          y <- x$y;
          x <- x$x
        }

        ## Even though par('usr') shows 0,1,0,1 after lattice draws
        ## its plot, it still needs resetting
        if(!length(x))
          x <- 0

        if(!length(y))
          y <- 1  ## because of formals()

        rlegend(x, y, legend=lev, pch=pch, ...)
        invisible()
      } else function(x=NULL, y=NULL, lev, pch, ...)
      {
        if(length(x)) {
          if(is.list(x)) {
            y <- x$y;
            x <- x$x
          }

          key(x=x, y=y, text=list(lev), 
              points=list(pch=pch),
              transparent=TRUE, ...)
        } else
          key(text=list(lev), 
              points=list(pch=pch),transparent=TRUE, ...)

        invisible()
      }

      formals(Key) <- list(x=NULL,y=NULL,lev=names(obj$group.freq),
                           pch=pch)
      storeTemp(Key)
    }
  }

  ncont <- sum(type==2)
  if(which != 'categorical' && ncont) {
    mf <- par('mfrow')
    if(length(mf)==0)
      mf <- c(1,1)

    if(ncont > 1 & max(mf)==1) {
      mf <- if(ncont <= 4)c(2,2)
            else if(ncont <= 6)c(2,3)
            else if(ncont <= 9)c(3,3)
            else c(4,3)

      ## if(ncont <= 12)c(4,3) else if(ncont <= 16) c(4,4) else c(5,4)
      nr <- mf[1]
      m  <- par('mar')
      par(mfrow=mf)
    }

    npages <- npages + ceiling(sum(type==2) / prod(mf))
    
    for(i in (1:length(type))[type==2]) {
      nam <- labelPlotmath(vn[i], Units[i])
      st <- obj$stats[[i]]
      if(nw==1)
        st <- as.matrix(st)

      if(conType=='dot') {
        quantile.columns <- dimnames(st)[[2]] %nin% c('Mean','SD')
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
          if(nw==1)
            names(stj) <- ''

          dotchart2(stj, xlab=nam, xlim=xlim, sort=FALSE,
                    pch=c(91,
                          if(FALSE)183
                          else 16,
                          93)[j],
                    dotfont=dotfont[1],
                    add=j > 1, ...)
        }

        Key2 <- function(x=NULL, y=NULL, quant, ...)
          {
            quant <- format(quant)
            txt <- paste('(',quant[2],',',quant[3],',',quant[4], 
                         ') quantiles shown\nx-axes scaled to (',quant[1],',',
                         quant[5],') quantiles', sep='')
            if(length(x)) {
              if(is.list(x)) {
                y <- x$y;
                x <- x$x
              }

              text(x,y,txt, cex=.8, adj=0, ...)
            } else
            mtitle(lr=txt, cex.l=.8, line=1, ...)

            invisible()
          }

        formals(Key2) <- list(x=NULL,y=NULL,quant=obj$quant)
        storeTemp(Key2)
        
      } else if(conType=='bp')
        bpplt(st, xlab=nam, cex.points=cex.means)
      else
        stripChart(obj$data[[i]], xlab=nam)

      if(all(prtest != 'none')) {
        fts <- formatTestStats(test[[varNames[i]]], prtest=prtest,
                               plotmath=.R.,
                               pdig=pdig, eps=eps)
        title(fts, line=.5)  ## .5 ignored in S-Plus
      }
    }
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
#group headings.
#cex.labels is a cex to be used only for category labels.  Default is cex.
#Added reset.par - set to T to reset par() after making plot.  You will
#need to set reset.par to T for the last call in a sequence.
dotchart2 <- 
  function(data, labels, groups = NULL, gdata = NA, horizontal = TRUE, 
           pch = 16, 
           xlab = "", ylab="", auxdata, auxgdata=NULL, auxtitle,
           lty = if(.R.)1 else 2,
           lines = TRUE, dotsize = .8, cex = par("cex"), 
           cex.labels = cex, cex.group.labels = cex.labels*1.25, sort.=TRUE, 
           add=FALSE, dotfont=par('font'),
           groupfont=2, reset.par=add, xaxis=TRUE,
           width.factor=1.1, lcolor=if(.R.) 'gray' else par('col'),
           ...)
{
  if(.R. && !add)
    {
      plot.new()   ## needed for strwidth
      par(new=TRUE)
    }

  ieaux <- if(missing(auxdata)) FALSE else is.expression(auxdata)
  
  mtextsrt <- function(..., srt=0)
    if(.R.) mtext(..., las=1) else mtext(..., srt=srt)

  ndata <- length(data)
  if(missing(labels))
    {
      if(!is.null(names(data)))
        labels <- names(data)
      else labels <- paste("#", seq(along = ndata))
    }
  else labels <- rep(as.character(labels), length = ndata)

  if(missing(groups))
    {
      glabels <- NULL
      gdata <- NULL
    } else
  {
    if(!sort.)
      {
        ##assume data sorted in groups, but re-number groups
        ##to be as if groups given in order 1,2,3,...
        ug <- unique(as.character(groups))
        groups <- factor(as.character(groups),levels=ug)
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
  if(!missing(auxdata))
    {
      auxdata <- c(auxdata, auxgdata)
      if(!ieaux) auxdata <- format(auxdata)
    }
  
  alllab <- paste(c(labels, glabels),'')
  ## set up margins and user coordinates, draw box
  tcex <- par('cex')
  tmai <- par("mai")
  oldplt <- par("plt")
  if(reset.par)
    on.exit(par(mai = tmai, cex = tcex, usr = tusr))

  par(cex = cex)
  mxlab <- .1+max(strwidth(labels, units='inches',cex=cex.labels),
                  if(length(glabels))
                  strwidth(glabels,units='inches',cex=cex.group.labels))*
                    width.factor
  if(horizontal)
    {
      tmai2 <- tmai[3:4]
      if(!missing(auxdata))
        tmai2[2] <- .2+width.factor*
          max(strwidth(if(ieaux) auxdata else format(auxdata),
                       units='inches',cex=cex.labels))
      
      par(mai = c(tmai[1], mxlab, tmai2))
      if(!add)
        plot(alldat, seq(along = alldat), type = "n",
             ylab = '', axes = FALSE, xlab = '', ...)
      
      logax <- par("xaxt") == "l"
    }
  else
    {
      par(mai = c(mxlab, tmai[2:4]))
      if(!add)
        plot(seq(along = alldat), alldat, type = "n",
             xlab = "", axes = FALSE, ylab = '', ...)
      
      logax <- par("yaxt") == "l"
    }

  tusr <- par("usr")
  if(!add && logax)
    {
      if(horizontal)
        abline(v = 10^tusr[1:2], h = tusr[3:4])
      else abline(v = tusr[1:2], h = 10^tusr[3:4])
    }
  else if(!add) abline(v = tusr[1:2], h = tusr[3:4])

  den <- ndata + 2 * length(glabels) + 1
  if(horizontal)
    {
      if(!add && xaxis)
        mgp.axis(1, axistitle=xlab)

      delt <- ( - (tusr[4] - tusr[3]))/den
      ypos <- seq(tusr[4], by = delt, length = ndata)
  }
  else
    {
      if(!add)
        mgp.axis(2, axistitle=xlab)

      delt <- (tusr[2] - tusr[1])/den
      ypos <- seq(tusr[1], by = delt, length = ndata)
    }

  if(!missing(groups))
    {
      ypos1 <- ypos + 2 * delt * (if(length(groups)>1)
                                  cumsum(c(1, diff(groups) > 0))
      else 1)
      diff2 <- c(3 * delt, diff(ypos1))
      ypos2 <- ypos1[abs(diff2 - 3 * delt) < abs(0.001 * delt)] - 
        delt
      ypos <- c(ypos1, ypos2) - delt
    }

  ##put on labels and data
  ypos <- ypos + delt
  nongrp <- 1:ndata
  if(horizontal)
    {
      xmin <- par('usr')[1]
      if(!add && lines)
        abline(h = ypos[nongrp], lty = lty, lwd=1, col=lcolor)

      points(alldat, ypos, pch = pch, cex = dotsize * cex, font=dotfont)
      if(!add && !missing(auxdata))
        {
          faux <- if(ieaux) auxdata else paste(' ', format(auxdata), sep='')

          upedge <- par('usr')[4]
          outerText(faux, ypos[nongrp], adj=1, cex=cex.labels)
          if(!missing(auxtitle))
            {
              auxtitle <- paste(' ', auxtitle, sep='')
              outerText(auxtitle,
                        upedge+strheight(auxtitle,cex=cex.labels)/2,
                        adj=1, cex=cex.labels, setAside=faux[1])
            }
        }

      if(!add)
        {
          labng <- alllab[nongrp]
          ## Bug in sending character strings to mtext or text containing
          ## [ or ] - they don't right-justify in S+
          bracket <- substring(labng,1,1)=='[' |
          substring(labng,nchar(labng),nchar(labng))==']'
          yposng <- ypos[nongrp]
          s <- !bracket
          if(!is.na(any(s)) && any(s))
            mtextsrt(paste(labng[s],''), 2, 0, at=yposng[s],
                     srt=0, adj=1, cex=cex.labels)

          s <- bracket
          if(!is.na(any(s)) && any(s))
            {
              if(.R.)
                text(rep(par('usr')[1],sum(s)),
                     yposng[s], labng[s], adj=1,
                     cex=cex.labels, srt=0,xpd=NA)
              else if(.SV4. && under.unix)
                text(rep(par('usr')[1],sum(s)),
                     yposng[s], labng[s], adj=1,
                     cex=cex.labels, srt=0)
              else
                {
                  xmin <- par('usr')[1] -
                    max(nchar(labng[s]))*0.5*cex.labels*par('1em')[1]
                  text(rep(xmin,sum(s)), yposng[s], labng[s], adj=0,
                       cex=cex.labels, srt=0)
                }
            }
          
          if(!missing(groups))
            mtextsrt(paste(alllab[ - nongrp],''), 2, 0, at = ypos[ - nongrp], 
                     srt = 0, adj = 1, cex = cex.group.labels, font=groupfont)
        }
    }
  else
    {
      if(!add && lines)
        abline(v = ypos[nongrp], lty = lty, lwd=1, col=lcolor)

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
  else
    {
      frac <- (oldplt[4] - oldplt[3])/(oldplt[4] - plt[3])
      umin <- tusr[4] - (tusr[4] - tusr[3]) * frac
      tusr <- c(tusr[1:2], umin, tusr[4])
    }

  invisible()
}


print.summary.formula.reverse <- 
  function(x, digits, prn=any(n != N), pctdig=0, 
           npct=c('numerator','both','denominator','none'),
           exclude1=TRUE, vnames=c("labels","names"), prUnits=TRUE,
           sep="/", abbreviate.dimnames=FALSE, 
           prefix.width=max(nchar(lab)), 
           min.colwidth, formatArgs=NULL, round=NULL,
           prtest=c('P','stat','df','name'), prmsd=FALSE, long=FALSE,
           pdig=3, eps=0.001, ...)
{
  npct   <- match.arg(npct)
  vnames <- match.arg(vnames)
  if(is.logical(prtest) && !prtest)
    prtest <- 'none'

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
  if(!length(test))
    prtest <- 'none'

  nw     <- if(lg <- length(x$group.freq)) lg
            else 1  #23Nov98

  gnames <- names(x$group.freq)

  if(!missing(digits)) {    #.Options$digits <- digits 6Aug00
    oldopt <- options(digits=digits)
    on.exit(options(oldopt))
  }


  cstats <- NULL
  for(i in 1:nv) {
    nn <- c(nn, n[i])
    nam <- if(vnames=="names") nams[i]
           else labels[i]

    if(prUnits && nchar(Units[i]))
      nam <- paste(nam,' [',translate(Units[i],'*',' '),']',sep='')

    tr <- if(length(test) && all(prtest!='none')) test[[nams[i]]]
          else NULL

    if(type[i]==1 || type[i]==3) {
      cs <- formatCats(stats[[i]], nam, tr, type[i],
                       if(length(x$group.freq)) x$group.freq else x$n[i],
                       npct, pctdig, exclude1, long, prtest,
                       pdig=pdig, eps=eps)
      nn <- c(nn, rep(NA, nrow(cs)-1))
    } else cs <- formatCons(stats[[i]], nam, tr, x$group.freq, prmsd,
                            sep, formatArgs, round, prtest,
                            pdig=pdig, eps=eps)

    cstats <- rbind(cstats, cs)
  }

  lab <- dimnames(cstats)[[1]]
  gl <- names(x$group.freq)
  gl <- if(length(gl)) paste(gl," \n(N=",x$group.freq,")",sep="")
        else ""

  if(length(test) && !all(prtest=='none'))
    gl <- c(gl,
            if(length(prtest)==1 && prtest!='stat')
              if(prtest=='P')'P-value'
              else prtest
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
      if(length(x$group.label))
        paste(" by",x$group.label)
      else
        paste("  (N=",x$N,")",sep=""),"\n\n", sep="")

  if(exists("print.char.matrix")) {
    if(missing(min.colwidth))
      min.colwidth <- max(min(nchar(gl)),min(nc[nc>0]))

    if(.R.)
      print.char.matrix(cstats, col.names=FALSE,
                        col.txt.align='left', ...)
    else
      print.char.matrix(cstats, abbreviate.dimnames=abbreviate.dimnames,
                        prefix.width=prefix.width,
                        min.colwidth=min.colwidth, ...)
  } else

  print(cstats, quote=FALSE)
  invisible(cstats)
}

## Function to format subtable for categorical var, for method='reverse'
formatCats <- function(tab, nam, tr, type, group.freq,
                       npct, pctdig, exclude1, long, prtest,
                       latex=FALSE, testUsed=character(0),
                       npct.size='scriptsize', pdig=3, eps=.001,
                       footnoteTest=TRUE, dotchart=FALSE)
{
  gnames <- names(group.freq)
  nr <- nrow(tab)

  ## If there was a missing column of tab because e.g. the variable was
  ## always NA for one (or more) of the groups, add columns of NAs
  if(ncol(tab) < length(group.freq)) {
    tabfull <- matrix(NA,nrow=nr,ncol=length(group.freq),
                      dimnames=list(dimnames(tab)[[1]],gnames))
    tabfull[,dimnames(tab)[[2]]] <- tab
    tab <- tabfull
  }

  denom <- if(type==1) apply(tab, 2, sum)
           else group.freq

  pct <- 100*(if(ncol(tab) > 1)sweep(tab, 2, denom, FUN='/') else tab/denom)
  cpct <- paste(format(round(pct, pctdig)),
                if(latex)"\\%"
                else "%",
                sep="")

  denom.rep <- matrix(rep(format(denom),nr),nrow=nr,byrow=TRUE)
  if(npct!='none')
    cpct <- paste(cpct,
                  if(latex)
                    switch(npct,
                           numerator=paste('{\\',npct.size,' (',format(tab),')}',sep=''),
                           denominator=paste('{\\',npct.size,' of',denom.rep,'}'),
                           both=paste('{\\',npct.size,' $\\frac{',
                                      format(tab),'}{',denom.rep,
                                      '}$}',sep=''))
                  else
                  switch(npct,
                         numerator=paste('(',format(tab),')',sep=''),
                         denominator=paste('of',denom.rep),
                         both=paste(format(tab),'/',denom.rep,sep='')))
  
  if(latex)
    cpct <- sedit(cpct,' ','~')

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
  jstart <- if(exc) 2
            else 1
  
  nw <- if(lg <- length(group.freq)) lg
        else 1

  lab <- if(binary) nam
         else if(long) c(nam, paste('   ',lev[jstart:nr]))
         else c(paste(nam,':',lev[jstart]),
                if(nr > jstart) paste('   ',lev[(jstart+1):nr]))
  
  cs <- matrix('', nrow=long+(if(exc)nr-1
                              else nr),
               ncol=nw + (length(tr) > 0),
               dimnames=list(lab, c(gnames,
                                    if(length(tr))''
                                    else NULL)))

  if(nw==1)
    cs[(long+1):nrow(cs),1] <- cpct[jstart:nr,]
  else
    cs[(long+1):nrow(cs),1:nw] <- cpct[jstart:nrow(cpct),gnames]

  if(latex && dotchart && ncol(pct) <= 3) {
    locs <- c(3,-3,5,-5,7,-7,9,-9)
    points <- c("\\circle*{4}","\\circle{4}","\\drawline(0,2)(-1.414213562,-1)(1.414213562,-1)(0,2)")
    
    point.loc <- sapply(jstart:nrow(pct),
                        function(i) {
                          paste(ifelse(is.na(pct[i,]), "",
                                       paste("\\put(", pct[i,], ",0){",points[1:ncol(pct)],"}",sep='')),
                                collapse='')
                        })

    error.loc <- character(nrow(tab) - exc)
    k <- 0
    for(i in jstart:ncol(tab)) {
      if(i > jstart) {
        p1prime <- (tab[,i] + 1)/(denom[i] + 2)
        d1 <- p1prime*(1-p1prime)/denom[i]
        for(j in jstart:(i-1)) {
          k <- k + 1
          p2prime <- (tab[,j] + 1)/(denom[j] + 2)
          error <- 196 * sqrt(d1 + p2prime * (1 - p2prime)/denom[j])
          bar <- ifelse(is.na(error), "",
                        paste("\\put(", (pct[,i] + pct[,j])/2 - error, ",",
                              locs[k],"){\\line(1,0){",error*2,"}}",
                              sep=''))
          error.loc <- paste(error.loc, bar, sep='')
        }
      }
    }

    scale <- character(nrow(tab) - exc)
    scale[1] <- "\\multiput(0,2)(25,0){5}{\\color[gray]{0.5}\\line(0,-1){4}}\\put(-5,0){\\makebox(0,0){\\tiny 0}}\\put(108,0){\\makebox(0,0){\\tiny 1}}"
                     
    cl <- paste("\\setlength\\unitlength{1in/100}\\begin{picture}(100,10)(0,-5)",
                scale,"\\put(0,0){\\color[gray]{0.5}\\line(1,0){100}}",
                point.loc, error.loc,
                "\\end{picture}", sep='')
    cs[(long+1):nrow(cs),ncol(cs)] <- cl
  }

  if(length(tr)) {
    ct <- formatTestStats(tr, type==3,
                          if(type==1)1
                          else 1:nr,
                          prtest, latex=latex, testUsed=testUsed,
                          pdig=pdig, eps=eps, footnoteTest=footnoteTest)

    if(length(ct)==1)
      cs[1,ncol(cs)] <- ct
    else
      cs[(long+1):nrow(cs),ncol(cs)] <- ct
  }

  cs
}


## Function to format subtable for continuous var, for method='reverse'
formatCons <- function(stats, nam, tr, group.freq, prmsd, sep='/',
                       formatArgs=NULL, round=NULL, prtest,
                       latex=FALSE, testUsed=character(0),
                       middle.bold=FALSE, outer.size=NULL, msdsize=NULL,
                       pdig=3, eps=.001, footnoteTest=TRUE)
{
  nw <- if(lg <- length(group.freq)) lg
        else 1

  ns <- dimnames(stats)[[2]]
  ns <- ifelse(ns %in% c('Mean','SD'), '-1', ns)
  ns <- as.numeric(ns)
  l  <- 1:length(ns)
  q1  <- l[abs(ns-.25) < .001]
  med <- l[abs(ns-.5) < .001]
  q3  <- l[abs(ns-.75) < .001]
  qu <- stats[,c(q1,med,q3),drop=FALSE]
  if(prmsd)
    qu <- cbind(qu,stats[,c('Mean','SD'),drop=FALSE])
  if(length(round)) qu <- round(qu, round)
  ww <- c(list(qu), formatArgs)
  cqu <- do.call('format', ww)
  cqu[is.na(qu)] <- ''
  if(latex) {
    st <- character(nrow(cqu))
    names(st) <- dimnames(qu)[[1]]   ## 31jul02
    bld <- if(middle.bold) '\\bf '
           else ''

    for(j in 1:nrow(cqu)) {
      st[j] <- paste("{\\",outer.size," ",cqu[j,1],
                     "~}{",bld,cqu[j,2],
                     " }{\\",outer.size," ",cqu[j,3],"} ",sep="")
      if(prmsd)
        st[j] <-
          if(length(msdsize))
            paste(st[j], '~{\\',msdsize,'(',cqu[j,4], '$\\pm$',
                  cqu[j,5],')}', sep='')
          else
            paste(st[j], '~(', cqu[j,4], '$\\pm$',
                  cqu[j,5],')', sep='')
    }
  }
  else st <-
    if(prmsd)
      apply(cqu, 1,
            function(x,sep) paste(x[1],sep,x[2],sep,x[3],'  ',
                                  x[4],'+/-',x[5],sep=''), sep=sep)
    else
      apply(cqu, 1, paste, collapse=sep)

  if(any(is.na(qu)))
    st <- ""

  if(nw==1)
    yj <- st
  else {
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
                            i=if(multchoice)NA
                              else 1,
                            prtest, latex=FALSE,
                            testUsed=character(0),
                            pdig=3, eps=.001,
                            plotmath=FALSE, footnoteTest=TRUE)
{
  ## tr=an element of testresults (created by summary.formula method='reverse')
  if(i > 1 && !multchoice)
    stop('logic error')
  
  pval     <- tr$P[i]
  teststat <- tr$stat[i]
  testname <- tr$testname

  if(any(is.na(pval)) || any(is.na(teststat))) {
    res <- rep('', length(pval))
    if(latex && length(testUsed))
      res <-
        if(footnoteTest)
          rep(paste('$^{',match(testname,testUsed),
                    '}$',sep=''), length(pval))
        else rep('', length(pval))

    return(res)
  }

  ## Note: multchoice tests always have only one type of d.f.
  deg <- if(multchoice)tr$df[i]
         else tr$df
  
  dof <- if(multchoice) as.character(deg)
         else paste(deg,collapse=',')
  
  statname <- if(latex) tr$latexstat
              else if(plotmath) tr$plotmathstat
              else tr$statname
  
  if(length(prtest)>1 && 'stat' %in% prtest && (latex || plotmath)) {
    ## replace "df" inside statname with actual d.f.
    if(length(grep('df',statname)))
      statname <- sedit(statname, 'df',
                        if(latex || length(deg)==1) dof
                        else paste('list(',dof,')', sep=''))
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
            if(footnoteTest && length(testUsed))
              paste('^{',match(testname,testUsed),
                    '}',sep=''),'$',sep='')
    else paste('$',
               if('stat' %in% prtest)
                 paste(statname,'=',format(round(teststat,2)),sep=''),
               if(all(c('stat','P') %in% prtest)) ',~',
### 21dec03        if('P' %in% prtest)paste('P',if(plt)'' else '=', pval,
###                                        sep=''),
               if('P' %in% prtest)paste('P',ifelse(plt,'','='), pval,
                                        sep=''),
               if(footnoteTest && length(testUsed))
                 paste('^{',match(testname,testUsed),
                       '}',sep=''),
               '$')
  } else if(plotmath) {
    if(length(prtest)==1)
      parse(text=switch(prtest,
### 21dec03             P=if(plt)paste('~P',pval,sep='') else
###                      paste('~P==',pval,sep=''),
                        P=ifelse(plt,paste('~P',pval,sep=''),
                                 paste('~P==',pval,sep='')),
                        stat=format(round(teststat,2)),
                        dof=format(dof),
                        name=statname))
    else
      parse(text=paste(if('stat' %in% prtest)
                         paste('~list(',statname,'==',
                               format(round(teststat,2)),sep=''),
                       if(all(c('stat','P') %in% prtest)) ', ',
### 21dec03   if('P' %in% prtest)paste(if(plt)'~P' else '~P==',pval,')',sep='')))
                       if('P' %in% prtest)paste(ifelse(plt,'~P','~P=='),pval,')',sep='')))
  } else {
    if(length(prtest)==1)
      switch(prtest,
             P=pval,
             stat=format(round(teststat,2)),
             df=dof, name=statname)
    else
      paste(if('stat' %in% prtest)
              paste(statname,'=',format(round(teststat,2)),sep=''),
            if('df' %in% prtest) paste('d.f.=',dof,sep=''),
### 21dec03      if('P' %in%  prtest)paste('P', if(plt)'' else '=', pval,
###                                         sep=''))
            if('P' %in%  prtest)paste('P', ifelse(plt,'','='), pval,
                                      sep=''))
  }
}


latex.summary.formula.reverse <- 
  function(object, title=first.word(deparse(substitute(object))),
           digits, prn = any(n!=N), pctdig=0, 
           npct=c('numerator','both','denominator','none'),
           npct.size='scriptsize', Nsize='scriptsize',
           exclude1=TRUE,  vnames=c("labels","names"), prUnits=TRUE,
           middle.bold=FALSE, outer.size="scriptsize",
           caption, rowlabel="",
           insert.bottom=TRUE, dcolumn=FALSE, formatArgs=NULL, round=NULL,
           prtest=c('P','stat','df','name'), prmsd=FALSE, msdsize=NULL,
           long=dotchart, pdig=3, eps=.001, auxCol=NULL, dotchart=FALSE, ...)
{
  x      <- object
  npct   <- match.arg(npct)
  vnames <- match.arg(vnames)
  if(is.logical(prtest) && !prtest)
    prtest <- 'none'

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
  nw     <- if(lg <- length(x$group.freq)) lg
            else 1  #23Nov98
  gnames <- names(x$group.freq)
  test   <- x$testresults
  if(!length(test))
    prtest <- 'none'

  gt1.test <-
    if(all(prtest=='none'))
      FALSE
    else
      length(unique(sapply(test,function(a)a$testname))) > 1

  if(!missing(digits)) {   #.Options$digits <- digits 6Aug00
    oldopt <- options(digits=digits)
    on.exit(options(oldopt))
  }

  if(missing(caption))
    caption <- paste("Descriptive Statistics",
                     if(length(x$group.label))
                       paste(" by",x$group.label)
                     else
                       paste("  $(N=",x$N,")$",sep=""), sep="")
    
  bld <- if(middle.bold) '\\bf '
         else ''

  cstats <- NULL
  testUsed <- auxc <- character(0)

  for(i in 1:nv) {
    if(length(auxCol))
      auxc <- c(auxc, auxCol[[1]][i])

    nn <- c(nn, n[i])   ## 12aug02
    nam <- if(vnames=="names") nams[i]
           else labels[i]

    if(prUnits && nchar(Units[i]) > 0)
      nam <- paste(nam, '~\\hfill\\tiny{',translate(Units[i],'*',' '),'}',sep='')

    tr  <- if(length(test) && all(prtest!='none')) test[[nams[i]]]
           else NULL

    if(length(test) && all(prtest!='none'))
      testUsed <- unique(c(testUsed, tr$testname))

    if(type[i]==1 || type[i]==3) {
      cs <- formatCats(stats[[i]], nam, tr, type[i],
                       if(length(x$group.freq)) x$group.freq else x$n[i],
                       npct, pctdig, exclude1, long, prtest,
                       latex=TRUE, testUsed=testUsed,
                       npct.size=npct.size,
                       pdig=pdig, eps=eps,
                       footnoteTest=gt1.test, dotchart=dotchart)
      nn <- c(nn, rep(NA, nrow(cs)-1))
    } else cs <- formatCons(stats[[i]], nam, tr, x$group.freq, prmsd,
                            prtest=prtest, formatArgs=formatArgs, round=round,
                            latex=TRUE, testUsed=testUsed,
                            middle.bold=middle.bold,
                            outer.size=outer.size, msdsize=msdsize,
                            pdig=pdig, eps=eps, footnoteTest=gt1.test)
                              
    cstats <- rbind(cstats, cs)
    if(length(auxc) && nrow(cstats) > 1)
      auxc <- c(auxc, rep(NA, nrow(cs)-1))
  }

  lab <- dimnames(cstats)[[1]]
  gl <- names(x$group.freq)
  ##gl <- if(length(gl)) paste(gl, " $(N=",x$group.freq,")$",sep="") else " "
  ## Thanks: Eran Bellin <ebellin@montefiore.org>   3Aug01
  if(!length(gl))
    gl <- " "

  lab <- sedit(lab,c(" ","&"),c("~","\\&"))  #was format(lab) 21Jan99
  lab <- latexTranslate(lab, greek=.R.)
  gl  <- latexTranslate(gl, greek=.R.)
  ## if(any(gl != " ")) gl <- paste(gl, " $(N=",x$group.freq,")$",sep="") # 3Aug01
  ## Added any( ) 26Mar02  21jan03
  extracolheads <-
    if(any(gl != " "))
      c(if(prn)'', paste('$N=',x$group.freq,'$',sep=''))
    else NULL # 21jan03

  if(length(test) && !all(prtest=='none')) {
    gl <- c(gl,
            if(length(prtest)==1 && prtest!='stat')
              if(prtest=='P') 'P-value'
              else prtest
            else 'Test Statistic')

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

  if(!insert.bottom)
    legend <- NULL
  else {
    legend <- character()
    if(any(type==2)) {
      legend <- paste("\\noindent {\\", outer.size, " $a$\\ }{", bld,
                      "$b$\\ }{\\", outer.size,
                      " $c$\\ } represent the lower quartile $a$, the median $b$, and the upper quartile $c$\\ for continuous variables.",
                      if(prmsd) '~~$x\\pm s$ represents $\\bar{X}\\pm 1$ SD.'
                      else '',
                      sep="")
    }
    
    if(prn) {
      legend <- c(legend, '$N$\\ is the number of non--missing values.')
    }

    if(any(type==1) && npct=='numerator') {
      legend <- c(legend, 'Numbers after percents are frequencies.')
    }
      
    if(length(testUsed))
      legend <-c(legend,
                 if(length(testUsed)==1)'\\noindent Test used:'
                 else '\\indent Tests used:',
                 if(length(testUsed)==1) paste(testUsed,'test')
                 else paste(paste('\\textsuperscript{\\normalfont ',1:length(testUsed),'}',testUsed,
                                  ' test',sep=''),collapse='; '))

    ## added rowname=lab 12aug02  added '\n\n' 4mar03 for ctable=T
  }

  if(length(auxc)) {
    if(length(auxc) != nrow(cstats))
      stop(paste('length of auxCol (',length(auxCol[[1]]),
                 ') is not equal to number or variables in table (',
                 nv,').', sep=''))
    auxcc <- format(auxc)
    auxcc[is.na(auxc)] <- ''
    cstats <- cbind(auxcc, cstats)
    nax <- names(auxCol)
    heads <- get2rowHeads(nax)
    names(cstats)[1] <- heads[[1]]
    if(length(col.just)) col.just <- c('r', col.just)
    if(length(extracolheads)) extracolheads <- c(heads[2], extracolheads)
  }
  resp <- latex.default(cstats, title=title, caption=caption, rowlabel=rowlabel,
                        col.just=col.just, numeric.dollar=FALSE, 
                        insert.bottom=legend,  rowname=lab, dcolumn=dcolumn,
                        extracolheads=extracolheads, extracolsize=Nsize,
                        ...)

  if(dotchart) 
    resp$style <- unique(c(resp$style, 'calc', 'epic', 'color'))
  
  resp
}


print.summary.formula.cross <- function(x, twoway=nvar==2, 
                                        prnmiss=any(stats$Missing>0), prn=TRUE,
                                        abbreviate.dimnames=FALSE, 
                                        prefix.width=max(nchar(v)),
                                        min.colwidth, formatArgs=NULL,
                                        ...)
{
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
    if(!length(z))
      z <- ylab

    z <- c(if(prn)"N",
           if(prnmiss)"Missing",
           z)  # 5Oct00
    
    header <- matrix(paste(z,collapse="\n"),1,1)
    if(.R.)
      print.char.matrix(header, col.names=FALSE)
    else
      print.char.matrix(header)

    d <- c(length(v),length(h),length(z))
    st <- array(NA, dim=d, dimnames=list(v,h,z))
    cstats <- array("", dim=d, dimnames=list(v,h,z))
    for(i in 1:length(V)) {
      j <- V==V[i,drop=FALSE] & H==H[i,drop=FALSE]
      st[V[i,drop=FALSE],H[i,drop=FALSE],] <-
        c(if(prn)stats$N[j],
          if(prnmiss)stats$Missing[j],
          if(ars)S[j,]
          else S[j])  # 5Oct00
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
    if(missing(min.colwidth))
      min.colwidth <- 
        max(min(nchar(dimnames(cstats2)[[2]])), 
            min(nchar(cstats)[nchar(cstats)>0]))

    return(invisible(if(.R.)
                       print.char.matrix(cstats2,
                                         col.names=TRUE, ...)
                     else
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
    for(i in 1:ncol(S))
      stats[[snam[i]]] <- S[,i]
    
  } else names(stats)[length(stats)] <- ylab

  stats <- as.data.frame(stats)
  invisible(print(stats, ...))
}


latex.summary.formula.cross <-
  function(object,
           title=first.word(deparse(substitute(object))),
           twoway=nvar==2,
           prnmiss=TRUE, prn=TRUE,
           caption=attr(object,"heading"), vnames=c('labels','names'),
           rowlabel="", ...)
{  
  stats <- object
  vnames <- match.arg(vnames)
  ul <- vnames=='labels'

  stats <- oldUnclass(stats)
  a <- attributes(stats)
  ylab <- attr(stats$S,"label")
  nvar <- length(a$Levels)
  nam <- c(names(a$Levels),
           if(prn)"N",
           if(prnmiss)"Missing",
           "S")
  
  ##Force lazy evaluation since stats about to change
  caption <- caption;
  title <- title
  stats <- stats[nam]
  S <- stats$S
  ars <- length(dim(S))
  inn <- c('cbind','c(','ALL',  'NA')
  out <- c('',     '(' ,'Total','Missing')
  caption <- latexTranslate(caption, inn, out, pb=TRUE, greek=.R.)

  if(twoway)
    rowlab <-
      if(ul)
        latexTranslate(a$labels[1],inn,out,pb=TRUE,greek=.R.)
      else 
        names(stats)[1]

  rvar <- stats[[1]]
  cvar <- stats[[2]]
  lev1 <- levels(rvar)
  lev2 <- levels(cvar)
  if(!twoway) {
    for(i in 1:nvar)
      stats[[i]] <- latexTranslate(as.character(stats[[i]]),inn,
                                   out,pb=TRUE,greek=.R.)

    ##Used to do this translating unconditionally   6Jun96

    if(ars) {
      stats$S <- NULL
      snam <- latexTranslate(dimnames(S)[[2]],inn,out,pb=TRUE,greek=.R.)
      for(i in 1:ncol(S))
        stats[[snam[i]]] <- S[,i]
    } else names(stats)[length(stats)] <- ylab

    stats <- structure(stats, row.names=rep("",length(stats$N)),
                       class="data.frame")
    if(hasArg(col.just)) {
      return(latex(stats, title=title, caption=caption, rowlabel=rowlabel, ...))
    } else return(latex(stats, title=title, caption=caption, rowlabel=rowlabel, 
                        col.just=c("l","l",rep("r",length(stats)-2)), ...))
  }

  ##Two-way
  S <- cbind(N=if(prn)stats$N,
             Missing=if(prnmiss && any(stats$Missing)) stats$Missing,  #5Oct00
             stats$S)
  
  nr <- length(lev1)
  nc <- length(lev2)
  ns <- ncol(S)
  snam <- dimnames(S)[[2]]
  snam <- latexTranslate(snam, inn, out, pb=TRUE,greek=.R.)
  dn <-
    if(ns > 1)
      rep(snam, nc)
    else
      latexTranslate(lev2,inn,out,pb=TRUE,greek=.R.) # 5Oct00

  st <- matrix(NA, nrow=nr, ncol=nc*ns, dimnames=list(NULL,dn))
  for(i in 1:nr) {
    l <- 0
    for(j in 1:nc) {
      w <- rvar==lev1[i] & cvar==lev2[j]
      if(any(w))
        for(k in 1:ns) {
          l <- l+1
          st[i,l] <- S[w,k]
        }
    }
  }

  latex(st, title=title, caption=caption, 
        rowlabel=if(rowlabel=='') rowlab else rowlabel,
        n.rgroup=c(nrow(st)-1,1),
        n.cgroup=if(ns>1) rep(ns,nc),  # ns>1 5Oct00
        cgroup  =if(ns>1) latexTranslate(lev2,inn,out,pb=TRUE,greek=.R.),
        check.names=FALSE,
        rowname=latexTranslate(lev1,inn,out,pb=TRUE,greek=.R.), ...)
}


##stratify is a modification of Therneau's survival4 strata function
##Saves label attributute and defaults shortlabel to T
stratify <- function(..., na.group = FALSE, shortlabel = TRUE)
{
  words <- as.list((match.call())[-1])
  if(!missing(na.group))
    words$na.group <- NULL

  if(!missing(shortlabel))
    words$shortlabel <- NULL

  allf <- list(...)
  
  if(length(allf) == 1 && is.list(ttt <- oldUnclass(allf[[1]]))) {
    allf <- ttt
    words <- names(ttt)
  }
  
  xlab <- sapply(allf, function(x){lab <- valueLabel(x); if(is.null(lab)) NA else lab})
  xname <- sapply(allf, function(x){name <- valueName(x); if(is.null(name)) NA else name})

  xname <- ifelse(is.na(xname), words, xname)
  xlab <- paste(ifelse(is.na(xlab), xname, xlab), collapse=' and ')
  
  xname <- paste(xname, collapse = ' and ')

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
    labs <- paste(rep(labs, rep(length(wlab), length(labs))),
                  rep(wlab, length(labs)), sep = ", ")
  }

  levs <- levs + 1
  ulevs <- sort(unique(levs[!is.na(levs)]))
  levs <- match(levs, ulevs)
  labs <- labs[ulevs]
  levels(levs) <- labs
  oldClass(levs) <- "factor"

  if(length(xlab))
    valueLabel(levs) <- xlab   #FEH 2Jun95

  if(length(xname))
    valueName(levs) <- xname
  
  levs
}


'[.summary.formula.response' <- function(x,i,j,drop=FALSE)
{
  at <- attributes(x)
  at$dim <- at$dimnames <- NULL

  if(!missing(j)) {
    x <- oldUnclass(x)[,j,drop=FALSE]
    at$ycolname <- at$ycolname[j]
    attributes(x) <- c(attributes(x), at)
  }

  if(missing(i))
    return(x)

  if(is.character(i)) {
    vn <- at$vname[at$vname!='']
    k <- match(i, vn, nomatch=0)
    if(any(k==0))
      stop(paste('requested variables not in object:',
                 paste(i[k==0],collapse=' ')))

    i <- k
  }

  j <- integer(0)
  nl <- at$nlevels
  is <- 1
  for(m in 1:length(nl)) {
    ie <- is+nl[m]-1
    if(any(i==m))
      j <- c(j,is:ie)

    is <- ie+1
  }

  at$vname   <- at$vname[j]
  at$vlabel  <- at$vlabel[j]
  at$nlevels <- at$nlevels[i]
  at$labels  <- at$labels[i]

  x <- oldUnclass(x)[j,,drop=FALSE]
  attributes(x) <- c(attributes(x), at)
  x
}


cumcategory <- function(y)
{
  if(!is.category(y))
    y <- factor(y)  ## was as.category 26Mar02

  lev <- levels(y)
  y <- oldUnclass(y)
  Y <- matrix(NA, nrow=length(y), ncol=length(lev)-1,
              dimnames=list(NULL,paste('>=',lev[-1],sep='')))
  storage.mode(Y) <- 'integer'
  for(i in 2:length(lev))
    Y[,i-1] <- 1*(y >= i)

  Y
}


summarize <- function(X, by, FUN, ..., 
                      stat.name=deparse(substitute(X)), 
                      type=c('variables','matrix'), subset=TRUE)
{
  type <- match.arg(type)
  if(missing(stat.name) && length(stat.name)>1)
    stat.name <- 'X'

  if(!is.list(by)) {
    nameby <- deparse(substitute(by))
    bylabel <- label(by)
    by <- list(by[subset])
    names(by) <- if(length(nameby)==1) nameby
                 else 'by'
  } else {
    bylabel <- sapply(by, label)
    if(!missing(subset))
      by <- lapply(by, function(y, subset) y[subset],
                   subset=subset)
  }

  nby <- length(by)
  
  bylabel <- ifelse(bylabel=='', names(by), bylabel)
  typical.computation <- FUN(X, ...)
  nc <- length(typical.computation)
  xlabel <- deparse(substitute(X))
  if(length(xlabel)!=1)
    xlabel <- 'X'
  
  if(length(xlab <- attr(X,'label')))
    xlabel <- xlab

  if(!missing(subset))
    X <- if(is.matrix(X)) X[subset,,drop=FALSE]
         else X[subset]

  if(!.R.)  # 21Mar01: S-Plus converts factor to integer during paste
    for(i in 1:nby)
      if(is.category(by[[i]]))
        by[[i]] <- as.character(by[[i]])
  
  byc <- do.call('paste',c(by,sep='|'))

  ## split does not handle matrices
  ##  msplit <- function(x, group) {
  ##    if(is.matrix(x)) {
  ##      group <- as.factor(group)
  ##      l <- levels(group)
  ##      res <- vector('list', length(l))
  ##      names(res) <- l
  ##      for(j in l) res[[j]] <- x[group==j,,drop=FALSE]
  ##      res
  ##    } else split(x, group)
  ##  }
  ## Following was streamlined 10oct02 using the new mApply
  ##  if(nc==1) r <- sapply(msplit(X, byc), FUN, ..., simplify=TRUE) else {
  ##    r <- sapply(msplit(X, byc), FUN, ..., simplify=TRUE)
  ##    r <- matrix(unlist(r), nrow=nc, dimnames=dimnames(r))
  ## 2Mar00: added unlist because sapply was creating an array of
  ## lists in S+2000
  ##  }

  r <- mApply(X, byc, FUN, ..., keepmatrix=nc>1)
  rdimn <- dimnames(r)[[1]]
  if(.R.) {   # someday can use unpaste defined in Misc.s
    ans <- strsplit(if(nc==1) names(r) else rdimn,'\\|')
    ans <- sapply(ans, function(x)if(length(x))x else '')

    ## strsplit returns list "transpose" of unpaste
    bb <- matrix(unlist(ans), nrow=nby)
    ans <- vector('list', nby)
    for(jj in 1:nby)
      ans[[jj]] <- bb[jj,]
  } else {
    ans <- if(nc==1)names(r) else rdimn
    
    if(nby==1)
      ans <- list(ans)
    else
      ans <- unPaste(ans, sep='|')
  }

  names(ans) <- names(by)
  if(nc>1 && (nc != ncol(r)))
    stop('program logic error')  # was nrow 10oct02
  
  snames <- names(typical.computation)
  if(!length(snames))
    snames <- paste(stat.name,1:nc,sep='')
  
  if(length(stat.name)==1)
    snames[1] <- stat.name
  else if(length(stat.name))
    snames <- stat.name
  
  oldopt <- options(warn=-1)
  on.exit(options(oldopt))
  notna <- rep(TRUE, length(ans[[1]]))
  for(i in 1:length(by)) {
    byi <- by[[i]]
    ansi <- ans[[i]]
    if(is.category(byi)) {
      if(!is.character(ansi))
        stop('program logic error:ansi not character')
      
      ansi <- factor(ansi, levels(byi))
    }
    else if(is.numeric(byi))
      ansi <- as.numeric(ansi)
    
    names(ansi) <- NULL
    label(ansi) <- bylabel[i]
    ans[[i]] <- ansi
    notna <- notna & !is.na(ansi)
  }

  if(type=='matrix' || nc==1) {
    ans[[stat.name]] <-
      if(nc==1)
        structure(r,names=NULL)
      else 
        structure(r, dimnames=list(NULL, snames), names=NULL)

    label(ans[[stat.name]]) <- xlabel
  } else {
    snames <- make.names(snames)
    for(i in 1:length(snames)) {
      ans[[snames[i]]] <- structure(r[,i], names=NULL)
      label(ans[[snames[i]]]) <- xlabel
    }
  }

  notna <- notna & !is.na(if(nc==1) r
                          else (r %*% rep(1,nc)))
  
  ans <- structure(ans, class='data.frame', 
                   row.names=1:length(ans[[1]]))
  ## removed [notna,] from end of above line; not sure why this was needed
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

smean.cl.normal <- function(x, mult=qt((1+conf.int)/2,n-1),
                            conf.int=.95, na.rm=TRUE)
{
  if(na.rm) x <- x[!is.na(x)]
  n <- length(x)
  if(n < 2)
    return(c(Mean=mean(x),Lower=NA,Upper=NA))
  
  xbar <- sum(x)/n
  se <- sqrt(sum((x - xbar)^2) / n / (n-1))
  c(Mean=xbar, Lower=xbar - mult*se, Upper=xbar + mult*se)
}


smean.sd <- function(x, na.rm=TRUE)
{
  if(na.rm)
    x <- x[!is.na(x)]
  
  n <- length(x)
  if(n == 0)
    return(c(Mean=NA, SD=NA))
  
  xbar <- sum(x)/n
  sd <- sqrt(sum((x - xbar)^2)/(n-1))
  c(Mean=xbar, SD=sd)
}


smean.sdl <- function(x, mult=2, na.rm=TRUE)
{
  if(na.rm)
    x <- x[!is.na(x)]
  
  n <- length(x)
  if(n == 0)
    return(c(Mean=NA, Lower=NA, Upper=NA))
  
  xbar <- sum(x)/n
  sd <- sqrt(sum((x - xbar)^2)/(n-1))
  c(Mean=xbar, Lower=xbar - mult * sd, Upper=xbar + mult * sd)
}


#S-Plus gives a parse error for R's .Internal()
#Might try not using an else to see if S still parses
smean.cl.boot <- if(.R.) {
  eval(parse(text=paste(c('function(x, conf.int=.95, B=1000, na.rm=TRUE, reps=FALSE) {',
                          'if(na.rm) x <- x[!is.na(x)]',
                          'n <- length(x)',
                          'xbar <- mean(x)',
                          'if(n < 2) return(c(Mean=xbar, Lower=NA, Upper=NA))',
                          'z <- unlist(lapply(1:B, function(i,x,N)',
                          'sum(x[.Internal(sample(N, N, TRUE, NULL))]),',
                          'x=x, N=n)) / n',
                          'quant <- quantile(z, c((1-conf.int)/2,(1+conf.int)/2))',
                          'names(quant) <- NULL',
                          'res <- c(Mean=xbar, Lower=quant[1], Upper=quant[2])',
                          'if(reps) attr(res,"reps") <- z',
                          'res}'),
                        collapse='\n')))

} else function(x, conf.int=.95, B=1000, na.rm=TRUE, reps=FALSE)
{
  if(na.rm)
    x <- x[!is.na(x)]

  n <- length(x)
  xbar <- mean(x)
  if(n < 2)
    return(c(Mean=xbar, Lower=NA, Upper=NA))

  z <- unlist(lapply(1:B, function(i,x,N)
                     sum(x[.Internal(sample.index(N, N, TRUE),
                                     "S_sample",TRUE,0)]), x=x, N=n)) / n
  quant <- quantile(z, c((1-conf.int)/2,(1+conf.int)/2))
  names(quant) <- NULL
  res <- c(Mean=xbar, Lower=quant[1], Upper=quant[2])
  if(reps)
    attr(res, 'reps') <- z

  res
}


smedian.hilow <- function(x, conf.int=.95, na.rm=TRUE)
{
  quant <- quantile(x, probs=c(.5,(1-conf.int)/2,(1+conf.int)/2), na.rm=na.rm)
  names(quant) <- c('Median','Lower','Upper')
  quant
}


asNumericMatrix <- function(x)
{
  a <- attributes(x)
  k <- length(a$names)
  at <- vector('list', k); names(at) <- a$names
  for(i in 1:k) {
    xi <- x[[i]]
    ischar <- FALSE
    A <- attributes(xi)
    if(is.character(xi)) {
      ischar <- TRUE
      xi <- factor(xi)
      A <- c(A, attributes(xi))
      x[[i]] <- xi
    }
    A$dim <- A$names <- A$dimnames <- NULL
    A$ischar <- ischar
    at[[i]] <- A
  }
  assign('origAttributes', at, pos=if(.R.)'.GlobalEnv' else 1)
  matrix(unlist(x), ncol=k,
         dimnames=list(a$row.names, a$names))
}


matrix2dataFrame <- function(x, at=origAttributes, restoreAll=TRUE)
{
  d <- dimnames(x)
  k <- length(d[[2]])
  w <- vector('list',k)
  nam <- names(w) <- d[[2]]
  sm <- storage.mode(x)
  
  for(i in 1:k) {
    a <- at[[nam[i]]]
    isc <- a$ischar
    if(!length(a))
      next

    xi <- x[,i]
    names(xi) <- NULL
    if(restoreAll) {
      a$ischar <- NULL
      if(isc) {
        xi <- as.character(xi)
        a$levels <- NULL
        if(length(a$class)) a$class <- setdiff(a$class, 'factor')
      }
      if('factor' %in% a$class) storage.mode(xi) <- 'integer'
      ## R won't let something be assigned class factor by brute
      ## force unless it's an integer object
      attributes(xi) <- a
    } else {
      if(length(l   <- a$label))
        label(xi) <- l
      
      if(length(u   <- a$units))
        units(xi) <- u
      
      if(length(lev <- a$levels)) {
        xi <- factor(xi, 1:length(lev), lev)
        if(isc) xi <- as.character(xi)
      }
    }
    
    w[[i]] <- xi
  }
  rn <- d[[1]]
  if(!length(rn)) rn <- as.character(seq(along=xi))
  structure(w, class='data.frame', row.names=rn)
}


stripChart <- function(x, xlim, xlab='', pch=1,
                       cex.labels=par('cex'), cex.points=.5,
                       lcolor=if(.R.)'gray' else par('col'),
                       grid=FALSE)
{
  groups <- names(x)
  if(missing(xlim))
    xlim <- range(unlist(x),na.rm=TRUE)
  
  i <- integer(0)

  if(grid) {
    lines <- llines;
    points <- lpoints;
    segments <- lsegments
  }

  if(.R.)
    plot.new()
  
  mai <- omai <- par('mai')
  on.exit(par(mai=omai))
  mxlab <- .3+max(strwidth(groups, units='inches', cex=cex.labels))
  mai[2] <- mxlab
  par(mai=mai, new=TRUE)
  
  plot(xlim, c(.5,length(groups)+.5), xlim=xlim, xlab='', ylab='',
       axes=FALSE, type='n')
  box()
  mgp.axis(1, axistitle=xlab)

  if(.R.)
    mtext(paste(groups,''), 2, 0, at=length(groups):1,
          adj=1, las=1, cex=cex.labels)
  else
    mtext(paste(groups,''), 2, 0, at=length(groups):1,
          adj=1, srt=0, cex=cex.labels)

  y <- 0
  abline(h = 1:length(groups), lty = 1, lwd=1, col=lcolor)

  for(Y in length(groups):1) {
    y <- y + 1
    X <- x[[y]]
    if(length(X))
      points(X, rep(Y, length(X)), pch=pch)
  }
}
