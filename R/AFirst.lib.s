## $Id: AFirst.lib.s,v 1.2 2004/06/01 02:30:59 harrelfe Exp $
under.unix   <- !(version$os=='Microsoft Windows' ||
                  version$os=='Win32' || version$os=='mingw32')

.R.          <- TRUE
.SV4.        <- FALSE

.noGenenerics <- TRUE  # faster loading as new methods not used

if(!exists('existsFunction')) existsFunction <- function(...)
  exists(..., mode='function')

.First.lib <- function(lib, pkg, verbose=TRUE, ...) {
  if(verbose)
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
