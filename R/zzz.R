## File Name: zzz.R
## File Version: 0.07
## File Last Change: 2018-11-23
#  zzz.R
#
# This function is simply copied from the mice package.


# on attach sirt
.onAttach <- function(libname,pkgname)
{
    d <- utils::packageDescription("LAM")
    d1 <- d$Version
    packageStartupMessage(
        paste("## ", d$Package," ", d1," (",d$Date,")",sep="")  )
}


version <- function(pkg="LAM")
{
    lib <- dirname( system.file(package=pkg))
    d <- utils::packageDescription(pkg)
    return( paste(d$Package,d$Version,d$Date,lib))
}

# .First.lib <- function(lib, pkg){
#          library.dynam("sirt", package=pkg, lib.loc=lib)
#          return(invisible(0))
#        }
