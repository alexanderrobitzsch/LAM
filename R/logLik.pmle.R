## File Name: logLik.pmle.R
## File Version: 0.04
## File Last Change: 2018-05-18

logLik.pmle <- function (object, ...)
{
    out <- logLik_lam(object, ...)
    return(out)
}
