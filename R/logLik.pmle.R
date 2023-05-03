## File Name: logLik.pmle.R
## File Version: 0.04

logLik.pmle <- function (object, ...)
{
    out <- logLik_lam(object, ...)
    return(out)
}
