## File Name: logLik.amh.R
## File Version: 0.04

logLik.amh <- function (object, ...)
{
    out <- logLik_lam(object, ...)
    return(out)
}
