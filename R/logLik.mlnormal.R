## File Name: logLik.mlnormal.R
## File Version: 0.04

logLik.mlnormal <- function (object, ...)
{
    out <- logLik_lam(object, ...)
    return(out)
}
