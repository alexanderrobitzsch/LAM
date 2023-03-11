## File Name: logLik.mlnormal.R
## File Version: 0.04
## File Last Change: 2018-05-18

logLik.mlnormal <- function (object, ...)
{
    out <- logLik_lam(object, ...)
    return(out)
}
