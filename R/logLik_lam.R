## File Name: logLik_lam.R
## File Version: 0.01
## File Last Change: 2018-05-18


logLik_lam <- function (object, ...)
{
    # extract log-likelihood
    out <- - object$ic$deviance / 2
    # number of parameters
    attr(out, "df") <- object$ic$np
    # extract number of observations
    attr(out, "nobs") <- object$ic$n
    class(out) <- "logLik"
    return(out)
}
