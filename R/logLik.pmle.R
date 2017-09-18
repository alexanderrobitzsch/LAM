## File Name: logLik.pmle.R
## File Version: 0.02
## File Last Change: 2017-01-18 11:02:48

logLik.pmle <- function (object, ...) {
    # extract log-likelihood
    out <- - object$ic$deviance / 2 
    # number of parameters
    attr(out, "df") <- object$ic$np
        # extract number of observations
    attr(out, "nobs") <- object$ic$n
    class(out) <- "logLik"
    return(out)
}
