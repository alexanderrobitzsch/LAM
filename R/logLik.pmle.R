
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
