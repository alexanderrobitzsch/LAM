## File Name: pmle_eval_prior.R
## File Version: 0.03
## File Last Change: 2018-11-23

pmle_eval_prior <- function(prior, pars, eps=1e-100)
{
    NP <- length(pars)
    priorval <- rep(NA, NP)
    eps <- 1e-100
    for (pp in 1:NP){
        prior_arg_pp <- prior[[pp]][[2]]
        prior_arg_pp[[1]] <- pars[pp]
        priorval[pp] <- log( do.call( prior[[pp]][[1]], prior_arg_pp )  + eps )
    }
    return(priorval)
}
