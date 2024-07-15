## File Name: amh_eval_priors.R
## File Version: 0.051


amh_eval_priors <- function( pars, prior )
{
    NP <- length(pars)
    priorval0 <- 0
    eps <- 1E-100
    for (pp in 1L:NP){
        #*** evaluate
        pars_pp <- names(pars)[pp]
        prior_arg_pp <- prior[[pp]][[2]]
        prior_arg_pp[[1]] <- pars[pp]
        priorval_pp <- log( do.call( prior[[pp]][[1]], prior_arg_pp ) + eps )
        priorval0 <- priorval0 + priorval_pp
    }
    return(priorval0)
}
