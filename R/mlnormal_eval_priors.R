## File Name: mlnormal_eval_priors.R
## File Version: 0.14

mlnormal_eval_priors <- function( pars , prior, sum_all = FALSE ){
    NP <- length(pars)
    priorval0 <- rep(NA,NP)
    eps <- 1E-100
    for (pp in 1:NP){
        #*** evaluate
        pars_pp <- names(pars)[pp]
        prior_pp <- prior[[ pars_pp ]]
        prior_arg_pp <- prior_pp[[2]]
        prior_arg_pp[[1]] <- pars[pp]
        priorval_pp <- log( do.call( prior_pp[[1]], prior_arg_pp ) + eps )
        priorval0[pp] <- priorval_pp
    }
    if ( sum_all ){
        priorval0 <- sum(priorval0)
    }
    return(priorval0)
}
