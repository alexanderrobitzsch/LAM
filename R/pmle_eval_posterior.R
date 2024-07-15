## File Name: pmle_eval_posterior.R
## File Version: 0.251


#*** evaluate posterior
pmle_eval_posterior <- function( data, model, prior, pars, eps=1E-100 )
{
    NP <- length(pars)
    #--- evaluate log-likelihood function
    ll <- do.call( model, list( pars=pars, data=data )  )
    #--- evaluate prior distributions
    prior1 <- 0
    LP <- length(prior)
    if (LP>0){
        for (pp in 1L:NP){
            prior_arg_pp <- prior[[pp]][[2]]
            prior_arg_pp[[1]] <- pars[pp]
            priorval <- log( do.call( what=prior[[pp]][[1]], args=prior_arg_pp ) + eps )
            prior1 <- prior1 + priorval
        }
    }
    names(prior1) <- NULL
    #--- compute objective function (posterior)
    post <- ll + prior1
    #--- output
    res <- list( ll=ll, prior=prior1, post=post )
    return(res)
}

