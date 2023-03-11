## File Name: amh_posterior.R
## File Version: 0.07
## File Last Change: 2018-11-23
amh_posterior <- function( pars, model, prior, data)
{
    #*** evaluate likelihood
    ll <- amh_loglike( model=model, data=data, pars=pars)
    #*** evaluate priors
    priorval <- amh_eval_priors( pars=pars, prior=prior )
    #*** compute posterior
    posteriorval <- ll + priorval
    #--- output
    res <- list( ll=ll, priorval=priorval,
                posteriorval=posteriorval    , pars=pars )
    return(res)
}
