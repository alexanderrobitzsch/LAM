## File Name: amh_sampling.R
## File Version: 0.393
## File Last Change: 2020-05-07


#--- sampling step in amh routine
amh_sampling <- function(pars, data, model, prior,
                proposal_sd, acceptance_parameters,
                pars_lower, pars_upper, pmle_pars, eps=1E-100, it=NULL,
                boundary_ignore=FALSE)
{
    NP <- length(pars)
    priorval0 <- 0
    for (pp in 1:NP){
        pars_old <- pars
        #*** evaluate posterior for old parameters
        pars_pp <- names(pars)[pp]
        ll_old <- do.call( model, list( pars=pars_old, data=data ) )
        prior_arg_pp <- prior[[pp]][[2]]
        prior_arg_pp[[1]] <- pars_old[pp]
        priorval_pp_old <- log( do.call( prior[[pp]][[1]], prior_arg_pp ) + eps )
        #*** evaluate posterior for new parameters
        pars_pp_new <- stats::rnorm( 1, mean=pars[pp], sd=proposal_sd[pp] )

        #* handle sampled values at boundaries
        res <- amh_sampling_boundary_values(pars_pp_new=pars_pp_new, pp=pp,
                pars_lower=pars_lower,    pars_upper=pars_upper, pars_old=pars_old,
                boundary_ignore=boundary_ignore)
        reject <- res$reject
        pars_pp_new <- res$pars_pp_new

        pars_new <- pars
        pars_new[pp] <- pars_pp_new
        ll_new <- do.call( model, list( pars=pars_new, data=data ) )
        prior_arg_pp <- prior[[pp]][[2]]
        prior_arg_pp[[1]] <- pars_new[pp]
        priorval_pp_new <- log( do.call( prior[[pp]][[1]], prior_arg_pp )  + eps )
        # calculate sampling probability
        mh_prob <- exp( ( ll_new + priorval_pp_new ) -  ( ll_old + priorval_pp_old ) )
        # acceptance or rejection
        rand <- stats::runif(1)
        if (reject){
            mh_prob <- 0
        }
        if ( rand < mh_prob ){
            accept <- 1
            pars <- pars_new
            dev <- -2*ll_new
            ll <- ll_new
            priorval0 <- priorval0 + priorval_pp_new
        } else {
            accept <- 0
            pars <- pars_old
            dev <- -2*ll_old
            ll <- ll_old
            priorval0 <- priorval0 + priorval_pp_old
        }
        acceptance_parameters[ pp, 1:2] <- acceptance_parameters[ pp, 1:2] + c( accept, 1 )
    }  # end pp
    #---- posterior update
    posteriorval <- ll + priorval0
    if ( pmle_pars$posteriorval < posteriorval ){
        pmle_pars$pars <- pars
        pmle_pars$posteriorval <- posteriorval
        pmle_pars$loglik <- ll
        pmle_pars$logprior <- priorval0
    }
    #--- output
    res <- list( pars=pars, acceptance_parameters=acceptance_parameters,
                        deviance=dev, loglik=ll, priorval=priorval0,
                        posteriorval=posteriorval, pmle_pars=pmle_pars)
    return(res)
}
