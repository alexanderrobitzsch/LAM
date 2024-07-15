## File Name: pmle.R
## File Version: 0.612


#-- penalized maximum likelihood estimation
pmle <- function( data, nobs, pars, model, prior=NULL, model_grad=NULL,
        pars_lower=NULL, pars_upper=NULL, method="L-BFGS-B",
        control=list(), verbose=TRUE, hessian=TRUE,
        optim_fct="nlminb", h=1e-4, ...)
{

    time <- list( 'start'=Sys.time() )
    CALL <- match.call()

    #*** convert prior if needed
    res <- pmle_process_prior( prior=prior, pars=pars )
    prior <- res$prior
    dens <- res$dens

    #*** data processing
    res <- pmle_data_proc( pars=pars, pars_lower=pars_lower, pars_upper=pars_upper )
    pars_lower <- res$pars_lower
    pars_upper <- res$pars_upper

    #*** define objective function
    pmle_obj <- function(x){
        res <- - pmle_eval_posterior( data=data, model=model,
                       prior=prior, pars=x )$post
        return(res)
    }

    #*** define likelihood function for computing standard errors
    pmle_ll <- function(x){
        prior0 <- list()
        res <- - pmle_eval_posterior( data=data, model=model,
                       prior=prior0, pars=x )$post
        return(res)
    }

    #*** define derivative if provided
    pmle_grad <- NULL
    use_grad <- FALSE
    if ( ! is.null(model_grad)){
        use_grad <- TRUE
        pmle_grad <- function(x){
            val1 <- - model_grad(pars=x)
            val2 <- - pmle_eval_prior_deriv(prior=prior, pars=x, h=h)
            res <- val1 + val2
            return(res)
        }
    }

    #**** start optimization function
    if (verbose){
        cat('***************************\n')
        cat('Starting Optimization\n\n')
        #- adapt trace
        if ( is.null(control$trace) ){
            if (optim_fct=='optim'){
                control$trace <- 3
            }
            if (optim_fct=='nlminb'){
                control$trace <- 1
            }
        }
        utils::flush.console()
    }

    #--- optimization
    if ( optim_fct=='optim'){
        res0 <- stats::optim( par=pars, fn=pmle_obj, gr=pmle_grad, method=method,
                    lower=pars_lower, upper=pars_upper,
                    control=control, hessian=hessian, ... )
    }
    if ( optim_fct=='nlminb'){
        requireNamespace('numDeriv')
        res0 <- stats::nlminb( start=pars, objective=pmle_obj, gradient=pmle_grad,
                    lower=pars_lower, upper=pars_upper,
                    control=control, ... )
    }
    converged <- res0$convergence==0
    coef1 <- res0$par
    if (hessian){
        requireNamespace('MASS')
        #-- compute Hessian matrix
        res0$hessian <- numDeriv::hessian(func=pmle_ll, x=res0$par)
        hess1 <- res0$hessian
        vcov1 <- MASS::ginv(X=hess1)
    } else {
        vcov1 <- hess1 <- NULL
    }
    if (verbose){
        cat('\n***************************\n')
        utils::flush.console()
    }

    # summary
    pmle_summary <- data.frame( 'parameter'=names(pars), 'est'=coef1 )
    rownames(pmle_summary) <- NULL
    if ( hessian ){
        pmle_summary$se <- sqrt( diag(vcov1))
        pmle_summary$t <- pmle_summary$est / pmle_summary$se
        pmle_summary$p <- 2* stats::pnorm( - abs( pmle_summary$t ) )
    }
    pmle_summary$active <- 1 - ( ( coef1==pars_lower ) | ( coef1==pars_upper ) )

    #*** evaluate log-likelihood
    res2 <- pmle_eval_posterior( data=data, model=model,
                prior=prior,  pars=coef1 )
    ll <- res2$ll

    #*** compute information criteria
    ic <- pmle_ic( dev=-2*ll, N=nobs, pars=coef1,
                model=model, data=data, post_values=res2 )
    time$end <- Sys.time()

    #**** output list
    res <- list( pmle_summary=pmle_summary, coef=coef1, vcov=vcov1, hessian=hess1,
        loglik=ll, ic=ic, deviance=-2*ll, model=model, prior=prior, prior_summary=dens,
        data=data, nobs=nobs, results_optim=res0, converged=converged,
        time=time, CALL=CALL, optim_fct=optim_fct, use_grad=use_grad )
    v1 <- paste0( 'Penalized Maximum Likelihood Estimation \n '    ,
                '   (Maximum Posterior Estimation, MAP)' )
    res$description <- v1
    class(res) <- 'pmle'
    return(res)
}
