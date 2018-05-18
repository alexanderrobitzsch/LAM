## File Name: mlnormal_fit_function_ml.R
## File Version: 0.22


mlnormal_fit_function_ml <- function( id_list , V1_list , G , N , yresid ,
            V_list , REML , XVX , do_compute , prior_args , theta , beta ,
            objfun0  , theta_increment , theta_change
            )
{

    do_iterate <- TRUE
    it <- 1

    while (do_iterate){
        ll <- - mlnormal_log_2pi() * N/2
        for (gg in 1:G){
            # gg <- 1
            if ( do_compute[gg] ){
                adet <- 0.5 * mlnormal_log_det( V_list[[gg]] )
            }
            ll <- ll - adet
            ind <- id_list[[gg]]
            yr <- yresid[ind]
            ll <- ll - 0.5 * crossprod( yr , V1_list[[gg]] ) %*% yr
        }
        if (REML){
            ll <- ll - mlnormal_log_det( XVX )
        }
        ll <- as.vector(ll)
        crit <- ll
        log_prior <- log_posterior <- NA

        #-------- priors | evaluate posterior
        if ( prior_args$use_prior | prior_args$use_penalty ){

            if ( prior_args$use_prior ){
                logprior_theta <- mlnormal_eval_priors( pars = theta ,
                            prior = prior_args$prior ,    sum_all = TRUE)
                logprior_beta <- mlnormal_eval_priors( pars = beta ,
                            prior = prior_args$prior ,    sum_all = TRUE)
            }
            if ( prior_args$use_penalty ){
                res <- mlnormal_eval_penalty( beta=beta , theta=theta ,
                            penalty_pars = prior_args$penalty_pars )
                logprior_theta <- - res$penalty_theta
                logprior_beta <- - res$penalty_beta
            }
            if (REML){
                logprior_beta <- 0
            }
            log_prior <- logprior_beta + logprior_theta
            log_posterior <- ll + log_prior
            crit <- log_posterior
        }

        if ( ( crit < objfun0 ) & ( it == 1 ) ){
            theta0 <- theta - theta_increment
            theta <- theta0 + 1/2 * theta_increment
            theta_change <- mlnormal_parameter_change( pars=theta , pars0=theta0 )
        } else {
            do_iterate <- FALSE
        }

        it <- it + 1
        if ( it == 2){
            do_iterate <- FALSE
        }
    }

    #--- output
    res <- list("loglike" = ll , log_posterior = log_posterior ,
                    objfun = crit , log_prior = log_prior ,
                    theta = theta , theta_change = theta_change )
    return(res)
}
