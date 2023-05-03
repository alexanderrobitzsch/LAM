## File Name: mlnormal_process_prior.R
## File Version: 0.18

#############################################################
# processing prior distributions
mlnormal_process_prior <- function( prior, beta, theta,
    numdiff.parm, descriptions, lambda_beta, weights_beta,
    lambda_theta, weights_theta)
{
    pars <- c( beta, theta )
    use_penalty <- FALSE
    #--- check for penalties
    use_penalty <- ( ! is.null(lambda_theta) ) | ( ! is.null(lambda_beta) )
    if ( use_penalty){
        prior <- NULL
    }
    use_prior <- ! ( is.null(prior) )
    no_prior_no_penalty <- ( ! use_prior )


    #*** no priors, no penalties
    if ( no_prior_no_penalty ){
        res <- list( use_prior=FALSE, prior=NULL, dens=NULL,
                    use_GLS=TRUE )
    }
    descriptions["log_like_verbose2"] <- descriptions["log_like_verbose"]

    #*** prior distributions
    if (use_prior){
        res <- pmle_process_prior( prior=prior, pars=pars )
        res$use_prior <- TRUE
        prior <- res$prior
        res$use_GLS <- FALSE
        descriptions["des_method"] <- paste0( descriptions["des_method"],
                                        " with Prior Distributions")
        descriptions["log_like_verbose2"] <- "Log posterior"
    }
    #*** penalty functions
    if (use_penalty){
        res <- list()
        res$use_prior <- FALSE
        if ( is.null( lambda_beta) ){
            lambda_beta <- 0
        }
        if ( is.null( weights_beta) ){
            weights_beta <- 1 + 0*beta
        }
        if ( is.null( lambda_theta) ){
            lambda_theta <- 0
        }
        if ( is.null( weights_theta) ){
            weights_theta <- 1 + 0*theta
        }
        res$use_GLS <- FALSE
        descriptions["des_method"] <- paste0( descriptions["des_method"],
                                        " with Penalty Function")
        descriptions["log_like_verbose2"] <- "Log posterior"
    }

    res$use_penalty <- use_penalty
    res$numdiff.parm <- numdiff.parm
    penalty_pars <- list()
    penalty_pars$lambda_beta <- lambda_beta
    penalty_pars$weights_beta <- weights_beta
    penalty_pars$lambda_theta <- lambda_theta
    penalty_pars$weights_theta <- weights_theta
    res$penalty_pars <- penalty_pars

    #--- output
    res$descriptions <- descriptions
    return(res)
}
###################################################################
