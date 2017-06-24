
mlnormal_postproc_eval_posterior <- function(ll , beta , theta ,
	prior_args, REML )
{
	use_prior <- prior_args$use_prior
	use_penalty <- prior_args$use_penalty	
	no_priors <- ( ! use_prior ) & ( ! use_penalty )
	ll <- as.vector(ll)
	
	#*** evaluate priors theta
	if ( use_prior){
		logprior_theta <- mlnormal_eval_priors( pars = theta , prior = prior_args$prior ,
								sum_all = TRUE)
		logprior_beta <- mlnormal_eval_priors( pars = beta , prior = prior_args$prior ,
								sum_all = TRUE)
		if (REML){
			logprior_beta <- 0
		}
		log_prior <- logprior_beta + logprior_theta
		log_posterior <- ll + log_prior	
		display_posterior <- TRUE
	}
	#--- no priors
	if ( no_priors){
		log_prior <- NA
		log_posterior <- NA
		display_posterior <- FALSE
	}
	if ( use_penalty){
		res <- mlnormal_eval_penalty( beta=beta , theta=theta , 
						penalty_pars = prior_args$penalty_pars )
		logprior_theta <- - res$penalty_theta
		logprior_beta <- - res$penalty_beta
		log_prior <- logprior_theta + logprior_beta
		log_posterior <- ll + log_prior
		display_posterior <- TRUE	
	}
	
	res <- list( "loglike" = ll , log_prior = log_prior , 
			log_posterior = log_posterior , use_prior = use_prior ,
			no_priors = no_priors , use_penalty = use_penalty , 
			REML = REML , display_posterior = display_posterior )			
	return(res)
}
