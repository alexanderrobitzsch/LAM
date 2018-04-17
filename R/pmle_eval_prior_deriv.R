## File Name: pmle_eval_prior_deriv.R
## File Version: 0.03

pmle_eval_prior_deriv <- function(prior, pars, h=1e-4, eps=1e-100)
{	
    abs_par <- abs(pars)
    hvec <- h * ifelse(abs_par > 1, abs_par, 1)
	priorval1 <- pmle_eval_prior(pars=pars+hvec, prior=prior)
	priorval2 <- pmle_eval_prior(pars=pars-hvec, prior=prior)
	prior_grad <- ( priorval1 - priorval2 ) / ( 2 * hvec )
	return(prior_grad)
}
