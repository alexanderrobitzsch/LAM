## File Name: amh_posterior.R
## File Version: 0.02
## File Last Change: 2017-01-18 11:02:46
amh_posterior <- function( pars , model , prior , data){
	#*** evaluate likelihood
	ll <- amh_loglike( model=model, data=data , pars=pars)
	#*** evaluate priors
	priorval <- amh_eval_priors( pars= pars , prior=prior )
	#*** compute posterior
	posteriorval <- ll + priorval
	#--- output
	res <- list( ll = ll , priorval = priorval ,
				posteriorval = posteriorval	, pars = pars )
	return(res)
}
