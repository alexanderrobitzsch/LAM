## File Name: loglike_mvnorm_NA_pattern.R
## File Version: 0.04


loglike_mvnorm_NA_pattern <- function( suff_stat, mu, Sigma, log=TRUE, lambda = 0 )
{
	ll <- 0
	NP <- suff_stat$NP
	nobs <- suff_stat$nobs
	M <- suff_stat$M
	S <- suff_stat$S
	varindex <- suff_stat$varindex	
	for (pp in 1:NP){
		varindex_pp <- varindex[[pp]]
		mu_pp <- mu[ varindex_pp ]
		Sigma_pp <- Sigma[ varindex_pp, varindex_pp, drop=FALSE ]
		#-- evaluate log-likelihood for data corresponding to missing pattern
		ll_pp <- loglike_mvnorm( M=M[[pp]], S=S[[pp]], mu=mu_pp, Sigma=Sigma_pp, 
						n=nobs[[pp]], log=log, lambda=lambda )
		ll <- ll + ll_pp
	}		
	#--- output
	return(ll)
}
