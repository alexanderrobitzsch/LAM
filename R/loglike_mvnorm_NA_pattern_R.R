## File Name: loglike_mvnorm_NA_pattern_R.R
## File Version: 0.18


loglike_mvnorm_NA_pattern_R <- function( suff_stat, mu, Sigma, log=TRUE, lambda = 0,
	ginv = FALSE, eps = 1e-30 )
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
		M_pp <- M[[pp]]
		S_pp <- S[[pp]]
		nobs_pp <- nobs[[pp]]
		#-- evaluate log-likelihood for data corresponding to missing pattern
		ll_pp <- loglike_mvnorm_R( M=M_pp, S=S_pp, mu=mu_pp, Sigma=Sigma_pp, 
						n=nobs_pp, log=log, lambda=lambda, ginv=ginv, eps=eps )						
		ll <- ll + ll_pp
	}		
	#--- output
	return(ll)
}
