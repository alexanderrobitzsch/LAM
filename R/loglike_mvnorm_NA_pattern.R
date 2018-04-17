## File Name: loglike_mvnorm_NA_pattern.R
## File Version: 0.18


loglike_mvnorm_NA_pattern <- function( suff_stat, mu, Sigma, log=TRUE, lambda = 0,
	ginv = FALSE, eps = 1e-30, use_rcpp=FALSE )
{
	if (use_rcpp){
		ll <- lam_rcpp_loglike_mvnorm_na_pattern_rcpp( suff_stat=suff_stat, mu=mu, Sigma=Sigma, 
					use_log=log, lambda=lambda, ginv=ginv, eps=eps ) 	
	} else {
		ll <- loglike_mvnorm_NA_pattern_R( suff_stat=suff_stat, mu=mu, Sigma=Sigma, log=log, 
					lambda=lambda, ginv=ginv, eps=eps ) 						
	}
	#--- output
	return(ll)
}
