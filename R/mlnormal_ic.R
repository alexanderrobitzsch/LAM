

###################################################################
mlnormal_ic <- function( dev , beta , theta , N , G ,  posterior_obj  ){
	# Information criteria
	ic <- list( "deviance" = as.vector(dev) , N =N , G=G)
	
	ic$loglike <- - dev / 2 
	
	ic$np.beta <- length(beta)
	ic$np.theta <- length(theta)
	ic$np <- ic$np.beta + ic$np.theta
    # AIC
    ic$AIC <- dev + 2*ic$np
    # BIC
#    ic$BIC <- dev + ( log(ic$n) )*ic$np
    # CAIC (consistent AIC)
#    ic$CAIC <- dev + ( log(ic$n) + 1 )*ic$np
	# corrected AIC
#    ic$AICc <- ic$AIC + 2*ic$np * ( ic$np + 1 ) / ( ic$n - ic$np - 1 )		

	ic$log_prior <-  posterior_obj$log_prior
	ic$log_posterior <-  posterior_obj$log_posterior

	return(ic)	
}	
###################################################################	
