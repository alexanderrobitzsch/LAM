

###################################################################
pmle_ic <- function( dev , N , pars , model , data , post_values ){	   	   
	# Information criteria
	ic <- list( "deviance" = dev , "n" = N )

	ic$loglike <- post_values$ll
	ic$prior <- post_values$prior
	ic$post <- post_values$post
	
	ic$np <- length(pars)
    # AIC
    ic$AIC <- dev + 2*ic$np
    # BIC
    ic$BIC <- dev + ( log(ic$n) )*ic$np
    # CAIC (consistent AIC)
    ic$CAIC <- dev + ( log(ic$n) + 1 )*ic$np
	# corrected AIC
    ic$AICc <- ic$AIC + 2*ic$np * ( ic$np + 1 ) / ( ic$n - ic$np - 1 )			
	return(ic)	
}	
###################################################################		
