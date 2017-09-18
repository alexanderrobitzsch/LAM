## File Name: pmle_data_proc.R
## File Version: 0.02
## File Last Change: 2017-01-18 11:02:51

###############################################################
# pmle data processing
pmle_data_proc <- function( pars , pars_lower , pars_upper ){
	NP <- length(pars)
	if ( is.null( pars_lower) ){
		pars_lower <- rep( - Inf , NP )
	}	
	if ( is.null( pars_upper) ){
		pars_upper <- rep(  Inf , NP )
	}	
	res <- list( pars_lower = pars_lower ,
				pars_upper = pars_upper )
	return(res)
}
