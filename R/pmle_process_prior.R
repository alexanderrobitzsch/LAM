
###########################################################
pmle_process_prior <- function( prior , pars ){
	#*** convert string to prior list
	if ( is.character(prior) ){	
		prior <- sirt::prior_model_parse( prior_model = prior )		
	}
	#*** correct list specification
	parnames <- names(pars)
	NP <- length(parnames)
	prior1 <- as.list(1:NP)
	names(prior1) <- parnames

	#**** correct prior specification
	prior0 <- list( "dimproper" , list(NA) )
	parnames0 <- setdiff( parnames , names(prior) )
	NP0 <- length(parnames0)
	if (NP0 > 0 ){
		for (pp in 1:NP0){
			prior[[ parnames0[pp] ]] <- prior0
		}
	}	
	#**** write prior
	for (pp in 1:NP){
		prior1[[pp]] <- prior[[ parnames[pp] ]]
	}	
	dens1 <- pmle_prior_extract_density( prior=prior1 )
	dens1 <- data.frame("parameter" = names(pars) , "prior" = dens1 )
	rownames(dens1) <- NULL	
	res <- list("prior"=prior1 , "dens" = dens1 )					
	return(res)
}					
###########################################################					


