## File Name: mlnormal_postproc_parameters.R
## File Version: 0.14
## File Last Change: 2017-03-03 18:24:55


#############################################################
# postprocessing of parameters
mlnormal_postproc_parameters <- function( theta , theta_init ,
		beta , beta_init , theta_infomat , XVX , level , prior_args ){ 
	
	theta_names <- names(theta_init)
	beta_names <- names(beta_init)	
	
	theta <- mlnormal_as_vector_names(pars = theta , parnames = theta_names	)
	beta <- mlnormal_as_vector_names(pars = beta , parnames = beta_names )
	NB <- length(beta)
	NT <- length(theta)
	
	theta_vcov <- solve(theta_infomat)
	rownames(theta_vcov) <- colnames(theta_vcov) <- theta_names
													
	theta_summary <- data.frame(
						"parm" = theta_names ,
						"prior" = NA , 
						"penalty" = NA , 
						"est" = theta , 
						"se" = mlnormal_sqrt_diag(matr=theta_vcov) )
    theta_summary <- sirt::parmsummary_extend( dfr = theta_summary , level = level )
	
	beta_vcov <- solve(XVX)
	beta_summary <- data.frame(
						"parm" = beta_names ,
						"prior" = NA , 	
						"penalty" = NA , 						
						"est" = beta , 
						"se" = mlnormal_sqrt_diag(matr=beta_vcov) )
    beta_summary <- sirt::parmsummary_extend( dfr = beta_summary , level = level )

	if ( prior_args$use_prior ){
		dens <- paste0(prior_args$dens$prior)
		beta_summary$prior <- dens[ 1:NB ]
		theta_summary$prior <- dens[ NB + 1:NT ]
	} else {
		beta_summary$prior <- NULL
		theta_summary$prior <- NULL
	}				

	if ( prior_args$use_penalty ){
		pens <- prior_args$penalty_pars
		beta_summary$penalty <- pens$lambda_beta * pens$weights_beta
		theta_summary$penalty <- pens$lambda_theta * pens$weights_theta 
	} else {
		beta_summary$penalty <- NULL
		theta_summary$penalty <- NULL
	}	
	
	#--- collect output
	coefs <- c( beta , theta )
		
	NP <- NB + NT
	parnames <- c( beta_names , theta_names )
	names(coefs) <- parnames
	vcovs <- matrix( 0 , nrow=NP , ncol=NP)
	rownames(vcovs) <- colnames(vcovs) <- parnames 
	vcovs[ 1:NB , 1:NB ] <- beta_vcov
	vcovs[ NB + 1:NT , NB + 1:NT ] <- theta_vcov
		
	#---------------------------------
	# OUTPUT
	res <- list( beta = beta , theta = theta , beta_summary = beta_summary ,
				theta_summary = theta_summary , beta_vcov = beta_vcov ,
				theta_vcov = theta_vcov , coefs = coefs , vcovs = vcovs)
	return(res)	
}
####################################################################
