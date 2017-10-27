## File Name: mlnormal_update_beta.R
## File Version: 0.44

################################################################
# update beta
mlnormal_update_beta <- function( NB , Z_index , G , beta , 
		Z_list , X_list , y_list , V1_list , theta , REML = FALSE ,
		id_list , N , V1zero = NULL , REML_shortcut = FALSE ,
		rcpp_args , X , y , use_Rcpp, prior_args , control_beta ){
	#-----------------
	beta0 <- beta
	P <- NULL
		
	if ( use_Rcpp ){
		mlnormal_update_beta_XVX_ <- mlnormal_update_beta_XVX_Rcpp
	} else {
		mlnormal_update_beta_XVX_ <- mlnormal_update_beta_XVX_R		
	}
	res <- mlnormal_update_beta_XVX_( NB=NB , Z_index=Z_index , 
				G = G  , V1_list = V1_list  , X_list = X_list , 
				y_list = y_list ,  rcpp_args = rcpp_args ,
				X = X , y = y )		
	XVX <- res$XVX
	XVY <- res$XVY
	XV_list <- res$XV_list

#  zz0 <- Sys.time()
# cat("++ XVX XVY ") ; zz1 <- Sys.time(); print(zz1-zz0) ; zz0 <- zz1

	#---- loop in case of REML ... compute P
	if ( ( ! REML_shortcut ) & REML ){
	    XVX1 <- solve(XVX)
		V1 <- mlnormal_fill_matrix_from_list( V1=V1zero , V1_list=V1_list , 
			      id_list=id_list , G =G )
		H1 <- XVX1 %*% crossprod(X , V1 )
		P <- V1 - V1 %*% X %*% H1 
	}						
	#---- end REML
		
	#**************************
	# update beta 

	
	#-- estimation routines
	# GLS
	if ( prior_args$use_GLS ){
		beta <- mlnormal_update_beta_GLS( XVX=XVX , XVY=XVY , 
					control_beta=control_beta) 	
	}
	# prior distributions				
	if ( prior_args$use_prior ){
		beta <- mlnormal_update_beta_iterations_priors(beta = beta0 , 
					prior_args = prior_args , XVX = XVX  , XVY = XVY ,
					control_beta = control_beta )											
	}
	# prior distributions				
	if ( prior_args$use_penalty ){
		beta <- mlnormal_update_beta_iterations_penalty(beta = beta0 , 
					prior_args = prior_args , XVX = XVX  , XVY = XVY ,
					control_beta = control_beta )											
	}	
	
	#** convert to vector
	beta <- mlnormal_as_vector_names(pars = beta , parnames = names(beta0) )
	
	#*** bounds on parameters
	beta <- sirt::bounds_parameters( pars = beta , lower = control_beta$beta_lower ,
					upper = control_beta$beta_upper  )
		
	# difference in beta values
	beta_change <- mlnormal_parameter_change( pars=beta , pars0=beta0 )
	
	#----------------------------------
	#--- output
	res <- list("beta" = beta , "beta_change" = beta_change ,
							XVX = XVX , XVY = XVY , P = P ,
							beta_increment = beta - beta0 )
	return(res)
}
################################################################
		
