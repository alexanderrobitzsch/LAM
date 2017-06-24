
mlnormal_eval_penalty_update_theta <- function( theta , prior_args , iter ,
	der , theta_infomat1 , control_theta )
{
	lambda_theta <- prior_args$penalty_pars$lambda_theta
	weights_theta <- prior_args$penalty_pars$weights_theta
	NT <- length(theta)
	theta0 <- theta

	#-----------------------------------------
	#--- Newton-Raphson step if iter = 1
	if (iter == 0){
		res <- mlnormal_update_theta_newton_step( theta = theta , der = der , 
					theta_infomat = theta_infomat1 , control_theta = control_theta  )
		theta <- res$theta
	}

	#--- 
	lambda <- lambda_theta * weights_theta
	do_iterate <- TRUE
	it <- 1

# Revalpr("theta")
	
	while(do_iterate){
		theta00 <- theta
		for (tt in 1:NT){
			# tt <- 1	# parameter tt
			eta_tt <- der[tt]
			xsi_tt <- theta_infomat1[ tt , tt ]
			# elim_tt <- c(tt)
			# alpha_tt <- theta[ - elim_tt ]
			# gamma_tt <- theta_infomat1[ - elim_tt , - elim_tt , drop=FALSE]
			# create term to be panalized
			# term_tt <- as.numeric( eta_tt + 2 * gamma_tt  %*% alpha_tt )

			term_tt <- theta[tt] + eta_tt / xsi_tt			
			lambda_tt <- lambda[tt] / xsi_tt
			term_tt <- mlnormal_soft_thresholding( x = term_tt , lambda = lambda_tt )			
			theta[tt] <- term_tt			
		}
		it <- it + 1
		theta_change <- mlnormal_parameter_change( pars=theta , pars0=theta00 )
		if ( it > 1 ){
			do_iterate <- FALSE
		}
	}	
	
	theta_infomat1 <-  mlnormal_covmat_add_ridge( covmat = theta_infomat1 ,
							eps = control_theta$ridge )
	Hinv <- solve( theta_infomat1 )			
	#--- output
	res <- list( der = der , theta_infomat = theta_infomat1 , 
					theta = theta , Hinv = Hinv )
	return(res)	
}
