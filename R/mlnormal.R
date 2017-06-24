
############################################################################
# general estimation function for normally distributed data
mlnormal <- function( y , X , id , Z_list , Z_index , beta=NULL , theta ,
			method = "ML" , prior=NULL , 
			lambda_beta = NULL , weights_beta = NULL , 
			lambda_theta = NULL , weights_theta = NULL , 
			beta_lower = NULL , beta_upper = NULL ,
			theta_lower = NULL , theta_upper = NULL ,
			maxit = 800 , globconv = 1E-5 , conv = 1E-6,
			verbose = TRUE , REML_shortcut = NULL , use_ginverse = FALSE ,
			vcov = TRUE , variance_shortcut = TRUE , use_Rcpp = TRUE , level = .95 ,
			numdiff.parm = 1E-4 , control_beta = NULL , control_theta = NULL )
{

	#*** preliminaries	
	CALL <- match.call()
	s1 <- Sys.time()
	disp <- mlnormal_create_disp( symbol="." , length=60 , line_break = TRUE )

zz0 <- Sys.time()		
	# process input and print
	res <- mlnormal_verbose_f0(verbose = verbose, disp=disp)
	
	res <- mlnormal_proc( id=id , y=y , X=X , beta = beta , theta = theta ,
				REML_shortcut = REML_shortcut , method=method ,
				Z_list = Z_list , Z_index = Z_index , 
				variance_shortcut=variance_shortcut , use_Rcpp = use_Rcpp )
 cat("* proc mlnormal ") ; zz1 <- Sys.time(); print(zz1-zz0) ; zz0 <- zz1	
 
	id_list <- res$id_list 
	G <- res$G
	y_list <- res$y_list
	X_list <- res$X_list
	N <- res$N
	NB <- res$NB
	NT <- res$NT
	theta <- res$theta
	beta <- res$beta		
	theta_init <- theta
	beta_init <- beta
	REML_shortcut <- res$REML_shortcut
	REML <- res$REML
	descriptions <- res$descriptions
	id <- res$id
	y <- res$y
	X <- res$X
	Z_list <- res$Z_list
	Z_index <- res$Z_index
	freq_id <- res$freq_id 
	do_compute <- res$do_compute
	rcpp_args <- res$rcpp_args
	use_Rcpp <- res$use_Rcpp
	# reorder_obs <- res$reorder_obs																							
												
	#*** control arguments
	res <- mlnormal_proc_control( control_beta = control_beta , 
				control_theta = control_theta , beta_lower = beta_lower , 
				beta_upper = beta_upper , theta_lower = theta_lower , 
				theta_upper = theta_upper )		
	control_beta <- res$control_beta
	control_theta <- res$control_theta
	
	#*** some information about method
	V1zero <- if (REML){ matrix(0, nrow=N, ncol=N) } else {NULL}
		
	#*** convert prior if necessary		
	res <- mlnormal_process_prior( prior=prior , beta=beta , theta=theta ,
					numdiff.parm = numdiff.parm , descriptions = descriptions,
					lambda_beta = lambda_beta , weights_beta = weights_beta ,
					lambda_theta = lambda_theta , weights_theta = weights_theta 
					)	
	prior <- res$prior
	dens <- res$dens
	use_prior <- res$use_prior
	prior_args <- res
	descriptions <- res$descriptions	
		
	#*** inits for iterations
	iter <- 1
	objfun <- ll <- 1E-300
	converged <- FALSE
	more_iterations <- TRUE 
	
	###******** iterate
	while ( more_iterations ){
		
		# print verbose = TRUE
		res <- mlnormal_verbose_f1(verbose=verbose, disp=disp , iter=iter)
		
		ll0 <- ll
		objfun0 <- objfun
zz0 <- Sys.time()	
		
		# update V			
		mlnormal_update_V <-
			if (use_Rcpp){ mlnormal_update_V_Rcpp } else { mlnormal_update_V_R }			
			
		res <- mlnormal_update_V( Z_index=Z_index , G=G , theta=theta ,  
					Z_list=Z_list , use_ginverse = use_ginverse ,
					variance_shortcut = variance_shortcut , freq_id = freq_id ,
					do_compute = do_compute , rcpp_args = rcpp_args)
		V_list <- res$V_list
		V1_list <- res$V1_list
		rcpp_args <- res$rcpp_args
# cat("* update V ") ; zz1 <- Sys.time(); print(zz1-zz0) ; zz0 <- zz1		
			
		# derivatives of V with respect to theta
		res <- mlnormal_update_ml_derivative_V( N=N , NT=NT , G=G ,
					Z_index=Z_index , Z_list=Z_list , theta=theta ,
					REML = REML , V1_list = V1_list ,
					variance_shortcut = variance_shortcut , freq_id = freq_id ,
					do_compute = do_compute
							)
		D1_V_list <- res$D1_V_list
		V1_D1V_V1_list <- res$V1_D1V_V1_list
		
# cat("* derivative V ") ; zz1 <- Sys.time(); print(zz1-zz0) ; zz0 <- zz1					

		# update beta
		res <- mlnormal_update_beta( NB=NB , Z_index = Z_index , G=G , beta=beta , 
					Z_list=Z_list , X_list=X_list , y_list=y_list , V1_list=V1_list ,
					theta = theta , REML = REML , id_list = id_list , N = N ,
					V1zero = V1zero , REML_shortcut = REML_shortcut ,
					rcpp_args = rcpp_args , X = X , y = y , use_Rcpp = use_Rcpp ,
					prior_args = prior_args , control_beta = control_beta )
		beta <- res$beta
		beta_change <- res$beta_change			
		XVX <- res$XVX 
		XVY <- res$XVY
		P <- res$P
#		cat("* update beta ") ; zz1 <- Sys.time(); print(zz1-zz0) ; zz0 <- zz1					
						
		# update theta
		res <- mlnormal_update_theta_ml( y=y , X=X , beta=beta , 
					Z_index=Z_index , NT=NT , G=G , id_list=id_list , V1_list=V1_list ,
					D1_V_list=D1_V_list, theta = theta , P = P , REML = REML ,
					V1zero = V1zero , REML_shortcut = REML_shortcut ,
					V1_D1V_V1_list = V1_D1V_V1_list  , XVX = XVX ,
					variance_shortcut = variance_shortcut , freq_id = freq_id ,
					do_compute = do_compute , prior_args = prior_args ,
					control_theta = control_theta , iter = iter 
					)
		theta <- res$theta
		theta_change <- res$theta_change
		yresid <- res$yresid
		theta_infomat <- res$theta_infomat
		theta_increment <- res$theta_increment
#  cat("* update theta ") ; zz1 <- Sys.time(); print(zz1-zz0) ; zz0 <- zz1								

		# compute maximum likelihood fitting function
		res <- mlnormal_fit_function_ml( id_list=id_list , V1_list=V1_list , 
				   G=G , N=N , yresid=yresid , V_list = V_list , REML = REML ,
				   XVX = XVX , do_compute = do_compute , prior_args = prior_args ,
				   theta = theta , beta = beta ,
				   objfun0 = objfun0 , theta_increment = theta_increment ,
				   theta_change = theta_change 	)
		ll <- res$loglike
		objfun <- res$objfun
		theta <- res$theta
		theta_change <- res$theta_change
#  cat("* compute likelihood value ") ; zz1 <- Sys.time(); print(zz1-zz0) ; zz0 <- zz1										
			
		#** convergence criteria
		conv_temp <- max( beta_change , theta_change )
		globconv_temp <- abs( objfun - objfun0 )
		
		# print progress	
		res <- mlnormal_verbose_f2(verbose=verbose, disp=disp, iter=iter, 
					descriptions=descriptions ,objfun=objfun , 
					objfun0=objfun0 , beta_change=beta_change , 
					theta_change=theta_change )
			
		# convergence indicator
        converged <- ( globconv_temp < globconv ) & ( conv_temp < conv )
		# indicator whether more iterations should be conducted
		more_iterations <- 	( iter < ( maxit + 1 ) ) & ( ! converged )		
		iter <- iter + 1						
			
	}
	###**** end iterations
					
	#---------------------------------------------------
	# extract information matrix for restricted maximum likelihood estimation
	if (REML_shortcut & REML & vcov ){
        if (verbose){		
		    cat(disp)
	        cat("Compute standard errors   " , 
		              paste( Sys.time() ) , "\n" )
			utils::flush.console()						
		}	
		res <- mlnormal_update_beta( NB=NB , Z_index = Z_index , G=G , beta=beta , 
					Z_list=Z_list , X_list=X_list , y_list=y_list , V1_list=V1_list ,
					theta = theta , REML = REML , id_list = id_list , N = N ,
					V1zero = V1zero , REML_shortcut = FALSE ,
					rcpp_args = rcpp_args , X = X , y = y , use_Rcpp = use_Rcpp ,
					prior_args = prior_args , control_beta = control_beta )
		P <- res$P
# cat("-- infomat update beta ") ; zz1 <- Sys.time(); print(zz1-zz0) ; zz0 <- zz1										
		res <- mlnormal_update_theta_ml( y=y , X=X , beta=beta , 
					Z_index=Z_index , NT=NT , G=G , id_list=id_list , V1_list=V1_list ,
					D1_V_list=D1_V_list, theta = theta , P = P , REML = REML ,
					V1zero = V1zero , REML_shortcut = FALSE ,
					V1_D1V_V1_list = V1_D1V_V1_list  , XVX = XVX ,
					variance_shortcut = variance_shortcut , freq_id = freq_id ,
					do_compute = do_compute , prior_args = prior_args ,
					control_theta = control_theta
						)
		theta_infomat <- res$theta_infomat
# cat("-- infomat computation ") ; zz1 <- Sys.time(); print(zz1-zz0) ; zz0 <- zz1												
															
	}  
		
	#*** arrange output of parameter vectors
	res <- mlnormal_postproc_parameters( theta=theta , theta_init = theta_init ,
			    beta=beta , beta_init=beta_init , theta_infomat=theta_infomat , 
			    XVX =XVX , level = level , prior_args = prior_args )	
	beta <- res$beta
	theta <- res$theta
	beta_summary <- res$beta_summary
	theta_summary <- res$theta_summary
	beta_vcov <- res$beta_vcov
	theta_vcov <- res$theta_vcov
	coefs <- res$coefs
	vcovs <- res$vcovs
	
	#*** evaluate likelihood value and posterior (in case of priors and
	#    or penalties)
	posterior_obj <- mlnormal_postproc_eval_posterior(ll=ll , beta=beta , 
						theta=theta , prior_args = prior_args , REML=REML )
		
	#*** information criteria
	ic <- mlnormal_ic( dev = - 2*ll , beta=beta , theta=theta , N=N , G=G,
				posterior_obj = posterior_obj )	
	
	#******************************************
	#***** OUTPUT  ****************************	
	s2 <- Sys.time()
	res <- list( theta = theta , beta = beta ,
				theta_summary = theta_summary , 
				beta_summary = beta_summary , 
				coef = coefs , vcov = vcovs , 					
				deviance = - 2* ll , ic = ic , 
				V_list = V_list , V1_list = V1_list , 
				D1_V_list = D1_V_list , 
				descriptions = descriptions , REML = REML , 
				vcov_REML = vcov & REML , 
				prior_args = prior_args , posterior_obj = posterior_obj ,
				control_beta = control_beta , control_theta = control_theta ,
				converged = converged , iter = iter-1 , 
				CALL = CALL , s1 = s1 , s2 = s2	, diff_time = s2-s1	
					)
	class(res) <- "mlnormal"
	return(res)
}
#########################################################################
