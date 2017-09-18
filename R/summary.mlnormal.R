## File Name: summary.mlnormal.R
## File Version: 0.15
## File Last Change: 2017-03-03 18:36:14
#*******************************************************
# Summary for mlnormal object
summary.mlnormal <- function( object , digits = 4 , file=NULL , ...){

    # open sink
    CDM::osink( file = file , suffix = paste0( "__SUMMARY.Rout") )

	cat("-----------------------------------------------------------------\n")
    d1 <- utils::packageDescription("LAM")
	cat( paste( d1$Package , " " , d1$Version , " (" , d1$Date , ")" , sep="") , "\n\n" )	
	cat( "Date of Analysis:" , paste( object$s2 ) , "\n" )
	cat("Computation Time:" , print(object$s2 - object$s1), "\n\n")
	
	cat("Call:\n", paste( deparse(object$CALL), sep = "\n", collapse = "\n"), 
				"\n\n", sep = "")	
	
	cat( object$descriptions["des_method"] , "\n\n")
	
#	modeltype <- object$irtmodel
#		cat( "   " , object$ic$n , "Cases, " , object$ic$I , "Items, " , 
#		        object$G , "Group(s)", # "," ,
#				"\n")  
	
	if ( object$REML & ! ( object$vcov_REML ) ){
		cat(" * Standard errors for random effects are not properly calculated! \n")
		cat(" * Use vcov = TRUE as an argument in 'mlnormal' if valid standard errors\n")
		cat("   for random effects parameters are requested.\n\n")
	}
	
    cat("-----------------------------------------------------------------\n")
	
	cat( "Number of observations =" , object$ic$N , "\n" )	
	cat( "Number of clusters =" , object$ic$G , "\n\n" )	
	
	cat( "Number of iterations =" , object$iter , "\n\n" )
    cat( "Deviance = " , round( object$deviance , 2 ) , "\n" )
    cat( paste0( object$descriptions["log_like_verbose"] 
					, " =" ) , round( object$ic$loglike , 2 ) , "\n" )	
	
	#---- print posterior
	if ( object$posterior_obj$display_posterior ){
		cat( "Log prior =" , 
				round( object$posterior_obj$log_prior , 2 ) , "\n" )	
		cat( "Log posterior =" , 
				round( object$posterior_obj$log_posterior , 2 ) , "\n" )		
	}
	
	cat("\n")	
    cat( "Number of estimated parameters = " , object$ic$np , "\n" )    
    cat( "  Number of estimated beta parameters = " , object$ic$np.beta , 
				"\n" )    	
    cat( "  Number of estimated theta parameters = " , object$ic$np.theta , 
				"\n\n" )    
	
	if ( ! object$REML ){	
		cat( "AIC  = " , round( object$ic$AIC , 1 ) , " | penalty =" , 
				round( object$ic$AIC - object$ic$deviance ,2 ) , 
				"   | AIC = -2*LL + 2*p  \n" )    
	}
				
#    cat( "AICc = " , round( object$ic$AICc , 0 ) ," | penalty =" , round( object$ic$AICc - object$ic$deviance ,2 ) )
#		cat("    | AICc = -2*LL + 2*p + 2*p*(p+1)/(n-p-1)  (bias corrected AIC)\n" )   	
#    cat( "BIC  = " , round( object$ic$BIC , 0 ) , " | penalty =" , round( object$ic$BIC - object$ic$deviance ,2 ) , 
#			"   | BIC = -2*LL + log(n)*p  \n" )  
#    cat( "CAIC = " , round( object$ic$CAIC , 0 ) ," | penalty =" , round( object$ic$CAIC - object$ic$deviance ,2 ) )
#		cat("   | CAIC = -2*LL + [log(n)+1]*p  (consistent AIC)\n\n" )   

    cat("-----------------------------------------------------------------\n")
	cat("Beta Parameters\n")
	excl <- c("parm","prior")
	
	obji <- object$beta_summary
	a <- mlnormal_summary_round_helper(obji, digits=digits, exclude = excl, print=TRUE)

    cat("-----------------------------------------------------------------\n")
	cat("Theta Parameters\n")
	obji <- object$theta_summary
	a <- mlnormal_summary_round_helper(obji, digits=digits, exclude = excl , print=TRUE)
	
	# close sink
    CDM::csink( file = file )		
}
#*******************************************************
