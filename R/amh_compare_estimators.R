## File Name: amh_compare_estimators.R
## File Version: 0.03

amh_compare_estimators <- function(res_MAP , res_Mean , res_pmle ){	
	pars0 <- res_MAP$pars
	NP <- length(pars0)	
	dfr <- matrix( 0 , nrow=NP+3 , ncol=3)
	colnames(dfr) <- c("MAP" , "mMAP" , "Mean")		
	res_pmle$ll <- res_pmle$loglik
	res_pmle$priorval <- res_pmle$logprior	
	for (cc in 1:3){			
		# cc <- 1
		res0 <- switch( cc , 
					res_MAP ,
					res_pmle ,
					res_Mean )		
		dfr[(1:NP)+3,cc] <- res0$pars
		dfr[1,cc] <- res0$ll
		dfr[2,cc] <- res0$priorval
		dfr[3,cc] <- res0$posteriorval
	}	
	dfr <- data.frame( "parm" = c( c("Log Likelihood" , 
				"Log Prior", "Log Posterior") , names(pars0) ) , dfr )		
	return(dfr)
}
