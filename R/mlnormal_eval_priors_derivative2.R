## File Name: mlnormal_eval_priors_derivative2.R
## File Version: 0.02

mlnormal_eval_priors_derivative2 <- function( pars , prior , h )
{	
	#--- adjust numerical differentiation parameter
	hvec <- mlnormal_adjust_numdiff_parameter( h = h, pars = pars  )
	#--- evaluate pars		
	x0 <- mlnormal_eval_priors( pars = pars , prior = prior )
	x1 <- mlnormal_eval_priors( pars = pars + hvec , prior = prior )
	x2 <- mlnormal_eval_priors( pars = pars - hvec , prior = prior )
	#--- compute gradient and second derivative
	der <- (x1-x2)/ (2*hvec)
	info <- ( x1 + x2 - 2*x0) / hvec^2
	infomat <- diag(info)	
	res <- list( der = der , info = info ,  infomat = infomat )	
	return(res)
}
