
mlnormal_update_theta_newton_step <- function( theta , der , theta_infomat ,
	control_theta )
{
	theta_infomat1 <-  mlnormal_covmat_add_ridge( covmat = theta_infomat ,
							eps = control_theta$ridge )
	Hinv <- solve( theta_infomat1 )				
	#*** change in parameters
	theta_diff <- Hinv %*% der
	theta <- theta + theta_diff	
	theta <- as.vector(theta)
	#--- output
	res <- list( theta = theta , Hinv = Hinv , 
				theta_infomat = theta_infomat )	
	return(res)		
}
