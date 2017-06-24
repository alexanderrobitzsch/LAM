
##############################################################
# returns the log-likelihood value of a multivariate
# normal distribution fitted with sufficient statistics
loglike_mvnorm <- function( M , S , mu , Sigma , n , log=TRUE , lambda = 0 ){	
	if ( lambda > 0){
		Sigma0 <- 0*Sigma
		diag(Sigma0) <- diag(Sigma)
		w <- n /( n + lambda )
		Sigma <- w * Sigma + (1-w)*Sigma0
	}
    Sigma1 <- solve(Sigma)
    p <- ncol(Sigma)
    det_Sigma <- det( Sigma )
    eps <- 1E-30
    if ( det_Sigma < eps ){   det_Sigma <- eps  }
    l1 <- - p * log( 2*pi) - t( M - mu ) %*% Sigma1 %*% ( M - mu ) - 
                 log( det_Sigma )  - sum( diag( Sigma1 %*% S ) )
    l1 <- n/2 * l1
    if (! log){ l1 <- exp(l1) }
    l1 <- l1[1,1]
    return(l1)
}
#################################################################		
