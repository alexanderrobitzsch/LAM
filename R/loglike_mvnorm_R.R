## File Name: loglike_mvnorm_R.R
## File Version: 0.381


#* returns the log-likelihood value of a multivariate
#* normal distribution fitted with sufficient statistics
loglike_mvnorm_R <- function( M, S, mu, Sigma, n, log=TRUE, lambda=0,
        ginv=FALSE, eps=1e-30 )
{
    if ( lambda > 0){
        Sigma0 <- 0*Sigma
        diag(Sigma0) <- diag(Sigma)
        w <- n /( n + lambda )
        Sigma <- w * Sigma + (1-w)*Sigma0
    }
    if (ginv){
        requireNamespace('MASS')
        Sigma1 <- MASS::ginv(Sigma)
    } else {
        Sigma1 <- solve(Sigma)
    }
    p <- ncol(Sigma)
    det_Sigma <- det(Sigma)
    if ( det_Sigma < eps ){
        det_Sigma <- eps
    }
    l1 <- - p * log( 2*pi) - t( M - mu ) %*% Sigma1 %*% ( M - mu ) -
                    log( det_Sigma )  - sum( diag( Sigma1 %*% S ) )
    l1 <- n/2 * l1
    if (! log){ l1 <- exp(l1) }
    l1 <- l1[1,1]
    return(l1)
}
