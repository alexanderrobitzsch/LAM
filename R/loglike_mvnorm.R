## File Name: loglike_mvnorm.R
## File Version: 0.30

##############################################################
# returns the log-likelihood value of a multivariate
# normal distribution fitted with sufficient statistics
loglike_mvnorm <- function( M, S, mu, Sigma, n, log=TRUE, lambda=0,
        ginv=FALSE, eps=1e-30, use_rcpp=FALSE )
{
    if (use_rcpp){
        l1 <- lam_rcpp_loglike_mvnorm( M=M, S=S, mu=mu, Sigma=Sigma, n=n, use_log=log,
                    lambda=lambda, ginv=ginv, eps=eps )
    } else {
        l1 <- loglike_mvnorm_R( M=M, S=S, mu=mu, Sigma=Sigma, n=n, log=log, lambda=lambda,
                    ginv=ginv, eps=eps )
    }
    return(l1)
}
#################################################################
