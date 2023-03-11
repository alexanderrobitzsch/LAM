## File Name: mlnormal_linear_regression_bayes.R
## File Version: 0.04
## File Last Change: 2018-11-23


mlnormal_linear_regression_bayes <- function( W, beta, W2, mu2 )
{
    # given OLS estimate beta with precison W and
    # prior distribution N(mu2, W2^-1)
    Wtot1 <- solve( W + W2 )
    beta <- Wtot1 %*% ( W %*% beta + W2 %*% mu2 )
    return(beta)
}
