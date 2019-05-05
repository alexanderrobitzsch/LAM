## File Name: vcov_to_se.R
## File Version: 0.02

vcov_to_se <- function(x, to_matrix=FALSE, byrow=TRUE)
{
    N <- nrow(x)
    y <- sqrt(diag(x))
    if (to_matrix){
        N1 <- sqrt(N)
        y <- matrix(y, nrow=N1, ncol=N1, byrow=byrow)
    }
    return(y)
}
