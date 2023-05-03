## File Name: vcov_linear_trafo.R
## File Version: 0.01

vcov_linear_trafo <- function(V, A)
{
    V2 <- A %*% V %*% t(A)
    return(V2)
}
