## File Name: lam_vcov_linear_trafo.R
## File Version: 0.02

lam_vcov_linear_trafo <- function(V, A)
{
    V2 <- A %*% V %*% t(A)
    return(V2)
}
