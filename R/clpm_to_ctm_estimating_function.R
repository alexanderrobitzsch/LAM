## File Name: clpm_to_ctm_estimating_function.R
## File Version: 0.03
## File Last Change: 2019-05-05


clpm_to_ctm_estimating_function <- function(x, delta1, delta2)
{
    ND <- length(x)
    N1 <- as.integer(sqrt(ND))
    Phi1 <- matrix(x, nrow=N1, ncol=N1, byrow=FALSE)
    A <- clpm_to_ctm_paths_to_drift(Phi1=Phi1, delta1=delta1)
    Phi2 <- clpm_to_ctm_drift_to_paths(A=A, delta2=delta2)
    y <- c( as.vector(A), as.vector(Phi2) )
    return(y)
}
