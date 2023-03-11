## File Name: clpm_to_ctm_drift_to_paths.R
## File Version: 0.01
## File Last Change: 2019-05-05


clpm_to_ctm_drift_to_paths <- function(A, delta2)
{
    requireNamespace("expm")
    Phi2 <- expm::expm(x=A*delta2)
    return(Phi2)
}
